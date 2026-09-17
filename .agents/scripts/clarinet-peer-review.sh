#!/usr/bin/env bash
#
# Adversarial peer review of a diff, run by a DIFFERENT vendor than the host.
#
# Harness-agnostic on purpose: the host attests who it is (or is detected), and
# the peer is whichever supported CLI runs a DIFFERENT VENDOR than the host.
# Run from Claude Code the peer is codex; run from Codex the peer is claude.
# Neither harness is assumed.
#
# The test is the vendor, not the CLI, because pi drives any provider it is
# configured for: a pi peer running an Anthropic model is no second opinion at
# all for a Claude Code host, however different the two binaries look.
#
# Independence comes from two things, and both matter:
#   - a different model vendor than the host, and
#   - suppressing the repo's agent instructions for the peer
#     (codex: -c project_doc_max_bytes=0 / claude: --safe-mode /
#     pi: --no-context-files), so the peer is not primed by the same
#     AGENTS.md / CLAUDE.md that shaped the host review.
# A finding both reviewers reach independently is worth more than either alone.
#
# Usage:
#   clarinet-peer-review.sh [--host claude|codex|pi] <diff-file> <brief-file> <out-file>
#   clarinet-peer-review.sh [--host ...] --dry-run     # print the argv, call nothing
#
# NON-BLOCKING BY DESIGN. Every failure logs a reason to stderr and exits 0
# without writing <out-file>. A missing CLI, an expired login or a timeout must
# never fail the review that called this. The caller detects success purely by
# the presence of <out-file>.
#
# DATA EGRESS: this sends the diff, and whatever repository files the peer reads
# for context, to the peer's vendor. Every send is logged below, naming the
# vendor. A peer on a local provider (ollama, llama-cpp) sends nothing off the
# machine, which is what PEER_PREFER=pi is for.
#
# Env:
#   PEER_MODEL     model id for the peer (its default otherwise; for pi, the
#                  largest-context model it lists)
#   PEER_EFFORT    reasoning effort (default xhigh for codex, high otherwise)
#   PEER_PREFER    move one CLI to the front of the candidate list; it still
#                  has to pass the different-vendor test
#   PEER_TIMEOUT   seconds before the peer is abandoned (default 600)

set -uo pipefail

log() { printf '[peer] %s\n' "$*" >&2; }
skip() { log "$*"; exit 0; }

PEER_TIMEOUT="${PEER_TIMEOUT:-600}"
# Whole seconds only: timeout(1) accepts suffixes and perl's alarm does not
# (it reads "10m" as 10), so an unvalidated value means two different timeouts
# on two machines — or none at all.
case $PEER_TIMEOUT in
  '' | *[!0-9]*)
    log "warning: PEER_TIMEOUT='$PEER_TIMEOUT' is not whole seconds; using 600"
    PEER_TIMEOUT=600
    ;;
esac

HOST=""
DRY_RUN=false
if [ "${1:-}" = "--host" ]; then
  HOST="${2:-}"
  shift 2
  # Unvalidated, a typo picks the host's own vendor as its "peer" — exactly what
  # the unattested-host refusal below exists to prevent.
  case $HOST in
    claude | codex | pi) ;;
    *) skip "unknown --host '$HOST'; expected claude, codex or pi" ;;
  esac
fi
if [ "${1:-}" = "--dry-run" ]; then
  DRY_RUN=true
  shift
fi

# Callers treat the presence of OUT as the sole success signal, so clear it
# before anything can skip — including the host/peer resolution below, whose
# skips are the most likely of all in practice.
case "${1:-}" in
  --dry-run | '') ;;
  *) [ -z "${3:-}" ] || rm -f "$3" ;;
esac

# --- who is the host? ------------------------------------------------------
# The host attests itself when it can; env detection is the fallback. An
# unknown host still runs, but the result cannot claim vendor independence.
if [ -z "$HOST" ]; then
  if [ -n "${CLAUDECODE:-}" ] || [ -n "${CLAUDE_CODE_SESSION_ID:-}" ]; then
    HOST=claude
  elif [ -n "${CODEX_SANDBOX:-}" ] || [ -n "${CODEX_HOME:-}" ] || [ -n "${CODEX_SESSION_ID:-}" ]; then
    HOST=codex
  elif [ -n "${PI_CODING_AGENT:-}" ] || [ -n "${PI_SESSION_ID:-}" ]; then
    HOST=pi
  else
    HOST=unknown
  fi
fi

# --- locate the CLIs -------------------------------------------------------
# The desktop app ships `codex` inside the bundle without linking it onto PATH.
# Append, never prepend, so a real CLI install stays authoritative.
if ! command -v codex >/dev/null 2>&1; then
  for d in "${HOME:-}/Applications/ChatGPT.app/Contents/Resources" \
    "/Applications/ChatGPT.app/Contents/Resources" \
    "${HOME:-}/Applications/Codex.app/Contents/Resources" \
    "/Applications/Codex.app/Contents/Resources"; do
    if [ -x "$d/codex" ]; then
      PATH="${PATH:+$PATH:}$d"
      export PATH
      break
    fi
  done
fi

peer_available() { command -v "$1" >/dev/null 2>&1; }

# pi ships no default model and its built-in default provider is usually
# unauthenticated, so the model is chosen here rather than left to pi. Largest
# context wins: the prompt is a whole diff, and a model too small to hold it
# truncates without saying so, which would look like a peer that found nothing.
pi_model() {
  if [ -n "${PEER_MODEL:-}" ]; then
    printf '%s' "$PEER_MODEL"
    return 0
  fi
  pi --list-models 2>/dev/null | awk '
    NR > 1 && NF >= 3 {
      ctx = $3; mult = 1
      if (ctx ~ /M$/) mult = 1000000; else if (ctx ~ /K$/) mult = 1000
      sub(/[KM]$/, "", ctx)
      if (ctx * mult > best) { best = ctx * mult; pick = $1 "/" $2 }
    }
    END { if (pick != "") print pick }
  '
}

# The vendor behind each CLI. codex and claude are fixed to one; pi is whatever
# provider its selected model belongs to, which is why it is resolved and not
# assumed.
vendor_of() {
  case "$1" in
    codex) printf 'openai' ;;
    claude) printf 'anthropic' ;;
    pi) printf '%s' "$(pi_model)" | cut -d/ -f1 ;;
  esac
}

case $HOST in
  claude) HOST_VENDOR=anthropic ;;
  codex) HOST_VENDOR=openai ;;
  # pi exports the provider it is running, so a pi host can be placed on the
  # same footing as the other two instead of being taken on trust.
  pi) HOST_VENDOR="${PI_PROVIDER:-}" ;;
  *) HOST_VENDOR="" ;;
esac

# Candidates in preference order. PEER_PREFER moves one to the front — for a
# local provider when the diff must not leave the machine — but cannot buy it
# past the vendor test below.
CANDIDATES=(codex claude pi)
if [ -n "${PEER_PREFER:-}" ]; then
  case " ${CANDIDATES[*]} " in
    *" $PEER_PREFER "*)
      REORDERED=("$PEER_PREFER")
      for c in "${CANDIDATES[@]}"; do
        [ "$c" = "$PEER_PREFER" ] || REORDERED+=("$c")
      done
      CANDIDATES=("${REORDERED[@]}")
      ;;
    *) log "warning: PEER_PREFER='$PEER_PREFER' is not a known peer CLI; ignoring" ;;
  esac
fi

# Prints "<cli> <vendor>" so the caller keeps both; recomputing the vendor
# outside would mean a second `pi --list-models`, and a race if it disagreed.
pick_peer() {
  local candidate vendor
  for candidate in "${CANDIDATES[@]}"; do
    peer_available "$candidate" || continue
    vendor="$(vendor_of "$candidate")"
    # An unresolvable vendor cannot be shown to differ from the host's, and
    # that is the one claim this script exists to make.
    [ -n "$vendor" ] || continue
    [ "$vendor" = "$HOST_VENDOR" ] && continue
    printf '%s %s' "$candidate" "$vendor"
    return 0
  done
  return 1
}

# Without a host vendor, nothing can be shown to differ from it, so any peer
# picked here might be the host's own vendor reviewing its own work — the one
# outcome this whole script exists to prevent. Refuse rather than guess: a
# review with no peer is honest, a review whose "independent" peer was the
# author is not. Pass --host explicitly to proceed on a harness this cannot
# detect.
[ -n "$HOST_VENDOR" ] ||
  skip "host vendor not determined (host: $HOST), so no peer can be attested as a different vendor; pass --host claude|codex|pi to force one"

PEER_PICK="$(pick_peer)" ||
  skip "no peer CLI on a different vendor than the host ($HOST/$HOST_VENDOR) is installed; no peer review"
PEER="${PEER_PICK% *}"
PEER_VENDOR="${PEER_PICK#* }"

case "$PEER" in
  codex) PEER_EFFORT="${PEER_EFFORT:-xhigh}" ;;
  claude) PEER_EFFORT="${PEER_EFFORT:-high}" ;;
  # Measured: a local model at "high" spent 15 minutes without converging on the
  # same diff that "medium" answered in two. Reasoning budget is not where a
  # small model's review gets better.
  pi) PEER_EFFORT="${PEER_EFFORT:-medium}" ;;
esac

# pick_peer resolved this in a subshell, so it did not survive; the pi route
# needs the full "provider/id", not just the vendor half of it.
PI_MODEL_ID=""
[ "$PEER" != pi ] || PI_MODEL_ID="$(pi_model)"

# --- argv per route --------------------------------------------------------
# Built in one place so --dry-run asserts on the same command the peer runs.
build_argv() {
  case "$PEER" in
    codex)
      # Prompt on stdin; last message to $RAW; repo agent docs suppressed.
      # No --json: the script reads only -o, and JSON mode swallows codex's own
      # error output (an out-of-credits failure arrives as exit 1 and nothing
      # else), which leaves the caller unable to say why the peer did not run.
      ARGV=(codex exec - --cd "$PWD" --skip-git-repo-check -s read-only
        -o "$RAW"
        -c "model_reasoning_effort=\"$PEER_EFFORT\""
        -c "project_doc_max_bytes=0")
      [ -z "${PEER_MODEL:-}" ] || ARGV+=(-m "$PEER_MODEL")
      ;;
    claude)
      # --safe-mode disables customizations, which is how this route drops the
      # repo's CLAUDE.md. Read stays enabled; everything that mutates does not.
      ARGV=(claude -p --safe-mode --disable-slash-commands
        --effort "$PEER_EFFORT" --permission-mode dontAsk
        --no-session-persistence --output-format text
        --disallowedTools Edit Write NotebookEdit Bash Task WebFetch WebSearch Skill 'mcp__*')
      [ -z "${PEER_MODEL:-}" ] || ARGV+=(--model "$PEER_MODEL")
      ;;
    pi)
      # Prompt on stdin, answer on stdout, same as the claude route.
      # --no-context-files is pi's analogue of project_doc_max_bytes=0: it is
      # what drops AGENTS.md / CLAUDE.md discovery. The other --no-* flags drop
      # project-local extensions, skills and prompt templates, which are the
      # rest of the context the host was primed with.
      #
      # --no-tools, unlike the other two routes, so this one reviews the diff
      # alone. It is a real loss of context, taken deliberately: with the read
      # tool enabled a local model spends the whole timeout in the tool loop and
      # returns nothing, and a peer that never answers is worth less than a
      # narrower one that does. The prompt below tells it so, so it does not
      # claim to have checked what it could not open.
      #
      # Replacing pi's coding-assistant system prompt is part of the same trade:
      # it is scaffolding for editing code, and this peer only emits a verdict.
      ARGV=(pi -p --no-session --no-approve
        --no-context-files --no-extensions --no-skills --no-prompt-templates
        --thinking "$PEER_EFFORT" --no-tools
        --system-prompt 'You are a code reviewer. Reply with only the JSON object requested.'
        --model "$PI_MODEL_ID")
      ;;
  esac
}

if $DRY_RUN; then
  RAW="<raw-out>"
  build_argv
  log "host: $HOST/$HOST_VENDOR  peer: $PEER/$PEER_VENDOR  effort: $PEER_EFFORT  model: ${PI_MODEL_ID:-${PEER_MODEL:-peer default}}"
  printf '  %q' "${ARGV[@]}" >&2
  printf '\n' >&2
  exit 0
fi

DIFF_FILE="${1:-}"
BRIEF_FILE="${2:-}"
OUT="${3:-}"
[ -n "$DIFF_FILE" ] && [ -n "$BRIEF_FILE" ] && [ -n "$OUT" ] ||
  skip "usage: clarinet-peer-review.sh [--host claude|codex|pi] <diff-file> <brief-file> <out-file>"
[ -s "$DIFF_FILE" ] || skip "empty diff; nothing to review"
[ -f "$BRIEF_FILE" ] || skip "brief not found: $BRIEF_FILE"

# The X run has to END the template: BSD mktemp treats a trailing suffix as a
# literal filename, so `-XXXXXX.md` creates that exact name once and then fails
# with "File exists" on every later call.
PROMPT="$(mktemp "${TMPDIR:-/tmp}/clarinet-peer-prompt-XXXXXX")" ||
  skip "could not create a temp file; continuing without a peer"
trap 'rm -f "$PROMPT" "${RAW:-}" "${RAW:-}.clean" "${ERRLOG:-}" "$OUT.part"' EXIT
RAW="$(mktemp "${TMPDIR:-/tmp}/clarinet-peer-raw-XXXXXX")" ||
  skip "could not create a temp file; continuing without a peer"
ERRLOG="$(mktemp "${TMPDIR:-/tmp}/clarinet-peer-err-XXXXXX")" ||
  skip "could not create a temp file; continuing without a peer"

{
  cat "$BRIEF_FILE"
  cat <<'EOF'

---

# Your task

Review ONLY the diff below, adversarially. You are a second opinion on a change
another reviewer has already passed; assume they missed something.

Ground rules:
- This repository's CI enforces formatting, clippy on every target (including
  wasm32), a lint banning stdout macros in wasm-reachable crates, and the full
  test suite. Do not report anything those would catch. (Whether they have been
  run on this particular change is the caller's to state, not an assumption you
  should make either way.)
EOF
  # Route-dependent: only the routes that actually carry a read tool may say so.
  # Telling a tool-less peer it can open files invites it to report what it
  # "found" in a file it never read.
  case "$PEER" in
    pi) printf '%s\n' "- You are shown the diff and nothing else. You cannot open repository files, so do not claim to have checked anything outside this diff; when a judgement needs a file you cannot see, say so instead of assuming." ;;
    *) printf '%s\n' "- You may read repository files for context. You cannot modify anything." ;;
  esac
  cat <<'EOF'
- Report only defects whose consequences justify acting. No style opinions.

Return ONLY a JSON object, with no prose around it and no code fence:

{"findings":[{"file":"path","line":123,"severity":"high|medium|low",
  "summary":"one sentence","failure_scenario":"concrete input or state, and what breaks"}]}

An empty findings array is a valid and useful answer.

# The diff

EOF
  cat "$DIFF_FILE"
} >"$PROMPT"

log "host $HOST -> peer $PEER (vendor: $PEER_VENDOR): sending ~$(wc -l <"$DIFF_FILE" | tr -d ' ') diff lines (model: ${PI_MODEL_ID:-${PEER_MODEL:-default}}, effort: $PEER_EFFORT)"

# --- run -------------------------------------------------------------------
build_argv
# Stock macOS ships neither timeout(1) nor gtimeout, and without one the timeout
# silently never arms — a hung peer then hangs its caller forever, the exact
# outcome the non-blocking contract exists to prevent. perl is always present
# there, and `alarm` gives the same bounded-exec semantics in one line.
if command -v gtimeout >/dev/null 2>&1; then
  TO_CMD=(gtimeout -k 10)
elif command -v timeout >/dev/null 2>&1; then
  TO_CMD=(timeout -k 10)
else
  TO_CMD=(perl -e 'alarm shift; exec @ARGV' --)
fi

# codex writes its answer to $RAW via -o; claude and pi print it to stdout.
case "$PEER" in
  codex) STDOUT_TARGET=/dev/null ;;
  *) STDOUT_TARGET="$RAW" ;;
esac

"${TO_CMD[@]}" "$PEER_TIMEOUT" "${ARGV[@]}" <"$PROMPT" >"$STDOUT_TARGET" 2>"$ERRLOG"
status=$?
if [ $status -ne 0 ]; then
  reason=$(grep -iE 'error|denied|credit|quota|rate.?limit|unauthor|expired' "$ERRLOG" 2>/dev/null | tail -1)
  [ -n "$reason" ] || reason=$(tail -1 "$ERRLOG" 2>/dev/null)
  case $status in
    124 | 142) skip "peer '$PEER' timed out after ${PEER_TIMEOUT}s; continuing without it" ;;
    *) skip "peer '$PEER' failed (exit $status): ${reason:-no diagnostic on stderr}; continuing without it" ;;
  esac
fi
[ -s "$RAW" ] || skip "peer '$PEER' returned nothing; continuing without it"

# --- normalize -------------------------------------------------------------
# Publish only a parseable findings object, so the caller never has to reason
# about a half-written or prose-wrapped reply. Models fence JSON even when told
# not to, so strip a fence before giving up.
#
# Without jq there is no way to honour that promise, so publish nothing rather
# than an unvalidated blob the caller would mistake for a real result.
command -v jq >/dev/null 2>&1 ||
  skip "jq not installed, so the reply cannot be validated; continuing without a peer"

sed -e 's/^```json[[:space:]]*$//' -e 's/^```[[:space:]]*$//' "$RAW" >"$RAW.clean"

# Build in a sibling temp file and rename: OUT must never be observable in a
# partial state, because its mere presence is what tells the caller to read it.
#
# `-s` matters: without it a reply holding two JSON documents writes both,
# concatenated, into a file the caller would then trust. `..` reaches the root,
# so this single expression also covers a plain top-level {"findings": [...]}.
if jq -es '[.. | objects | select(has("findings"))] | first | select(. != null)' \
  <"$RAW.clean" >"$OUT.part" 2>/dev/null; then
  mv -f "$OUT.part" "$OUT" || skip "could not publish the peer reply to $OUT"
else
  rm -f "$OUT.part"
  skip "peer '$PEER' reply was not schema-shaped JSON; continuing without it"
fi

log "peer '$PEER' review written to $OUT ($(jq '.findings | length' <"$OUT" 2>/dev/null || echo '?') findings)"
