#!/usr/bin/env bash
#
# Adversarial peer review of a diff, run by a DIFFERENT vendor than the host.
#
# Harness-agnostic on purpose: the host attests who it is (or is detected), and
# the peer is whichever supported CLI is NOT the host. Run from Claude Code the
# peer is codex; run from Codex the peer is claude. Neither harness is assumed.
#
# Independence comes from two things, and both matter:
#   - a different model vendor than the host, and
#   - suppressing the repo's agent instructions for the peer
#     (codex: -c project_doc_max_bytes=0 / claude: --safe-mode), so the peer is
#     not primed by the same AGENTS.md / CLAUDE.md that shaped the host review.
# A finding both reviewers reach independently is worth more than either alone.
#
# Usage:
#   clarinet-peer-review.sh [--host claude|codex] <diff-file> <brief-file> <out-file>
#   clarinet-peer-review.sh [--host ...] --dry-run     # print the argv, call nothing
#
# NON-BLOCKING BY DESIGN. Every failure logs a reason to stderr and exits 0
# without writing <out-file>. A missing CLI, an expired login or a timeout must
# never fail the review that called this. The caller detects success purely by
# the presence of <out-file>.
#
# DATA EGRESS: this sends the diff, and whatever repository files the peer reads
# for context, to the peer's vendor. Every send is logged below.
#
# Env:
#   PEER_MODEL     model id for the peer (its default otherwise)
#   PEER_EFFORT    reasoning effort (default xhigh for codex, high for claude)
#   PEER_TIMEOUT   seconds before the peer is abandoned (default 600)

set -uo pipefail

log() { printf '[peer] %s\n' "$*" >&2; }
skip() { log "$*"; exit 0; }

PEER_TIMEOUT="${PEER_TIMEOUT:-600}"

HOST=""
DRY_RUN=false
if [ "${1:-}" = "--host" ]; then
  HOST="${2:-}"
  shift 2
fi
if [ "${1:-}" = "--dry-run" ]; then
  DRY_RUN=true
  shift
fi

# --- who is the host? ------------------------------------------------------
# The host attests itself when it can; env detection is the fallback. An
# unknown host still runs, but the result cannot claim vendor independence.
if [ -z "$HOST" ]; then
  if [ -n "${CLAUDECODE:-}" ] || [ -n "${CLAUDE_CODE_SESSION_ID:-}" ]; then
    HOST=claude
  elif [ -n "${CODEX_SANDBOX:-}" ] || [ -n "${CODEX_HOME:-}" ] || [ -n "${CODEX_SESSION_ID:-}" ]; then
    HOST=codex
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

# Candidates in preference order, minus the host. Vendor independence is the
# whole point, so the host's own CLI is never eligible.
pick_peer() {
  local candidate
  for candidate in codex claude; do
    [ "$candidate" = "$HOST" ] && continue
    peer_available "$candidate" && {
      printf '%s' "$candidate"
      return 0
    }
  done
  return 1
}

# An unattested host cannot be excluded from the candidates, so any peer picked
# here might be the host's own vendor reviewing its own work — the one outcome
# this whole script exists to prevent. Refuse rather than guess: a review with no
# peer is honest, a review whose "independent" peer was the author is not.
# Pass --host explicitly to proceed on a harness this cannot detect.
[ "$HOST" != unknown ] ||
  skip "host harness not detected, so no peer can be attested as a different vendor; pass --host claude|codex to force one"

PEER="$(pick_peer)" ||
  skip "no peer CLI other than the host ($HOST) is installed; no peer review"

case "$PEER" in
  codex) PEER_EFFORT="${PEER_EFFORT:-xhigh}" ;;
  claude) PEER_EFFORT="${PEER_EFFORT:-high}" ;;
esac

# --- argv per route --------------------------------------------------------
# Built in one place so --dry-run asserts on the same command the peer runs.
build_argv() {
  case "$PEER" in
    codex)
      # Prompt on stdin; last message to $RAW; repo agent docs suppressed.
      ARGV=(codex exec - --cd "$PWD" --skip-git-repo-check -s read-only
        --json -o "$RAW"
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
  esac
}

if $DRY_RUN; then
  RAW="<raw-out>"
  build_argv
  log "host: $HOST  peer: $PEER  effort: $PEER_EFFORT  model: ${PEER_MODEL:-peer default}"
  printf '  %q' "${ARGV[@]}" >&2
  printf '\n' >&2
  exit 0
fi

DIFF_FILE="${1:-}"
BRIEF_FILE="${2:-}"
OUT="${3:-}"
[ -n "$DIFF_FILE" ] && [ -n "$BRIEF_FILE" ] && [ -n "$OUT" ] ||
  skip "usage: clarinet-peer-review.sh [--host claude|codex] <diff-file> <brief-file> <out-file>"
# Callers treat the presence of OUT as the sole success signal, so a result
# from an earlier run must never survive into this one.
rm -f "$OUT"

[ -s "$DIFF_FILE" ] || skip "empty diff; nothing to review"
[ -f "$BRIEF_FILE" ] || skip "brief not found: $BRIEF_FILE"

# The X run has to END the template: BSD mktemp treats a trailing suffix as a
# literal filename, so `-XXXXXX.md` creates that exact name once and then fails
# with "File exists" on every later call.
PROMPT="$(mktemp "${TMPDIR:-/tmp}/clarinet-peer-prompt-XXXXXX")" ||
  skip "could not create a temp file; continuing without a peer"
RAW="$(mktemp "${TMPDIR:-/tmp}/clarinet-peer-raw-XXXXXX")" ||
  skip "could not create a temp file; continuing without a peer"
trap 'rm -f "$PROMPT" "$RAW" "$RAW.clean" "$OUT.part"' EXIT

{
  cat "$BRIEF_FILE"
  cat <<'EOF'

---

# Your task

Review ONLY the diff below, adversarially. You are a second opinion on a change
another reviewer has already passed; assume they missed something.

Ground rules:
- This repository's CI already checks formatting, clippy on every target
  (including wasm32), a lint banning stdout macros in wasm-reachable crates,
  and the full test suite. All of them pass on this change. Do not report
  anything those would catch.
- You may read repository files for context. You cannot modify anything.
- Report only defects whose consequences justify acting. No style opinions.

Return ONLY a JSON object, with no prose around it and no code fence:

{"findings":[{"file":"path","line":123,"severity":"high|medium|low",
  "summary":"one sentence","failure_scenario":"concrete input or state, and what breaks"}]}

An empty findings array is a valid and useful answer.

# The diff

EOF
  cat "$DIFF_FILE"
} >"$PROMPT"

log "host $HOST -> peer $PEER: sending ~$(wc -l <"$DIFF_FILE" | tr -d ' ') diff lines (model: ${PEER_MODEL:-default}, effort: $PEER_EFFORT)"

# --- run -------------------------------------------------------------------
build_argv
TO_BIN="$(command -v gtimeout || command -v timeout || true)"

# codex writes its answer to $RAW via -o; claude prints it to stdout.
STDOUT_TARGET=/dev/null
[ "$PEER" != claude ] || STDOUT_TARGET="$RAW"

# Stock macOS ships neither timeout(1) nor gtimeout, so without a fallback the
# timeout silently never arms and a hung peer hangs its caller forever — the
# exact outcome the non-blocking contract exists to prevent.
run_peer() {
  if [ -n "$TO_BIN" ]; then
    "$TO_BIN" "$PEER_TIMEOUT" "${ARGV[@]}" <"$PROMPT" >"$STDOUT_TARGET" 2>/dev/null
    return $?
  fi

  "${ARGV[@]}" <"$PROMPT" >"$STDOUT_TARGET" 2>/dev/null &
  local pid=$! waited=0
  while kill -0 "$pid" 2>/dev/null; do
    if [ "$waited" -ge "$PEER_TIMEOUT" ]; then
      kill -TERM "$pid" 2>/dev/null
      sleep 2
      kill -KILL "$pid" 2>/dev/null
      wait "$pid" 2>/dev/null
      return 124
    fi
    sleep 1
    waited=$((waited + 1))
  done
  wait "$pid"
}

run_peer
status=$?
[ $status -eq 0 ] || skip "peer '$PEER' failed (exit $status) or timed out after ${PEER_TIMEOUT}s; continuing without it"
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
if jq -e 'select(type == "object" and has("findings"))' <"$RAW.clean" >"$OUT.part" 2>/dev/null ||
  jq -ers '[.. | objects | select(has("findings"))] | first | select(. != null)' \
    <"$RAW.clean" >"$OUT.part" 2>/dev/null; then
  mv -f "$OUT.part" "$OUT"
else
  rm -f "$OUT.part"
  skip "peer '$PEER' reply was not schema-shaped JSON; continuing without it"
fi

log "peer '$PEER' review written to $OUT ($(jq '.findings | length' <"$OUT" 2>/dev/null || echo '?') findings)"
