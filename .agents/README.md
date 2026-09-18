# `.agents/` — harness-neutral agent tooling

Skills and scripts for reviewing and simplifying changes to this repo, kept
independent of any one coding agent.

```
.agents/
├── scripts/
│   ├── clarinet-gates.sh        resolve a target diff -> surfaces -> the CI gates that apply
│   └── clarinet-peer-review.sh  adversarial review by a different model vendor than the host
└── skills/
    ├── clarinet-review/         correctness review (report-only)
    ├── clarinet-simplify/       quality pass (applies fixes)
    └── rust-conventions/        how to write Rust here (reference, no workflow)
```

`rust-conventions` is the single source for the repo's Rust rules. It is a
reference rather than a procedure: the other two skills point at it instead of
carrying their own copy, and `CLAUDE.md` points at it rather than inlining it.
A skill is the only form of this that both harnesses read natively — `@` imports
are Claude-only and `AGENTS.md` is Codex-only.

## Why `.agents/` and not `.claude/`

It is Codex's own convention: `codex-rs/ext/skills/src/host_roots.rs` defines
`AGENTS_DIR_NAME = ".agents"` and probes repo ancestors for `.agents/skills`.
Codex therefore finds these skills with no configuration at all.

Claude Code looks in `.claude/skills`, so that whole path is a symlink here:

```
.claude/skills -> ../.agents/skills
```

One canonical copy, discovered by both, and a skill added here needs no second
step to reach Claude Code. The cost is that `.claude/skills` cannot also hold a
Claude-only skill; if one is ever wanted, replace the directory symlink with
per-skill symlinks alongside it. A third harness needs either its own symlink or
a config entry pointing here — not a fork of the content.

## What stays neutral, and what cannot

The scripts are plain bash and the risk map is plain markdown, so both are
portable as-is. Two things in `clarinet-review/SKILL.md` are unavoidably
harness-specific, and both are written as a table or a conditional rather than
assuming one tool:

- **the general review pass** — `/code-review` on Claude Code, `codex review`
  on Codex, inline if neither exists
- **structured findings reporting** — Claude Code has `ReportFindings`; other
  harnesses just write the findings out

## The peer review

`clarinet-peer-review.sh` detects its host (`CLAUDECODE` -> claude, `CODEX_*` ->
codex, `PI_CODING_AGENT` -> pi) and sends the review to a **different vendor**,
with the repo's own agent instructions suppressed so the peer is not primed by
the same context the host already has:

| CLI | Vendor | Instructions suppressed by |
| --- | --- | --- |
| `codex` | openai | `-c project_doc_max_bytes=0` |
| `claude` | anthropic | `--safe-mode` |
| `pi` | whatever provider its model belongs to | `--no-context-files` (plus `--no-extensions --no-skills --no-prompt-templates`) |

The test is the **vendor, not the CLI**, because pi drives any provider it is
configured for. A pi peer running an Anthropic model is no second opinion at all
for a Claude Code host, however different the two binaries look — so a pi host
is matched on its exported `PI_PROVIDER`, and a pi peer on the provider of the
model it would actually run. A host whose vendor cannot be determined gets no
peer rather than a guessed one.

Candidates are tried in the order `codex claude pi`. `PEER_PREFER` moves one to
the front but cannot buy it past the vendor test.

### Picking the model

Each route chooses its own model, by discovery rather than a pinned slug — a
hardcoded one goes stale the moment a model is renamed, and the failure would be
a peer that silently stops running. `PEER_MODEL` overrides all of it.

- **codex** asks its own catalogue (`models_cache.json`) for a review-specialised
  model that supports the requested effort. This is worth more than it sounds:
  against a diff with a planted `exit 1` → `exit 0` in an error path, codex's
  general-purpose models returned an empty findings array and the review model
  caught it.
- **pi** takes the largest-context model it lists, because a model too small to
  hold the diff truncates without saying so.
- **claude** uses its default.

Anything unexpected — no catalogue, no `jq`, no match — falls back to the CLI's
own default, which still reviews.

It is non-blocking by contract: a missing CLI, a missing login or a timeout
logs a reason, exits 0, and writes no output file. Callers detect success
purely by the presence of that file. It must never fail the review that
called it.

### Egress

It sends the diff, and any repository files the peer reads for context, to that
vendor. Every send is logged, naming the vendor.

A peer on a **local** provider sends nothing off the machine, which is what
`PEER_PREFER=pi` is for — useful for a diff that should not leave, and as a
fallback when a cloud peer is rate-limited or out of credits. pi ships no default
model, so the route picks the largest-context model `pi --list-models` offers;
override with `PEER_MODEL=provider/id`. Local models are weaker reviewers and
their findings need the same verification as any other peer's — see step 7 of
`clarinet-review/SKILL.md`.
