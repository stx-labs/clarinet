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

Claude Code looks in `.claude/skills`, so that directory holds symlinks:

```
.claude/skills/clarinet-review   -> ../../.agents/skills/clarinet-review
.claude/skills/clarinet-simplify -> ../../.agents/skills/clarinet-simplify
```

One canonical copy, discovered by both. A third harness needs either its own
symlink or a config entry pointing here — not a fork of the content.

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

`clarinet-peer-review.sh` detects its host (`CLAUDECODE` -> claude,
`CODEX_*` -> codex) and sends the review to a *different vendor*, with the
repo's own agent instructions suppressed (`-c project_doc_max_bytes=0` for
codex, `--safe-mode` for claude) so the peer is not primed by the same context
the host already has.

It is non-blocking by contract: a missing CLI, a missing login or a timeout
logs a reason, exits 0, and writes no output file. Callers detect success
purely by the presence of that file. It must never fail the review that
called it.

It sends the diff, and any repository files the peer reads for context, to
that vendor. Every send is logged.
