---
name: clarinet-improve
description: Find and implement Clarinet improvements within a time budget, delivering focused draft PRs. For cleanup or review of a particular change, use clarinet-simplify or clarinet-review.
---

# Improve Clarinet

## Scope

- Default: whole repository, including code unchanged on this branch.
- Work from existing contracts; preserve observable behavior except for confirmed bug fixes.
- Prioritize confirmed bugs and high-confidence removals. Choose topics from the
  [discovery guide](references/discovery.md) and run log
  (`.agents/logs/improvements.md`, repository root; gitignored). Rotate topics during longer runs.
- Allow dependency removals, compatible deduplication, and focused upgrades or
  replacements addressing demonstrated problems.
- Unless explicitly in scope, defer product/compatibility decisions: public APIs,
  defaults, protocol semantics, broad dependency migrations, cache-sharing policy,
  SDK reset semantics.
- MSRV/runtime increases need material benefit and an explicit compatibility
  decision; minor syntax cleanup is insufficient.

## Time budget

| Invocation | Budget |
| --- | --- |
| No limit | **1 hour**; announce before starting |
| Duration or deadline | Use supplied limit; clarify ambiguous date/timezone |
| Vague longer run, e.g. `overnight` | Ask for duration or deadline |

- Read clock; calculate deadline. Include discovery, implementation, simplification,
  review, checks, PR creation, and handoff.
- Check remaining time between issues and before lengthy commands. Start another
  issue only if the full workflow can reasonably finish before deadline.
- Stop by deadline, reporting unfinished work; stop earlier if useful work is exhausted
  or blocked. Resumption/compaction retains deadline unless user changes it.
- Skill instructions provide neither an enforced timeout nor session persistence;
  use harness continuation support for long runs.

```text
$clarinet-improve for 1 hour; focus on unused dependencies
$clarinet-improve until 07:00 UTC tomorrow
```

## Run

1. Read existing run log first; create if absent. Revalidate candidates against HEAD;
   revisit rejections only with new evidence.
2. Record starting branch/HEAD, staged/unstaged/untracked work, UTC start/deadline,
   and budget. Preserve the original checkout; prefer a separate worktree.
3. Repeat the issue workflow below while time permits. Log each issue's branch,
   verification, PR link or blocker, and next actions. Retain completed, deferred,
   and rejected candidates with reasons.

## Issue workflow

Invoking this skill authorizes commits, pushes, and draft PRs for completed issues,
unless the user requests local-only work. Use [clarinet-ship](../clarinet-ship/SKILL.md)
for applicable checks on each PR's final diff and for publishing; the exceptions
below override its git conventions.

1. **Isolate:** choose one change or a few small related changes per PR, e.g. three
   small Clippy lint fixes or two formatter whitespace fixes.
   Before each issue, fetch `main` from **`stx-labs/clarinet`** (verify the remote URL).
   Fast-forward local `main` when safe; preserve divergent commits and occupied
   worktrees. Always start a separate branch (kebab-case topic) from the freshly
   fetched upstream `main` tip, never from another issue's branch or a stale local base.
2. **Implement:** use relevant [repository checks](references/checks.md) and collect
   the evidence below. Defer issues lacking required validation; continue independent work.
3. **Simplify:** run [clarinet-simplify](../clarinet-simplify/SKILL.md) on this issue's diff.
4. **Self-review:** run [clarinet-review](../clarinet-review/SKILL.md) on the resulting
   diff, then fix confirmed findings. This workflow authorizes those fixes.
   Repeat simplification/review as needed for fixes; validate the final diff before publishing.
5. **Publish:** commit, push, and open a **draft** PR using `clarinet-ship`. Record
   publishing blockers and preserve the branch if publication fails.
6. **Continue:** after opening the PR, select another issue if the full workflow fits
   the remaining budget. Otherwise, hand off.

### Publishing exceptions

- Prefix PR titles with `[AI] `, e.g. `[AI] perf(linter): Reduce runtime by 10%`.
- Conventional Commit format is optional for individual commits.
- Never mark PRs ready during this run; initial human review must precede readiness.

### Validation evidence

| Change | Evidence |
| --- | --- |
| Bug fix | Regression test failing before fix |
| Refactor | Equivalent-output checks |
| Optimization | Comparable before/after release measurements |
| Test changes | Gap closed or reason removed tests added no protection |

## Handoff

Update resolved TODOs with their changes. Link the run log and draft PRs in the final
report; list unfinished work, branch locations, and validation or publishing blockers.
Preserve unfinished work locally at deadline; do not publish an unvalidated PR.
Before finishing, including deadline or blocker exits, verify the original checkout
is on its starting branch with its original staged, unstaged, and untracked work intact.
Keep improvement work on its own branches/worktrees; restore any starting work moved aside.
