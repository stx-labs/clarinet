---
name: clarinet-maintain
description: Find and perform Clarinet maintenance tasks without new product requirements. For cleanup or review of a particular change, use clarinet-simplify or clarinet-review.
---

# Maintain Clarinet

## Scope

- Default: whole repository, including code unchanged on this branch.
- Work from existing contracts; preserve observable behavior except for confirmed bug fixes.
- Prioritize confirmed bugs and high-confidence removals. Choose topics from the
  [discovery guide](references/discovery.md); rotate topics during longer runs.
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

- Read clock; calculate deadline. Include discovery, implementation, checks, handoff.
- Check remaining time between batches and before lengthy commands. Reserve time
  for validation; avoid changes that cannot reasonably be verified before deadline.
- Stop by deadline, reporting unfinished work; stop earlier if useful work is exhausted
  or blocked. Resumption/compaction retains deadline unless user changes it.
- Skill instructions provide neither an enforced timeout nor session persistence;
  use harness continuation support for long runs.

```text
$clarinet-maintain for 1 hour; focus on unused dependencies
$clarinet-maintain until 07:00 UTC tomorrow
```

## Run

1. Read `.agents/notes/TODO.md` if present, including Completed, Decisions, audit
   notes. Revalidate historical findings; revisit rejections only with new evidence.
2. Keep a run log: `.agents/reviews/maintenance-<branch>.md` (gitignored). Replace
   branch `/` with `-`; use short HEAD when detached. Record HEAD, UTC start/deadline,
   budget, candidates/verdicts, verification, next actions. Update after each batch.
3. Complete coherent batches using relevant [repository checks](references/maintenance-checks.md).
   Log deferred decisions; continue independent work. Defer candidates lacking
   required validation rather than accumulating batches behind that gap.

## Validate and handoff

| Change | Evidence |
| --- | --- |
| Bug fix | Regression test failing before fix |
| Refactor | Equivalent-output checks |
| Optimization | Comparable before/after release measurements |
| Straightforward removal | Existing build/test checks |
| Test changes | Affected suite; gap closed or reason removed tests added no protection |

- Run applicable **Checks** from [clarinet-ship](../clarinet-ship/SKILL.md), validation only.
- Apply [clarinet-simplify](../clarinet-simplify/SKILL.md) and
  [clarinet-review](../clarinet-review/SKILL.md) where useful, scoped to this run's diff.
- Update resolved TODOs and run log. Leave changes **uncommitted** by default;
  link log in final report.

Background only: [research and alternatives](references/research.md).
