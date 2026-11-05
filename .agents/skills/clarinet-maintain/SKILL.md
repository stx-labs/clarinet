---
name: clarinet-maintain
description: Find and implement proactive or unattended maintenance in Clarinet, including dead code and dependency removal, bug fixes, refactoring, and optimization. For cleanup or review of a particular change, use clarinet-simplify or clarinet-review.
---

# Maintain Clarinet

Choose work whose expected outcome can be established from existing contracts,
without new product requirements. Preserve observable behavior except for confirmed
bug fixes.

## Establish the run

- Accept a wall-clock duration or an absolute deadline in the invocation, such as
  `for 1 hour`, `for 8 hours`, or `until 07:00 UTC tomorrow`. If none is supplied,
  default to **1 hour** and announce that budget before starting. If the user asks
  for a longer run without a concrete limit (for example, `overnight`), ask for a
  duration or deadline; do not silently substitute the one-hour default. Clarify an
  ambiguous deadline's date or timezone unless established by context.
- Read the clock at the start and calculate a concrete deadline. The budget includes
  discovery, implementation, validation, and handoff. Check remaining time between
  batches and before lengthy commands, reserving time for verification and reporting.
  Do not start changes that cannot reasonably be validated within the remaining time.
- Default to the whole repository, including code unchanged on this branch. Continue
  with coherent batches within the budget; stop earlier if useful work is exhausted
  or blocked. At the deadline, stop and report unfinished work and incomplete checks
  rather than extending the run or claiming unverified changes are complete.
- Read `.agents/notes/TODO.md` if present, including Completed and Decisions and
  audit notes. Revalidate historical findings against this checkout, and retain
  recorded rejections unless new evidence warrants revisiting them.
- Keep a compact run log in `.agents/reviews/maintenance-<branch>.md`, replacing `/`
  with `-` (use the short HEAD when detached). This directory is gitignored. Record
  starting HEAD, start time and deadline in UTC, the supplied or default budget,
  candidates and verdicts, verification, and next actions. Update it after each batch
  so interrupted runs can resume. A continuation retains the original deadline unless
  the user changes it; do not restart the budget after an interruption or compaction.

Examples:

```text
$clarinet-maintain
$clarinet-maintain for 1 hour; focus on unused dependencies
$clarinet-maintain until 07:00 UTC tomorrow
```

These are instructions for managing elapsed time, not a host-enforced timeout or a
mechanism for keeping the session alive. Use the harness's continuation support for
long runs when needed; the skill alone does not provide it.

## Choose work

Prefer confirmed correctness problems and high-confidence removals over cosmetic
changes. Use the [discovery guide](references/discovery.md) to investigate dependencies,
dead code, unit test coverage and quality, performance, language idioms, static
analysis, build settings, unsafe code, and structural or type-driven refactors.
Choose relevant topics for the run; this is a menu, not a checklist to exhaust
before making progress. Investigate beyond TODOs and lint output.

Defer candidates that need a product or compatibility decision, such as cache-sharing
policy or SDK reset semantics. Record the question and continue independent work.
Dependency removals, compatible deduplication, and focused upgrades or replacements
can qualify when they address a demonstrated problem and preserve supported behavior
and platforms. Defer broad dependency migrations and changes to public APIs,
defaults, or Stacks protocol semantics unless explicitly in scope. An MSRV or runtime
baseline increase needs a material benefit and an explicit compatibility decision;
minor syntax cleanup alone does not justify one.

## Complete each batch

1. Consult the relevant [maintenance checks](references/maintenance-checks.md).
2. Require a regression test that fails before a bug fix, equivalent-output checks
   for refactors, and comparable before/after release measurements for optimizations.
   Straightforward removals can use existing build/test checks.
   For test-only batches, run the affected tests and record the behavioral gap closed
   or why removed tests provided no unique protection.
3. Run the applicable **Checks** from [clarinet-ship](../clarinet-ship/SKILL.md);
   use only its validation workflow.
4. Use [clarinet-simplify](../clarinet-simplify/SKILL.md) and
   [clarinet-review](../clarinet-review/SKILL.md) where useful, limited to this run's
   diff.
5. Update the log and any resolved TODO entries.

Defer work blocked by unavailable validation instead of accumulating more batches
behind the same gap.

## Handoff

Leave changes uncommitted by default. Link the run log in the final report.

Background: [research and alternatives](references/research.md).
