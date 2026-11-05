---
name: clarinet-maintain
description: Find and implement proactive or unattended maintenance in Clarinet, including dead code and dependency removal, bug fixes, refactoring, and optimization. For cleanup or review of a particular change, use clarinet-simplify or clarinet-review.
---

# Maintain Clarinet

Choose work whose expected outcome can be established from existing contracts,
without new product requirements.

## Establish the run

- Default to the whole repository, including code unchanged on this branch. A bare
  invocation completes one coherent batch; an extended or overnight pass continues
  with further batches within the requested budget.
- Read `.agents/notes/TODO.md` if present, including Completed and Decisions and
  audit notes. Revalidate historical findings against this checkout, and retain
  recorded rejections unless new evidence warrants revisiting them.
- Keep a compact run log in `.agents/reviews/maintenance-<branch>.md`, replacing `/`
  with `-` (use the short HEAD when detached). This directory is gitignored. Record
  starting HEAD, candidates and verdicts, verification, and next actions. Update it
  after each batch so interrupted runs can resume.

## Choose work

Prefer confirmed correctness problems and high-confidence removals over cosmetic
changes. Investigate beyond TODOs and lint output.

Defer candidates that need a product or compatibility decision, such as cache-sharing
policy or SDK reset semantics. Record the question and continue independent work.
Dependency upgrades and changes to public APIs, defaults, or Stacks protocol
semantics require explicit scope beyond general maintenance.

## Complete each batch

1. Consult the relevant [maintenance checks](references/maintenance-checks.md).
2. Require a regression test that fails before a bug fix, equivalent-output checks
   for refactors, and comparable before/after release measurements for optimizations.
   Straightforward removals can use existing build/test checks.
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
