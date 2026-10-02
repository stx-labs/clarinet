---
name: clarinet-simplify
description: Simplify a change in the Clarinet repo and apply the fixes — reuse, idiom, dead code, redundant comments. Use when asked to simplify, clean up or tidy a branch, commit or uncommitted work.
---

# Simplify a Clarinet change

Make the change easier to read without changing what it does. Correctness bugs are out of scope; that is `clarinet-review`.

## Scope

Default to uncommitted work plus the current branch against `main`. If the user names a branch, commit, range or PR, use that instead. When the target is a past commit, the fixes land in the working tree; say so.

Skip generated and vendored files: `Cargo.lock`, `pnpm-lock.yaml`, `components/stacks-network/data/*`, `components/clarity-repl/src/repl/boot/*.clar`.

## What to look for

- Duplicated logic, or logic that re-implements something already in the workspace.
- Code the change made dead, and comments it made redundant.
- Rust that doesn't follow `.agents/rust-conventions.md`.

Run your harness's general simplify pass if it's available to you: the `simplify` skill in Claude Code. Otherwise, simplify inline. Then add the Clarinet-specific checks above. Aim for code a reviewer understands faster, not for a smaller diff.

## Finish

Run the checks for the surfaces you touched (listed in `clarinet-ship`). If one fails, fix the cause or revert that simplification. Never weaken a test to make it pass.

Report what you changed, one line per change with `file:line`, and anything you chose not to change and why.
