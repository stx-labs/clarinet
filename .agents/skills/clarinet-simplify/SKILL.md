---
name: clarinet-simplify
description: Simplify a change in the Clarinet repo and apply the fixes — reuse, idiom, dead code, redundant comments. Use when asked to simplify, clean up or tidy a branch, commit or uncommitted work, and before writing or editing Rust here, since it holds the repo's Rust conventions.
---

# Simplify a Clarinet change

Make the change easier to read without changing what it does. Correctness bugs are out of scope; that is `clarinet-review`.

## Scope

Default to uncommitted work plus the current branch against `main`. If the user names a branch, commit, range or PR, use that instead. When the target is a past commit, the fixes land in the working tree; say so.

Skip generated and vendored files: `Cargo.lock`, `pnpm-lock.yaml`, `components/stacks-network/data/*`, `components/clarity-repl/src/repl/boot/*.clar`.

## What to look for

- Duplicated logic, or logic that re-implements something already in the workspace.
- Code the change made dead, and comments it made redundant.
- Rust that doesn't follow the conventions below.

Use your harness's own simplify pass if it has one, then add the Clarinet-specific checks below. Aim for code a reviewer understands faster, not for a smaller diff.

## Rust conventions

Write modern, idiomatic Rust. Prefer:

- Iterator chains and combinators over manual loops where they improve clarity
- `if let` / `let else` / `match` over chains of `if`/`else` with `.is_some()`/`.is_ok()` checks
- `?` for error propagation instead of manual `match`/`unwrap`
- Destructuring in function arguments and match arms
- Meaningful type aliases and newtypes where they aid readability
- `impl Into<T>` / `AsRef<T>` parameters for flexible function signatures when appropriate
- Implement `From<T>` (not `Into<T>`) for type conversions; the blanket impl provides `Into` for free
- Derive macros (`Clone`, `Debug`, `Default`, etc.) rather than manual implementations
- Standard library traits (`From`, `Display`, `FromStr`) for type conversions
- `indoc!` / `formatdoc!` for multi-line strings
- `strum` derives (`EnumString`, `Display`, `EnumIter`) instead of hand-written enum boilerplate

Reuse what exists instead of re-deriving it:

- Default epoch and Clarity version: `clarinet_defaults::{DEFAULT_EPOCH, DEFAULT_CLARITY_VERSION}`
- A boot contract's epoch: `clarity_repl::repl::boot::get_boot_contract_epoch_and_clarity_version`
- Formatting a Clarity `Value`: `clarity_repl::repl::clarity_values::value_to_string`

Crates shared with the SDK and the LSP compile to wasm32:

- Use `uprint!` / `ueprint!`, not `println!` / `eprintln!`, which print nothing on wasm32.
- Keep `#[cfg(target_arch = "wasm32")]` gates. Collapsing one breaks a build you probably didn't run.

Keep comments to the minimum. A comment explains why, never what.

## Finish

Run the checks for the surfaces you touched (see `AGENTS.md`). If one fails, fix the cause or revert that simplification. Never weaken a test to make it pass.

Report what you changed, one line per change with `file:line`, and anything you chose not to change and why.
