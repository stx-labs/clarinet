---
name: rust-conventions
description: This skill should be used whenever writing, modifying or reviewing Rust in the Clarinet repo — before editing any .rs file, adding a crate dependency, or judging whether existing Rust fits the house style. It carries the repo's Rust style rules, the constants and helpers to reuse instead of re-deriving, and the platform rules that keep shared crates compiling for both the CLI and wasm32.
---

# Rust conventions for Clarinet

Read this before editing Rust here, not after. These are constraints that hold continuously, so nothing below is conditional on the kind of task.

## Style

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
- `indoc!` for multi-line string literals, so the content is not held hostage by
  its indentation in the source. `formatdoc!` is the interpolating form
- `strum` derives for enum boilerplate rather than hand-written `impl`s:
  `EnumString` for parsing, `Display` for rendering, `EnumIter` to enumerate.
  Pair them with `#[strum(serialize_all = "…")]` rather than spelling out each
  variant's string — `snake_case` and `lowercase` are both already in use here.
  Add the crate as `strum = { workspace = true, features = ["derive"] }`

Two rewrites this codebase has actually made, worth recognising:

- A closure returning an iterator, invoked twice just to test emptiness → bind it once and use `.peekable()` + `.peek()`
- Repeated `format!`-then-`eval_clarity_string` round trips where one Clarity expression would do

## Reuse what already exists

| Instead of | Use |
| --- | --- |
| A literal `StacksEpochId::…` / `ClarityVersion::…` default | `clarinet_defaults::{DEFAULT_EPOCH, DEFAULT_CLARITY_VERSION}` |
| A hardcoded epoch in a message or comparison | `clarity_repl::repl::boot::get_boot_contract_epoch_and_clarity_version(name)` |
| A literal sBTC principal or contract id | `clarity_repl::repl::boot::{SBTC_MAINNET_ADDRESS, SBTC_TESTNET_ADDRESS, SBTC_DEPOSIT_MAINNET_ADDRESS, SBTC_TOKEN_MAINNET_ADDRESS, SBTC_CONTRACTS_NAMES, SBTC_BOOT_CONTRACTS}` |
| Ad-hoc formatting of a Clarity `Value` | `clarity_repl::repl::clarity_values::value_to_string` |
| A hand-rolled HTTP mock in an RPC test | `stacks_rpc_client::mock_stacks_rpc` (the crate's `mock` feature) |
| A version literal for a new external crate | root `[workspace.dependencies]`, kept alphabetical, then `{ workspace = true }` |

Epoch and Clarity-version literals age badly — they move with each Stacks release, and a baked-in one turns a correct message into a wrong one without any gate noticing. Derive them.

## Platform rules for shared crates

`clarity-repl`, `clarinet-files`, `clarinet-deployments`, `clarinet-utils`, `clarinet-defaults`, `clarinet-format`, `clarity-static-cost` and `hiro-system-kit` compile for the CLI **and** for wasm32. When editing any of them:

- **`println!` / `eprintln!` / `print!` / `eprint!` are banned.** They compile on wasm32 and are then silently discarded, so the output vanishes in the SDK and the VSCode extension. Use `clarity_repl::uprint!` / `ueprint!`, which dispatch to `console.log` / `console.error`. CI enforces this via `lints/wasm/clippy.toml`.
- **`std::time::Instant::now()` / `SystemTime::now()` compile on wasm32 and panic there.** Every call in `clarity-repl` sits behind `#[cfg(not(target_arch = "wasm32"))]`; new timing code must too.
- **Never collapse a `#[cfg(target_arch = "wasm32")]` / `cfg(not(...))` pair** to "the obvious one". There are ~100 of these gates on purpose.
- **Native-only crates** — `bitcoin`, `dirs`, `bitcoincore-rpc`, `libsecp256k1`, `stacks-rpc-client` — are declared under `cfg(not(target_arch = "wasm32"))`. Reaching one from an ungated path breaks the wasm build.
- **`clarity-repl`'s default feature is `dap`**, and both wasm consumers build it with `--no-default-features`. Code touching `tokio`, `debug_types` or the DAP server must stay behind `#[cfg(feature = "dap")]`.
- Logic duplicated between `clarinet-cli` and `clarinet-sdk-wasm` (or `clarity-lsp`) belongs in whichever shared crate both already depend on.

## Dependencies

- A new external crate goes in the root `[workspace.dependencies]`, kept alphabetical, and is referenced as `{ workspace = true }`.
- The eight stacks-core deps (`clarity`, `clarity-types`, `libsigner`, `pox-locking`, `stacks-codec`, `stacks-common`, `stacks-transactions`, `stackslib`) share one git `rev`. A partial bump can still compile while producing two incompatible copies of the Clarity types — move all eight together.

## Tests

- `cargo tst` is `nextest run --workspace --no-fail-fast --locked --exclude clarinet-sdk-wasm`. **Tests in `clarinet-sdk-wasm` do not run under it** — they run under `wasm-pack test --node`, and only if they are `#[wasm_bindgen_test]`. A plain `#[test]` there is green because it never executed.
- The alias hardcodes `--workspace`, so `cargo tst -p <crate>` does not filter. Narrow with `cargo nextest run -p <crate> --locked`.
- A new test should fail without the fix. One that passes on both sides of the change proves nothing.
- Put `mod tests` at the end of the file — every tests module in this workspace does.

## Comments

Keep them to the minimum. A comment earns its place by explaining *why*, never by restating the code. A doc comment on a new helper stating its non-obvious contract is welcome.

## Before you finish

`.agents/scripts/clarinet-gates.sh <target>` prints the exact CI commands for the surfaces your change reaches. Formatting is `cargo fmt-stacks`; clippy and the wasm lints are in that list.
