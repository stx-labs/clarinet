# Rust conventions

Write modern, idiomatic Rust. Prefer:

- Iterator chains and combinators over manual loops where they improve clarity
- `if let` / `let else` / `match` over chains of `if`/`else` with `.is_some()`/`.is_ok()` checks
- `?` for error propagation instead of manual `match`/`unwrap`
- Destructuring in function arguments and match arms
- Meaningful type aliases and newtypes where they aid readability
- `impl Into<T>` / `AsRef<T>` parameters for flexible function signatures when appropriate
- `From` / `TryFrom` / `FromStr` impls for conversions, not standalone `to_x` / `parse_x` helpers; implement `From`, not `Into`
- Derive macros (`Clone`, `Debug`, `Default`, etc.) rather than manual implementations
- `Display` for rendering a type, not a `to_string`-style helper
- Inline variables in format strings: `format!("{name}")`, not `format!("{}", name)`
- `indoc!` / `formatdoc!` for multi-line strings
- `strum` derives (`EnumString`, `Display`, `EnumIter`) instead of hand-written enum boilerplate

Let the type system carry correctness:

- `Option` / `Result`, never sentinel values (`-1`, `""`, `u64::MAX`)
- Make invalid states unrepresentable: an enum instead of a struct of flags and optional fields, a newtype that validates on construction

Reuse what exists instead of re-deriving it:

- Anything stacks-core already provides. Import it rather than copying it. If it isn't exposed, say so and suggest the upstream change, instead of duplicating it here.
- Default epoch and Clarity version: `clarinet_defaults::{DEFAULT_EPOCH, DEFAULT_CLARITY_VERSION}`
- A boot contract's epoch: `clarity_repl::repl::boot::get_boot_contract_epoch_and_clarity_version`
- Formatting a Clarity `Value`: `clarity_repl::repl::clarity_values::value_to_string`

Crates shared with the SDK and the LSP compile to wasm32:

- Use `uprint!` / `ueprint!`, not `println!` / `eprintln!`, which print nothing on wasm32.
- Keep `#[cfg(target_arch = "wasm32")]` gates. Collapsing one breaks a build you probably didn't run.

Keep comments short. Explain why, never restate the code.
