---
name: clarinet-simplify
description: This skill should be used when the user asks to simplify, clean up, DRY, or KISS a change in the Clarinet repo — a branch, a commit, or uncommitted work. It resolves the target diff, runs the generic quality pass, applies the Clarinet-specific reuse and idiom fixes a generic pass cannot know, then verifies against the repo's real CI gates for every surface the change reaches.
argument-hint: "[branch | commit | --staged | --unstaged | --worktree] (default: uncommitted work, else this branch vs main)"
---

# Simplify a Clarinet change

Quality only: reuse, DRY, idiom, efficiency. Correctness bugs are out of scope — those belong to `clarinet-review`.

Success is code a reviewer understands faster, not a smaller diff. `0e0e0900` was a good simplify pass and it *added* 78 net lines — 55 in tests, 23 in source — because naming things costs lines and buys clarity. Do not optimise for the line count.

## 1. Resolve the target and the surfaces it reaches

Pass the user's target argument through verbatim, with no argument at all when they gave none:

```bash
.agents/scripts/clarinet-gates.sh <target>
```

That prints four sections: `TARGET` (what got resolved), `FILES`, `SURFACES` (which of native / SDK-wasm / LSP-wasm / TS the change reaches), and `GATES` (the CI commands that apply). Keep the `GATES` list — step 4 runs it.

If `FILES` is empty, stop and ask what to simplify rather than guessing.

**Preflight.** Drop anything not hand-written source before reading the diff:

- `components/stacks-network/data/*` — devnet snapshot tarballs
- `Cargo.lock`, `pnpm-lock.yaml`, `**/*.tar.gz`
- `components/clarity-repl/src/repl/boot/*.clar` — vendored mainnet contract sources

(The wasm-pack output in `clarinet-sdk-wasm/pkg-node` and `pkg-browser` is gitignored, so it never reaches a diff.)

If nothing survives the preflight, say so and stop.

**If the target is a past commit**, the fixes still land in the working tree — a commit can't be edited in place. Say so in the report so the user can `git commit --fixup` or squash.

## 2. Run the generic quality pass

Use whatever generic quality pass your harness ships, rather than hand-rolling one:

| Harness | Invocation |
| --- | --- |
| Claude Code | `Skill(skill: "simplify", args: "<resolved TARGET>")` |
| Codex | no equivalent — run the reuse/simplification/efficiency pass inline yourself |
| neither | same: run it inline |

It owns the language-agnostic dimensions — reuse, simplification, efficiency, altitude. Do not redo that work by hand where it exists. Where it does not, cover those dimensions yourself before moving on, so step 3 stays additive either way.

A harness-provided pass defaults to the current diff, so when step 1 resolved something else, name the target explicitly.

## 3. Apply the Clarinet pass

The generic pass cannot know any of the following. Check each against the diff.

### Reuse what the repo already has

| Instead of | Use |
| --- | --- |
| A literal `StacksEpochId::…` / `ClarityVersion::…` default | `clarinet_defaults::{DEFAULT_EPOCH, DEFAULT_CLARITY_VERSION}` |
| A hardcoded epoch in a message or comparison | `clarity_repl::repl::boot::get_boot_contract_epoch_and_clarity_version(name)` |
| A literal sBTC principal or contract id | `clarity_repl::repl::boot::{SBTC_MAINNET_ADDRESS, SBTC_TESTNET_ADDRESS, SBTC_DEPOSIT_MAINNET_ADDRESS, SBTC_TOKEN_MAINNET_ADDRESS, SBTC_CONTRACTS_NAMES, SBTC_BOOT_CONTRACTS}` |
| Ad-hoc formatting of a Clarity `Value` | `clarity_repl::repl::clarity_values::value_to_string` |
| `println!` / `eprintln!` / `print!` / `eprint!` | `clarity_repl::uprint!` / `ueprint!` (see the surfaces note below) |
| A hand-rolled HTTP mock in an RPC test | `stacks_rpc_client::mock_stacks_rpc` (the crate's `mock` feature) |
| A version literal for a new external crate | root `[workspace.dependencies]`, kept alphabetical, then `{ workspace = true }` |

Epoch and Clarity-version literals age badly — they move with each Stacks release, and a baked-in one turns a correct message into a wrong one without any gate noticing. Derive them.

### Rust idioms

`CLAUDE.md` has the authoritative list; follow it rather than a second copy here. Two additions that are specific to this codebase:

- A closure returning an iterator, invoked twice just to test emptiness → bind it once and use `.peekable()` + `.peek()`.
- Repeated `format!`-then-`eval_clarity_string` round trips where one Clarity expression would do.

### Keep the three surfaces in mind

Everything under `clarity-repl`, `clarinet-files`, `clarinet-deployments`, `clarinet-utils`, `clarinet-defaults`, `clarinet-format`, `clarity-static-cost` and `hiro-system-kit` compiles for the CLI *and* for wasm32. That constrains the simplification:

- **Never** collapse a `#[cfg(target_arch = "wasm32")]` / `cfg(not(...))` pair to "the obvious one". There are ~100 of these gates on purpose.
- `println!` and friends compile on wasm32 and are then silently discarded, so output vanishes in the SDK and the extension. `lints/wasm/clippy.toml` bans them, and the `disallowed_macros` gate in step 4 is what catches it.
- Native-only crates (`bitcoin`, `dirs`, `bitcoincore-rpc`, `libsecp256k1`, `stacks-rpc-client`) are declared under `cfg(not(target_arch = "wasm32"))`. Moving code that uses them into an ungated shared path breaks the wasm build.
- `clarity-repl`'s default feature is `dap`; the SDK and LSP build it with `--no-default-features`. Code touching `tokio`, `debug_types` or the DAP server must stay behind `#[cfg(feature = "dap")]`.
- Logic duplicated between `clarinet-cli` and `clarinet-sdk-wasm` (or `clarity-lsp`) belongs in whichever shared crate both already depend on — that is the highest-value DRY fix in this repo, and also the riskiest, so verify all three surfaces after it.

### Comments

Keep them to the minimum. A comment earns its place by explaining *why*, never by restating the code. Delete the ones the simplification made redundant. A doc comment on a new helper stating its non-obvious contract is welcome.

## 4. Verify

Run the `GATES` from step 1 in the order printed, stopping at the first failure. They are the same commands CI runs, so a clean pass here means a clean pass there.

When a gate fails, fix the specific cause or revert that one simplification. Never weaken an assertion, loosen a type, or delete a test to make a gate pass. If the target reaches `sdk-ts`, note that its gate needs `pnpm run build:sdk-wasm` first and takes minutes — run it, don't skip it.

Two gotchas worth knowing when a gate fails:

- The `tst` alias hardcodes `--workspace`, so `cargo tst -p <crate>` does **not** filter. Narrow with `cargo nextest run -p <crate> --locked`.
- Devnet snapshot tests read `~/.clarinet/cache/devnet`, and a stale marker file there blocks re-extraction. Wipe that directory before trusting a snapshot-test failure.

## 5. Report

- Applied changes grouped by category (reuse / idiom / efficiency / cross-surface), one line each with `file:line`.
- Findings deliberately skipped, with the reason.
- Gate results, one line per gate.
- If the target was a past commit, the reminder that the fixes are sitting in the working tree.

Do not report a line-count delta as if it were the result.
