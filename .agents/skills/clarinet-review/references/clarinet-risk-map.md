# Clarinet risk map

**Only what survives a green CI.**

This repo's CI gates compile every surface, run the tests, and lint the banned stdout macros. Whatever they catch is theirs to report — don't re-hunt it by hand, and don't list it here. This file exists for defects that a clean gate run still lets through.

It is deliberately short, and it is meant to grow from evidence rather than from anticipation. The recording protocol is at the bottom; read it before adding anything.

## Structural classes

These three follow from how the repo is built, so they apply to any change, not just to changes that resemble a past bug.

### 1. The three surfaces can drift apart

The repo's own framing: three tools — CLI, TypeScript SDK, VSCode extension — built on one set of Rust components. The gates prove each surface *compiles*. Nothing proves they *agree*.

For a change in a shared crate, ask:

- Does a hand-written TypeScript type in `components/clarinet-sdk/common/src/` still match what the Rust side serialises? A renamed serde field or a widened enum compiles on both sides and diverges only at runtime.
- Was the same logic fixed on one surface and left stale on another?
- Does behaviour that the CLI's output or tests depend on still hold?

### 2. Code that compiles on wasm32 but is wrong there

wasm32 has no stdout, no clock and no filesystem, and the compiler objects to less of that than you would expect.

Two demonstrated cases mark the boundary. `println!` compiles and is silently discarded — the repo bans it through `lints/wasm/clippy.toml`, so **the gate owns that one** and you need not look for it. `std::time::Instant::now()` also compiles clean for wasm32 with no gate objecting, which is why every call in `clarity-repl` sits behind `#[cfg(not(target_arch = "wasm32"))]`.

The useful question is not "does it compile" — the gates answer that — but "can this platform actually do it".

### 3. A test that never runs looks exactly like a passing one

`cargo tst` is `--workspace --exclude clarinet-sdk-wasm`. Tests in that crate run only under `wasm-pack test --node`, and only when they are `#[wasm_bindgen_test]`; a plain `#[test]` there is green because it never executed.

Generally, for any new test: does it fail without the fix? A test that passes on both sides of the change is not evidence of anything.

## Observed findings

Entries earn their place by having happened, with a commit or PR as evidence. One occurrence is an entry here — not a class above. Promote an entry to a class only when it recurs in unrelated changes. When a class acquires a CI gate, delete it from this file; the gate owns it from then on.

Format: `date — the defect in one line — evidence — how it surfaced`.

- 2026-08-31 — a user-facing warning hardcoded an epoch that the boot-contract table already derives, so it would go stale at the next epoch bump — `994f48fa` — found in review, no gate would have caught it. Not yet known whether this generalises to "constants tracking an external release cadence" or stays a one-off.

Nothing else is recorded yet, by design. Resist back-filling this from memory or from a recent session: an entry with no commit behind it is a guess, and a guess here costs every future review a wasted lap.
