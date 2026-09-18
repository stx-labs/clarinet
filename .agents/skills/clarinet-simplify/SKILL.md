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

That prints four sections: `TARGET` (what got resolved), `FILES`, `SURFACES` (which build surfaces the change reaches), and `GATES` (the CI commands that apply). Keep the `GATES` list — step 4 runs it.

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

### Settle its contradictions before a fix lands

Those dimensions are judged independently, so two of them will sometimes reach **opposite conclusions about the same mechanism** — one proposing an abstraction another says is unwarranted. That is not a duplicate to dedup; dedup keeps whichever arrived first. It is a question, and this skill applies what it accepts, so it has to be settled against the code rather than by picking.

Count the call sites yourself and check the captures and lifetimes, not the shape. A finding that claims *N duplicated sites* is the most likely one to be wrong, because the shape matches long before the constraints do. On #2551 one angle found four copies of `thread_named(…).spawn(move || create_basic_runtime().block_on(…))` and wanted a helper for them; two of the four held the runtime across a loop or two branches, and a third passed borrowed closure-locals that no `F: Future + Send + 'static` helper can accept. Two real sites is not an abstraction.

## 3. Apply the Clarinet pass

The generic pass cannot know any of the following. Check each against the diff.

### Follow the repo's Rust conventions

Read the `rust-conventions` skill (`.agents/skills/rust-conventions/SKILL.md`) and apply it to the diff. It is the single source for the style rules, the constants and helpers to reuse instead of re-deriving, the platform rules for the crates that compile to both native and wasm32, and the test gotchas — all of which a generic quality pass has no way to know.

Two things it says that matter most during a simplify pass:

- **Reuse before you rewrite.** Epoch and Clarity-version literals, sBTC principals, Clarity value formatting and RPC mocks all have canonical sources; a simplification that leaves a literal in place has missed the point.
- **Never simplify away a platform gate.** Collapsing a `cfg(target_arch)` pair or moving native-only code into a shared path is the one way a quality pass can break the build on a surface you did not run.

### Comments

`rust-conventions` covers the rule; the simplify-specific part is to **delete the comments the simplification made redundant**.

## 4. Verify

Run the `GATES` from step 1 in the order printed, stopping at the first failure. They are the same commands CI runs, so a clean pass here means a clean pass there.

When a gate fails, fix the specific cause or revert that one simplification. Never weaken an assertion, loosen a type, or delete a test to make a gate pass. If the target reaches `sdk-ts`, note that its gate needs `pnpm run build:sdk-wasm` first and takes minutes — run it, don't skip it.

One gotcha the conventions do not cover: devnet snapshot tests read `~/.clarinet/cache/devnet`, and a stale marker file there blocks re-extraction. Wipe that directory before trusting a snapshot-test failure.

## 5. Report

- Applied changes grouped by category (reuse / idiom / efficiency / cross-surface), one line each with `file:line`.
- Findings deliberately skipped, with the reason.
- Gate results, one line per gate.
- If the target was a past commit, the reminder that the fixes are sitting in the working tree.

