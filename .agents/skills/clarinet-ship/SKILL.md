---
name: clarinet-ship
description: Get a Clarinet change ready to land — run the checks CI will run, commit, push and open the pull request. Use when asked to commit, push, open a PR or mark one ready, and for the repo's git conventions.
---

# Ship a Clarinet change

Commit, push and open PRs only when the user asks.

## Checks

CI runs every job on every PR. Locally, run the checks for the surfaces the diff reaches. If you can't tell which surfaces it reaches, run them all.

Any Rust change:

```bash
cargo fmt-stacks --check
cargo clippy --workspace --tests --exclude clarinet-sdk-wasm
cargo tst
```

Crates in the SDK's dependency graph (most shared crates):

```bash
cargo clippy --package clarinet-sdk-wasm --target wasm32-unknown-unknown
CLIPPY_CONF_DIR=$PWD/lints/wasm cargo clippy --no-default-features --lib --target wasm32-unknown-unknown -p clarinet-sdk-wasm -- -A clippy::all -D clippy::disallowed_macros
wasm-pack test --node components/clarinet-sdk-wasm
pnpm run build:sdk-wasm && pnpm --filter ./components/clarinet-sdk/node run test
```

`cargo tst` skips `clarinet-sdk-wasm`. Its tests run only under `wasm-pack test`, and only if they are `#[wasm_bindgen_test]`.

`clarity-lsp` or `components/clarity-vscode`:

```bash
cargo clippy --target=wasm32-unknown-unknown --no-default-features --package=clarity-lsp
pnpm --dir components/clarity-vscode run lint
pnpm --dir components/clarity-vscode run test:server
```

`components/clarity-vscode` is its own pnpm workspace, so it needs `pnpm --dir components/clarity-vscode install --frozen-lockfile` first.

Report which checks ran and which didn't. A skipped check is not a passing one.

## Git conventions

- Commit messages and PR titles follow [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/#summary): `fix:`, `feat:`, `chore:`, `refactor:`, `docs:`, `test:`, `perf:`.
- PRs are squash-merged, so the PR title becomes the commit on `main`. Make it the one that matters.
- Name branches with the same prefixes, e.g. `fix/short-description`.
- Fill in `.github/PULL_REQUEST_TEMPLATE.md`. Keep the description short and written for the reviewer.

## Before marking ready for review

If `clarinet-simplify` and `clarinet-review` haven't been run on the latest changes, offer to run them.
