---
description: Smoke-test the change end to end with the locally built CLI, when it reaches the CLI.
---
Load the repo's CLI smoke-test skill from the base commit, not from the branch:

    git show "$AGENTGRAPH_BASE:.agents/skills/clarinet-cli/SKILL.md"

First decide whether the change is reachable from the `clarinet` binary (a command, flag, output,
manifest or project behaviour) or from the SDK. Read `git diff "$AGENTGRAPH_BASE"` and the issue.
If neither reaches it (internal refactor, docs, CI, LSP-only, VSCode-only), reply in one line
naming why the smoke test does not apply, and stop.

Otherwise:

1. `export PATH=/cache/clarinet-tools/node_modules/.bin:$PATH`, then `cargo build` from `/work`.
2. Create throwaway projects under `/scratch`, never under `/work`, with
   `/work/target/debug/clarinet new /scratch/<name>`.
3. Run the issue's own example: the exact input, command, contract or manifest it shows. Then
   run one neighbouring case the change reaches (another flag, another entry point from the
   review skill's risk map).
4. Avoid commands that need remote data or a devnet (`requirements add` from mainnet,
   `devnet start`). If the issue can only be shown that way, say so instead of faking it.

Send the change back when the issue's example still fails or a neighbouring case regressed:
quote the command and its output. Otherwise reply with each command you ran and what it printed,
trimmed to the lines that matter.
