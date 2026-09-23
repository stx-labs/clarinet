---
name: clarinet-review
description: Review a change in the Clarinet repo for correctness — a branch, a commit, uncommitted work or a pull request. Use when asked to review changes, review a PR, or check work before marking a PR ready. Reports findings without editing code.
---

# Review a Clarinet change

Find defects in the change. Read-only: don't edit code unless the user then asks for fixes. To confirm a finding, you may write a throwaway test on a temporary branch or worktree, but remove it and leave no lasting changes. Never post to GitHub.

## Scope

Default to uncommitted work plus the current branch against `main`. If the user names a branch, commit, range or PR number, review that instead. For a PR, read its description and existing review comments first, so you don't repeat what has already been raised.

Pin down the intent from the PR description or commit messages. A change that contradicts its own stated intent is the first finding.

## Review

Run your harness's general review pass if it's available to you: the `code-review` skill in Claude Code, `codex review` in Codex (it reviews the current checkout). Otherwise, review inline. Then check the risk map below.

Report only defects the change introduces or exposes. Formatting and lint belong to `cargo fmt-stacks` and clippy. Report violations of `.agents/rust-conventions.md` as minor.

Check each finding against the surrounding code before reporting it. Drop the ones that don't hold up.

## Risk map

Defects that passed CI here and were caught only in review. Add an entry only once a finding has been confirmed and fixed, citing the PR.

- **A rule added at one entry point misses the others.** The same session is reached through the console's `::` commands, `simnet.execute`, `runSnippet`, the DAP, the SDK and deployment plans. When a change adds a rule (remap an id, rewrite a source, validate an argument), list every entry point and check each one. The one that was missed is unchanged code, so it won't be in the diff. Missed in `::encode` (#2518).
- **A hardcoded epoch goes stale.** Epoch and Clarity version literals change with Stacks releases. Derive them (see `.agents/rust-conventions.md`). A warning with a baked-in epoch (#2513).

## Report

Group the findings: **blocking**, then **minor**, then **resolved** since the last review. For each, give `file:line`, the defect in one sentence, and the input or state that triggers it. If there are no findings, say so.

Write the report to `.agents/reviews/<name>.md` (gitignored), with `<name>` set to `PR-<number>` or the branch name with `/` replaced by `-`. If the file already exists, update it: move fixed findings to resolved and add new ones. That way the next run, in any harness, picks up where this one stopped.
