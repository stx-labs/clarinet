---
name: clarinet-review
description: This skill should be used when the user asks to review a change in the Clarinet repo — a branch, a commit, uncommitted work, or a pull request — for correctness. It resolves the target, runs the repo's CI gates for the surfaces the change reaches, delegates the general correctness review to the harness's own review tool, launches an adversarial peer review on a different model vendor, then checks the defect classes that survive a green CI: the CLI, SDK and extension drifting apart, code that compiles for wasm32 but cannot work there, and tests that never run. Reports findings; applies nothing until asked.
argument-hint: "[branch | commit | PR number | --staged | --unstaged] [--level low|medium|high|max] (default: uncommitted work, else this branch vs main)"
---

# Review a Clarinet change

Report-only. Findings are presented and then the user decides — nothing is applied until they say which ones.

The goal is defects whose consequences justify acting, judged against what the change was meant to do. Style opinions are not findings; `cargo fmt-stacks` and clippy already own that.

## 1. Resolve the target and the surfaces it reaches

Pass the user's target argument through verbatim, with no argument at all when they gave none:

```bash
.agents/scripts/clarinet-gates.sh <target>
```

Prints `TARGET`, `FILES`, `SURFACES` and `GATES`. Keep `SURFACES` — it selects which risk classes apply in step 5 — and keep `GATES` for step 4.

A bare number is read as a PR and diffed via `gh pr diff`, with no checkout. The gates then describe the *working tree*, not the PR, and the script says so; either `gh pr checkout` in a worktree first or skip step 4 and say you skipped it.

If `FILES` is empty, stop and ask what to review.

## 2. Launch the adversarial peer

Start this **before** anything else below: it takes minutes, while the rest takes seconds, so it runs while you work.

```bash
# Keyed to the worktree: this repo is worked in several at once, and a fixed
# path would have two concurrent reviews overwrite each other's result.
P="${TMPDIR:-/tmp}/clarinet-peer-$(basename "$(git rev-parse --show-toplevel)")"
rm -f "$P.json" "$P.pid"
<the DIFF command from step 1> > "$P.diff"
.agents/scripts/clarinet-peer-review.sh "$P.diff" \
  .agents/skills/clarinet-review/references/clarinet-risk-map.md \
  "$P.json" &
echo $! > "$P.pid"
```

Record the PID: without it step 7 cannot tell "the peer found nothing" apart from "the peer is still running", and a short review would silently skip it.

The script works out which harness is hosting you and sends the review to a *different vendor* — codex when you are Claude Code, claude when you are Codex — with the repo's agent instructions suppressed so the peer is not primed by the same context you have. It matches on the vendor rather than the CLI, because `pi` drives whichever provider it is pointed at.

`PEER_PREFER=pi` routes to a local model instead: nothing leaves the machine, and it still works when the cloud peer is rate-limited. Its findings are weaker, not stronger — verify them exactly as step 7 says.

It is non-blocking by contract: on a missing CLI, a missing login or a timeout it exits 0 and writes nothing. Collect it in step 7 by testing for the file. Never wait on it before doing your own work, and never let its absence change the rest of the review.

## 3. Establish intent

A defect is a gap between intent and behaviour, so intent has to be pinned down first:

- Commit messages in range, or the PR body and its linked issue (`gh pr view <n>`).
- For a PR, existing review comments — a finding someone already raised is not news.
- `git log -p` on the touched functions when the change interacts with older logic.

Write the intent down in one or two sentences. If the change contradicts its own stated intent, that is the first finding.

## 4. Run the gates

Run the `GATES` list from step 1, in order. These are the exact commands CI runs, so anything they flag is a **confirmed** finding at zero inference cost — and the wasm stdout-macro gate in particular catches a class that is invisible in a plain diff read.

Report a gate failure as a finding with its output attached. If a gate can't run (missing wasm32 target, PR not checked out, `pnpm run build:sdk-wasm` skipped for time), say which one and why rather than implying it passed.

## 5. The Clarinet pass

Read `references/clarinet-risk-map.md` and work its classes against the diff. Collect findings — **do not report them yet.**

The map holds only defects that survive a green CI: the three surfaces drifting apart, code that compiles for wasm32 but cannot work there, and tests that never run. It is short on purpose. If something in the diff worries you and no class covers it, that is a normal outcome — judge it on its merits and say so plainly; the map is a floor, not a checklist that bounds the review.

For Rust diffs, the `rust-conventions` skill (`.agents/skills/rust-conventions/SKILL.md`) is the authority on what the house style actually is. Use it to separate the two kinds of deviation, because they are not equally reportable:

- **Reportable** — a rule with a consequence behind it: a hardcoded epoch that will go stale, a constant re-derived instead of imported, a platform gate that a shared crate needs, a test placed where it will never run.
- **Not reportable** — a preference with no consequence: a manual loop that reads fine, a type alias someone would have named differently. `cargo fmt-stacks` and clippy own formatting, and neither this skill nor that one turns taste into a finding.

Do not re-derive anything a gate in step 4 already reports.

## 6. Delegate the general review

Use whatever general review your harness already ships, rather than hand-rolling one:

| Harness | Invocation |
| --- | --- |
| Claude Code | `Skill(skill: "code-review", args: "<resolved TARGET> [--level <level>]")` |
| Codex | `codex review --base <base>` / `--commit <sha>` / `--uncommitted` — note these act on the **current checkout**, so a target resolved to some other branch needs `git checkout` first, or skip this step and say so |
| neither | run the general pass inline yourself |

It owns general correctness plus reuse, simplification and efficiency. Pass any effort/level argument straight through. Do **not** ask it to apply fixes — this skill is report-only.

If it reports its own findings, let it; you merge in step 7.

## 7. Report once

Present one merged view, most severe first:

- **Gate failures** from step 4 — always confirmed.
- **Clarinet findings** from step 5, each labelled `CONFIRMED` (traced through the code) or `PLAUSIBLE` (consistent with the code, not proven), with `file:line`, the defect in one sentence, and a concrete failure scenario: the input or state that triggers it and what goes wrong.
- **General findings** — already reported by step 6; reference them rather than restating, and drop any that duplicate a Clarinet finding.
- **Peer findings** — first wait for it to finish, then decide:

  ```bash
  for _ in $(seq 1 120); do
    [ -f "$P.pid" ] && kill -0 "$(cat "$P.pid")" 2>/dev/null || break
    sleep 5
  done
  ```

  The peer's own timeout bounds it, and this loop caps the wait at ten minutes regardless. Then: if `$P.json` exists, read it and merge, labelling each as peer-sourced and naming the vendor. Verify each one against the code before repeating it: the peer never ran the gates and cannot see the repo's instructions, so it is the most likely source of a confident-sounding false positive. Drop what does not survive. If the file is absent, say in one line that no peer ran and why, and move on.

If your harness has a structured findings report (Claude Code's `ReportFindings`), it is called at most once per review: if step 6 already used it, put the remaining findings in the response text; if not, use it for the merged list. Otherwise just write them out.

Then stop and ask which to apply. Say plainly when nothing survived verification; a clean review is a result.

## 8. On approval

Apply only the approved findings, then re-run the gates from step 4 and report the results. Never weaken an assertion or delete a test to make a gate pass.

Commits follow Conventional Commits, and PRs land by squash-and-merge.

## 9. Feed the map

This is how the map earns its keep over time. Once a finding is confirmed *and* the fix has landed, offer to append it to the **Observed findings** log in `references/clarinet-risk-map.md`, following the protocol there: one line, with the commit or PR as evidence.

Only findings that no gate caught are worth recording — a gate failure is already permanently encoded in CI. Offer, don't append silently, and never record a finding that was merely plausible.
