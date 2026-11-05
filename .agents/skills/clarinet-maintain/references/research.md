# Research on autonomous repository maintenance

Assessed November 4, 2026 for Clarinet at `cc739bd9`, on `feat/repo-cleanup`.

A small Clarinet-specific skill is worthwhile. Claude Code already supplies useful
cleanup, review, and continuation commands; Codex supplies review and unattended
execution. The additional value is selecting appropriate repository work, respecting
recorded decisions, validating all affected surfaces, and leaving a useful handoff.
Keep those rules local and use the existing tools for execution.

## Existing skills and workflows

These assessments come from the linked implementation instructions and license
files. They describe suitability for this repository, not comparative bug-finding
benchmarks. Repository links below pin the inspected revisions. Workflow licensing
and model execution costs are separate questions.

| Candidate | License at inspected revision | Useful capabilities | Fit for Clarinet |
| --- | --- | --- | --- |
| [Trail of Bits Code Improver](https://github.com/trailofbits/skills/blob/470419e16f97e112d01fc32f4898aea1755015d9/plugins/code-improver/README.md) | [CC BY-SA 4.0](https://github.com/trailofbits/skills/blob/470419e16f97e112d01fc32f4898aea1755015d9/LICENSE) | Review/fix iteration, persistent findings, scope checks, regression evidence, bounded rounds, and detection of non-converging fixes. | Closest reusable improvement loop, especially for Claude-only use. Its implementation requires Claude's dynamic Workflow tool and an installed reviewer; its documentation explicitly excludes Codex. The generic entry also requires the user to name a reviewer and confirm scope. It could support a later Claude-specific workflow, but does not replace a shared maintenance policy. |
| [Autonomous Maintainer for Codex](https://github.com/tkgo11/codex-autonomous-maintainer/blob/452c0d740c35db7a5f68e13d7eddbeb203ac467e/standalone/SKILL.md) | [MIT](https://github.com/tkgo11/codex-autonomous-maintainer/blob/452c0d740c35db7a5f68e13d7eddbeb203ac467e/LICENSE.md) | Broad repository discovery, implementation, verification, resumable state, and a standalone variant without OMX. | Most direct Codex alternative. Defaults include proactive features, aggressive replacement, checkpoint commits, and PR preparation. It can be configured conservatively with `feature_policy=off rewrite_policy=surgical compatibility=public-contract commit=false delivery=none`, but its substantial lifecycle and bookkeeping exceed this need. An option if comprehensive transformation becomes the goal. |
| [Desloppify](https://github.com/peteromallet/desloppify/blob/3a7735d531a96b6a226bfbdc9fd662b14195f857/README.md) | [OSNL 0.2](https://github.com/peteromallet/desloppify/blob/3a7735d531a96b6a226bfbdc9fd662b14195f857/LICENSE) | Mechanical and subjective analysis, persistent prioritized work, dead code, duplication, complexity, and Rust/TypeScript support. | Strong functional match, with a larger scanner and scoring system. Its current license allows free internal use but restricts some commercial redistribution. Do not present it as an unrestricted open-source option. Its default skill also directs commits and periodic pushes; adopting it would require adapting the workflow. |
| [ECC refactor-cleaner](https://github.com/affaan-m/ECC/blob/b38be6a7f6350964d59328b52f0624e1d1d552d1/agents/refactor-cleaner.md) and [refactor-clean](https://github.com/affaan-m/ECC/blob/b38be6a7f6350964d59328b52f0624e1d1d552d1/commands/refactor-clean.md) | [MIT](https://github.com/affaan-m/ECC/blob/b38be6a7f6350964d59328b52f0624e1d1d552d1/LICENSE) | Tool-assisted dead-code/dependency detection, reference checks, and deletion followed by tests; the command includes Rust tooling. | Useful cleanup material, with a JS-heavy agent. The agent directs commits after batches, while the command recommends whole-file `git checkout --` rollback. Adapt before using in a dirty checkout. Neither supplies the full bug-discovery and maintenance-selection workflow. |
| [jerseycheese code-health-audit](https://github.com/jerseycheese/agent-skills/blob/a24160af576642723de29f30ebab1ba503f5af0a/skills/code-health-audit/SKILL.md) and [dead-code-cleanup](https://github.com/jerseycheese/agent-skills/blob/a24160af576642723de29f30ebab1ba503f5af0a/skills/dead-code-cleanup/SKILL.md) | [MIT](https://github.com/jerseycheese/agent-skills/blob/a24160af576642723de29f30ebab1ba503f5af0a/LICENSE) | Validates suspected unused code, examines dynamic/public consumers, and remembers declined audit suggestions. | Sensible checks, but the audit switches to and updates the integration branch, asks for confirmation, and proceeds to PR delivery. Cleanup explicitly requests approval before deletion and directs commits. These defaults suit an interactive workflow better than an unattended pass on this branch. |
| [agent-sh deslop](https://github.com/agent-sh/deslop/blob/dd00c5431f012ee9d62c2efff55aa770c74e080e/skills/deslop/SKILL.md) | [Package declares MIT](https://github.com/agent-sh/deslop/blob/dd00c5431f012ee9d62c2efff55aa770c74e080e/package.json); no root LICENSE file in the inspected tree | Detects stale references, documentation drift, duplicates, complexity, and cleanup candidates; supports repository/path scope and model confirmation. | Useful optional detector. The skill explicitly excludes logic bugs, races, and missed edge cases, and returns findings for its caller to apply. It needs its plugin scripts and confirmation configuration or manual confirmation. Not a complete overnight maintainer. |
| [Anthropic code-simplifier](https://github.com/anthropics/claude-plugins-official/blob/fa4eb54397ee2faccb6940512a19c017cb1337f3/plugins/code-simplifier/agents/code-simplifier.md) and [PR Review Toolkit](https://github.com/anthropics/claude-plugins-official/blob/fa4eb54397ee2faccb6940512a19c017cb1337f3/plugins/pr-review-toolkit/README.md) | [Apache 2.0](https://github.com/anthropics/claude-plugins-official/blob/fa4eb54397ee2faccb6940512a19c017cb1337f3/plugins/code-simplifier/LICENSE), also [toolkit license](https://github.com/anthropics/claude-plugins-official/blob/fa4eb54397ee2faccb6940512a19c017cb1337f3/plugins/pr-review-toolkit/LICENSE) | Simplification plus focused reviewers for errors, tests, comments, types, and correctness. | Good optional specialists. Their default focus is recently changed code or a PR; the standalone simplifier also includes JS/React preferences that need local interpretation. Current bundled Claude commands already cover much of the relevant work. |
| [Superpowers](https://github.com/obra/superpowers/blob/22f0b463e7a5e579904dd6211a520321035526f7/README.md) | [MIT](https://github.com/obra/superpowers/blob/22f0b463e7a5e579904dd6211a520321035526f7/LICENSE) | Composable debugging, verification, review, and implementation workflows across multiple harnesses. | Good general development methodology; individual debugging and verification skills can help. The overall workflow starts with design clarification and approval, rather than autonomously selecting low-risk maintenance. Installing the full methodology would add more process than this gap requires. |
| [steipete maintainer-orchestrator](https://github.com/steipete/agent-scripts/blob/d210c0a77c8f9ae5e1d939b13238a75e87cdb69a/skills/maintainer-orchestrator/SKILL.md) | [MIT](https://github.com/steipete/agent-scripts/blob/d210c0a77c8f9ae5e1d939b13238a75e87cdb69a/LICENSE) | Coordinates an ongoing queue across issues, PRs, or repositories. | A broader maintainer control workflow, including GitHub actions and owner-specific standing authority. Useful for managing a contribution portfolio; excessive for improving one local checkout. Its author's authority statements must not be transplanted into this repository. |

Compiler and Clippy diagnostics remain useful starting points. For additional
dependency detection, [cargo-machete](https://github.com/bnjbvr/cargo-machete) is
MIT-licensed and explicitly documents imprecision, generated-code false positives,
and renamed-crate handling. [Knip's workspace support](https://knip.dev/features/monorepos-and-workspaces)
can help analyze the TypeScript packages, provided entry points and the separate
extension workspace are represented correctly. Neither tool's output establishes
that deleting a public interface or a feature-enabling dependency is safe.

## Built-in capabilities

### Claude Code

The installed CLI reports **2.1.318**. Current official documentation describes:

- `/simplify [target]`: applies cleanup findings concerning reuse, simplification,
  efficiency, and abstraction. It does not look for correctness bugs.
- `/code-review ... --fix`: finds correctness bugs and can apply fixes; it accepts
  diff, PR, branch, and path targets. `/review` is an alias.
- `/goal`: continues across turns toward a completion condition.
- `/batch`: splits large changes into independent worktrees.

These are meaningful built-ins, not reasons to recreate generic reviewers.
[Claude command reference](https://code.claude.com/docs/en/commands).

Bare `/loop` is especially relevant: its default prompt continues unfinished work,
tends the branch's PR, then runs maintenance such as bug hunts or simplification.
An explicit prompt can invoke a project skill. `.claude/loop.md` can replace the
default prompt; recurring session tasks require the session to remain running and
expire after seven days. This is sufficient continuation machinery for a Claude
maintenance session. No loop or schedule was configured as part of this change.
[Claude scheduled tasks](https://code.claude.com/docs/en/scheduled-tasks).

**Assessment:** the built-ins are sufficient for individual cleanup and bug-review
passes. For Clarinet, add the small local policy that tells the agent what work to
choose, which ideas are already declined, and what native/WASM/SDK proof it needs.

### Codex

The installed CLI reports **0.165.0**. Its local help confirms `codex review`,
`codex exec`, `--uncommitted`, `--base`, `--commit`, and non-interactive output
options. Review is a useful findings step; `exec` can run an implementation
workflow. The CLI and official command documentation provide no dedicated general
repository-cleanup subcommand analogous to Claude's `/simplify`.
[Codex developer commands](https://developers.openai.com/codex/cli/slash-commands),
[non-interactive execution](https://developers.openai.com/codex/noninteractive).

Codex also documents `/goal` for persistent objectives. Scheduled tasks can invoke
skills, so the workflow can be reused without maintaining a separate shell loop.
Scheduling and persistence do not themselves define maintenance priorities or
prove a patch correct.
[Goal commands](https://developers.openai.com/codex/cli/slash-commands),
[scheduled tasks](https://developers.openai.com/codex/app/automations).

Codex discovers repository skills in `.agents/skills` and supports symlinked skill
folders. Claude Code supports project skills under `.claude/skills`; this checkout
already has `.claude/skills -> ../.agents/skills`. One standard `SKILL.md` can serve
both harnesses without installing a plugin or copying instructions.
[Codex skills](https://developers.openai.com/codex/skills),
[Claude skills](https://code.claude.com/docs/en/skills).

## Clarinet-specific decisions

The existing skills leave a narrow gap: `clarinet-simplify` cleans up a selected
change, `clarinet-review` reports defects introduced or exposed by a change, and
`clarinet-ship` handles checks and delivery. `clarinet-maintain` discovers and fixes
pre-existing problems throughout the requested scope, reusing those checks and
review workflows where useful.

The TODO notes demonstrate why selection matters. Unused dependencies, malformed
input panics, repeated UTXO removals, and redundant serialization are plausible
maintenance candidates once confirmed. Requirement-cache identity and SDK reset
semantics can require a compatibility decision. Completed fixes and retained
dependency false positives must not become new overnight work merely because a
scanner flags them again.

The new skill therefore uses coherent batches, evidence before removal, regressions
for bugs, equivalent-output checks for refactors, measurements for performance
claims, and the existing native/WASM/extension validation matrix. It records
deferred decisions and continues independent work. It leaves changes uncommitted
unless instructed otherwise. The instructions are original repository guidance;
no third-party skill implementation or tooling was vendored.

## Invocation examples

Codex:

```text
Use $clarinet-maintain for an overnight pass on this branch until 07:00 UTC.
Prioritize confirmed bugs and unused dependencies. Do not commit.
```

Claude Code:

```text
/clarinet-maintain Focus on formatter and LSP robustness. Work for up to two hours.
Leave changes uncommitted and record anything that needs a design decision.
```

For repeated Claude execution, supply an explicit maintenance prompt to `/loop`.
For Codex, use the skill in a goal or scheduled-task prompt. Set a concrete time or
token budget for an unattended run; a skill file alone is not an execution service.

## Validation and limits

The skill's frontmatter and naming pass the bundled `skill-creator` validator.
Relative links and the existing Claude symlink are checked locally. Manual scenario
review covers a fresh checkout without private TODO notes, retained WASM feature
dependencies, public exports without local callers, stale audit results, ambiguous
cache policy, overlapping user edits, unavailable WASM checks, and a time-limited
handoff. These are instruction checks, not observed autonomous execution results.

No maintenance run was performed against the repository, and the candidate tools
were not installed or benchmarked against Clarinet. Actual overnight effectiveness
will depend on the model, environment, budget, and review of the resulting patches.
