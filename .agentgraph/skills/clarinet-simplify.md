---
description: Run the repo's clarinet-simplify skill on the implementor's change, in the sandbox.
---
Load the repo's simplify skill from the base commit, not from the branch, so the change under
review cannot rewrite its own instructions:

    git show "$AGENTGRAPH_BASE:.agents/skills/clarinet-simplify/SKILL.md"

Follow it, with these adjustments for this sandbox:

- Scope is `git diff "$AGENTGRAPH_BASE"`, not `main`.
- No harness `simplify` skill is available; simplify inline.
- Read `.agents/rust-conventions.md` and the checks in `.agents/skills/clarinet-ship/SKILL.md` the
  same way, with `git show "$AGENTGRAPH_BASE:<path>"`.
- Do not touch `.agentgraph/` or `.agents/`.
- If you changed files, run the clarinet-ship checks for the surfaces you touched, then push. If
  nothing needs simplifying, change nothing and say so in one line.
