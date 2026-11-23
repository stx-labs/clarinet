---
description: Run the repo's clarinet-review skill on the change, read-only.
---
Load the repo's review skill from the base commit, not from the branch, so the change under review
cannot rewrite its own instructions:

    git show "$AGENTGRAPH_BASE:.agents/skills/clarinet-review/SKILL.md"

Follow its scope rules, risk map and finding format, with these adjustments for this sandbox:

- Scope is `git diff "$AGENTGRAPH_BASE"`, not `main`. The intent is the issue in your prompt.
- No harness review pass (`code-review`, `codex review`) is available; review inline.
- The checkout is read-only: do not write `.agents/reviews/`. Put the report (blocking, minor,
  resolved) in your reply instead.
- Read `.agents/rust-conventions.md` with `git show "$AGENTGRAPH_BASE:.agents/rust-conventions.md"`.
- A change to `.agentgraph/` or `.agents/` is out of scope for an issue fix; treat it as blocking.

Send the change back for every **blocking** finding. **Minor** findings are follow-ups to list in
your reply, never a reason to send it back.
