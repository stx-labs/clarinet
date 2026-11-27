---
on: { issues: { label: agent:ready }, issue_comment: {} }
branch: agentgraph/{issue}
steps:
  implement:
    agent: implementor
    tries: 3
    checks: [tests]
  simplify:
    agent: simplifier
    skill: clarinet-simplify
    carry: true
    tries: 2
    checks: [tests]
  e2e:
    agent: smoketester
    skill: clarinet-e2e
    carry: true
    back: implement
    rounds: 1
    until: the issue's own example works with the locally built CLI, or the change does not reach the CLI or SDK
  review:
    agent: reviewer
    skill: clarinet-review
    back: implement
    rounds: 1
    until: the change does everything the issue asks, nothing it does not need, and has no blocking finding
  merge: { wait: human, back: implement }
---
One issue becomes one pull request. The implementor changes the repo until the tests pass and opens
the pull request. The simplifier tidies the change with clarinet-simplify and pushes, keeping the
tests green. When the change reaches the CLI or SDK, the smoke tester runs the issue's example with
the locally built binary and can send it back once. The reviewer runs clarinet-review on the result
and can send it back once. A human merges.
