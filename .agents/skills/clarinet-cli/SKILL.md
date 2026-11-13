---
name: clarinet-cli
description: Smoke-test Clarinet changes with temporary Clarity projects and the locally built CLI.
---

# Clarinet CLI

- Build from the repository root with `cargo build`. Use `<repo>/target/debug/clarinet`, not an installed release.
- Create a temporary project with `clarinet new <path>`. Run subsequent commands from its directory containing `Clarinet.toml`.
- Use `clarinet contracts new <name>` and `clarinet requirements add <address>` to scaffold contracts and dependencies.
- Use `pnpm` for TypeScript tests. For local SDK builds and tests, follow `AGENTS.md`.
- Discover other commands and options with `clarinet --help` or `clarinet <command> --help`.

Here, `clarinet` means the locally built binary.
