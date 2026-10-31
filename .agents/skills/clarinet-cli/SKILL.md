---
name: clarinet-cli
description: Use the clarinet CLI to create and manage Clarity smart contract projects on Stacks.
---

# Clarinet CLI

You have access to the `clarinet` CLI — the primary tool for building, testing, and deploying Clarity smart contracts on the Stacks blockchain.

## Core commands

### Project management
- `clarinet new <project-name>` — Create a new Clarinet project with the standard directory structure
- `clarinet contracts new <contract-name>` — Create a new contract within an existing project (generates the `.clar` file and test file)
- `clarinet requirements add <contract-address>` — Add a contract dependency (e.g. a trait or deployed contract your project depends on)

### Testing
- Unit tests are written in TypeScript — use the package manager matching the project's lockfile (`package-lock.json`, `pnpm-lock.yaml`, `yarn.lock`, `bun.lockb`), defaulting to `pnpm` if none is present

### Discovering more commands
- Run `clarinet --help` to see all available commands
- Run `clarinet <command> --help` to see detailed usage for any specific command

## Usage guidelines

- Always run clarinet commands from the **project root** (the directory containing `Clarinet.toml`)
- **Never manually edit `Clarinet.toml` to add contracts or requirements** — always use `clarinet contracts new` and `clarinet requirements add` instead, which handle all necessary scaffolding
- When creating a new project with `clarinet new`, `cd` into the new directory before running further commands
