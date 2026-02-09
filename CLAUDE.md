# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# Project Overview

Clarinet is a development toolkit for building, testing, and deploying Clarity smart contracts on the Stacks blockchain. It provides a CLI, REPL, testing framework, debugger, and local devnet environment.

This repository contains three tools htat are built on top of the same Rust components
- CLI
- TypeScript SDK
- VSCode Extension (LSP wrapper)

# Architecture

## Core Rust Components

- `clarity-repl` - Interactive REPL with DAP debugger support
- `clarity-lsp` - Language Server Protocol for IDE integration
- `clarinet-files` - Manifest parsing (Clarinet.toml, project structure)
- `clarinet-deployments` - Contract deployment orchestration
- `clarinet-format` - Clarity code formatter
- `stacks-network` - Local devnet orchestration (Docker-based)
- `stacks-rpc-client` - Stacks node HTTP client
- `stacks-codec` - Stacks wire format encoding/decoding
- `clarinet-utils` - Cryptographic utilities
- `hiro-system-kit` - Cross-platform system helpers

## CLI

- `clarinet-cli` - Main CLI binary. Default member of `./cargo.toml`

### Commands

```bash
# Build
cargo build

# Run all Rust tests (uses cargo-nextest)
cargo tst

# Run single test
cargo tst -p <crate-name> -- <test-name>

# Run clippy
cargo clippy --workspace --exclude clarinet-sdk-wasm

# Format code
cargo fmt-stacks

# Check formatting
cargo fmt-stacks --check
```

## SDK (Wasm + TypeScript)

- `clarinet-sdk-wasm` - Rust core built on top of the core components and CLI, compiled to WebAssembly
- `clarinet-sdk` - TypeScript wrapper around `clarinet-sdk-wasm`

Exposes some of the CLI features thanks to Wasm, and contains the Clarity unit testing framework.
The NPM workspace is configureed in the root `./package.json`

### Commands

```bash
# Compile the Wasm bindgen (required before SDK tests)
npm run build:sdk-wasm

# Build the SDK
npm run build:sdk

# Run all SDK tests
npm test -w components/clarinet-sdk/node

# Run tests matching a pattern
npm test -w components/clarinet-sdk/node -- tests/<filename>.test.ts -t <test name pattern>

# Run clippy
cargo clippy --package clarinet-sdk-wasm --target wasm32-unknown-unknown
```

## VSCode extension

- `clarity-vscode` - TypeScript/WASM extension for Microsoft Visual Studio Code

A wrapper around the `clarity-lsp`, compiled to Wasm to produced a self-contained VSCode extension binary.
It has it's own `package.json` in `./components/clarity-vscode/package.json`

## Build Commands

The commands have to run in the `components/clarity-vscode`
```bash
# Build the extension
npm run vsce:package

# Run end-to-end tests
npm test
```

## Key Dependencies

Clarity interpreter and Stacks libraries come from `stacks-network/stacks-core` git dependency (see `Cargo.toml` for current revision).

## Conventions

- Follow [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/#summary) for commit messages
- PRs merge via "squash and merge"

## Rust Style

Write modern, idiomatic Rust. Prefer:
- Iterator chains and combinators over manual loops where they improve clarity
- `if let` / `let else` / `match` over chains of `if`/`else` with `.is_some()`/`.is_ok()` checks
- `?` for error propagation instead of manual `match`/`unwrap`
- Destructuring in function arguments and match arms
- Meaningful type aliases and newtypes where they aid readability
- `impl Into<T>` / `AsRef<T>` parameters for flexible function signatures when appropriate
- Implement `From<T>` (not `Into<T>`) for type conversions; the blanket impl provides `Into` for free
- Derive macros (`Clone`, `Debug`, `Default`, etc.) rather than manual implementations
- Standard library traits (`From`, `Display`, `FromStr`) for type conversions
