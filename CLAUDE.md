# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Clarinet is a development toolkit for building, testing, and deploying Clarity smart contracts on the Stacks blockchain. It provides a CLI, REPL, testing framework, debugger, and local devnet environment.

## Build Commands

```bash
# Build CLI (debug)
cargo build

# Build CLI (release)
cargo build --release

# Install CLI to system path
cargo clarinet-install

# Build WASM SDK (required before SDK tests)
npm run build:sdk-wasm

# Build TypeScript SDK
npm run build:sdk
```

## Testing

```bash
# Run all Rust tests (uses cargo-nextest)
cargo tst

# Run single test
cargo tst -p <crate-name> -- <test-name>

# Run tests with coverage (LCOV output)
cargo cov

# Run tests with HTML coverage report
cargo cov-dev

# Run SDK tests
npm test
```

## Linting and Formatting

```bash
# Format code (Stacks style with grouped imports)
cargo fmt-stacks

# Check formatting without modifying
cargo fmt-stacks --check

# Run clippy
cargo clippy --workspace --exclude clarinet-sdk-wasm

# Run clippy for WASM target
cargo clippy --package clarinet-sdk-wasm --target wasm32-unknown-unknown
```

## Architecture

This is a Cargo workspace with 14 Rust crates in `components/`:

**CLI & User Interfaces:**
- `clarinet-cli` - Main CLI binary (default member)
- `clarity-repl` - Interactive REPL with DAP debugger support
- `clarity-lsp` - Language Server Protocol for IDE integration

**SDK (WASM + TypeScript):**
- `clarinet-sdk-wasm` - Rust core compiled to WebAssembly
- `clarinet-sdk` - TypeScript SDK with `node` and `browser` packages (not a workspace member)

**Project Management:**
- `clarinet-files` - Manifest parsing (Clarinet.toml, project structure)
- `clarinet-deployments` - Contract deployment orchestration
- `clarinet-format` - Clarity code formatter

**Blockchain Integration:**
- `stacks-network` - Local devnet orchestration (Docker-based)
- `stacks-rpc-client` - Stacks node HTTP client
- `chainhook-sdk` - Event indexing and webhooks
- `chainhook-types` - Bitcoin/Stacks data schemas

**Serialization:**
- `stacks-codec` - Stacks wire format encoding/decoding

**Utilities:**
- `clarinet-utils` - Cryptographic utilities
- `clarity-events` - Event processing
- `hiro-system-kit` - Cross-platform system helpers

**Plugins & Extensions (not workspace members):**
- `clarity-vscode` - TypeScript/WASM extension for Microsoft Visual Studio Code
- `clarity-jupyter-kernel` - Jupyter kernel for Clarity (currently unmaintained)

## Key Dependencies

Clarity interpreter and Stacks libraries come from `stacks-network/stacks-core` git dependency (see `Cargo.toml` for current revision).

## SDK Development

For VSCode users working on the SDK, open the workspace file at `components/clarinet-sdk/clarinet-sdk.code-workspace` which configures rust-analyzer for WASM targets.

## Conventions

- Follow [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/#summary) for commit messages
- PRs merge via "squash and merge"
- Rust stable toolchain (>= 1.89.0), Node >= v24.4.1

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
