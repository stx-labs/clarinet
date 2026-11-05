# Clarinet maintenance checks

## Dead code and dependencies

- Previous audits retained SDK `getrandom` for its WASM backend,
  `wasm-bindgen-futures` for generated async bindings, and `colored` for macros.
- Check `stacks-signers` when removing a `stacks-network` dependency. Derive other
  supported target/feature combinations from manifests and CI; `--all-features`
  alone does not cover them.
- The SDK's root pnpm workspace and `components/clarity-vscode` are separate
  workspaces; configure TypeScript scanners accordingly.
- Exclude generated WASM packages, vendored boot contracts in
  `components/clarity-repl/src/repl/boot/`, and `components/stacks-network/data/`
  from routine cleanup.

## Shared behavior

Compare native debug execution with WASM SDK execution when changing transaction
or serialization helpers.

## Performance

Replacing an intermediate JSON `Value` with direct serialization can change object-key
order. Check consumers that compare raw event or cost strings. Metadata caches may
depend on source, Clarity version, epoch, issuer, and configuration; account for
these when hoisting repeated analysis.
