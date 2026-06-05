# common

Shared TypeScript helpers used by both `@stacks/clarinet-sdk` (node) and `@stacks/clarinet-sdk-browser`.

## Setup

This directory is **not** a published or workspace package — it has no `package.json`. It's a plain shared folder, consumed by relative imports:

```ts
import { ... } from "../../common/src/sdkProxyHelpers.js";
```

Each consumer's `tsconfig` pulls `common/src` into its compilation via `include: ["./src", "../common/src"]`, and emits the common files alongside its own output (`dist/{esm,cjs}/common/src/...`). Runtime dependencies used by common (`@stacks/transactions`) are declared by each consumer.

The `tsconfig.json` in this directory exists for IDE clarity only — it enables consistent type-checking when opening files here directly. It does not participate in the build.

## Trade-offs

**Why this shape:**
- One source of truth for shared logic without the overhead of publishing or bundling.
- No extra build tooling — plain `tsc` covers everything.

**What it costs:**
- Published `dist` paths include the cross-directory layout (e.g. `dist/cjs/node/src/index.js` rather than `dist/index.js`). Cosmetic — consumers see the `exports` map, not the raw paths.
- `rootDir` in each consumer's tsconfig has to span its own directory and `common/`, which is a mild quirk.
- The common code is duplicated across the node and browser dist outputs (once per consumer). At ~one file, this is negligible.

## Potential future improvement

If `common/` grows or a third consumer is added, the cleaner pattern is:

1. Promote `common/` to a real internal workspace package: `@stacks/clarinet-sdk-common`, `private: true`, listed in `pnpm-workspace.yaml`.
2. Have node and browser depend on it via `"workspace:*"` and import it by name.
3. Replace `tsc` emit with a bundler (`tsup`, `unbuild`, etc.) for the two published packages. The bundler inlines `@stacks/clarinet-sdk-common` into each output, so the internal package never gets published.

Result: clean published paths, no relative-import gymnastics, single shared source. Cost: a bundler dependency and a small migration of the build scripts. Not worth it for the current scope.

## Why these helpers exist in the first place

`sdkProxyHelpers.ts` is largely a glue layer over the wasm bindings exported by `@stacks/clarinet-sdk-wasm` / `@stacks/clarinet-sdk-wasm-browser`. The wasm crate today returns several values as JSON strings (events, costs) and uses constructor-style wasm-bindgen APIs (`new CallFnArgs(...)`), so the SDK adds a Proxy + parser layer to give consumers a nicer functional, typed surface.

The helpers fall into three categories:

| Code | Could be pushed into the wasm crate? | Why |
|---|---|---|
| `ExecutionCost`, `ClarityCosts`, `ClarityEvent` shapes + `parseEvents` / `parseCosts` parsers | **Yes** | These are pure mirrors of Rust structs. With `#[wasm_bindgen]` (or `serde-wasm-bindgen`) on the Rust side, the wasm crate could return typed objects instead of JSON strings, and the generated `.d.ts` would carry the types directly. The two parser functions would go away. |
| `CallFn`, `DeployContract`, `MineBlock`, `Execute`, `Tx` union, `tx` builders | **No** | These reference `ClarityValue` from `@stacks/transactions`. Wasm can't import a JS package's types. |
| `ParsedTransactionResult` (the `result: ClarityValue` field in particular) | **Partial** | Half wasm-native (costs, events shell), half JS-native (the `ClarityValue` field). Could become a hybrid where the wasm types are reused but the JS-side wraps in `ClarityValue`. |

### Realistic future work

1. **Move cost/event types into the wasm crate.** Annotate the Rust structs with `#[wasm_bindgen]` or use `serde-wasm-bindgen` so the typed shapes flow through to `.d.ts`. The SDK re-exports them. This eliminates `parseCosts`, `parseEvents`, and three type definitions here — roughly half of `sdkProxyHelpers.ts`.

2. **Bigger lever: kill the Proxy entirely.** The Proxy in `node/src/sdkProxy.ts` (and the browser equivalent) exists because the wasm-bindgen API takes constructor-style arguments (`new CallFnArgs(contract, method, ...)`) and we want consumers to see plain `(contract, method, args, sender)`. If the wasm crate exposed function bindings in the consumer-friendly shape directly, the Proxy machinery — and the `CallFn` / `DeployContract` / `MineBlock` / etc. function-type aliases — could all be deleted. `sdkProxyHelpers.ts` would shrink to just the `tx` builders + `Tx` union.

3. **What stays in JS regardless.** The `tx` builders + `Tx` discriminated union are pure JS convenience for batching transactions; they have no Rust analogue and should stay here. Anything that constructs or consumes `ClarityValue` directly also stays in JS.

Tackling (1) is a low-risk Rust-side typing improvement. (2) is a redesign of the Rust ↔ JS boundary and should be planned more carefully.
