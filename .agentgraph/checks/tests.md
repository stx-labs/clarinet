---
mode: block
run: >-
  export PATH=/cache/clarinet-tools/node_modules/.bin:$PATH;
  pnpm --version &&
  cargo test --locked --test cli test_check &&
  node components/clarinet-sdk-wasm/build.mjs --dev &&
  pnpm run build:sdk &&
  pnpm --filter ./components/clarinet-sdk/node run test:types &&
  pnpm --filter ./components/clarinet-sdk/node run test:no-mxs
---
Run the CLI check tests, rebuild the WASM and TypeScript SDKs, check SDK types,
and run the offline SDK suite. Remote-data tests require external services and
remain a separate CI check.
