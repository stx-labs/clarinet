# Clarinet SDK Workspace

This workspace regroups
`@stacks/clarinet-sdk` for node.js and `@stacks/clarinet-sdk-browser` for web browsers.  
They respectively rely on `@stacks/clarinet-sdk-wasm` and `@stacks/clarinet-sdk-wasm-browser`.

The Wasm packages are built separately for Node.js and browsers. Both SDKs share
`./common/src/sdkProxy.ts`, with small adapters in `./node/src/sdkProxy.ts` and
`./browser/src/sdkProxy.ts`.

## Contributing

The clarinet-sdk requires a few steps to be built and tested locally.

Clone the clarinet repo and `cd` into it:

```sh
git clone git@github.com:stx-labs/clarinet.git
cd clarinet
```

Open the SDK workspace in VSCode, it's especially useful to get rust-analyzer
to consider the right files with the right cargo features.

```sh
code components/clarinet-sdk/clarinet-sdk.code-workspace
```

The SDK mainly relies on two components:

- the Rust component: `components/clarinet-sdk-wasm`
- the TS component: `components/clarinet-sdk`

To work with these two packages locally, the first one needs to be built with
wasm-pack (install [wasm-pack](https://wasm-bindgen.github.io/wasm-pack/installer)).

```sh
# install dependencies without running prepare scripts
# (the SDK packages' `prepare` hook depends on the Wasm build,
# which doesn't exist yet on a fresh clone)
pnpm install --ignore-scripts
# build the Wasm package
pnpm run build:sdk-wasm
# install dependencies and build the node package
pnpm install
# make sure the installation works
pnpm test
```

### Release

The Node.js and browser versions can be published with this single command.
Check both package versions first.

```sh
# the Wasm package must be published first
# $ pnpm run publish:sdk-wasm
pnpm run publish:sdk
```
