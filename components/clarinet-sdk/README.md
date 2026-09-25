# Clarinet SDK Workspace

This workspace regroups
`@stacks/clarinet-sdk` for node.js and `@stacks/clarinet-sdk-browser` for web browsers.  
They respectively rely on `@stacks/clarinet-sdk-wasm` and `@stacks/clarinet-sdk-browser-wasm`.

Because of the way the wasm packages are build, with wasm-pack, it made sense to have two different
packages for Node.js and the browsers, but it has some caveats. Especially, some of the code is
duplicated in `./browser/src/sdkProxy.ts` and `./node/src/sdkProxy.ts`. In the future, we hope to
be able to simplify this build, it would require some breaking changes so it could be part of
Clarinet 3.x.

## Transaction behavior

Simnet public calls, deployments, and native STX transfers use the shared
stacks-core Clarity transaction frame. Included execution failures and
post-condition aborts roll back payload writes but consume the sender's nonce;
rejected transactions consume no nonce. Existing convenience calls do not charge
transaction fees.

Deployments use stacks-node's initializer. A successful `deployContract` returns
`Cl.ok(Cl.bool(true))`, regardless of the initializer's final expression, and
stores the contract even if it defines no functions. Duplicate contract names
are rejected before analysis without consuming a nonce. Use read-only calls,
`getDataVar`, and events to check initialization effects. Use `execute` when you
want to evaluate a snippet and receive its expression value. Deployment costs
include contract storage and subsequent calls account for the stored contract's
actual data size, so cost assertions may need updating.

`transferSTX` represents a native transfer: self-transfers, zero-amount transfers,
and transfers exceeding the available unlocked balance throw without consuming a
nonce. This differs
from calling the Clarity `stx-transfer?` function inside a contract, which retains
its ordinary response semantics. Private calls remain a simnet convenience and
use the same transaction frame.

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
# (the SDK packages' `prepare` hook depends on the wasm build,
# which doesn't exist yet on a fresh clone)
pnpm install --ignore-scripts
# build the wasm package
pnpm run build:sdk-wasm
# install dependencies and build the node package
pnpm install
# make sure the installation works
pnpm test
```

### Release

The Node.js and browser versions can be published with this single command.
Make sure to check the check both packages versions first.

```sh
# the wasm package must be published first
# $ pnpm run publish:sdk-wasm
pnpm run publish:sdk
```
