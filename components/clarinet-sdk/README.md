# Clarinet SDK Workspace

This workspace regroups
`@stacks/clarinet-sdk` for node.js and `@stacks/clarinet-sdk-browser` for web browsers.  
They respectively rely on `@stacks/clarinet-sdk-wasm` and `@stacks/clarinet-sdk-browser-wasm`.

Because of the way the wasm packages are build, with wasm-pack, it made sense to have two different
packages for Node.js and the browsers, but it has some caveats. Especially, some of the code is
duplicated in `./browser/src/sdkProxy.ts` and `./node/src/sdkProxy.ts`. In the future, we hope to
be able to simplify this build, it would require some breaking changes so it could be part of
Clarinet 3.x.

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
wasm-pack (install [wasm-pack](https://rustwasm.github.io/wasm-pack/installer)).

```sh
# build the wasm package
npm run build:sdk-wasm
# install dependencies and build the node package
npm install
# make sure the installation works
npm test
```

### Release

The Node.js and browser versions can be published with this single command.  
Make sure to check the check both packages versions first.

```sh
# the wasm package must be published first
# $ npm run publish:sdk-wasm
npm run publish:sdk
```
