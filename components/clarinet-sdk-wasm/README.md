# Clarity SDK WASM

This component exposes Clarinet features to a JS interface through wasm-bindgen. It's built with
wasm-pack.  
It powers [@stacks/clarinet-sdk](https://npmjs.com/package/@stacks/clarinet-sdk) and
[@stacks/clarinet-sdk-browser](https://npmjs.com/package/@stacks/clarinet-sdk-browser).

## Contributing

### Build package

Install [wasm-pack](https://rustwasm.github.io/wasm-pack/installer/).

In the root directory of Clarinet, run the following command to build the packages for Node.js and
the browser. Under the hood, it will run `wasm-pack build` twice, once for each target.

```sh
npm run build:sdk-wasm
```

Alternatively, it's also possible to build the packages separately. It should only be done for
development purpose.

**Build for node**

```sh
wasm-pack build --release --scope stacks --out-dir pkg-node --target nodejs
```

**Build for the browser**

```sh
wasm-pack build --release --scope stacks --out-dir pkg-browser --target web
```

### Release

The package is built twice with `wasm-pack` as it can't target `node` and `web` at the same time.
The following script will build for both target, it will also rename the package name for the
browser build.

```sh
npm run build:sdk-wasm
```

Once built, the packages can be released by running the following command. Note that by default we
release with the beta tag.

```sh
npm run publish:sdk-wasm
```
