// @ts-check
"use strict";
/** @typedef {import('@rspack/core').Configuration} RspackConfig **/

const path = require("path");
const rspack = require("@rspack/core");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const PRODUCTION = process.env.NODE_ENV === "production";

/** @type RspackConfig["mode"] */
const mode = PRODUCTION ? "production" : "none";
/** @type RspackConfig["devtool"] */
const devtool = PRODUCTION ? false : "source-map";

const swcLoader = {
  test: /\.ts$/,
  exclude: /node_modules/,
  use: [
    {
      loader: "builtin:swc-loader",
      options: {
        jsc: {
          parser: {
            syntax: "typescript",
          },
        },
      },
    },
  ],
};

// `.vscodeignore` allowlists exact files, so any extra chunk the bundler
// emitted would silently be left out of the package. Never split chunks.
const asyncChunks = false;

const browserOutput = {
  filename: "[name].js",
  path: path.join(__dirname, "client", "dist"),
  library: { type: "commonjs" },
  asyncChunks,
};

const browserResolve = {
  extensions: [".ts", ".js"],
  fallback: { path: require.resolve("path-browserify") },
};

/** @type RspackConfig */
const clientBrowserConfig = {
  context: path.join(__dirname, "client"),
  mode,
  devtool,
  target: "webworker",
  entry: {
    clientBrowser: "./src/clientBrowser.ts",
    "tests/suite/index": "./tests/suite/index.ts",
  },
  output: browserOutput,
  resolve: browserResolve,
  plugins: [
    new rspack.DefinePlugin({
      __DEV_MODE__: JSON.stringify(false),
    }),
  ],
  module: { rules: [swcLoader] },
  externals: { vscode: "commonjs vscode" },
};

/** @type RspackConfig */
const clientNodeConfig = {
  context: path.join(__dirname, "client"),
  mode,
  devtool,
  target: "node",
  entry: { clientNode: "./src/clientNode.ts" },
  output: browserOutput,
  resolve: browserResolve,
  plugins: [
    new rspack.DefinePlugin({
      __DEV_MODE__: JSON.stringify(false),
    }),
  ],
  module: { rules: [swcLoader] },
  externals: { vscode: "commonjs vscode" },
};

const serverOutput = {
  filename: "[name].js",
  path: path.join(__dirname, "server", "dist"),
  library: { type: "var", name: "serverExportVar" },
  asyncChunks,
};

/** @type RspackConfig */
const serverBrowserConfig = {
  context: path.join(__dirname, "server"),
  mode,
  devtool,
  target: "webworker",
  entry: { serverBrowser: "./src/serverBrowser.ts" },
  output: serverOutput,
  resolve: { extensions: [".ts", ".js"] },
  plugins: [
    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, "../clarity-lsp"),
      extraArgs: "--release --target=web",
      outDir: path.resolve(__dirname, "server/src/clarity-lsp-browser"),
      outName: "lsp-browser",
    }),
    // `serverBrowser.ts` fetches the Wasm over HTTP rather than importing it.
    // The only module-graph reference is `new URL()` in wasm-bindgen's unused
    // init glue, which production tree-shaking removes, so copy the file
    // explicitly instead of relying on it being emitted as an asset.
    new rspack.CopyRspackPlugin({
      patterns: ["./src/clarity-lsp-browser/lsp-browser_bg.wasm"],
    }),
  ],
  module: {
    rules: [
      swcLoader,
      {
        // Don't turn that same `new URL()` into a second, hashed copy of the
        // Wasm in non-production builds, where the glue survives. Scoped to the
        // glue so the rest of the bundle keeps normal `new URL()` assets.
        test: /clarity-lsp-browser[\\/]lsp-browser\.js$/,
        parser: { url: false },
      },
    ],
  },
};

/** @type RspackConfig */
const serverNodeConfig = {
  context: path.join(__dirname, "server"),
  mode,
  devtool,
  target: "node",
  entry: { serverNode: "./src/serverNode.ts" },
  output: serverOutput,
  resolve: { extensions: [".ts", ".js"] },
  plugins: [
    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, "../clarity-lsp"),
      extraArgs: "--release --target=nodejs",
      outDir: path.resolve(__dirname, "server/src/clarity-lsp-node"),
      outName: "lsp-node",
    }),
    new rspack.CopyRspackPlugin({
      patterns: ["./src/clarity-lsp-node/lsp-node_bg.wasm"],
    }),
  ],
  module: { rules: [swcLoader] },
};

/** @type RspackConfig */
const dapNodeConfig = {
  context: path.join(__dirname, "debug"),
  mode,
  devtool,
  target: "node",
  entry: { debug: "./debug.ts" },
  output: {
    filename: "[name].js",
    path: path.join(__dirname, "debug", "dist"),
    library: { type: "var", name: "serverExportVar" },
    asyncChunks,
  },
  module: { rules: [swcLoader] },
};

module.exports = [
  clientBrowserConfig,
  serverBrowserConfig,
  clientNodeConfig,
  serverNodeConfig,
  dapNodeConfig,
];
