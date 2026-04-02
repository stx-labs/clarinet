// @ts-check
"use strict";
/** @typedef {import('@rspack/core').Configuration} RspackConfig **/

const path = require("path");
const rspack = require("@rspack/core");
const WasmPackPlugin = require("@wasm-tool/wasm-pack-plugin");

const { name, publisher, version } = require("./package.json");

const PRODUCTION = process.env.NODE_ENV === "production";
const TEST = process.env.NODE_ENV === "test";

/** @type RspackConfig["mode"] */
const mode = PRODUCTION ? "production" : "none";
/** @type RspackConfig["devtool"] */
const devtool = PRODUCTION ? false : "source-map";

let extensionURL = `https://${publisher}.vscode-unpkg.net/${publisher}/${name}/${version}/extension/`;
if (TEST) extensionURL = "http://localhost:3001/static/devextensions/";

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

const browserOutput = {
  filename: "[name].js",
  path: path.join(__dirname, "client", "dist"),
  libraryTarget: "commonjs",
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
  libraryTarget: "var",
  library: "serverExportVar",
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
    new rspack.DefinePlugin({
      __EXTENSION_URL__: JSON.stringify(extensionURL),
    }),
    new WasmPackPlugin({
      crateDirectory: path.resolve(__dirname, "../clarity-lsp"),
      forceMode: "production",
      extraArgs: "--release --target=web",
      outDir: path.resolve(__dirname, "server/src/clarity-lsp-browser"),
      outName: "lsp-browser",
    }),
  ],
  module: {
    rules: [
      swcLoader,
      {
        test: /src\/clarity-lsp-browser\/lsp-browser_bg\.wasm$/,
        type: "asset/resource",
        generator: { filename: "lsp-browser_bg.wasm" },
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
      forceMode: "production",
      extraArgs: "--release --target=nodejs",
      outDir: path.resolve(__dirname, "server/src/clarity-lsp-node"),
      outName: "lsp-node",
    }),
    new rspack.CopyRspackPlugin({
      patterns: [
        {
          from: "./src/clarity-lsp-node/lsp-node_bg.wasm",
          to: path.join(__dirname, "server", "dist"),
        },
      ],
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
    libraryTarget: "var",
    library: "serverExportVar",
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
