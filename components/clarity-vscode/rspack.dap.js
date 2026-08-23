// @ts-check
// Minimal rspack config to build only the DAP debug adapter (debug/dist/debug.js).
// Use this during development to avoid the full Wasm LSP build.
//   pnpm run build:dap

const path = require("path");

const PRODUCTION = process.env.NODE_ENV === "production";

/** @type {import('@rspack/core').Configuration} */
module.exports = {
  context: path.join(__dirname, "debug"),
  mode: PRODUCTION ? "production" : "none",
  devtool: PRODUCTION ? false : "source-map",
  target: "node",
  entry: { debug: "./debug.ts" },
  output: {
    filename: "[name].js",
    path: path.join(__dirname, "debug", "dist"),
    library: { type: "var", name: "serverExportVar" },
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        use: [
          {
            loader: "builtin:swc-loader",
            options: { jsc: { parser: { syntax: "typescript" } } },
          },
        ],
      },
    ],
  },
};
