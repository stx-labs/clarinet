// @ts-check
"use strict";

const rspack = require("@rspack/core");

const configs = require("./rspack.config");

const [clientBrowserConfig, serverBrowserConfig] = configs;

clientBrowserConfig.plugins = [
  new rspack.DefinePlugin({
    __DEV_MODE__: JSON.stringify(true),
  }),
];

module.exports = [clientBrowserConfig, serverBrowserConfig];
