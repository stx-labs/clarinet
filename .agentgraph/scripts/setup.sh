#!/bin/sh
set -eu
export PATH=/cache/clarinet-tools/node_modules/.bin:$PATH
cargo fetch --locked
node components/clarinet-sdk-wasm/build.mjs --dev
pnpm install --frozen-lockfile --ignore-scripts
pnpm run build:sdk
lode register /work
lode index /work
