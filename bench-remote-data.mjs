#!/usr/bin/env node
// Benchmark for the SDK remote-data HTTP path (curl shellout on main vs
// worker-thread sync HTTP on feat/replace-curl-shellout-with-worker-thread).
//
// Drives the real path end-to-end through the built SDK: every timed
// operation triggers one or more synchronous HTTP calls to the Stacks API
// from inside the wasm runtime.
//
// Usage:
//   node bench-remote-data.mjs [iterations] [atBlockCalls]
//
//   iterations    full cold-cache rounds (default 5)
//   atBlockCalls  `get-count-at-block` calls per round, each at a distinct
//                 height so each one fetches fresh data (default 10)
//
// Requires the SDK to be built on the current branch:
//   pnpm run build:sdk-wasm && pnpm run build:sdk
//
// Set HIRO_API_KEY to avoid rate limiting. Results are printed and written
// to bench-results.<branch>.json next to this file for cross-branch diffing.

import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { execSync } from "node:child_process";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import { performance } from "node:perf_hooks";

const SCRIPT_DIR = path.dirname(fileURLToPath(import.meta.url));
const ITERATIONS = Number.parseInt(process.argv[2] ?? "5", 10);
const AT_BLOCK_CALLS = Number.parseInt(process.argv[3] ?? "10", 10);

// Same fixture the SDK's remote-data tests use.
const API_URL = "https://api.testnet.hiro.so";
const COUNTER = "STJCAB2T9TR2EJM7YS4DM2CGBBVTF7BV237Y8KNV.counter";
const SENDER = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";
const INITIAL_HEIGHT = 57_000;
// Heights for at-block calls; each distinct height forces fresh fetches.
const AT_BLOCK_BASE = 56_230;
const AT_BLOCK_MAX = 56_990;

function git(cmd) {
  try {
    return execSync(`git ${cmd}`, { cwd: SCRIPT_DIR, encoding: "utf-8" }).trim();
  } catch {
    return "unknown";
  }
}

const BRANCH = git("rev-parse --abbrev-ref HEAD");
const COMMIT = git("rev-parse --short HEAD");

// Load the built SDK package (dist + workspace node_modules).
const SDK_DIR = path.join(SCRIPT_DIR, "components/clarinet-sdk/node");
const SDK_ENTRY = path.join(SDK_DIR, "dist/cjs/node/src/index.js");
if (!fs.existsSync(SDK_ENTRY)) {
  console.error(
    `SDK build not found at ${SDK_ENTRY}\n` +
      "Build it first: pnpm run build:sdk-wasm && pnpm run build:sdk",
  );
  process.exit(1);
}
const require = createRequire(import.meta.url);
const { getSDK } = require(SDK_ENTRY);
const { Cl } = require(path.join(SDK_DIR, "node_modules/@stacks/transactions"));

// Run from a temp dir: the remote-data fs cache lands in <cwd>/.cache, and we
// want to control it without touching the repo.
const WORK_DIR = fs.mkdtempSync(path.join(os.tmpdir(), "clarinet-bench-"));
process.chdir(WORK_DIR);
const CACHE_DIR = path.join(WORK_DIR, ".cache");

function wipeCache() {
  fs.rmSync(CACHE_DIR, { recursive: true, force: true });
}

function stats(samples) {
  if (samples.length === 0) return null;
  const sorted = [...samples].sort((a, b) => a - b);
  const pick = (q) => sorted[Math.min(sorted.length - 1, Math.floor(q * sorted.length))];
  const total = sorted.reduce((a, b) => a + b, 0);
  return {
    count: sorted.length,
    min: sorted[0],
    median: pick(0.5),
    p95: pick(0.95),
    max: sorted[sorted.length - 1],
    mean: total / sorted.length,
    total,
  };
}

const timings = { init: [], coldCall: [], atBlock: [] };
const errors = [];

function timed(bucket, fn) {
  const t0 = performance.now();
  const value = fn();
  const ms = performance.now() - t0;
  if (bucket) timings[bucket].push(ms);
  return { value, ms };
}

async function runIteration(simnet, { measure }) {
  wipeCache();

  const t0 = performance.now();
  await simnet.initEmptySession({
    enabled: true,
    api_url: API_URL,
    initial_height: INITIAL_HEIGHT,
  });
  if (measure) timings.init.push(performance.now() - t0);

  // Cold call: fetches contract source, metadata, marf data.
  const cold = timed(measure ? "coldCall" : null, () =>
    simnet.callReadOnlyFn(COUNTER, "get-count", [], SENDER),
  );
  if (cold.value.result.type !== "uint") {
    throw new Error(`unexpected get-count result: ${JSON.stringify(cold.value.result)}`);
  }

  // Spread at-block calls across distinct heights — each needs a block hash
  // lookup plus data fetches at that height.
  const step = Math.max(1, Math.floor((AT_BLOCK_MAX - AT_BLOCK_BASE) / Math.max(1, AT_BLOCK_CALLS)));
  for (let k = 0; k < AT_BLOCK_CALLS; k++) {
    const height = AT_BLOCK_BASE + k * step;
    timed(measure ? "atBlock" : null, () =>
      simnet.callReadOnlyFn(COUNTER, "get-count-at-block", [Cl.uint(height)], SENDER),
    );
  }
}

function fmt(ms) {
  return ms >= 1000 ? `${(ms / 1000).toFixed(2)}s` : `${ms.toFixed(1)}ms`;
}

function printStats(label, s) {
  if (!s) return;
  console.log(
    `  ${label.padEnd(22)} n=${String(s.count).padStart(3)}  ` +
      `min=${fmt(s.min).padStart(8)}  median=${fmt(s.median).padStart(8)}  ` +
      `mean=${fmt(s.mean).padStart(8)}  p95=${fmt(s.p95).padStart(8)}  ` +
      `max=${fmt(s.max).padStart(8)}  total=${fmt(s.total)}`,
  );
}

console.log(`branch=${BRANCH} commit=${COMMIT} node=${process.version}`);
console.log(
  `api=${API_URL} iterations=${ITERATIONS} atBlockCalls=${AT_BLOCK_CALLS} ` +
    `apiKey=${process.env.HIRO_API_KEY ? "yes" : "NO (risk of 429s)"}`,
);

const simnet = await getSDK();
const benchStart = performance.now();

// Warmup round (unmeasured): wasm instantiation, worker spawn, DNS/TLS warm.
process.stdout.write("warmup… ");
await runIteration(simnet, { measure: false });
console.log("done");

for (let i = 1; i <= ITERATIONS; i++) {
  process.stdout.write(`iteration ${i}/${ITERATIONS}… `);
  const t0 = performance.now();
  try {
    await runIteration(simnet, { measure: true });
    console.log(fmt(performance.now() - t0));
  } catch (e) {
    errors.push({ iteration: i, message: e?.message ?? String(e) });
    console.log(`FAILED: ${e?.message ?? e}`);
  }
}

const wallMs = performance.now() - benchStart;
const summary = {
  branch: BRANCH,
  commit: COMMIT,
  node: process.version,
  apiUrl: API_URL,
  iterations: ITERATIONS,
  atBlockCalls: AT_BLOCK_CALLS,
  date: new Date().toISOString(),
  wallMs,
  errors,
  stats: {
    initEmptySession: stats(timings.init),
    coldReadOnlyCall: stats(timings.coldCall),
    atBlockCall: stats(timings.atBlock),
  },
  samples: timings,
};

console.log(`\nresults (${BRANCH} @ ${COMMIT}):`);
printStats("initEmptySession", summary.stats.initEmptySession);
printStats("cold callReadOnlyFn", summary.stats.coldReadOnlyCall);
printStats("at-block call", summary.stats.atBlockCall);
console.log(`  wall time: ${fmt(wallMs)}  errors: ${errors.length}`);

const outFile = path.join(SCRIPT_DIR, `bench-results.${BRANCH.replace(/[^\w.-]+/g, "-")}.json`);
fs.writeFileSync(outFile, JSON.stringify(summary, null, 2));
console.log(`written: ${outFile}`);

wipeCache();
fs.rmSync(WORK_DIR, { recursive: true, force: true });
process.exit(errors.length > 0 ? 1 : 0);
