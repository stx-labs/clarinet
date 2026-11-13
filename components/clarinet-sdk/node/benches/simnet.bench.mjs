#!/usr/bin/env node
// Simnet throughput benchmark for the built SDK package (Wasm + JS glue).
//
// Mirrors `components/clarity-repl/benches/simnet.rs`, but runs through the
// published SDK, so it measures the Wasm build that users actually get. Use it
// to compare build settings (opt-level, LTO, ...) between two builds.
//
// Usage (from this package's directory, after `pnpm run build:sdk-wasm` and
// `pnpm run build:sdk`):
//   node benches/simnet.bench.mjs [--quick] [--json <file>] [--filter <substring>]

import fs from "node:fs";
import path from "node:path";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";
import { parseArgs } from "node:util";

const { values: args } = parseArgs({
  options: {
    quick: { type: "boolean", default: false },
    json: { type: "string" },
    filter: { type: "string" },
  },
});

const packageDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
process.chdir(packageDir);

const manifestPath = "tests/fixtures/Clarinet.toml";
const deploymentPlanPath = "tests/fixtures/deployments/default.simnet-plan.yaml";

// Loading the SDK compiles and instantiates the Wasm module, so time it too.
const loadStart = performance.now();
const { initSimnet } = await import("@stacks/clarinet-sdk");
const { Cl } = await import("@stacks/transactions");

const removeDeploymentPlan = () => fs.rmSync(deploymentPlanPath, { force: true });
removeDeploymentPlan();

let simnet = await initSimnet(manifestPath);
const coldStartMs = performance.now() - loadStart;

const sender = simnet.getAccounts().get("wallet_1");
const LIST_LEN = 500;
const items = Array.from({ length: LIST_LEN }, (_, i) => `u${i}`).join(" ");

const contractSource = `
(define-data-var buff-data (buff 32) 0x01)
(define-map history uint (buff 32))

(define-read-only (noop-ro (i uint) (d (buff 32)))
  (ok true))

(define-public (noop-pub (i uint) (d (buff 32)))
  (ok true))

(define-public (save (i uint) (d (buff 32)))
  (begin
    (map-insert history i d)
    (ok (var-set buff-data d))))

(define-read-only (read-ab (height uint))
  (at-block (unwrap-panic (get-stacks-block-info? id-header-hash height)) (var-get buff-data)))

;; simplified fibonacci that only stores the value for the current block height
;; (modulo a prime, so it can run for more than ~185 iterations without overflow)
(define-data-var current uint u1)
(define-constant deployed-at stacks-block-height)
(define-public (fib)
  (let ((previous (if (<= (- stacks-block-height u2) deployed-at)
    u0
    (at-block
      (unwrap! (get-stacks-block-info? id-header-hash (- stacks-block-height u2)) (err u1))
      (var-get current)))))
    (ok (var-set current (mod (+ previous (var-get current)) u1000000007)))))

;; CPU-bound: interpreter overhead (fold + arithmetic)
(define-constant ITEMS (list ${items}))
(define-private (arith-step (i uint) (acc uint))
  (+ (mod (* acc u31) u1000000007) (* i i) u7))
(define-read-only (arith)
  (fold arith-step ITEMS u1))

;; CPU-bound: native hashing
(define-private (hash-step (i uint) (acc (buff 32)))
  (sha256 (concat acc (sha256 i))))
(define-read-only (hash-chain)
  (fold hash-step ITEMS 0x00))
`;

/** Start a fresh session with the bench contract deployed. */
async function freshSession() {
  simnet = await initSimnet(manifestPath);
  const { result } = simnet.deployContract("bench", contractSource, null, simnet.deployer);
  if (result.type !== "ok") throw new Error(`deploy failed: ${JSON.stringify(result)}`);
  return simnet;
}

const u = (i) => Cl.uint(i);
const buff = (i) => {
  const b = new Uint8Array(4);
  new DataView(b.buffer).setUint32(0, i);
  return Cl.buffer(b);
};
const expectType = (res, type) => {
  if (res.result.type !== type) throw new Error(`unexpected result: ${JSON.stringify(res.result)}`);
  return res;
};
const expectOk = (res) => expectType(res, "ok");

// `iters` operations per round; ns/op is recorded per round.
const scale = args.quick ? 0.2 : 1;
const benches = [
  {
    // Uncached init: reads the existing deployment plan, regenerates it and compares
    // (YAML parse + emit), then deploys the fixture contracts. Without `noCache`,
    // `initSimnet` would only clone the session cached by the first call.
    name: "initSimnet (existing plan)",
    iters: 3,
    rounds: 10,
    setup: async () => {},
    run: async () => {
      await initSimnet(manifestPath, true);
    },
  },
  {
    name: "read-only noop",
    iters: 500,
    rounds: 15,
    setup: freshSession,
    run: (i) => expectOk(simnet.callReadOnlyFn("bench", "noop-ro", [u(i), buff(i)], sender)),
  },
  {
    name: "public noop (mines a block)",
    iters: 300,
    rounds: 15,
    setup: freshSession,
    run: (i) => expectOk(simnet.callPublicFn("bench", "noop-pub", [u(i), buff(i)], sender)),
  },
  {
    name: "save (map-insert + var-set)",
    iters: 300,
    rounds: 15,
    setup: freshSession,
    run: (i) => expectOk(simnet.callPublicFn("bench", "save", [u(i), buff(i)], sender)),
  },
  {
    name: "save + fib (at-block)",
    iters: 150,
    rounds: 15,
    setup: freshSession,
    run: (i) => {
      simnet.callPublicFn("bench", "save", [u(i), buff(i)], sender);
      expectOk(simnet.callPublicFn("bench", "fib", [], sender));
    },
  },
  {
    name: "save + read-ab (at-block, early height)",
    iters: 150,
    rounds: 15,
    // Reads the deployment block, which the first `save` makes historical.
    setup: async () => {
      await freshSession();
      return simnet.blockHeight;
    },
    run: (i, height) => {
      simnet.callPublicFn("bench", "save", [u(i), buff(i)], sender);
      expectType(simnet.callReadOnlyFn("bench", "read-ab", [u(height)], sender), "buffer");
    },
  },
  {
    name: `arith fold (${LIST_LEN} items)`,
    iters: 40,
    rounds: 15,
    setup: freshSession,
    run: () => expectType(simnet.callReadOnlyFn("bench", "arith", [], sender), "uint"),
  },
  {
    name: `sha256 chain (${LIST_LEN} items)`,
    iters: 40,
    rounds: 15,
    setup: freshSession,
    run: () => expectType(simnet.callReadOnlyFn("bench", "hash-chain", [], sender), "buffer"),
  },
];

const median = (xs) => {
  const s = [...xs].sort((a, b) => a - b);
  const m = s.length >> 1;
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
};
const fmt = (ns) =>
  ns >= 1e6
    ? `${(ns / 1e6).toFixed(2)} ms`
    : ns >= 1e3
      ? `${(ns / 1e3).toFixed(1)} µs`
      : `${ns.toFixed(0)} ns`;

const results = { coldStartMs, benches: {} };
console.log(`cold start (import + first initSimnet): ${coldStartMs.toFixed(0)} ms\n`);
console.log(
  `${"benchmark".padEnd(42)} ${"median".padStart(10)} ${"min".padStart(10)} ${"spread".padStart(7)}`,
);

try {
  for (const b of benches) {
    if (args.filter && !b.name.includes(args.filter)) continue;
    const iters = Math.max(1, Math.round(b.iters * scale));
    const rounds = args.quick ? 5 : b.rounds;
    const ctx = await b.setup();
    let i = 0;
    // warm-up round (JIT, caches)
    for (let k = 0; k < iters; k++) await b.run(i++, ctx);

    const samples = [];
    for (let r = 0; r < rounds; r++) {
      const start = performance.now();
      for (let k = 0; k < iters; k++) await b.run(i++, ctx);
      samples.push(((performance.now() - start) * 1e6) / iters);
    }
    const med = median(samples);
    const spread = (Math.max(...samples) - Math.min(...samples)) / med;
    results.benches[b.name] = {
      medianNs: med,
      minNs: Math.min(...samples),
      samplesNs: samples,
      iters,
    };
    console.log(
      `${b.name.padEnd(42)} ${fmt(med).padStart(10)} ${fmt(Math.min(...samples)).padStart(10)} ${`${(spread * 100).toFixed(0)}%`.padStart(7)}`,
    );
  }
} finally {
  removeDeploymentPlan();
}

if (args.json) fs.writeFileSync(args.json, JSON.stringify(results, null, 2));
