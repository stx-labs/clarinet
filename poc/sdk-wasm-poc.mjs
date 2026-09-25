// SDK counterpart of components/clarity-repl/examples/wasm_poc.rs.
//   node poc/sdk-wasm-poc.mjs --mode interp|wasm [--iterations 200]
// Writes poc/out/sdk-<mode>.json and merges timings into poc/out/sdk-timings.json.

import fs from "node:fs";
import path from "node:path";
import { createRequire } from "node:module";
import { fileURLToPath, pathToFileURL } from "node:url";
import { performance } from "node:perf_hooks";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const sdkDir = path.join(root, "components/clarinet-sdk/node");
const require = createRequire(path.join(sdkDir, "package.json"));
const { Cl } = await import(pathToFileURL(require.resolve("@stacks/transactions")).href);

const argv = process.argv.slice(2);
const arg = (name, fallback) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 ? argv[i + 1] : fallback;
};
const mode = arg("mode", process.env.CLARINET_CLARITY_WASM === "1" ? "wasm" : "interp");
if (!["interp", "wasm"].includes(mode)) throw new Error(`bad --mode ${mode}`);
const iterations = Number(arg("iterations", 200));
const clarityWasm = mode === "wasm";

const DEPLOYER = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
const WALLET_1 = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
const fixture = path.join(root, "poc/clarity-wasm-fixture");
const outDir = path.join(root, "poc/out");

// Same Devnet accounts as the fixture, no contracts: the scenario deploys them itself like wasm_poc.rs.
const emptyProject = path.join(outDir, "sdk-empty-project");
fs.mkdirSync(path.join(emptyProject, "settings"), { recursive: true });
fs.copyFileSync(
  path.join(fixture, "settings/Devnet.toml"),
  path.join(emptyProject, "settings/Devnet.toml"),
);
fs.writeFileSync(
  path.join(emptyProject, "Clarinet.toml"),
  "[project]\nname = 'sdk-empty-project'\ntelemetry = false\nrequirements = []\n\n[contracts]\n\n[repl.analysis]\npasses = []\n",
);

// The SDK echoes prints/errors to the console; capture them so I/O doesn't skew timings.
const consoleLines = [];
const realLog = console.log;
for (const level of ["log", "info", "warn", "error"]) {
  console[level] = (...a) => consoleLines.push(`[${level}] ${a.map(String).join(" ")}`);
}

const t0 = performance.now();
const { initSimnet } = await import(
  pathToFileURL(path.join(sdkDir, "dist/esm/node/src/index.js")).href
);
const timings = { mode, iterations, sdk_import_ms: performance.now() - t0 };
const options = { trackCosts: true, trackCoverage: false, clarityWasm };

const time = (key, fn) => {
  const start = performance.now();
  const r = fn();
  timings[key] = performance.now() - start;
  return r;
};
const timeAsync = async (key, fn) => {
  const start = performance.now();
  const r = await fn();
  timings[key] = performance.now() - start;
  return r;
};

const reprTuple = (entries) =>
  `(tuple ${Object.keys(entries)
    .sort()
    .map((k) => `(${k} ${repr(entries[k])})`)
    .join(" ")})`;

// Rust `Display for Value`, which is what wasm_poc.rs records for results.
function repr(cv) {
  switch (cv.type) {
    case "int":
      return `${cv.value}`;
    case "uint":
      return `u${cv.value}`;
    case "true":
      return "true";
    case "false":
      return "false";
    case "none":
      return "none";
    case "some":
      return `(some ${repr(cv.value)})`;
    case "ok":
      return `(ok ${repr(cv.value)})`;
    case "err":
      return `(err ${repr(cv.value)})`;
    case "tuple":
      return reprTuple(cv.value);
    case "list":
      return `(list ${cv.value.map(repr).join(" ")})`;
    case "address":
    case "contract":
      return Cl.prettyPrint(cv);
    default:
      return Cl.prettyPrint(cv);
  }
}

const normEvents = (events) =>
  events.map(({ event, data }) =>
    event === "print_event"
      ? {
          contract_identifier: data.contract_identifier,
          topic: data.topic,
          value: Cl.prettyPrint(data.value),
        }
      : { event, data },
  );

function outcome(simnet, fn) {
  try {
    const r = fn();
    return {
      result: repr(r.result),
      events: normEvents(r.events),
      cost: r.costs,
      trace: simnet.getLastContractCallTrace?.() ?? null,
    };
  } catch (e) {
    return { error: String(e?.message ?? e), trace: simnet.getLastContractCallTrace?.() ?? null };
  }
}

// 1. manifest path: the fixture deploys counter + caller from its own plan.
const manifestSimnet = await timeAsync("init_fixture_manifest_ms", () =>
  initSimnet(path.join(fixture, "Clarinet.toml"), true, options),
);
const manifestProbe = {
  epoch: manifestSimnet.currentEpoch,
  "counter::divide u7 u0": outcome(manifestSimnet, () =>
    manifestSimnet.callReadOnlyFn(
      `${DEPLOYER}.counter`,
      "divide",
      [Cl.uint(7), Cl.uint(0)],
      WALLET_1,
    ),
  ),
  "counter::increment": outcome(manifestSimnet, () =>
    manifestSimnet.callPublicFn(`${DEPLOYER}.counter`, "increment", [], WALLET_1),
  ),
};

// 2. scripted scenario, mirroring wasm_poc.rs.
const simnet = await timeAsync("init_empty_manifest_ms", () =>
  initSimnet(path.join(emptyProject, "Clarinet.toml"), true, options),
);
simnet.setEpoch("3.1");

const deploys = [];
time("deploys_ms", () => {
  for (const name of ["counter", "caller"]) {
    const src = fs.readFileSync(path.join(fixture, `contracts/${name}.clar`), "utf8");
    deploys.push({
      contract: name,
      deploy: outcome(simnet, () =>
        simnet.deployContract(name, src, { clarityVersion: 3 }, DEPLOYER),
      ),
    });
  }
});

const counter = `${DEPLOYER}.counter`;
const caller = `${DEPLOYER}.caller`;
const u = Cl.uint;
const p = Cl.principal;
// prettier-ignore
const script = [
  ["public ok",                   counter, "increment",       [],             DEPLOYER],
  ["public ok with arg",          counter, "add",             [u(3)],         DEPLOYER],
  ["public err",                  counter, "add",             [u(11)],        DEPLOYER],
  ["read-only getter",            counter, "get-count",       [],             DEPLOYER],
  ["read-only map getter",        counter, "get-total",       [p(DEPLOYER)],  DEPLOYER],
  ["cross-contract public",       caller,  "bump-counter",    [],             WALLET_1],
  ["cross-contract public err",   caller,  "add-to-counter",  [u(50)],        WALLET_1],
  ["cross-contract public ok",    caller,  "add-to-counter",  [u(4)],         WALLET_1],
  ["cross-contract read-only",    caller,  "read-counter",    [],             WALLET_1],
  ["pox-4 get-pox-info",          caller,  "pox-info",        [],             WALLET_1],
  ["pox-4 read-only with arg",    caller,  "reward-cycle-of", [u(5000)],      WALLET_1],
  ["runtime err unwrap-panic",    caller,  "unwrap-none",     [],             WALLET_1],
  ["runtime err unwrap-panic ro", counter, "panic-none",      [],             WALLET_1],
  ["runtime err div by zero",     caller,  "divide-by-zero",  [u(7)],         WALLET_1],
  ["runtime err div by zero ro",  counter, "divide",          [u(7), u(0)],   WALLET_1],
  ["read-only divide ok",         counter, "divide",          [u(7), u(2)],   WALLET_1],
  ["mutate 1",                    counter, "increment",       [],             WALLET_1],
  ["mutate 2",                    caller,  "bump-counter",    [],             DEPLOYER],
  ["mutate 3",                    counter, "add",             [u(10)],        WALLET_1],
  ["read back count",             counter, "get-count",       [],             DEPLOYER],
  ["read back total deployer",    counter, "get-total",       [p(DEPLOYER)],  DEPLOYER],
  ["read back total wallet_1",    counter, "get-total",       [p(WALLET_1)],  DEPLOYER],
];
const READ_ONLY = ["read-counter", "pox-info", "reward-cycle-of", "divide", "panic-none"];
const isReadOnly = (m) => m.startsWith("get-") || READ_ONLY.includes(m);

const calls = [];
time("script_22_calls_ms", () => {
  for (const [label, contract, method, args, sender] of script) {
    const fn = isReadOnly(method) ? "callReadOnlyFn" : "callPublicFn";
    calls.push({
      label,
      call: `${contract}::${method}`,
      sender,
      outcome: outcome(simnet, () => simnet[fn](contract, method, args, sender)),
    });
  }
});

const snippets = [
  "(contract-call? .counter get-count)",
  "(contract-call? .caller bump-counter)",
  "(print (contract-call? .counter get-total tx-sender))",
].map((snippet) => ({ snippet, outcome: outcome(simnet, () => simnet.execute(snippet)) }));

// Repeated loop runs after the scenario so it doesn't change recorded results.
const loopCosts = { increment: null, "get-count": null };
time("repeat_loop_ms", () => {
  for (let i = 0; i < iterations; i++) {
    const a = simnet.callPublicFn(counter, "increment", [], WALLET_1);
    const b = simnet.callReadOnlyFn(counter, "get-count", [], WALLET_1);
    if (i === 0) {
      loopCosts.increment = a.costs?.total ?? null;
      loopCosts["get-count"] = b.costs?.total ?? null;
    }
  }
});
timings.repeat_loop_per_call_us = (timings.repeat_loop_ms * 1000) / (iterations * 2);
const finalCount = repr(simnet.callReadOnlyFn(counter, "get-count", [], WALLET_1).result);

const out = {
  mode,
  deploys,
  calls,
  snippets,
  manifest_probe: manifestProbe,
  repeat_loop: { iterations, first_iteration_costs: loopCosts, final_count: finalCount },
  console: {
    lines: consoleLines.length,
    unique: [...new Set(consoleLines.map((l) => l.replace(/u\d+/g, "u#")))],
  },
};
fs.writeFileSync(path.join(outDir, `sdk-${mode}.json`), JSON.stringify(out, null, 2) + "\n");

const timingsPath = path.join(outDir, "sdk-timings.json");
const all = fs.existsSync(timingsPath) ? JSON.parse(fs.readFileSync(timingsPath, "utf8")) : {};
all[mode] = timings;
fs.writeFileSync(timingsPath, JSON.stringify(all, null, 2) + "\n");
realLog(JSON.stringify(timings));
