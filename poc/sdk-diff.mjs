// node poc/sdk-diff.mjs  ->  poc/out/sdk.diff.txt
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const out = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "out");
const load = (name) => JSON.parse(fs.readFileSync(path.join(out, `${name}.json`), "utf8"));

const camel = (c) =>
  c && {
    runtime: c.runtime,
    readCount: c.read_count ?? c.readCount,
    readLength: c.read_length ?? c.readLength,
    writeCount: c.write_count ?? c.writeCount,
    writeLength: c.write_length ?? c.writeLength,
  };

// Bring native (serialize_event / snake_case costs / bare errors) and SDK outcomes to one shape.
function norm(o = {}) {
  if (o.error !== undefined) {
    return { error: o.error.replace(/^Call contract function error: .*? -> /, ""), trace: o.trace ?? null };
  }
  return {
    result: o.result,
    events: (o.events ?? []).map((e) => {
      const ce = e.contract_event ?? e;
      return `${ce.contract_identifier} ${ce.topic} ${ce.value}`;
    }),
    cost: JSON.stringify(camel(o.cost?.total) ?? null),
    trace: o.trace ?? null,
  };
}

function entries(run) {
  const m = new Map();
  for (const d of run.deploys) m.set(`deploy ${d.contract}`, { ...norm(d.deploy), result: undefined });
  for (const c of run.calls) m.set(c.label, { ...norm(c.outcome), trace: c.trace ?? c.outcome.trace ?? null, sender: c.sender, call: c.call });
  for (const s of run.snippets) m.set(`snippet ${s.snippet}`, { ...norm(s.outcome), cost: undefined, trace: undefined });
  return m;
}

function compare(title, aName, bName, fields) {
  const a = entries(load(aName));
  const b = entries(load(bName));
  const lines = [`## ${title}: ${aName} vs ${bName}`];
  const counts = Object.fromEntries(fields.map((f) => [f, 0]));
  let same = 0;
  for (const [label, x] of a) {
    const y = b.get(label);
    if (!y) {
      lines.push(`MISSING in ${bName}: ${label}`);
      continue;
    }
    const diffs = fields.filter((f) => JSON.stringify(x[f]) !== JSON.stringify(y[f]));
    if (!diffs.length) {
      same++;
      continue;
    }
    lines.push(`* ${label}`);
    for (const f of diffs) {
      counts[f]++;
      lines.push(`    ${f}:\n      ${aName}: ${JSON.stringify(x[f])}\n      ${bName}: ${JSON.stringify(y[f])}`);
    }
  }
  for (const label of b.keys()) if (!a.has(label)) lines.push(`MISSING in ${aName}: ${label}`);
  const summary = `summary: ${a.size} entries, ${same} identical; differing field counts ${JSON.stringify(counts)}`;
  return [lines[0], summary, ...lines.slice(1), ""].join("\n");
}

const all = ["sender", "call", "result", "error", "events", "cost", "trace"];
const report = [
  compare("SDK wasm vs native wasm (same engine, different host)", "native-wasm", "sdk-wasm", all),
  compare("SDK interp vs native interp (sanity)", "native-interp", "sdk-interp", all),
  compare("SDK wasm vs SDK interp (engine switch)", "sdk-interp", "sdk-wasm", all),
  `## Explanations
- native vs SDK (both engines): results, errors, events and costs are identical. The one trace diff is
  pox-4 get-pox-info showing burn-block-height u2 (native) vs u3 (SDK): the SDK session sits one burn
  block higher (initSession + setEpoch("3.1") + deployContract advancing the tip), unrelated to the engine.
- SDK interp vs SDK wasm, all expected and identical to native-interp vs native-wasm:
  - cost: clarity-wasm only charges the contract-call/load cost (runtime 869 or 727, read_count 3), no
    per-expression, data-var or map read/write costs. Deploy costs are all zero in wasm mode.
  - error: runtime errors carry an empty stack \`Some([])\` instead of the interpreter's FunctionIdentifier stack.
  - trace: the eval-hook based trace only sees the top-level call in wasm; print events and nested
    contract-call frames into wasm contracts are missing. Calls into pox-4 (interpreted boot contract)
    still show pox-4 frames. The interpreter's duplicated final line for cross-contract calls is also absent.
- snippet and deploy results are the same across all four runs (SDK reports deploy results as \`true\`,
  native as \`None\`; deploy results are excluded from the comparison).
`,
].join("\n");
fs.writeFileSync(path.join(out, "sdk.diff.txt"), report);
console.log(report.split("\n").filter((l) => l.startsWith("##") || l.startsWith("summary")).join("\n"));
