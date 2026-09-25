# POC report: clarity-wasm (wasmi) in Clarinet Simnet

Written 2026-09-25. Covers the native CLI and the JS SDK on Node.

## Setup

The stacks-core and clarity-wasm branches are local and unpushed, and the root `Cargo.toml` points at them through absolute-path `[patch]` blocks.

| Repo         | Branch                          | Commit      | Base                                                                                                                                       |
| ------------ | ------------------------------- | ----------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| Clarinet     | `feat/clarity-wasm-wasmi-poc`   | this branch | main 2692fda1                                                                                                                              |
| stacks-core  | `feat/clarity-wasm-on-275c8f86` | 7e1de7b325  | 275c8f8611, runtime from `melcar-stacks/stacks-core` `wasmi` 1c5512ee6f ([#7662](https://github.com/stacks-network/stacks-core/pull/7662)) |
| clarity-wasm | `feat/compile-only`             | 252fb87e    | `wasmi` 12d26f43 ([#870](https://github.com/stx-labs/clarity-wasm/pull/870))                                                               |

Reproduce:

- Native: `cargo run -p clarity-repl --features clarity-wasm --example wasm_poc`, then again with `CLARINET_CLARITY_WASM=1`. Outputs and diff are in `poc/out/native-*`.
- SDK: `CLARINET_SDK_FEATURES=clarity-wasm pnpm run build:sdk-wasm`, `pnpm run build:sdk`, then `node poc/sdk-wasm-poc.mjs --mode interp` and `--mode wasm`, then `node poc/sdk-diff.mjs`. Outputs are in `poc/out/sdk-*`.
- Fixture: `poc/clarity-wasm-fixture/` (Clarity 3, epoch 3.1).

## 1. Verdict

Simnet can deploy and run Clarity contracts as clarity-wasm modules on wasmi, both in the native CLI and in the JS SDK on Node. It is opt-in: the `clarity-wasm` cargo feature, then `CLARINET_CLARITY_WASM=1` (native) or `clarityWasm: true` / the same env var (SDK). With the feature off, the build and behaviour are unchanged.

Results, events, error kinds and state read-back match the interpreter on every scripted case. Costs, error stack traces and tooling visibility do not. The runtime is usable for functional testing, but not yet for cost reports or cost-limit testing. In the SDK it is now within ~7% of the interpreter per call (it was 1.9x slower in the first cut). Natively it is still 2x slower, and the remaining cost is known (§6).

## 2. What works

- **Builds.** Native default, native `clarity-wasm`, and SDK wasm32 (node and browser targets) with and without the feature. Clippy is clean on all of them, including the wasm32 clippy for `clarinet-sdk-wasm`. `cargo tree -i clarity` shows a single `clarity` crate.
- **No test regressions.** The `clarity-repl` suite gives 730 passed and 10 skipped: on main, on the default build, with the feature, and with the feature plus `CLARINET_CLARITY_WASM=1`.
- **Deploy.** User contracts are compiled with clar2wasm inside the process (the SDK does it inside wasm32) and initialized through `clarity_wasm::initialize_contract`. A generator error becomes a "Wasm Generator Error" diagnostic. Boot and sBTC contracts stay interpreted, detected by issuer.
- **Calls.**
  - Public, read-only, `ok`/`err`, data-var, map and print all work.
  - `contract-call?` works wasm → wasm (caller → counter) and wasm → interpreted (caller → testnet pox-4).
  - Interpreted → wasm works too: REPL snippets call into wasm contracts.
  - Runtime errors (`unwrap-panic none`, division by zero) come back with the same error kind.
  - `execute_function_as_transaction` dispatches on `wasm_module`, so calls needed no Clarinet code.
- **Paths covered.** `wasm_poc` (native), `clarinet console` (native CLI), SDK `initSimnet` with an empty manifest plus `deployContract`, and SDK `initSimnet` on a manifest whose contracts come from the deployment plan.
- **Parity.** Across the 27 scripted cases (2 deploys, 22 calls, 3 snippets):
  - SDK wasm vs native wasm: 26 identical. The one difference is a `burn-block-height` value that differs by 1, and the interpreter shows the same difference, so the engine isn't the cause.
  - wasm vs interpreter: results and events are identical everywhere. The only differences are costs, error stacks and traces (§4).
- **wasm32 specifics.**
  - wasmi's automatic dispatch picked its loop dispatch without the `tail-call` target feature, so no `portable-dispatch` config was needed.
  - One level of nested `contract-call?` fits in the default 1 MB stack.
  - The unsafe lifetime transmute in `ClarityWasmStore::new` ran fine on wasm32.
  - rusqlite and clap are absent from the wasm32 tree.

## 3. Patches carried

| Repo         | Branch @ commit                                                | Size                                                                                   | Upstream?                       |
| ------------ | -------------------------------------------------------------- | -------------------------------------------------------------------------------------- | ------------------------------- |
| stacks-core  | `feat/clarity-wasm-on-275c8f86` @ 7e1de7b325 (from 275c8f8611) | 17 files, +12,463/−14. Mostly `clarity_wasm.rs` (~11.8k lines, copied from 1c5512ee6f) | Yes, as input to rebasing #7662 |
| clarity-wasm | `feat/compile-only` @ 252fb87e (from `wasmi` 12d26f43)         | 10 files, +121/−27                                                                     | Yes, as-is                      |
| wasmi        | none                                                           | –                                                                                      | –                               |
| Clarinet     | `feat/clarity-wasm-wasmi-poc`, 4 commits on 2692fda1           | 10 source files, +165/−14 (plus lock, fixture, example, drivers)                       | Partly, see below               |

**stacks-core fwd-port**:

- Upstream: all of it belongs in #7662. #7662 is 1408 commits behind Clarinet's pin, and a trial merge gave 80 conflicts, so that PR needs a rebase regardless.
- Two pieces are less invasive than what the branch does and are worth proposing as-is:
  - the gated `DefinedFunction::with_return_type()` builder, instead of a 6th constructor arg;
  - `Contract::initialize_from_ast_with_analysis`, instead of threading `contract_analysis` through `initialize_smart_contract` and stackslib.
- `TypeMap::concretize()` and `TypeSignature::concretize_deep()` are ungated and needed by any clar2wasm built against current develop.
- `clarity-wasm` is out of `default` here. The branch has it on by default, which pulls wasmi into every default `clarity` consumer.

**clarity-wasm compile-only:**

- The `runtime` feature (default on) makes wasmi, rusqlite, chrono, sha2, regex and clap optional, and gates `initialize`, `linker`, `datastore` and `tools`. The stacks-core deps use `default-features = false`.
- Import fixes against current stacks-core: `C32_ADDRESS_VERSION_*` now comes from `stacks_common::address`, and `get_type_size` / `PRINCIPAL_BYTES` from clar2wasm's own `wasm_utils`.
- Upstream: yes. This is the #409 split, and it turned out small.
- Loose end: `clar2wasm/tests/*` need `required-features = ["runtime"]`.

**Clarinet:**

- The feature wiring, settings flag, deploy branch in `interpreter.rs`, `SDKOptions.clarityWasm` and `CLARINET_SDK_FEATURES` in `build.mjs` are small, opt-in, and a reasonable base for a real PR.
- POC-only, not for merge:
  - the absolute-path `[patch]` blocks in the root `Cargo.toml` (they must become git revs);
  - the `CLARINET_CLARITY_WASM_LOG` eprintlns;
  - `poc/` and the `wasm_poc` example.
- Clarinet still hand-writes the wasm deploy. A public stacks-core deploy API (research ask 8) would remove it.

## 4. Behaviour differences (wasm vs interpreter)

1. **Costs are essentially missing.**
   - Deploys cost 0.
   - Every call into a wasm contract costs a flat runtime of 869 (counter) or 727 (caller), with 0 writes. That's only the `LoadContract` charge. Interpreted: `increment` is runtime 13,326 with 2 writes.
   - Interpreted code reached from wasm (pox-4) is still charged.
   - Cost limits are therefore not enforced, and `::get_costs` and SDK cost reports under-report.
   - Three causes:
     - wasm costs go to `GlobalContext.cost_meter`, which is never synced into `cost_track`;
     - `compile_contract` uses `WasmGenerator::new`, which emits no cost code;
     - host-side data-op costs are still TODO upstream (clarity-wasm #344).
2. **Runtime errors lose their stack.** The error kind matches, but the stack is `Some([])` instead of the function list, and the tracer's "Error occured in …:line:col" context is gone.
3. **Traces are shallower.** The top-level frame is kept. Prints and nested contract-call frames inside wasm are missing from `last_contract_call_trace`, because `execute_contract_from_wasm` opens no `CallTraceFrame`.
4. **SDK `print` output is not echoed.** In the SDK driver run, 6 console lines vs 217 interpreted.
5. **Not yet a behaviour difference, but untested:** the call-depth limit, deep `contract-call?` nesting on wasm32, and large contracts.

## 5. Tooling that breaks in wasm mode

| Tool                                                   | State                                                    | Why                                    |
| ------------------------------------------------------ | -------------------------------------------------------- | -------------------------------------- |
| Cost reports, `::get_costs`, cost limits               | Wrong (under-reported, not enforced)                     | §4 item 1                              |
| Call tracer (SDK, always on)                           | Partial: top frame only, no nested frames or prints      | No frames opened from wasm             |
| Print logger (SDK)                                     | Broken for wasm contracts                                | EvalHook never fires inside wasm (#45) |
| Coverage (LCOV)                                        | Blind inside wasm contracts (reasoned from #45, not run) | Same                                   |
| Perf / flamegraph                                      | Blind (not run)                                          | Same, plus missing costs               |
| CLI and DAP debuggers                                  | Can't step into wasm contracts (not run)                 | Same                                   |
| Error messages                                         | Kind only, no location or stack                          | §4 item 2                              |
| Print events as data, post-conditions, state read-back | Work                                                     | Go through `GlobalContext`, not hooks  |

Integration quirks found along the way:

- With the env var set, `clarinet check` also compiles to wasm, and so would the LSP in the CLI binary, because the feature unifies there. Harmless.
- The SDK interpreter holds its own clone of `repl_settings`, so the flag must be set in both places. `apply_clarity_wasm` does this.
- Unverified: the SDK project cache is keyed by manifest path only, not by `clarityWasm`.
- The browser package calls `new SDK(vfs)` with no options, so browser wasm mode needs a wrapper change. The browser smoke test was not run.

## 6. Size and timing

**Bundle** (`clarinet_sdk_bg.wasm`, after wasm-pack `wasm-opt -Oz`, in bytes):

|                          | node raw          | node gzip         | browser raw       | browser gzip      |
| ------------------------ | ----------------- | ----------------- | ----------------- | ----------------- |
| main 2692fda1            | 7,894,428         | 2,474,274         | 7,875,110         | 2,463,211         |
| branch, default features | 7,875,727         | 2,474,774         | 7,871,287         | 2,468,714         |
| branch, `clarity-wasm`   | 11,794,030        | 3,663,828         | 11,797,486        | 3,669,907         |
| delta, feature vs main   | +3.90 MB (+49.4%) | +1.19 MB (+48.1%) | +3.92 MB (+49.8%) | +1.21 MB (+49.0%) |

The fwd-port alone is size-neutral. The whole delta is clar2wasm (walrus, wasm-encoder), wasmi 2.0 and the host functions.

**Timings** (ms, Node 26, release wasm, stable across runs). "wasm, first cut" is fwd-port 417785c78e; "wasm" is 7e1de7b325, which adds the three fixes below:

|                                                     | interp             | wasm, first cut     | wasm               | ratio |
| --------------------------------------------------- | ------------------ | ------------------- | ------------------ | ----- |
| SDK `initSimnet`, empty manifest                    | 96–97              | 98–100              | 100–101            | 1.04x |
| SDK `initSimnet`, fixture manifest (2 plan deploys) | 214–226            | 235–239             | 246–249            | 1.13x |
| SDK 2 × `deployContract`                            | 3.4–3.6            | 7.5–8.6             | 6.7–7.4            | ~2x   |
| SDK 22-call script                                  | 18–19              | 44–46               | 27                 | ~1.5x |
| SDK 1000 × (increment + get-count)                  | 1124 (562 µs/call) | 2007 (1004 µs/call) | 1203 (601 µs/call) | 1.07x |
| Native 5000 × (increment + get-count)               | 39 µs/call         | —                   | 78 µs/call         | 2.0x  |

The native loop is `POC_ITERATIONS=5000` on the `wasm_poc` example. The SDK spends ~500 µs per call outside the engine in either mode, which hides most of the engine gap there.

Per-call cost, native wasm loop:

| Fwd-port state                   | µs/call |
| -------------------------------- | ------- |
| shared `Engine` + `Module` cache | 255     |
| + `wasm_module` stored as hex    | 86      |
| + cached host `Linker`           | 78      |

1. **Shared `Engine`, `Module` cache** (74740ab2ca). `GlobalContext` built its own `Engine::default()`, and `initialize_contract`/`call_function` ran `Module::new` on every call. Now one process-wide engine, and parsed modules cached by a hash of the wasm bytes. SDK: 1004 → 822 µs/call.
2. **`wasm_module` as hex.** Profiling showed ~79% of a native wasm call in `ClarityDatabase::get_contract`, with serde_json parsing the ~23 KB module one integer at a time: `Option<Vec<u8>>` serializes as a JSON number array, and the contract context is re-read on every call. A hex string cuts that to one string parse plus a decode. This was the dominant cost, not wasmi. SDK: 822 → ~600 µs/call.
3. **Cached host `Linker`.** Host functions don't depend on the store, so the 101 registrations happen once per thread and each call clones the linker and adds its 5 cost globals. No lifetime refactor was needed: the fwd-port already erases the store data's lifetimes. It only shows once item 2 is in: −10% natively, within noise in the SDK.

Outputs are unchanged after each step: native output is byte-identical, and the SDK diff summary is the same.

What remains, natively: hex decoding (~31%, `hex_bytes` is char by char) and hashing the bytes for the cache lookup (~6%), both because the whole module still comes back with every `get_contract`; and wasmi instantiation (~23%), which allocates host funcs and exports per call. Storing the module under its own key, loaded only on a cache miss, removes the first two. `InstancePre` (#468) would cut the third.

## 7. Contradictions with the research

Against the research notes `clarity-wasm-simnet.md` and `clarity-wasm-simnet-short.md` (2026-09-24, not in this repo):

1. **"On wasmi, Clarinet only has to bump the stacks-core rev."** This is an end-state claim, and it mostly holds once stacks-core and clarity-wasm are aligned upstream:
   - The fwd-port and the compile-only split are upstream work, not Clarinet work. #7662 is 1408 commits behind Clarinet's pin today (80 conflicts), and that's their rebase to do.
   - What stays on Clarinet's side even after alignment: the hand-written wasm deploy in `interpreter.rs`, unless stacks-core ships a public deploy API (ask 8), plus the opt-in flag wiring. That's ~165 lines in this POC.
   - So it's "bump the rev plus a small, one-time integration", not a contradiction.
2. **"Bundle grows by roughly 2–2.5 MB (uncompressed)."** Measured +3.9 MB raw, +1.2 MB gzip. The research's ~374 KB gzip is wasmi alone. The compiler and host functions are about two thirds of the delta.
3. **"Real-world gaps should be much smaller [than micro-benchmarks]."** The research compared engines with each other, not with the interpreter. On a real Simnet workload, wasm started 1.9x slower than the interpreter in the SDK and is now ~1.07x, after fixes to per-call setup, not guest compute. The largest cost was not wasmi at all but JSON-deserializing the module bytes on every call. Natively it is still 2x, with per-call setup still dominating.
4. **"wasmi + fuel or cost code → exact SDK cost fidelity."** The principle holds, but the current wasmi branch has no fidelity at all: costs aren't synced into `cost_track`, and no cost code is emitted. Any cost-fidelity claim for Simnet waits on the metering design.
5. **"Use `portable-dispatch`" (short doc).** Not needed: wasmi's automatic dispatch already falls back on `wasm32-unknown-unknown`. The long doc's "`auto`/`portable-dispatch`" is the accurate wording.
6. **#409 as a real ask.** Confirmed, but it's smaller than the research implies: 10 files, +121/−27. On the wasmi branch, clar2wasm pulls wasmi rather than wasmtime.

These research claims held:

- Host functions work against Simnet's datastore with no backing-store changes.
- Mixed execution works in both directions.
- The per-call `Module`/`Linker` rebuild (#468) is present on wasmi. The POC now caches both; per-call instantiation remains.
- The branch stores raw bytes (`Option<Vec<u8>>`, JSON-serialized in the contract context).
- `GlobalContext` hardcodes `Engine::default()`.
- `compile_contract` emits no cost code.
- There are no eval hooks in wasm, and every EvalHook tool goes blind.

## 8. Suggested next steps

1. Store the wasm module under its own key so `get_contract` stops carrying it, then try `InstancePre` (#468), and re-time. This decides whether wasm mode is viable for `clarinet test`.
2. Rebase #7662 onto current develop, using the fwd-port's less-invasive API shapes, and upstream the clar2wasm `runtime` split.
3. Once metering is settled: sync `cost_meter` into `cost_track`, and compile with cost code.
4. Add a `WasmObserver` (enter/exit, print, error with expression id) to restore the tracer, logger and error locations.
5. Clarinet: replace the absolute-path patches with git revs, add the browser `clarityWasm` option, key the SDK cache by `clarityWasm`, and add a deep-nesting and large-contract stress case.
