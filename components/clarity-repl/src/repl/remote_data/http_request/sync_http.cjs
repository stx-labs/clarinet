"use strict";

// Synchronous HTTP for the Node-WASM build of clarity-repl.
//
// The WASM main thread can't await Promises, so we offload `fetch` to a
// long-lived Node worker thread (see sync_http_worker.cjs) and block the main
// thread on `Atomics.wait` against a SharedArrayBuffer until the worker flips
// the signal byte. All cross-thread state — request URL/headers, response
// status/headers/body or error — flows through that SAB; no `postMessage`,
// because the main thread is blocked and can't run JS callbacks.

const { Worker } = require("node:worker_threads");
const path = require("node:path");
const LAYOUT = require("./sync_http_layout.cjs");

const {
  HEADER_BYTES,
  OFFSET_STATUS,
  OFFSET_STATUS_TEXT_LEN,
  OFFSET_HEADERS_LEN,
  OFFSET_BODY_LEN,
  OFFSET_URL_LEN,
  OFFSET_REQ_HDRS_LEN,
  SIGNAL_RESPONSE_DONE,
  SIGNAL_REQUEST_PENDING,
} = LAYOUT;

// The worker source lives next to this file. build.mjs copies it into
// pkg-node/snippets/.../ alongside the wasm-bindgen-generated shim.
const WORKER_FILE = path.join(__dirname, "sync_http_worker.cjs");

const MIN_SAB_SIZE = 64 * 1024;
const MAX_SAB_SIZE = 256 * 1024 * 1024;
const DEFAULT_SAB_SIZE = 8 * 1024 * 1024;

function parseSabSize() {
  const raw = process.env.CLARINET_SDK_SAB_SIZE;
  if (!raw) return DEFAULT_SAB_SIZE;
  const n = Number.parseInt(raw, 10);
  if (!Number.isFinite(n)) return DEFAULT_SAB_SIZE;
  if (n < MIN_SAB_SIZE) return MIN_SAB_SIZE;
  if (n > MAX_SAB_SIZE) return MAX_SAB_SIZE;
  return n;
}

const SAB_SIZE = parseSabSize();
const DATA_CAPACITY = SAB_SIZE - HEADER_BYTES;

// Main-thread Atomics.wait heartbeat (ms). Without it, the JS event loop never
// pumps while main is blocked, so worker.on('error')/'exit') can't flip
// state.dead if the worker thread dies (terminate, OOM, native crash) — main
// would block in the kernel futex forever.
const WORKER_HEARTBEAT_MS = 1000;

let workerState = null;

// Registered once at module load — references whichever worker is current via
// the workerState closure, so respawns don't accumulate listeners.
process.on("beforeExit", () => {
  const w = workerState?.worker;
  if (!w) return;
  try {
    w.terminate();
  } catch (_) {}
});

function ensureWorker() {
  if (workerState && !workerState.dead) return workerState;

  const sab = new SharedArrayBuffer(SAB_SIZE);
  const header = new Int32Array(sab, 0, HEADER_BYTES / 4);
  const data = new Uint8Array(sab, HEADER_BYTES, DATA_CAPACITY);
  const signal = new Int32Array(sab, 0, 1);
  const worker = new Worker(WORKER_FILE, { workerData: { sab } });
  worker.unref();

  const state = { worker, sab, header, data, signal, dead: false, cause: null };

  worker.on("error", (err) => {
    state.dead = true;
    state.cause = err;
    if (process.env.CLARINET_SDK_DEBUG_SYNC_HTTP) {
      process.stderr.write(`[sync_http] worker error: ${err && err.stack ? err.stack : err}\n`);
    }
  });
  worker.on("exit", (code) => {
    state.dead = true;
    if (state.cause == null && code !== 0) {
      state.cause = new Error(`sync_http worker exited with code ${code}`);
    }
    if (process.env.CLARINET_SDK_DEBUG_SYNC_HTTP) {
      process.stderr.write(`[sync_http] worker exit code=${code}\n`);
    }
  });

  workerState = state;
  return state;
}

const encoder = new TextEncoder();
const decoder = new TextDecoder();

function workerCause(state) {
  if (!state.cause) return "unknown error";
  return state.cause.message || String(state.cause);
}

function syncHttpRequest(url, headersJson) {
  const state = ensureWorker();
  if (state.dead) {
    throw new Error("sync_http worker is not running: " + workerCause(state));
  }

  // Read HIRO_API_KEY per-request so callers can set it after module load
  // (e.g. dotenv.config() / beforeEach()). JSON.parse is intentionally not
  // wrapped — malformed input is a caller bug, not something to silently drop.
  const baseHeaders = headersJson ? JSON.parse(headersJson) : {};
  const apiKey = process.env.HIRO_API_KEY;
  if (apiKey && baseHeaders["x-api-key"] === undefined) {
    baseHeaders["x-api-key"] = apiKey;
  }

  const urlBytes = encoder.encode(url);
  const hdrBytes = encoder.encode(JSON.stringify(baseHeaders));
  if (urlBytes.length + hdrBytes.length > DATA_CAPACITY) {
    throw new Error(
      `request too large: ${urlBytes.length + hdrBytes.length} bytes, capacity ${DATA_CAPACITY}`,
    );
  }

  state.data.set(urlBytes, 0);
  state.data.set(hdrBytes, urlBytes.length);
  Atomics.store(state.header, OFFSET_URL_LEN / 4, urlBytes.length);
  Atomics.store(state.header, OFFSET_REQ_HDRS_LEN / 4, hdrBytes.length);
  Atomics.store(state.header, OFFSET_STATUS / 4, 0);
  Atomics.store(state.header, OFFSET_STATUS_TEXT_LEN / 4, 0);
  Atomics.store(state.header, OFFSET_HEADERS_LEN / 4, 0);
  Atomics.store(state.header, OFFSET_BODY_LEN / 4, 0);

  Atomics.store(state.signal, 0, SIGNAL_REQUEST_PENDING);
  Atomics.notify(state.signal, 0, 1);

  // Block until the worker flips signal back to RESPONSE_DONE. The heartbeat
  // bound is required so a dead worker doesn't deadlock main — see comment on
  // WORKER_HEARTBEAT_MS.
  for (;;) {
    const r = Atomics.wait(state.signal, 0, SIGNAL_REQUEST_PENDING, WORKER_HEARTBEAT_MS);
    if (r === "not-equal" || r === "ok") break;
    if (state.dead) break;
  }

  if (state.dead) {
    throw new Error("sync_http worker died during request: " + workerCause(state));
  }

  const status = Atomics.load(state.header, OFFSET_STATUS / 4);
  const statusTextLen = Atomics.load(state.header, OFFSET_STATUS_TEXT_LEN / 4);
  const headersLen = Atomics.load(state.header, OFFSET_HEADERS_LEN / 4);
  const bodyLen = Atomics.load(state.header, OFFSET_BODY_LEN / 4);

  if (bodyLen < 0) {
    throw new Error(decoder.decode(state.data.subarray(0, -bodyLen)));
  }

  // Layout: [statusText][headers][body]
  const headersStart = statusTextLen;
  const bodyStart = headersStart + headersLen;
  const statusText =
    statusTextLen > 0 ? decoder.decode(state.data.subarray(0, statusTextLen)) : "";
  const headers =
    headersLen > 0
      ? JSON.parse(decoder.decode(state.data.subarray(headersStart, bodyStart)))
      : {};
  const body = decoder.decode(state.data.subarray(bodyStart, bodyStart + bodyLen));

  return { status, statusText, headers, body };
}

// Block the current thread for `ms` ms via Atomics.wait on a never-notified
// SAB. Required because std::thread::sleep is a no-op on wasm32.
const sleepSab = new SharedArrayBuffer(4);
const sleepView = new Int32Array(sleepSab);

function syncSleep(ms) {
  if (!Number.isFinite(ms) || ms <= 0) return;
  Atomics.wait(sleepView, 0, 0, ms);
}

// For tests and graceful shutdown. Next syncHttpRequest lazily starts a fresh worker.
function closeWorker() {
  if (!workerState) return;
  const w = workerState.worker;
  workerState = null;
  try {
    w.terminate();
  } catch (_) {}
}

module.exports = { syncHttpRequest, syncSleep, closeWorker };
