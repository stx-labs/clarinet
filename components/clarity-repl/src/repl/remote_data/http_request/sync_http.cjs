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

// The worker source lives next to this file. wasm-bindgen copies sync_http.cjs
// into pkg-node/snippets/.../ automatically because Rust imports from it;
// build.mjs copies sync_http_worker.cjs into the same directory as a post-
// build step.
const WORKER_FILE = path.join(__dirname, "sync_http_worker.cjs");

// ---- SAB layout (header at offset 0, data area at offset HEADER_BYTES) ----
//   0  Int32  signal           0=response_done (worker idle), 1=request_pending
//   4  Int32  status_code      HTTP status, or -1 for transport error
//   8  Int32  status_text_len  response status text (HTTP reason phrase) byte length
//  12  Int32  headers_len      response headers JSON byte length
//  16  Int32  body_len         response body byte length; negative means error
//  20  Int32  url_len          request URL byte length
//  24  Int32  req_hdrs_len     request headers JSON byte length
const HEADER_BYTES = 1024;
const OFFSET_STATUS = 4;
const OFFSET_STATUS_TEXT_LEN = 8;
const OFFSET_HEADERS_LEN = 12;
const OFFSET_BODY_LEN = 16;
const OFFSET_URL_LEN = 20;
const OFFSET_REQ_HDRS_LEN = 24;

// The signal alternates between RESPONSE_DONE (worker idle, main may issue
// next request) and REQUEST_PENDING (worker is processing). Three-state
// schemes race: by the time the worker loops back to wait, the main thread
// may already have reset the signal, and the worker would re-enter with
// stale request data.
const SIGNAL_RESPONSE_DONE = 0;
const SIGNAL_REQUEST_PENDING = 1;

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
const API_KEY = process.env.HIRO_API_KEY || null;

// Main-thread Atomics.wait heartbeat (ms). Each tick returns briefly to JS so
// worker.on('error') / 'exit' callbacks can run and flip state.dead, which is
// the only signal main has if the worker thread dies mid-request.
const WORKER_HEARTBEAT_MS = 1000;

// Worker polling interval. We do not use Atomics.waitAsync because in Node 24
// on macOS the worker's event loop stops scheduling the waitAsync resolution
// after a few successful iterations (reproduced reliably with 3+ sequential
// requests). setTimeout-based polling goes through libuv's timer subsystem,
// which is reliable. 1 ms polling adds <1 ms of latency per request and is
// effectively idle CPU while the worker is waiting.
const WORKER_POLL_MS = 1;

// Upper bound for a single fetch call, enforced inside the worker.
const FETCH_TIMEOUT_MS = 30_000;

let workerState = null;

function ensureWorker() {
  if (workerState && !workerState.dead) return workerState;

  const sab = new SharedArrayBuffer(SAB_SIZE);
  const header = new Int32Array(sab, 0, HEADER_BYTES / 4);
  const data = new Uint8Array(sab, HEADER_BYTES, DATA_CAPACITY);
  const signal = new Int32Array(sab, 0, 1);
  const worker = new Worker(WORKER_FILE, {
    workerData: {
      sab,
      headerBytes: HEADER_BYTES,
      offsetStatus: OFFSET_STATUS,
      offsetStatusTextLen: OFFSET_STATUS_TEXT_LEN,
      offsetHeadersLen: OFFSET_HEADERS_LEN,
      offsetBodyLen: OFFSET_BODY_LEN,
      offsetUrlLen: OFFSET_URL_LEN,
      offsetReqHdrsLen: OFFSET_REQ_HDRS_LEN,
      signalResponseDone: SIGNAL_RESPONSE_DONE,
      signalRequestPending: SIGNAL_REQUEST_PENDING,
      pollMs: WORKER_POLL_MS,
      fetchTimeoutMs: FETCH_TIMEOUT_MS,
    },
  });
  worker.unref();
  process.on("beforeExit", () => {
    try {
      worker.terminate();
    } catch (_) {}
  });

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

function syncHttpRequest(url, headersJson) {
  const state = ensureWorker();
  if (state.dead) {
    throw new Error(
      "sync_http worker is not running: " +
        (state.cause ? state.cause.message || String(state.cause) : "unknown error"),
    );
  }

  // Build request headers: caller-provided + API key injection.
  let baseHeaders;
  try {
    baseHeaders = headersJson ? JSON.parse(headersJson) : {};
  } catch {
    baseHeaders = {};
  }
  if (API_KEY && baseHeaders["x-api-key"] === undefined) {
    baseHeaders["x-api-key"] = API_KEY;
  }
  const finalHeadersJson = JSON.stringify(baseHeaders);

  const urlBytes = encoder.encode(url);
  const hdrBytes = encoder.encode(finalHeadersJson);
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

  // Block until the worker flips signal back to RESPONSE_DONE — but with a
  // heartbeat, so even if the worker dies (e.g. uncaught async error inside
  // the worker thread, OOM, manual kill) we don't deadlock forever. Without
  // the heartbeat the JS event loop never runs, so worker.on('error') /
  // worker.on('exit') can't fire to flip state.dead, and we'd block in the
  // kernel futex with no way out.
  for (;;) {
    const r = Atomics.wait(
      state.signal,
      0,
      SIGNAL_REQUEST_PENDING,
      WORKER_HEARTBEAT_MS,
    );
    if (r === "not-equal" || r === "ok") break;
    // r === "timed-out". The brief return to JS gives queued worker error/exit
    // callbacks a chance to run; if the worker is gone, state.dead is now set.
    if (state.dead) break;
  }

  if (state.dead) {
    throw new Error(
      "sync_http worker died during request: " +
        (state.cause ? state.cause.message || String(state.cause) : "unknown error"),
    );
  }

  const status = Atomics.load(state.header, OFFSET_STATUS / 4);
  const statusTextLen = Atomics.load(state.header, OFFSET_STATUS_TEXT_LEN / 4);
  const headersLen = Atomics.load(state.header, OFFSET_HEADERS_LEN / 4);
  const bodyLen = Atomics.load(state.header, OFFSET_BODY_LEN / 4);

  if (bodyLen < 0) {
    const msg = decoder.decode(state.data.subarray(0, -bodyLen));
    throw new Error(msg);
  }

  // Layout: [statusText][headers][body]
  const statusText =
    statusTextLen > 0 ? decoder.decode(state.data.subarray(0, statusTextLen)) : "";
  const headersStart = statusTextLen;
  const bodyStart = headersStart + headersLen;
  const headers =
    headersLen > 0
      ? JSON.parse(decoder.decode(state.data.subarray(headersStart, bodyStart)))
      : {};
  const body = decoder.decode(state.data.subarray(bodyStart, bodyStart + bodyLen));

  return { status, statusText, headers, body };
}

// Block the current thread for `ms` milliseconds using Atomics.wait on a
// dedicated, never-notified SAB. Required because std::thread::sleep is a
// no-op on wasm32.
const sleepSab = new SharedArrayBuffer(4);
const sleepView = new Int32Array(sleepSab);

function syncSleep(ms) {
  if (!Number.isFinite(ms) || ms <= 0) return;
  Atomics.wait(sleepView, 0, 0, ms);
}

// Terminate the worker eagerly. Intended for tests and graceful shutdown. The
// next syncHttpRequest call will lazily start a fresh worker.
function closeWorker() {
  if (!workerState) return;
  const w = workerState.worker;
  workerState = null;
  try {
    w.terminate();
  } catch (_) {}
}

module.exports = { syncHttpRequest, syncSleep, closeWorker };
