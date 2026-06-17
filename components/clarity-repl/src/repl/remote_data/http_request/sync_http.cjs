"use strict";

// Synchronous HTTP for the Node-WASM build of clarity-repl.
//
// The WASM main thread can't await Promises, so we offload `fetch` to a
// long-lived Node worker thread (see workerMain below) and block the main
// thread on `Atomics.wait` against a SharedArrayBuffer until the worker flips
// the signal byte. All cross-thread state — request URL/headers, response
// status/headers/body or error — flows through that SAB; no `postMessage`,
// because the main thread is blocked and can't run JS callbacks.
//
// This file is deliberately self-contained: it is the only snippet
// wasm-bindgen knows about, so anything loaded from a sibling file would be
// missing wherever the snippet is copied without our build script
// (wasm-bindgen-test runners, bundlers). The worker is therefore spawned from
// an eval'd serialization of workerMain() and the layout constants travel via
// workerData instead of a shared module.

const { Worker } = require("node:worker_threads");

// SAB header at offset 0, request/response data area at offset HEADER_BYTES:
//   0  Int32  signal           0=response_done (worker idle), 1=request_pending
//   4  Int32  status_code      HTTP status, or -1 for transport error
//   8  Int32  status_text_len  response status text byte length
//  12  Int32  headers_len      response headers JSON byte length
//  16  Int32  body_len         response body byte length; negative means error
//  20  Int32  url_len          request URL byte length
//  24  Int32  req_hdrs_len     request headers JSON byte length
//  28  Int32  liveness         counter the worker bumps on a timer; main reads
//                              it to tell a slow response from a dead worker
const LAYOUT = {
  HEADER_BYTES: 1024,
  OFFSET_STATUS: 4,
  OFFSET_STATUS_TEXT_LEN: 8,
  OFFSET_HEADERS_LEN: 12,
  OFFSET_BODY_LEN: 16,
  OFFSET_URL_LEN: 20,
  OFFSET_REQ_HDRS_LEN: 24,
  OFFSET_LIVENESS: 28,
  // How often the worker bumps the liveness counter. Its event loop is free
  // while a fetch is in flight (and while idle it parks in Atomics.wait, but
  // main only checks liveness while a request is pending), so a live worker
  // always bumps at least once per main-side heartbeat.
  LIVENESS_INTERVAL_MS: 250,
  // The signal alternates between RESPONSE_DONE (worker idle, main may issue
  // next request) and REQUEST_PENDING (worker is processing). A three-state
  // scheme races: by the time the worker loops back to wait, main may already
  // have reset the signal and the worker would re-enter with stale data.
  SIGNAL_RESPONSE_DONE: 0,
  SIGNAL_REQUEST_PENDING: 1,
  FETCH_TIMEOUT_MS: 30_000,
};

const {
  HEADER_BYTES,
  OFFSET_STATUS,
  OFFSET_STATUS_TEXT_LEN,
  OFFSET_HEADERS_LEN,
  OFFSET_BODY_LEN,
  OFFSET_URL_LEN,
  OFFSET_REQ_HDRS_LEN,
  OFFSET_LIVENESS,
  SIGNAL_RESPONSE_DONE,
  SIGNAL_REQUEST_PENDING,
} = LAYOUT;

// Entire worker thread, serialized via toString() and spawned with
// `new Worker(code, { eval: true })`. It must be fully self-contained: no
// references to this module's scope — everything arrives through workerData.
function workerMain() {
  "use strict";

  const { workerData } = require("node:worker_threads");
  const {
    HEADER_BYTES,
    OFFSET_STATUS,
    OFFSET_STATUS_TEXT_LEN,
    OFFSET_HEADERS_LEN,
    OFFSET_BODY_LEN,
    OFFSET_URL_LEN,
    OFFSET_REQ_HDRS_LEN,
    OFFSET_LIVENESS,
    SIGNAL_RESPONSE_DONE,
    SIGNAL_REQUEST_PENDING,
    FETCH_TIMEOUT_MS,
    LIVENESS_INTERVAL_MS,
  } = workerData.layout;

  const sab = workerData.sab;
  const dataCapacity = sab.byteLength - HEADER_BYTES;
  const signal = new Int32Array(sab, 0, 1);
  const header = new Int32Array(sab, 0, HEADER_BYTES / 4);
  const data = new Uint8Array(sab, HEADER_BYTES, dataCapacity);
  const decoder = new TextDecoder();
  const encoder = new TextEncoder();

  // Test-only hook: boot the thread but never serve or bump liveness, so the
  // main-thread dead-worker detection can be exercised end-to-end.
  if (process.env.CLARINET_SDK_TEST_SYNC_HTTP_STALL_WORKER) return;

  // Liveness beacon for the blocked main thread (its event loop can't pump, so
  // it can't observe worker 'error'/'exit' — see WORKER_HEARTBEAT_MS in the
  // parent module). If this thread dies, the counter freezes and main bails out.
  setInterval(() => {
    Atomics.add(header, OFFSET_LIVENESS / 4, 1);
  }, LIVENESS_INTERVAL_MS).unref();

  function readUrl() {
    const len = Atomics.load(header, OFFSET_URL_LEN / 4);
    return decoder.decode(data.subarray(0, len));
  }

  function readRequestHeaders() {
    const urlLen = Atomics.load(header, OFFSET_URL_LEN / 4);
    const hdrLen = Atomics.load(header, OFFSET_REQ_HDRS_LEN / 4);
    if (hdrLen === 0) return {};
    return JSON.parse(decoder.decode(data.subarray(urlLen, urlLen + hdrLen)));
  }

  function writeError(message) {
    const encoded = encoder.encode(message);
    let len = Math.min(encoded.length, dataCapacity);
    // Don't cut a multi-byte UTF-8 sequence in half: back off past any
    // continuation bytes (0b10xxxxxx) so the truncated message decodes cleanly.
    if (len < encoded.length) {
      while (len > 0 && (encoded[len] & 0xc0) === 0x80) len -= 1;
    }
    data.set(encoded.subarray(0, len), 0);
    Atomics.store(header, OFFSET_STATUS / 4, -1);
    Atomics.store(header, OFFSET_STATUS_TEXT_LEN / 4, 0);
    Atomics.store(header, OFFSET_HEADERS_LEN / 4, 0);
    // Negative body_len signals an error; magnitude is the message length.
    Atomics.store(header, OFFSET_BODY_LEN / 4, -len);
  }

  function writeResponse(status, statusText, headers, body) {
    const encodedStatusText = encoder.encode(statusText || "");
    const encodedHeaders = encoder.encode(JSON.stringify(headers));
    const total = encodedStatusText.length + encodedHeaders.length + body.length;
    if (total > dataCapacity) {
      writeError("response too large: " + total + " bytes, capacity " + dataCapacity);
      return;
    }
    // Layout in data area: [statusText][headersJson][body]
    data.set(encodedStatusText, 0);
    data.set(encodedHeaders, encodedStatusText.length);
    data.set(body, encodedStatusText.length + encodedHeaders.length);
    Atomics.store(header, OFFSET_STATUS / 4, status);
    Atomics.store(header, OFFSET_STATUS_TEXT_LEN / 4, encodedStatusText.length);
    Atomics.store(header, OFFSET_HEADERS_LEN / 4, encodedHeaders.length);
    Atomics.store(header, OFFSET_BODY_LEN / 4, body.length);
  }

  function signalDone() {
    Atomics.store(signal, 0, SIGNAL_RESPONSE_DONE);
    Atomics.notify(signal, 0, 1);
  }

  function errMessage(e) {
    return e && e.message ? e.message : String(e);
  }

  // `requestActive` gates the unhandled-error nets below: a late-arriving
  // rejection (e.g. an orphan AbortController abort from a prior request) must
  // NOT overwrite the SAB while main is blocked waiting on a different request
  // or hasn't yet issued one.
  //
  // `generation` guards the other direction: when failRequest answers an
  // in-flight request early, the still-pending fetch in handleOneRequest must
  // not write its eventual result over the SAB — main may already have issued
  // the NEXT request into the same buffer. failRequest bumps the generation;
  // handleOneRequest only writes/signals if its generation is still current.
  let requestActive = false;
  let generation = 0;

  function failRequest(prefix, err) {
    if (!requestActive) return;
    requestActive = false;
    generation += 1;
    try {
      writeError(prefix + ": " + errMessage(err));
      signalDone();
    } catch (_) {}
  }

  // Belt-and-braces: any uncaught async error would terminate the worker by
  // default (Node ≥ 20), silently deadlocking main on the next request. Catch
  // and surface them, but only when a request is actually in flight.
  process.on("unhandledRejection", (err) => failRequest("worker unhandledRejection", err));
  process.on("uncaughtException", (err) => failRequest("worker uncaughtException", err));

  async function handleOneRequest(gen) {
    let url, headers;
    try {
      url = readUrl();
      headers = readRequestHeaders();
    } catch (e) {
      writeError("failed to decode request: " + errMessage(e));
      return;
    }
    // Explicit AbortController + clearTimeout so the timer doesn't keep ticking
    // after a fast success — an orphan abort firing post-completion can produce
    // rejections that fire on the *next* request's await checkpoint.
    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), FETCH_TIMEOUT_MS);
    try {
      const res = await fetch(url, { headers, signal: controller.signal });
      const buf = new Uint8Array(await res.arrayBuffer());
      if (gen !== generation) return;
      const hdrObj = {};
      res.headers.forEach((v, k) => {
        hdrObj[k] = v;
      });
      writeResponse(res.status, res.statusText || "", hdrObj, buf);
    } catch (e) {
      if (gen !== generation) return;
      writeError(errMessage(e));
    } finally {
      clearTimeout(timer);
    }
  }

  (async () => {
    for (;;) {
      try {
        // Blocking Atomics.wait is reliable across Node versions; the Node 24 /
        // macOS issue was specific to Atomics.waitAsync.
        while (Atomics.load(signal, 0) !== SIGNAL_REQUEST_PENDING) {
          Atomics.wait(signal, 0, SIGNAL_RESPONSE_DONE);
        }
        // Yield to the event loop before flipping requestActive=true. Atomics.wait
        // parks the loop, so any unhandledRejection queued while we were idle
        // (e.g. an orphan abort from a prior request) is still pending. Letting
        // it fire here — while requestActive=false — keeps the gate effective
        // and avoids attributing the orphan error to a fresh in-flight request.
        await new Promise((r) => setImmediate(r));
        requestActive = true;
        const gen = generation;
        await handleOneRequest(gen);
        // failRequest may have answered (and invalidated) this request already;
        // signalling again here would ack a request we never handled.
        if (gen !== generation) continue;
        signalDone();
        requestActive = false;
      } catch (loopErr) {
        // Last-ditch recovery: never let the loop exit silently.
        try {
          writeError("worker loop error: " + errMessage(loopErr));
          signalDone();
          requestActive = false;
        } catch (_) {}
      }
    }
  })();
}

const WORKER_SOURCE = `(${workerMain})()`;

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

// Main-thread Atomics.wait heartbeat (ms). While main is blocked, its event
// loop never pumps, so worker.on('error')/'exit' CANNOT fire — JS callbacks
// only run when the stack empties. Death detection therefore can't rely on
// state.dead; instead the worker bumps a liveness counter in the SAB from a
// timer (see LIVENESS_INTERVAL_MS), and main samples it on each heartbeat
// timeout. A counter frozen for MAX_STALE_HEARTBEATS consecutive heartbeats
// means the worker thread is gone (failed to start, terminated, OOM, native
// crash) and main throws instead of blocking forever.
const WORKER_HEARTBEAT_MS = 1000;
const MAX_STALE_HEARTBEATS = 5;

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
  const worker = new Worker(WORKER_SOURCE, {
    eval: true,
    workerData: { sab, layout: LAYOUT },
  });
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
  // bound lets us sample the worker's liveness counter so a dead worker
  // doesn't deadlock main — see comment on WORKER_HEARTBEAT_MS.
  let lastBeat = Atomics.load(state.header, OFFSET_LIVENESS / 4);
  let staleHeartbeats = 0;
  for (;;) {
    const r = Atomics.wait(state.signal, 0, SIGNAL_REQUEST_PENDING, WORKER_HEARTBEAT_MS);
    if (r === "not-equal" || r === "ok") break;
    const beat = Atomics.load(state.header, OFFSET_LIVENESS / 4);
    if (beat !== lastBeat) {
      lastBeat = beat;
      staleHeartbeats = 0;
      continue;
    }
    staleHeartbeats += 1;
    if (staleHeartbeats >= MAX_STALE_HEARTBEATS) {
      state.dead = true;
      state.cause ??= new Error(
        `worker unresponsive for ${staleHeartbeats * WORKER_HEARTBEAT_MS}ms (thread died or failed to start)`,
      );
      break;
    }
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
