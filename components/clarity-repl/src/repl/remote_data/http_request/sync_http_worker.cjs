"use strict";

// Worker for sync_http.cjs. Runs `fetch()` asynchronously and signals the
// blocked main thread via the SharedArrayBuffer it received in workerData.
// All layout constants and tuning knobs are passed via workerData so this
// file can stay literal JS (and keep editor syntax highlighting).

const { workerData } = require("node:worker_threads");

const sab = workerData.sab;
const HEADER_BYTES = workerData.headerBytes;
const OFFSET_STATUS = workerData.offsetStatus;
const OFFSET_STATUS_TEXT_LEN = workerData.offsetStatusTextLen;
const OFFSET_HEADERS_LEN = workerData.offsetHeadersLen;
const OFFSET_BODY_LEN = workerData.offsetBodyLen;
const OFFSET_URL_LEN = workerData.offsetUrlLen;
const OFFSET_REQ_HDRS_LEN = workerData.offsetReqHdrsLen;
const SIGNAL_RESPONSE_DONE = workerData.signalResponseDone;
const SIGNAL_REQUEST_PENDING = workerData.signalRequestPending;
const POLL_MS = workerData.pollMs;
const FETCH_TIMEOUT_MS = workerData.fetchTimeoutMs;

const dataCapacity = sab.byteLength - HEADER_BYTES;
const signal = new Int32Array(sab, 0, 1);
const header = new Int32Array(sab, 0, HEADER_BYTES / 4);
const data = new Uint8Array(sab, HEADER_BYTES, dataCapacity);
const decoder = new TextDecoder();
const encoder = new TextEncoder();

function readUrl() {
  const len = Atomics.load(header, OFFSET_URL_LEN / 4);
  return decoder.decode(data.subarray(0, len));
}

function readRequestHeaders() {
  const urlLen = Atomics.load(header, OFFSET_URL_LEN / 4);
  const hdrLen = Atomics.load(header, OFFSET_REQ_HDRS_LEN / 4);
  if (hdrLen === 0) return {};
  try {
    return JSON.parse(decoder.decode(data.subarray(urlLen, urlLen + hdrLen)));
  } catch {
    return {};
  }
}

function writeError(message) {
  const encoded = encoder.encode(message);
  const len = Math.min(encoded.length, dataCapacity);
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

// Belt-and-braces: any uncaught async error in the worker would terminate the
// thread by default (Node ≥ 20 with `--unhandled-rejections=throw`), which
// would silently deadlock the main thread on the next request. Catch them so
// the worker survives and the main thread still sees a clean error.
process.on("unhandledRejection", (err) => {
  try {
    writeError("worker unhandledRejection: " + (err && err.message ? err.message : String(err)));
    signalDone();
  } catch (_) {}
});
process.on("uncaughtException", (err) => {
  try {
    writeError("worker uncaughtException: " + (err && err.message ? err.message : String(err)));
    signalDone();
  } catch (_) {}
});

function sleepMs(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

(async () => {
  for (;;) {
    try {
      // Poll the signal via setTimeout until main flips it to REQUEST_PENDING.
      // Atomics.waitAsync turned out to be unreliable on Node 24 / macOS — its
      // Promise occasionally fails to resolve on a sibling Atomics.notify after
      // a few sequential requests, deadlocking the main thread. setTimeout goes
      // through libuv's timer subsystem, which is reliable. Idle CPU is
      // negligible; latency added per request is at most ~1 ms.
      while (Atomics.load(signal, 0) !== SIGNAL_REQUEST_PENDING) {
        await sleepMs(POLL_MS);
      }

      let url, headers;
      try {
        url = readUrl();
        headers = readRequestHeaders();
      } catch (e) {
        writeError("failed to decode request: " + (e && e.message ? e.message : String(e)));
        signalDone();
        continue;
      }
      try {
        const res = await fetch(url, {
          headers,
          signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
        });
        const buf = new Uint8Array(await res.arrayBuffer());
        const hdrObj = {};
        res.headers.forEach((v, k) => {
          hdrObj[k] = v;
        });
        writeResponse(res.status, res.statusText || "", hdrObj, buf);
      } catch (e) {
        writeError(e && e.message ? e.message : String(e));
      }
      signalDone();
    } catch (loopErr) {
      // Last-ditch recovery: never let the loop exit silently.
      try {
        writeError("worker loop error: " + (loopErr && loopErr.message ? loopErr.message : String(loopErr)));
        signalDone();
      } catch (_) {}
    }
  }
})();
