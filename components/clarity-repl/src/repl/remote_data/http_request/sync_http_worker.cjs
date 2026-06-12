"use strict";

// Worker for sync_http.cjs. Runs `fetch()` asynchronously and signals the
// blocked main thread via the SharedArrayBuffer it received in workerData.

const { workerData } = require("node:worker_threads");
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
  FETCH_TIMEOUT_MS,
} = require("./sync_http_layout.cjs");

const sab = workerData.sab;
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
  return JSON.parse(decoder.decode(data.subarray(urlLen, urlLen + hdrLen)));
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

function errMessage(e) {
  return e && e.message ? e.message : String(e);
}

// `requestActive` gates the unhandled-error nets below: a late-arriving
// rejection (e.g. an orphan AbortController abort from a prior request) must
// NOT overwrite the SAB while main is blocked waiting on a different request
// or hasn't yet issued one.
let requestActive = false;

function failRequest(prefix, err) {
  if (!requestActive) return;
  try {
    writeError(prefix + ": " + errMessage(err));
    signalDone();
    requestActive = false;
  } catch (_) {}
}

// Belt-and-braces: any uncaught async error would terminate the worker by
// default (Node ≥ 20), silently deadlocking main on the next request. Catch
// and surface them, but only when a request is actually in flight.
process.on("unhandledRejection", (err) => failRequest("worker unhandledRejection", err));
process.on("uncaughtException", (err) => failRequest("worker uncaughtException", err));

async function handleOneRequest() {
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
    const hdrObj = {};
    res.headers.forEach((v, k) => {
      hdrObj[k] = v;
    });
    writeResponse(res.status, res.statusText || "", hdrObj, buf);
  } catch (e) {
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
      await handleOneRequest();
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
