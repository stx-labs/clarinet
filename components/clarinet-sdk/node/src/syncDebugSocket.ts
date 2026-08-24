/**
 * Synchronous TCP socket client using Worker threads + SharedArrayBuffer +
 * Atomics.wait. Allows the test runner's main thread to make blocking calls to
 * the `clarinet dap` SDK server without async/await, so the existing synchronous
 * Simnet API surface can be fulfilled by a remote debug session.
 *
 * Architecture (mirrors the sync_http.cjs pattern in clarinet-sdk-wasm):
 *   - Main thread writes a JSON request into the SAB, signals the worker, then
 *     blocks on Atomics.wait until the worker flips the signal back.
 *   - Worker thread holds a persistent TCP socket to the debug server. On each
 *     wakeup it reads the request, writes it to the socket, reads the newline-
 *     delimited JSON response, writes it back into the SAB and signals done.
 */
import { Worker } from "node:worker_threads";

// SAB layout (all offsets in bytes, interpreted as Int32 unless noted):
//   0  signal       0 = idle (response ready), 1 = request pending
//   4  request_len  byte length of the request in the data area
//   8  response_len byte length of response (negative = error message length)
//  12  liveness     counter bumped by worker timer; main polls for liveness
//  64+ data area    request bytes followed by response bytes (or error message)
const HEADER_BYTES = 64;
const OFFSET_SIGNAL = 0;    // Int32 index 0
const OFFSET_REQ_LEN = 1;   // Int32 index 1
const OFFSET_RESP_LEN = 2;  // Int32 index 2
const OFFSET_LIVENESS = 3;  // Int32 index 3
const SIGNAL_IDLE = 0;
const SIGNAL_PENDING = 1;
const LIVENESS_INTERVAL_MS = 250;
const WORKER_HEARTBEAT_MS = 1000;
const MAX_STALE_HEARTBEATS = 15;
const DEFAULT_SAB_SIZE = 4 * 1024 * 1024; // 4 MB

// ---------------------------------------------------------------------------
// Worker implementation – serialized and spawned with eval:true so this file
// stays self-contained. No TypeScript types inside: they get stripped by tsc
// and the string is valid JS. All data comes through workerData.
// ---------------------------------------------------------------------------
function debugSocketWorkerMain() {
  /* eslint-disable */
  const { workerData, parentPort } = require("node:worker_threads");
  const net = require("node:net");

  const {
    sab,
    port,
    headerBytes,
    signalIdle,
    signalPending,
    livenessIntervalMs,
    offsetSignal,
    offsetReqLen,
    offsetRespLen,
    offsetLiveness,
  } = workerData;

  const dataCapacity = sab.byteLength - headerBytes;
  const signal = new Int32Array(sab, 0, 1);
  const header = new Int32Array(sab, 0, headerBytes / 4);
  const data = new Uint8Array(sab, headerBytes, dataCapacity);
  const encoder = new TextEncoder();
  const decoder = new TextDecoder();

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  let pendingResolve: ((r: any) => void) | null = null;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  let pendingReject: ((e: any) => void) | null = null;
  let recvBuf = "";

  const socket = net.createConnection({ port, host: "127.0.0.1" });

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  socket.on("data", (chunk: any) => {
    recvBuf += chunk.toString("utf8");
    const lines = recvBuf.split("\n");
    recvBuf = lines.pop() ?? "";
    for (const line of lines) {
      const trimmed = line.trim();
      if (trimmed && pendingResolve) {
        const res = pendingResolve;
        pendingResolve = null;
        pendingReject = null;
        res(trimmed);
        break;
      }
    }
  });

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  socket.on("error", (err: any) => {
    if (pendingReject) {
      const rej = pendingReject;
      pendingResolve = null;
      pendingReject = null;
      rej(err);
    }
  });

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  function writeResponse(jsonStr: any) {
    const bytes = encoder.encode(jsonStr);
    if (bytes.length > dataCapacity) {
      const msg = encoder.encode("response too large: " + bytes.length);
      data.set(msg);
      Atomics.store(header, offsetRespLen, -msg.length);
    } else {
      data.set(bytes);
      Atomics.store(header, offsetRespLen, bytes.length);
    }
    Atomics.store(signal, offsetSignal, signalIdle);
    Atomics.notify(signal, offsetSignal, 1);
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  function writeError(msg: any) {
    const bytes = encoder.encode(String(msg));
    const truncated = bytes.subarray(0, Math.min(bytes.length, dataCapacity));
    data.set(truncated);
    Atomics.store(header, offsetRespLen, -truncated.length);
    Atomics.store(signal, offsetSignal, signalIdle);
    Atomics.notify(signal, offsetSignal, 1);
  }

  // Liveness beacon so the main thread can detect a dead worker.
  setInterval(() => {
    Atomics.add(header, offsetLiveness, 1);
  }, livenessIntervalMs).unref();

  process.on("unhandledRejection", (err) => writeError("unhandledRejection: " + err));
  process.on("uncaughtException", (err) => writeError("uncaughtException: " + err));

  (async () => {
    // Wait for socket to connect before signalling ready.
    await new Promise((resolve, reject) => {
      socket.once("connect", resolve);
      socket.once("error", reject);
    });

    parentPort.postMessage("ready");

    for (;;) {
      // Block until the main thread signals a pending request.
      while (Atomics.load(signal, offsetSignal) !== signalPending) {
        Atomics.wait(signal, offsetSignal, signalIdle);
      }
      // Yield to let queued microtasks (e.g. orphaned rejections) drain first.
      await new Promise((r) => setImmediate(r));

      const reqLen = Atomics.load(header, offsetReqLen);
      const requestStr = decoder.decode(data.subarray(0, reqLen));

      try {
        const responseStr = await new Promise((resolve, reject) => {
          const timer = setTimeout(() => {
            pendingResolve = null;
            pendingReject = null;
            reject(new Error("debug server request timed out"));
          }, 30_000);

          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          pendingResolve = (result: any) => {
            clearTimeout(timer);
            resolve(result);
          };
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          pendingReject = (err: any) => {
            clearTimeout(timer);
            reject(err);
          };

          socket.write(requestStr + "\n");
        });

        writeResponse(responseStr);
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (e: any) {
        writeError(e && e.message ? e.message : String(e));
      }
    }
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  })().catch((err: any) => {
    // Connection failed during startup; signal main thread.
    writeError("worker startup failed: " + (err && err.message ? err.message : String(err)));
    process.exit(1);
  });
  /* eslint-enable */
}

const WORKER_SOURCE = `(${debugSocketWorkerMain.toString()})()`;

type State = {
  worker: Worker;
  header: Int32Array;
  signal: Int32Array;
  data: Uint8Array;
  dead: boolean;
  cause: Error | null;
};

let state: State | null = null;

const encoder = new TextEncoder();
const decoder = new TextDecoder();

const DATA_CAPACITY = DEFAULT_SAB_SIZE - HEADER_BYTES;

/**
 * Spawn the worker and wait for its TCP socket to connect.
 * Must be called (and awaited) before any `syncSend` call.
 */
export async function connectSyncSocket(port: number): Promise<void> {
  if (state && !state.dead) return;

  const sab = new SharedArrayBuffer(DEFAULT_SAB_SIZE);
  const signal = new Int32Array(sab, 0, 1);
  const header = new Int32Array(sab, 0, HEADER_BYTES / 4);
  const data = new Uint8Array(sab, HEADER_BYTES, DATA_CAPACITY);

  const worker = new Worker(WORKER_SOURCE, {
    eval: true,
    workerData: {
      sab,
      port,
      headerBytes: HEADER_BYTES,
      signalIdle: SIGNAL_IDLE,
      signalPending: SIGNAL_PENDING,
      livenessIntervalMs: LIVENESS_INTERVAL_MS,
      offsetSignal: OFFSET_SIGNAL,
      offsetReqLen: OFFSET_REQ_LEN,
      offsetRespLen: OFFSET_RESP_LEN,
      offsetLiveness: OFFSET_LIVENESS,
    },
  });

  worker.unref();

  const s: State = { worker, header, signal, data, dead: false, cause: null };

  worker.on("error", (err) => {
    s.dead = true;
    s.cause = err;
  });
  worker.on("exit", (code) => {
    s.dead = true;
    if (!s.cause && code !== 0)
      s.cause = new Error(`debug socket worker exited with code ${code}`);
  });

  state = s;

  // Wait for the worker to signal "ready" (TCP socket connected).
  await new Promise<void>((resolve, reject) => {
    const timeout = setTimeout(
      () => reject(new Error("debug socket worker did not become ready within 15 s")),
      15_000,
    );
    worker.once("message", (msg) => {
      clearTimeout(timeout);
      if (msg === "ready") resolve();
      else reject(new Error(`unexpected worker message: ${msg}`));
    });
    worker.once("error", (err) => {
      clearTimeout(timeout);
      reject(err);
    });
  });
}

/**
 * Send a JSON request to the debug server synchronously (blocks via Atomics.wait).
 * Returns the raw JSON response string.
 */
export function syncSend(request: Record<string, unknown>): string {
  if (!state || state.dead) {
    throw new Error(
      "debug socket not connected" + (state?.cause ? `: ${state.cause.message}` : ""),
    );
  }

  const reqBytes = encoder.encode(JSON.stringify(request));
  if (reqBytes.length > DATA_CAPACITY) {
    throw new Error(`debug request too large: ${reqBytes.length} bytes`);
  }

  state.data.set(reqBytes);
  Atomics.store(state.header, OFFSET_REQ_LEN, reqBytes.length);
  Atomics.store(state.signal, OFFSET_SIGNAL, SIGNAL_PENDING);
  Atomics.notify(state.signal, OFFSET_SIGNAL, 1);

  // Block until the worker finishes, with liveness heartbeating.
  let lastBeat = Atomics.load(state.header, OFFSET_LIVENESS);
  let stale = 0;
  for (;;) {
    const r = Atomics.wait(state.signal, OFFSET_SIGNAL, SIGNAL_PENDING, WORKER_HEARTBEAT_MS);
    if (r === "not-equal" || r === "ok") break;
    const beat = Atomics.load(state.header, OFFSET_LIVENESS);
    if (beat !== lastBeat) {
      lastBeat = beat;
      stale = 0;
      continue;
    }
    stale++;
    if (stale >= MAX_STALE_HEARTBEATS) {
      state.dead = true;
      if (!state.cause) {
        state.cause = new Error(
          `debug socket worker unresponsive for ${stale * WORKER_HEARTBEAT_MS} ms`,
        );
      }
      break;
    }
  }

  if (state.dead) {
    throw new Error(
      "debug socket worker died during request: " + (state.cause?.message ?? "unknown"),
    );
  }

  const respLen = Atomics.load(state.header, OFFSET_RESP_LEN);
  if (respLen < 0) {
    throw new Error(decoder.decode(state.data.subarray(0, -respLen)));
  }
  return decoder.decode(state.data.subarray(0, respLen));
}

/** Terminate the worker and release the socket. */
export function closeSyncSocket(): void {
  const w = state?.worker;
  state = null;
  try {
    w?.terminate();
  } catch {
    // ignore
  }
}
