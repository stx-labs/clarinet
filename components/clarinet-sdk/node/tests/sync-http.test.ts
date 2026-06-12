import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { spawn, spawnSync, ChildProcess } from "node:child_process";
import { describe, expect, it, beforeAll, afterAll } from "vitest";

type SyncHttpRequest = (
  url: string,
  headersJson: string,
) => { status: number; statusText: string; headers: Record<string, string>; body: string };
type SyncHttpModule = {
  syncHttpRequest: SyncHttpRequest;
  syncSleep: (ms: number) => void;
  closeWorker: () => void;
};

function locateSyncHttpCjs(): string {
  const pkgPath = require.resolve("@stacks/clarinet-sdk-wasm/package.json");
  const snippetsRoot = path.join(path.dirname(pkgPath), "snippets");
  const stack: string[] = [snippetsRoot];
  while (stack.length > 0) {
    const dir = stack.pop()!;
    for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
      const p = path.join(dir, entry.name);
      if (entry.isDirectory()) stack.push(p);
      else if (entry.isFile() && entry.name === "sync_http.cjs") return p;
    }
  }
  throw new Error(`sync_http.cjs not found under ${snippetsRoot}`);
}

const SYNC_HTTP_PATH = locateSyncHttpCjs();
const loadSyncHttp = (): SyncHttpModule => require(SYNC_HTTP_PATH);

// syncHttpRequest blocks the calling thread via Atomics.wait. We therefore can't
// host the mock HTTP server in the same Node process — its event loop would be
// frozen while we wait. Run the server in a detached child instead, and route
// per-test behaviour via the request URL.
// Use os.tmpdir() so a crashed test never leaves an untracked file in the repo;
// mkdtempSync gives a unique directory per process so parallel vitest runs
// don't fight over the same path.
const TMP_DIR = fs.mkdtempSync(path.join(os.tmpdir(), "clarinet-sync-http-"));
const RECORDED_HEADERS_PATH = path.join(TMP_DIR, "recorded-headers.json");
let mockChild: ChildProcess;
let mockBaseUrl: string;

function spawnMockServer(): Promise<{ url: string; child: ChildProcess }> {
  const child = spawn(
    process.execPath,
    [
      "-e",
      `
      const http = require('node:http');
      const fs = require('node:fs');
      const RECORD = ${JSON.stringify(RECORDED_HEADERS_PATH)};
      const big = 'x'.repeat(200 * 1024);
      const server = http.createServer((req, res) => {
        const path = req.url || '/';
        if (path.startsWith('/record')) {
          try { fs.writeFileSync(RECORD, JSON.stringify(req.headers)); } catch (_) {}
          res.writeHead(200, 'OK', { 'content-type': 'application/json' });
          res.end(JSON.stringify({ ok: true }));
          return;
        }
        if (path.startsWith('/big')) {
          res.writeHead(200, 'OK', { 'content-type': 'text/plain' });
          res.end(big);
          return;
        }
        if (path.startsWith('/marker')) {
          res.writeHead(200, 'OK', { 'content-type': 'application/json', 'x-marker': 'hello' });
          res.end(JSON.stringify({ ok: true, n: 42 }));
          return;
        }
        res.writeHead(200, 'OK', { 'content-type': 'application/json' });
        res.end(JSON.stringify({ ok: true }));
      });
      server.listen(0, '127.0.0.1', () => {
        const addr = server.address();
        process.stdout.write('READY:http://127.0.0.1:' + addr.port + '\\n');
      });
      `,
    ],
    { stdio: ["ignore", "pipe", "pipe"] },
  );

  return new Promise((resolve) => {
    let buf = "";
    child.stdout!.on("data", (chunk) => {
      buf += chunk.toString();
      const m = buf.match(/READY:(http:\/\/[^\s]+)/);
      if (m) resolve({ url: m[1]!, child });
    });
    child.stderr!.on("data", (c) => process.stderr.write(c));
  });
}

beforeAll(async () => {
  const r = await spawnMockServer();
  mockBaseUrl = r.url;
  mockChild = r.child;
});

afterAll(() => {
  // Terminate the in-process worker so vitest's fork can exit cleanly.
  try {
    loadSyncHttp().closeWorker();
  } catch {}
  mockChild.kill();
  try {
    fs.rmSync(TMP_DIR, { recursive: true, force: true });
  } catch {}
});

describe("sync_http glue", () => {
  it("returns body, headers and status text on 200", () => {
    const { syncHttpRequest } = loadSyncHttp();
    const r = syncHttpRequest(
      mockBaseUrl + "/marker",
      JSON.stringify({ Accept: "application/json" }),
    );
    expect(r.status).toBe(200);
    expect(r.statusText).toBe("OK");
    expect(r.headers["x-marker"]).toBe("hello");
    expect(JSON.parse(r.body)).toEqual({ ok: true, n: 42 });
  });

  it("forwards HIRO_API_KEY as x-api-key", () => {
    // The env var is read at module load time, so we spawn a fresh Node to load
    // sync_http.cjs with the env applied.
    try {
      fs.unlinkSync(RECORDED_HEADERS_PATH);
    } catch {}
    const out = spawnSync(
      process.execPath,
      [
        "-e",
        `const { syncHttpRequest } = require(${JSON.stringify(SYNC_HTTP_PATH)});
         const r = syncHttpRequest(${JSON.stringify(mockBaseUrl + "/record")}, "{}");
         process.stdout.write(String(r.status));`,
      ],
      { env: { ...process.env, HIRO_API_KEY: "test-key-abc" } },
    );
    expect(out.status, out.stderr.toString()).toBe(0);
    expect(out.stdout.toString()).toBe("200");
    expect(fs.existsSync(RECORDED_HEADERS_PATH)).toBe(true);
    const recorded = JSON.parse(fs.readFileSync(RECORDED_HEADERS_PATH, "utf-8"));
    expect(recorded["x-api-key"]).toBe("test-key-abc");
  });

  it("returns a clear error when the response exceeds the SAB capacity", () => {
    // 64 KiB SAB → ~63 KiB data area; the 200 KiB body must overflow it.
    const out = spawnSync(
      process.execPath,
      [
        "-e",
        `const { syncHttpRequest } = require(${JSON.stringify(SYNC_HTTP_PATH)});
         try {
           syncHttpRequest(${JSON.stringify(mockBaseUrl + "/big")}, "{}");
           console.error("expected throw");
           process.exit(2);
         } catch (e) {
           process.stdout.write(e.message);
         }`,
      ],
      { env: { ...process.env, CLARINET_SDK_SAB_SIZE: String(64 * 1024) } },
    );
    expect(out.status, out.stderr.toString()).toBe(0);
    expect(out.stdout.toString()).toMatch(/response too large: \d+ bytes, capacity \d+/);
  });

  it("survives a transport error and serves the next request", () => {
    const { syncHttpRequest } = loadSyncHttp();

    // First call: TCP connect to a privileged port nothing is bound to. Linux/macOS
    // refuse the connection immediately, which is exactly the kind of transport
    // failure we want to exercise. (.invalid TLDs work in theory but real-world
    // resolvers stall for seconds before returning NXDOMAIN.)
    expect(() =>
      syncHttpRequest(
        "http://127.0.0.1:1/x",
        JSON.stringify({ Accept: "application/json" }),
      ),
    ).toThrow();

    // Second call: the same worker must still answer correctly.
    const r = syncHttpRequest(
      mockBaseUrl + "/healthcheck",
      JSON.stringify({ Accept: "application/json" }),
    );
    expect(r.status).toBe(200);
    expect(JSON.parse(r.body)).toEqual({ ok: true });
  });

  // Regression: the original deadlock surfaced as "success → transport error →
  // success" hanging on the third call. The first two calls always completed;
  // the third blocked forever in Atomics.wait because the worker had died on
  // the fetch-fail path and main couldn't run worker.on('error')/'exit' from
  // inside a synchronous wait. The worker-side unhandledRejection net keeps the
  // worker alive through that path; if it ever regresses, the liveness check
  // turns the hang into an error instead of a deadlock.
  it("handles success → transport error → success in sequence (no deadlock)", () => {
    const { syncHttpRequest } = loadSyncHttp();

    const r1 = syncHttpRequest(
      mockBaseUrl + "/marker",
      JSON.stringify({ Accept: "application/json" }),
    );
    expect(r1.status).toBe(200);

    expect(() =>
      syncHttpRequest(
        "http://127.0.0.1:1/x",
        JSON.stringify({ Accept: "application/json" }),
      ),
    ).toThrow();

    const r3 = syncHttpRequest(
      mockBaseUrl + "/marker",
      JSON.stringify({ Accept: "application/json" }),
    );
    expect(r3.status).toBe(200);
    expect(r3.statusText).toBe("OK");
    expect(JSON.parse(r3.body)).toEqual({ ok: true, n: 42 });
  });

  // Regression: a worker that boots but never serves (thread died, failed to
  // start, OOM, native crash) must surface as an error, not an infinite hang.
  // worker.on('error')/'exit' can't fire while main is blocked in Atomics.wait,
  // so detection relies on the worker's SAB liveness counter staying frozen.
  // The STALL_WORKER env hook makes the worker return before serving or
  // starting its liveness beacon — exactly the frozen-counter shape.
  it("throws instead of hanging when the worker is dead", () => {
    const out = spawnSync(
      process.execPath,
      [
        "-e",
        `const { syncHttpRequest } = require(${JSON.stringify(SYNC_HTTP_PATH)});
         try {
           syncHttpRequest(${JSON.stringify(mockBaseUrl + "/marker")}, "{}");
           console.error("expected throw");
           process.exit(2);
         } catch (e) {
           process.stdout.write(e.message);
         }`,
      ],
      // Death is declared after MAX_STALE_HEARTBEATS × WORKER_HEARTBEAT_MS
      // (~5s); the spawnSync timeout is the deadlock backstop.
      {
        env: { ...process.env, CLARINET_SDK_TEST_SYNC_HTTP_STALL_WORKER: "1" },
        timeout: 25_000,
      },
    );
    expect(out.status, out.stderr.toString()).toBe(0);
    expect(out.stdout.toString()).toMatch(/worker died during request: worker unresponsive/);
  }, 30_000);
});
