/**
 * Regression tests for the DAP relay's `initialize` handshake and request
 * forwarding. These run against the bundle used by VSCode.
 *
 *   pnpm run build:dap && pnpm run test:dap
 */

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { chmod, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import * as net from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import { fileURLToPath } from "node:url";

const ADAPTER = fileURLToPath(new URL("../dist/debug.js", import.meta.url));
const REPLY_TIMEOUT_MS = 2_000;
const RELAY_TIMEOUT_MS = 4_000;

if (!existsSync(ADAPTER)) {
  throw new Error(
    `debug adapter bundle not found at ${ADAPTER}\n` +
      `Build it first: pnpm --dir components/clarity-vscode run build:dap`,
  );
}

/** Wrap a DAP message in the `Content-Length` framing VSCode uses. */
function frame(message) {
  const body = JSON.stringify(message);
  return `Content-Length: ${Buffer.byteLength(body, "utf8")}\r\n\r\n${body}`;
}

function extractMessages(buffer) {
  const messages = [];
  let pos = 0;

  while (pos < buffer.length) {
    const headerEnd = buffer.indexOf("\r\n\r\n", pos);
    if (headerEnd === -1) break;
    const header = buffer.subarray(pos, headerEnd).toString("ascii");
    const match = /Content-Length: (\d+)/i.exec(header);
    assert.ok(match, `missing Content-Length header: ${header}`);

    const bodyStart = headerEnd + 4;
    const bodyEnd = bodyStart + Number(match[1]);
    if (buffer.length < bodyEnd) break;
    messages.push(JSON.parse(buffer.subarray(bodyStart, bodyEnd).toString("utf8")));
    pos = bodyEnd;
  }

  return { messages, remaining: buffer.subarray(pos) };
}

const INITIALIZE = frame({
  seq: 1,
  type: "request",
  command: "initialize",
  arguments: { adapterID: "clarinet", clientID: "vscode", pathFormat: "path" },
});

/** Spawn the adapter. The child is killed when the test finishes. */
function spawnAdapter(t, options = {}) {
  const child = spawn(process.execPath, [ADAPTER], {
    stdio: ["pipe", "pipe", "pipe"],
    ...options,
  });
  let stderr = "";
  let stdout = Buffer.alloc(0);
  child.stderr.on("data", (chunk) => (stderr += chunk.toString("utf8")));
  child.stdout.on("data", (chunk) => (stdout = Buffer.concat([stdout, chunk])));
  t.after(() => child.kill("SIGKILL"));
  return {
    child,
    stderr: () => stderr,
    stdout: () => extractMessages(stdout).messages,
  };
}

/** Poll `predicate` until it holds or `ms` elapses. */
async function waitFor(predicate, ms) {
  const deadline = Date.now() + ms;
  while (Date.now() < deadline) {
    if (await predicate()) return true;
    await new Promise((r) => setTimeout(r, 25));
  }
  return false;
}

test("`initialize` is answered before the transport is known", async (t) => {
  const { child, stderr, stdout } = spawnAdapter(t);

  child.stdin.write(INITIALIZE);
  const answered = await waitFor(() => stdout().length === 1, REPLY_TIMEOUT_MS);
  assert.ok(answered, `the adapter did not answer initialize; stderr: ${stderr()}`);
  assert.deepEqual(stdout()[0], {
    seq: 1,
    type: "response",
    request_seq: 1,
    success: true,
    command: "initialize",
    body: {
      supportsConfigurationDoneRequest: true,
      supportsFunctionBreakpoints: true,
      supportsStepInTargetsRequest: true,
      supportTerminateDebuggee: true,
      supportsLoadedSourcesRequest: true,
      supportsDataBreakpoints: true,
      supportsBreakpointLocationsRequest: true,
    },
  });
});

// `initialize` is handled locally; only subsequent requests reach the adapter.
test("`attach` forwards requests and preserves unique output sequences", async (t) => {
  const chunks = [];
  const server = net.createServer((socket) => {
    socket.on("data", (chunk) => chunks.push(chunk.toString("utf8")));
    socket.write(frame({ seq: 0, type: "event", event: "initialized" }));
  });
  t.after(() => server.close());

  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  const { port } = server.address();

  const { child, stdout } = spawnAdapter(t);
  child.stdin.write(INITIALIZE);
  child.stdin.write(
    frame({ seq: 2, type: "request", command: "attach", arguments: { port } }),
  );

  const relayed = await waitFor(
    () => chunks.join("").includes('"attach"'),
    RELAY_TIMEOUT_MS,
  );

  assert.ok(
    relayed,
    "the relay never forwarded the buffered `attach` request to the DAP port; " +
      "`DAPDebugger::attach` is what initialises the debug state, so without it " +
      "the adapter panics on the first `setBreakpoints` or `disconnect`",
  );
  assert.ok(
    !chunks.join("").includes('"initialize"'),
    "the relay forwarded `initialize` to the adapter as well as answering it " +
      "locally; the editor would receive two responses for the same request seq",
  );
  const receivedBackendMessage = await waitFor(
    () => stdout().length === 2,
    RELAY_TIMEOUT_MS,
  );
  assert.ok(receivedBackendMessage, "the relay did not forward the adapter's output");
  assert.deepEqual(
    stdout().map(({ seq }) => seq),
    [1, 2],
  );
  assert.deepEqual(stdout()[1], {
    seq: 2,
    type: "event",
    event: "initialized",
  });
});

test("`launch` starts clarinet and forwards only the launch request", async (t) => {
  const dir = await mkdtemp(join(tmpdir(), "clarinet-dap-test-"));
  const input = join(dir, "input");
  const executable = join(dir, "clarinet");
  await writeFile(executable, `#!/bin/sh\ncat > "${input}"\n`);
  await chmod(executable, 0o755);
  t.after(() => rm(dir, { recursive: true, force: true }));

  const { child } = spawnAdapter(t, {
    env: { ...process.env, PATH: `${dir}:${process.env.PATH}` },
  });
  child.stdin.write(INITIALIZE);
  child.stdin.write(
    frame({ seq: 2, type: "request", command: "launch", arguments: {} }),
  );
  child.stdin.end();

  const relayed = await waitFor(async () => {
    try {
      return (await readFile(input, "utf8")).includes('"launch"');
    } catch {
      return false;
    }
  }, RELAY_TIMEOUT_MS);
  assert.ok(relayed, "the relay did not forward launch to clarinet dap");
  const forwarded = await readFile(input, "utf8");
  assert.ok(
    !forwarded.includes('"initialize"'),
    "the relay forwarded initialize to clarinet dap",
  );
});
