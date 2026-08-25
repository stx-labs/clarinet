/**
 * Regression test for finding 1 of the PR #2483 review: the DAP adapter relay
 * must answer `initialize`.
 *
 * VSCode launches this file as the debug adapter for both `program`-based
 * configurations (`debuggers[].program = ./debug/dist/debug.js` in
 * package.json): the pre-existing "Clarinet Debugger" launch config and the new
 * "Clarinet Debugger: Attach" snippet. It sends `initialize` first and awaits
 * the **response** before sending `launch` or `attach`. The rewritten relay acts
 * only on `launch`/`attach`, so `initialize` falls through both branches and
 * nothing is ever written to stdout — the session hangs with an empty Debug
 * Console.
 *
 * On `main` this file spawned `clarinet dap` immediately with inherited stdio,
 * so `initialize` was answered by the Rust side. The buffering rewrite removed
 * the only thing that could answer it.
 *
 * Tests the built bundle rather than the source, matching the convention in
 * `clarinet-sdk/node/tests` ("test the built package and not the source code")
 * and because the bundle is literally what VSCode executes.
 *
 *   pnpm run build:dap && pnpm run test:dap
 */

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { existsSync } from "node:fs";
import * as net from "node:net";
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

const INITIALIZE = frame({
  seq: 1,
  type: "request",
  command: "initialize",
  arguments: { adapterID: "clarinet", clientID: "vscode", pathFormat: "path" },
});

/** Spawn the adapter. The child is killed when the test finishes. */
function spawnAdapter(t) {
  const child = spawn(process.execPath, [ADAPTER], {
    stdio: ["pipe", "pipe", "pipe"],
  });
  let stderr = "";
  child.stderr.on("data", (chunk) => (stderr += chunk.toString("utf8")));
  t.after(() => child.kill("SIGKILL"));
  return { child, stderr: () => stderr };
}

/** Resolve with whatever the adapter writes to stdout, or `""` on timeout. */
function readStdout(child, ms = REPLY_TIMEOUT_MS) {
  return new Promise((resolve) => {
    let out = "";
    const timer = setTimeout(() => resolve(out), ms);
    child.stdout.on("data", (chunk) => {
      out += chunk.toString("utf8");
      clearTimeout(timer);
      resolve(out);
    });
  });
}

/** Poll `predicate` until it holds or `ms` elapses. */
async function waitFor(predicate, ms) {
  const deadline = Date.now() + ms;
  while (Date.now() < deadline) {
    if (predicate()) return true;
    await new Promise((r) => setTimeout(r, 25));
  }
  return false;
}

test("`initialize` is answered before the transport is known", async (t) => {
  const { child, stderr } = spawnAdapter(t);

  child.stdin.write(INITIALIZE);
  const reply = await readStdout(child);

  assert.notEqual(
    reply,
    "",
    `the adapter wrote nothing to stdout within ${REPLY_TIMEOUT_MS}ms of receiving ` +
      `\`initialize\`.\nVSCode awaits the initialize *response* before it sends ` +
      `\`launch\` or \`attach\`, so both \`program\`-based debug configurations ` +
      `deadlock with an empty Debug Console.\nadapter stderr: ${JSON.stringify(stderr())}`,
  );
  assert.match(
    reply,
    /Content-Length:/,
    `expected a Content-Length-framed DAP message, got: ${JSON.stringify(reply)}`,
  );
  assert.match(
    reply,
    /"type"\s*:\s*"response"/,
    `expected a DAP response, got: ${JSON.stringify(reply)}`,
  );
});

/**
 * Control: proves the harness itself is sound and isolates the defect above to
 * the missing `initialize` response. Once `attach` arrives the relay does open
 * the socket and replay the buffered stdin — including the `initialize` bytes it
 * never answered. This test passes today.
 */
test("control: `attach` opens the socket and replays buffered stdin", async (t) => {
  const chunks = [];
  const server = net.createServer((socket) => {
    socket.on("data", (chunk) => chunks.push(chunk.toString("utf8")));
  });
  t.after(() => server.close());

  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  const { port } = server.address();

  const { child } = spawnAdapter(t);
  child.stdin.write(INITIALIZE);
  child.stdin.write(
    frame({ seq: 2, type: "request", command: "attach", arguments: { port } }),
  );

  const relayed = await waitFor(
    () => chunks.join("").includes('"initialize"'),
    RELAY_TIMEOUT_MS,
  );

  assert.ok(
    relayed,
    "the relay never forwarded the buffered `initialize` to the DAP port; " +
      "the test harness or the attach branch is broken, not just the " +
      "initialize response",
  );
});
