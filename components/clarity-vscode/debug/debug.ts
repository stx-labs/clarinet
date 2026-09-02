import { lookpath } from "lookpath";
import { spawn } from "child_process";
import * as net from "net";

// Pull complete Content-Length–framed DAP messages from a buffer.
function extractMessages(buffer: Buffer): { messages: any[]; remaining: Buffer } {
  const messages: any[] = [];
  let pos = 0;

  while (pos < buffer.length) {
    const headerEnd = buffer.indexOf("\r\n\r\n", pos);
    if (headerEnd === -1) break;

    const header = buffer.subarray(pos, headerEnd).toString("ascii");
    const match = /Content-Length: (\d+)/i.exec(header);
    if (!match) {
      pos = headerEnd + 4;
      continue;
    }

    const contentLength = Number(match[1]);
    const bodyStart = headerEnd + 4;
    const bodyEnd = bodyStart + contentLength;

    if (buffer.length < bodyEnd) break;

    try {
      messages.push(JSON.parse(buffer.subarray(bodyStart, bodyEnd).toString("utf8")));
    } catch {
      // Skip malformed messages
    }
    pos = bodyEnd;
  }

  return { messages, remaining: buffer.subarray(pos) };
}

/** Encode a DAP message for the transport. */
function frame(message: unknown): Buffer {
  const body = Buffer.from(JSON.stringify(message), "utf8");
  return Buffer.concat([
    Buffer.from(`Content-Length: ${body.length}\r\n\r\n`, "ascii"),
    body,
  ]);
}

// The relay answers `initialize` because the client waits for it before sending
// the `launch` or `attach` request that selects the transport. Keep this response
// in sync with the Rust adapter.
const INITIALIZE_BODY = {
  supportsConfigurationDoneRequest: true,
  supportsFunctionBreakpoints: true,
  supportsStepInTargetsRequest: true,
  supportTerminateDebuggee: true,
  supportsLoadedSourcesRequest: true,
  supportsDataBreakpoints: true,
  supportsBreakpointLocationsRequest: true,
};

// Complete messages and trailing input buffered until the transport opens.
const forward: Buffer[] = [];
// Explicit type accommodates the result of Buffer.subarray().
let accumulated: Buffer = Buffer.alloc(0);
let handled = false;
let sendSeq = 1;

function answerInitialize(requestSeq: number) {
  process.stdout.write(
    frame({
      seq: sendSeq++,
      type: "response",
      request_seq: requestSeq,
      success: true,
      command: "initialize",
      body: INITIALIZE_BODY,
    }),
  );
}

function relayOutput(source: NodeJS.ReadableStream) {
  let buffered: Buffer = Buffer.alloc(0);
  source.on("data", (chunk: Buffer) => {
    buffered = Buffer.concat([buffered, chunk]);
    const { messages, remaining } = extractMessages(buffered);
    buffered = remaining;

    for (const message of messages) {
      message.seq = sendSeq++;
      process.stdout.write(frame(message));
    }
  });
}

/** Replay buffered input to the transport. */
function replay(write: (chunk: Buffer) => void) {
  for (const message of forward) write(message);
  forward.length = 0;
  if (accumulated.length > 0) {
    write(accumulated);
    accumulated = Buffer.alloc(0);
  }
}

async function startLaunch() {
  // Only look up clarinet when actually needed for launch mode.
  const clarinet = await lookpath("clarinet");
  if (!clarinet) {
    process.stderr.write("[clarinet-dap] 'clarinet' not found in PATH\n");
    process.exit(1);
  }

  const dap = spawn(clarinet, ["dap"], { stdio: "pipe" });
  replay((chunk) => dap.stdin!.write(chunk));
  process.stdin.resume();
  process.stdin.pipe(dap.stdin!);
  relayOutput(dap.stdout!);
  dap.stderr!.pipe(process.stderr);
  dap.on("exit", (code) => process.exit(code ?? 0));
}

function startAttach(port: number) {
  const socket = net.createConnection({ port, host: "127.0.0.1" }, () => {
    replay((chunk) => socket.write(chunk));
    process.stdin.resume();
    process.stdin.pipe(socket);
    relayOutput(socket);
  });

  socket.on("error", (err: Error) => {
    process.stderr.write(`[clarinet-dap] failed to connect to port ${port}: ${err.message}\n`);
    process.exit(1);
  });

  socket.on("close", () => process.exit(0));
}

const onData = async (chunk: Buffer) => {
  if (handled) return;
  accumulated = Buffer.concat([accumulated, chunk]);

  const { messages, remaining } = extractMessages(accumulated);
  accumulated = remaining;

  // Classify the batch first so coalesced messages are not dropped.
  let transport: { kind: "launch" } | { kind: "attach"; port: number } | undefined;
  for (const msg of messages) {
    if (msg?.type === "request" && msg.command === "initialize") {
      answerInitialize(msg.seq);
      continue;
    }

    forward.push(frame(msg));

    if (transport || msg?.type !== "request") continue;
    if (msg.command === "launch") {
      transport = { kind: "launch" };
    } else if (msg.command === "attach") {
      transport = { kind: "attach", port: (msg.arguments as any)?.port ?? 7777 };
    }
  }

  if (!transport) return;

  // Pause stdin while the transport opens so new input stays buffered in order.
  handled = true;
  process.stdin.removeListener("data", onData);
  process.stdin.pause();

  if (transport.kind === "launch") {
    await startLaunch();
  } else {
    startAttach(transport.port);
  }
};

process.stdin.on("data", onData);
