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

/** Wrap a DAP message in the Content-Length framing used on the wire. */
function frame(message: unknown): Buffer {
  const body = Buffer.from(JSON.stringify(message), "utf8");
  return Buffer.concat([
    Buffer.from(`Content-Length: ${body.length}\r\n\r\n`, "ascii"),
    body,
  ]);
}

/**
 * Body of our `initialize` response.
 *
 * This relay owns the connection to the editor, so it has to answer
 * `initialize` itself: the editor waits for that response before it sends
 * `launch` or `attach`, and until one of those arrives we cannot know which
 * transport to open. Answering it is also required of any adapter by the DAP
 * spec.
 *
 * `initialize` is deliberately *not* forwarded once a transport opens.
 * `DAPDebugger::initialize` (clarity-repl/src/repl/debug/dap/mod.rs) only writes
 * this response and sets no state, and neither `init()` (which waits for
 * `launch`) nor `init_attach()` (which waits for `configurationDone`) depends on
 * having seen it — so consuming it here cannot leave the adapter half-configured,
 * and the editor never receives two responses for the same request.
 *
 * The shape mirrors what `clarity-repl` emits, including the extra
 * `capabilities` nesting: `debug_types::InitializeResponse` is a struct with a
 * `capabilities` field and is not flattened, so the Rust adapter puts the
 * capabilities one level deeper than the DAP spec does. Emitting the
 * spec-correct shape here would make this relay advertise capabilities the
 * stdio adapter does not, changing the request sequence the editor uses — in
 * particular whether it sends `configurationDone`, which `threads()` currently
 * compensates for. That is an interactive path with no automated coverage, so
 * the two are kept identical and should be corrected together.
 */
const INITIALIZE_BODY = {
  capabilities: {
    supportsConfigurationDoneRequest: true,
    supportsFunctionBreakpoints: true,
    supportsStepInTargetsRequest: true,
    supportTerminateDebuggee: true,
    supportsLoadedSourcesRequest: true,
    supportsDataBreakpoints: true,
    supportsBreakpointLocationsRequest: true,
  },
};

// Messages to hand to the transport once it exists, plus any trailing partial
// message still accumulating.
const forward: Buffer[] = [];
// Annotated as `Buffer` (i.e. `Buffer<ArrayBufferLike>`) rather than inferred:
// `Buffer.alloc` returns `Buffer<ArrayBuffer>`, which `subarray`'s
// `Buffer<ArrayBufferLike>` result is not assignable to.
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

/** Hand everything buffered so far to a newly opened transport, in order. */
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
  dap.stdout!.pipe(process.stdout);
  dap.stderr!.pipe(process.stderr);
  dap.on("exit", (code) => process.exit(code ?? 0));
}

function startAttach(port: number) {
  const socket = net.createConnection({ port, host: "127.0.0.1" }, () => {
    replay((chunk) => socket.write(chunk));
    process.stdin.resume();
    process.stdin.pipe(socket);
    socket.pipe(process.stdout);
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

  // Classify the whole batch before opening anything, so messages that share a
  // chunk with `launch`/`attach` are still forwarded rather than dropped.
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

  // Stop reading stdin while the transport opens. Pausing (rather than only
  // removing the listener) keeps anything that arrives in the meantime buffered
  // by Node instead of being emitted and discarded; `replay` then `resume`
  // hand it over in order.
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
