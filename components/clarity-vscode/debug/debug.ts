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

// Buffer stdin until we can determine whether this is a launch or attach session.
const inputChunks: Buffer[] = [];
let accumulated = Buffer.alloc(0);
let handled = false;

const onData = async (chunk: Buffer) => {
  if (handled) return;
  inputChunks.push(chunk);
  accumulated = Buffer.concat([accumulated, chunk]);

  const { messages } = extractMessages(accumulated);

  for (const msg of messages) {
    if (msg.type !== "request") continue;

    if (msg.command === "launch") {
      handled = true;
      process.stdin.removeListener("data", onData);
      process.stdin.pause();

      // Only look up clarinet when actually needed for launch mode.
      const clarinet = await lookpath("clarinet");
      if (!clarinet) {
        process.stderr.write("[clarinet-dap] 'clarinet' not found in PATH\n");
        process.exit(1);
      }

      const dap = spawn(clarinet, ["dap"], { stdio: "pipe" });
      for (const buf of inputChunks) {
        dap.stdin!.write(buf);
      }
      process.stdin.resume();
      process.stdin.pipe(dap.stdin!);
      dap.stdout!.pipe(process.stdout);
      dap.stderr!.pipe(process.stderr);
      dap.on("exit", (code) => process.exit(code ?? 0));
      return;
    }

    if (msg.command === "attach") {
      handled = true;
      const port: number = (msg.arguments as any)?.port ?? 7777;
      process.stdin.removeListener("data", onData);

      const socket = net.createConnection({ port, host: "127.0.0.1" }, () => {
        // Replay buffered stdin (initialize + attach messages).
        for (const buf of inputChunks) {
          socket.write(buf);
        }
        // Forward future stdin directly to socket and socket responses to stdout.
        process.stdin.on("data", (chunk: Buffer) => socket.write(chunk));
        socket.on("data", (chunk: Buffer) => process.stdout.write(chunk));
      });

      socket.on("error", (err: Error) => {
        process.stderr.write(`[clarinet-dap] failed to connect to port ${port}: ${err.message}\n`);
        process.exit(1);
      });

      socket.on("close", () => process.exit(0));
      return;
    }
  }
};

process.stdin.on("data", onData);
