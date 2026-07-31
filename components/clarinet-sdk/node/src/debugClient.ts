import * as net from "net";
import { spawn, type ChildProcess } from "child_process";

import { Cl, type ClarityValue } from "@stacks/transactions";

export type DebugCallResult = {
  /** The Clarity return value as a human-readable string, e.g. `"(ok u1)"`. */
  value: string;
};

type PendingRequest = {
  resolve: (r: SdkResponse) => void;
  reject: (e: Error) => void;
};

type SdkResponse = {
  id: number;
  result?: { value: string };
  error?: string;
};

/**
 * A client that connects to a `clarinet dap --dap-port … --sdk-port …` server
 * and evaluates Clarity expressions under DAP debugger control.
 *
 * Breakpoints set in VSCode (or another DAP-capable editor) in `.clar` source
 * files will be hit when the corresponding contract code is reached.
 *
 * @example
 * ```ts
 * const debugger = await connectDebugServer();
 * const result = await debugger.callPublicFn("counter", "increment", [], deployer);
 * expect(result.value).toBe("(ok u1)");
 * await debugger.disconnect();
 * ```
 */
export class DebugClient {
  private readonly socket: net.Socket;
  private nextId = 1;
  private readonly pending = new Map<number, PendingRequest>();
  private buffer = "";
  /** Set when this client owns the `clarinet dap` process (via `startDebugServer`). */
  private readonly _process?: ChildProcess;

  constructor(socket: net.Socket, process?: ChildProcess) {
    this.socket = socket;
    this._process = process;

    socket.on("data", (chunk: Buffer) => {
      this.buffer += chunk.toString("utf8");
      const lines = this.buffer.split("\n");
      // Keep any incomplete line in the buffer
      this.buffer = lines.pop() ?? "";
      for (const line of lines) {
        const trimmed = line.trim();
        if (!trimmed) continue;
        try {
          const response = JSON.parse(trimmed) as SdkResponse;
          const pending = this.pending.get(response.id);
          if (pending) {
            this.pending.delete(response.id);
            pending.resolve(response);
          }
        } catch {
          // Ignore malformed lines
        }
      }
    });

    socket.on("error", (err: Error) => {
      for (const { reject } of this.pending.values()) {
        reject(err);
      }
      this.pending.clear();
    });
  }

  private send(request: Record<string, unknown>): Promise<SdkResponse> {
    const id = this.nextId++;
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
      this.socket.write(JSON.stringify({ ...request, id }) + "\n");
    });
  }

  /**
   * Call a public contract function through the debug server.
   * Breakpoints in the contract source will pause execution.
   */
  async callPublicFn(
    contract: string,
    method: string,
    args: ClarityValue[],
    sender: string,
  ): Promise<DebugCallResult> {
    const argStrings = args.map((a) => Cl.stringify(a));
    const response = await this.send({
      method: "call",
      contract,
      function: method,
      args: argStrings,
      sender,
    });
    if (response.error) throw new Error(response.error);
    return { value: response.result!.value };
  }

  /**
   * Call a read-only contract function through the debug server.
   * Behaves the same as `callPublicFn` for debugging purposes.
   */
  async callReadOnlyFn(
    contract: string,
    method: string,
    args: ClarityValue[],
    sender: string,
  ): Promise<DebugCallResult> {
    return this.callPublicFn(contract, method, args, sender);
  }

  /**
   * Evaluate an arbitrary Clarity snippet in the simnet session under the debugger.
   */
  async execute(snippet: string): Promise<DebugCallResult> {
    const response = await this.send({ method: "eval", snippet });
    if (response.error) throw new Error(response.error);
    return { value: response.result!.value };
  }

  /** Gracefully disconnect from the debug server. */
  async disconnect(): Promise<void> {
    try {
      await this.send({ method: "disconnect" });
    } finally {
      this.socket.destroy();
      this._process?.kill();
    }
  }
}

function openSocket(port: number): Promise<net.Socket> {
  return new Promise((resolve, reject) => {
    const socket = net.createConnection({ port, host: "127.0.0.1" }, () => {
      socket.removeListener("error", reject);
      resolve(socket);
    });
    socket.once("error", reject);
  });
}

/**
 * Connect to a running `clarinet dap` debug server.
 *
 * The port is resolved in order:
 * 1. The `port` argument
 * 2. The `CLARINET_DEBUG_PORT` environment variable
 * 3. The default of `7778`
 *
 * @example
 * ```ts
 * // In a Vitest test file
 * import { connectDebugServer } from "@stacks/clarinet-sdk";
 *
 * const debugClient = await connectDebugServer();
 * ```
 */
export async function connectDebugServer(port?: number): Promise<DebugClient> {
  const resolvedPort = port ?? Number(process.env["CLARINET_DEBUG_PORT"] ?? "7778");
  const socket = await openSocket(resolvedPort);
  return new DebugClient(socket);
}

/**
 * Spawn a `clarinet dap` server and return a connected {@link DebugClient}.
 *
 * This is the zero-configuration entry point: no terminal command is needed.
 * The server process is owned by the returned client and is killed automatically
 * when {@link DebugClient.disconnect} is called.
 *
 * When `dapPort` is supplied the server also listens for a DAP client (e.g.
 * VSCode) on that port so breakpoints in `.clar` files will be hit. Without
 * `dapPort` the server runs in SDK-only mode: execution is uninterrupted but
 * the full `DebugClient` API is still available.
 *
 * @example
 * ```ts
 * // SDK-only mode (no VSCode attachment needed)
 * const client = await startDebugServer({ manifest: "./Clarinet.toml" });
 * const result = await client.callPublicFn("counter", "increment", [], deployer);
 * await client.disconnect();
 *
 * // With breakpoints - attach VSCode to dapPort (7777) after calling this
 * const client = await startDebugServer({ dapPort: 7777, sdkPort: 7778 });
 * ```
 */
export async function startDebugServer(options?: {
  /** Path to Clarinet.toml. Defaults to `"./Clarinet.toml"`. */
  manifest?: string;
  /** SDK port for this client to connect on. Defaults to `7778`. */
  sdkPort?: number;
  /**
   * When set, the server also accepts a DAP client (e.g. VSCode) on this port,
   * enabling breakpoint debugging. When omitted the server runs in SDK-only mode.
   */
  dapPort?: number;
}): Promise<DebugClient> {
  const sdkPort = options?.sdkPort ?? 7778;
  const manifest = options?.manifest ?? "./Clarinet.toml";

  const args = ["dap", "--sdk-port", String(sdkPort), "--manifest", manifest];
  if (options?.dapPort != null) {
    args.push("--dap-port", String(options.dapPort));
  }

  const child = spawn("clarinet", args, {
    stdio: ["ignore", "ignore", "pipe"],
  });

  // Wait for the ready signal printed to stderr by run_dap_server.
  await new Promise<void>((resolve, reject) => {
    const readyToken = `CLARINET_DAP_SDK_READY:${sdkPort}`;
    let stderrBuf = "";
    const timeout = setTimeout(
      () => reject(new Error("clarinet dap server did not start within 15 s")),
      15_000,
    );

    child.stderr!.on("data", (chunk: Buffer) => {
      stderrBuf += chunk.toString("utf8");
      if (stderrBuf.includes(readyToken)) {
        clearTimeout(timeout);
        resolve();
      }
    });

    child.on("error", (err) => {
      clearTimeout(timeout);
      reject(new Error(`failed to spawn clarinet: ${err.message}`));
    });

    child.on("exit", (code) => {
      clearTimeout(timeout);
      reject(new Error(`clarinet dap exited unexpectedly with code ${code}`));
    });
  });

  const socket = await openSocket(sdkPort);
  return new DebugClient(socket, child);
}
