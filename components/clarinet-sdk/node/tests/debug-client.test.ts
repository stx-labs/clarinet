import * as net from "node:net";
import { afterEach, describe, expect, it } from "vitest";
import { Cl } from "@stacks/transactions";

import { startDebugServer } from "../src/debugClient";

type SdkRequest = {
  id: number;
  method: string;
  contract?: string;
  function?: string;
  args?: string[];
  sender?: string;
  snippet?: string;
};

type SdkResponse = {
  id: number;
  result?: { value: string } | null;
  error?: string;
};

const servers: net.Server[] = [];

function closeServer(server: net.Server): Promise<void> {
  if (!server.listening) return Promise.resolve();

  const { promise, resolve, reject } = Promise.withResolvers<void>();
  server.close((err) => {
    if (err) reject(err);
    else resolve();
  });
  return promise;
}

afterEach(async () => {
  await Promise.all(servers.splice(0).map(closeServer));
});

async function startMockSdkServer(
  handle: (request: SdkRequest) => SdkResponse,
): Promise<{ port: number; requests: SdkRequest[] }> {
  const requests: SdkRequest[] = [];
  const server = net.createServer((socket) => {
    let buffer = "";
    socket.on("data", (chunk: Buffer) => {
      buffer += chunk.toString("utf8");
      const lines = buffer.split("\n");
      buffer = lines.pop() ?? "";

      for (const line of lines) {
        const trimmed = line.trim();
        if (!trimmed) continue;
        const request = JSON.parse(trimmed) as SdkRequest;
        requests.push(request);
        socket.write(`${JSON.stringify(handle(request))}\n`);
      }
    });
  });

  servers.push(server);
  const { promise, resolve } = Promise.withResolvers<void>();
  server.listen(0, "127.0.0.1", resolve);
  await promise;
  const address = server.address();
  if (!address || typeof address === "string") {
    throw new Error("mock SDK server did not bind a TCP port");
  }
  return { port: address.port, requests };
}

describe("DebugClient", () => {
  it("connects to an existing SDK server and sends framed call requests", async () => {
    const { port, requests } = await startMockSdkServer((request) => ({
      id: request.id,
      result: request.method === "disconnect" ? null : { value: "(ok u42)" },
    }));

    const client = await startDebugServer({ port });
    const result = await client.callPublicFn(
      "counter",
      "increment",
      [Cl.uint(1)],
      "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM",
    );
    await client.disconnect();

    expect(result).toEqual({ value: "(ok u42)" });
    expect(requests).toEqual([
      {
        id: 1,
        method: "call",
        contract: "counter",
        function: "increment",
        args: ["u1"],
        sender: "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM",
      },
      { id: 2, method: "disconnect" },
    ]);
  });

  it("propagates SDK errors from execute", async () => {
    const { port } = await startMockSdkServer((request) => ({
      id: request.id,
      ...(request.method === "disconnect"
        ? { result: null }
        : { error: "boom" }),
    }));

    const client = await startDebugServer({ port });
    await expect(client.execute("(+ 1 true)")).rejects.toThrow("boom");
    await client.disconnect();
  });
});
