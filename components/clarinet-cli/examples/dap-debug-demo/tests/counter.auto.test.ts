/**
 * counter.auto.test.ts — startDebugServer() demo
 *
 * The SDK spawns `clarinet dap` automatically. No terminal command, no
 * launch.json, no VSCode extension required.
 *
 * To exercise breakpoints:
 *   1. Open contracts/counter.clar and set a breakpoint (e.g. on the `asserts!` line).
 *   2. Pass dapPort to startDebugServer so a DAP listener is also started.
 *   3. Attach VSCode: Run > Start Debugging > "Clarinet DAP (attach)" pointing at that port.
 *   4. Run this test normally (`pnpm test`) - execution will pause at the breakpoint.
 */

import { describe, it, expect } from "vitest";
import { startDebugServer } from "@stacks/clarinet-sdk";
import { Cl } from "@stacks/transactions";

const deployer = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

describe("counter - auto server", () => {
  it("increments from zero", async () => {
    const client = await startDebugServer({ manifest: "./Clarinet.toml" });

    const result = await client.callPublicFn("counter", "increment", [], deployer);
    expect(result.value).toBe("(ok u1)");

    const count = await client.callReadOnlyFn("counter", "get-count", [], deployer);
    expect(count.value).toBe("u1");

    await client.disconnect();
  });

  it("adds a specific amount", async () => {
    const client = await startDebugServer({ manifest: "./Clarinet.toml" });

    await client.callPublicFn("counter", "increment", [], deployer);
    const result = await client.callPublicFn("counter", "add", [Cl.uint(9)], deployer);
    expect(result.value).toBe("(ok u10)");

    await client.disconnect();
  });

  it("resets to zero", async () => {
    const client = await startDebugServer({ manifest: "./Clarinet.toml" });

    await client.callPublicFn("counter", "increment", [], deployer);
    await client.callPublicFn("counter", "reset", [], deployer);

    const count = await client.callReadOnlyFn("counter", "get-count", [], deployer);
    expect(count.value).toBe("u0");

    await client.disconnect();
  });
});
