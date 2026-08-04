/**
 * counter.codelens.test.ts — CodeLens debug demo
 *
 * This file uses `startDebugServer`, which makes the Clarity VSCode extension
 * show a "Debug with Clarinet" button above each test below.
 *
 * Clicking that button will:
 *   1. Spawn `clarinet dap` on ephemeral ports
 *   2. Attach the VSCode debugger (no launch.json needed)
 *   3. Run this specific test in a terminal with CLARINET_DEBUG_PORT set
 *      so that `startDebugServer` connects to the extension-managed server
 *
 * Set a breakpoint in contracts/counter.clar before clicking the button.
 */

import { describe, it, expect } from "vitest";
import { startDebugServer } from "@stacks/clarinet-sdk";
import { Cl } from "@stacks/transactions";

const deployer = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

describe("counter - CodeLens flow", () => {
  it("increments from zero", async () => {
    const client = await startDebugServer();

    const result = await client.callPublicFn("counter", "increment", [], deployer);
    expect(result.value).toBe("(ok u1)");

    const count = await client.callReadOnlyFn("counter", "get-count", [], deployer);
    expect(count.value).toBe("u1");

    await client.disconnect();
  });

  it("adds a specific amount", async () => {
    const client = await startDebugServer();

    await client.callPublicFn("counter", "increment", [], deployer);
    const result = await client.callPublicFn("counter", "add", [Cl.uint(9)], deployer);
    expect(result.value).toBe("(ok u10)");

    await client.disconnect();
  });

  it("resets to zero", async () => {
    const client = await startDebugServer();

    await client.callPublicFn("counter", "increment", [], deployer);
    await client.callPublicFn("counter", "reset", [], deployer);

    const count = await client.callReadOnlyFn("counter", "get-count", [], deployer);
    expect(count.value).toBe("u0");

    await client.disconnect();
  });
});
