/**
 * counter.test.ts — startDebugServer() demo
 *
 * Running `pnpm test` spawns `clarinet dap` automatically
 *
 * The VSCode Clarity extension also shows a "Debug with Clarinet" CodeLens
 * button above each test block.
 */

import { describe, it, expect } from "vitest";
import { startDebugServer } from "@stacks/clarinet-sdk";
import { Cl } from "@stacks/transactions";

const deployer = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

// These tests spawn a `clarinet dap` process and require the binary in PATH.
// Skip in CI where the binary is not available.
describe.skipIf(!!process.env.CI)("counter - auto server", () => {
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
