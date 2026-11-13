import v8 from "node:v8";
import vm from "node:vm";
import { afterEach, describe, expect, it, vi } from "vitest";

// test the built package and not the source code
// makes it simpler to handle wasm build
import { type Simnet, initSimnet } from "..";

const manifestPath = "tests/fixtures/Clarinet.toml";

v8.setFlagsFromString("--expose-gc");
const gc: () => void = vm.runInNewContext("gc");

// FinalizationRegistry callbacks run in their own task, after the collection.
async function runFinalizers() {
  for (let i = 0; i < 3; i++) {
    gc();
    await new Promise((resolve) => setTimeout(resolve, 0));
  }
}

afterEach(() => {
  vi.restoreAllMocks();
});

describe("releasing a simnet", () => {
  it.each([
    ["free", (simnet: Simnet) => simnet.free()],
    ["Symbol.dispose", (simnet: Simnet) => simnet[Symbol.dispose]()],
  ])("unregisters the finalizer on %s", async (_name, release) => {
    const simnet = await initSimnet(manifestPath, true);
    const unregister = vi.spyOn(FinalizationRegistry.prototype, "unregister");

    release(simnet);

    expect(unregister.mock.results.map(({ value }) => value)).toContain(true);
  });

  it("keeps the newest simnet usable after freeing the replaced ones", async () => {
    let simnet = await initSimnet(manifestPath, true);

    for (let i = 0; i < 5; i++) {
      const next = await initSimnet(manifestPath, true);
      simnet.free();
      simnet = next;
      await runFinalizers();
    }

    expect(simnet.blockHeight).toBe(3);
    simnet.mineEmptyBlock();
    expect(simnet.blockHeight).toBe(4);
    expect(simnet.getAccounts().size).toBeGreaterThan(0);
  });
});
