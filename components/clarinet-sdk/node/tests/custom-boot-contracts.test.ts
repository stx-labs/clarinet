import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

import { Simnet, initSimnet } from "..";

const poxDeployer = "ST000000000000000000002AMW42H";

let simnet: Simnet;

describe("override_boot_contracts_source under the wasm SDK", () => {
  beforeEach(async () => {
    simnet = await initSimnet("tests/fixtures/ManifestCustomBootContracts.toml");
    // pox-4 belongs to Epoch 2.5; advance the epoch so it gets deployed.
    simnet.setEpoch("2.5");
  });

  it("replaces pox-4 with the source pointed to by the manifest", () => {
    const result = simnet.callReadOnlyFn(`${poxDeployer}.pox-4`, "sentinel", [], poxDeployer);
    expect(result.result).toStrictEqual(Cl.ok(Cl.stringAscii("override-active")));
  });
});
