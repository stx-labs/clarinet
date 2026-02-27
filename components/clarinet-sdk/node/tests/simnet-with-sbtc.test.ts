import fs from "node:fs";
import path from "node:path";
import { describe, expect, it, beforeEach, afterEach } from "vitest";

// test the built package and not the source code
// makes it simpler to handle wasm build
import { Simnet, initSimnet } from "..";

const address1 = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
const address2 = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";

let simnet: Simnet;

const deploymentPlanPath = path.join(
  process.cwd(),
  "tests/fixtures/deployments/default.simnet-plan.yaml",
);

function deleteExistingDeploymentPlan() {
  if (fs.existsSync(deploymentPlanPath)) {
    fs.unlinkSync(deploymentPlanPath);
  }
}

beforeEach(async () => {
  deleteExistingDeploymentPlan();
  simnet = await initSimnet("tests/fixtures/ManifestWithSBTC.toml", false, {
    trackCosts: true,
    trackCoverage: false,
  });
});

afterEach(() => {
  deleteExistingDeploymentPlan();
});

describe("sbtc funding", () => {
  it("boots in epoch 3.0", () => {
    expect(simnet.currentEpoch).toBe("3.0");
  });

  it("automatically deployed the sbtc-token contract", () => {
    const contracts = simnet.getContractsInterfaces();
    expect(contracts.has("SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-registry")).toBe(true);
    expect(contracts.has("SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token")).toBe(true);
    expect(contracts.has("SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-deposit")).toBe(true);
  });

  it("automatically funded the test wallets", () => {
    const balances = simnet.getAssetsMap();
    expect(balances.size).toBe(2);
    const stxBalance = balances.get("STX")!;
    expect(stxBalance.size).toBe(4);
    expect(stxBalance.get(address1)).toBe(100000000000000n);
    expect(stxBalance.get(address2)).toBe(100000000000000n);

    const sbtcBalance = balances.get(".sbtc-token.sbtc-token")!;
    expect(sbtcBalance.size).toBe(4);
    expect(sbtcBalance.get(address1)).toBe(1000000000n);
    expect(sbtcBalance.get(address2)).toBe(1000000000n);
  });
});
