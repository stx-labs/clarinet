import fs from "node:fs";
import path from "node:path";
import { describe, expect, it, beforeEach, afterEach } from "vitest";

// test the built package and not the source code
// makes it simpler to handle wasm build
import { Simnet, initSimnet } from "..";

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
  simnet = await initSimnet("tests/fixtures/ManifestLatest.toml");
});

afterEach(() => {
  deleteExistingDeploymentPlan();
});

describe("basic simnet interactions", () => {
  it("can use contract with latest epoch", () => {
    const contractInterfaces = simnet.getContractsInterfaces();
    const counterContract = contractInterfaces.get(`${simnet.deployer}.counter`);

    expect(counterContract?.epoch).toBe("Epoch33");
  });
});
