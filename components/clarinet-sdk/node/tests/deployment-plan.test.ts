import fs from "node:fs";
import path from "node:path";
import { describe, expect, it, beforeEach, afterEach } from "vitest";

// test the built package and not the source code
// makes it simpler to handle wasm build
import { initSimnet } from "..";
import { Cl } from "@stacks/transactions";

const nbOfBootContracts = 26;

const deploymentPlanPath = path.join(
  process.cwd(),
  "tests/fixtures/deployments/default.simnet-plan.yaml",
);

const customDeploymentPlanPath = path.join(
  process.cwd(),
  "tests/fixtures/deployments/custom.simnet-plan.yaml",
);

function deleteExistingDeploymentPlan() {
  if (fs.existsSync(deploymentPlanPath)) {
    fs.unlinkSync(deploymentPlanPath);
  }
}

beforeEach(() => {
  deleteExistingDeploymentPlan();
});

afterEach(() => {
  deleteExistingDeploymentPlan();
});

describe("deployment plans test", async () => {
  it("simnet deployment plan is created if it does not exist", async () => {
    expect(fs.existsSync(deploymentPlanPath)).toBe(false);

    // load a new simnet with no cache
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true);

    expect(fs.existsSync(deploymentPlanPath)).toBe(true);

    // make sure the simnet is running
    expect(simnet.blockHeight).toBe(3);
  });

  it("can use custom deployment plan", async () => {
    fs.copyFileSync(customDeploymentPlanPath, deploymentPlanPath);
    expect(fs.existsSync(deploymentPlanPath)).toBe(true);
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true);

    // test that all 3 contracts are deployed
    const contracts = simnet.getContractsInterfaces();
    expect(contracts.size).toBe(nbOfBootContracts + 5);

    // the additional custom tx should have been applied
    const count = simnet.getDataVar("counter", "count");
    expect(count).toStrictEqual(Cl.uint(2));
  });

  it("re-applies contract call tx when the deployment plan is updated", async () => {
    fs.copyFileSync(customDeploymentPlanPath, deploymentPlanPath);
    expect(fs.existsSync(deploymentPlanPath)).toBe(true);
    const simnet = await initSimnet("tests/fixtures/LightManifest.toml", true);

    // only two contract should be deployed with the light manifest
    const contracts = simnet.getContractsInterfaces();
    expect(contracts.size).toBe(nbOfBootContracts + 2);

    // the additional custom tx should have been applied
    const count = simnet.getDataVar("counter", "count");
    expect(count).toStrictEqual(Cl.uint(2));
  });

  it("can handle contract removal", async () => {
    // generate the simnet deployment plan
    await initSimnet("tests/fixtures/Clarinet.toml", true);

    // rename the counter.clar to _counter.clar
    fs.renameSync(
      "tests/fixtures/contracts/counter.clar",
      "tests/fixtures/contracts/_counter.clar",
    );
    const manifestContent = fs.readFileSync("tests/fixtures/Clarinet.toml", "utf-8");
    const newContent = manifestContent.replace("counter.clar", "_counter.clar");
    fs.writeFileSync("tests/fixtures/Clarinet.toml", newContent);

    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true);
    const contractInterfaces = simnet.getContractsInterfaces();
    expect(contractInterfaces.get(`${simnet.deployer}.counter`)).toBeDefined();

    // revert the changes
    fs.renameSync(
      "tests/fixtures/contracts/_counter.clar",
      "tests/fixtures/contracts/counter.clar",
    );
    fs.writeFileSync("tests/fixtures/Clarinet.toml", manifestContent);
  });
});
