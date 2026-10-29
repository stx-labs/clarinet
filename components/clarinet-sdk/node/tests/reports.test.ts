import fs from "node:fs";
import path from "node:path";
import { Cl } from "@stacks/transactions";
import { describe, expect, it, beforeEach, afterEach } from "vitest";

// test the built package and not the source code
// makes it simpler to handle wasm build
import { initSimnet } from "..";

const address1 = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";

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
});

afterEach(() => {
  deleteExistingDeploymentPlan();
});

describe("simnet can get code coverage", () => {
  it("does not report coverage by default", async () => {
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true);

    simnet.callPublicFn("counter", "increment", [], address1);

    const reports = simnet.collectReport(false, "");
    expect(reports.coverage.length).toBe(0);
  });

  it("reports coverage if enabled", async () => {
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
      trackCoverage: true,
      trackCosts: false,
    });

    simnet.setCurrentTestName("test1");
    simnet.callPublicFn("counter", "increment", [], address1);
    simnet.callPublicFn("counter", "increment", [], address1);
    simnet.callPrivateFn("counter", "inner-increment", [], address1);

    const reports = simnet.collectReport(false, "");

    // increment is called twice
    expect(reports.coverage.includes("FNDA:2,increment")).toBe(true);
    // inner-increment is called one time directly and twice by `increment`
    expect(reports.coverage.includes("FNDA:3,inner-increment")).toBe(true);
    expect(reports.coverage.startsWith("TN:")).toBe(true);
    expect(reports.coverage.endsWith("end_of_record\n")).toBe(true);
  });
});

describe("simnet can get costs reports", () => {
  it("does not report costs by default", async () => {
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true);
    simnet.callPublicFn("counter", "increment", [], address1);

    const reports = simnet.collectReport(false, "");
    expect(() => JSON.parse(reports.costs)).not.toThrow();

    const parsedReports = JSON.parse(reports.costs);
    expect(parsedReports).toHaveLength(0);
  });

  it("report costs if enabled", async () => {
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
      trackCoverage: false,
      trackCosts: true,
    });
    simnet.callPublicFn("counter", "increment", [], address1);

    const reports = simnet.collectReport(false, "");
    expect(() => JSON.parse(reports.costs)).not.toThrow();

    const parsedReports = JSON.parse(reports.costs);
    expect(parsedReports).toHaveLength(1);

    const report = parsedReports[0];
    expect(report.contract_id).toBe(`${simnet.deployer}.counter`);
    expect(report.method).toBe("increment");
    expect(report.cost_result.total.write_count).toBe(3);
  });

  it("reports the call arguments", async () => {
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
      trackCoverage: false,
      trackCosts: true,
    });
    const trait = Cl.contractPrincipal(simnet.deployer, "multiplier-contract");
    simnet.callPublicFn("counter", "add", [Cl.uint(2)], address1);
    simnet.callPublicFn("counter", "call-multiply", [trait], address1);

    const reports = JSON.parse(simnet.collectReport(false, "").costs);
    expect(reports.map((report: { args: string[] }) => report.args)).toStrictEqual([
      ["u2"],
      [`${simnet.deployer}.multiplier-contract`],
    ]);
  });
});

describe("simnet can report both costs and coverage", () => {
  it("can report both costs and coverage", async () => {
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
      trackCoverage: true,
      trackCosts: true,
    });
    simnet.callPublicFn("counter", "increment", [], address1);

    const reports = simnet.collectReport(false, "");

    const parsedReports = JSON.parse(reports.costs);
    expect(parsedReports).toHaveLength(1);

    expect(reports.coverage.length).greaterThan(0);
  });
});

describe("simnet.run-snippet and .execute also report coverage", () => {
  it("simnet.execute reports coverage", async () => {
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
      trackCoverage: true,
      trackCosts: false,
    });
    simnet.execute("(contract-call? .counter increment)");
    simnet.execute("(contract-call? .counter increment)");

    const reports = simnet.collectReport(false, "");
    // line 33, within the increment function, is executed twice
    expect(reports.coverage).toContain("DA:33,2");
  });

  it("simnet.runSnippet reports coverage", async () => {
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
      trackCoverage: true,
      trackCosts: false,
    });
    simnet.runSnippet("(contract-call? .counter increment)");
    simnet.runSnippet("(contract-call? .counter increment)");

    const reports = simnet.collectReport(false, "");
    // line 33, within the increment function, is executed twice
    expect(reports.coverage).toContain("DA:33,2");
  });
});
