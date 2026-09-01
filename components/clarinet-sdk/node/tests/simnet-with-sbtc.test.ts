import fs from "node:fs";
import path from "node:path";
import { Cl } from "@stacks/transactions";
import { describe, expect, it, beforeEach, afterEach } from "vitest";

// test the built package and not the source code
// makes it simpler to handle wasm build
import { Simnet, initSimnet } from "..";

const address1 = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
const address2 = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";

const sbtcDeployer = "SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4";
const sbtcToken = `${sbtcDeployer}.sbtc-token`;

// (define-constant ERR_NOT_OWNER (err u4)) in sbtc-token.clar
const ERR_NOT_OWNER = 4n;

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

// ManifestWithSBTC.toml declares no contract and no requirement: the sBTC
// contracts are boot contracts, so `sbtc_balance` must be minted without the
// project having to opt in to anything.
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
  it("boots in the latest epoch, since no contract pins an earlier one", () => {
    expect(simnet.currentEpoch).toBe("4.0");
  });

  it("automatically deployed the sbtc contracts", () => {
    const contracts = simnet.getContractsInterfaces();
    expect(contracts.has(`${sbtcDeployer}.sbtc-registry`)).toBe(true);
    expect(contracts.has(`${sbtcDeployer}.sbtc-token`)).toBe(true);
    expect(contracts.has(`${sbtcDeployer}.sbtc-deposit`)).toBe(true);
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

  it("reports the total supply the genesis funding minted", () => {
    const total = simnet.callReadOnlyFn(sbtcToken, "get-total-supply", [], address1);
    // deployer: 100_000_000_000_000, the 3 other wallets: 1_000_000_000 each
    expect(total.result).toStrictEqual(Cl.ok(Cl.uint(100003000000000n)));
  });
});

describe("sbtc-token basic functionality", () => {
  it("exposes its SIP-010 metadata", () => {
    expect(simnet.callReadOnlyFn(sbtcToken, "get-name", [], address1).result).toStrictEqual(
      Cl.ok(Cl.stringAscii("sBTC")),
    );
    expect(simnet.callReadOnlyFn(sbtcToken, "get-symbol", [], address1).result).toStrictEqual(
      Cl.ok(Cl.stringAscii("sBTC")),
    );
    expect(simnet.callReadOnlyFn(sbtcToken, "get-decimals", [], address1).result).toStrictEqual(
      Cl.ok(Cl.uint(8)),
    );
  });

  it("reports the balance the genesis funding assigned", () => {
    const res = simnet.callReadOnlyFn(sbtcToken, "get-balance", [Cl.principal(address1)], address1);
    expect(res.result).toStrictEqual(Cl.ok(Cl.uint(1000000000n)));
  });

  it("transfers sBTC between wallets", () => {
    const amount = 42n;
    const res = simnet.callPublicFn(
      sbtcToken,
      "transfer",
      [Cl.uint(amount), Cl.principal(address1), Cl.principal(address2), Cl.none()],
      address1,
    );

    expect(res.result).toStrictEqual(Cl.ok(Cl.bool(true)));

    const transferEvent = res.events.find((e) => e.event === "ft_transfer_event");
    expect(transferEvent).toBeDefined();
    expect(transferEvent!.data.amount).toBe(amount.toString());

    expect(
      simnet.callReadOnlyFn(sbtcToken, "get-balance", [Cl.principal(address1)], address1).result,
    ).toStrictEqual(Cl.ok(Cl.uint(1000000000n - amount)));
    expect(
      simnet.callReadOnlyFn(sbtcToken, "get-balance", [Cl.principal(address2)], address1).result,
    ).toStrictEqual(Cl.ok(Cl.uint(1000000000n + amount)));
  });

  it("refuses to move sBTC the caller does not own", () => {
    const res = simnet.callPublicFn(
      sbtcToken,
      "transfer",
      // address1 signs, but tries to move address2's balance
      [Cl.uint(1n), Cl.principal(address2), Cl.principal(address1), Cl.none()],
      address1,
    );

    expect(res.result).toStrictEqual(Cl.error(Cl.uint(ERR_NOT_OWNER)));
  });
});
