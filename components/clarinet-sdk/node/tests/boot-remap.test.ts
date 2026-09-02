import fs from "node:fs";
import path from "node:path";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { Cl } from "@stacks/transactions";

// test the built package and not the source code
// makes it simpler to handle wasm build
import { Simnet, initSimnet } from "..";

// Simnet deploys the boot contracts under both addresses, but its chain state
// is testnet-flavored: only the ST000... contracts move consensus state. A
// contract that names the mainnet PoX address has its principal rewritten at
// deployment time so the lock actually happens.
// See https://github.com/stx-labs/clarinet/issues/2491
const MAINNET_BOOT = "SP000000000000000000002Q6VF78";
const TESTNET_BOOT = "ST000000000000000000002AMW42H";

const deployerAddr = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
const address1 = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";

const initialSTXBalance = 100_000_000 * 1e6;
const stacked = initialSTXBalance * 0.9;

// A contract stacks its own balance, so it has to be funded first. Half the
// wallet balance leaves room for the transfer itself.
const funded = initialSTXBalance / 2;
const toStack = stacked / 2;

const deploymentPlanPath = path.join(
  process.cwd(),
  "tests/fixtures/deployments/default.simnet-plan.yaml",
);

function deleteExistingDeploymentPlan() {
  if (fs.existsSync(deploymentPlanPath)) {
    fs.unlinkSync(deploymentPlanPath);
  }
}

/** `stack-stx` on pox-3, locking 90% of the initial balance for one cycle. */
const stackStxArgs = [
  Cl.uint(stacked),
  Cl.tuple({
    version: Cl.bufferFromHex("00"),
    hashbytes: Cl.bufferFromHex("7321b74e2b6a7e949e6c4ad313035b1665095017"),
  }),
  Cl.uint(0),
  Cl.uint(1),
];

/** Source that stacks the deploying contract's own balance via `pox-3`. */
function stackerSource(bootAddress: string) {
  return `
    (define-public (stack (amount uint))
        (as-contract
            (contract-call? '${bootAddress}.pox-3 stack-stx
                amount
                { version: 0x00, hashbytes: 0x7321b74e2b6a7e949e6c4ad313035b1665095017 }
                burn-block-height
                u1)))
  `;
}

function lockedAmount(simnet: Simnet, principal: string) {
  return simnet.execute(`(get locked (stx-account '${principal}))`).result;
}

/** Fund `contractName`, have it stack, and assert the STX actually locked. */
function expectStackerLocks(simnet: Simnet, contractName: string) {
  const stacker = `${deployerAddr}.${contractName}`;
  simnet.transferSTX(funded, stacker, deployerAddr);

  const result = simnet.callPublicFn(contractName, "stack", [Cl.uint(toStack)], deployerAddr);
  expect(result.result.type).toBe("ok");
  expect(lockedAmount(simnet, stacker)).toStrictEqual(Cl.uint(toStack));
}

describe("a manifest contract calling the mainnet pox address", () => {
  let simnet: Simnet;

  beforeEach(async () => {
    deleteExistingDeploymentPlan();
    simnet = await initSimnet("tests/fixtures/ManifestWithBootRemap.toml", true);
    simnet.setEpoch("2.4");
  });

  afterEach(() => {
    deleteExistingDeploymentPlan();
  });

  it("locks stx", () => {
    expectStackerLocks(simnet, "mainnet-pox-stacker");
  });

  it("records the remap in the generated deployment plan", () => {
    const plan = fs.readFileSync(deploymentPlanPath, "utf-8");
    expect(plan).toContain("remap-principals:");
    expect(plan).toContain(`${MAINNET_BOOT}: ${TESTNET_BOOT}`);
  });
});

// `deployContract` bypasses the deployment plan entirely, so it has to apply
// the remap on its own.
describe("deployContract", () => {
  let simnet: Simnet;

  beforeEach(async () => {
    simnet = await initSimnet("tests/fixtures/LightManifest.toml");
    simnet.setEpoch("2.4");
  });

  it.each([
    ["mainnet", MAINNET_BOOT],
    ["testnet", TESTNET_BOOT],
  ])("locks stx for a contract spelling the %s pox address", (label, bootAddress) => {
    const contractName = `stacker-${label}`;
    simnet.deployContract(contractName, stackerSource(bootAddress), null, deployerAddr);

    expectStackerLocks(simnet, contractName);
  });

  it("leaves sbtc, requirement and bare burn-address principals alone", () => {
    const source = `
      (define-constant SBTC 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token)
      (define-constant NFT 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait)
      (define-constant BURN '${MAINNET_BOOT})
      (define-read-only (get-burn) BURN)
      (define-read-only (get-sbtc) SBTC)
      (define-read-only (get-nft) NFT)
    `;
    simnet.deployContract("untouched-refs", source, null, deployerAddr);

    expect(simnet.callReadOnlyFn("untouched-refs", "get-burn", [], deployerAddr).result).toStrictEqual(
      Cl.principal(MAINNET_BOOT),
    );
    expect(simnet.callReadOnlyFn("untouched-refs", "get-sbtc", [], deployerAddr).result).toStrictEqual(
      Cl.contractPrincipal("SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4", "sbtc-token"),
    );
    expect(simnet.callReadOnlyFn("untouched-refs", "get-nft", [], deployerAddr).result).toStrictEqual(
      Cl.contractPrincipal("SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9", "nft-trait"),
    );
  });
});

// The direct-call surface: a test calling a boot contract by its mainnet
// address. There is no deployment to rewrite here, so the call target itself
// is redirected.
describe("calling pox-3 directly", () => {
  let simnet: Simnet;

  beforeEach(async () => {
    simnet = await initSimnet("tests/fixtures/LightManifest.toml");
    simnet.setEpoch("2.4");
  });

  it.each([
    ["mainnet", MAINNET_BOOT],
    ["testnet", TESTNET_BOOT],
  ])("locks stx through the %s address", (_label, bootAddress) => {
    const stackStx = simnet.callPublicFn(
      `${bootAddress}.pox-3`,
      "stack-stx",
      stackStxArgs,
      address1,
    );

    // Both spellings must be indistinguishable — same result, same events,
    // same resulting state. That is what makes the redirect safe to read.
    expect(stackStx.result).toStrictEqual(
      Cl.ok(
        Cl.tuple({
          "lock-amount": Cl.uint(stacked),
          "unlock-burn-height": Cl.uint(2100),
          stacker: Cl.principal(address1),
        }),
      ),
    );
    expect(stackStx.events).toHaveLength(2);

    expect(simnet.execute(`(stx-account '${address1})`).result).toStrictEqual(
      Cl.tuple({
        locked: Cl.uint(stacked),
        unlocked: Cl.uint(initialSTXBalance - stacked),
        "unlock-height": Cl.uint(2100),
      }),
    );
  });

  // A call redirected without redirecting the reads would report the mainnet
  // pox-3 (REWARD_CYCLE_LENGTH u2100) for a call that behaved per the testnet
  // one (u1050) — reported state disagreeing with executed state.
  it("resolves reads to the contract the calls execute against", () => {
    const mainnetSource = simnet.getContractSource(`${MAINNET_BOOT}.pox-3`);
    const testnetSource = simnet.getContractSource(`${TESTNET_BOOT}.pox-3`);
    expect(mainnetSource).toStrictEqual(testnetSource);

    // Sanity: the two on-disk boot sources really do differ, so the assertion
    // above is meaningful rather than vacuous.
    expect(testnetSource).toContain("(define-constant REWARD_CYCLE_LENGTH u1050)");
    expect(mainnetSource).not.toContain("(define-constant REWARD_CYCLE_LENGTH u2100)");
  });

  it("shares one state between the two addresses", () => {
    simnet.callPublicFn(`${MAINNET_BOOT}.pox-3`, "stack-stx", stackStxArgs, address1);

    // The second attempt, spelled the other way, must see the first lock.
    const again = simnet.callPublicFn(`${TESTNET_BOOT}.pox-3`, "stack-stx", stackStxArgs, address1);
    expect(again.result.type).toBe("err");
  });
});
