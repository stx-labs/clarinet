import crypto from "node:crypto";
import { describe, expect, it, beforeEach } from "vitest";

import { Pox4SignatureTopic, StackingClient, poxAddressToTuple } from "@stacks/stacking";
import { STACKS_DEVNET } from "@stacks/network";
import { getPublicKeyFromPrivate, publicKeyToBtcAddress } from "@stacks/encryption";
import { Cl, ClarityType, getAddressFromPrivateKey } from "@stacks/transactions";

Error.stackTraceLimit = 200;

// test the built package and not the source code
// makes it simpler to handle wasm build
import { Simnet, initSimnet } from "..";

const MAX_U128 = 340282366920938463463374607431768211455n;
const maxAmount = MAX_U128;

const randInt = () => crypto.randomInt(0, 0xffffffffffff);

const address1 = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
const address2 = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";
const poxDeployer = "ST000000000000000000002AMW42H";

let simnet: Simnet;

const initialSTXBalance = 100_000_000 * 1e6;

describe("test pox-3", () => {
  const poxContract = `${poxDeployer}.pox-3`;
  beforeEach(async () => {
    simnet = await initSimnet("tests/fixtures/LightManifest.toml");
    simnet.setEpoch("2.4");
  });

  const ustxAmount = initialSTXBalance * 0.9; // lock 90% of the initial balance

  it("can transfer-stx", () => {
    // safe check that address1 can transfer 90% of its balance if not locked
    const transfer = simnet.transferSTX(ustxAmount, address2, address1);
    expect(transfer.result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });

  it("can call is-pox-active", () => {
    const isPoxActive = simnet.callReadOnlyFn(
      poxContract,
      "is-pox-active",
      [Cl.uint(100)],
      address1,
    );
    expect(isPoxActive.result).toStrictEqual(Cl.bool(true));
  });

  it("can stack-stx on pox-3", () => {
    const stackStxArgs = [
      Cl.uint(ustxAmount),
      Cl.tuple({
        version: Cl.bufferFromHex("00"),
        hashbytes: Cl.bufferFromHex("7321b74e2b6a7e949e6c4ad313035b1665095017"),
      }),
      Cl.uint(0),
      Cl.uint(1),
    ];
    const stackStx = simnet.callPublicFn(poxContract, "stack-stx", stackStxArgs, address1);
    expect(stackStx.events).toHaveLength(2);
    expect(stackStx.result).toStrictEqual(
      Cl.ok(
        Cl.tuple({
          "lock-amount": Cl.uint(ustxAmount),
          "unlock-burn-height": Cl.uint(2100),
          stacker: Cl.principal(address1),
        }),
      ),
    );

    const stxAccount = simnet.execute(`(stx-account '${address1})`);
    expect(stxAccount.result).toStrictEqual(
      Cl.tuple({
        locked: Cl.uint(ustxAmount),
        unlocked: Cl.uint(initialSTXBalance - ustxAmount),
        "unlock-height": Cl.uint(2100),
      }),
    );

    const transfer = simnet.transferSTX(ustxAmount, address2, address1);
    expect(transfer.result).toStrictEqual(Cl.error(Cl.uint(1)));
  });

  it("unlocks stx after a certain number of blocks", () => {
    const stackStxArgs = [
      Cl.uint(ustxAmount),
      Cl.tuple({
        version: Cl.bufferFromHex("00"),
        hashbytes: Cl.bufferFromHex("7321b74e2b6a7e949e6c4ad313035b1665095017"),
      }),
      Cl.uint(0),
      Cl.uint(1),
    ];
    simnet.callPublicFn(poxContract, "stack-stx", stackStxArgs, address1);

    simnet.mineEmptyBlocks(2098);
    const stxAccountBefore = simnet.execute(`(stx-account '${address1})`);
    expect(stxAccountBefore.result).toStrictEqual(
      Cl.tuple({
        locked: Cl.uint(ustxAmount),
        unlocked: Cl.uint(initialSTXBalance - ustxAmount),
        "unlock-height": Cl.uint(2100),
      }),
    );

    simnet.mineEmptyBlocks(1);
    const stxAccountAfter = simnet.execute(`(stx-account '${address1})`);
    expect(stxAccountAfter.result).toStrictEqual(
      Cl.tuple({
        locked: Cl.uint(0),
        unlocked: Cl.uint(initialSTXBalance),
        "unlock-height": Cl.uint(0),
      }),
    );
  });

  it("can get pox boot contract code coverage", async () => {
    const simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
      trackCoverage: true,
      trackCosts: false,
    });
    simnet.setEpoch("2.4");

    const stackStxArgs = [
      Cl.uint(ustxAmount),
      Cl.tuple({
        version: Cl.bufferFromHex("00"),
        hashbytes: Cl.bufferFromHex("7321b74e2b6a7e949e6c4ad313035b1665095017"),
      }),
      Cl.uint(0),
      Cl.uint(1),
    ];
    const stackStx = simnet.callPublicFn(poxContract, "stack-stx", stackStxArgs, address1);
    expect(stackStx.events).toHaveLength(2);

    const bootContractsPath = "./contracts/boot_contracts/";
    const { coverage } = simnet.collectReport(true, bootContractsPath);
    expect(coverage).toContain(`SF:${bootContractsPath}/pox-3`);
    expect(coverage).toContain("FNDA:1,stack-stx");
  });
});

describe("test pox-4", () => {
  const poxContract = `${poxDeployer}.pox-4`;

  // wallet_1 and wallet_2 private keys
  const stackingKeys = [
    "7287ba251d44a4d3fd9276c88ce34c5c52a038955511cccaf77e61068649c17801",
    "530d9f61984c888536871c6573073bdfc0058896dc1adfe9a6a10dfacadc209101",
  ];

  const accounts = stackingKeys.map((privKey) => {
    const network = STACKS_DEVNET;

    const pubKey = getPublicKeyFromPrivate(privKey);
    const stxAddress = getAddressFromPrivateKey(privKey, network);
    const signerPubKey = getPublicKeyFromPrivate(privKey);

    return {
      privKey,
      pubKey,
      stxAddress,
      btcAddr: publicKeyToBtcAddress(pubKey),
      signerPrivKey: privKey,
      signerPubKey: signerPubKey,
      client: new StackingClient({ address: stxAddress, network }),
    };
  });

  const stackingThreshold = 50000000000;

  beforeEach(async () => {
    simnet = await initSimnet("tests/fixtures/Clarinet.toml");
    simnet.setEpoch("3.0");
  });

  it("can call get-pox-info", () => {
    const poxInfo = simnet.callReadOnlyFn(poxContract, "get-pox-info", [], address1);
    expect(poxInfo.result.type).toBe(ClarityType.ResponseOk);
  });

  it("can call get-pox-info", () => {
    const account = accounts[0];
    const rewardCycle = 0;
    const burnBlockHeight = 0;
    const period = 1;
    const authId = randInt();
    const poxInfo = simnet.callReadOnlyFn(poxContract, "get-pox-info", [], address1);

    expect(poxInfo.result.type).toBe(ClarityType.ResponseOk);

    expect(poxInfo.result).toHaveProperty(
      "value.value.min-amount-ustx",
      Cl.uint(stackingThreshold),
    );
    expect(poxInfo.result).toHaveProperty("value.value.reward-cycle-id", Cl.uint(rewardCycle));

    const sigArgs = {
      authId,
      maxAmount,
      rewardCycle,
      period,
      topic: Pox4SignatureTopic.StackStx,
      poxAddress: account.btcAddr,
      signerPrivateKey: account.signerPrivKey,
    };
    const signerSignature = account.client.signPoxSignature(sigArgs);
    const ustxAmount = Math.floor(stackingThreshold * 1.5);

    /*
      (stack-stx (amount-ustx uint)
        (pox-addr (tuple (version (buff 1)) (hashbytes (buff 32))))
        (start-burn-ht uint)
        (lock-period uint)
        (signer-sig (optional (buff 65)))
        (signer-key (buff 33))
        (max-amount uint)
        (auth-id uint))
    */

    const stackStxArgs = [
      Cl.uint(ustxAmount),
      poxAddressToTuple(account.btcAddr),
      Cl.uint(burnBlockHeight),
      Cl.uint(period),
      Cl.some(Cl.bufferFromHex(signerSignature)),
      Cl.bufferFromHex(account.signerPubKey),
      Cl.uint(maxAmount),
      Cl.uint(authId),
    ];

    const stackStx = simnet.callPublicFn(poxContract, "stack-stx", stackStxArgs, address1);

    expect(stackStx.result).toStrictEqual(
      Cl.ok(
        Cl.tuple({
          "lock-amount": Cl.uint(75000000000),
          "signer-key": Cl.bufferFromHex(account.signerPubKey),
          stacker: Cl.principal(address1),
          "unlock-burn-height": Cl.uint(2100),
        }),
      ),
    );

    const stxAccount = simnet.execute(`(stx-account '${address1})`);
    expect(stxAccount.result).toStrictEqual(
      Cl.tuple({
        locked: Cl.uint(ustxAmount),
        unlocked: Cl.uint(initialSTXBalance - ustxAmount),
        "unlock-height": Cl.uint(2100),
      }),
    );
  });
});
