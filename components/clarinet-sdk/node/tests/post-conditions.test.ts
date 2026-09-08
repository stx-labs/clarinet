import {
  Cl,
  Pc,
  PostConditionMode,
  postConditionToHex,
  postConditionToWire,
} from "@stacks/transactions";
import { beforeEach, describe, expect, it } from "vitest";

// test the built package and not the source code
// makes it simpler to handle wasm build
import { Simnet, initSimnet, tx } from "..";

const deployer = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
const address1 = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
const address2 = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";

// `counter.increment` moves exactly this much STX from the sender to the contract.
const INCREMENT_COST = 1_000_000;

let simnet: Simnet;

beforeEach(async () => {
  simnet = await initSimnet("tests/fixtures/Clarinet.toml");
});

function getCount() {
  const { result } = simnet.callReadOnlyFn("counter", "get-count", [], address1);
  return result;
}

describe("post-conditions on contract calls", () => {
  it("accepts a call whose asset movement satisfies the conditions", () => {
    const { result } = simnet.callPublicFn("counter", "increment", [], address1, {
      postConditions: [Pc.principal(address1).willSendEq(INCREMENT_COST).ustx()],
    });

    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
    expect(getCount()).toStrictEqual(Cl.ok(Cl.tuple({ count: Cl.uint(1) })));
  });

  it("aborts a call whose asset movement violates the conditions", () => {
    expect(() =>
      simnet.callPublicFn("counter", "increment", [], address1, {
        postConditions: [
          Pc.principal(address1)
            .willSendEq(INCREMENT_COST + 1)
            .ustx(),
        ],
      }),
    ).toThrow(/Post-condition check failure/);
  });

  it("rolls back state when a post-condition aborts the call", () => {
    expect(() =>
      simnet.callPublicFn("counter", "increment", [], address1, {
        postConditions: [Pc.principal(address1).willSendEq(1).ustx()],
      }),
    ).toThrow(/Post-condition check failure/);

    // The counter never incremented and the sender kept its STX.
    expect(getCount()).toStrictEqual(Cl.ok(Cl.tuple({ count: Cl.uint(0) })));
    expect(simnet.getAssetsMap().get("STX")?.get(address1)).toBe(100000000000000n);
  });

  it("still consumes a nonce when a post-condition aborts the call", () => {
    const before = simnet.getAccountNonce(address1);

    expect(() =>
      simnet.callPublicFn("counter", "increment", [], address1, {
        postConditions: [Pc.principal(address1).willSendEq(1).ustx()],
      }),
    ).toThrow(/Post-condition check failure/);

    // Mainnet includes a post-condition abort, so the nonce moves.
    expect(simnet.getAccountNonce(address1)).toBe(before + 1n);
  });

  it("defaults to deny, so unlisted asset movement fails", () => {
    expect(() =>
      simnet.callPublicFn("counter", "increment", [], address1, { postConditions: [] }),
    ).toThrow(/Post-condition check failure/);
  });

  it("accepts the stacks.js mode enum", () => {
    const { result } = simnet.callPublicFn("counter", "increment", [], address1, {
      postConditions: [],
      postConditionMode: PostConditionMode.Allow,
    });

    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });

  it("turns enforcement on when only the mode is given", () => {
    expect(() =>
      simnet.callPublicFn("counter", "increment", [], address1, { postConditionMode: "deny" }),
    ).toThrow(/Post-condition check failure/);
  });

  it("leaves asset movement unconstrained when no options are passed", () => {
    const { result } = simnet.callPublicFn("counter", "increment", [], address1);
    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });

  it("rejects originator mode before it is supported, without running the call", () => {
    expect(() =>
      simnet.callPublicFn("counter", "transfer-100", [Cl.principal(address2)], address1, {
        postConditionMode: PostConditionMode.Originator,
      }),
    ).toThrow(/Originator post-condition mode is not supported before Stacks 3.4/);

    // Rejected rather than aborted, so nothing moved and no nonce was consumed.
    expect(simnet.getAccountNonce(address1)).toBe(0n);
  });

  it("constrains the origin in originator mode once the epoch supports it", () => {
    simnet.setEpoch("4.0");

    expect(() =>
      simnet.callPublicFn("counter", "transfer-100", [Cl.principal(address2)], address1, {
        postConditionMode: "originator",
      }),
    ).toThrow(/Post-condition check failure/);

    const { result } = simnet.callPublicFn(
      "counter",
      "transfer-100",
      [Cl.principal(address2)],
      address1,
      {
        postConditions: [Pc.principal(address1).willSendEq(100).ustx()],
        postConditionMode: "originator",
      },
    );
    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });

  it("accepts a condition given as consensus hex", () => {
    const hex = postConditionToHex(Pc.principal(address1).willSendEq(INCREMENT_COST).ustx());
    expect(typeof hex).toBe("string");

    const { result } = simnet.callPublicFn("counter", "increment", [], address1, {
      postConditions: [hex],
    });

    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });

  it("accepts a stacks.js wire post-condition", () => {
    const condition = postConditionToWire(Pc.principal(address1).willSendEq(INCREMENT_COST).ustx());

    const { result } = simnet.callPublicFn("counter", "increment", [], address1, {
      postConditions: [condition],
    });

    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });

  it("rejects a malformed condition without running the call", () => {
    expect(() =>
      simnet.callPublicFn("counter", "increment", [], address1, {
        postConditions: ["not-hex"],
      }),
    ).toThrow(/invalid post-condition hex/);

    expect(getCount()).toStrictEqual(Cl.ok(Cl.tuple({ count: Cl.uint(0) })));
  });

  it("rejects an unknown mode", () => {
    expect(() =>
      // @ts-expect-error deliberately outside the union
      simnet.callPublicFn("counter", "increment", [], address1, { postConditionMode: "maybe" }),
    ).toThrow(/invalid post-condition mode 'maybe'/);
  });
});

describe("post-conditions on STX transfers", () => {
  it("ignores deny mode when the post-condition list is empty", () => {
    const senderBalance = simnet.getAssetsMap().get("STX")!.get(address1)!;
    const recipientBalance = simnet.getAssetsMap().get("STX")!.get(address2)!;
    const senderNonce = simnet.getAccountNonce(address1);

    const { result } = simnet.transferSTX(1000, address2, address1, {
      postConditions: [],
      postConditionMode: "deny",
    });

    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
    expect(simnet.getAssetsMap().get("STX")!.get(address1)).toBe(senderBalance - 1000n);
    expect(simnet.getAssetsMap().get("STX")!.get(address2)).toBe(recipientBalance + 1000n);
    expect(simnet.getAccountNonce(address1)).toBe(senderNonce + 1n);
  });

  it("rejects the transaction before execution", () => {
    const senderBalance = simnet.getAssetsMap().get("STX")?.get(address1);
    const senderNonce = simnet.getAccountNonce(address1);

    expect(() =>
      simnet.transferSTX(1000, address2, address1, {
        postConditions: [Pc.principal(address1).willSendEq(1000).ustx()],
      }),
    ).toThrow(
      /Invalid Stacks transaction: TokenTransfer transactions do not support post-conditions/,
    );

    expect(simnet.getAssetsMap().get("STX")?.get(address1)).toBe(senderBalance);
    expect(simnet.getAccountNonce(address1)).toBe(senderNonce);
  });

  it("restores the session sender after a rejected transfer", () => {
    const initialSender = simnet.execute("tx-sender").result;

    expect(() =>
      simnet.transferSTX(1000, address2, address1, {
        postConditions: [Pc.principal(address1).willSendEq(1000).ustx()],
      }),
    ).toThrow(/TokenTransfer transactions do not support post-conditions/);

    expect(simnet.execute("tx-sender").result).toStrictEqual(initialSender);
  });
});

describe("post-conditions on contract deployments", () => {
  const source = `(begin (unwrap-panic (stx-transfer? u500 tx-sender '${address2})))`;

  it("accepts a deployment that satisfies the conditions", () => {
    const { result } = simnet.deployContract("pays-on-deploy", source, null, address1, {
      postConditions: [Pc.principal(address1).willSendEq(500).ustx()],
    });

    expect(result).toStrictEqual(Cl.bool(true));
  });

  it("aborts a deployment that violates the conditions", () => {
    const senderBalance = simnet.getAssetsMap().get("STX")!.get(address1)!;
    const recipientBalance = simnet.getAssetsMap().get("STX")!.get(address2)!;
    const nonce = simnet.getAccountNonce(address1);

    expect(() =>
      simnet.deployContract("pays-on-deploy", source, null, address1, {
        postConditions: [Pc.principal(address1).willSendEq(1).ustx()],
      }),
    ).toThrow(/Post-condition check failure/);

    // The aborted deployment left no contract behind.
    expect(simnet.getContractSource(`${address1}.pays-on-deploy`)).toBeUndefined();
    expect(simnet.getAssetsMap().get("STX")!.get(address1)).toBe(senderBalance);
    expect(simnet.getAssetsMap().get("STX")!.get(address2)).toBe(recipientBalance);
    expect(simnet.getAccountNonce(address1)).toBe(nonce + 1n);
  });
});

describe("post-conditions with multiple principals", () => {
  it("checks STX sent by a contract principal", () => {
    const contract = `${deployer}.counter`;

    simnet.callPublicFn("counter", "increment", [], deployer);
    const contractBalance = simnet.getAssetsMap().get("STX")!.get(contract)!;

    const { result } = simnet.callPublicFn("counter", "withdraw", [Cl.uint(100)], deployer, {
      postConditions: [Pc.principal(contract).willSendEq(100).ustx()],
    });

    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
    expect(simnet.getAssetsMap().get("STX")!.get(contract)).toBe(contractBalance - 100n);

    const nonce = simnet.getAccountNonce(deployer);
    expect(() =>
      simnet.callPublicFn("counter", "withdraw", [Cl.uint(100)], deployer, {
        postConditions: [Pc.principal(contract).willSendEq(99).ustx()],
      }),
    ).toThrow(/Post-condition check failure/);
    expect(simnet.getAssetsMap().get("STX")!.get(contract)).toBe(contractBalance - 100n);
    expect(simnet.getAccountNonce(deployer)).toBe(nonce + 1n);
  });

  it("enforces every condition independently", () => {
    const conditions = [
      Pc.principal(address1).willSendEq(100).ustx(),
      Pc.principal(address2).willSendEq(0).ustx(),
    ];

    expect(
      simnet.callPublicFn("counter", "transfer-100", [Cl.principal(address2)], address1, {
        postConditions: conditions,
      }).result,
    ).toStrictEqual(Cl.ok(Cl.bool(true)));

    expect(() =>
      simnet.callPublicFn("counter", "transfer-100", [Cl.principal(address2)], address1, {
        postConditions: [conditions[0], Pc.principal(address2).willSendEq(1).ustx()],
      }),
    ).toThrow(/Post-condition check failure/);
  });
});

describe("post-conditions on fungible tokens", () => {
  const source = `
(define-fungible-token widget)
(define-public (mint (amount uint) (to principal)) (ft-mint? widget amount to))
(define-public (send (amount uint) (to principal)) (ft-transfer? widget amount tx-sender to))
(define-read-only (balance-of (who principal)) (ft-get-balance widget who))
`;

  let token: string;

  beforeEach(() => {
    simnet.deployContract("widget", source, null, deployer);
    token = `${deployer}.widget`;
    simnet.callPublicFn("widget", "mint", [Cl.uint(1000), Cl.principal(address1)], deployer);
  });

  it("accepts a transfer that satisfies the conditions", () => {
    const { result } = simnet.callPublicFn(
      "widget",
      "send",
      [Cl.uint(50), Cl.principal(address2)],
      address1,
      { postConditions: [Pc.principal(address1).willSendEq(50).ft(token, "widget")] },
    );

    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });

  it("aborts a transfer that violates the conditions", () => {
    expect(() =>
      simnet.callPublicFn("widget", "send", [Cl.uint(50), Cl.principal(address2)], address1, {
        postConditions: [Pc.principal(address1).willSendEq(49).ft(token, "widget")],
      }),
    ).toThrow(/Post-condition check failure/);

    // The tokens never moved.
    const { result } = simnet.callReadOnlyFn(
      "widget",
      "balance-of",
      [Cl.principal(address1)],
      address1,
    );
    expect(result).toStrictEqual(Cl.uint(1000));
  });
});

describe("post-conditions on non-fungible tokens", () => {
  // The `Nonfungible` variant embeds a Clarity value for the asset id, so this
  // exercises value serialization inside a post-condition.
  const source = `
(define-non-fungible-token badge uint)
(define-public (mint (id uint) (to principal)) (nft-mint? badge id to))
(define-public (send (id uint) (to principal)) (nft-transfer? badge id tx-sender to))
(define-read-only (owner-of (id uint)) (nft-get-owner? badge id))
`;

  let token: string;

  beforeEach(() => {
    simnet.deployContract("badge", source, null, deployer);
    token = `${deployer}.badge`;
    simnet.callPublicFn("badge", "mint", [Cl.uint(7), Cl.principal(address1)], deployer);
  });

  it("accepts a transfer that satisfies the conditions", () => {
    const { result } = simnet.callPublicFn(
      "badge",
      "send",
      [Cl.uint(7), Cl.principal(address2)],
      address1,
      {
        postConditions: [Pc.principal(address1).willSendAsset().nft(token, "badge", Cl.uint(7))],
      },
    );

    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });

  it("aborts when the condition names a different asset id", () => {
    expect(() =>
      simnet.callPublicFn("badge", "send", [Cl.uint(7), Cl.principal(address2)], address1, {
        postConditions: [Pc.principal(address1).willSendAsset().nft(token, "badge", Cl.uint(8))],
      }),
    ).toThrow(/Post-condition check failure/);

    expect(simnet.callReadOnlyFn("badge", "owner-of", [Cl.uint(7)], address1).result).toStrictEqual(
      Cl.some(Cl.principal(address1)),
    );
  });
});

describe("post-conditions in a mined block", () => {
  it("carries conditions through mineBlock", () => {
    const [ok] = simnet.mineBlock([
      tx.callPublicFn("counter", "increment", [], address1, {
        postConditions: [Pc.principal(address1).willSendEq(INCREMENT_COST).ustx()],
      }),
    ]);

    expect(ok.result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });

  it("surfaces a violation in the batch as an error", () => {
    const address1Nonce = simnet.getAccountNonce(address1);
    const address2Nonce = simnet.getAccountNonce(address2);

    expect(() =>
      simnet.mineBlock([
        tx.callPublicFn("counter", "increment", [], address1, {
          postConditions: [Pc.principal(address1).willSendEq(INCREMENT_COST).ustx()],
        }),
        tx.callPublicFn("counter", "increment", [], address2, {
          postConditions: [Pc.principal(address2).willSendEq(1).ustx()],
        }),
      ]),
    ).toThrow(/Post-condition check failure/);

    // Transactions before the abort remain committed; the aborted transaction
    // rolls back its payload but still consumes its nonce.
    expect(getCount()).toStrictEqual(Cl.ok(Cl.tuple({ count: Cl.uint(1) })));
    expect(simnet.getAccountNonce(address1)).toBe(address1Nonce + 1n);
    expect(simnet.getAccountNonce(address2)).toBe(address2Nonce + 1n);
  });

  it("carries conditions through a deployContract in mineBlock", () => {
    const source = `(begin (unwrap-panic (stx-transfer? u500 tx-sender '${address2})))`;

    const [ok] = simnet.mineBlock([
      tx.deployContract("pays-in-block", source, null, address1, {
        postConditions: [Pc.principal(address1).willSendEq(500).ustx()],
      }),
    ]);
    expect(ok.result).toStrictEqual(Cl.bool(true));

    expect(() =>
      simnet.mineBlock([
        tx.deployContract("pays-again", source, null, address1, {
          postConditions: [Pc.principal(address1).willSendEq(1).ustx()],
        }),
      ]),
    ).toThrow(/Post-condition check failure/);
  });

  it("rejects post-conditions on a transferSTX in mineBlock", () => {
    expect(() =>
      simnet.mineBlock([
        tx.transferSTX(1000, address2, address1, {
          postConditions: [Pc.principal(address1).willSendEq(1000).ustx()],
        }),
      ]),
    ).toThrow(/TokenTransfer transactions do not support post-conditions/);
  });
});

describe("a contract body runs as its declared deployer", () => {
  it("debits the deployer, not whoever the session last acted as", () => {
    const source = `(begin (unwrap-panic (stx-transfer? u500 tx-sender '${address2})))`;
    const before = simnet.getAssetsMap().get("STX")!;

    simnet.deployContract("pays-on-deploy", source, null, address1);

    const after = simnet.getAssetsMap().get("STX")!;
    expect(after.get(address1)).toBe(before.get(address1)! - 500n);
    expect(after.get(deployer)).toBe(before.get(deployer));
    expect(after.get(address2)).toBe(before.get(address2)! + 500n);
  });
});

describe("deployer is unaffected", () => {
  it("keeps existing behavior for calls with no options", () => {
    const { result } = simnet.callPublicFn("counter", "add", [Cl.uint(2)], deployer);
    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });
});
