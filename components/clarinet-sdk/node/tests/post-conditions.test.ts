import { Cl, Pc, postConditionToHex } from "@stacks/transactions";
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

  it("permits unlisted asset movement in allow mode", () => {
    const { result } = simnet.callPublicFn("counter", "increment", [], address1, {
      postConditions: [],
      postConditionMode: "allow",
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
        postConditionMode: "originator",
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
  it("accepts a transfer that satisfies the conditions", () => {
    const { result } = simnet.transferSTX(1000, address2, address1, {
      postConditions: [Pc.principal(address1).willSendEq(1000).ustx()],
    });

    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });

  it("aborts a transfer that violates the conditions", () => {
    expect(() =>
      simnet.transferSTX(1000, address2, address1, {
        postConditions: [Pc.principal(address1).willSendEq(999).ustx()],
      }),
    ).toThrow(/Post-condition check failure/);

    expect(simnet.getAssetsMap().get("STX")?.get(address1)).toBe(100000000000000n);
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
    expect(() =>
      simnet.deployContract("pays-on-deploy", source, null, address1, {
        postConditions: [Pc.principal(address1).willSendEq(1).ustx()],
      }),
    ).toThrow(/Post-condition check failure/);

    // The aborted deployment left no contract behind.
    expect(simnet.getContractSource(`${address1}.pays-on-deploy`)).toBeUndefined();
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
  });

  it("carries conditions through a transferSTX in mineBlock", () => {
    const [ok] = simnet.mineBlock([
      tx.transferSTX(1000, address2, address1, {
        postConditions: [Pc.principal(address1).willSendEq(1000).ustx()],
      }),
    ]);

    expect(ok.result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });
});

describe("read-only calls", () => {
  it("takes no post-condition options", () => {
    // @ts-expect-error read-only calls move no assets
    simnet.callReadOnlyFn("counter", "get-count", [], address1, { postConditionMode: "deny" });
  });
});

describe("deployer is unaffected", () => {
  it("keeps existing behavior for calls with no options", () => {
    const { result } = simnet.callPublicFn("counter", "add", [Cl.uint(2)], deployer);
    expect(result).toStrictEqual(Cl.ok(Cl.bool(true)));
  });
});
