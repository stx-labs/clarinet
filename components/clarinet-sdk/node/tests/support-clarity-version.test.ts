import { Cl, ClarityType } from "@stacks/transactions";
import { describe, expect, it, beforeEach } from "vitest";

// test the built package and not the source code
// makes it simpler to handle wasm build
import { Simnet, initSimnet } from "..";

let simnet: Simnet;

beforeEach(async () => {
  simnet = await initSimnet("tests/fixtures/Clarinet.toml");
});

describe("clarity 1", () => {
  it("handles clarity 1", () => {
    simnet.setEpoch("2.05");
    let resOk = simnet.execute('(index-of "stacks" "s")');
    expect(resOk.result).toStrictEqual(Cl.some(Cl.uint(0)));

    // `index-of?` was introduced in clarity 2
    expect(() => simnet.execute('(index-of? "stacks" "s")')).toThrowError(
      "error: use of unresolved function 'index-of?'",
    );

    // `tenure-height` was introduced in clarity 3
    expect(() => simnet.execute("(print tenure-height)")).toThrowError(
      "error: use of unresolved variable 'tenure-height'",
    );
  });
});

describe("clarity 2", () => {
  it("handles clarity 2", () => {
    simnet.setEpoch("2.4");
    // `index-of` is still available in clarity 2
    let resOk1 = simnet.execute('(index-of "stacks" "s")');
    expect(resOk1.result).toStrictEqual(Cl.some(Cl.uint(0)));

    // `index-of?` is available in clarity 2
    let resOk2 = simnet.execute('(index-of? "stacks" "s")');
    expect(resOk2.result).toStrictEqual(Cl.some(Cl.uint(0)));

    // `block-height` is available in clarity 1 & 2
    let resOk3 = simnet.execute("(print block-height)");
    expect(resOk3.result).toStrictEqual(Cl.uint(3));

    // `tenure-height` was introduced in clarity 3
    expect(() => simnet.execute("(print tenure-height)")).toThrowError(
      "error: use of unresolved variable 'tenure-height'",
    );
  });
});

describe("clarity 3", () => {
  it("handles clarity 3", () => {
    simnet.setEpoch("3.0");
    // `index-of` is still available in clarity 2
    let resOk1 = simnet.execute('(index-of "stacks" "s")');
    expect(resOk1.result).toStrictEqual(Cl.some(Cl.uint(0)));

    // `index-of?` is available in clarity 2
    let resOk2 = simnet.execute('(index-of? "stacks" "s")');
    expect(resOk2.result).toStrictEqual(Cl.some(Cl.uint(0)));

    // `tenure-height` was introduced in clarity 3
    let resOk3 = simnet.execute("(print tenure-height)");
    expect(resOk3.result).toStrictEqual(Cl.uint(4));

    // `block-height` was removed in clarity 3
    expect(() => simnet.execute("(print block-height)")).toThrowError(
      "error: use of unresolved variable 'block-height'",
    );
  });
});

describe("clarity 4", () => {
  it("handles `to-ascii`", () => {
    simnet.setEpoch("3.3");

    const src = `(define-public (convert (addr principal)) (to-ascii? addr))`;
    simnet.deployContract("to-ascii-test", src, { clarityVersion: 4 }, simnet.deployer);

    const { result } = simnet.callPublicFn(
      "to-ascii-test",
      "convert",
      [Cl.contractPrincipal("ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM", "counter")],
      simnet.deployer,
    );
    expect(result).toStrictEqual(
      Cl.ok(Cl.stringAscii("ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.counter")),
    );
  });

  // doesn't work atm, probably need to tweak the contract hash handling in the simnet datastore
  it("handles `contract-hash?`", () => {
    simnet.setEpoch("3.3");

    const src = `(define-public (get-contract-hash-for (addr principal)) (contract-hash? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.counter))`;
    simnet.deployContract("get-contract-hash", src, { clarityVersion: 4 }, simnet.deployer);

    const { result } = simnet.callPublicFn(
      "get-contract-hash",
      "get-contract-hash-for",
      [Cl.contractPrincipal("ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM", "counter")],
      simnet.deployer,
    );
    expect(result.type).toBe(ClarityType.ResponseOk);
  });
});
