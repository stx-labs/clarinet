import { tx } from "@stacks/clarinet-sdk";
import { Cl } from "@stacks/transactions";
import { assert, expect, it } from "vitest";

const accounts = simnet.getAccounts();
const address1 = accounts.get("wallet_1")!;
const address2 = accounts.get("wallet_2")!;

/*
  The test below is an example. Learn more in the documentation: https://docs.stacks.co/clarinet/testing-with-clarinet-sdk
*/

it("Ensure that counter can be incremented multiple times per block, across multiple blocks", () => {
  const block1 = simnet.mineBlock([
    tx.callPublicFn("counter", "increment", [Cl.uint(1)], address1),
    tx.callPublicFn("counter", "increment", [Cl.uint(4)], address2),
    tx.callPublicFn("counter", "increment", [Cl.uint(10)], address1),
  ]);

  const txs1 = block1.map((r, i) => {
    assert(r.status === "success", `tx ${i}: ${r.status}`);
    return r.execution;
  });

  expect(txs1[0].result).toBeOk(Cl.uint(2));
  expect(txs1[1].result).toBeOk(Cl.uint(6));
  expect(txs1[2].result).toBeOk(Cl.uint(16));

  const block2 = simnet.mineBlock([
    tx.callPublicFn("counter", "increment", [Cl.uint(1)], address2),
    tx.callPublicFn("counter", "increment", [Cl.uint(4)], address2),
    tx.callPublicFn("counter", "increment", [Cl.uint(10)], address1),
    tx.transferSTX(1, address2, address1),
  ]);

  const txs2 = block2.map((r, i) => {
    assert(r.status === "success", `tx ${i}: ${r.status}`);
    return r.execution;
  });

  expect(txs2[0].result).toBeOk(Cl.uint(17));
  expect(txs2[1].result).toBeOk(Cl.uint(21));
  expect(txs2[2].result).toBeOk(Cl.uint(31));

  const assets = simnet.getAssetsMap();
  expect(assets.get("STX")?.get(address1)).toBe(99999999999999n);

  const { result } = simnet.callReadOnlyFn(
    "counter",
    "read-counter",
    [],
    simnet.deployer,
  );
  expect(result).toBeOk(Cl.uint(31));
});
