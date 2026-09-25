import { Cl, Pc } from "@stacks/transactions";
import { beforeEach, expect, it } from "vitest";
import { initSimnet, Simnet } from "..";

const sender = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
const recipient = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
let simnet: Simnet;

beforeEach(async () => {
  simnet = await initSimnet("tests/fixtures/Clarinet.toml");
});

it.each(["u123", "(err u123)", "false"])(
  "returns the node receipt for a function-free deployment ending in %s",
  (lastExpression) => {
    const source = `(define-data-var answer uint u42) ${lastExpression}`;
    const before = simnet.getAccountNonce(sender);
    expect(simnet.deployContract("data-only", source, null, sender).result).toEqual(
      Cl.ok(Cl.bool(true)),
    );
    expect(simnet.getDataVar("data-only", "answer")).toEqual(Cl.uint(42));
    expect(simnet.getContractSource("data-only")).toBe(source);
    expect(simnet.getAccountNonce(sender)).toBe(before + 1n);
    expect(simnet.execute("u123").result).toEqual(Cl.uint(123));
    expect(simnet.getAccountNonce(sender)).toBe(before + 1n);
  },
);

it("rejects a duplicate deployment before analysis without consuming a nonce", () => {
  simnet.deployContract("existing", "(define-data-var answer uint u42)", null, sender);
  const nonce = simnet.getAccountNonce(sender);
  expect(() => simnet.deployContract("existing", "(+ u1 true)", null, sender)).toThrow(
    /Duplicate contract/,
  );
  expect(simnet.getAccountNonce(sender)).toBe(nonce);
  expect(simnet.getDataVar("existing", "answer")).toEqual(Cl.uint(42));
});

it("rejects invalid native transfers without consuming a nonce", () => {
  const nonce = simnet.getAccountNonce(sender);
  const balance = simnet.getAssetsMap().get("STX")!.get(sender)!;
  expect(() => simnet.transferSTX(1, sender, sender)).toThrow(/send to itself/);
  expect(() => simnet.transferSTX(0, recipient, sender)).toThrow(/amount must be positive/);
  expect(() => simnet.transferSTX(balance + 1n, recipient, sender)).toThrow(
    /insufficient unlocked balance/,
  );
  expect(simnet.getAccountNonce(sender)).toBe(nonce);
  expect(simnet.getAssetsMap().get("STX")!.get(sender)).toBe(balance);
});

it("rolls back initializer writes, events, and interfaces on a post-condition abort", () => {
  const nonce = simnet.getAccountNonce(sender);
  const balances = simnet.getAssetsMap().get("STX");
  const source = `(define-data-var answer uint u42)
    (stx-transfer? u10 tx-sender '${recipient})`;
  expect(() =>
    simnet.deployContract("aborted", source, null, sender, {
      postConditions: [Pc.principal(sender).willSendEq(11).ustx()],
    }),
  ).toThrow(/Post-condition check failure/);
  expect(simnet.getAccountNonce(sender)).toBe(nonce + 1n);
  expect(simnet.getAssetsMap().get("STX")).toEqual(balances);
  expect(simnet.getContractSource("aborted")).toBeUndefined();
  expect(simnet.getContractsInterfaces().has(`${sender}.aborted`)).toBe(false);
  // The aborted deployment left no contract behind, so the same name is reusable.
  expect(simnet.deployContract("aborted", source, null, sender).result).toEqual(
    Cl.ok(Cl.bool(true)),
  );
});

it("checks FT and NFT movements from a contract principal and rolls both back on abort", () => {
  const vault = `${sender}.vault`;
  const source = `
    (define-fungible-token coin)
    (define-non-fungible-token badge uint)
    (define-public (withdraw (to principal))
      (as-contract (begin
        (try! (ft-transfer? coin u10 tx-sender to))
        (nft-transfer? badge u1 tx-sender to))))
    (define-read-only (balance) (ft-get-balance coin '${vault}))
    (define-read-only (owner) (nft-get-owner? badge u1))
    (ft-mint? coin u10 '${vault})
    (nft-mint? badge u1 '${vault})`;
  simnet.deployContract("vault", source, { clarityVersion: 3 }, sender);
  const nonce = simnet.getAccountNonce(sender);
  const conditions = [
    Pc.principal(vault).willSendEq(10).ft(vault, "coin"),
    Pc.principal(vault).willSendAsset().nft(vault, "badge", Cl.uint(1)),
  ];
  expect(() =>
    simnet.callPublicFn("vault", "withdraw", [Cl.principal(recipient)], sender, {
      postConditions: [
        conditions[0],
        Pc.principal(vault).willSendAsset().nft(vault, "badge", Cl.uint(2)),
      ],
    }),
  ).toThrow(/Post-condition check failure/);
  expect(simnet.getAccountNonce(sender)).toBe(nonce + 1n);
  expect(simnet.callReadOnlyFn("vault", "balance", [], sender).result).toEqual(Cl.uint(10));
  expect(simnet.callReadOnlyFn("vault", "owner", [], sender).result).toEqual(
    Cl.some(Cl.principal(vault)),
  );
  const result = simnet.callPublicFn("vault", "withdraw", [Cl.principal(recipient)], sender, {
    postConditions: conditions,
  });
  expect(result.result).toEqual(Cl.ok(Cl.bool(true)));
  expect(result.events).toHaveLength(2);
  expect(simnet.getAccountNonce(sender)).toBe(nonce + 2n);
  expect(simnet.callReadOnlyFn("vault", "balance", [], sender).result).toEqual(Cl.uint(0));
  expect(simnet.callReadOnlyFn("vault", "owner", [], sender).result).toEqual(
    Cl.some(Cl.principal(recipient)),
  );
});
