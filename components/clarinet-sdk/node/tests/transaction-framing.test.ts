import { Cl, Pc } from "@stacks/transactions";
import { beforeEach, expect, it } from "vitest";
import { initSimnet, Simnet, tx } from "..";

const sender = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
const recipient = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
let simnet: Simnet;

beforeEach(async () => {
  simnet = await initSimnet("tests/fixtures/Clarinet.toml");
});

it("executes read-only batch targets as transactions and keeps RPC reads nonce-free", () => {
  simnet.deployContract(
    "read-targets",
    `(define-data-var count uint u0)
     (define-public (increment) (ok (var-set count (+ (var-get count) u1))))
     (define-read-only (peek) (var-get count))
     (define-read-only (fail) (err u7))
     (define-read-only (panic) (/ u1 u0))
     (define-private (hidden) u99)`,
    null,
    sender,
  );
  const nonce = simnet.getAccountNonce(sender);
  const height = simnet.blockHeight;
  const results = simnet.mineBlock([
    tx.callPublicFn("read-targets", "peek", [], sender),
    tx.callPublicFn("read-targets", "increment", [], sender),
    tx.callPublicFn("read-targets", "peek", [], sender),
    tx.callPublicFn("read-targets", "fail", [], sender),
    tx.callPublicFn("read-targets", "panic", [], sender),
    tx.callPublicFn("read-targets", "hidden", [], sender),
    tx.callPublicFn("read-targets", "peek", [], sender),
  ]);
  expect(results).toMatchObject([
    { status: "success", execution: { result: Cl.uint(0) } },
    { status: "success", execution: { result: Cl.ok(Cl.bool(true)) } },
    { status: "success", execution: { result: Cl.uint(1) } },
    {
      status: "included_failure",
      kind: "abort_by_response",
      execution: { result: Cl.error(Cl.uint(7)) },
    },
    {
      status: "included_failure",
      kind: "runtime_error",
      execution: { result: Cl.error(Cl.none()), events: [] },
      error: expect.stringMatching(/DivisionByZero/),
    },
    {
      status: "included_failure",
      kind: "runtime_error",
      execution: { result: Cl.error(Cl.none()), events: [] },
      error: "hidden is not a public function",
    },
    { status: "success", execution: { result: Cl.uint(1) } },
  ]);
  expect(simnet.blockHeight).toBe(height + 1);
  expect(simnet.getAccountNonce(sender)).toBe(nonce + 7n);
  expect(simnet.callReadOnlyFn("read-targets", "peek", [], sender).result).toEqual(Cl.uint(1));
  expect(simnet.getAccountNonce(sender)).toBe(nonce + 7n);
  expect(simnet.blockHeight).toBe(height + 1);
  expect(simnet.callPublicFn("read-targets", "peek", [], sender).result).toEqual(Cl.uint(1));
  expect(simnet.getAccountNonce(sender)).toBe(nonce + 8n);
  expect(simnet.blockHeight).toBe(height + 2);
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

it.each([0, 99, 257])("rejects Clarity version %i instead of using a default", (version) => {
  const nonce = simnet.getAccountNonce(sender);
  const height = simnet.blockHeight;
  expect(() =>
    simnet.deployContract("versioned", "true", { clarityVersion: version }, sender),
  ).toThrow(`Invalid Clarity version: ${version}`);
  expect(
    simnet.mineBlock([tx.deployContract("versioned", "true", { clarityVersion: version }, sender)]),
  ).toEqual([{ status: "rejected", error: `Invalid Clarity version: ${version}` }]);
  expect(simnet.getAccountNonce(sender)).toBe(nonce);
  // Malformed input throws before the standalone call mines its block.
  expect(simnet.blockHeight).toBe(height + 1);
  expect(simnet.getContractsInterfaces().has(`${sender}.versioned`)).toBe(false);
});

it("rejects non-integer versions without coercion and continues mining valid entries", () => {
  const versions: unknown[] = [
    1.5,
    4294967297,
    -4294967295,
    Number.MAX_SAFE_INTEGER,
    NaN,
    Infinity,
    -Infinity,
    "1",
    true,
    false,
    null,
    1n,
    [1],
    {},
  ];
  const nonce = simnet.getAccountNonce(sender);
  const height = simnet.blockHeight;
  const error = "Invalid Clarity version: expected an unsigned 32-bit integer";
  const entries = versions.map((version, index) => {
    // Exercise JavaScript callers that do not have TypeScript's number constraint.
    const options = { clarityVersion: version as number };
    const name = `invalid-version-${index}`;
    expect(() => simnet.deployContract(name, "true", options, sender)).toThrow(error);
    return tx.deployContract(name, "true", options, sender);
  });
  expect(simnet.blockHeight).toBe(height);
  expect(simnet.getAccountNonce(sender)).toBe(nonce);

  const results = simnet.mineBlock([
    tx.deployContract("before-invalid-versions", "true", { clarityVersion: 1 }, sender),
    ...entries,
    tx.deployContract("after-invalid-versions", "true", null, sender),
  ]);
  expect(results[0].status).toBe("success");
  expect(results.slice(1, -1)).toEqual(versions.map(() => ({ status: "rejected", error })));
  expect(results.at(-1)?.status).toBe("success");
  expect(simnet.blockHeight).toBe(height + 1);
  expect(simnet.getAccountNonce(sender)).toBe(nonce + 2n);
  for (let index = 0; index < versions.length; index++) {
    expect(simnet.getContractsInterfaces().has(`${sender}.invalid-version-${index}`)).toBe(false);
  }

  expect(simnet.mineBlock(entries)).toEqual(versions.map(() => ({ status: "rejected", error })));
  expect(simnet.blockHeight).toBe(height + 2);
  expect(simnet.getAccountNonce(sender)).toBe(nonce + 2n);
});

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
  const height = simnet.blockHeight;
  const balance = simnet.getAssetsMap().get("STX")!.get(sender)!;
  expect(() => simnet.transferSTX(1, sender, sender)).toThrow(/send to itself/);
  expect(() => simnet.transferSTX(0, recipient, sender)).toThrow(/amount must be positive/);
  expect(() => simnet.transferSTX(balance + 1n, recipient, sender)).toThrow(
    /insufficient unlocked balance/,
  );
  expect(simnet.getAccountNonce(sender)).toBe(nonce);
  expect(simnet.getAssetsMap().get("STX")!.get(sender)).toBe(balance);
  // Like `mineBlock([entry])`, each call advances one block, even when rejected.
  expect(simnet.blockHeight).toBe(height + 3);
  simnet.transferSTX(1, recipient, sender);
  expect(simnet.blockHeight).toBe(height + 4);
});

it("rejects invalid transfers in a block and keeps mining it", () => {
  const nonce = simnet.getAccountNonce(sender);
  const height = simnet.blockHeight;
  const balance = simnet.getAssetsMap().get("STX")!.get(sender)!;
  const results = simnet.mineBlock([
    tx.transferSTX(1, sender, sender),
    tx.transferSTX(0, recipient, sender),
    tx.transferSTX(Number(balance + 1n), recipient, sender),
    tx.transferSTX(1, recipient, sender),
  ]);
  expect(results.map(({ status }) => status)).toEqual([
    "rejected",
    "rejected",
    "rejected",
    "success",
  ]);
  expect(results[3]).toMatchObject({ execution: { result: Cl.ok(Cl.bool(true)) } });
  expect(simnet.getAccountNonce(sender)).toBe(nonce + 1n);
  expect(simnet.blockHeight).toBe(height + 1);
  expect(simnet.getAssetsMap().get("STX")!.get(sender)).toBe(balance - 1n);
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

it("reports every mixed-batch outcome and keeps only committed deployment caches", () => {
  const nonce = simnet.getAccountNonce(sender);
  const height = simnet.blockHeight;
  const balance = simnet.getAssetsMap().get("STX")!.get(sender)!;
  const source = `(define-data-var count uint u0)
    (define-public (increment) (ok (var-set count (+ (var-get count) u1))))
    (define-public (fail-response) (begin (var-set count u99) (err u7)))
    (define-public (fail-runtime) (begin (var-set count u99) (ok (/ u1 u0))))`;
  const results = simnet.mineBlock([
    tx.deployContract("batch", source, null, sender),
    tx.callPublicFn("batch", "increment", [], sender),
    tx.transferSTX(0, recipient, sender),
    tx.callPublicFn("batch", "fail-response", [], sender),
    tx.callPublicFn("batch", "fail-runtime", [], sender),
    tx.deployContract("batch", "(+ u1 true)", null, sender),
    tx.deployContract(
      "retry",
      `(define-data-var count uint u0) (stx-transfer? u1 tx-sender '${recipient})`,
      null,
      sender,
      {
        postConditions: [Pc.principal(sender).willSendEq(2).ustx()],
      },
    ),
    tx.deployContract("retry", "(define-private (private-value) u42)", null, sender),
    tx.callPrivateFn("retry", "private-value", [], sender),
    tx.callPublicFn("batch", "increment", [], sender),
    tx.transferSTX(1, recipient, sender),
  ]);
  expect(results.map((r) => r.status)).toEqual([
    "success",
    "success",
    "rejected",
    "included_failure",
    "included_failure",
    "rejected",
    "included_failure",
    "success",
    "success",
    "success",
    "success",
  ]);
  expect(results[3]).toMatchObject({
    kind: "abort_by_response",
    execution: { result: Cl.error(Cl.uint(7)), events: [] },
  });
  // Like a node's receipt: `(err none)` when execution produced no value.
  expect(results[4]).toMatchObject({
    kind: "runtime_error",
    execution: { result: Cl.error(Cl.none()), events: [] },
    error: expect.stringMatching(/DivisionByZero/),
  });
  // An aborted deployment reports `(ok true)` and its attempted, uncommitted events.
  expect(results[6]).toMatchObject({
    kind: "abort_by_post_condition",
    execution: {
      result: Cl.ok(Cl.bool(true)),
      events: [expect.objectContaining({ event: "stx_transfer_event" })],
    },
    error: expect.stringMatching(/Post-condition/),
  });
  expect(results[8]).toMatchObject({ execution: { result: Cl.uint(42) } });
  expect(simnet.getDataVar("batch", "count")).toEqual(Cl.uint(2));
  expect(simnet.getContractSource("batch")).toBe(source);
  expect(simnet.getContractSource("retry")).toBe("(define-private (private-value) u42)");
  expect(
    simnet
      .getContractsInterfaces()
      .get(`${sender}.retry`)!
      .functions.map((f) => f.name),
  ).toEqual(["private-value"]);
  expect(simnet.getAccountNonce(sender)).toBe(nonce + 9n);
  expect(simnet.getAssetsMap().get("STX")!.get(sender)).toBe(balance - 1n);
  expect(simnet.blockHeight).toBe(height + 1);
});

it("mines empty and entirely rejected blocks once without charging nonces", () => {
  const height = simnet.blockHeight;
  const nonce = simnet.getAccountNonce(sender);
  expect(simnet.mineBlock([])).toEqual([]);
  expect(simnet.blockHeight).toBe(height + 1);
  expect(simnet.mineBlock([tx.transferSTX(0, recipient, sender)])).toEqual([
    expect.objectContaining({ status: "rejected" }),
  ]);
  expect(simnet.blockHeight).toBe(height + 2);
  expect(simnet.getAccountNonce(sender)).toBe(nonce);
});

it.each(["2.05", "2.1"] as const)("uses the VM inclusion rules in epoch %s", (epoch) => {
  simnet.setEpoch(epoch);
  const nonce = simnet.getAccountNonce(sender);
  const results = simnet.mineBlock([
    tx.callPublicFn("not-deployed", "missing", [], sender),
    tx.transferSTX(1, recipient, sender),
  ]);
  expect(results[0].status).toBe(epoch === "2.05" ? "rejected" : "included_failure");
  expect(results[1].status).toBe("success");
  expect(simnet.getAccountNonce(sender)).toBe(nonce + (epoch === "2.05" ? 1n : 2n));
});
