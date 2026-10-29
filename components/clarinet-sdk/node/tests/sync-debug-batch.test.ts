import { Cl, Pc, serializeCV } from "@stacks/transactions";
import { expect, it, vi } from "vitest";
import { tx } from "../../common/src/sdkProxyHelpers";
import { createSyncDebugSimnet } from "../src/syncDebugSimnet";
import { syncSend } from "../src/syncDebugSocket";

vi.mock("../src/syncDebugSocket", () => ({ syncSend: vi.fn() }));

it("preserves batch outcomes and transaction options through the synchronous adapter", () => {
  const sender = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
  const execution = { result: serializeCV(Cl.ok(Cl.bool(true))), events: "[]", costs: "null" };
  vi.mocked(syncSend)
    .mockReturnValueOnce(JSON.stringify({ result: { accounts: { deployer: sender } } }))
    .mockReturnValueOnce(
      JSON.stringify({
        result: {
          results: [
            { status: "rejected", error: "invalid transfer" },
            {
              status: "included_failure",
              kind: "runtime_error",
              error: "runtime",
              execution: { ...execution, result: serializeCV(Cl.error(Cl.none())) },
            },
            {
              status: "included_failure",
              kind: "abort_by_response",
              error: "(err u7)",
              execution: { ...execution, result: serializeCV(Cl.error(Cl.uint(7))) },
            },
            { status: "success", execution },
          ],
        },
      }),
    );
  const simnet = createSyncDebugSimnet();
  const call = tx.callPublicFn("counter", "increment", [Cl.uint(1)], sender, {
    postConditions: [Pc.principal(sender).willSendEq(1).ustx()],
  });
  const results = simnet.mineBlock([
    tx.transferSTX(0, sender, sender),
    call,
    call,
    tx.deployContract("new", "true", { clarityVersion: 3 }, sender),
  ]);
  expect(results).toEqual([
    { status: "rejected", error: "invalid transfer" },
    {
      status: "included_failure",
      kind: "runtime_error",
      error: "runtime",
      execution: { result: Cl.error(Cl.none()), events: [], costs: null, performance: undefined },
    },
    {
      status: "included_failure",
      kind: "abort_by_response",
      error: "(err u7)",
      execution: { result: Cl.error(Cl.uint(7)), events: [], costs: null, performance: undefined },
    },
    {
      status: "success",
      execution: { result: Cl.ok(Cl.bool(true)), events: [], costs: null, performance: undefined },
    },
  ]);
  expect(vi.mocked(syncSend).mock.calls[1][0]).toMatchObject({
    method: "mineBlock",
    txs: [
      { type: "transferSTX", amount: 0 },
      {
        type: "callPublicFn",
        args: [serializeCV(Cl.uint(1))],
        postConditions: call.callPublicFn!.postConditions,
      },
      { type: "callPublicFn" },
      { type: "deployContract", options: { clarityVersion: 3 } },
    ],
  });
});

it("retains nonempty batch events and decodes print and NFT values", () => {
  vi.mocked(syncSend).mockClear();
  const sender = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
  const recipient = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
  const transfer = {
    event: "stx_transfer_event",
    data: { sender, recipient, amount: "1", memo: `0x${"00".repeat(34)}` },
  };
  const print = {
    event: "print_event",
    data: {
      contract_identifier: `${sender}.events`,
      topic: "print",
      raw_value: serializeCV(Cl.uint(42)),
    },
  };
  const mint = {
    event: "nft_mint_event",
    data: {
      asset_identifier: `${sender}.events::badge`,
      recipient: sender,
      raw_value: serializeCV(Cl.uint(7)),
    },
  };
  const outcome = (events: unknown[]) => ({
    status: "success",
    execution: {
      result: serializeCV(Cl.ok(Cl.bool(true))),
      events: JSON.stringify(events.map((event) => JSON.stringify(event))),
      costs: "null",
    },
  });
  vi.mocked(syncSend)
    .mockReturnValueOnce(JSON.stringify({ result: { accounts: { deployer: sender } } }))
    .mockReturnValueOnce(
      JSON.stringify({ result: { results: [outcome([transfer]), outcome([print, mint])] } }),
    );
  const simnet = createSyncDebugSimnet();
  const results = simnet.mineBlock([
    tx.transferSTX(1, recipient, sender),
    tx.deployContract(
      "events",
      "(define-non-fungible-token badge uint) (print u42) (nft-mint? badge u7 tx-sender)",
      null,
      sender,
    ),
  ]);
  expect(results[0]).toMatchObject({ status: "success", execution: { events: [transfer] } });
  expect(results[1]).toMatchObject({
    status: "success",
    execution: {
      events: [
        { ...print, data: { ...print.data, value: Cl.uint(42) } },
        { ...mint, data: { ...mint.data, value: Cl.uint(7) } },
      ],
    },
  });
});

it("submits to the debug mempool and parses signed mining metadata", () => {
  vi.mocked(syncSend).mockReset();
  const txid = "ab".repeat(32);
  vi.mocked(syncSend)
    .mockReturnValueOnce(JSON.stringify({ result: { accounts: {} } }))
    .mockReturnValueOnce(JSON.stringify({ result: { txid } }))
    .mockReturnValueOnce(
      JSON.stringify({
        result: {
          results: [
            {
              status: "included_failure",
              kind: "abort_by_response",
              error: "response",
              transaction: { txid, fee: "1000", nonce: "0" },
              execution: {
                result: serializeCV(Cl.error(Cl.uint(7))),
                events: "[]",
                costs: "null",
                performance: null,
              },
            },
          ],
        },
      }),
    );
  const simnet = createSyncDebugSimnet();
  expect(simnet.broadcastTransaction("signed-hex")).toEqual({ txid });
  expect(simnet.mineBlock()[0]).toMatchObject({
    status: "included_failure",
    kind: "abort_by_response",
    transaction: { txid, fee: 1000n, nonce: 0n },
    execution: { result: Cl.error(Cl.uint(7)) },
  });
  expect(vi.mocked(syncSend).mock.calls[1][0]).toEqual({
    method: "broadcastTransaction",
    rawHex: "signed-hex",
  });
  expect(vi.mocked(syncSend).mock.calls[2][0]).toEqual({ method: "mineBlock", txs: [] });
});

it("rejects invalid versions before JSON serialization and preserves queued result order", () => {
  vi.mocked(syncSend).mockReset();
  const sender = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
  const execution = { result: serializeCV(Cl.ok(Cl.bool(true))), events: "[]", costs: "null" };
  const queued = {
    status: "success",
    transaction: { txid: "ab".repeat(32), fee: "1000", nonce: "0" },
    execution,
  };
  vi.mocked(syncSend)
    .mockReturnValueOnce(JSON.stringify({ result: { accounts: { deployer: sender } } }))
    .mockReturnValueOnce(
      JSON.stringify({ result: { results: [queued, { status: "success", execution }] } }),
    );
  const simnet = createSyncDebugSimnet();
  const versions: unknown[] = [NaN, Infinity, 1.5, 4294967297, "1", true, null, 1n];
  const invalid = versions.map((version, index) =>
    tx.deployContract(`bad-${index}`, "true", { clarityVersion: version as number }, sender),
  );
  const results = simnet.mineBlock([
    ...invalid,
    tx.deployContract("valid", "true", { clarityVersion: 1 }, sender),
  ]);
  expect(results[0]).toMatchObject({
    status: "success",
    transaction: { txid: queued.transaction.txid },
  });
  expect(results.slice(1, -1)).toEqual(
    versions.map(() => ({
      status: "rejected",
      error: "Invalid Clarity version: expected an unsigned 32-bit integer",
    })),
  );
  expect(results.at(-1)?.status).toBe("success");
  expect(vi.mocked(syncSend).mock.calls[1][0]).toMatchObject({
    method: "mineBlock",
    txs: [{ type: "deployContract", name: "valid", options: { clarityVersion: 1 } }],
  });
});
