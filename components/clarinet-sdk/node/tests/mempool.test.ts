import {
  Cl,
  makeContractCall,
  makeContractDeploy,
  makeSTXTokenTransfer,
  sponsorTransaction,
} from "@stacks/transactions";
import { beforeEach, expect, it } from "vitest";
import { initSimnet, type Simnet } from "..";
import fixtures from "./fixtures/raw-transactions/transactions.json";

const { sender, sponsor } = fixtures;
const senderKey = "1".padStart(64, "0") + "01";
const sponsorKey = "2".padStart(64, "0") + "01";
const options = { network: "testnet" as const, senderKey, fee: 1000n, nonce: 0n };
let simnet: Simnet;
const balance = (address: string) => simnet.getAssetsMap().get("STX")!.get(address)!;
beforeEach(async () => {
  simnet = await initSimnet("tests/fixtures/Clarinet.toml");
  simnet.setEpoch("3.4");
  simnet.mintSTX(sender, 1000000n);
  simnet.mintSTX(sponsor, 1000000n);
});
const transfer = (nonce = 0n, amount = 100n, fee = 1000n) =>
  makeSTXTokenTransfer({ ...options, nonce, amount, fee, recipient: sponsor });

it("queues signed transactions, deduplicates submissions, and mines the queue once", async () => {
  const first = await transfer();
  const second = await transfer(1n);
  const height = simnet.blockHeight;
  const burnHeight = simnet.burnBlockHeight;
  for (const tx of [first, first, second]) {
    expect(simnet.broadcastTransaction(tx.serialize())).toEqual({ txid: tx.txid() });
    expect(simnet.blockHeight).toBe(height);
    expect(simnet.burnBlockHeight).toBe(burnHeight);
    expect(simnet.getAccountNonce(sender)).toBe(0n);
    expect(balance(sender)).toBe(1000000n);
  }
  const receipts = simnet.mineBlock();
  expect(receipts.map((r) => [r.status, r.transaction?.txid])).toEqual([
    ["success", first.txid()],
    ["success", second.txid()],
  ]);
  expect(simnet.blockHeight).toBe(height + 1);
  expect(simnet.getAccountNonce(sender)).toBe(2n);
  expect(balance(sender)).toBe(997800n);
  expect(simnet.broadcastTransaction(first.serialize())).toMatchObject({ reason: "BadNonce" });
  expect(simnet.mineBlock()).toEqual([]);
});

it("matches the serialized-byte minimum fee, including a 0x prefix", async () => {
  const sample = await transfer();
  const size = BigInt(sample.serialize().length / 2);
  const low = await transfer(0n, 100n, size - 1n);
  expect(simnet.broadcastTransaction(low.serialize())).toEqual({
    error: "transaction rejected",
    reason: "FeeTooLow",
    reason_data: { actual: Number(size - 1n), expected: Number(size) },
    txid: low.txid(),
  });
  const exact = await transfer(0n, 100n, size);
  expect(simnet.broadcastTransaction(`0x${exact.serialize()}`)).toEqual({ txid: exact.txid() });
  expect(balance(sender)).toBe(1000000n);
  expect(simnet.mineBlock()[0].status).toBe("success");
  expect(balance(sender)).toBe(1000000n - 100n - size);
});

it("rejects malformed bytes, bad signatures, and conflicting queued nonces", async () => {
  expect(simnet.broadcastTransaction("xyz")).toMatchObject({ reason: "Deserialization" });
  expect(simnet.broadcastTransaction("xyz")).not.toHaveProperty("txid");
  const tx = await transfer();
  tx.setFee(1001n);
  expect(simnet.broadcastTransaction(tx.serialize())).toMatchObject({
    reason: "SignatureValidation",
  });
  const first = await transfer();
  expect(simnet.broadcastTransaction(first.serialize())).toEqual({ txid: first.txid() });
  expect(simnet.broadcastTransaction((await transfer(0n, 200n)).serialize())).toMatchObject({
    reason: "ConflictingNonceInMempool",
  });
  expect(simnet.mineBlock()).toHaveLength(1);
});

it("rechecks state at mining and continues after rejection", async () => {
  const first = await transfer(0n, 600000n);
  const second = await transfer(1n, 600000n);
  for (const tx of [first, second]) {
    expect(simnet.broadcastTransaction(tx.serialize())).toEqual({ txid: tx.txid() });
  }
  const results = simnet.mineBlock([
    { transferSTX: { amount: 1, recipient: sender, sender: sponsor } },
  ]);
  expect(results.map((r) => r.status)).toEqual(["success", "rejected", "success"]);
  expect(results[1].transaction?.txid).toBe(second.txid());
  expect(simnet.getAccountNonce(sender)).toBe(1n);
  expect(balance(sender)).toBe(399001n);
  expect(simnet.mineBlock()).toEqual([]);
});

it("queues sponsored transactions without charging either account", async () => {
  const tx = await sponsorTransaction({
    transaction: await makeSTXTokenTransfer({
      ...options,
      recipient: sponsor,
      amount: 100n,
      sponsored: true,
    }),
    sponsorPrivateKey: sponsorKey,
    sponsorNonce: 0n,
    fee: 1000n,
  });
  expect(simnet.broadcastTransaction(tx.serialize())).toEqual({ txid: tx.txid() });
  expect(balance(sender)).toBe(1000000n);
  expect(balance(sponsor)).toBe(1000000n);
  expect(simnet.getAccountNonce(sponsor)).toBe(0n);
  expect(simnet.mineBlock()[0].status).toBe("success");
  expect(balance(sender)).toBe(999900n);
  expect(balance(sponsor)).toBe(999100n);
  expect(simnet.getAccountNonce(sponsor)).toBe(1n);
});

it("validates against mined contracts, then reports execution failures at mining", async () => {
  const deploy = await makeContractDeploy({
    ...options,
    contractName: "queued",
    clarityVersion: 5,
    codeBody: "(define-public (fail) (err u7))",
  });
  expect(simnet.broadcastTransaction(deploy.serialize())).toEqual({ txid: deploy.txid() });
  expect(simnet.getContractsInterfaces().has(`${sender}.queued`)).toBe(false);
  const call = await makeContractCall({
    ...options,
    nonce: 1n,
    contractAddress: sender,
    contractName: "queued",
    functionName: "fail",
    functionArgs: [],
  });
  expect(simnet.broadcastTransaction(call.serialize())).toMatchObject({ reason: "NoSuchContract" });
  expect(simnet.mineBlock()[0].status).toBe("success");
  expect(simnet.getContractsInterfaces().has(`${sender}.queued`)).toBe(true);
  expect(simnet.broadcastTransaction(call.serialize())).toEqual({ txid: call.txid() });
  expect(simnet.getAccountNonce(sender)).toBe(1n);
  expect(simnet.mineBlock()[0]).toMatchObject({
    status: "included_failure",
    kind: "abort_by_response",
    result: Cl.error(Cl.uint(7)),
    transaction: { txid: call.txid(), fee: 1000n, nonce: 1n },
  });
  expect(simnet.getAccountNonce(sender)).toBe(2n);
});

it("uses Clarity trait casting during admission", async () => {
  simnet.deployContract(
    "traits",
    "(define-trait action ((run () (response uint uint))))",
    null,
    sender,
  );
  simnet.deployContract(
    "implementation",
    `(impl-trait .traits.action) (define-public (run) (ok u1))`,
    null,
    sender,
  );
  simnet.deployContract(
    "consumer",
    `(use-trait action .traits.action)
    (define-public (invoke (target <action>)) (contract-call? target run))`,
    null,
    sender,
  );
  const call = await makeContractCall({
    ...options,
    nonce: simnet.getAccountNonce(sender),
    contractAddress: sender,
    contractName: "consumer",
    functionName: "invoke",
    functionArgs: [Cl.contractPrincipal(sender, "implementation")],
    postConditionMode: "allow",
  });
  expect(simnet.broadcastTransaction(call.serialize())).toEqual({ txid: call.txid() });
  expect(simnet.mineBlock()[0]).toMatchObject({
    status: "success",
    execution: { result: Cl.ok(Cl.uint(1)) },
  });
});

it("retains nonce gaps and mines out-of-order submissions when their predecessors arrive", async () => {
  const future = await transfer(1n);
  expect(simnet.broadcastTransaction(future.serialize())).toEqual({ txid: future.txid() });
  expect(simnet.mineBlock()).toEqual([]);
  const first = await transfer();
  expect(simnet.broadcastTransaction(first.serialize())).toEqual({ txid: first.txid() });
  expect(simnet.mineBlock().map((r) => r.transaction?.txid)).toEqual([first.txid(), future.txid()]);
  expect(simnet.getAccountNonce(sender)).toBe(2n);
  const tooFar = await transfer(29n);
  expect(simnet.broadcastTransaction(tooFar.serialize())).toMatchObject({
    reason: "TooMuchChaining",
    reason_data: { expected: 28, actual: 29 },
  });
});
