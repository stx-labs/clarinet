// Deterministic public test keys; regenerate with node generate.mjs.
import { writeFileSync } from "node:fs";
import {
  Pc,
  getAddressFromPrivateKey,
  makeSTXTokenTransfer,
  makeContractDeploy,
  makeContractCall,
  sponsorTransaction,
} from "@stacks/transactions";
const senderKey = "1".padStart(64, "0") + "01";
const sponsorKey = "2".padStart(64, "0") + "01";
const sender = getAddressFromPrivateKey(senderKey, "testnet");
const sponsor = getAddressFromPrivateKey(sponsorKey, "testnet");
const options = { network: "testnet", senderKey, nonce: 0n, fee: 10n };
const source = `(define-data-var count uint u0)
(define-public (increment) (begin (var-set count (+ (var-get count) u1)) (ok (var-get count))))
(define-public (fail) (begin (var-set count u999) (try! (stx-transfer? u100 tx-sender '${sponsor})) (err u7)))
(define-public (crash) (begin (var-set count u999) (try! (stx-transfer? u100 tx-sender '${sponsor})) (ok (/ u1 u0))))
(define-public (send) (stx-transfer? u100 tx-sender '${sponsor}))
(define-public (payer) (ok tx-sponsor?))`;
const txs = {};
async function transfer(name, extra = {}) {
  txs[name] = await makeSTXTokenTransfer({
    ...options,
    recipient: sponsor,
    amount: 100n,
    memo: "raw memo",
    ...extra,
  });
}
const withSponsor = (transaction, { key = sponsorKey, nonce = 0n } = {}) =>
  sponsorTransaction({
    transaction,
    sponsorPrivateKey: key,
    sponsorNonce: nonce,
    fee: 10n,
    network: "testnet",
  });
const call = (functionName, extra = {}) =>
  makeContractCall({
    ...options,
    nonce: 1n,
    contractAddress: sender,
    contractName: "raw",
    functionName,
    functionArgs: [],
    postConditionMode: "allow",
    ...extra,
  });
const abortConditions = {
  postConditionMode: "deny",
  postConditions: [Pc.principal(sender).willSendEq(101).ustx()],
};
await transfer("transfer");
await transfer("wrong_nonce", { nonce: 1n });
await transfer("overflow_nonce", { nonce: 18446744073709551615n });
await transfer("wrong_network", { network: "mainnet" });
await transfer("zero", { amount: 0n });
await transfer("self", { recipient: sender });
await transfer("insufficient", { amount: 1000001n });
await transfer("insufficient_fee", { fee: 1000001n });
await transfer("sponsored", { sponsored: true });
txs.sponsored = await withSponsor(txs.sponsored);
await transfer("same_sponsor", { sponsored: true });
txs.same_sponsor = await withSponsor(txs.same_sponsor, { key: senderKey });
txs.deploy = await makeContractDeploy({
  ...options,
  contractName: "raw",
  codeBody: source,
  clarityVersion: 3,
});
txs.bad_deploy = await makeContractDeploy({
  ...options,
  contractName: "bad",
  codeBody: "(+ u1 true)",
  clarityVersion: 3,
});
for (const name of ["increment", "fail", "crash", "send"]) {
  txs[name] = await call(name);
}
txs.abort = await call("send", abortConditions);
txs.sponsored_call = await withSponsor(await call("increment", { sponsored: true }));
txs.duplicate_deploy = await makeContractDeploy({
  ...options,
  nonce: 1n,
  contractName: "raw",
  codeBody: "invalid syntax (",
  clarityVersion: 3,
});
txs.abort_deploy = await makeContractDeploy({
  ...options,
  contractName: "aborted",
  codeBody: `(define-data-var count uint u42) (stx-transfer? u100 tx-sender '${sponsor})`,
  clarityVersion: 3,
  ...abortConditions,
});
txs.sponsor_context = await withSponsor(await call("payer", { sponsored: true }));
txs.sponsored_abort = await withSponsor(
  await call("send", { sponsored: true, ...abortConditions }),
);
await transfer("bad_sponsor_nonce", { sponsored: true });
txs.bad_sponsor_nonce = await withSponsor(txs.bad_sponsor_nonce, { nonce: 1n });
const fixtures = {
  sender,
  sponsor,
  source,
  transactions: Object.fromEntries(
    Object.entries(txs).map(([name, tx]) => [name, { hex: tx.serialize(), txid: tx.txid() }]),
  ),
};
writeFileSync(
  new URL("transactions.json", import.meta.url),
  JSON.stringify(fixtures, null, 2) + "\n",
);
