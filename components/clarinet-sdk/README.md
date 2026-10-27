# Clarinet SDK Workspace

This workspace regroups
`@stacks/clarinet-sdk` for node.js and `@stacks/clarinet-sdk-browser` for web browsers.  
They respectively rely on `@stacks/clarinet-sdk-wasm` and `@stacks/clarinet-sdk-wasm-browser`.

The Wasm packages are built separately for Node.js and browsers. Both SDKs share
`./common/src/sdkProxy.ts`, with small adapters in `./node/src/sdkProxy.ts` and
`./browser/src/sdkProxy.ts`.

## Transaction behavior

Simnet public calls, deployments, and native STX transfers use the shared
stacks-core Clarity transaction frame. Included execution failures and
post-condition aborts roll back payload writes but consume the sender's nonce;
rejected transactions consume no nonce. Existing convenience calls do not charge
transaction fees.

Deployments use stacks-node's initializer. A successful `deployContract` returns
`Cl.ok(Cl.bool(true))`, regardless of the initializer's final expression, and
stores the contract even if it defines no functions. Duplicate contract names
are rejected before analysis without consuming a nonce. Use read-only calls,
`getDataVar`, and events to check initialization effects. Use `execute` when you
want to evaluate a snippet and receive its expression value. Deployment costs
include contract storage and subsequent calls account for the stored contract's
actual data size, so cost assertions may need updating.

`transferSTX` represents a native transfer: self-transfers, zero-amount transfers,
and transfers exceeding the available unlocked balance throw without consuming a
nonce. Inside `mineBlock`, these transfers return a `rejected` outcome without
consuming a nonce. Calling the Clarity `stx-transfer?` function inside a contract
retains its ordinary response semantics. Private calls remain a simnet convenience
and use the same transaction frame.

### Migrating `mineBlock` to 4.0

`mineBlock(txs)` returns one `BlockTransactionResult` per entry, in input order:

- `success`: `execution` contains the parsed result, events, costs, and performance.
- `included_failure`: `error` describes the failure. For a returned `(err ...)`,
  `execution` contains its parsed result, events, costs, and performance. For VM,
  analysis, or post-condition errors, `execution` is `null`. Included failures
  consume a nonce according to the VM's epoch rules; their payload writes roll back.
- `rejected`: `error` describes why the transaction was rejected. No payload writes
  or nonce changes commit.

Later entries execute after either kind of failure. Every call advances exactly
one block before execution, even for empty or entirely rejected batches. All
entries execute at that inclusion height. Convenience entries remain unsigned
and fee-free.

Previously, entries returned the execution object directly, most failures threw
and stopped the batch, and invalid native transfers returned `(err u1/u2/u3)`
and consumed a nonce. Replace direct access such as `results[0].result` with a
status check and `results[0].execution.result`:

```ts
for (const outcome of simnet.mineBlock(transactions)) {
  if (outcome.status === "success") {
    console.log(outcome.execution.result);
  } else {
    console.log(outcome.status, outcome.error);
  }
}
```

The synchronous debug adapter uses the same batch outcome contract, transaction
nonce rules, and inclusion height.

Contract-call transactions may target `define-public` or `define-read-only`
functions. A read-only target still consumes a transaction nonce when invoked
through `callPublicFn` or `mineBlock`; use `callReadOnlyFn` for a nonce-free query.
These execution paths do not apply the node's mempool admission policy, which
rejects direct transactions targeting read-only functions.

## Signed transactions

`simnet.broadcastTransaction(rawHex)` submits a complete signed Stacks transaction
serialized by Stacks.js to a session-local mempool. It returns `{ txid }` on
acceptance or `{ error, reason, reason_data, txid? }` on rejection. Malformed
input has no TXID. Submission is synchronous and never executes the payload,
charges fees, consumes nonces, or advances the chain.

Admission checks signatures, network/epoch rules, origin and sponsor nonces,
unlocked funds, transfer validity, duplicate deployments, and public-function
access and argument types (including Clarity trait casts). Read-only, private,
and missing functions are rejected. The minimum fee matches the pinned node:
1 micro-STX per serialized byte. Supply explicit fees and nonces so Stacks.js
does not fetch them from a remote node.

```ts
const transaction = await makeSTXTokenTransfer({
  network: "testnet",
  senderKey,
  recipient,
  amount: 100n,
  fee: 1000n,
  nonce: simnet.getAccountNonce(sender),
});
const admission = simnet.broadcastTransaction(transaction.serialize());
if ("error" in admission) {
  throw new Error(admission.reason);
}
const [outcome] = simnet.mineBlock();
if (outcome.status === "success") {
  console.log(outcome.transaction?.txid, outcome.execution.result);
}
```

`mineBlock()` selects queued transactions with ready nonces in submission order, followed by any
explicit entries supplied to `mineBlock(txs)`, advancing one block for the whole
batch. Its results use the same `success`, `included_failure`, and `rejected`
statuses as convenience entries. Signed results additionally carry `transaction`
metadata: `txid` (64 lowercase hex characters without `0x`), `fee`, and `nonce`
(both `bigint`). Signed included failures carry `kind` (`abort_by_response`,
`abort_by_post_condition`, `runtime_error`, or `analysis_error`), nullable `result`,
attempted `events`, and `costs`; response failures can also include `performance`.
They retain fees and nonce charges while rolling back aborted payload writes.

Admission does not guarantee inclusion: mining checks current state again.
Rejected mined entries charge nothing, and later entries still run. Each selected
entry is attempted once and removed, including rejected entries. Future-nonce
transactions remain queued until their predecessors can be mined. Repeated
submission of identical pending bytes returns the same TXID without duplicating
the queue. Replaying a mined transaction fails nonce validation. Future
nonces can be queued for an origin or sponsor up to the node's chaining bound
(mined nonce + 26); `getAccountNonce` continues to
report the mined nonce. Calls to a pending deployment must wait until it is mined.

For direct block execution without mempool policy, use
`mineBlock([{ broadcastTx: rawHex }])`. This permits block-valid read-only calls
and low-fee transactions. Convenience calls remain unsigned and fee-free.
Empty-block helpers advance the chain without draining the mempool.

This is a deterministic local queue, with no network propagation, fee bidding,
replacement or expiry. Coinbase, tenure-change, and
poison-microblock payloads remain unsupported. Raw payloads, post-conditions,
contract source, and memo bytes execute exactly as signed.

Migration: broadcast previously mined immediately and returned an execution
result. Check the admission response, then call `mineBlock()` for execution.

## Contributing

The clarinet-sdk requires a few steps to be built and tested locally.

Clone the clarinet repo and `cd` into it:

```sh
git clone git@github.com:stx-labs/clarinet.git
cd clarinet
```

Open the SDK workspace in VSCode, it's especially useful to get rust-analyzer
to consider the right files with the right cargo features.

```sh
code components/clarinet-sdk/clarinet-sdk.code-workspace
```

The SDK mainly relies on two components:

- the Rust component: `components/clarinet-sdk-wasm`
- the TS component: `components/clarinet-sdk`

To work with these two packages locally, the first one needs to be built with
wasm-pack (install [wasm-pack](https://wasm-bindgen.github.io/wasm-pack/installer)).

```sh
# install dependencies without running prepare scripts
# (the SDK packages' `prepare` hook depends on the Wasm build,
# which doesn't exist yet on a fresh clone)
pnpm install --ignore-scripts
# build the Wasm package
pnpm run build:sdk-wasm
# install dependencies and build the node package
pnpm install
# make sure the installation works
pnpm test
```

### Release

The Node.js and browser versions can be published with this single command.
Check both package versions first.

```sh
# the Wasm package must be published first
# $ pnpm run publish:sdk-wasm
pnpm run publish:sdk
```
