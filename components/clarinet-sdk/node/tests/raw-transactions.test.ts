import {
  Cl,
  deserializeTransaction,
  makeContractCall,
  makeContractDeploy,
  sponsorTransaction,
} from "@stacks/transactions";
import { beforeEach, expect, it, vi } from "vitest";
import { initSimnet, type Simnet, type RawTransactionResult } from "..";
import fixtures from "./fixtures/raw-transactions/transactions.json";

let simnet: Simnet;
const { sender, sponsor, transactions } = fixtures;
const balance = (address: string) => simnet.getAssetsMap().get("STX")!.get(address)!;
// Direct miner entries exercise execution independently of mempool policy.
const executeRaw = (hex: string): RawTransactionResult => {
  const outcome = simnet.mineBlock([{ broadcastTx: hex }])[0];
  if (outcome.status === "success") return { ...outcome, transaction: outcome.transaction! };
  if (outcome.status === "rejected")
    return { ...outcome, transaction: outcome.transaction ?? null };
  return {
    status: outcome.kind,
    transaction: outcome.transaction!,
    error: outcome.error,
    ...outcome.execution,
  };
};
const broadcast = (name: keyof typeof transactions) => executeRaw(transactions[name].hex);
beforeEach(async () => {
  simnet = await initSimnet("tests/fixtures/Clarinet.toml");
  simnet.setEpoch("3.4");
  simnet.mintSTX(sender, 1000000n);
  simnet.mintSTX(sponsor, 1000000n);
});

it("executes signed bytes, preserves the memo and canonical ID, and rejects replay", () => {
  const height = simnet.blockHeight;
  const result = broadcast("transfer");
  expect(result.status).toBe("success");
  expect(result.transaction).toEqual({ txid: transactions.transfer.txid, fee: 10n, nonce: 0n });
  expect(result.transaction!.txid).toBe(deserializeTransaction(transactions.transfer.hex).txid());
  if (result.status !== "success") throw new Error(result.status);
  expect(result.execution.result).toEqual(Cl.ok(Cl.bool(true)));
  expect(result.execution.performance).toBeUndefined();
  expect(result.execution.events[0].data.memo).toBe("726177206d656d6f".padEnd(68, "0"));
  expect(balance(sender)).toBe(999890n);
  expect(balance(sponsor)).toBe(1000100n);
  expect(simnet.getAccountNonce(sender)).toBe(1n);
  expect(simnet.blockHeight).toBe(height + 1);
  expect(broadcast("transfer").status).toBe("rejected");
  expect(balance(sender)).toBe(999890n);
  expect(simnet.blockHeight).toBe(height + 2);
});

it.each([false, true])(
  "updates raw contract call traces, including failures (%s)",
  async (fail) => {
    simnet.callPublicFn("stacks-trace-test", "call-sub", [Cl.uint(9), Cl.uint(1)], sender);
    expect(simnet.getLastContractCallTrace()).toContain("args: u9 u1");
    const transaction = await makeContractCall({
      network: "testnet",
      senderKey: "1".padStart(64, "0") + "01",
      contractAddress: simnet.deployer,
      contractName: "stacks-trace-test",
      functionName: "call-sub",
      functionArgs: [Cl.uint(fail ? 3 : 4), Cl.uint(fail ? 4 : 3)],
      nonce: simnet.getAccountNonce(sender),
      fee: 10n,
      postConditionMode: "allow",
    });
    const consoleSpy = vi.spyOn(console, "error").mockImplementation(() => {});
    try {
      const result = executeRaw(transaction.serialize());
      expect(result.status).toBe(fail ? "runtime_error" : "success");
      const trace = simnet.getLastContractCallTrace();
      expect(trace).toContain(fail ? "args: u3 u4" : "args: u4 u3");
      expect(trace).not.toContain("args: u9 u1");
      if (fail) {
        expect(consoleSpy).toHaveBeenCalledWith(trace);
        expect(consoleSpy).toHaveBeenCalledWith(expect.stringContaining("ArithmeticUnderflow"));
      } else {
        expect(consoleSpy).not.toHaveBeenCalled();
      }
      // Undecodable input isn't a contract call and preserves the last call's trace.
      consoleSpy.mockClear();
      expect(executeRaw("xyz").status).toBe("rejected");
      expect(simnet.getLastContractCallTrace()).toBe(trace);
      // A decoded call rejected at admission replaces the old execution trace.
      expect(executeRaw(transaction.serialize()).status).toBe("rejected");
      expect(simnet.getLastContractCallTrace()).not.toBe(trace);
      expect(simnet.getLastContractCallTrace()).not.toContain("args:");
      expect(consoleSpy).not.toHaveBeenCalled();
    } finally {
      consoleSpy.mockRestore();
    }
  },
);

it.each([
  [false, false],
  [false, true],
  [true, false],
  [true, true],
])("traces nested events once and in order (raw=%s, abort=%s)", async (raw, abort) => {
  simnet.deployContract(
    "trace-child",
    `(define-public (emit (n uint))
       (begin (print n) (print (+ n u1)) (ok true)))`,
    null,
    simnet.deployer,
  );
  simnet.deployContract(
    "trace-parent",
    `(define-public (run (abort bool))
       (begin
         (print u1)
         (unwrap-panic (contract-call? .trace-child emit u2))
         (print u4)
         (unwrap-panic (contract-call? .trace-child emit u5))
         (print u7)
         (if abort (err u8) (ok true))))`,
    null,
    simnet.deployer,
  );
  if (raw) {
    const tx = await makeContractCall({
      network: "testnet",
      senderKey: "1".padStart(64, "0") + "01",
      contractAddress: simnet.deployer,
      contractName: "trace-parent",
      functionName: "run",
      functionArgs: [Cl.bool(abort)],
      nonce: simnet.getAccountNonce(sender),
      fee: 10n,
      postConditionMode: "allow",
    });
    expect(executeRaw(tx.serialize()).status).toBe(abort ? "abort_by_response" : "success");
  } else {
    expect(simnet.callPublicFn("trace-parent", "run", [Cl.bool(abort)], sender).result).toEqual(
      abort ? Cl.error(Cl.uint(8)) : Cl.ok(Cl.bool(true)),
    );
  }
  const events = [...simnet.getLastContractCallTrace().matchAll(/print event: u(\d+)/g)].map(
    ([, value]) => Number(value),
  );
  expect(events).toEqual([1, 2, 3, 4, 5, 6, 7]);
});

it.each([true, false])(
  "returns raw call performance only when tracking is enabled (%s)",
  async (trackPerformance) => {
    simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
      trackCosts: false,
      trackCoverage: false,
      trackPerformance,
    });
    simnet.enablePerformance("runtime");
    simnet.setEpoch("3.4");
    simnet.mintSTX(sender, 2000000n);
    const transaction = await makeContractCall({
      network: "testnet",
      senderKey: "1".padStart(64, "0") + "01",
      contractAddress: simnet.deployer,
      contractName: "counter",
      functionName: "increment",
      functionArgs: [],
      nonce: simnet.getAccountNonce(sender),
      fee: 10n,
      postConditionMode: "allow",
    });
    const result = executeRaw(transaction.serialize());
    expect(result.status).toBe("success");
    if (result.status !== "success") throw new Error(result.status);
    if (trackPerformance) {
      expect(result.execution.performance).toContain(`${simnet.deployer}.counter`);
    } else {
      expect(result.execution.performance).toBeUndefined();
    }
    const replay = executeRaw(transaction.serialize());
    expect(replay.status).toBe("rejected");
    expect(replay).not.toHaveProperty("performance");
    expect(replay).not.toHaveProperty("execution");
  },
);

it.each([true, false])(
  "reports fresh performance for raw err responses when tracking is enabled (%s)",
  async (trackPerformance) => {
    simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
      trackCosts: true,
      trackPerformance,
    });
    simnet.enablePerformance("runtime");
    simnet.setEpoch("3.4");
    simnet.mintSTX(sender, 1000000n);
    simnet.deployContract(
      "perf-errors",
      `(define-public (fail) (err u42))
       (define-public (crash) (ok (/ u1 u0)))`,
      null,
      simnet.deployer,
    );
    const previous = simnet.callPublicFn(
      "stacks-trace-test",
      "call-sub",
      [Cl.uint(9), Cl.uint(1)],
      sender,
    );
    const call = async (functionName: string) => {
      const tx = await makeContractCall({
        network: "testnet",
        senderKey: "1".padStart(64, "0") + "01",
        contractAddress: simnet.deployer,
        contractName: "perf-errors",
        functionName,
        functionArgs: [],
        nonce: simnet.getAccountNonce(sender),
        fee: 10n,
        postConditionMode: "allow",
      });
      return executeRaw(tx.serialize());
    };
    const result = await call("fail");
    expect(result.status).toBe("abort_by_response");
    if (result.status !== "abort_by_response") throw new Error(result.status);
    expect(result.result).toEqual(Cl.error(Cl.uint(42)));
    if (trackPerformance) {
      expect(previous.performance).toContain("stacks-trace-test");
      expect(result.performance).toContain(`${simnet.deployer}.perf-errors`);
      expect(result.performance).not.toBe(previous.performance);
    } else {
      expect(result.performance).toBeUndefined();
    }
    const consoleSpy = vi.spyOn(console, "error").mockImplementation(() => {});
    try {
      const failure = await call("crash");
      expect(failure.status).toBe("runtime_error");
      expect(failure).toHaveProperty("performance", undefined);
    } finally {
      consoleSpy.mockRestore();
    }
  },
);

it.each([true, false])(
  "reports raw call costs only when tracking is enabled (%s)",
  async (trackCosts) => {
    simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
      trackCosts,
      trackCoverage: false,
    });
    simnet.setEpoch("3.4");
    simnet.mintSTX(sender, 1000000n);
    simnet.setCurrentTestName("signed transfer call");
    const contractAddress = simnet.deployer;
    const transaction = await makeContractCall({
      network: "testnet",
      senderKey: "1".padStart(64, "0") + "01",
      contractAddress,
      contractName: "counter",
      functionName: "transfer-100",
      functionArgs: [Cl.principal(sponsor)],
      nonce: simnet.getAccountNonce(sender),
      fee: 10n,
      postConditionMode: "allow",
    });
    const result = executeRaw(transaction.serialize());
    expect(result.status).toBe("success");
    if (result.status !== "success") throw new Error(result.status);
    // A rejected replay must not add another report.
    expect(executeRaw(transaction.serialize()).status).toBe("rejected");
    const reports = JSON.parse(simnet.collectReport(false, "").costs);
    expect(reports).toHaveLength(trackCosts ? 1 : 0);
    if (trackCosts) {
      expect(reports[0]).toMatchObject({
        test_name: "signed transfer call",
        contract_id: `${contractAddress}.counter`,
        method: "transfer-100",
        args: [sponsor],
        cost_result: { total: { runtime: result.execution.costs!.total.runtime } },
      });
      expect(reports[0].cost_result.total.runtime).toBeGreaterThan(0);
    }
    expect(JSON.parse(simnet.collectReport(false, "").costs)).toEqual([]);
  },
);

it("formats raw and convenience cost-report arguments identically", async () => {
  simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
    trackCosts: true,
    trackCoverage: false,
  });
  simnet.setEpoch("3.4");
  simnet.mintSTX(sender, 1000000n);
  simnet.deployContract(
    "report-args",
    "(define-public (echo (who principal) (data { a: uint, who: principal })) (ok data))",
    null,
    simnet.deployer,
  );
  const args = [Cl.principal(sponsor), Cl.tuple({ a: Cl.uint(1), who: Cl.principal(sender) })];
  simnet.callPublicFn("report-args", "echo", args, sender);
  const transaction = await makeContractCall({
    network: "testnet",
    senderKey: "1".padStart(64, "0") + "01",
    contractAddress: simnet.deployer,
    contractName: "report-args",
    functionName: "echo",
    functionArgs: args,
    nonce: simnet.getAccountNonce(sender),
    fee: 10n,
    postConditionMode: "allow",
  });
  expect(executeRaw(transaction.serialize()).status).toBe("success");
  const reports = JSON.parse(simnet.collectReport(false, "").costs);
  expect(reports).toHaveLength(2);
  expect(reports[0].args).toEqual([sponsor, `(tuple (a u1) (who ${sender}))`]);
  expect(reports[1].args).toEqual(reports[0].args);
});

it("reports returned err responses like convenience calls, excluding other payloads", async () => {
  simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
    trackCosts: true,
    trackCoverage: false,
  });
  simnet.setEpoch("3.4");
  simnet.mintSTX(sender, 1000000n);
  broadcast("transfer");
  expect(JSON.parse(simnet.collectReport(false, "").costs)).toEqual([]);
  // Use a fresh session so the fixed deployment fixture has the expected nonce.
  simnet = await initSimnet("tests/fixtures/Clarinet.toml", true, {
    trackCosts: true,
    trackCoverage: false,
  });
  simnet.setEpoch("3.4");
  simnet.mintSTX(sender, 1000000n);
  simnet.setCurrentTestName("signed err response");
  expect(broadcast("deploy").status).toBe("success");
  expect(broadcast("fail").status).toBe("abort_by_response");
  const reports = JSON.parse(simnet.collectReport(false, "").costs);
  expect(reports).toHaveLength(1);
  expect(reports[0]).toMatchObject({
    test_name: "signed err response",
    contract_id: `${sender}.raw`,
    method: "fail",
    args: [],
  });
});

it.each([
  "wrong_nonce",
  "wrong_network",
  "zero",
  "self",
  "insufficient",
  "insufficient_fee",
] as const)("rejects %s without any settlement", (name) => {
  const height = simnet.blockHeight;
  expect(broadcast(name).status).toBe("rejected");
  expect(balance(sender)).toBe(1000000n);
  expect(balance(sponsor)).toBe(1000000n);
  expect(simnet.getAccountNonce(sender)).toBe(0n);
  expect(simnet.blockHeight).toBe(height + 1);
});

it.each(["xyz", "00", transactions.transfer.hex + "00"])("rejects malformed input", (hex) => {
  expect(executeRaw(hex)).toMatchObject({ status: "rejected", transaction: null });
  expect(simnet.getAccountNonce(sender)).toBe(0n);
});

it("checks signatures on Wasm", () => {
  const tx = deserializeTransaction(transactions.transfer.hex);
  tx.setFee(11n);
  expect(executeRaw(tx.serialize()).status).toBe("rejected");
  expect(balance(sender)).toBe(1000000n);
});

it.each(["sponsored", "same_sponsor"] as const)("settles %s exactly once per principal", (name) => {
  expect(broadcast(name).status).toBe("success");
  expect(simnet.getAccountNonce(sender)).toBe(1n);
  expect(simnet.getAccountNonce(sponsor)).toBe(name === "sponsored" ? 1n : 0n);
  expect(balance(sender)).toBe(name === "sponsored" ? 999900n : 999890n);
  expect(balance(sponsor)).toBe(name === "sponsored" ? 1000090n : 1000100n);
});

it("deploys and calls through the shared frame and updates contract caches", () => {
  expect(broadcast("deploy").status).toBe("success");
  expect(simnet.getContractSource(`${sender}.raw`)).toBe(fixtures.source);
  expect(simnet.getContractsInterfaces().has(`${sender}.raw`)).toBe(true);
  expect(broadcast("increment").status).toBe("success");
  expect(simnet.getDataVar(`${sender}.raw`, "count")).toEqual(Cl.uint(1));
  expect(balance(sender)).toBe(999980n);
});

it.each([
  ["fail", "abort_by_response"],
  ["crash", "runtime_error"],
  ["abort", "abort_by_post_condition"],
] as const)("keeps fees and nonces but rolls back %s", (name, status) => {
  broadcast("deploy");
  const result = broadcast(name);
  expect(result.status).toBe(status);
  expect(result.transaction).toEqual({ txid: transactions[name].txid, fee: 10n, nonce: 1n });
  expect(simnet.getDataVar(`${sender}.raw`, "count")).toEqual(Cl.uint(0));
  expect(balance(sender)).toBe(999980n);
  expect(balance(sponsor)).toBe(1000000n);
  expect(simnet.getAccountNonce(sender)).toBe(2n);
  if (name === "abort" && result.status === "abort_by_post_condition")
    expect(result.events).toHaveLength(1);
  // Like a node's receipt: `(err none)` when execution produced no value.
  if (name === "crash") expect(result).toMatchObject({ result: Cl.error(Cl.none()), events: [] });
});

it("charges included analysis failures without caching a contract", () => {
  expect(broadcast("bad_deploy")).toMatchObject({
    status: "analysis_error",
    result: Cl.error(Cl.none()),
    events: [],
  });
  expect(balance(sender)).toBe(999990n);
  expect(simnet.getAccountNonce(sender)).toBe(1n);
  expect(simnet.getContractsInterfaces().has(`${sender}.bad`)).toBe(false);
});

it("settles a sponsored call", () => {
  broadcast("deploy");
  expect(broadcast("sponsored_call").status).toBe("success");
  expect(balance(sender)).toBe(999990n);
  expect(balance(sponsor)).toBe(999990n);
  expect(simnet.getAccountNonce(sender)).toBe(2n);
  expect(simnet.getAccountNonce(sponsor)).toBe(1n);
});

it("passes tx-sponsor? to the VM", () => {
  broadcast("deploy");
  const result = broadcast("sponsor_context");
  expect(result.status).toBe("success");
  if (result.status === "success")
    expect(result.execution.result).toEqual(Cl.ok(Cl.some(Cl.principal(sponsor))));
});

it("rejects a sponsor nonce mismatch without touching either account", () => {
  expect(broadcast("bad_sponsor_nonce").status).toBe("rejected");
  expect(balance(sender)).toBe(1000000n);
  expect(balance(sponsor)).toBe(1000000n);
  expect(simnet.getAccountNonce(sender)).toBe(0n);
  expect(simnet.getAccountNonce(sponsor)).toBe(0n);
});

it("retains sponsor fees and both nonces after a post-condition abort", () => {
  broadcast("deploy");
  expect(broadcast("sponsored_abort").status).toBe("abort_by_post_condition");
  expect(balance(sender)).toBe(999990n);
  expect(balance(sponsor)).toBe(999990n);
  expect(simnet.getAccountNonce(sender)).toBe(2n);
  expect(simnet.getAccountNonce(sponsor)).toBe(1n);
});

it("rejects duplicate contracts before parsing and rolls back the fee", () => {
  broadcast("deploy");
  const result = broadcast("duplicate_deploy");
  expect(result.status).toBe("rejected");
  if (result.status === "rejected") expect(result.error).toContain("Duplicate contract");
  expect(balance(sender)).toBe(999990n);
  expect(simnet.getAccountNonce(sender)).toBe(1n);
  expect(simnet.getContractSource(`${sender}.raw`)).toBe(fixtures.source);
});

it("does not cache a deployment aborted by its post-conditions", () => {
  const result = broadcast("abort_deploy");
  expect(result).toMatchObject({ status: "abort_by_post_condition", result: Cl.ok(Cl.bool(true)) });
  expect(simnet.getContractsInterfaces().has(`${sender}.aborted`)).toBe(false);
  expect(balance(sender)).toBe(999990n);
  expect(balance(sponsor)).toBe(1000000n);
  expect(simnet.getAccountNonce(sender)).toBe(1n);
});

it("rejects a deployment version that the epoch does not allow", () => {
  simnet.setEpoch("2.5");
  expect(broadcast("deploy").status).toBe("rejected");
  expect(balance(sender)).toBe(1000000n);
  expect(simnet.getAccountNonce(sender)).toBe(0n);
});

it.each([
  ["2.05", 1, "block-height"],
  ["3.4", 5, "stacks-block-height"],
  ["4.0", 6, "stacks-block-height"],
] as const)(
  "executes in the inclusion block and advances rejected blocks in epoch %s",
  async (epoch, clarityVersion, heightVariable) => {
    simnet.setEpoch(epoch);
    const options = {
      network: "testnet" as const,
      senderKey: "1".padStart(64, "0") + "01",
      fee: 10n,
      postConditionMode: "allow" as const,
    };
    const height = simnet.blockHeight;
    const deployment = await makeContractDeploy({
      ...options,
      nonce: simnet.getAccountNonce(sender),
      contractName: "heights",
      clarityVersion,
      codeBody: `(define-data-var deployed-at uint ${heightVariable})
      (define-read-only (observe-height) (ok ${heightVariable}))
      (define-public (height) (ok ${heightVariable}))
      (define-public (fail) (err ${heightVariable}))`,
    });
    expect(executeRaw(deployment.serialize()).status).toBe("success");
    expect(simnet.getDataVar(`${sender}.heights`, "deployed-at")).toEqual(Cl.uint(height + 1));
    expect(simnet.blockHeight).toBe(height + 1);

    const call = async (functionName: string) =>
      makeContractCall({
        ...options,
        nonce: simnet.getAccountNonce(sender),
        contractAddress: sender,
        contractName: "heights",
        functionName,
        functionArgs: [],
      });
    const signed = await call("height");
    const result = executeRaw(signed.serialize());
    expect(result.status).toBe("success");
    if (result.status !== "success") throw new Error(result.status);
    expect(result.execution.result).toEqual(Cl.ok(Cl.uint(height + 2)));
    expect(simnet.blockHeight).toBe(height + 2);

    const failure = executeRaw((await call("fail")).serialize());
    expect(failure).toMatchObject({
      status: "abort_by_response",
      result: Cl.error(Cl.uint(height + 3)),
    });
    expect(simnet.blockHeight).toBe(height + 3);

    const burnHeight = simnet.burnBlockHeight;
    const nonce = simnet.getAccountNonce(sender);
    const funds = balance(sender);
    const duplicate = await makeContractDeploy({
      ...options,
      nonce,
      contractName: "heights",
      clarityVersion,
      codeBody: "(define-constant duplicate true)",
    });
    let rejectedCount = 0;
    for (const hex of ["xyz", signed.serialize(), duplicate.serialize()]) {
      rejectedCount++;
      expect(executeRaw(hex).status).toBe("rejected");
      expect(simnet.blockHeight).toBe(height + 3 + rejectedCount);
      expect(simnet.burnBlockHeight).toBeGreaterThanOrEqual(burnHeight);
      expect(simnet.getAccountNonce(sender)).toBe(nonce);
      expect(balance(sender)).toBe(funds);
      // Read through the VM to confirm the inclusion height.
      expect(
        simnet.callReadOnlyFn(`${sender}.heights`, "observe-height", [], sender).result,
      ).toEqual(Cl.ok(Cl.uint(height + 3 + rejectedCount)));
    }
    expect(simnet.callPublicFn(`${sender}.heights`, "height", [], sender).result).toEqual(
      Cl.ok(Cl.uint(height + 4 + rejectedCount)),
    );
  },
);

it.each(["2.05", "3.4", "4.0"] as const)(
  "checks mempool function access and arguments without changing block rules in epoch %s",
  async (epoch) => {
    simnet.setEpoch(epoch);
    simnet.deployContract(
      "admission",
      `(define-read-only (read) (ok u1))
       (define-private (private) (ok u2))
       (define-public (write (n uint)) (ok n))`,
      null,
      sender,
    );
    const nonce = simnet.getAccountNonce(sender);
    const funds = balance(sender);
    const sponsorFunds = balance(sponsor);
    const sponsorNonce = simnet.getAccountNonce(sponsor);
    const height = simnet.blockHeight;
    const burnHeight = simnet.burnBlockHeight;
    for (const [contractName, functionName, functionArgs, error] of [
      ["admission", "read", [], "NoSuchPublicFunction"],
      ["admission", "private", [], "NoSuchPublicFunction"],
      ["admission", "missing", [], "NoSuchPublicFunction"],
      ["missing", "write", [], "NoSuchContract"],
      ["admission", "write", [], "BadFunctionArgument"],
      ["admission", "write", [Cl.bool(true)], "BadFunctionArgument"],
      ["admission", "write", [Cl.uint(1), Cl.uint(2)], "BadFunctionArgument"],
    ] as const) {
      const unsignedSponsor = await makeContractCall({
        network: "testnet",
        senderKey: "1".padStart(64, "0") + "01",
        contractAddress: sender,
        contractName,
        functionName,
        functionArgs: [...functionArgs],
        nonce,
        fee: 1000n,
        sponsored: true,
        postConditionMode: "allow",
      });
      const transaction = await sponsorTransaction({
        transaction: unsignedSponsor,
        sponsorPrivateKey: "2".padStart(64, "0") + "01",
        sponsorNonce,
        fee: 1000n,
      });
      expect(simnet.broadcastTransaction(transaction.serialize())).toMatchObject({
        reason: error,
        txid: transaction.txid(),
      });
      expect(simnet.getAccountNonce(sender)).toBe(nonce);
      expect(simnet.getAccountNonce(sponsor)).toBe(sponsorNonce);
      expect(balance(sender)).toBe(funds);
      expect(balance(sponsor)).toBe(sponsorFunds);
      expect(simnet.blockHeight).toBe(height);
      expect(simnet.burnBlockHeight).toBe(burnHeight);
    }
    const mined = simnet.mineBlock([
      { callPublicFn: { contract: `${sender}.admission`, method: "read", args: [], sender } },
    ]);
    expect(mined[0].status).toBe("success");
    expect(simnet.getAccountNonce(sender)).toBe(nonce + 1n);
    expect(simnet.blockHeight).toBe(height + 1);
  },
);
