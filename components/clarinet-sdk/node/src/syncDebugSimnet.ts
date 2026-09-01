/**
 * Synchronous simnet proxy that routes all calls to a `clarinet dap` debug server
 * via a SharedArrayBuffer + Atomics.wait worker (see syncDebugSocket.ts).
 *
 * When CLARINET_DEBUG_PORT is set, initSimnet() returns one of these instead of
 * the WASM simnet. The API surface is identical, so existing tests run unchanged
 * while breakpoints in .clar files fire in VSCode (or any DAP-capable editor).
 */
import { Cl, type ClarityValue } from "@stacks/transactions";
import { syncSend } from "./syncDebugSocket.js";
import {
  parseEvents,
  parseCosts,
  type ClarityEvent,
  type ClarityCosts,
  type ParsedTransactionResult,
  type Tx,
} from "../../common/src/sdkProxyHelpers.js";

// Response shape from the debug server for transaction results.
type ServerTxResult = {
  result: string; // 0x-prefixed hex-encoded Clarity value
  events: string; // JSON array string (same format as WASM)
  costs: string;  // JSON string or "null"
};

function parseTxResult(r: ServerTxResult): ParsedTransactionResult {
  if (!r.result) {
    throw new Error((r as unknown as { error?: string }).error ?? "transaction failed with no result");
  }
  return {
    result: Cl.deserialize(r.result),
    events: parseEvents(r.events),
    costs: parseCosts(r.costs),
    performance: undefined,
  };
}

/** Call syncSend and throw if the server responded with an error. */
function send(request: Record<string, unknown>): Record<string, unknown> {
  const raw = syncSend(request);
  const resp = JSON.parse(raw) as { result?: Record<string, unknown>; error?: string };
  if (resp.error) throw new Error(resp.error);
  return resp.result ?? {};
}

/** Serialize a ClarityValue to a Clarity syntax string for use in snippets. */
function argString(v: ClarityValue): string {
  return Cl.stringify(v);
}

export type SyncDebugSimnet = {
  readonly deployer: string;
  readonly blockHeight: number;
  readonly burnBlockHeight: number;
  readonly stacksBlockHeight: number;
  getAccounts(): Map<string, string>;
  mineBlock(txs: Tx[]): ParsedTransactionResult[];
  mineEmptyBlock(count?: number): number;
  mineEmptyStacksBlock(): number;
  callPublicFn(
    contract: string,
    method: string,
    args: ClarityValue[],
    sender: string,
  ): ParsedTransactionResult;
  callReadOnlyFn(
    contract: string,
    method: string,
    args: ClarityValue[],
    sender: string,
  ): ParsedTransactionResult;
  callPrivateFn(
    contract: string,
    method: string,
    args: ClarityValue[],
    sender: string,
  ): ParsedTransactionResult;
  execute(snippet: string): ParsedTransactionResult;
  runSnippet(snippet: string): string;
  getAssetsMap(): Map<string, Map<string, bigint>>;
  getLastContractCallTrace(): string | undefined;
  setCurrentTestName(name: string): void;
  collectReport(
    includeBootContracts: boolean,
    bootContractsPath: string,
  ): { coverage: string; costs: string };
  // initSession is async to match the Simnet interface called in vitest.setup.ts
  initSession(cwd: string, manifestPath: string): Promise<void>;
};

/**
 * Create a synchronous debug simnet proxy connected to the debug server at the
 * given port. The caller must have already called `connectSyncSocket(port)`.
 *
 * `deployer` is fetched immediately (synchronously) from the server.
 */
export function createSyncDebugSimnet(): SyncDebugSimnet {
  // Fetch deployer and accounts at construction time (sync, server is connected).
  const initData = send({ method: "getAccounts" }) as {
    accounts: Record<string, string>;
  };
  const accountsRecord = initData.accounts ?? {};
  const deployer = accountsRecord["deployer"] ?? "";

  return {
    deployer,

    get blockHeight(): number {
      const resp = send({ method: "getBlockHeight" }) as { blockHeight: number };
      return resp.blockHeight;
    },

    get burnBlockHeight(): number {
      const resp = send({ method: "getBurnBlockHeight" }) as { burnBlockHeight: number };
      return resp.burnBlockHeight;
    },

    get stacksBlockHeight(): number {
      const resp = send({ method: "getBlockHeight" }) as { blockHeight: number };
      return resp.blockHeight;
    },

    getAccounts(): Map<string, string> {
      return new Map(Object.entries(accountsRecord));
    },

    mineEmptyBlock(count = 1): number {
      const resp = send({ method: "mineEmptyBlock", count }) as { blockHeight: number };
      return resp.blockHeight;
    },

    mineEmptyStacksBlock(): number {
      const resp = send({ method: "mineEmptyBlock", count: 1 }) as { blockHeight: number };
      return resp.blockHeight;
    },

    setCurrentTestName(_name: string): void {
      // No-op in debug mode: coverage tracking is not supported.
    },

    collectReport(
      _includeBootContracts: boolean,
      _bootContractsPath: string,
    ): { coverage: string; costs: string } {
      // Debug mode does not support coverage/cost reports.
      throw new Error("collectReport: coverage and cost reports are not supported in debug mode");
    },

    runSnippet(snippet: string): string {
      const resp = send({ method: "execute", snippet }) as ServerTxResult;
      return Cl.stringify(Cl.deserialize(resp.result));
    },

    mineBlock(txs: Tx[]): ParsedTransactionResult[] {
      const serialized = txs.map((tx) => {
        if (tx.callPublicFn) {
          return {
            type: "callPublicFn",
            contract: tx.callPublicFn.contract,
            function: tx.callPublicFn.method,
            args: tx.callPublicFn.args.map(argString),
            sender: tx.callPublicFn.sender,
          };
        }
        if (tx.callPrivateFn) {
          return {
            type: "callPrivateFn",
            contract: tx.callPrivateFn.contract,
            function: tx.callPrivateFn.method,
            args: tx.callPrivateFn.args.map(argString),
            sender: tx.callPrivateFn.sender,
          };
        }
        if (tx.deployContract) {
          return {
            type: "deployContract",
            name: tx.deployContract.name,
            content: tx.deployContract.content,
            sender: tx.deployContract.sender,
          };
        }
        if (tx.transferSTX) {
          return {
            type: "transferSTX",
            amount: tx.transferSTX.amount,
            recipient: tx.transferSTX.recipient,
            sender: tx.transferSTX.sender,
          };
        }
        return { type: "unknown" };
      });

      const resp = send({ method: "mineBlock", txs: serialized }) as {
        results: ServerTxResult[];
      };
      return (resp.results ?? []).map(parseTxResult);
    },

    callPublicFn(
      contract: string,
      method: string,
      args: ClarityValue[],
      sender: string,
    ): ParsedTransactionResult {
      const resp = send({
        method: "callPublicFn",
        contract,
        function: method,
        args: args.map(argString),
        sender,
      }) as ServerTxResult;
      return parseTxResult(resp);
    },

    callReadOnlyFn(
      contract: string,
      method: string,
      args: ClarityValue[],
      sender: string,
    ): ParsedTransactionResult {
      const resp = send({
        method: "callReadOnlyFn",
        contract,
        function: method,
        args: args.map(argString),
        sender,
      }) as ServerTxResult;
      return parseTxResult(resp);
    },

    callPrivateFn(
      contract: string,
      method: string,
      args: ClarityValue[],
      sender: string,
    ): ParsedTransactionResult {
      const resp = send({
        method: "callPrivateFn",
        contract,
        function: method,
        args: args.map(argString),
        sender,
      }) as ServerTxResult;
      return parseTxResult(resp);
    },

    execute(snippet: string): ParsedTransactionResult {
      const resp = send({ method: "execute", snippet }) as ServerTxResult;
      return parseTxResult(resp);
    },

    getAssetsMap(): Map<string, Map<string, bigint>> {
      const resp = send({ method: "getAssetsMap" }) as {
        assetsMap: Record<string, Record<string, string>>;
      };
      const outer = new Map<string, Map<string, bigint>>();
      for (const [asset, holders] of Object.entries(resp.assetsMap ?? {})) {
        const inner = new Map<string, bigint>();
        for (const [addr, bal] of Object.entries(holders)) {
          inner.set(addr, BigInt(bal));
        }
        outer.set(asset, inner);
      }
      return outer;
    },

    getLastContractCallTrace(): string | undefined {
      return undefined;
    },

    async initSession(cwd: string, manifestPath: string): Promise<void> {
      send({ method: "initSession", cwd, manifestPath });
    },
  };
}
