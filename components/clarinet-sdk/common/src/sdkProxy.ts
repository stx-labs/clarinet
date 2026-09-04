import { Cl, serializeCVBytes } from "@stacks/transactions";

import {
  parseCosts,
  parseEvents,
  serializePostConditions,
  type CallFn,
  type CallReadOnlyFn,
  type DeployContract,
  type Execute,
  type GetDataVar,
  type GetMapEntry,
  type MineBlock,
  type ParsedTransactionResult,
  type TransferSTX,
} from "./sdkProxyHelpers.js";

type WasmTransactionRes = {
  result: string;
  events: string;
  costs: string;
  performance?: string;
};

type ProxySdk = {
  callReadOnlyFn(args: unknown): WasmTransactionRes;
  callPublicFn(args: unknown): WasmTransactionRes;
  callPrivateFn(args: unknown): WasmTransactionRes;
  deployContract(args: unknown): WasmTransactionRes;
  execute(snippet: string): WasmTransactionRes;
  getDataVar(contract: string, varName: string): string;
  getMapEntry(contract: string, mapName: string, mapKey: Uint8Array): string;
  mineBlock(txs: unknown[]): WasmTransactionRes[];
  runSnippet(snippet: string): string;
  transferSTX(args: unknown): WasmTransactionRes;
};

/** @deprecated use `simnet.execute(command)` instead */
type RunSnippet<S extends ProxySdk> = S["runSnippet"];

export type ProxiedSimnet<S extends ProxySdk> = {
  [K in keyof S]: K extends "callReadOnlyFn"
    ? CallReadOnlyFn
    : K extends "callPublicFn" | "callPrivateFn"
      ? CallFn
      : K extends "execute"
        ? Execute
        : K extends "runSnippet"
          ? RunSnippet<S>
          : K extends "deployContract"
            ? DeployContract
            : K extends "transferSTX"
              ? TransferSTX
              : K extends "mineBlock"
                ? MineBlock
                : K extends "getDataVar"
                  ? GetDataVar
                  : K extends "getMapEntry"
                    ? GetMapEntry
                    : S[K];
};

type ProxyBindings<ContractOptions> = {
  CallFnArgs: new (
    contract: string,
    method: string,
    args: Uint8Array[],
    sender: string,
    postConditions?: string[],
    postConditionMode?: string,
  ) => unknown;
  ContractOptions: new (clarityVersion?: number) => ContractOptions;
  DeployContractArgs: new (
    name: string,
    content: string,
    options: ContractOptions,
    sender: string,
    postConditions?: string[],
    postConditionMode?: string,
  ) => unknown;
  TransferSTXArgs: new (
    amount: bigint,
    recipient: string,
    sender: string,
    postConditions?: string[],
    postConditionMode?: string,
  ) => unknown;
};

function parseTxResponse(response: WasmTransactionRes): ParsedTransactionResult {
  return {
    result: Cl.deserialize(response.result),
    events: parseEvents(response.events),
    costs: parseCosts(response.costs),
    performance: response.performance,
  };
}

export function createSessionProxy<S extends ProxySdk, ContractOptions>(
  bindings: ProxyBindings<ContractOptions>,
): ProxyHandler<S> {
  const { CallFnArgs, ContractOptions, DeployContractArgs, TransferSTXArgs } = bindings;

  return {
    get(session, prop, receiver) {
      const sdk: ProxySdk = session;

      if (prop === "callReadOnlyFn" || prop === "callPublicFn" || prop === "callPrivateFn") {
        const callFn: CallFn = (contract, method, args, sender, options) => {
          const { postConditions, postConditionMode } = serializePostConditions(options);
          const response = sdk[prop](
            new CallFnArgs(
              contract,
              method,
              args.map(serializeCVBytes),
              sender,
              postConditions,
              postConditionMode,
            ),
          );
          return parseTxResponse(response);
        };
        return callFn;
      }

      if (prop === "execute") {
        const execute: Execute = (snippet) => parseTxResponse(sdk.execute(snippet));
        return execute;
      }

      if (prop === "deployContract") {
        const deployContract: DeployContract = (
          name,
          content,
          options,
          sender,
          postConditionOptions,
        ) => {
          const rustOptions = options
            ? new ContractOptions(options.clarityVersion)
            : new ContractOptions();
          const { postConditions, postConditionMode } =
            serializePostConditions(postConditionOptions);
          return parseTxResponse(
            sdk.deployContract(
              new DeployContractArgs(
                name,
                content,
                rustOptions,
                sender,
                postConditions,
                postConditionMode,
              ),
            ),
          );
        };
        return deployContract;
      }

      if (prop === "transferSTX") {
        const transferSTX: TransferSTX = (amount, recipient, sender, options) => {
          const { postConditions, postConditionMode } = serializePostConditions(options);
          return parseTxResponse(
            sdk.transferSTX(
              new TransferSTXArgs(
                BigInt(amount),
                recipient,
                sender,
                postConditions,
                postConditionMode,
              ),
            ),
          );
        };
        return transferSTX;
      }

      if (prop === "mineBlock") {
        const mineBlock: MineBlock = (txs) => {
          const serializedTxs = txs.map((tx) => {
            if (tx.callPublicFn) {
              return {
                callPublicFn: {
                  ...tx.callPublicFn,
                  args_maps: tx.callPublicFn.args.map(serializeCVBytes),
                },
              };
            }
            if (tx.callPrivateFn) {
              return {
                callPrivateFn: {
                  ...tx.callPrivateFn,
                  args_maps: tx.callPrivateFn.args.map(serializeCVBytes),
                },
              };
            }
            if (tx.deployContract) {
              return { deployContract: { ...tx.deployContract } };
            }
            return tx;
          });
          return sdk.mineBlock(serializedTxs).map(parseTxResponse);
        };
        return mineBlock;
      }

      if (prop === "getDataVar") {
        const getDataVar: GetDataVar = (...args) => Cl.deserialize(sdk.getDataVar(...args));
        return getDataVar;
      }

      if (prop === "getMapEntry") {
        const getMapEntry: GetMapEntry = (contract, mapName, mapKey) =>
          Cl.deserialize(sdk.getMapEntry(contract, mapName, serializeCVBytes(mapKey)));
        return getMapEntry;
      }

      return Reflect.get(session, prop, receiver);
    },
  };
}
