import { Cl, serializeCVBytes } from "@stacks/transactions";

import {
  parseBlockTransactionResult,
  validateClarityVersion,
  mineBlockWithValidatedVersions,
  parseTransactionResult,
  type SerializedTransactionResult,
  type SerializedBlockTransactionResult,
  serializePostConditions,
  type BroadcastTransaction,
  type CallFn,
  type CallReadOnlyFn,
  type DeployContract,
  type Execute,
  type GetDataVar,
  type GetMapEntry,
  type MineBlock,
  type TransferSTX,
} from "./sdkProxyHelpers.js";

type ProxySdk = {
  broadcastTransaction(rawHex: string): string;
  callReadOnlyFn(args: unknown): SerializedTransactionResult;
  callPublicFn(args: unknown): SerializedTransactionResult;
  callPrivateFn(args: unknown): SerializedTransactionResult;
  deployContract(args: unknown): SerializedTransactionResult;
  execute(snippet: string): SerializedTransactionResult;
  getDataVar(contract: string, varName: string): string;
  getMapEntry(contract: string, mapName: string, mapKey: Uint8Array): string;
  mineBlock(txs: unknown[]): SerializedBlockTransactionResult[];
  transferSTX(args: unknown): SerializedTransactionResult;
};

export type ProxiedSimnet<S extends ProxySdk> = {
  [K in keyof S]: K extends "broadcastTransaction"
    ? BroadcastTransaction
    : K extends "callReadOnlyFn"
      ? CallReadOnlyFn
      : K extends "callPublicFn" | "callPrivateFn"
        ? CallFn
        : K extends "execute"
          ? Execute
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
          return parseTransactionResult(response);
        };
        return callFn;
      }

      if (prop === "broadcastTransaction") {
        const broadcast: BroadcastTransaction = (rawHex) =>
          JSON.parse(sdk.broadcastTransaction(rawHex));
        return broadcast;
      }

      if (prop === "execute") {
        const execute: Execute = (snippet) => parseTransactionResult(sdk.execute(snippet));
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
          validateClarityVersion(options?.clarityVersion);
          const rustOptions = options
            ? new ContractOptions(options.clarityVersion)
            : new ContractOptions();
          const { postConditions, postConditionMode } =
            serializePostConditions(postConditionOptions);
          return parseTransactionResult(
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
          return parseTransactionResult(
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
        const mineBlock: MineBlock = (txs = []) =>
          mineBlockWithValidatedVersions(txs, (txs) => {
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
            return sdk.mineBlock(serializedTxs).map(parseBlockTransactionResult);
          });
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
