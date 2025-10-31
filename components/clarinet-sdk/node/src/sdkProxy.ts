import { Cl, serializeCVBytes } from "@stacks/transactions";
import {
  CallFnArgs,
  DeployContractArgs,
  TransferSTXArgs,
  ContractOptions,
  type SDK,
  type TransactionRes,
} from "@stacks/clarinet-sdk-wasm";

import {
  parseEvents,
  type CallFn,
  type DeployContract,
  type GetDataVar,
  type GetMapEntry,
  type MineBlock,
  type ParsedTransactionResult,
  type Execute,
  type TransferSTX,
  parseCosts,
} from "../../common/src/sdkProxyHelpers.js";

/** @deprecated use `simnet.execute(command)` instead */
type RunSnippet = SDK["runSnippet"];

// because the session is wrapped in a proxy the types need to be hardcoded
export type Simnet = {
  [K in keyof SDK]: K extends "callReadOnlyFn" | "callPublicFn" | "callPrivateFn"
    ? CallFn
    : K extends "execute"
      ? Execute
      : K extends "runSnippet"
        ? RunSnippet
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
                  : SDK[K];
};

function parseTxResponse(response: TransactionRes): ParsedTransactionResult {
  return {
    result: Cl.deserialize(response.result),
    events: parseEvents(response.events),
    costs: parseCosts(response.costs),
    performance: response.performance,
  };
}

export function getSessionProxy() {
  return {
    get(session: SDK, prop: keyof SDK, receiver: any) {
      // some of the WASM methods are proxied here to:
      // - serialize clarity values input argument
      // - deserialize output into clarity values

      if (prop === "callReadOnlyFn" || prop === "callPublicFn" || prop === "callPrivateFn") {
        const callFn: CallFn = (contract, method, args, sender) => {
          const response = session[prop](
            new CallFnArgs(contract, method, args.map(serializeCVBytes), sender),
          );
          return parseTxResponse(response);
        };
        return callFn;
      }

      if (prop === "execute") {
        const execute: Execute = (snippet) => {
          const response = session.execute(snippet);
          return parseTxResponse(response);
        };
        return execute;
      }

      if (prop === "deployContract") {
        const callDeployContract: DeployContract = (name, content, options, sender) => {
          const rustOptions = options
            ? new ContractOptions(options.clarityVersion)
            : new ContractOptions();

          const response = session.deployContract(
            new DeployContractArgs(name, content, rustOptions, sender),
          );
          return parseTxResponse(response);
        };
        return callDeployContract;
      }

      if (prop === "transferSTX") {
        const callTransferSTX: TransferSTX = (amount, ...args) => {
          const response = session.transferSTX(new TransferSTXArgs(BigInt(amount), ...args));
          return parseTxResponse(response);
        };
        return callTransferSTX;
      }

      if (prop === "mineBlock") {
        const callMineBlock: MineBlock = (txs) => {
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
              return {
                deployContract: {
                  ...tx.deployContract,
                },
              };
            }
            return tx;
          });

          const responses: TransactionRes[] = session.mineBlock(serializedTxs);
          return responses.map(parseTxResponse);
        };
        return callMineBlock;
      }

      if (prop === "getDataVar") {
        const getDataVar: GetDataVar = (...args) => {
          const response = session.getDataVar(...args);
          const result = Cl.deserialize(response);
          return result;
        };
        return getDataVar;
      }

      if (prop === "getMapEntry") {
        const getMapEntry: GetMapEntry = (contract, mapName, mapKey) => {
          const response = session.getMapEntry(contract, mapName, serializeCVBytes(mapKey));
          const result = Cl.deserialize(response);
          return result;
        };
        return getMapEntry;
      }

      return Reflect.get(session, prop, receiver);
    },
  };
}
