import {
  Cl,
  ClarityValue,
  ClarityVersion,
  PostCondition,
  postConditionToHex,
} from "@stacks/transactions";

export type ClarityEvent = {
  event: string;
  data: { raw_value?: string; value?: ClarityValue; [key: string]: any };
};

export type ExecutionCost = {
  writeLength: number;
  writeCount: number;
  readLength: number;
  readCount: number;
  runtime: number;
};

export type ClarityCosts = {
  total: ExecutionCost;
  limit: ExecutionCost;
  memory: number;
  memory_limit: number;
};

export type ParsedTransactionResult = {
  result: ClarityValue;
  events: ClarityEvent[];
  costs: ClarityCosts | null;
  performance: string | undefined;
};

/** How a transaction's asset movement is constrained. */
export type PostConditionMode = "allow" | "deny" | "originator";

/**
 * Post-conditions to enforce on a simnet transaction.
 *
 * A condition is either a stacks.js `PostCondition` or the hex encoding of one.
 * Hex is the escape hatch for the `Staking` and `Pox` conditions that stacks.js
 * cannot build yet.
 *
 * Passing neither field leaves asset movement unconstrained, as it has always
 * been. Passing either one turns enforcement on, and `postConditionMode`
 * defaults to `"deny"` — so anything you did not account for fails, which is
 * what a wallet does on mainnet.
 */
export type PostConditionOptions = {
  postConditions?: (PostCondition | string)[];
  postConditionMode?: PostConditionMode;
};

/** `PostConditionOptions` as the Wasm SDK takes it: conditions already encoded. */
export type SerializedPostConditions = {
  postConditions?: string[];
  postConditionMode?: PostConditionMode;
};

export function serializePostConditions(options?: PostConditionOptions): SerializedPostConditions {
  const conditions = options?.postConditions;
  const mode = options?.postConditionMode;

  // An omitted list disables enforcement entirely; an empty list denies every
  // asset movement under the default mode. Both have to reach Rust intact.
  return {
    ...(conditions !== undefined && {
      postConditions: conditions.map((pc) =>
        typeof pc === "string" ? pc : postConditionToHex(pc),
      ),
    }),
    ...(mode !== undefined && { postConditionMode: mode }),
  };
}

export type CallFn = (
  contract: string,
  method: string,
  args: ClarityValue[],
  sender: string,
  options?: PostConditionOptions,
) => ParsedTransactionResult;

/** A read-only call moves no assets, so it takes no post-conditions. */
export type CallReadOnlyFn = (
  contract: string,
  method: string,
  args: ClarityValue[],
  sender: string,
) => ParsedTransactionResult;

export type DeployContractOptions = {
  clarityVersion: ClarityVersion;
};
export type DeployContract = (
  name: string,
  content: string,
  options: DeployContractOptions | null,
  sender: string,
  postConditionOptions?: PostConditionOptions,
) => ParsedTransactionResult;

export type TransferSTX = (
  amount: number | bigint,
  recipient: string,
  sender: string,
  options?: PostConditionOptions,
) => ParsedTransactionResult;

/**
 * A transaction in a `mineBlock` batch. Post-conditions are already encoded,
 * because the batch is handed to Wasm as plain JSON.
 */
export type Tx =
  | {
      callPublicFn: {
        contract: string;
        method: string;
        args: ClarityValue[];
        sender: string;
      } & SerializedPostConditions;
      callPrivateFn?: never;
      deployContract?: never;
      transferSTX?: never;
    }
  | {
      callPublicFn?: never;
      callPrivateFn: {
        contract: string;
        method: string;
        args: ClarityValue[];
        sender: string;
      } & SerializedPostConditions;
      deployContract?: never;
      transferSTX?: never;
    }
  | {
      callPublicFn?: never;
      callPrivateFn?: never;
      deployContract: {
        name: string;
        content: string;
        options: DeployContractOptions | null;
        sender: string;
      } & SerializedPostConditions;
      transferSTX?: never;
    }
  | {
      callPublicFn?: never;
      callPrivateFn?: never;
      deployContract?: never;
      transferSTX: {
        amount: number;
        recipient: string;
        sender: string;
      } & SerializedPostConditions;
    };

export const tx = {
  callPublicFn: (
    contract: string,
    method: string,
    args: ClarityValue[],
    sender: string,
    options?: PostConditionOptions,
  ): Tx => ({
    callPublicFn: { contract, method, args, sender, ...serializePostConditions(options) },
  }),
  callPrivateFn: (
    contract: string,
    method: string,
    args: ClarityValue[],
    sender: string,
    options?: PostConditionOptions,
  ): Tx => ({
    callPrivateFn: { contract, method, args, sender, ...serializePostConditions(options) },
  }),
  deployContract: (
    name: string,
    content: string,
    options: DeployContractOptions | null,
    sender: string,
    postConditionOptions?: PostConditionOptions,
  ): Tx => ({
    deployContract: {
      name,
      content,
      options,
      sender,
      ...serializePostConditions(postConditionOptions),
    },
  }),
  transferSTX: (
    amount: number,
    recipient: string,
    sender: string,
    options?: PostConditionOptions,
  ): Tx => ({
    transferSTX: { amount, recipient, sender, ...serializePostConditions(options) },
  }),
};

export function parseEvents(events: string): ClarityEvent[] {
  try {
    // @todo: improve type safety
    return JSON.parse(events).map((e: string) => {
      const { event, data } = JSON.parse(e);
      if ("raw_value" in data) {
        data.value = Cl.deserialize(data.raw_value);
      }
      return {
        event: event,
        data: data,
      };
    });
  } catch (e) {
    console.error(`Fail to parse events: ${e}`);
    return [];
  }
}

export function parseCosts(costs: string): ClarityCosts | null {
  try {
    let { memory, memory_limit, total, limit } = JSON.parse(costs);
    return {
      memory: memory,
      memory_limit: memory_limit,
      total: {
        writeLength: total.write_length,
        writeCount: total.write_count,
        readLength: total.read_length,
        readCount: total.read_count,
        runtime: total.runtime,
      },
      limit: {
        writeLength: limit.write_length,
        writeCount: limit.write_count,
        readLength: limit.read_length,
        readCount: limit.read_count,
        runtime: limit.runtime,
      },
    };
  } catch (_e) {
    return null;
  }
}

export type MineBlock = (txs: Array<Tx>) => ParsedTransactionResult[];
export type Execute = (snippet: string) => ParsedTransactionResult;
export type GetDataVar = (contract: string, dataVar: string) => ClarityValue;
export type GetMapEntry = (contract: string, mapName: string, mapKey: ClarityValue) => ClarityValue;
