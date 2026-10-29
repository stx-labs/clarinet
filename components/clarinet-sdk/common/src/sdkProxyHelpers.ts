import {
  Cl,
  ClarityValue,
  ClarityVersion,
  ContractCallOptions,
  PostCondition,
  PostConditionMode,
  PostConditionModeName,
  PostConditionWire,
  postConditionToHex,
  serializePostConditionWire,
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

export type RawTransactionMetadata = { txid: string; fee: bigint; nonce: bigint };

export type BroadcastTransactionResult =
  | { txid: string }
  | { error: string; reason: string; reason_data?: Record<string, unknown>; txid?: string };

export type BroadcastTransaction = (rawHex: string) => BroadcastTransactionResult;

/**
 * Post-conditions to enforce on a simnet transaction.
 *
 * Conditions use the same builder, wire, and hex formats as stacks.js. Hex is
 * also the escape hatch for conditions that stacks.js cannot build yet.
 *
 * Passing neither field preserves the SDK's unchecked behavior. Passing either
 * enables enforcement; `postConditionMode` defaults to `"deny"`.
 */
export type PostConditionOptions = Pick<
  ContractCallOptions,
  "postConditions" | "postConditionMode"
>;

/** `PostConditionOptions` as the Wasm SDK takes it: conditions already encoded. */
export type SerializedPostConditions = {
  postConditions?: string[];
  postConditionMode?: PostConditionModeName;
};

export function serializePostConditions(options?: PostConditionOptions): SerializedPostConditions {
  const conditions = options?.postConditions;
  const mode = options?.postConditionMode;

  // Preserve the distinction between an omitted list and an explicit empty one.
  return {
    ...(conditions !== undefined && {
      postConditions: conditions.map((pc) => {
        if (typeof pc === "string") return pc;
        return typeof pc.type === "string"
          ? postConditionToHex(pc as PostCondition)
          : serializePostConditionWire(pc as PostConditionWire);
      }),
    }),
    ...(mode !== undefined && {
      postConditionMode:
        mode === PostConditionMode.Allow
          ? "allow"
          : mode === PostConditionMode.Deny
            ? "deny"
            : mode === PostConditionMode.Originator
              ? "originator"
              : mode,
    }),
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
/** Deploy using the node's initialization rules. Success returns `(ok true)`,
 * regardless of the initializer's final expression. */
export type DeployContract = (
  name: string,
  content: string,
  options: DeployContractOptions | null,
  sender: string,
  postConditionOptions?: PostConditionOptions,
) => ParsedTransactionResult;

/** Execute a native STX transfer. Invalid transfers (self-transfers, zero
 * amounts and insufficient funds) throw without consuming a nonce. In
 * `mineBlock`, they return a rejected outcome without consuming a
 * nonce instead. */
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
      broadcastTx: string;
      callPublicFn?: never;
      callPrivateFn?: never;
      deployContract?: never;
      transferSTX?: never;
    }
  | {
      broadcastTx?: never;
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
      broadcastTx?: never;
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
      broadcastTx?: never;
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
      broadcastTx?: never;
      callPublicFn?: never;
      callPrivateFn?: never;
      deployContract?: never;
      transferSTX: {
        amount: number;
        recipient: string;
        sender: string;
      } & SerializedPostConditions;
    };

/** Check lossless u32 transport; Rust validates which Clarity versions are supported. */
export function getClarityVersionInputError(version: unknown): string | undefined {
  if (version === undefined) return;
  if (
    typeof version !== "number" ||
    !Number.isInteger(version) ||
    version < 0 ||
    version > 0xffffffff
  ) {
    return "Invalid Clarity version: expected an unsigned 32-bit integer";
  }
}

/** Keep invalid versions as per-entry rejections, including when mining queued transactions. */
export function mineBlockWithValidatedVersions(
  txs: Tx[],
  mine: (txs: Tx[]) => BlockTransactionResult[],
): BlockTransactionResult[] {
  const errors = txs.map((entry) =>
    getClarityVersionInputError(entry.deployContract?.options?.clarityVersion),
  );
  const valid = txs.filter((_, index) => errors[index] === undefined);
  const results = mine(valid);
  const queuedCount = results.length - valid.length;
  let index = queuedCount;
  return [
    ...results.slice(0, queuedCount),
    ...errors.map((error): BlockTransactionResult =>
      error === undefined ? results[index++] : { status: "rejected", error },
    ),
  ];
}

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

export type FailureKind =
  "abort_by_response" | "abort_by_post_condition" | "runtime_error" | "analysis_error";

/** Signed entries carry their transaction metadata; convenience entries do not. */
export type MinedTransactionDetails = {
  transaction?: RawTransactionMetadata | null;
};

/**
 * One outcome per entry; rejected entries do not consume a nonce. Like a node's
 * transaction receipt, an included failure's `execution` reports `(err none)`
 * when execution produced no value, and post-condition aborts report the
 * events they attempted, none of which were committed.
 */
export type BlockTransactionResult = MinedTransactionDetails &
  (
    | { status: "success"; execution: ParsedTransactionResult }
    | {
        status: "included_failure";
        kind: FailureKind;
        error: string;
        execution: ParsedTransactionResult;
      }
    | { status: "rejected"; error: string }
  );

export type MineBlock = (txs?: Array<Tx>) => BlockTransactionResult[];

export type SerializedTransactionResult = {
  result: string;
  events: string;
  costs: string;
  performance?: string;
};

export function parseTransactionResult(
  response: SerializedTransactionResult,
): ParsedTransactionResult {
  return {
    result: Cl.deserialize(response.result),
    events: parseEvents(response.events),
    costs: parseCosts(response.costs),
    performance: response.performance ?? undefined,
  };
}

export type SerializedBlockTransactionResult = {
  transaction?: { txid: string; fee: string; nonce: string } | null;
} & (
  | { status: "success"; execution: SerializedTransactionResult }
  | {
      status: "included_failure";
      kind: FailureKind;
      error: string;
      execution: SerializedTransactionResult;
    }
  | { status: "rejected"; error: string }
);

export function parseBlockTransactionResult(
  response: SerializedBlockTransactionResult,
): BlockTransactionResult {
  const details: MinedTransactionDetails = {};
  if (response.transaction !== undefined) {
    details.transaction =
      response.transaction === null
        ? null
        : {
            txid: response.transaction.txid,
            fee: BigInt(response.transaction.fee),
            nonce: BigInt(response.transaction.nonce),
          };
  }
  switch (response.status) {
    case "rejected":
      return { ...details, status: "rejected", error: response.error };
    case "success":
      return {
        ...details,
        status: "success",
        execution: parseTransactionResult(response.execution),
      };
    case "included_failure":
      return {
        ...details,
        status: "included_failure",
        kind: response.kind,
        error: response.error,
        execution: parseTransactionResult(response.execution),
      };
  }
}
export type Execute = (snippet: string) => ParsedTransactionResult;
export type GetDataVar = (contract: string, dataVar: string) => ClarityValue;
export type GetMapEntry = (contract: string, mapName: string, mapKey: ClarityValue) => ClarityValue;
