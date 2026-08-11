import { Cl, ClarityValue, ClarityVersion } from "@stacks/transactions";

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

export type TraceKind = "call" | "return" | "event" | "error";

export type TraceEntry = {
  kind: TraceKind;
  /** Call-stack depth (0 = top-level call). */
  depth: number;
  contract: string;
  /** Function name; empty for `event` and `error` entries. */
  function: string;
  line: number;
  column: number;
  /** Argument values as Clarity strings, present on `call` entries. */
  args?: string[];
  /** Return value (`return`), event description (`event`), or undefined. */
  value?: string;
  /** Error message, present on `error` entries. */
  error?: string;
};

export type ParsedTransactionResult = {
  result: ClarityValue;
  events: ClarityEvent[];
  costs: ClarityCosts | null;
  performance: string | undefined;
  /** Structured execution trace, one entry per function call/return/event/error. */
  trace: TraceEntry[];
};

export type CallFn = (
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
) => ParsedTransactionResult;

export type TransferSTX = (
  amount: number | bigint,
  recipient: string,
  sender: string,
) => ParsedTransactionResult;

export type Tx =
  | {
      callPublicFn: {
        contract: string;
        method: string;
        args: ClarityValue[];
        sender: string;
      };
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
      };
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
      };
      transferSTX?: never;
    }
  | {
      callPublicFn?: never;
      callPrivateFn?: never;
      deployContract?: never;
      transferSTX: { amount: number; recipient: string; sender: string };
    };

export const tx = {
  callPublicFn: (contract: string, method: string, args: ClarityValue[], sender: string): Tx => ({
    callPublicFn: { contract, method, args, sender },
  }),
  callPrivateFn: (contract: string, method: string, args: ClarityValue[], sender: string): Tx => ({
    callPrivateFn: { contract, method, args, sender },
  }),
  deployContract: (
    name: string,
    content: string,
    options: DeployContractOptions | null,
    sender: string,
  ): Tx => ({
    deployContract: { name, content, options, sender },
  }),
  transferSTX: (amount: number, recipient: string, sender: string): Tx => ({
    transferSTX: { amount, recipient, sender },
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

export function parseTrace(trace: string | null | undefined): TraceEntry[] {
  if (!trace) return [];
  try {
    return JSON.parse(trace) as TraceEntry[];
  } catch {
    return [];
  }
}

export function printTrace(label: string, trace: TraceEntry[]): void {
  console.log(`\n── trace: ${label} ──`);
  for (const entry of trace) {
    const indent = "  ".repeat(entry.depth);
    if (entry.kind === "call") {
      const args = entry.args?.length ? `(${entry.args.join(", ")})` : "()";
      console.log(`${indent}→ call  ${entry.contract}.${entry.function}${args}  [${entry.line}:${entry.column}]`);
    } else if (entry.kind === "return") {
      console.log(`${indent}← return ${entry.contract}.${entry.function} = ${entry.value}`);
    } else if (entry.kind === "event") {
      console.log(`${indent}★ ${entry.value}`);
    } else if (entry.kind === "error") {
      console.log(`${indent}✗ error at ${entry.contract} ${entry.line}:${entry.column}: ${entry.error}`);
    }
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
