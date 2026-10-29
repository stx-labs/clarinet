import type { ClarityValue } from "@stacks/transactions";
import type { Simnet } from "../src/sdkProxy";
import type { BlockTransactionResult, FailureKind } from "../src/index";

declare const simnet: Simnet;
const outcomes: BlockTransactionResult[] = simnet.mineBlock([]);
for (const outcome of outcomes) {
  // @ts-expect-error Results are only reported under `execution`.
  outcome.result;
  if (outcome.status === "success") {
    const value: ClarityValue = outcome.execution.result;
    void value;
  } else if (outcome.status === "included_failure") {
    const message: string = outcome.error;
    const kind: FailureKind = outcome.kind;
    const value: ClarityValue = outcome.execution.result;
    void [message, kind, value];
  } else {
    const message: string = outcome.error;
    void message;
    // @ts-expect-error Rejected entries have no execution result.
    outcome.execution;
  }
}
