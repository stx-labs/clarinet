import type { ClarityValue } from "@stacks/transactions";
import type { Simnet } from "../src/sdkProxy";
import type { BlockTransactionResult } from "../src/index";

declare const simnet: Simnet;
const outcomes: BlockTransactionResult[] = simnet.mineBlock([]);
for (const outcome of outcomes) {
  const rawOutput: ClarityValue | null | undefined = outcome.result;
  void rawOutput;
  if (outcome.status === "success") {
    const value: ClarityValue = outcome.execution.result;
    void value;
  } else if (outcome.status === "included_failure") {
    const message: string = outcome.error;
    const value: ClarityValue | undefined = outcome.execution?.result;
    void [message, value];
  } else {
    const message: string = outcome.error;
    void message;
    // @ts-expect-error Rejected entries have no execution result.
    outcome.execution;
  }
}
