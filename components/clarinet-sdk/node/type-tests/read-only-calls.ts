import type { Simnet } from "../src/sdkProxy";

declare const simnet: Simnet;

simnet.callReadOnlyFn("counter", "get-count", [], "ST000000000000000000002AMW42H");

// Public calls are transactions and accept post-condition options.
simnet.callPublicFn("counter", "increment", [], "ST000000000000000000002AMW42H", {
  postConditions: [],
});

// Read-only calls cannot move assets, so post-condition options are invalid.
// @ts-expect-error callReadOnlyFn intentionally has no post-condition options parameter
simnet.callReadOnlyFn("counter", "get-count", [], "ST000000000000000000002AMW42H", {
  postConditionMode: "deny",
});
