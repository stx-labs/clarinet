## Approach A: Rust-generated concrete types
(clarity-ts-typegen → /generated/typed/)

Output: export type IncrementArgs = { step: UIntCV };

Pros:
+ Clear, readable error messages ("expected UIntCV, got BooleanCV")
+ Types are cmd-clickable — jump straight to the definition
+ Zero TS compiler overhead — plain type aliases
+ Easy to review in PRs — generated types are self-documenting
+ No extra dependency for consumers (just @stacks/transactions)

Cons:
- Rust codegen must map all ContractInterfaceAtomType variants
- Generated files are checked in or need a build step
- If Stacks.js renames a CV type, Rust codegen must update

Mitigated by:
→ Clarity has 13 type variants. Stable for years. Low churn.
→ Stacks.js CV type names (UIntCV, BooleanCV, etc.) haven't changed.
→ Unit tests cover all 13 variants — breakage is caught immediately.


## Approach B: ABI as const + TypeScript utility types
(clarity-abi-typegen → generated/abi/)

Output: export const counterAbi = { ... } as const;
         type X = Args<typeof counterAbi, "increment">  // resolves to { step: UIntCV }

Pros:
+ ABI is the single source of truth — types can't drift by construction
+ Minimal Rust code — just JSON serialization
+ Utility types work on any ABI (fetched from RPC, not just Clarinet)
+ No generated type files to maintain — only data

Cons:
- Error messages are deeply nested conditional types, hard to read
- IDE hover shows resolved type, but errors reference the type machinery
- Slight TS compiler perf cost (conditional type evaluation)
- Consumers must understand "typeof abiConst" pattern
- Tuple field names stay kebab-case (TS key access: args["my-field"])
- Requires shipping + versioning the utility type library

Mitigated by:
→ Reusability is real, but only matters if non-Clarinet consumers exist.
→ "Types can't drift" is strong in theory, but with 13 stable variants
   and unit tests, drift is not a practical risk for Approach A either.


## Bottom line

The key differentiator is developer experience at the point of error.
Approach A wins on:  error clarity, readability, zero abstraction cost.
Approach B wins on:  architectural elegance, reusability beyond Clarinet.

For a team shipping Clarity contracts, Approach A is the pragmatic choice.
Approach B becomes compelling if the utility types need to serve a broader
ecosystem (e.g., a standalone @stacks/abi-types package used outside Clarinet).
