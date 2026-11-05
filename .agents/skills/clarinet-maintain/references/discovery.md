# Finding maintenance work

Start with cheap inspection of manifests, CI, existing diagnostics, and the areas
implicated by previous findings. Pick promising candidates and follow their callers
and data flow. During longer runs, rotate topics rather than repeatedly scanning
the same files. Record searched areas and rejected candidates in the run log.

The commands below discover candidates; their output does not establish that a
change is safe. Use installed tools when available and check their supported flags.
If an optional scanner is unavailable, use targeted inspection or record the gap;
adding every scanner to the repository is not a prerequisite. Consult the
[repository checks](maintenance-checks.md) before acting on findings.

## Rust dependencies

- **Unused:** start with `cargo machete`. For findings needing compiler evidence,
  optionally use `cargo +nightly udeps -p <crate> --all-targets` with the relevant
  features and target. Udeps needs nightly for analysis; this does not change the
  project's supported toolchain. Check build scripts, generated bindings, macros,
  renamed crates, and dependencies used only to enable features before removal.
- **Duplicate versions:** run `cargo tree --workspace --duplicates --locked`, then
  `cargo tree --workspace --invert <crate>@<version> --locked` to find the owners.
  Look for compatible requirement alignment and unnecessary direct dependencies.
  Multiple versions can be necessary; do not force incompatible versions together
  or upgrade the Stacks dependency family merely to shrink this list. Inspect the
  feature graph when aligning dependencies.
- **Deprecated or unmaintained:** check registry notices, upstream announcements,
  advisories, and release/migration documentation. Low commit frequency alone is
  insufficient evidence. Prefer removing an unused dependency or reusing an
  existing one. For a replacement, establish the concrete maintenance benefit and
  compare API semantics, formats, features, licenses, MSRV, and native/WASM support.
  Keep lockfile changes scoped to the candidate and inspect unexpected churn.

Tool references: [cargo-machete](https://github.com/bnjbvr/cargo-machete),
[cargo-udeps](https://github.com/est31/cargo-udeps).

## TypeScript dependencies and dead exports

Run discovery separately in the root SDK workspace and `components/clarity-vscode`,
using each workspace's declared pnpm version. Include separately managed examples
when they are in scope.

- **Unused dependencies, files, and exports:** use `pnpm exec knip` if available.
  Configure workspace entry points, package exports, scripts, tests, bundler plugins,
  generated WASM imports, and extension activation/debug entry points. Confirm
  suspected dead exports against published SDK consumers; no local caller does not
  mean an exported API is unused.
- **Duplicate versions:** inspect `pnpm -r list --depth Infinity` and
  `pnpm -r why <package>`. Use `pnpm dedupe --check` to identify possible
  consolidation, then examine compatible ranges and peer dependencies. An actual
  `pnpm dedupe` can change resolved versions; validate its diff and affected packages.
  Distinct peer contexts and incompatible majors may require separate instances.
- **Deprecated or unmaintained:** inspect registry metadata with
  `pnpm view <package> deprecated repository --json` and verify upstream status.
  Apply the Rust replacement criteria, also checking Node/browser support, package
  exports, ESM/CommonJS behavior, typings, and extension bundling.

Tool references: [Knip workspaces](https://knip.dev/features/monorepos-and-workspaces),
[pnpm dedupe](https://pnpm.io/cli/dedupe), [pnpm why](https://pnpm.io/cli/why).

## Dead code, stale comments, and robustness

Use compiler warnings and reference searches (`rg -n '<symbol>' components`) to
trace suspected unused helpers, fields, configuration, and feature gates. Check
other targets, tests, examples, generated consumers, and public interfaces before
deleting. Verify comments against their code and callers; correct misleading
invariants and obsolete workarounds, preserving explanations of non-obvious choices.

For bugs, trace boundary inputs through parsing, indexing, arithmetic, and error
handling. Look for malformed-input panics, incorrect state transitions, and missing
cleanup on failure. Establish the expected behavior from existing contracts and
produce a reproducer before choosing a fix.

## Unit test coverage and quality

Review Rust unit tests and TypeScript test suites alongside the behavior they exercise.
Trace assertions to production code, supported contracts, and past regressions.
Use reference searches and existing coverage reports to locate gaps; `cargo cov-dev`
generates the repository's native Rust HTML coverage report when its tooling is
available. Check SDK/WASM tests separately. Coverage is a discovery aid: an executed
line may have no meaningful assertion, and a low percentage alone does not justify
adding tests.

- **Missing coverage:** add focused tests for observable behavior with plausible
  failure modes, especially boundary values, malformed inputs, error paths, state
  transitions, and target-specific behavior. Derive expected results independently
  of the implementation. Identify the defect each test would catch; where useful,
  verify sensitivity with a temporary, reverted fault. Tests for existing correct
  behavior should pass without production changes; newly discovered bugs follow
  the regression-first workflow.
- **Duplicate tests:** compare inputs, execution paths, assertions, and the contract
  each protects. Consolidate or remove tests with no distinct protection, preserving
  useful cases through parameterization when appropriate. Similar tests may cover
  different boundaries, platforms, or historical bugs; preserve that coverage.
- **Obsolete tests:** verify that a feature or limitation has actually been removed
  from the supported contract before deleting or updating its tests and unused
  fixtures. A regression test remains useful after its bug is fixed. A failing test
  is not evidence that its expectation is obsolete.
- **Tautological or trivial tests:** look for self-comparisons, expected values
  computed by the same code being tested, mocks that only confirm their own setup,
  and assertions about language/library behavior without a repository invariant.
  Replace them with meaningful behavioral assertions or remove them if they protect
  nothing. A small or obvious test can still guard a real contract.

Run the affected suite after additions, consolidation, or removal. Keep tests
deterministic and focused on contracts rather than implementation details. Record
the coverage gained or the reason a removed test added no protection; do not weaken
assertions, delete failing tests, or rewrite snapshots merely to make the suite pass.

## Performance and allocations

Inspect repeated parsing, serialization, filesystem/network work, linear searches
inside loops, repeated vector removal, and locks held across expensive work.
Searches such as `rg -n '\.clone\(|\.to_owned\(|\.to_string\(|collect\(' components -g '*.rs'`
can locate allocation candidates; each may be necessary for ownership or
clarity. Also inspect repeated array/object copying and nested scans in TypeScript.

Follow candidates into realistic call paths. Prefer borrowing, reuse, preallocation,
or a more appropriate algorithm when that simplifies the work. Check ordering,
duplicate handling, cache invalidation, and error behavior. Use existing benchmarks
or a representative release workload to compare before/after time, allocations,
or memory, as appropriate. Retain measurements and inputs; a shorter expression is
not evidence of a speedup.

## Language idioms and static analysis

- Follow `.agents/rust-conventions.md`. Check `rust-version`, toolchain configuration,
  CI, and documented support before using newer syntax or library APIs. If the MSRV
  is unspecified, establish the intended baseline rather than inferring that every
  developer can use the latest stable compiler. Apply the same check to TypeScript
  output targets and Node/browser requirements. Favor clearer code over syntax churn.
- Run existing Clippy checks first. Explore additional nursery diagnostics on a
  relevant crate with `cargo clippy -p <crate> --tests -- -W clippy::nursery` and the
  appropriate target/features. Review each diagnostic and suggested fix; enable
  individual useful lints only after checking their signal across supported builds.
  Verify affected crates inherit workspace lints. Avoid wholesale nursery denial or
  automatic fixes whose behavioral assumptions have not been checked.
- The extension already has ESLint: `pnpm --dir components/clarity-vscode run lint`.
  Consider selected typescript-eslint rules with type information, such as
  `no-floating-promises`, `no-misused-promises`, and `switch-exhaustiveness-check`.
  SDK linting can reuse that tooling when useful. Configure the correct TypeScript
  projects, check false positives and CI cost, and validate promise/error semantics
  before fixing findings. Introduce checks incrementally instead of replacing the
  lint stack or enabling every strict rule.

References: [Clippy lint configuration](https://doc.rust-lang.org/clippy/usage.html),
[typescript-eslint typed linting](https://typescript-eslint.io/getting-started/typed-linting/).

## Binary size and build performance

Inspect workspace profiles, `.cargo/config.toml`, build scripts, and release CI for
the settings actually used. Experiment with LTO, codegen units, optimization levels,
and symbol stripping only against comparable release artifacts and workloads.
Measure binary/WASM size, runtime, build time, and debugging tradeoffs. Workspace
profiles and a separately invoked WASM build may receive different settings.

Preserve portability, unwinding/error behavior, diagnostics, and required target
flags. Settings such as `target-cpu=native`, `panic=abort`, or disabled overflow checks
can change compatibility or behavior and need that decision explicitly in scope.
Do not adopt a flag merely because another project's binary got smaller.

Reference: [Cargo profiles](https://doc.rust-lang.org/cargo/reference/profiles.html).

## Unsafe code and native dependencies

Locate local unsafe/FFI boundaries with
`rg -n '\bunsafe\b|extern "C"' components -g '*.rs'`. Inspect manifests, build scripts,
and dependency paths for C wrappers or c2rust translations. Prefer an existing safe
API when it removes an unsafe block without losing required semantics or performance.
For dependency replacements, compare algorithm/format compatibility, platform and
WASM coverage, maintenance, and measured cost. Pure Rust alone does not establish
correctness; cryptographic or protocol-sensitive replacements need especially strong
equivalence evidence. Favor a bounded replacement over rewriting an implementation.

## Structure and types

Look for functions mixing parsing, validation, orchestration, and formatting;
repeated groups of arguments; modules with unrelated responsibilities; and dependency
cycles or coupling that suggest a better crate boundary. Extract cohesive operations
or types when that makes responsibilities clearer. Line count alone does not justify
splitting a function, and moving code between crates must account for feature gates
and the native/WASM dependency graph.

Look for `Result<_, String>` losing error structure, boolean parameters obscuring
intent, finite string choices, and structs permitting invalid combinations. Consider
typed errors, enums/discriminated unions, validated newtypes, or options structs when
they express a real invariant. Preserve error messages, serialized forms, defaults,
accepted inputs, and public signatures. Changing an exposed error type or rejecting
previously accepted strings is a compatibility change, even when the new type is
clearer; use internal conversions or defer the public migration.
