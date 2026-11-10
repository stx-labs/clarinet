# Maintenance discovery

Use available tools or targeted inspection; record unavailable checks.
Apply [repository checks](checks.md).

## TODO/FIXME items

Start with known issues:

```bash
# Contents, including hidden files; respects ignore rules
rg --hidden -n 'TODO|FIXME' -g '!.git/**'

# Filenames, including hidden and gitignored files
rg --files --hidden --no-ignore -g '*TODO*' -g '*FIXME*' -g '!.git/**'
```

## Dependencies

### Rust

| Topic | Commands |
| --- | --- |
| Unused | `cargo machete`; optional compiler analysis: `cargo +nightly udeps -p <crate> --all-targets` with relevant target/features |
| Duplicate versions | `cargo tree --workspace --duplicates --locked`; trace owners: `cargo tree --workspace --invert <crate>@<version> --locked` |
| Deprecated/unmaintained | Registry notices, upstream announcements, advisories, migration docs |

- Removal false positives: generated bindings, build scripts, macros, renamed crates,
  feature-enabling dependencies. Udeps's analysis toolchain does not raise project MSRV.
- Align compatible requirements/features. Don't upgrade the Stacks dependency family
  merely to deduplicate.

### TypeScript

Run separately in root SDK workspace and `components/clarity-vscode`, using each
workspace's pnpm version. Include separately managed examples when in scope.

| Topic | Commands |
| --- | --- |
| Unused dependencies/files/exports | `pnpm exec knip` when available |
| Duplicate versions | `pnpm -r list --depth Infinity`; `pnpm -r why <package>`; `pnpm dedupe --check` |
| Deprecated/unmaintained | `pnpm view <package> deprecated repository --json`; verify upstream status |

- Configure Knip entry points: package exports, scripts/tests, bundler plugins,
  generated WASM imports, extension activation/debug entry points. Local non-use
  does not establish that a published SDK export is dead.
- Distinct peer contexts/incompatible majors may require duplicates. Actual
  `pnpm dedupe` can change resolved versions; inspect lockfile diff.

### Replacement criteria

- Establish maintenance problem; inactivity alone is insufficient. Prefer removal
  or reuse of an existing dependency.
- Compare semantics, formats, features, license, MSRV, native/WASM support; for JS,
  also Node/browser support, exports, ESM/CommonJS, typings, extension bundling.
- Keep upgrades and lockfile churn scoped to the candidate.

Docs: [machete](https://github.com/bnjbvr/cargo-machete),
[udeps](https://github.com/est31/cargo-udeps),
[Knip workspaces](https://knip.dev/features/monorepos-and-workspaces),
[pnpm dedupe](https://pnpm.io/cli/dedupe), [pnpm why](https://pnpm.io/cli/why).

## Dead code, comments, bugs

- Trace compiler warnings and `rg` references across targets, features, generated
  consumers, examples, public interfaces.
- Remove obsolete workarounds; correct comments contradicting current invariants.
- Inspect malformed-input panics, state transitions, failure cleanup at boundaries.
  Derive expected behavior from existing contracts.

## Unit tests

Use coverage reports and assertion review. `cargo cov-dev` produces native Rust HTML
coverage; inspect SDK/WASM tests separately. Prioritize plausible failure modes over
coverage percentage.

| Finding | Action |
| --- | --- |
| Missing coverage | Add boundary, malformed-input, error-path, state-transition, target-specific cases; identify defect each would catch |
| Duplicate tests | Consolidate/remove only when inputs, paths, assertions provide no distinct protection; preserve platform and regression cases |
| Obsolete feature/limitation | Confirm removal from supported contract before changing tests/fixtures; fixed bugs still need regression tests |
| Tautological tests | Replace/remove self-comparisons, expectations derived by tested code, mocks asserting their own setup, language/library trivia without repo invariants |

## Performance

- Candidates: repeated parsing/serialization/I/O; nested linear scans; repeated
  vector removal; long-held locks; Rust clones/allocations; JS array/object copies.
- Search: `rg -n '\.clone\(|\.to_owned\(|\.to_string\(|collect\(' components -g '*.rs'`.
- Trace realistic callers; assess borrowing, reuse, preallocation, algorithm changes.
  Preserve ordering, duplicates, cache identity, error behavior.
- Compare representative release workloads: time, allocations, memory as relevant.
  Retain inputs/results.

## Language and static analysis

- Follow `.agents/rust-conventions.md`. Check MSRV/toolchain/CI before newer Rust
  constructs; unspecified MSRV does not imply latest stable. Check TS output targets
  and Node/browser baselines too.
- After existing checks, explore nursery lints:
  `cargo clippy -p <crate> --tests -- -W clippy::nursery` with relevant target/features.
  Adopt individual useful lints after checking false positives and fixes across
  supported builds; verify crates inherit workspace lints.
- Extend existing extension ESLint with typed rules: `no-floating-promises`,
  `no-misused-promises`, `switch-exhaustiveness-check`; reuse tooling for SDK where
  useful. Configure TS projects; assess false positives, promise semantics, CI cost.

Docs: [Clippy](https://doc.rust-lang.org/clippy/usage.html),
[typed linting](https://typescript-eslint.io/getting-started/typed-linting/).

## Binary size and build performance

- Inspect actual release settings: workspace profiles, `.cargo/config.toml`, scripts,
  CI; native and separately invoked WASM builds may differ.
- Experiment: LTO, codegen units, optimization level, stripping. Measure artifact size,
  runtime, build time, debugging tradeoffs on comparable release builds.
- `target-cpu=native`, `panic=abort`, disabled overflow checks require explicit
  compatibility/behavior scope.

Docs: [Cargo profiles](https://doc.rust-lang.org/cargo/reference/profiles.html).

## Unsafe code and native dependencies

- Locate unsafe/FFI: `rg -n '\bunsafe\b|extern "C"' components -g '*.rs'`.
  Inspect manifests/build scripts for C wrappers, c2rust translations.
- Prefer bounded safe-API/dependency replacements. Apply replacement criteria;
  establish algorithm/format equivalence, especially for crypto/protocol code.

## Structure and types

- Split mixed responsibilities, repeated argument groups, unrelated module contents,
  excessive crate coupling where clarity improves. Preserve native/WASM feature graph.
- Candidates: `Result<_, String>`, boolean/finite-string parameters, invalid flag/option
  combinations. Consider typed errors, enums/discriminated unions, validated newtypes,
  options structs.
- Preserve messages, serialization, defaults, accepted inputs, public signatures.
  Use internal conversions or defer public type migrations.

