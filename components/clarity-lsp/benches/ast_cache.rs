//! Benchmarks for the AST cache.
//!
//! Targets `generate_default_deployment_with_cache` — the function the cache
//! directly affects. Benching the outer `build_state` wraps this in fresh
//! session setup + per-contract REPL analysis, which dominates total
//! latency and drowns out the caching signal.
//!
//! Three scenarios across project sizes:
//!   - `cold`:     no cache (fresh LSP session)
//!   - `all_hit`:  warm cache, no source changes
//!   - `one_miss`: warm cache, one contract's source mutated (N-1 hits + 1 miss)
//!
//! Fixture is synthesized in-memory — no disk I/O. Each contract is
//! ~40 lines of realistic Clarity (constants, data-vars, maps, public +
//! read-only functions) so the parser has meaningful work to skip.

use std::collections::HashMap;
use std::hint::black_box;
use std::path::PathBuf;

use clarinet_deployments::{generate_default_deployment_with_cache, CachedContractAST};
use clarinet_files::{FileAccessor, FileAccessorResult, ProjectManifest, StacksNetwork};
use clarity_repl::utils::Environment;
use divan::Bencher;
use tokio::runtime::Runtime;

const MANIFEST_PATH: &str = "/project/Clarinet.toml";
const DEVNET_PATH: &str = "/project/settings/Devnet.toml";

/// Header that establishes identity (so each contract differs by content
/// hash) and a handful of realistic constants / storage declarations.
const CONTRACT_HEADER: &str = r#"
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-UNKNOWN-USER (err u102))
(define-constant ERR-PAUSED (err u103))
(define-constant ERR-OVERFLOW (err u104))
(define-constant CONTRACT-ID u{i})
(define-constant OWNER tx-sender)

(define-data-var total-supply uint u0)
(define-data-var paused bool false)
(define-data-var admin principal tx-sender)

(define-map balances principal uint)
(define-map allowances { owner: principal, spender: principal } uint)
(define-map history { block: uint, owner: principal } { amount: uint, op: (string-ascii 16) })
"#;

/// A block of function definitions; repeated a few times per contract to
/// give the parser something substantial to chew through. `{f}` is
/// substituted with a suffix so every function name stays unique.
const CONTRACT_FUNCTIONS: &str = r#"
(define-read-only (get-balance-{f} (owner principal))
  (default-to u0 (map-get? balances owner))
)

(define-read-only (get-allowance-{f} (owner principal) (spender principal))
  (default-to u0 (map-get? allowances { owner: owner, spender: spender }))
)

(define-private (record-{f} (who principal) (amount uint) (op (string-ascii 16)))
  (map-set history
    { block: stacks-block-height, owner: who }
    { amount: amount, op: op }
  )
)

(define-public (transfer-{f} (amount uint) (recipient principal))
  (let ((sender-balance (get-balance-{f} tx-sender)))
    (asserts! (not (var-get paused)) ERR-PAUSED)
    (asserts! (>= sender-balance amount) ERR-INVALID-AMOUNT)
    (map-set balances tx-sender (- sender-balance amount))
    (map-set balances recipient (+ (get-balance-{f} recipient) amount))
    (record-{f} tx-sender amount "transfer")
    (ok true)
  )
)

(define-public (approve-{f} (spender principal) (amount uint))
  (begin
    (map-set allowances { owner: tx-sender, spender: spender } amount)
    (ok true)
  )
)

(define-public (transfer-from-{f} (amount uint) (owner principal) (recipient principal))
  (let (
    (allowance (get-allowance-{f} owner tx-sender))
    (owner-balance (get-balance-{f} owner))
  )
    (asserts! (not (var-get paused)) ERR-PAUSED)
    (asserts! (>= allowance amount) ERR-NOT-AUTHORIZED)
    (asserts! (>= owner-balance amount) ERR-INVALID-AMOUNT)
    (map-set allowances { owner: owner, spender: tx-sender } (- allowance amount))
    (map-set balances owner (- owner-balance amount))
    (map-set balances recipient (+ (get-balance-{f} recipient) amount))
    (record-{f} owner amount "transferFrom")
    (ok true)
  )
)

(define-public (mint-{f} (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
    (var-set total-supply (+ (var-get total-supply) amount))
    (map-set balances recipient (+ (get-balance-{f} recipient) amount))
    (record-{f} recipient amount "mint")
    (ok true)
  )
)

(define-public (burn-{f} (amount uint))
  (let ((owner-balance (get-balance-{f} tx-sender)))
    (asserts! (>= owner-balance amount) ERR-INVALID-AMOUNT)
    (map-set balances tx-sender (- owner-balance amount))
    (var-set total-supply (- (var-get total-supply) amount))
    (record-{f} tx-sender amount "burn")
    (ok true)
  )
)
"#;

/// Number of function blocks per contract. Each block is ~60 lines, so a
/// contract ends up ~400 lines — large enough that parse time is no longer
/// lost in per-call fixed overhead.
const FUNCTIONS_PER_CONTRACT: usize = 6;

const DEVNET_TOML: &str = r#"
[network]
name = "devnet"
deployment_fee_rate = 10

[accounts.deployer]
mnemonic = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw"
balance = 100_000_000_000_000
sbtc_balance = 1_000_000_000
"#;

fn build_manifest(n: usize) -> String {
    let mut out = String::from("[project]\nname = 'bench-project'\ntelemetry = false\n\n");
    for i in 0..n {
        out.push_str(&format!(
            "[contracts.contract_{i}]\npath = 'contracts/contract_{i}.clar'\nepoch = 'latest'\nclarity_version = 3\n\n"
        ));
    }
    out.push_str("[repl.analysis.lint_groups]\nall = false\n");
    out
}

fn contract_source(i: usize, salt: &str) -> String {
    // `salt` lets the caller distinguish the "modified" fixture used by
    // `one_miss` — changing it invalidates the content hash for that file.
    let mut out = CONTRACT_HEADER.replace("{i}", &format!("{i}{salt}"));
    for f in 0..FUNCTIONS_PER_CONTRACT {
        out.push_str(&CONTRACT_FUNCTIONS.replace("{f}", &format!("{f}")));
    }
    out
}

#[derive(Clone)]
struct BenchFileAccessor {
    files: HashMap<String, String>,
}

impl BenchFileAccessor {
    fn new(n: usize, modified_index: Option<usize>) -> Self {
        let mut files = HashMap::new();
        files.insert(MANIFEST_PATH.to_string(), build_manifest(n));
        files.insert(DEVNET_PATH.to_string(), DEVNET_TOML.to_string());
        for i in 0..n {
            let salt = if Some(i) == modified_index {
                "_modified"
            } else {
                ""
            };
            files.insert(
                format!("/project/contracts/contract_{i}.clar"),
                contract_source(i, salt),
            );
        }
        Self { files }
    }
}

impl FileAccessor for BenchFileAccessor {
    fn file_exists(&self, path: String) -> FileAccessorResult<bool> {
        let exists = self.files.contains_key(&path);
        Box::pin(async move { Ok(exists) })
    }

    fn read_file(&self, path: String) -> FileAccessorResult<String> {
        let result = self
            .files
            .get(&path)
            .cloned()
            .ok_or_else(|| format!("bench fixture missing: {path}"));
        Box::pin(async move { result })
    }

    fn read_files(&self, paths: Vec<String>) -> FileAccessorResult<HashMap<String, String>> {
        let mut out = HashMap::with_capacity(paths.len());
        for p in paths {
            if let Some(c) = self.files.get(&p) {
                out.insert(p, c.clone());
            }
        }
        Box::pin(async move { Ok(out) })
    }

    fn write_file(&self, _: String, _: &[u8]) -> FileAccessorResult<()> {
        Box::pin(async { Ok(()) })
    }
}

fn runtime() -> Runtime {
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap()
}

/// Fetch a manifest from the accessor. Not part of anything we want to time,
/// and the returned manifest is re-used across all iterations in a bench.
fn load_manifest(rt: &Runtime, accessor: &BenchFileAccessor) -> ProjectManifest {
    rt.block_on(async {
        ProjectManifest::from_file_accessor(std::path::Path::new(MANIFEST_PATH), false, accessor)
            .await
            .expect("fixture manifest should parse")
    })
}

/// Run one `generate_default_deployment_with_cache`, returning the cache
/// entries it produced.
fn run(
    rt: &Runtime,
    manifest: &ProjectManifest,
    accessor: &BenchFileAccessor,
    mut cache: Option<HashMap<(PathBuf, Environment), CachedContractAST>>,
) -> HashMap<(PathBuf, Environment), CachedContractAST> {
    rt.block_on(async {
        let (_, artifacts, _) = generate_default_deployment_with_cache(
            manifest,
            &StacksNetwork::Simnet,
            false,
            Some(accessor),
            None,
            Environment::OnChain,
            cache.as_mut(),
        )
        .await
        .expect("fixture deployment should succeed");
        artifacts.ast_cache_entries
    })
}

fn warm_cache(
    rt: &Runtime,
    manifest: &ProjectManifest,
    accessor: &BenchFileAccessor,
) -> HashMap<(PathBuf, Environment), CachedContractAST> {
    run(rt, manifest, accessor, None)
}

// ---- benches ----

/// Fresh session: no cache entries to validate, every contract parses.
#[divan::bench(args = [1, 5, 20], sample_count = 30)]
fn cold(bencher: Bencher, n: usize) {
    let rt = runtime();
    let accessor = BenchFileAccessor::new(n, None);
    let manifest = load_manifest(&rt, &accessor);
    bencher.bench_local(|| {
        black_box(run(&rt, &manifest, black_box(&accessor), None));
    });
}

/// Repeated save with nothing changed — every contract hits the cache.
#[divan::bench(args = [1, 5, 20], sample_count = 30)]
fn all_hit(bencher: Bencher, n: usize) {
    let rt = runtime();
    let accessor = BenchFileAccessor::new(n, None);
    let manifest = load_manifest(&rt, &accessor);
    let warm = warm_cache(&rt, &manifest, &accessor);
    bencher.with_inputs(|| warm.clone()).bench_values(|cache| {
        black_box(run(&rt, &manifest, black_box(&accessor), Some(cache)));
    });
}

/// Typical "user edited one file" save: N-1 cache hits + 1 miss.
#[divan::bench(args = [1, 5, 20], sample_count = 30)]
fn one_miss(bencher: Bencher, n: usize) {
    let rt = runtime();
    // Warm the cache from the *original* fixture...
    let original = BenchFileAccessor::new(n, None);
    let manifest = load_manifest(&rt, &original);
    let warm = warm_cache(&rt, &manifest, &original);
    // ...but save against the *modified* fixture, so contract 0 misses.
    let modified = BenchFileAccessor::new(n, Some(0));
    bencher.with_inputs(|| warm.clone()).bench_values(|cache| {
        black_box(run(&rt, &manifest, black_box(&modified), Some(cache)));
    });
}

fn main() {
    divan::main();
}
