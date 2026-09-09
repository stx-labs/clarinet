//! Regression coverage for gaps in requirement auto-detection.
//!
//! Each test below pins one behavior that the auto-detection path gets wrong.
//! They are written to pass once the described fix is applied, so they fail on
//! the current implementation — that is the point of them.

use std::fs;
use std::path::Path;

use clarinet_deployments::generate_default_deployment;
use clarinet_deployments::types::TransactionSpecification;
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity_repl::utils::Environment;
use indoc::formatdoc;
use mockito::{Server, ServerGuard};
use tempfile::TempDir;

/// Well-known Clarinet test mnemonic, matching the generated settings files.
const TEST_MNEMONIC: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";

/// An arbitrary third-party deployer, the way a project would spell it.
const EXTERNAL_DEPLOYER: &str = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9";

/// sBTC's mainnet deployer.
const SBTC_MAINNET_DEPLOYER: &str = "SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4";

/// Stand-in for a real requirement's body. Dependency-free, to keep the tests
/// fast and independent of any real contract's own requirements.
const PLAIN_SOURCE: &str = "(define-read-only (get-one) (ok u1))";

/// Write `settings/Testnet.toml` plus a manifest declaring a single contract at
/// `contracts/caller.clar`, with `requirements` set from `requirements_toml`.
fn write_project(root: &Path, contract_source: &str, requirements_toml: &str) {
    fs::create_dir_all(root.join("settings")).unwrap();
    fs::create_dir_all(root.join("contracts")).unwrap();

    fs::write(
        root.join("settings/Testnet.toml"),
        formatdoc!(
            r#"
            [network]
            name = "testnet"
            deployment_fee_rate = 10

            [accounts.deployer]
            mnemonic = "{TEST_MNEMONIC}"
            "#
        ),
    )
    .unwrap();

    fs::write(
        root.join("Clarinet.toml"),
        formatdoc!(
            r#"
            [project]
            name = "auto-detection-test"
            authors = []
            description = ""
            telemetry = false
            {requirements_toml}

            [contracts.caller]
            path = "contracts/caller.clar"
            clarity_version = 3
            epoch = "3.0"
            "#
        ),
    )
    .unwrap();

    fs::write(root.join("contracts/caller.clar"), contract_source).unwrap();
}

/// Serve `deployer.name` -> `source` for each entry, so the tests stay offline.
async fn mock_contracts(entries: &[(&str, &str, &str)]) -> ServerGuard {
    let mut server = Server::new_async().await;
    for (deployer, name, source) in entries {
        server
            .mock(
                "GET",
                format!("/extended/v1/contract/{deployer}.{name}").as_str(),
            )
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                serde_json::json!({
                    "source_code": source,
                    "block_height": 175232,
                    "clarity_version": 3
                })
                .to_string(),
            )
            // Auto-detection may or may not reach a given contract; a mock that
            // goes unused must not fail the test.
            .expect_at_least(0)
            .create_async()
            .await;
    }
    server
}

/// The `contract-id`s of every `RequirementPublish` in a generated testnet plan.
async fn testnet_requirement_publishes(root: &Path, api_url: &str) -> Vec<String> {
    let manifest = ProjectManifest::from_location(&root.join("Clarinet.toml"), false).unwrap();
    let (deployment, _artifacts, _) = generate_default_deployment(
        &manifest,
        &StacksNetwork::Testnet,
        false,
        None,
        Some(api_url),
        Environment::OnChain,
    )
    .await
    .expect("testnet deployment plan should be generated");

    deployment
        .plan
        .batches
        .iter()
        .flat_map(|batch| &batch.transactions)
        .filter_map(|tx| match tx {
            TransactionSpecification::RequirementPublish(spec) => {
                Some(spec.contract_id.to_string())
            }
            _ => None,
        })
        .collect()
}

/// Issue 1: trait contracts referenced by `impl-trait` / `use-trait` are not
/// auto-detected, so they never reach the deployment plan.
///
/// `ASTDependencyDetector::detect_dependencies` returns a map keyed by *the
/// contract doing the referencing*, with the contracts it depends on in the
/// `DependencySet` values. `generate_default_deployment_with_cache` reads
/// `dependencies.keys()`, which yields the project's own contracts — and those
/// are then dropped by the `user_contract_ids` filter, so the `Ok` branch
/// contributes nothing at all.
///
/// Only unresolvable references survive, via the error's `non_inferable` field.
/// That is why `contract-call?` to an unknown contract works (it registers a
/// pending function check) while `impl-trait` / `use-trait` do not: they call
/// `add_dependency` and register nothing pending, so detection returns `Ok` and
/// the trait is silently lost. Trait contracts (SIP-009/010) are the most
/// common thing a project lists in `[[project.requirements]]`.
///
/// Fix: read the dependencies out of the values rather than the keys, and keep
/// the unresolvable ones —
///
/// ```ignore
/// let (inferable, non_inferable) = match ASTDependencyDetector::detect_dependencies(..) {
///     Ok(inferable) => (inferable, Vec::new()),
///     Err((inferable, non_inferable)) => (inferable, non_inferable),
/// };
/// let auto_detected: Vec<QualifiedContractIdentifier> = inferable
///     .values()
///     .flat_map(|deps| deps.iter().map(|dep| dep.contract_id.clone()))
///     .chain(non_inferable)
///     .filter(|id| !boot_contracts_ids.contains(id))
///     .collect();
/// ```
///
/// The `boot_contracts_ids` filter becomes necessary once the values are read:
/// boot contracts appear in dependency sets, because `add_dependency` only
/// skips preloaded *sources*, not preloaded targets.
#[tokio::test]
async fn issue_1_trait_references_are_auto_detected() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    // Neither trait is listed in [[project.requirements]].
    write_project(
        root,
        &format!(
            "(impl-trait '{EXTERNAL_DEPLOYER}.nft-trait.nft-trait)\n\
             (use-trait ft '{EXTERNAL_DEPLOYER}.ft-trait.sip-010-trait)\n\
             (define-read-only (get-owner (id uint)) (ok none))\n"
        ),
        "",
    );

    let server = mock_contracts(&[
        (
            EXTERNAL_DEPLOYER,
            "nft-trait",
            "(define-trait nft-trait ((get-owner (uint) (response (optional principal) uint))))",
        ),
        (
            EXTERNAL_DEPLOYER,
            "ft-trait",
            "(define-trait sip-010-trait ((transfer (uint principal principal) (response bool uint))))",
        ),
    ])
    .await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    for name in ["nft-trait", "ft-trait"] {
        assert!(
            published.contains(&format!("{EXTERNAL_DEPLOYER}.{name}")),
            "{name} is referenced by the project and should be auto-detected and \
             published as a requirement; got {published:?}"
        );
    }
}

/// Issue 2: a reference to a contract that cannot be fetched aborts the whole
/// deployment plan, instead of being reported against the offending line.
///
/// Auto-detected requirements go through the same `retrieve_contract(..).await?`
/// as explicit ones, so a typo'd principal, a contract that does not exist, or
/// simply being offline turns into a hard `Err` out of
/// `generate_default_deployment`. Before auto-detection this could only happen
/// for a contract the user had explicitly declared.
///
/// The LSP calls this on every rebuild, once per `CHECK_ENVIRONMENTS` entry, so
/// the failure means `build_state` returns `Err` and the user loses *every*
/// diagnostic in the project — replaced by an opaque HTTP error with no
/// location. On `main` the same contract yields a plan plus a normal analysis
/// diagnostic on the line holding the bad reference.
///
/// Fix: distinguish the two sources. A fetch failure for an *explicit*
/// requirement should stay fatal — the user declared it, so it is a real error.
/// A failure for an *auto-detected* one should drop that contract from the
/// queue and let the analysis pass report the bad reference where it lives.
/// That needs the queue to carry the requirement's origin, which it currently
/// does not.
#[tokio::test]
async fn issue_2_unresolvable_reference_does_not_abort_the_plan() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &format!("(define-public (go) (contract-call? '{EXTERNAL_DEPLOYER}.typo get-x))\n"),
        "",
    );

    // `.typo` does not exist on chain: the API answers 404, as it would for a
    // mistyped principal or contract name.
    let mut server = Server::new_async().await;
    server
        .mock(
            "GET",
            format!("/extended/v1/contract/{EXTERNAL_DEPLOYER}.typo").as_str(),
        )
        .with_status(404)
        .create_async()
        .await;

    let manifest = ProjectManifest::from_location(&root.join("Clarinet.toml"), false).unwrap();
    let result = generate_default_deployment(
        &manifest,
        &StacksNetwork::Testnet,
        false,
        None,
        Some(&server.url()),
        Environment::OnChain,
    )
    .await;

    assert!(
        result.is_ok(),
        "an unresolvable auto-detected reference must not abort plan generation, \
         it should be left to the analysis pass to report; got {:?}",
        result.err()
    );
}

/// Issue 3: dependencies referenced only from `#[env(simnet)]` code are
/// auto-detected and published to real networks.
///
/// The early AST pass that feeds auto-detection reads each contract's source
/// verbatim. The later pass that builds the publish specifications strips
/// simnet-only code first:
///
/// ```ignore
/// if environment == Environment::OnChain {
///     if let Ok(Some(clean)) = remove_env_simnet(&source) { source = clean; .. }
/// }
/// ```
///
/// Because the early pass skips that step, a mock contract referenced only from
/// test-only code becomes a requirement and is published to testnet or mainnet.
///
/// Fix: apply the same `remove_env_simnet` guard to the source before building
/// the AST used for detection.
#[tokio::test]
async fn issue_3_env_simnet_dependencies_stay_off_chain() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &format!(
            "(define-public (real) (ok true))\n\
             ;; #[env(simnet)]\n\
             (define-public (test-only) (contract-call? '{EXTERNAL_DEPLOYER}.mock get-one))\n"
        ),
        "",
    );

    let server = mock_contracts(&[(EXTERNAL_DEPLOYER, "mock", PLAIN_SOURCE)]).await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    assert!(
        !published.contains(&format!("{EXTERNAL_DEPLOYER}.mock")),
        "a dependency referenced only from #[env(simnet)] code must not be \
         published to testnet; got {published:?}"
    );
}

/// Issue 6: declaring `sbtc-token` as a requirement silently pulls in
/// `sbtc-deposit` as well.
///
/// The requirements block gained a rule with no counterpart on `main`:
///
/// ```ignore
/// if has_sbtc_token && !has_sbtc_deposit {
///     queue.push_front(QualifiedContractIdentifier::parse(&sbtc_deposit_str).unwrap());
/// }
/// ```
///
/// A project that reads sBTC balances has no need of the deposit contract, and
/// nothing in the PR explains the rule. Its `auto_detected` half is also
/// unreachable: `sbtc-token` is preloaded into `requirements_data`, so calls to
/// it resolve and never reach `non_inferable`, which (per issue 1) is the only
/// channel auto-detection currently reads. So the rule can only ever fire from
/// an *explicit* `sbtc-token` requirement, which is what this test sets up.
///
/// Fix: drop the rule, or document what it works around and cover it with a
/// test of its own.
#[tokio::test]
async fn issue_6_sbtc_token_requirement_does_not_pull_in_sbtc_deposit() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        "(define-read-only (noop) (ok true))\n",
        &formatdoc!(
            r#"

            [[project.requirements]]
            contract_id = "{SBTC_MAINNET_DEPLOYER}.sbtc-token"
            "#
        ),
    );

    let server = mock_contracts(&[
        (SBTC_MAINNET_DEPLOYER, "sbtc-token", PLAIN_SOURCE),
        (SBTC_MAINNET_DEPLOYER, "sbtc-deposit", PLAIN_SOURCE),
    ])
    .await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    assert!(
        !published.contains(&format!("{SBTC_MAINNET_DEPLOYER}.sbtc-deposit")),
        "only the declared sbtc-token requirement should be published, but \
         sbtc-deposit was added too; got {published:?}"
    );
}
