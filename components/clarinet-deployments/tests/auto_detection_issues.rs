//! Regression tests for requirement auto-detection.

use std::fs;
use std::path::Path;

use clarinet_deployments::generate_default_deployment;
use clarinet_deployments::types::TransactionSpecification;
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity_repl::utils::Environment;
use indoc::formatdoc;
use mockito::{Server, ServerGuard};
use tempfile::TempDir;

/// Mnemonic used by generated test settings.
const TEST_MNEMONIC: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";

/// External contract deployer used by test fixtures.
const EXTERNAL_DEPLOYER: &str = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9";

/// Mainnet sBTC deployer.
const SBTC_MAINNET_DEPLOYER: &str = "SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4";

/// Dependency-free requirement source.
const PLAIN_SOURCE: &str = "(define-read-only (get-one) (ok u1))";

/// Write a project with one contract and the supplied requirements.
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

/// Serve contract sources from a local mock API.
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

/// Return requirement contract IDs from a generated testnet plan.
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

/// Trait references are included as requirements.
#[tokio::test]
async fn trait_references_are_auto_detected() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    // Neither trait is declared explicitly.
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

/// Unresolvable inferred references are left for contract analysis to report.
#[tokio::test]
async fn unresolvable_reference_does_not_abort_the_plan() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &format!("(define-public (go) (contract-call? '{EXTERNAL_DEPLOYER}.typo get-x))\n"),
        "",
    );

    // Simulate a contract that does not exist.
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

/// Simnet-only references are excluded from on-chain deployment plans.
#[tokio::test]
async fn env_simnet_dependencies_stay_off_chain() {
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

/// Loading an external callee's signature must reveal its trait argument dependencies.
///
/// The initial scan discovers `callee`, but cannot identify `implementation` as
/// a dependency until it knows that `take` accepts a trait argument. Loading
/// `callee` does not currently trigger another scan of the user contract, so the
/// generated plan omits `implementation`.
///
/// Re-scan user contracts after loading requirements, repeating discovery and
/// loading until no new dependencies are found. Track failed resolutions too,
/// so an unavailable dependency cannot keep this process running indefinitely.
#[tokio::test]
async fn external_trait_argument_is_auto_detected_after_loading_callee() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    // Neither external contract is explicitly declared. The type of the second
    // contract literal is only known once the callee's source has been loaded.
    write_project(
        root,
        &formatdoc!(
            "
            (define-public (go)
              (contract-call? '{EXTERNAL_DEPLOYER}.callee take
                '{EXTERNAL_DEPLOYER}.implementation))
            "
        ),
        "",
    );

    let server = mock_contracts(&[
        (
            EXTERNAL_DEPLOYER,
            "callee",
            "(define-trait reader ((get-one () (response uint uint))))
             (define-public (take (target <reader>))
               (contract-call? target get-one))",
        ),
        (
            EXTERNAL_DEPLOYER,
            "implementation",
            "(impl-trait .callee.reader)
             (define-read-only (get-one) (ok u1))",
        ),
    ])
    .await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    assert!(
        published.contains(&format!("{EXTERNAL_DEPLOYER}.callee")),
        "the directly called external contract should be published; got {published:?}"
    );
    assert!(
        published.contains(&format!("{EXTERNAL_DEPLOYER}.implementation")),
        "loading the callee should reveal that its trait argument is another \
         requirement to publish; got {published:?}"
    );
}

/// Declaring an sBTC token requirement does not imply unrelated requirements.
#[tokio::test]
async fn sbtc_token_requirement_does_not_pull_in_sbtc_deposit() {
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
