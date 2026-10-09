//! Regression tests for requirement auto-detection.

use std::fs;
use std::path::Path;

use clarinet_deployments::generate_default_deployment;
use clarinet_deployments::types::TransactionSpecification;
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarinet_utils::DEFAULT_DEPLOYER_MNEMONIC as TEST_MNEMONIC;
use clarity_repl::repl::boot::SBTC_MAINNET_ADDRESS;
use clarity_repl::utils::Environment;
use indoc::formatdoc;
use mockito::{Server, ServerGuard};
use tempfile::TempDir;

/// External contract deployer used by test fixtures.
const EXTERNAL_DEPLOYER: &str = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9";

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
            cache_dir = "./.cache"
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

/// An unresolvable dependency of a loaded requirement must not loop forever.
///
/// `callee` is auto-detected and retrieved, but the contract it calls cannot be.
/// Resolution must stop retrying the missing contract (and re-enqueueing
/// `callee`) instead of spinning indefinitely.
#[tokio::test]
async fn unresolvable_dependency_of_requirement_terminates() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &format!("(define-public (go) (contract-call? '{EXTERNAL_DEPLOYER}.callee get-one))\n"),
        "",
    );

    let callee_source =
        format!("(define-read-only (get-one) (contract-call? '{EXTERNAL_DEPLOYER}.missing nope))");
    let mut server = mock_contracts(&[(EXTERNAL_DEPLOYER, "callee", callee_source.as_str())]).await;
    server
        .mock(
            "GET",
            format!("/extended/v1/contract/{EXTERNAL_DEPLOYER}.missing").as_str(),
        )
        .with_status(404)
        .create_async()
        .await;

    let manifest = ProjectManifest::from_location(&root.join("Clarinet.toml"), false).unwrap();
    let result = tokio::time::timeout(
        std::time::Duration::from_secs(10),
        generate_default_deployment(
            &manifest,
            &StacksNetwork::Testnet,
            false,
            None,
            Some(&server.url()),
            Environment::OnChain,
        ),
    )
    .await;

    assert!(
        result.is_ok(),
        "resolution must terminate instead of re-enqueueing an unresolvable dependency"
    );
    assert!(
        result.unwrap().is_ok(),
        "an unresolvable dependency of a loaded requirement must not abort the plan"
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

    // The mock endpoint must never be called — a simnet-only reference must
    // not trigger a network fetch for an on-chain deployment.
    let mut server = Server::new_async().await;
    let mock_request = server
        .mock(
            "GET",
            format!("/extended/v1/contract/{EXTERNAL_DEPLOYER}.mock").as_str(),
        )
        .expect(0)
        .create_async()
        .await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    mock_request.assert_async().await;

    assert!(
        !published.contains(&format!("{EXTERNAL_DEPLOYER}.mock")),
        "a dependency referenced only from #[env(simnet)] code must not be \
         published to testnet; got {published:?}"
    );
}

/// Loading an external callee's signature reveals its trait argument
/// dependencies: `implementation` is only identifiable once `take` is known to
/// accept a trait.
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

/// Loading an explicitly declared callee's signature also reveals its trait
/// argument dependencies.
#[tokio::test]
async fn trait_argument_is_auto_detected_after_loading_explicit_callee() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &formatdoc!(
            "
            (define-public (go)
              (contract-call? '{EXTERNAL_DEPLOYER}.callee take
                '{EXTERNAL_DEPLOYER}.implementation))
            "
        ),
        &formatdoc!(
            r#"

            [[project.requirements]]
            contract_id = "{EXTERNAL_DEPLOYER}.callee"
            "#
        ),
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
        "the declared requirement should be published; got {published:?}"
    );
    assert!(
        published.contains(&format!("{EXTERNAL_DEPLOYER}.implementation")),
        "loading the explicitly declared callee should reveal its trait \
         argument as another requirement to publish; got {published:?}"
    );
}

/// A contract that appears in `[[project.requirements]]` AND is referenced by
/// user code via `use-trait` must appear exactly once in the plan.
#[tokio::test]
async fn explicit_requirement_not_duplicated_when_also_auto_detected() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &format!(
            "(use-trait nft '{EXTERNAL_DEPLOYER}.nft-trait.nft-trait)\n\
             (define-read-only (noop) (ok none))\n"
        ),
        &formatdoc!(
            r#"
            [[project.requirements]]
            contract_id = "{EXTERNAL_DEPLOYER}.nft-trait"
            "#
        ),
    );

    let server = mock_contracts(&[(
        EXTERNAL_DEPLOYER,
        "nft-trait",
        "(define-trait nft-trait ((get-owner (uint) (response (optional principal) uint))))",
    )])
    .await;

    let published = testnet_requirement_publishes(root, &server.url()).await;
    let count = published
        .iter()
        .filter(|id| *id == &format!("{EXTERNAL_DEPLOYER}.nft-trait"))
        .count();
    assert_eq!(
        count, 1,
        "nft-trait must appear exactly once even when both explicitly declared and \
         auto-detected from user code; got {published:?}"
    );
}

/// When the same contract is in `[[project.requirements]]` AND auto-detected,
/// a fetch failure is a hard error (not silently skipped). Explicit entries
/// promote auto-detected contracts to required: if the address is wrong or the
/// contract is unavailable, plan generation fails rather than continuing without it.
#[tokio::test]
async fn explicit_requirement_fetch_failure_aborts_plan() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &format!(
            "(use-trait nft '{EXTERNAL_DEPLOYER}.nft-trait.nft-trait)\n\
             (define-read-only (noop) (ok none))\n"
        ),
        &formatdoc!(
            r#"
            [[project.requirements]]
            contract_id = "{EXTERNAL_DEPLOYER}.nft-trait"
            "#
        ),
    );

    // Server returns 404 — the contract cannot be retrieved.
    let mut server = Server::new_async().await;
    server
        .mock(
            "GET",
            format!("/extended/v1/contract/{EXTERNAL_DEPLOYER}.nft-trait").as_str(),
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
        result.is_err(),
        "an explicit requirement that cannot be fetched must abort plan generation, \
         not be silently skipped like an auto-detected one; got Ok"
    );
}

/// An explicit `[[project.requirements]]` entry must not prevent auto-detection
/// of other externally-referenced contracts.
#[tokio::test]
async fn explicit_requirement_does_not_suppress_auto_detection_of_others() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    // nft-trait is explicit; ft-trait is only auto-detected via use-trait.
    write_project(
        root,
        &format!(
            "(use-trait ft '{EXTERNAL_DEPLOYER}.ft-trait.sip-010-trait)\n\
             (define-read-only (noop) (ok none))\n"
        ),
        &formatdoc!(
            r#"
            [[project.requirements]]
            contract_id = "{EXTERNAL_DEPLOYER}.nft-trait"
            "#
        ),
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

    assert!(
        published.contains(&format!("{EXTERNAL_DEPLOYER}.nft-trait")),
        "the explicitly declared nft-trait should be published; got {published:?}"
    );
    assert!(
        published.contains(&format!("{EXTERNAL_DEPLOYER}.ft-trait")),
        "ft-trait referenced by user code must be auto-detected even though \
         a different explicit requirement is also declared; got {published:?}"
    );
}

/// On simnet, an explicit requirement and an auto-detected requirement (both
/// loaded from the local cache) must both appear in the generated plan.
#[tokio::test]
async fn simnet_explicit_and_auto_detected_requirements_both_deployed() {
    const SECOND_DEPLOYER: &str = "SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9";

    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    // external-a is explicit; external-b is only auto-detected via contract-call?.
    write_project(
        root,
        &formatdoc!(
            "
            (define-public (go)
              (contract-call? '{SECOND_DEPLOYER}.external-b get-one))
            "
        ),
        &formatdoc!(
            r#"
            [[project.requirements]]
            contract_id = "{EXTERNAL_DEPLOYER}.external-a"
            "#
        ),
    );

    // Simnet generation reads Devnet.toml for network settings.
    fs::write(
        root.join("settings/Devnet.toml"),
        formatdoc!(
            r#"
            [network]
            name = "devnet"
            deployment_fee_rate = 10

            [accounts.deployer]
            mnemonic = "{TEST_MNEMONIC}"
            balance = 100_000_000_000_000
            "#
        ),
    )
    .unwrap();

    // Write cache files so simnet generation can load both without network access.
    // Use string concatenation (not PathBuf::with_extension) since the contract
    // name may contain dots that would be misinterpreted as file extensions.
    let cache = root.join(".cache/requirements");
    fs::create_dir_all(&cache).unwrap();
    for (deployer, name) in [
        (EXTERNAL_DEPLOYER, "external-a"),
        (SECOND_DEPLOYER, "external-b"),
    ] {
        let stem = format!("{deployer}.{name}");
        fs::write(cache.join(format!("{stem}.clar")), PLAIN_SOURCE).unwrap();
        fs::write(
            cache.join(format!("{stem}.json")),
            r#"{"epoch":"Epoch30","clarity_version":"Clarity3"}"#,
        )
        .unwrap();
    }

    let manifest = ProjectManifest::from_location(&root.join("Clarinet.toml"), false).unwrap();
    let (deployment, _, _) = generate_default_deployment(
        &manifest,
        &StacksNetwork::Simnet,
        false,
        None,
        None,
        Environment::Simnet,
    )
    .await
    .expect("simnet deployment plan should be generated");

    let emulated: Vec<String> = deployment
        .plan
        .batches
        .iter()
        .flat_map(|b| &b.transactions)
        .filter_map(|tx| match tx {
            TransactionSpecification::EmulatedContractPublish(spec) => {
                Some(format!("{}.{}", spec.emulated_sender, spec.contract_name))
            }
            _ => None,
        })
        .collect();

    assert!(
        emulated.contains(&format!("{EXTERNAL_DEPLOYER}.external-a")),
        "the explicitly declared external-a should be in the simnet plan; got {emulated:?}"
    );
    assert!(
        emulated.contains(&format!("{SECOND_DEPLOYER}.external-b")),
        "the auto-detected external-b should be in the simnet plan; got {emulated:?}"
    );
}

/// An auto-detected reference to an sBTC contract must produce a
/// RequirementPublish on testnet.
#[tokio::test]
async fn auto_detected_sbtc_is_published_on_testnet() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    // Reference sbtc-token without any explicit [[project.requirements]] entry.
    write_project(
        root,
        &formatdoc!(
            "
            (define-read-only (balance)
              (contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-token get-balance tx-sender))
            "
        ),
        "",
    );

    let server = mock_contracts(&[(SBTC_MAINNET_ADDRESS, "sbtc-token", PLAIN_SOURCE)]).await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    assert!(
        published.contains(&format!("{SBTC_MAINNET_ADDRESS}.sbtc-token")),
        "auto-detected sbtc-token must be published on testnet; got {published:?}"
    );
}

/// A direct `contract-call?` (not trait-mediated) is auto-detected and
/// published as a requirement.
#[tokio::test]
async fn direct_contract_call_is_auto_detected() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &format!(
            "(define-public (call-ext) \
             (contract-call? '{EXTERNAL_DEPLOYER}.helper get-one))"
        ),
        "",
    );

    let server = mock_contracts(&[(EXTERNAL_DEPLOYER, "helper", PLAIN_SOURCE)]).await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    assert!(
        published.contains(&format!("{EXTERNAL_DEPLOYER}.helper")),
        "a directly called external contract must be auto-detected and published \
         as a requirement; got {published:?}"
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
            contract_id = "{SBTC_MAINNET_ADDRESS}.sbtc-token"
            "#
        ),
    );

    let mut server = mock_contracts(&[(SBTC_MAINNET_ADDRESS, "sbtc-token", PLAIN_SOURCE)]).await;
    // sbtc-deposit must never be fetched — declaring sbtc-token must not
    // implicitly pull in unrelated sBTC contracts.
    let deposit_request = server
        .mock(
            "GET",
            format!("/extended/v1/contract/{SBTC_MAINNET_ADDRESS}.sbtc-deposit").as_str(),
        )
        .expect(0)
        .create_async()
        .await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    deposit_request.assert_async().await;

    assert!(
        !published.contains(&format!("{SBTC_MAINNET_ADDRESS}.sbtc-deposit")),
        "only the declared sbtc-token requirement should be published, but \
         sbtc-deposit was added too; got {published:?}"
    );
}

/// `contract-hash?` with a literal principal is a static dependency.
/// `contract-hash?` requires Clarity 4 (Epoch 3.3+).
#[tokio::test]
async fn contract_hash_literal_is_auto_detected() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    // write_project hardcodes Clarity 3 / Epoch 3.0; write the manifest manually
    // so we can use Clarity 4 / Epoch 3.3, where contract-hash? is available.
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
            name = "contract-hash-test"
            authors = []
            description = ""
            telemetry = false
            cache_dir = "./.cache"

            [contracts.caller]
            path = "contracts/caller.clar"
            clarity_version = 4
            epoch = "3.3"
            "#
        ),
    )
    .unwrap();

    fs::write(
        root.join("contracts/caller.clar"),
        format!("(define-read-only (hash) (contract-hash? '{EXTERNAL_DEPLOYER}.hasher))"),
    )
    .unwrap();

    let server = mock_contracts(&[(EXTERNAL_DEPLOYER, "hasher", PLAIN_SOURCE)]).await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    assert!(
        published.contains(&format!("{EXTERNAL_DEPLOYER}.hasher")),
        "a contract referenced via contract-hash? with a literal principal must \
         be auto-detected and published as a requirement; got {published:?}"
    );
}

/// A reference under a project account names a contract missing from the
/// manifest. The devnet keys are public, so fetching it would pull in whatever
/// a third party deployed under them; it is left for analysis to report.
#[tokio::test]
async fn reference_under_project_account_is_not_fetched() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        "(define-public (go) (contract-call? .not-in-manifest get-x))\n",
        "",
    );

    let mut server = Server::new_async().await;
    let any_request = server
        .mock("GET", mockito::Matcher::Any)
        .expect(0)
        .create_async()
        .await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    any_request.assert_async().await;
    assert!(published.is_empty(), "got {published:?}");
}

/// A contract principal inside a trait argument's expression that is only
/// data must not be published: once fetched, it doesn't define the trait's
/// functions.
#[tokio::test]
async fn data_principal_in_trait_argument_is_not_published() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &format!(
            "(define-public (go)\n\
               (contract-call? '{EXTERNAL_DEPLOYER}.callee take\n\
                 (get impl {{ owner: '{EXTERNAL_DEPLOYER}.other, impl: '{EXTERNAL_DEPLOYER}.implementation }})))\n"
        ),
        "",
    );

    let server = mock_contracts(&[
        (
            EXTERNAL_DEPLOYER,
            "callee",
            "(define-trait reader ((get-one () (response uint uint))))\n\
             (define-public (take (target <reader>)) (contract-call? target get-one))",
        ),
        (EXTERNAL_DEPLOYER, "implementation", PLAIN_SOURCE),
        (
            EXTERNAL_DEPLOYER,
            "other",
            "(define-read-only (get-two) (ok u2))",
        ),
    ])
    .await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    assert_eq!(
        published,
        [
            format!("{EXTERNAL_DEPLOYER}.callee"),
            format!("{EXTERNAL_DEPLOYER}.implementation"),
        ]
    );
}

/// A trait argument bound by a `let` outside the call is resolved once the
/// external callee's signature is loaded, from the bindings at the call site.
#[tokio::test]
async fn let_bound_trait_argument_is_auto_detected() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &format!(
            "(define-public (go)\n\
               (let ((t '{EXTERNAL_DEPLOYER}.implementation))\n\
                 (contract-call? '{EXTERNAL_DEPLOYER}.callee take t)))\n"
        ),
        "",
    );

    let server = mock_contracts(&[
        (
            EXTERNAL_DEPLOYER,
            "callee",
            "(define-trait reader ((get-one () (response uint uint))))\n\
             (define-public (take (target <reader>)) (contract-call? target get-one))",
        ),
        (EXTERNAL_DEPLOYER, "implementation", PLAIN_SOURCE),
    ])
    .await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    assert!(
        published.contains(&format!("{EXTERNAL_DEPLOYER}.implementation")),
        "got {published:?}"
    );
}

// PR #2508 review, finding #27: "Fetched requirements retain speculative edges
// after their targets are loaded".
//
// `wrapper`'s dependencies are finalized while `other` isn't loaded yet, so the
// speculative edge to it can't be ruled out. Once `other` is loaded it plainly
// lacks `get-one`, but `wrapper` is never re-examined, and `other` is published
// through it.
#[tokio::test]
async fn data_principal_in_requirement_trait_argument_is_not_published() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_project(
        root,
        &format!("(define-public (go) (contract-call? '{EXTERNAL_DEPLOYER}.wrapper go))\n"),
        "",
    );

    let server = mock_contracts(&[
        (
            EXTERNAL_DEPLOYER,
            "wrapper",
            &format!(
                "(define-public (go)\n\
                   (contract-call? '{EXTERNAL_DEPLOYER}.callee take\n\
                     (get impl {{ owner: '{EXTERNAL_DEPLOYER}.other, impl: '{EXTERNAL_DEPLOYER}.implementation }})))\n"
            ),
        ),
        (
            EXTERNAL_DEPLOYER,
            "callee",
            "(define-trait reader ((get-one () (response uint uint))))\n\
             (define-public (take (target <reader>)) (contract-call? target get-one))",
        ),
        (EXTERNAL_DEPLOYER, "implementation", PLAIN_SOURCE),
        (
            EXTERNAL_DEPLOYER,
            "other",
            "(define-read-only (get-two) (ok u2))",
        ),
    ])
    .await;

    let published = testnet_requirement_publishes(root, &server.url()).await;

    assert!(
        !published.contains(&format!("{EXTERNAL_DEPLOYER}.other")),
        "`other` lacks the trait's functions, so it is only data; got {published:?}"
    );
}

// PR #2508 review, finding #26: "Remote-data mode drops explicit signatures
// needed to order local contracts".
//
// Under simnet remote data, the explicit `callee` requirement is never loaded,
// so `caller`'s trait argument `.implementation` isn't detected and `caller` is
// ordered first, which fails full analysis.
#[tokio::test]
async fn remote_data_loads_explicit_requirements_to_order_local_contracts() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    let server = mock_contracts(&[(
        EXTERNAL_DEPLOYER,
        "callee",
        "(define-trait reader ((get-one () (response uint uint))))\n\
         (define-public (take (target <reader>)) (contract-call? target get-one))",
    )])
    .await;

    write_remote_data_project(
        root,
        &format!("[[project.requirements]]\ncontract_id = \"{EXTERNAL_DEPLOYER}.callee\""),
        &[
            (
                "caller",
                &format!(
                    "(define-public (go) (contract-call? '{EXTERNAL_DEPLOYER}.callee take .implementation))\n"
                ),
            ),
            ("implementation", PLAIN_SOURCE),
        ],
    );

    let deployed = remote_data_simnet_publishes(root, &server.url()).await;

    let position = |name: &str| deployed.iter().position(|c| c == name).unwrap();
    assert!(
        position("implementation") < position("caller"),
        "`caller` passes `.implementation` as a trait argument; got {deployed:?}"
    );
}

/// Under simnet remote data the boot contracts aren't seeded, but they are on
/// the remote chain: a reference to one must not be fetched.
#[tokio::test]
async fn remote_data_does_not_fetch_boot_contracts() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();

    write_remote_data_project(
        root,
        "",
        &[(
            "caller",
            &format!(
                "(define-read-only (cycle) (contract-call? 'SP000000000000000000002Q6VF78.pox-4 current-pox-reward-cycle))\n\
                 (define-read-only (supply) (contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-token get-total-supply))\n"
            ),
        )],
    );

    let mut server = Server::new_async().await;
    let any_request = server
        .mock("GET", mockito::Matcher::Any)
        .expect(0)
        .create_async()
        .await;

    let deployed = remote_data_simnet_publishes(root, &server.url()).await;

    any_request.assert_async().await;
    assert_eq!(deployed, ["caller"]);
}

/// Write a project with simnet remote data enabled, the supplied requirements
/// and `(name, source)` contracts.
fn write_remote_data_project(root: &Path, requirements_toml: &str, contracts: &[(&str, &str)]) {
    fs::create_dir_all(root.join("settings")).unwrap();
    fs::create_dir_all(root.join("contracts")).unwrap();

    let contracts_toml: String = contracts
        .iter()
        .map(|(name, _)| {
            formatdoc!(
                r#"
                [contracts.{name}]
                path = "contracts/{name}.clar"
                clarity_version = 3
                epoch = "3.0"

                "#
            )
        })
        .collect();
    fs::write(
        root.join("Clarinet.toml"),
        formatdoc!(
            r#"
            [project]
            name = "auto-detection-test"
            authors = []
            description = ""
            telemetry = false
            cache_dir = "./.cache"
            {requirements_toml}

            {contracts_toml}
            [repl.remote_data]
            enabled = true
            api_url = "https://api.hiro.so"
            initial_height = 522000
            "#
        ),
    )
    .unwrap();
    fs::write(
        root.join("settings/Devnet.toml"),
        formatdoc!(
            r#"
            [network]
            name = "devnet"
            deployment_fee_rate = 10

            [accounts.deployer]
            mnemonic = "{TEST_MNEMONIC}"
            balance = 100_000_000_000_000
            "#
        ),
    )
    .unwrap();
    for (name, source) in contracts {
        fs::write(root.join(format!("contracts/{name}.clar")), source).unwrap();
    }
}

/// Return the names of the contracts published by a generated simnet plan,
/// with remote data enabled.
async fn remote_data_simnet_publishes(root: &Path, api_url: &str) -> Vec<String> {
    // `true` keeps `[repl.remote_data]` enabled.
    let manifest = ProjectManifest::from_location(&root.join("Clarinet.toml"), true).unwrap();
    assert!(manifest.repl_settings.remote_data.enabled);
    let (deployment, _artifacts, _) = generate_default_deployment(
        &manifest,
        &StacksNetwork::Simnet,
        false,
        None,
        Some(api_url),
        Environment::Simnet,
    )
    .await
    .expect("simnet deployment plan should be generated");

    deployment
        .plan
        .batches
        .iter()
        .flat_map(|batch| &batch.transactions)
        .filter_map(|tx| match tx {
            TransactionSpecification::EmulatedContractPublish(spec) => {
                Some(spec.contract_name.to_string())
            }
            TransactionSpecification::ContractPublish(spec) => Some(spec.contract_name.to_string()),
            _ => None,
        })
        .collect()
}
