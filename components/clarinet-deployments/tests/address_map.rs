//! Integration tests for [[project.address_map]] behavior in generated deployments.
//!
//! Covers two new code paths introduced alongside address_map:
//!
//! 1. **Skip-redeploy**: when an entry has a `testnet` override that differs from
//!    its `contract_id`, no `RequirementPublish` transaction is emitted — Clarinet
//!    remaps the identifier in source text at broadcast time instead.
//!
//! 2. **Auto-detection**: a user contract that calls an external contract causes
//!    that external contract to be auto-detected and published as a requirement,
//!    even if the user never listed it in `[[project.address_map]]`.

use std::fs;
use std::path::Path;

use clarinet_deployments::generate_default_deployment;
use clarinet_deployments::types::TransactionSpecification;
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity_repl::utils::Environment;
use indoc::formatdoc;
use mockito::{Server, ServerGuard};
use tempfile::TempDir;

const TEST_MNEMONIC: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";
const REQUIREMENT_SOURCE: &str = "(define-read-only (get-one) (ok u1))";
const EXTERNAL_DEPLOYER: &str = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9";

fn write_testnet_settings(root: &Path) {
    fs::create_dir_all(root.join("settings")).unwrap();
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
}

/// Serve `deployer.contract_name` from a mock API so the test stays offline.
async fn mock_contract(deployer: &str, contract_name: &str) -> ServerGuard {
    let mut server = Server::new_async().await;
    server
        .mock(
            "GET",
            format!("/extended/v1/contract/{deployer}.{contract_name}").as_str(),
        )
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(
            serde_json::json!({
                "source_code": REQUIREMENT_SOURCE,
                "block_height": 175232,
                "clarity_version": 3
            })
            .to_string(),
        )
        .create_async()
        .await;
    server
}

/// When an address_map entry has a `testnet` override that differs from its
/// `contract_id`, the testnet plan must NOT contain a RequirementPublish for
/// that contract — it will be remapped at broadcast time instead.
#[tokio::test]
async fn testnet_address_map_override_skips_redeploy() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();
    fs::create_dir_all(root.join("contracts")).unwrap();
    write_testnet_settings(root);

    fs::write(
        root.join("Clarinet.toml"),
        formatdoc!(
            r#"
            [project]
            name = "remap-test"
            authors = []
            description = ""
            telemetry = false

            [[project.address_map]]
            contract_id = "{EXTERNAL_DEPLOYER}.nft-trait"
            testnet = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.nft-trait"
            "#
        ),
    )
    .unwrap();

    // The contract source is still fetched to build the AST for dependency
    // detection, so the mock must serve it.
    let server = mock_contract(EXTERNAL_DEPLOYER, "nft-trait").await;

    let manifest = ProjectManifest::from_location(&root.join("Clarinet.toml"), false).unwrap();
    let (deployment, _artifacts, _) = generate_default_deployment(
        &manifest,
        &StacksNetwork::Testnet,
        false,
        None,
        Some(&server.url()),
        Environment::OnChain,
    )
    .await
    .expect("testnet deployment plan should be generated");

    // No RequirementPublish should appear for the overridden contract.
    let has_publish = deployment
        .plan
        .batches
        .iter()
        .flat_map(|b| &b.transactions)
        .any(|tx| matches!(tx, TransactionSpecification::RequirementPublish(_)));
    assert!(
        !has_publish,
        "a contract with a testnet override should not generate a RequirementPublish"
    );

    // The deployment spec must carry the address_map entry so apply_on_chain_deployment
    // can remap the identifier in source text at broadcast time.
    let entry = deployment
        .address_map
        .iter()
        .find(|e| e.contract_id == format!("{EXTERNAL_DEPLOYER}.nft-trait"))
        .expect("deployment.address_map should contain the override entry");
    assert_eq!(
        entry.testnet.as_deref(),
        Some("ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.nft-trait")
    );
}

/// A user contract that calls an external contract should cause that external
/// contract to be auto-detected as a requirement and included in the testnet
/// plan as a RequirementPublish, even when it is not listed in address_map.
#[tokio::test]
async fn auto_detected_requirement_included_in_testnet_plan() {
    let temp_dir = TempDir::new().unwrap();
    let root = temp_dir.path();
    fs::create_dir_all(root.join("contracts")).unwrap();
    write_testnet_settings(root);

    fs::write(
        root.join("Clarinet.toml"),
        formatdoc!(
            r#"
            [project]
            name = "auto-detect-test"
            authors = []
            description = ""
            telemetry = false

            [contracts.caller]
            path = "contracts/caller.clar"
            clarity_version = 3
            epoch = "3.0"
            "#
        ),
    )
    .unwrap();

    // The user contract calls an external contract that is NOT listed in address_map.
    // Clarinet should auto-detect it via AST analysis and pull it in as a requirement.
    fs::write(
        root.join("contracts/caller.clar"),
        format!(
            "(define-public (call-ext) \
             (contract-call? '{EXTERNAL_DEPLOYER}.nft-trait get-one))"
        ),
    )
    .unwrap();

    let server = mock_contract(EXTERNAL_DEPLOYER, "nft-trait").await;

    let manifest = ProjectManifest::from_location(&root.join("Clarinet.toml"), false).unwrap();
    let (deployment, _artifacts, _) = generate_default_deployment(
        &manifest,
        &StacksNetwork::Testnet,
        false,
        None,
        Some(&server.url()),
        Environment::OnChain,
    )
    .await
    .expect("testnet deployment plan should be generated");

    let publishes: Vec<_> = deployment
        .plan
        .batches
        .iter()
        .flat_map(|b| &b.transactions)
        .filter_map(|tx| match tx {
            TransactionSpecification::RequirementPublish(spec) => Some(spec),
            _ => None,
        })
        .collect();

    let nft_publish = publishes
        .iter()
        .find(|spec| spec.contract_id.name.as_str() == "nft-trait");
    assert!(
        nft_publish.is_some(),
        "nft-trait should be auto-detected and included as a RequirementPublish; \
         got {} RequirementPublish tx(s): {:?}",
        publishes.len(),
        publishes
            .iter()
            .map(|s| s.contract_id.to_string())
            .collect::<Vec<_>>()
    );
}
