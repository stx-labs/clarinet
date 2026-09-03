//! Regression coverage for the sBTC testnet deployer address.
//!
//! `SBTC_TESTNET_ADDRESS` is only ever read on testnet code paths: the
//! requirement remap exercised below, the contract-id remap in
//! `onchain::apply_on_chain_deployment`, and the `print`-event filter in
//! clarity-repl's logger hook. Simnet deploys the sBTC contracts at their
//! *mainnet* address, so no simnet test can catch a stale or malformed value.

use std::fs;
use std::path::Path;

use clarinet_deployments::generate_default_deployment;
use clarinet_deployments::types::TransactionSpecification;
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity_repl::utils::Environment;
use indoc::formatdoc;
use mockito::{Server, ServerGuard};
use tempfile::TempDir;

/// sBTC's mainnet deployer, as a project would spell it in `Clarinet.toml`.
const SBTC_MAINNET_DEPLOYER: &str = "SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4";

/// sBTC's testnet deployer, written out rather than imported from
/// `clarity_repl::repl::boot`. Comparing against the constant would make this
/// test follow any edit to it and therefore catch nothing; the point is to pin
/// the value. `SN` is the testnet multisig version byte, matching mainnet `SM`.
const SBTC_TESTNET_DEPLOYER: &str = "SN3VMHXEN64ZZF71JQ5VESXDWTR301XTTXGF4J8F1";

/// A contract that is not sBTC, used to check the remap is not over-broad.
const OTHER_DEPLOYER: &str = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9";

/// Well-known Clarinet test mnemonic, matching the generated settings files.
const TEST_MNEMONIC: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";

/// Stand-in for the real requirement source. The remap keys off the issuer
/// address, not the body, so a dependency-free contract keeps the test fast.
const REQUIREMENT_SOURCE: &str = "(define-read-only (get-one) (ok u1))";

/// Write a minimal project whose only requirement is `contract_id`.
fn write_project(root: &Path, contract_id: &str) {
    fs::create_dir_all(root.join("settings")).unwrap();
    fs::create_dir_all(root.join("contracts")).unwrap();

    #[rustfmt::skip]
    let manifest = formatdoc!(r#"
        [project]
        name = "sbtc-remap-test"
        authors = []
        description = ""
        telemetry = false

        [[project.requirements]]
        contract_id = "{contract_id}"
    "#);

    #[rustfmt::skip]
    let testnet_settings = formatdoc!(r#"
        [network]
        name = "testnet"
        deployment_fee_rate = 10

        [accounts.deployer]
        mnemonic = "{TEST_MNEMONIC}"
    "#);

    fs::write(root.join("Clarinet.toml"), manifest).unwrap();
    fs::write(root.join("settings/Testnet.toml"), testnet_settings).unwrap();
}

/// Serve `deployer.contract_name` from a mock API so the test stays offline.
async fn mock_requirement(deployer: &str, contract_name: &str) -> ServerGuard {
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

/// Generate a testnet plan and return its single requirement-publish sender and
/// principal remap.
async fn testnet_requirement_remap(
    deployer: &str,
    contract_name: &str,
) -> (String, Vec<(String, String)>) {
    let server = mock_requirement(deployer, contract_name).await;
    let temp_dir = TempDir::new().unwrap();
    write_project(temp_dir.path(), &format!("{deployer}.{contract_name}"));

    let manifest =
        ProjectManifest::from_location(&temp_dir.path().join("Clarinet.toml"), false).unwrap();

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

    let spec = deployment
        .plan
        .batches
        .iter()
        .flat_map(|batch| &batch.transactions)
        .find_map(|tx| match tx {
            TransactionSpecification::RequirementPublish(spec) => Some(spec),
            _ => None,
        })
        .expect("testnet plan should contain a requirement-publish transaction");

    let remap_principals = spec
        .remap_principals
        .iter()
        .map(|(from, to)| (from.to_string(), to.to_string()))
        .collect();

    (spec.remap_sender.to_string(), remap_principals)
}

#[tokio::test]
async fn testnet_plan_remaps_sbtc_requirement_to_the_testnet_deployer() {
    let (remap_sender, remap_principals) =
        testnet_requirement_remap(SBTC_MAINNET_DEPLOYER, "sbtc-deposit").await;

    assert_eq!(
        remap_sender, SBTC_TESTNET_DEPLOYER,
        "an sBTC requirement must be published from the sBTC testnet deployer, \
         otherwise the contract lands at the wrong address on testnet"
    );

    assert_eq!(
        remap_principals,
        vec![(
            SBTC_MAINNET_DEPLOYER.to_string(),
            SBTC_TESTNET_DEPLOYER.to_string()
        )],
        "the sBTC mainnet deployer must be rewritten to the testnet deployer \
         inside the contract source"
    );
}

#[tokio::test]
async fn testnet_plan_remaps_non_sbtc_requirements_to_the_project_deployer() {
    let (remap_sender, remap_principals) =
        testnet_requirement_remap(OTHER_DEPLOYER, "nft-trait").await;

    assert_ne!(
        remap_sender, SBTC_TESTNET_DEPLOYER,
        "only sBTC requirements may be remapped to the sBTC testnet deployer"
    );
    assert_eq!(
        remap_principals.len(),
        1,
        "a non-sBTC requirement remaps its issuer to the project deployer"
    );
    assert_eq!(remap_principals[0].0, OTHER_DEPLOYER);
    assert_eq!(
        remap_principals[0].1, remap_sender,
        "issuer should be rewritten to whoever publishes the requirement"
    );
}
