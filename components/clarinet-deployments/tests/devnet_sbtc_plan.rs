//! The sBTC contracts are part of the protocol surface since PoX-5, so a devnet
//! plan must publish all of them — including `sbtc-deposit`, which the chains
//! coordinator watches for before minting the configured `sbtc_balance`.

use std::fs;
use std::path::Path;

use clarinet_deployments::generate_default_deployment;
use clarinet_deployments::types::TransactionSpecification;
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarinet_utils::DEFAULT_DEPLOYER_MNEMONIC as TEST_MNEMONIC;
use clarity_repl::repl::boot::{SBTC_CONTRACTS_NAMES, SBTC_MAINNET_ADDRESS};
use clarity_repl::utils::Environment;
use indoc::formatdoc;
use tempfile::TempDir;

/// Write a project with a single contract, the way `clarinet new` followed by
/// `clarinet contract new` would, with the given source.
fn write_project(root: &Path, source: &str) {
    fs::create_dir_all(root.join("settings")).unwrap();
    fs::create_dir_all(root.join("contracts")).unwrap();

    #[rustfmt::skip]
    let manifest = formatdoc!(r#"
        [project]
        name = "devnet-sbtc-test"
        authors = []
        description = ""
        telemetry = false

        [contracts.noop]
        path = "contracts/noop.clar"
        epoch = "latest"
    "#);

    #[rustfmt::skip]
    let devnet_settings = formatdoc!(r#"
        [network]
        name = "devnet"
        deployment_fee_rate = 10

        [accounts.deployer]
        mnemonic = "{TEST_MNEMONIC}"
        balance = 100_000_000_000_000
        sbtc_balance = 1_000_000_000
    "#);

    fs::write(root.join("Clarinet.toml"), manifest).unwrap();
    fs::write(root.join("settings/Devnet.toml"), devnet_settings).unwrap();
    fs::write(root.join("contracts/noop.clar"), source).unwrap();
}

async fn published_requirements(source: &str) -> Vec<String> {
    let temp_dir = TempDir::new().unwrap();
    write_project(temp_dir.path(), source);

    let manifest =
        ProjectManifest::from_location(&temp_dir.path().join("Clarinet.toml"), false).unwrap();

    let (deployment, _artifacts, _) = generate_default_deployment(
        &manifest,
        &StacksNetwork::Devnet,
        false,
        None,
        None,
        Environment::OnChain,
    )
    .await
    .expect("devnet deployment plan should be generated");

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

fn sbtc_contract_ids() -> Vec<String> {
    SBTC_CONTRACTS_NAMES
        .iter()
        .map(|name| format!("{SBTC_MAINNET_ADDRESS}.{name}"))
        .collect()
}

#[tokio::test]
async fn devnet_plan_publishes_every_sbtc_contract() {
    assert_eq!(
        published_requirements("(define-read-only (noop) u1)\n").await,
        sbtc_contract_ids(),
        "a stock devnet plan must publish the sBTC contracts, in dependency order"
    );
}

#[tokio::test]
async fn devnet_plan_publishes_referenced_sbtc_contract_once() {
    let source = format!(
        "(define-read-only (noop) (contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-token get-name))\n"
    );

    assert_eq!(
        published_requirements(&source).await,
        sbtc_contract_ids(),
        "a referenced sBTC contract must not be published twice"
    );
}
