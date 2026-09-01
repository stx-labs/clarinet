//! The sBTC contracts are part of the protocol surface since PoX-5, so a devnet
//! plan must publish all of them — including `sbtc-deposit`, which the chains
//! coordinator watches for before minting the configured `sbtc_balance`.

use std::fs;
use std::path::Path;

use clarinet_deployments::generate_default_deployment;
use clarinet_deployments::types::TransactionSpecification;
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity_repl::repl::boot::{SBTC_CONTRACTS_NAMES, SBTC_MAINNET_ADDRESS};
use clarity_repl::utils::Environment;
use indoc::formatdoc;
use tempfile::TempDir;

/// Well-known Clarinet test mnemonic, matching the generated settings files.
const TEST_MNEMONIC: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";

/// Write a project with a single contract and no requirements, the way
/// `clarinet new` followed by `clarinet contract new` would.
fn write_project(root: &Path) {
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
    fs::write(
        root.join("contracts/noop.clar"),
        "(define-read-only (noop) u1)\n",
    )
    .unwrap();
}

#[tokio::test]
async fn devnet_plan_publishes_every_sbtc_contract() {
    let temp_dir = TempDir::new().unwrap();
    write_project(temp_dir.path());

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

    let published: Vec<String> = deployment
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
        .collect();

    let expected: Vec<String> = SBTC_CONTRACTS_NAMES
        .iter()
        .map(|name| format!("{SBTC_MAINNET_ADDRESS}.{name}"))
        .collect();

    assert_eq!(
        published, expected,
        "a stock devnet plan must publish the sBTC contracts, in dependency order"
    );
}
