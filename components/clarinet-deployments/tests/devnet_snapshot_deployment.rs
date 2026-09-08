//! Regression coverage for deploying on a devnet that boots from a snapshot.
//!
//! The snapshot already holds transactions from the deployer (the sBTC
//! requirement contracts), so `apply_on_chain_deployment` must read nonces from
//! the node instead of assuming they start at zero, and must not republish
//! requirements that already exist on chain.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{channel, RecvTimeoutError};
use std::time::{Duration, Instant};

use clarinet_deployments::onchain::{
    apply_on_chain_deployment, DeploymentCommand, DeploymentEvent,
};
use clarinet_deployments::types::{
    ContractPublishSpecification, DeploymentSpecification, EpochSpec,
    RequirementPublishSpecification, TransactionPlanSpecification, TransactionSpecification,
    TransactionsBatchSpecification,
};
use clarinet_files::{NetworkManifest, StacksNetwork};
use clarity::vm::types::{PrincipalData, QualifiedContractIdentifier, StandardPrincipalData};
use clarity::vm::{ClarityVersion, ContractName};
use indoc::formatdoc;
use mockito::{Mock, Server, ServerGuard};
use serde_json::json;
use tempfile::TempDir;

/// Well-known Clarinet test mnemonic and the devnet address it derives to.
const DEPLOYER_MNEMONIC: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";
const DEPLOYER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

/// sBTC's mainnet deployer, as a project would spell it in `Clarinet.toml`.
const SBTC_MAINNET_DEPLOYER: &str = "SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4";

/// Nonce the deployer already has in the snapshot (sbtc-registry + sbtc-token).
const SNAPSHOT_DEPLOYER_NONCE: u64 = 2;

const CONTRACT_SOURCE: &str = "(define-read-only (get-one) (ok u1))";

/// Offset of the origin nonce in a serialized single-sig standard transaction:
/// version (1) + chain id (4) + auth type (1) + hash mode (1) + signer (20).
const NONCE_OFFSET: usize = 27;

fn write_devnet_settings(root: &Path) {
    fs::create_dir_all(root.join("settings")).unwrap();

    #[rustfmt::skip]
    let devnet_settings = formatdoc!(r#"
        [network]
        name = "devnet"
        deployment_fee_rate = 10

        [accounts.deployer]
        mnemonic = "{DEPLOYER_MNEMONIC}"
        balance = 100_000_000_000_000

        [devnet]
    "#);

    fs::write(root.join("settings/Devnet.toml"), devnet_settings).unwrap();
}

fn principal(address: &str) -> StandardPrincipalData {
    PrincipalData::parse_standard_principal(address).unwrap()
}

fn requirement_publish(contract_name: &str) -> TransactionSpecification {
    TransactionSpecification::RequirementPublish(RequirementPublishSpecification {
        contract_id: QualifiedContractIdentifier::parse(&format!(
            "{SBTC_MAINNET_DEPLOYER}.{contract_name}"
        ))
        .unwrap(),
        remap_sender: principal(DEPLOYER),
        remap_principals: BTreeMap::from([(principal(SBTC_MAINNET_DEPLOYER), principal(DEPLOYER))]),
        source: CONTRACT_SOURCE.to_string(),
        clarity_version: ClarityVersion::Clarity3,
        cost: 10_000,
        location: PathBuf::from(format!("requirements/{contract_name}.clar")),
    })
}

fn contract_publish(contract_name: &str) -> TransactionSpecification {
    TransactionSpecification::ContractPublish(ContractPublishSpecification {
        contract_name: ContractName::try_from(contract_name.to_string()).unwrap(),
        expected_sender: principal(DEPLOYER),
        location: PathBuf::from(format!("contracts/{contract_name}.clar")),
        source: CONTRACT_SOURCE.to_string(),
        clarity_version: ClarityVersion::Clarity3,
        cost: 10_000,
        anchor_block_only: true,
    })
}

/// The clarity-starter plan: three sBTC requirements at epoch 3.0, then the
/// project's own contract at epoch 4.0.
fn deployment(stacks_node_url: &str) -> DeploymentSpecification {
    DeploymentSpecification {
        id: 0,
        name: "Devnet deployment".to_string(),
        network: StacksNetwork::Devnet,
        stacks_node: Some(stacks_node_url.to_string()),
        bitcoin_node: Some("http://devnet:devnet@localhost:18443".to_string()),
        genesis: None,
        plan: TransactionPlanSpecification {
            batches: vec![
                TransactionsBatchSpecification {
                    id: 0,
                    transactions: vec![
                        requirement_publish("sbtc-registry"),
                        requirement_publish("sbtc-token"),
                        requirement_publish("sbtc-deposit"),
                    ],
                    epoch: Some(EpochSpec::Epoch3_0),
                },
                TransactionsBatchSpecification {
                    id: 1,
                    transactions: vec![contract_publish("counter")],
                    epoch: Some(EpochSpec::Epoch4_0),
                },
            ],
        },
        contracts: BTreeMap::new(),
    }
}

fn json_mock(server: &mut ServerGuard, path: &str, status: usize, body: &str) -> Mock {
    server
        .mock("GET", path)
        .with_status(status)
        .with_header("content-type", "application/json")
        .with_body(body)
        .create()
}

/// A node whose chain is already past every epoch, as when booting from the
/// epoch 4.0 snapshot. Polled repeatedly while waiting for epochs and confirmations.
fn mock_node_info(server: &mut ServerGuard) -> Mock {
    server
        .mock("GET", "/v2/info")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(
            json!({
            "peer_version": 4207599116u64,
            "pox_consensus": "4f4de3d4ab3246299c039084a12c801c9dc70323",
            "burn_block_height": 200,
            "stable_pox_consensus": "a2c4972bf818f554809e25fa637b780c77c20b62",
            "stable_burn_block_height": 199,
            "server_version": "stacks-node 0.0.1",
            "network_id": 2147483648u64,
            "parent_network_id": 3669344250u64,
            "stacks_tip_height": 70,
            "stacks_tip": "6bb0e4706fdfb9624a23d9144f2161c61d5c58816643b48ffdb735887bdbf5fa",
            "stacks_tip_consensus_hash": "4f4de3d4ab3246299c039084a12c801c9dc70323",
            "genesis_chainstate_hash": "74237aa39aa50a83de11a4f53e9d3bb7d43461d1de9873f402e5453ae60bc59b",
            })
            .to_string(),
        )
        .expect_at_least(1)
        .create()
}

fn mock_deployer_nonce(server: &mut ServerGuard, nonce: u64) -> Mock {
    json_mock(
        server,
        &format!("/v2/accounts/{DEPLOYER}"),
        200,
        &json!({
            "balance": "0x000000000000000000005af31077cdac",
            "nonce": nonce,
            "balance_proof": "",
            "nonce_proof": "",
        })
        .to_string(),
    )
}

fn mock_published_contract(server: &mut ServerGuard, contract_name: &str) -> Mock {
    json_mock(
        server,
        &format!("/v2/contracts/source/{DEPLOYER}/{contract_name}"),
        200,
        &json!({ "source": CONTRACT_SOURCE, "publish_height": 41 }).to_string(),
    )
}

/// Expect exactly one broadcast of a transaction publishing `contract_name`
/// with origin nonce `nonce`.
fn mock_broadcast(server: &mut ServerGuard, contract_name: &'static str, nonce: u64) -> Mock {
    server
        .mock("POST", "/v2/transactions")
        .match_request(move |request| {
            let body = request.body().unwrap();
            let tx_nonce =
                u64::from_be_bytes(body[NONCE_OFFSET..NONCE_OFFSET + 8].try_into().unwrap());
            tx_nonce == nonce
                && body
                    .windows(contract_name.len())
                    .any(|window| window == contract_name.as_bytes())
        })
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(format!(r#""{contract_name}-txid""#))
        .expect(1)
        .create()
}

/// Run the deployment to completion and return every event it emitted.
fn run_deployment(stacks_node_url: &str, project_root: &Path) -> Vec<DeploymentEvent> {
    let network_manifest = NetworkManifest::from_project_root(
        project_root,
        &StacksNetwork::Devnet.get_networks(),
        false,
        Some(&project_root.join(".cache")),
        None,
    )
    .unwrap();
    let deployment = deployment(stacks_node_url);

    let (event_tx, event_rx) = channel();
    let (command_tx, command_rx) = channel();
    std::thread::spawn(move || {
        apply_on_chain_deployment(
            network_manifest,
            deployment,
            event_tx,
            command_rx,
            None,
            None,
        );
    });
    command_tx.send(DeploymentCommand::Start).unwrap();

    let deadline = Instant::now() + Duration::from_secs(30);
    let mut events = vec![];
    loop {
        match event_rx.recv_timeout(deadline.saturating_duration_since(Instant::now())) {
            Ok(event) => {
                let done = matches!(
                    event,
                    DeploymentEvent::DeploymentCompleted | DeploymentEvent::Interrupted(_)
                );
                events.push(event);
                if done {
                    break;
                }
            }
            Err(RecvTimeoutError::Timeout) => panic!("deployment did not complete in time"),
            Err(RecvTimeoutError::Disconnected) => break,
        }
    }
    events
}

#[test]
fn snapshot_deployment_skips_published_requirements_and_uses_node_nonces() {
    let mut server = Server::new();
    let node_info = mock_node_info(&mut server);
    let deployer_nonce = mock_deployer_nonce(&mut server, SNAPSHOT_DEPLOYER_NONCE);

    // Already in the snapshot: must be skipped, not republished.
    let _sbtc_registry = mock_published_contract(&mut server, "sbtc-registry");
    let _sbtc_token = mock_published_contract(&mut server, "sbtc-token");

    // Not in the snapshot: reported missing once while encoding, then found
    // once the broadcast is confirmed.
    let sbtc_deposit_missing = json_mock(
        &mut server,
        &format!("/v2/contracts/source/{DEPLOYER}/sbtc-deposit"),
        404,
        "No contract source data found",
    )
    .expect(1);
    let _sbtc_deposit_published = mock_published_contract(&mut server, "sbtc-deposit");
    let _counter_published = mock_published_contract(&mut server, "counter");

    // The two remaining transactions continue from the deployer's on-chain nonce.
    let sbtc_deposit_broadcast =
        mock_broadcast(&mut server, "sbtc-deposit", SNAPSHOT_DEPLOYER_NONCE);
    let counter_broadcast = mock_broadcast(&mut server, "counter", SNAPSHOT_DEPLOYER_NONCE + 1);

    let temp_dir = TempDir::new().unwrap();
    write_devnet_settings(temp_dir.path());

    let events = run_deployment(&server.url(), temp_dir.path());

    if let Some(DeploymentEvent::Interrupted(message)) = events
        .iter()
        .find(|event| matches!(event, DeploymentEvent::Interrupted(_)))
    {
        panic!("deployment was interrupted: {message}");
    }
    assert!(
        matches!(events.last(), Some(DeploymentEvent::DeploymentCompleted)),
        "deployment should complete"
    );

    let mut tracked: Vec<String> = events
        .iter()
        .filter_map(|event| match event {
            DeploymentEvent::TransactionUpdate(tracker) => Some(tracker.name.clone()),
            _ => None,
        })
        .collect();
    tracked.sort();
    tracked.dedup();
    assert_eq!(
        tracked,
        vec![
            format!("Publish {DEPLOYER}.counter"),
            format!("Publish {DEPLOYER}.sbtc-deposit"),
        ],
        "only the contracts missing from the snapshot should be tracked"
    );

    node_info.assert();
    deployer_nonce.assert();
    sbtc_deposit_missing.assert();
    sbtc_deposit_broadcast.assert();
    counter_broadcast.assert();
}
