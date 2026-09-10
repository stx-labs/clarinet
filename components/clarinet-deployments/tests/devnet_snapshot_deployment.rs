//! Regression coverage for deploying on a devnet that boots from a snapshot.
//!
//! The snapshot already holds transactions from the deployer (the sBTC
//! requirement contracts), so `apply_on_chain_deployment` must read nonces from
//! the node instead of assuming they start at zero, and must not republish
//! requirements that already exist on chain.

use std::collections::{BTreeMap, BTreeSet};
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
use clarinet_files::{NetworkManifest, NetworkManifestFile, StacksNetwork};
use clarity::codec::StacksMessageCodec;
use clarity::vm::types::{PrincipalData, QualifiedContractIdentifier, StandardPrincipalData};
use clarity::vm::{ClarityVersion, ContractName};
use clarity_repl::repl::boot::SBTC_MAINNET_ADDRESS;
use indoc::formatdoc;
use mockito::{Mock, Server, ServerGuard};
use serde_json::json;
use stacks_codec::transaction::{StacksTransaction, TransactionPayload};

const DEPLOYER_MNEMONIC: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";
const DEPLOYER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

/// Nonce the deployer already has in the snapshot (sbtc-registry + sbtc-token).
const SNAPSHOT_DEPLOYER_NONCE: u64 = 2;

const CONTRACT_SOURCE: &str = "(define-read-only (get-one) (ok u1))";

fn network_manifest() -> NetworkManifest {
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
    let mut file: NetworkManifestFile = toml::from_str(&devnet_settings).unwrap();
    NetworkManifest::from_network_manifest_file(
        &mut file,
        &StacksNetwork::Devnet.get_networks(),
        false,
        None,
        None,
    )
    .unwrap()
}

fn principal(address: &str) -> StandardPrincipalData {
    PrincipalData::parse_standard_principal(address).unwrap()
}

fn requirement_publish(contract_name: &str) -> TransactionSpecification {
    TransactionSpecification::RequirementPublish(RequirementPublishSpecification {
        contract_id: QualifiedContractIdentifier::parse(&format!(
            "{SBTC_MAINNET_ADDRESS}.{contract_name}"
        ))
        .unwrap(),
        remap_sender: principal(DEPLOYER),
        remap_principals: BTreeMap::from([(principal(SBTC_MAINNET_ADDRESS), principal(DEPLOYER))]),
        source: CONTRACT_SOURCE.to_string(),
        clarity_version: ClarityVersion::Clarity3,
        cost: 10_000,
        location: format!("requirements/{contract_name}.clar").into(),
    })
}

fn contract_publish(contract_name: &str) -> TransactionSpecification {
    TransactionSpecification::ContractPublish(ContractPublishSpecification {
        contract_name: ContractName::try_from(contract_name.to_string()).unwrap(),
        expected_sender: principal(DEPLOYER),
        location: format!("contracts/{contract_name}.clar").into(),
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

/// Left uncreated so callers can add expectations.
fn json_mock(server: &mut ServerGuard, path: &str, status: usize, body: impl AsRef<[u8]>) -> Mock {
    server
        .mock("GET", path)
        .with_status(status)
        .with_header("content-type", "application/json")
        .with_body(body)
}

/// A node whose chain is already past every epoch, as when booting from the
/// epoch 4.0 snapshot. Polled repeatedly while waiting for epochs and confirmations.
fn mock_node_info(server: &mut ServerGuard) -> Mock {
    let info = json!({
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
    });
    json_mock(server, "/v2/info", 200, info.to_string())
        .expect_at_least(1)
        .create()
}

fn mock_deployer_nonce(server: &mut ServerGuard, nonce: u64) -> Mock {
    let account = json!({
        "balance": "0x000000000000000000005af31077cdac",
        "nonce": nonce,
        "balance_proof": "",
        "nonce_proof": "",
    });
    json_mock(
        server,
        &format!("/v2/accounts/{DEPLOYER}"),
        200,
        account.to_string(),
    )
    .create()
}

fn mock_published_contract(server: &mut ServerGuard, contract_name: &str) -> Mock {
    let contract = json!({ "source": CONTRACT_SOURCE, "publish_height": 41 });
    json_mock(
        server,
        &format!("/v2/contracts/source/{DEPLOYER}/{contract_name}"),
        200,
        contract.to_string(),
    )
    .create()
}

fn mock_broadcast(server: &mut ServerGuard, contract_name: &'static str, nonce: u64) -> Mock {
    server
        .mock("POST", "/v2/transactions")
        .match_request(move |request| {
            let tx =
                StacksTransaction::consensus_deserialize(&mut request.body().unwrap().as_slice())
                    .unwrap();
            tx.get_origin_nonce() == nonce
                && matches!(
                    &tx.payload,
                    TransactionPayload::SmartContract(contract, _)
                        if contract.name.as_str() == contract_name
                )
        })
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(format!(r#""{contract_name}-txid""#))
        .create()
}

fn run_deployment(stacks_node_url: &str) -> Vec<DeploymentEvent> {
    let network_manifest = network_manifest();
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
    let mut events: Vec<DeploymentEvent> = vec![];
    while !matches!(
        events.last(),
        Some(DeploymentEvent::DeploymentCompleted | DeploymentEvent::Interrupted(_))
    ) {
        match event_rx.recv_timeout(deadline.saturating_duration_since(Instant::now())) {
            Ok(event) => events.push(event),
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

    // Not in the snapshot: missing while encoding, present once confirmed.
    let sbtc_deposit_missing = json_mock(
        &mut server,
        &format!("/v2/contracts/source/{DEPLOYER}/sbtc-deposit"),
        404,
        "No contract source data found",
    )
    .expect(1)
    .create();
    let _sbtc_deposit_published = mock_published_contract(&mut server, "sbtc-deposit");
    let _counter_published = mock_published_contract(&mut server, "counter");

    // The two remaining transactions continue from the deployer's on-chain nonce.
    let sbtc_deposit_broadcast =
        mock_broadcast(&mut server, "sbtc-deposit", SNAPSHOT_DEPLOYER_NONCE);
    let counter_broadcast = mock_broadcast(&mut server, "counter", SNAPSHOT_DEPLOYER_NONCE + 1);

    let events = run_deployment(&server.url());

    assert!(
        matches!(events.last(), Some(DeploymentEvent::DeploymentCompleted)),
        "deployment did not complete: {:?}",
        events.last()
    );

    let tracked: BTreeSet<String> = events
        .iter()
        .filter_map(|event| match event {
            DeploymentEvent::TransactionUpdate(tracker) => Some(tracker.name.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(
        tracked,
        BTreeSet::from([
            format!("Publish {DEPLOYER}.counter"),
            format!("Publish {DEPLOYER}.sbtc-deposit"),
        ]),
        "only the contracts missing from the snapshot should be tracked"
    );

    node_info.assert();
    deployer_nonce.assert();
    sbtc_deposit_missing.assert();
    sbtc_deposit_broadcast.assert();
    counter_broadcast.assert();
}
