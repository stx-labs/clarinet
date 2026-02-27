use std::collections::BTreeMap;
use std::path::PathBuf;

use clarinet_deployments::types::*;
use clarinet_files::StacksNetwork;
use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
use clarity_repl::clarity::{ClarityName, ClarityVersion, ContractName};

fn get_test_txs() -> (TransactionSpecification, TransactionSpecification) {
    let contract_id =
        QualifiedContractIdentifier::parse("ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.test")
            .unwrap();
    let tx_sender = contract_id.issuer;

    let contract_publish_tx =
        TransactionSpecification::EmulatedContractPublish(EmulatedContractPublishSpecification {
            contract_name: ContractName::try_from("test".to_string()).unwrap(),
            emulated_sender: tx_sender.clone(),
            location: PathBuf::from("/contracts/test.clar"),
            source: "(ok true)".to_string(),
            clarity_version: ClarityVersion::Clarity2,
            is_requirement: false,
        });

    let contract_call_txs =
        TransactionSpecification::EmulatedContractCall(EmulatedContractCallSpecification {
            contract_id: QualifiedContractIdentifier::parse(
                "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.test",
            )
            .unwrap(),
            emulated_sender: tx_sender,
            method: ClarityName::try_from("test".to_string()).unwrap(),
            parameters: vec![],
        });

    (contract_publish_tx, contract_call_txs)
}

fn build_test_deployement_plan(
    batches: Vec<TransactionsBatchSpecification>,
) -> DeploymentSpecification {
    DeploymentSpecification {
        id: 1,
        name: "test".to_string(),
        network: StacksNetwork::Simnet,
        stacks_node: None,
        bitcoin_node: None,
        genesis: None,
        contracts: BTreeMap::new(),
        plan: TransactionPlanSpecification { batches },
    }
}

#[test]
fn test_extract_no_contract_publish_txs() {
    let (contract_publish_tx, contract_call_txs) = get_test_txs();

    let plan = build_test_deployement_plan(vec![
        TransactionsBatchSpecification {
            id: 0,
            transactions: vec![contract_publish_tx.clone()],
            epoch: Some(EpochSpec::Epoch2_4),
        },
        TransactionsBatchSpecification {
            id: 1,
            transactions: vec![contract_call_txs.clone()],
            epoch: Some(EpochSpec::Epoch2_4),
        },
    ]);

    let (new_plan, custom_txs) = plan.extract_no_contract_publish_txs();

    assert_eq!(
        new_plan,
        build_test_deployement_plan(vec![TransactionsBatchSpecification {
            id: 0,
            transactions: vec![contract_publish_tx],
            epoch: Some(EpochSpec::Epoch2_4),
        },])
    );

    assert_eq!(
        custom_txs,
        vec![TransactionsBatchSpecification {
            id: 1,
            transactions: vec![contract_call_txs],
            epoch: Some(EpochSpec::Epoch2_4),
        }]
    );
}

#[test]
fn test_merge_batches() {
    let (contract_publish_tx, contract_call_txs) = get_test_txs();

    let plan = build_test_deployement_plan(vec![
        TransactionsBatchSpecification {
            id: 0,
            transactions: vec![contract_publish_tx],
            epoch: Some(EpochSpec::Epoch2_4),
        },
        TransactionsBatchSpecification {
            id: 1,
            transactions: vec![contract_call_txs],
            epoch: Some(EpochSpec::Epoch2_4),
        },
    ]);

    let (mut new_plan, custom_txs) = plan.extract_no_contract_publish_txs();

    assert_ne!(plan, new_plan);

    new_plan.merge_batches(custom_txs);

    assert_eq!(plan, new_plan);
}
