use std::collections::BTreeMap;
use std::sync::mpsc::{channel, Sender};

use crate::chainhook::types::{
    BitcoinBlockSignaling, BitcoinNetwork, BlockchainEvent, BlockchainUpdatedWithHeaders,
    StacksBlockUpdate, StacksChainEvent, StacksChainUpdatedWithBlocksData, StacksNetwork,
    StacksNodeConfig,
};
use hiro_system_kit;

use super::{ObserverEvent, PredicatesConfig, DEFAULT_INGESTION_PORT};
use crate::chainhook::hooks::bitcoin::{
    BitcoinChainhookInstance, BitcoinChainhookSpecification,
    BitcoinChainhookSpecificationNetworkMap, BitcoinPredicateType, OutputPredicate,
};
use crate::chainhook::hooks::stacks::{
    StacksChainhookInstance, StacksChainhookSpecification, StacksChainhookSpecificationNetworkMap,
    StacksContractCallBasedPredicate, StacksPredicate,
};
use crate::chainhook::hooks::types::{
    ChainhookInstance, ChainhookSpecificationNetworkMap, ChainhookStore, ExactMatchingRule,
    HookAction,
};
use crate::chainhook::indexer::tests::helpers::transactions::{
    generate_test_tx_bitcoin_p2pkh_transfer, generate_test_tx_stacks_contract_call,
};
use crate::chainhook::indexer::tests::helpers::{accounts, bitcoin_blocks, stacks_blocks};
use crate::chainhook::observer::{
    start_observer_commands_handler, EventObserverConfig, ObserverCommand,
    PredicateDeregisteredEvent,
};
use crate::chainhook::utils::{AbstractBlock, Context};

fn generate_test_config() -> (EventObserverConfig, ChainhookStore) {
    let config: EventObserverConfig = EventObserverConfig {
        registered_chainhooks: ChainhookStore::new(),
        predicates_config: PredicatesConfig::default(),
        bitcoin_rpc_proxy_enabled: false,
        bitcoind_rpc_username: "user".into(),
        bitcoind_rpc_password: "user".into(),
        bitcoind_rpc_url: "http://localhost:18443".into(),
        display_stacks_ingestion_logs: false,
        bitcoin_block_signaling: BitcoinBlockSignaling::Stacks(
            StacksNodeConfig::default_localhost(DEFAULT_INGESTION_PORT),
        ),
        bitcoin_network: BitcoinNetwork::Regtest,
        stacks_network: StacksNetwork::Devnet,
        prometheus_monitoring_port: None,
    };
    (config, ChainhookStore::new())
}

fn stacks_chainhook_contract_call(
    id: u8,
    contract_identifier: &str,
    expire_after_occurrence: Option<u64>,
    method: &str,
) -> StacksChainhookSpecificationNetworkMap {
    let mut networks = BTreeMap::new();
    networks.insert(
        StacksNetwork::Devnet,
        StacksChainhookSpecification {
            start_block: None,
            end_block: None,
            blocks: None,
            expire_after_occurrence,
            capture_all_events: None,
            decode_clarity_values: Some(true),
            include_contract_abi: None,
            predicate: StacksPredicate::ContractCall(StacksContractCallBasedPredicate {
                contract_identifier: contract_identifier.to_string(),
                method: method.to_string(),
            }),
            action: HookAction::Noop,
        },
    );

    StacksChainhookSpecificationNetworkMap {
        uuid: format!("{}", id),
        name: format!("Chainhook {}", id),
        owner_uuid: None,
        networks,
        version: 1,
    }
}

fn bitcoin_chainhook_p2pkh(
    id: u8,
    address: &str,
    expire_after_occurrence: Option<u64>,
) -> BitcoinChainhookSpecificationNetworkMap {
    let mut networks = BTreeMap::new();
    networks.insert(
        BitcoinNetwork::Regtest,
        BitcoinChainhookSpecification {
            start_block: None,
            end_block: None,
            blocks: None,
            expire_after_occurrence,
            predicate: BitcoinPredicateType::Outputs(OutputPredicate::P2pkh(
                ExactMatchingRule::Equals(address.to_string()),
            )),
            action: HookAction::Noop,
            include_proof: None,
            include_inputs: None,
            include_outputs: None,
            include_witness: None,
        },
    );

    BitcoinChainhookSpecificationNetworkMap {
        uuid: format!("{}", id),
        name: format!("Chainhook {}", id),
        owner_uuid: None,
        version: 1,
        networks,
    }
}

fn generate_and_register_new_stacks_chainhook(
    observer_commands_tx: &Sender<ObserverCommand>,
    observer_events_rx: &crossbeam_channel::Receiver<ObserverEvent>,
    id: u8,
    contract_name: &str,
    method: &str,
) -> StacksChainhookInstance {
    let contract_identifier = format!("{}.{}", accounts::deployer_stx_address(), contract_name);
    let chainhook = stacks_chainhook_contract_call(id, &contract_identifier, None, method);
    let _ = observer_commands_tx.send(ObserverCommand::RegisterPredicate(
        ChainhookSpecificationNetworkMap::Stacks(chainhook.clone()),
    ));
    let mut chainhook = chainhook
        .into_specification_for_network(&StacksNetwork::Devnet)
        .unwrap();
    chainhook.enabled = true;
    let _ = observer_commands_tx.send(ObserverCommand::EnablePredicate(ChainhookInstance::Stacks(
        chainhook.clone(),
    )));
    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::PredicateRegistered(_)) => {
            // assert_eq!(
            //     ChainhookSpecification::Stacks(chainhook.clone()),
            //     registered_chainhook
            // );
            true
        }
        _ => false,
    });
    let _ = observer_commands_tx.send(ObserverCommand::EnablePredicate(ChainhookInstance::Stacks(
        chainhook.clone(),
    )));
    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::PredicateEnabled(_)) => {
            // assert_eq!(
            //     ChainhookSpecification::Bitcoin(chainhook.clone()),
            //     registered_chainhook
            // );
            true
        }
        _ => false,
    });
    chainhook
}

fn generate_and_register_new_bitcoin_chainhook(
    observer_commands_tx: &Sender<ObserverCommand>,
    observer_events_rx: &crossbeam_channel::Receiver<ObserverEvent>,
    id: u8,
    p2pkh_address: &str,
    expire_after_occurrence: Option<u64>,
) -> BitcoinChainhookInstance {
    let chainhook = bitcoin_chainhook_p2pkh(id, p2pkh_address, expire_after_occurrence);
    let _ = observer_commands_tx.send(ObserverCommand::RegisterPredicate(
        ChainhookSpecificationNetworkMap::Bitcoin(chainhook.clone()),
    ));
    let mut chainhook = chainhook
        .into_specification_for_network(&BitcoinNetwork::Regtest)
        .unwrap();
    chainhook.enabled = true;
    let _ = observer_commands_tx.send(ObserverCommand::EnablePredicate(
        ChainhookInstance::Bitcoin(chainhook.clone()),
    ));
    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::PredicateRegistered(_)) => {
            // assert_eq!(
            //     ChainhookSpecification::Bitcoin(chainhook.clone()),
            //     registered_chainhook
            // );
            true
        }
        _ => false,
    });
    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::PredicateEnabled(_)) => {
            // assert_eq!(
            //     ChainhookSpecification::Bitcoin(chainhook.clone()),
            //     registered_chainhook
            // );
            true
        }
        _ => false,
    });
    chainhook
}

fn assert_predicates_triggered_event(
    observer_events_rx: &crossbeam_channel::Receiver<ObserverEvent>,
    expected_len: usize,
) {
    assert!(
        match observer_events_rx.recv() {
            Ok(ObserverEvent::PredicatesTriggered(len)) => {
                assert_eq!(
                    len, expected_len,
                    "expected {} predicate(s) to be triggered",
                    expected_len
                );
                true
            }
            _ => false,
        },
        "expected PredicatesTriggered event to occur"
    );
}

fn assert_stacks_chain_event(observer_events_rx: &crossbeam_channel::Receiver<ObserverEvent>) {
    assert!(
        matches!(
            observer_events_rx.recv(),
            Ok(ObserverEvent::StacksChainEvent(_))
        ),
        "expected StacksChainEvent event to occur"
    );
}

#[test]
fn test_stacks_chainhook_register_deregister() {
    let (observer_commands_tx, observer_commands_rx) = channel();
    let (observer_events_tx, observer_events_rx) = crossbeam_channel::unbounded();

    let handle = std::thread::spawn(move || {
        let (config, chainhook_store) = generate_test_config();
        let _ = hiro_system_kit::nestable_block_on(start_observer_commands_handler(
            config,
            chainhook_store,
            observer_commands_rx,
            Some(observer_events_tx),
            None,
            None,
            Context::empty(),
        ));
    });

    // Create and register a new chainhook
    let chainhook = generate_and_register_new_stacks_chainhook(
        &observer_commands_tx,
        &observer_events_rx,
        1,
        "counter",
        "increment",
    );

    // Simulate a block that does not include a trigger
    let transactions = vec![generate_test_tx_stacks_contract_call(
        0,
        &accounts::wallet_1_stx_address(),
        "counter",
        "decrement",
        vec!["u1"],
    )];
    let chain_event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks: vec![StacksBlockUpdate::new(
            stacks_blocks::generate_test_stacks_block(0, 1, transactions, None).expect_block(),
        )],
        confirmed_blocks: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event));
    // Should signal that no hook were triggered
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::PredicateEnabled(_))
    ));

    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 0);
    // Should propagate block
    assert_stacks_chain_event(&observer_events_rx);

    // Simulate a block that does include a trigger
    let transactions = vec![generate_test_tx_stacks_contract_call(
        1,
        &accounts::wallet_1_stx_address(),
        "counter",
        "increment",
        vec!["u1"],
    )];
    let chain_event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks: vec![StacksBlockUpdate::new(
            stacks_blocks::generate_test_stacks_block(0, 2, transactions, None).expect_block(),
        )],
        confirmed_blocks: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event));
    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 1);

    assert!(
        match observer_events_rx.recv() {
            Ok(ObserverEvent::StacksPredicateTriggered(payload)) => {
                assert_eq!(
                    payload.apply.len(),
                    1,
                    "expected 1 predicate to be triggered"
                );
                assert_eq!(
                    payload.apply[0].transactions.len(),
                    1,
                    "expected triggered predicate to have 1 transaction"
                );
                true
            }
            _ => false,
        },
        "expected StacksPredicateTriggered event to occur"
    );

    // Should propagate block
    assert_stacks_chain_event(&observer_events_rx);

    // Simulate a block that does include 2 trigger
    let transactions = vec![
        generate_test_tx_stacks_contract_call(
            1,
            &accounts::wallet_1_stx_address(),
            "counter",
            "increment",
            vec!["u1"],
        ),
        generate_test_tx_stacks_contract_call(
            2,
            &accounts::wallet_2_stx_address(),
            "counter",
            "increment",
            vec!["u2"],
        ),
        generate_test_tx_stacks_contract_call(
            3,
            &accounts::wallet_3_stx_address(),
            "counter",
            "decrement",
            vec!["u2"],
        ),
    ];
    let chain_event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks: vec![StacksBlockUpdate::new(
            stacks_blocks::generate_test_stacks_block(0, 2, transactions, None).expect_block(),
        )],
        confirmed_blocks: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event));
    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 1);

    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::StacksPredicateTriggered(payload)) => {
            assert_eq!(payload.apply.len(), 1);
            assert_eq!(payload.apply[0].transactions.len(), 2);
            true
        }
        _ => false,
    });

    // Should propagate block
    assert_stacks_chain_event(&observer_events_rx);

    // Deregister the hook
    let _ = observer_commands_tx.send(ObserverCommand::DeregisterStacksPredicate(
        chainhook.uuid.clone(),
    ));
    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::PredicateDeregistered(PredicateDeregisteredEvent {
            predicate_uuid: deregistered_chainhook,
            ..
        })) => {
            assert_eq!(chainhook.uuid, deregistered_chainhook);
            true
        }
        _ => false,
    });

    // Simulate a block that does not include a trigger
    let transactions = vec![generate_test_tx_stacks_contract_call(
        2,
        &accounts::wallet_1_stx_address(),
        "counter",
        "decrement",
        vec!["u1"],
    )];
    let chain_event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks: vec![StacksBlockUpdate::new(
            stacks_blocks::generate_test_stacks_block(0, 2, transactions, None).expect_block(),
        )],
        confirmed_blocks: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event));
    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 0);
    // Should propagate block
    assert_stacks_chain_event(&observer_events_rx);

    // Simulate a block that does include a trigger
    let transactions = vec![generate_test_tx_stacks_contract_call(
        3,
        &accounts::wallet_1_stx_address(),
        "counter",
        "increment",
        vec!["u1"],
    )];
    let chain_event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks: vec![StacksBlockUpdate::new(
            stacks_blocks::generate_test_stacks_block(0, 3, transactions, None).expect_block(),
        )],
        confirmed_blocks: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event));
    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 0);
    // Should propagate block
    assert_stacks_chain_event(&observer_events_rx);

    let _ = observer_commands_tx.send(ObserverCommand::Terminate);
    handle.join().expect("unable to terminate thread");
}

#[test]
fn test_stacks_chainhook_auto_deregister() {
    let (observer_commands_tx, observer_commands_rx) = channel();
    let (observer_events_tx, observer_events_rx) = crossbeam_channel::unbounded();

    let handle = std::thread::spawn(move || {
        let (config, chainhook_store) = generate_test_config();
        let _ = hiro_system_kit::nestable_block_on(start_observer_commands_handler(
            config,
            chainhook_store,
            observer_commands_rx,
            Some(observer_events_tx),
            None,
            None,
            Context::empty(),
        ));
    });

    // Create and register a new chainhook
    let contract_identifier = format!("{}.{}", accounts::deployer_stx_address(), "counter");
    let chainhook = stacks_chainhook_contract_call(0, &contract_identifier, Some(1), "increment");
    let _ = observer_commands_tx.send(ObserverCommand::RegisterPredicate(
        ChainhookSpecificationNetworkMap::Stacks(chainhook.clone()),
    ));
    let mut chainhook = chainhook
        .into_specification_for_network(&StacksNetwork::Devnet)
        .unwrap();
    chainhook.enabled = true;
    let _ = observer_commands_tx.send(ObserverCommand::EnablePredicate(ChainhookInstance::Stacks(
        chainhook.clone(),
    )));
    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::PredicateRegistered(_)) => {
            // assert_eq!(
            //     ChainhookSpecification::Stacks(chainhook.clone()),
            //     registered_chainhook
            // );
            true
        }
        _ => false,
    });

    // Simulate a block that does not include a trigger
    let transactions = vec![generate_test_tx_stacks_contract_call(
        0,
        &accounts::wallet_1_stx_address(),
        "counter",
        "decrement",
        vec!["u1"],
    )];
    let chain_event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks: vec![StacksBlockUpdate::new(
            stacks_blocks::generate_test_stacks_block(0, 1, transactions, None).expect_block(),
        )],
        confirmed_blocks: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event));
    // Should signal that no hook were triggered
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::PredicateEnabled(_))
    ));

    assert!(
        match observer_events_rx.recv() {
            Ok(ObserverEvent::PredicatesTriggered(len)) => {
                assert_eq!(len, 0);
                true
            }
            Ok(e) => {
                println!("{:?}", e);
                true
            }
            _ => false,
        },
        "expected PredicatesTriggered event to occur"
    );
    // Should propagate block
    assert_stacks_chain_event(&observer_events_rx);

    // Simulate a block that does include a trigger
    let transactions = vec![generate_test_tx_stacks_contract_call(
        1,
        &accounts::wallet_1_stx_address(),
        "counter",
        "increment",
        vec!["u1"],
    )];
    let chain_event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks: vec![StacksBlockUpdate::new(
            stacks_blocks::generate_test_stacks_block(0, 2, transactions, None).expect_block(),
        )],
        confirmed_blocks: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event));
    // Should signal that hooks were triggered
    assert_predicates_triggered_event(&observer_events_rx, 1);

    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::StacksPredicateTriggered(_))
    ));

    // Should propagate block
    assert_stacks_chain_event(&observer_events_rx);

    // Simulate another block that does include a trigger
    let transactions = vec![generate_test_tx_stacks_contract_call(
        3,
        &accounts::wallet_1_stx_address(),
        "counter",
        "increment",
        vec!["u1"],
    )];
    let chain_event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks: vec![StacksBlockUpdate::new(
            stacks_blocks::generate_test_stacks_block(0, 3, transactions, None).expect_block(),
        )],
        confirmed_blocks: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateStacksChainEvent(chain_event));
    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 0);

    // Should signal that a hook was deregistered
    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::PredicateDeregistered(PredicateDeregisteredEvent {
            predicate_uuid: deregistered_hook,
            ..
        })) => {
            assert_eq!(deregistered_hook, chainhook.uuid);
            true
        }
        _ => false,
    });

    // Should propagate block
    assert_stacks_chain_event(&observer_events_rx);

    let _ = observer_commands_tx.send(ObserverCommand::Terminate);
    handle.join().expect("unable to terminate thread");
}

#[test]
fn test_bitcoin_chainhook_register_deregister() {
    let (observer_commands_tx, observer_commands_rx) = channel();
    let (observer_events_tx, observer_events_rx) = crossbeam_channel::unbounded();

    let handle = std::thread::spawn(move || {
        let (config, chainhook_store) = generate_test_config();
        let _ = hiro_system_kit::nestable_block_on(start_observer_commands_handler(
            config,
            chainhook_store,
            observer_commands_rx,
            Some(observer_events_tx),
            None,
            None,
            Context::empty(),
        ));
    });

    // Create and register a new chainhook (wallet_2 received some sats)
    let chainhook = generate_and_register_new_bitcoin_chainhook(
        &observer_commands_tx,
        &observer_events_rx,
        1,
        &accounts::wallet_2_btc_address(),
        None,
    );

    // Simulate a block that does not include a trigger (wallet_1 to wallet_3)
    let transactions = vec![generate_test_tx_bitcoin_p2pkh_transfer(
        0,
        &accounts::wallet_1_btc_address(),
        &accounts::wallet_3_btc_address(),
        3,
    )];
    let block = bitcoin_blocks::generate_test_bitcoin_block(0, 1, transactions, None);
    let _ = observer_commands_tx.send(ObserverCommand::CacheBitcoinBlock(block.clone()));
    let chain_event = BlockchainEvent::BlockchainUpdatedWithHeaders(BlockchainUpdatedWithHeaders {
        new_headers: vec![block.get_header()],
        confirmed_headers: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateBitcoinChainEvent(chain_event));
    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 0);

    // Should propagate block
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::BitcoinChainEvent(_))
    ));

    // Simulate a block that does include a trigger (wallet_1 to wallet_2)
    let transactions = vec![generate_test_tx_bitcoin_p2pkh_transfer(
        0,
        &accounts::wallet_1_btc_address(),
        &accounts::wallet_2_btc_address(),
        3,
    )];
    let block = bitcoin_blocks::generate_test_bitcoin_block(0, 2, transactions, None);
    let _ = observer_commands_tx.send(ObserverCommand::CacheBitcoinBlock(block.clone()));
    let chain_event = BlockchainEvent::BlockchainUpdatedWithHeaders(BlockchainUpdatedWithHeaders {
        new_headers: vec![block.get_header()],
        confirmed_headers: vec![],
    });

    let _ = observer_commands_tx.send(ObserverCommand::PropagateBitcoinChainEvent(chain_event));
    // Should signal that 1 hook was triggered
    assert_predicates_triggered_event(&observer_events_rx, 1);

    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::BitcoinPredicateTriggered(payload)) => {
            assert_eq!(payload.apply.len(), 1);
            assert_eq!(payload.apply[0].block.transactions.len(), 1);
            true
        }
        _ => false,
    });

    // Should propagate block
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::BitcoinChainEvent(_))
    ));

    // Simulate a block that does include a trigger (wallet_1 to wallet_2)
    let transactions = vec![
        generate_test_tx_bitcoin_p2pkh_transfer(
            0,
            &accounts::wallet_1_btc_address(),
            &accounts::wallet_2_btc_address(),
            3,
        ),
        generate_test_tx_bitcoin_p2pkh_transfer(
            1,
            &accounts::wallet_3_btc_address(),
            &accounts::wallet_2_btc_address(),
            5,
        ),
        generate_test_tx_bitcoin_p2pkh_transfer(
            1,
            &accounts::wallet_3_btc_address(),
            &accounts::wallet_1_btc_address(),
            5,
        ),
    ];
    let block = bitcoin_blocks::generate_test_bitcoin_block(0, 2, transactions, None);
    let _ = observer_commands_tx.send(ObserverCommand::CacheBitcoinBlock(block.clone()));
    let chain_event = BlockchainEvent::BlockchainUpdatedWithHeaders(BlockchainUpdatedWithHeaders {
        new_headers: vec![block.get_header()],
        confirmed_headers: vec![],
    });

    let _ = observer_commands_tx.send(ObserverCommand::PropagateBitcoinChainEvent(chain_event));
    // Should signal that 1 hook was triggered
    assert_predicates_triggered_event(&observer_events_rx, 1);

    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::BitcoinPredicateTriggered(payload)) => {
            assert_eq!(payload.apply.len(), 1);
            assert_eq!(payload.apply[0].block.transactions.len(), 2);
            true
        }
        _ => false,
    });

    // Should propagate block
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::BitcoinChainEvent(_))
    ));

    // Deregister the hook
    let _ = observer_commands_tx.send(ObserverCommand::DeregisterBitcoinPredicate(
        chainhook.uuid.clone(),
    ));
    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::PredicateDeregistered(PredicateDeregisteredEvent {
            predicate_uuid: deregistered_chainhook,
            ..
        })) => {
            assert_eq!(chainhook.uuid, deregistered_chainhook);
            true
        }
        _ => false,
    });

    // Simulate a block that does not include a trigger
    let transactions = vec![generate_test_tx_bitcoin_p2pkh_transfer(
        2,
        &accounts::wallet_1_btc_address(),
        &accounts::wallet_3_btc_address(),
        1,
    )];
    let block = bitcoin_blocks::generate_test_bitcoin_block(0, 2, transactions, None);
    let _ = observer_commands_tx.send(ObserverCommand::CacheBitcoinBlock(block.clone()));
    let chain_event = BlockchainEvent::BlockchainUpdatedWithHeaders(BlockchainUpdatedWithHeaders {
        new_headers: vec![block.get_header()],
        confirmed_headers: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateBitcoinChainEvent(chain_event));

    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 0);

    // Should propagate block
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::BitcoinChainEvent(_))
    ));

    // Simulate a block that does include a trigger
    let transactions = vec![generate_test_tx_bitcoin_p2pkh_transfer(
        3,
        &accounts::wallet_1_btc_address(),
        &accounts::wallet_2_btc_address(),
        1,
    )];
    let block = bitcoin_blocks::generate_test_bitcoin_block(0, 3, transactions, None);
    let _ = observer_commands_tx.send(ObserverCommand::CacheBitcoinBlock(block.clone()));
    let chain_event = BlockchainEvent::BlockchainUpdatedWithHeaders(BlockchainUpdatedWithHeaders {
        new_headers: vec![block.get_header()],
        confirmed_headers: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateBitcoinChainEvent(chain_event));
    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 0);

    // Should propagate block
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::BitcoinChainEvent(_))
    ));

    let _ = observer_commands_tx.send(ObserverCommand::Terminate);
    handle.join().expect("unable to terminate thread");
}

#[test]
fn test_bitcoin_chainhook_auto_deregister() {
    let (observer_commands_tx, observer_commands_rx) = channel();
    let (observer_events_tx, observer_events_rx) = crossbeam_channel::unbounded();

    let handle = std::thread::spawn(move || {
        let (config, chainhook_store) = generate_test_config();
        let _ = hiro_system_kit::nestable_block_on(start_observer_commands_handler(
            config,
            chainhook_store,
            observer_commands_rx,
            Some(observer_events_tx),
            None,
            None,
            Context::empty(),
        ));
    });

    // Create and register a new chainhook (wallet_2 received some sats)
    let chainhook = generate_and_register_new_bitcoin_chainhook(
        &observer_commands_tx,
        &observer_events_rx,
        1,
        &accounts::wallet_2_btc_address(),
        Some(1),
    );

    // Simulate a block that does not include a trigger (wallet_1 to wallet_3)
    let transactions = vec![generate_test_tx_bitcoin_p2pkh_transfer(
        0,
        &accounts::wallet_1_btc_address(),
        &accounts::wallet_3_btc_address(),
        3,
    )];
    let block = bitcoin_blocks::generate_test_bitcoin_block(0, 1, transactions, None);
    let _ = observer_commands_tx.send(ObserverCommand::CacheBitcoinBlock(block.clone()));
    let chain_event = BlockchainEvent::BlockchainUpdatedWithHeaders(BlockchainUpdatedWithHeaders {
        new_headers: vec![block.get_header()],
        confirmed_headers: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateBitcoinChainEvent(chain_event));

    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 0);

    // Should propagate block
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::BitcoinChainEvent(_))
    ));

    // Simulate a block that does include a trigger (wallet_1 to wallet_2)
    let transactions = vec![generate_test_tx_bitcoin_p2pkh_transfer(
        0,
        &accounts::wallet_1_btc_address(),
        &accounts::wallet_2_btc_address(),
        3,
    )];

    let block = bitcoin_blocks::generate_test_bitcoin_block(0, 2, transactions, None);
    let _ = observer_commands_tx.send(ObserverCommand::CacheBitcoinBlock(block.clone()));
    let chain_event = BlockchainEvent::BlockchainUpdatedWithHeaders(BlockchainUpdatedWithHeaders {
        new_headers: vec![block.get_header()],
        confirmed_headers: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateBitcoinChainEvent(chain_event));

    // Should signal that 1 hook was triggered
    assert_predicates_triggered_event(&observer_events_rx, 1);

    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::BitcoinPredicateTriggered(_))
    ));

    // Should propagate block
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::BitcoinChainEvent(_))
    ));

    // Simulate a block that does not include a trigger
    let transactions = vec![generate_test_tx_bitcoin_p2pkh_transfer(
        2,
        &accounts::wallet_1_btc_address(),
        &accounts::wallet_3_btc_address(),
        1,
    )];

    let block = bitcoin_blocks::generate_test_bitcoin_block(0, 2, transactions, None);
    let _ = observer_commands_tx.send(ObserverCommand::CacheBitcoinBlock(block.clone()));
    let chain_event = BlockchainEvent::BlockchainUpdatedWithHeaders(BlockchainUpdatedWithHeaders {
        new_headers: vec![block.get_header()],
        confirmed_headers: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateBitcoinChainEvent(chain_event));

    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 0);

    // Should propagate block
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::BitcoinChainEvent(_))
    ));

    // Simulate a block that does include a trigger
    let transactions = vec![generate_test_tx_bitcoin_p2pkh_transfer(
        3,
        &accounts::wallet_1_btc_address(),
        &accounts::wallet_2_btc_address(),
        1,
    )];

    let block = bitcoin_blocks::generate_test_bitcoin_block(0, 3, transactions, None);
    let _ = observer_commands_tx.send(ObserverCommand::CacheBitcoinBlock(block.clone()));
    let chain_event = BlockchainEvent::BlockchainUpdatedWithHeaders(BlockchainUpdatedWithHeaders {
        new_headers: vec![block.get_header()],
        confirmed_headers: vec![],
    });
    let _ = observer_commands_tx.send(ObserverCommand::PropagateBitcoinChainEvent(chain_event));

    // Should signal that no hook were triggered
    assert_predicates_triggered_event(&observer_events_rx, 0);

    // Should signal that a hook was deregistered
    assert!(match observer_events_rx.recv() {
        Ok(ObserverEvent::PredicateDeregistered(PredicateDeregisteredEvent {
            predicate_uuid: deregistered_hook,
            ..
        })) => {
            assert_eq!(deregistered_hook, chainhook.uuid);
            true
        }
        _ => false,
    });

    // Should propagate block
    assert!(matches!(
        observer_events_rx.recv(),
        Ok(ObserverEvent::BitcoinChainEvent(_))
    ));

    let _ = observer_commands_tx.send(ObserverCommand::Terminate);
    handle.join().expect("unable to terminate thread");
}
