use std::collections::HashMap;

use assert_json_diff::assert_json_eq;
use serde_json::Value as JsonValue;
use test_case::test_case;

use self::fixtures::get_all_event_payload_types;
use super::stacks::{
    evaluate_stacks_chainhooks_on_chain_event, handle_stacks_hook_action, StacksChainhookInstance,
    StacksChainhookOccurrence, StacksContractCallBasedPredicate, StacksContractDeploymentPredicate,
    StacksFtEventBasedPredicate, StacksNftEventBasedPredicate, StacksPredicate,
    StacksPrintEventBasedPredicate, StacksStxEventBasedPredicate, StacksTrait,
    StacksTriggerChainhook,
};
use super::types::{ExactMatchingRule, FileHook};
use crate::chainhook::hooks::stacks::serialize_stacks_payload_to_json;
use crate::chainhook::hooks::tests::fixtures::{
    get_expected_occurrence, get_test_event_payload_by_type,
};
use crate::chainhook::hooks::types::HookAction;
use crate::chainhook::observer::EventObserverConfig;
use crate::chainhook::types::{
    StacksBlockUpdate, StacksChainEvent, StacksChainUpdatedWithBlocksData, StacksNetwork,
    StacksTransactionData, StacksTransactionEvent, StacksTransactionEventPayload,
    StacksTransactionEventPosition,
};
use crate::chainhook::utils::{AbstractStacksBlock, Context};

pub mod fixtures;

// FtEvent predicate tests
#[test_case(
    vec![vec![get_test_event_payload_by_type("ft_mint")]],
    StacksPredicate::FtEvent(StacksFtEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["mint".to_string()]
    }),
    1;
    "FtEvent predicates match mint event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("ft_transfer")]],
    StacksPredicate::FtEvent(StacksFtEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["transfer".to_string()]
    }),
    1;
    "FtEvent predicates match transfer event"
)]
#[test_case(
    vec![vec![StacksTransactionEventPayload::FTTransferEvent(crate::chainhook::types::FTTransferEventData {
        sender: "".to_string(),
        asset_class_identifier: "different-id".to_string(),
        amount: "".to_string(),
        recipient: "".to_string(),
    }), StacksTransactionEventPayload::FTTransferEvent(crate::chainhook::types::FTTransferEventData {
        sender: "".to_string(),
        asset_class_identifier: "asset-id".to_string(),
        amount: "".to_string(),
        recipient: "".to_string(),
    })]],
    StacksPredicate::FtEvent(StacksFtEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["transfer".to_string()]
    }),
    1;
    "FtEvent predicates match transfer event if matching event is not first in transaction"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("ft_burn")]],
    StacksPredicate::FtEvent(StacksFtEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["burn".to_string()]
    }),
    1;
    "FtEvent predicates match burn event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("ft_mint")]],
    StacksPredicate::FtEvent(StacksFtEventBasedPredicate {
        asset_identifier: "wrong-id".to_string(),
        actions: vec!["mint".to_string()]
    }),
    0;
    "FtEvent predicates reject no-match asset id for mint event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("ft_transfer")]],
    StacksPredicate::FtEvent(StacksFtEventBasedPredicate {
        asset_identifier: "wrong-id".to_string(),
        actions: vec!["transfer".to_string()]
    }),
    0;
    "FtEvent predicates reject no-match asset id for transfer event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("ft_burn")]],
    StacksPredicate::FtEvent(StacksFtEventBasedPredicate {
        asset_identifier: "wrong-id".to_string(),
        actions: vec!["burn".to_string()]
    }),
    0;
    "FtEvent predicates reject no-match asset id for burn event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("ft_mint")],vec![get_test_event_payload_by_type("ft_transfer")],vec![get_test_event_payload_by_type("ft_burn")]],
    StacksPredicate::FtEvent(StacksFtEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["mint".to_string(),"transfer".to_string(), "burn".to_string()]
    }),
    3;
    "FtEvent predicates match multiple events"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("ft_transfer")],vec![get_test_event_payload_by_type("ft_burn")]],
    StacksPredicate::FtEvent(StacksFtEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["mint".to_string()]
    }),
    0;
    "FtEvent predicates don't match if missing event"
)]
// NftEvent predicate tests
#[test_case(
    vec![vec![get_test_event_payload_by_type("nft_mint")]],
    StacksPredicate::NftEvent(StacksNftEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["mint".to_string()]
    }),
    1;
    "NftEvent predicates match mint event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("nft_transfer")]],
    StacksPredicate::NftEvent(StacksNftEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["transfer".to_string()]
    }),
    1;
    "NftEvent predicates match transfer event"
)]
#[test_case(
    vec![vec![StacksTransactionEventPayload::NFTTransferEvent(crate::chainhook::types::NFTTransferEventData {
        sender: "".to_string(),
        asset_class_identifier: "different-id".to_string(),
        hex_asset_identifier: "different-id".to_string(),
        recipient: "".to_string(),
    }), StacksTransactionEventPayload::NFTTransferEvent(crate::chainhook::types::NFTTransferEventData {
        sender: "".to_string(),
        asset_class_identifier: "asset-id".to_string(),
        hex_asset_identifier: "asset-id".to_string(),
        recipient: "".to_string(),
    })]],
    StacksPredicate::NftEvent(StacksNftEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["transfer".to_string()]
    }),
    1;
    "NftEvent predicates match transfer event if matching event is not first in transaction"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("nft_burn")]],
    StacksPredicate::NftEvent(StacksNftEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["burn".to_string()]
    }),
    1;
    "NftEvent predicates match burn event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("nft_mint")]],
    StacksPredicate::NftEvent(StacksNftEventBasedPredicate {
        asset_identifier: "wrong-id".to_string(),
        actions: vec!["mint".to_string()]
    }),
    0;
    "NftEvent predicates reject no-match asset id for mint event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("nft_transfer")]],
    StacksPredicate::NftEvent(StacksNftEventBasedPredicate {
        asset_identifier: "wrong-id".to_string(),
        actions: vec!["transfer".to_string()]
    }),
    0;
    "NftEvent predicates reject no-match asset id for transfer event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("nft_burn")]],
    StacksPredicate::NftEvent(StacksNftEventBasedPredicate {
        asset_identifier: "wrong-id".to_string(),
        actions: vec!["burn".to_string()]
    }),
    0;
    "NftEvent predicates reject no-match asset id for burn event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("nft_mint")],vec![get_test_event_payload_by_type("nft_transfer")],vec![get_test_event_payload_by_type("nft_burn")]],
    StacksPredicate::NftEvent(StacksNftEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["mint".to_string(),"transfer".to_string(), "burn".to_string()]
    }),
    3;
    "NftEvent predicates match multiple events"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("nft_transfer")],vec![get_test_event_payload_by_type("nft_burn")]],
    StacksPredicate::NftEvent(StacksNftEventBasedPredicate {
        asset_identifier: "asset-id".to_string(),
        actions: vec!["mint".to_string()]
    }),
    0;
    "NftEvent predicates don't match if missing event"
)]
// StxEvent predicate tests
#[test_case(
    vec![vec![get_test_event_payload_by_type("stx_mint")]],
    StacksPredicate::StxEvent(StacksStxEventBasedPredicate {
        actions: vec!["mint".to_string()]
    }),
    1;
    "StxEvent predicates match mint event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("stx_transfer")]],
    StacksPredicate::StxEvent(StacksStxEventBasedPredicate {
        actions: vec!["transfer".to_string()]
    }),
    1;
    "StxEvent predicates match transfer event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("stx_lock")]],
    StacksPredicate::StxEvent(StacksStxEventBasedPredicate {
        actions: vec!["lock".to_string()]
    }),
    1;
    "StxEvent predicates match lock event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("stx_burn")]],
    StacksPredicate::StxEvent(StacksStxEventBasedPredicate {
        actions: vec!["burn".to_string()]
    }),
    1;
    "StxEvent predicates match burn event"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("stx_mint")],vec![get_test_event_payload_by_type("stx_transfer")],vec![get_test_event_payload_by_type("stx_lock")]],
    StacksPredicate::StxEvent(StacksStxEventBasedPredicate {
        actions: vec!["mint".to_string(), "transfer".to_string(), "lock".to_string()]
    }),
    3;
    "StxEvent predicates match multiple events"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("stx_transfer")],vec![get_test_event_payload_by_type("stx_lock")]],
    StacksPredicate::StxEvent(StacksStxEventBasedPredicate {
        actions: vec!["mint".to_string()]
    }),
    0;
    "StxEvent predicates don't match if missing event"
)]
// PrintEvent predicate tests
#[test_case(
    vec![vec![get_test_event_payload_by_type("smart_contract_print_event")]],
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::Contains {
        contract_identifier: "ST3AXH4EBHD63FCFPTZ8GR29TNTVWDYPGY0KDY5E5.loan-data".to_string(),
        contains: "some-value".to_string()
    }),
    1;
    "PrintEvent predicate matches contract_identifier and contains"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("smart_contract_not_print_event")]],
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::Contains {
        contract_identifier: "ST3AXH4EBHD63FCFPTZ8GR29TNTVWDYPGY0KDY5E5.loan-data".to_string(),
        contains: "some-value".to_string(),
    }),
    0;
    "PrintEvent predicate does not check events with topic other than print"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("smart_contract_print_event")]],
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::Contains {
        contract_identifier: "wront-id".to_string(),
        contains: "some-value".to_string(),
    }),
    0;
    "PrintEvent predicate rejects non matching contract_identifier"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("smart_contract_print_event")]],
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::Contains {
        contract_identifier:
            "ST3AXH4EBHD63FCFPTZ8GR29TNTVWDYPGY0KDY5E5.loan-data".to_string(),
        contains: "wrong-value".to_string(),
    }),
    0;
    "PrintEvent predicate rejects non matching contains value"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("smart_contract_print_event")]],
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::Contains {
        contract_identifier: "*".to_string(),
        contains: "some-value".to_string(),
    }),
    1;
    "PrintEvent predicate contract_identifier wildcard checks all print events for match"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("smart_contract_print_event")]],
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::Contains {
        contract_identifier: "ST3AXH4EBHD63FCFPTZ8GR29TNTVWDYPGY0KDY5E5.loan-data".to_string(),
        contains: "*".to_string(),
    }),
    1;
    "PrintEvent predicate contains wildcard matches all values for matching events"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("smart_contract_print_event")], vec![get_test_event_payload_by_type("smart_contract_print_event_empty")]],
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::Contains {
        contract_identifier: "*".to_string(),
        contains: "*".to_string(),
    }),
    2;
    "PrintEvent predicate contract_identifier wildcard and contains wildcard matches all values on all print events"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("smart_contract_print_event")]],
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::MatchesRegex {
        contract_identifier: "ST3AXH4EBHD63FCFPTZ8GR29TNTVWDYPGY0KDY5E5.loan-data".to_string(),
        regex: "(some)|(value)".to_string(),
    }),
    1;
    "PrintEvent predicate matches contract_identifier and regex"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("smart_contract_print_event")]],
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::MatchesRegex {
        contract_identifier: "*".to_string(),
        regex: "(some)|(value)".to_string(),
    }),
    1;
    "PrintEvent predicate contract_identifier wildcard checks all print events for match with regex"
)]
#[test_case(
    vec![vec![get_test_event_payload_by_type("smart_contract_print_event")]],
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::MatchesRegex {
        contract_identifier: "*".to_string(),
        regex: "[".to_string(),
    }),
    0
    ;
    "PrintEvent predicate does not match invalid regex"
)]
fn test_stacks_predicates(
    blocks_with_events: Vec<Vec<StacksTransactionEventPayload>>,
    predicate: StacksPredicate,
    expected_applies: u64,
) {
    // Prepare block
    let new_blocks = blocks_with_events
        .into_iter()
        .map(|payloads| StacksBlockUpdate {
            block: fixtures::build_stacks_testnet_block_from_smart_contract_event_data(
                payloads
                    .into_iter()
                    .enumerate()
                    .map(|(index, payload)| StacksTransactionEvent {
                        event_payload: payload,
                        position: StacksTransactionEventPosition {
                            index: index as u32,
                        },
                    })
                    .collect::<Vec<_>>()
                    .as_ref(),
            ),
            parent_microblocks_to_apply: vec![],
            parent_microblocks_to_rollback: vec![],
        })
        .collect();
    let event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks,
        confirmed_blocks: vec![],
    });
    // Prepare predicate
    let chainhook = StacksChainhookInstance {
        uuid: "".to_string(),
        owner_uuid: None,
        name: "".to_string(),
        network: StacksNetwork::Testnet,
        version: 1,
        blocks: None,
        start_block: None,
        end_block: None,
        expire_after_occurrence: None,
        capture_all_events: None,
        decode_clarity_values: None,
        include_contract_abi: None,
        predicate,
        action: HookAction::Noop,
        enabled: true,
        expired_at: None,
    };

    let predicates = vec![&chainhook];
    let (triggered, _predicates_evaluated, _expired) =
        evaluate_stacks_chainhooks_on_chain_event(&event, predicates, &Context::empty());

    if expected_applies == 0 {
        assert_eq!(triggered.len(), 0)
    } else {
        let actual_applies: u64 = triggered[0].apply.len().try_into().unwrap();
        assert_eq!(actual_applies, expected_applies);
    }
}

#[test_case(
    StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::Deployer("ST13F481SBR0R7Z6NMMH8YV2FJJYXA5JPA0AD3HP9".to_string())),
    1;
    "Deployer predicate matches by contract deployer"
)]
#[test_case(
    StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::Deployer("*".to_string())),
    1;
    "Deployer predicate wildcard deployer catches all occurrences"
)]
#[test_case(
    StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::Deployer("wrong-deployer".to_string())),
    0;
    "Deployer predicate does not match non-matching deployer"
)]
#[test_case(
    StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::ImplementTrait(StacksTrait::Sip09)),
    0;
    "ImplementSip predicate returns no values for Sip09"
)]
#[test_case(
    StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::ImplementTrait(StacksTrait::Sip10)),
    0;
    "ImplementSip predicate returns no values for Sip10"
)]
#[test_case(
    StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::ImplementTrait(StacksTrait::Any)),
    0;
    "ImplementSip predicate returns no values for Any"
)]
fn test_stacks_predicate_contract_deploy(predicate: StacksPredicate, expected_applies: u64) {
    // Prepare block
    let new_blocks = vec![
        StacksBlockUpdate {
            block: fixtures::build_stacks_testnet_block_with_contract_deployment(),
            parent_microblocks_to_apply: vec![],
            parent_microblocks_to_rollback: vec![],
        },
        StacksBlockUpdate {
            block: fixtures::build_stacks_testnet_block_with_contract_call(),
            parent_microblocks_to_apply: vec![],
            parent_microblocks_to_rollback: vec![],
        },
    ];
    let event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks,
        confirmed_blocks: vec![],
    });
    // Prepare predicate
    let chainhook = StacksChainhookInstance {
        uuid: "".to_string(),
        owner_uuid: None,
        name: "".to_string(),
        network: StacksNetwork::Testnet,
        version: 1,
        blocks: None,
        start_block: None,
        end_block: None,
        expire_after_occurrence: None,
        capture_all_events: None,
        decode_clarity_values: None,
        include_contract_abi: None,
        predicate,
        action: HookAction::Noop,
        enabled: true,
        expired_at: None,
    };

    let predicates = vec![&chainhook];
    let (triggered, _predicates_evaluated, _predicates_expired) =
        evaluate_stacks_chainhooks_on_chain_event(&event, predicates, &Context::empty());

    if expected_applies == 0 {
        assert_eq!(triggered.len(), 0)
    } else if triggered.is_empty() {
        panic!("expected more than one block to be applied, but no predicates were triggered")
    } else {
        let actual_applies: u64 = triggered[0].apply.len().try_into().unwrap();
        assert_eq!(actual_applies, expected_applies);
    }
}

#[test]
fn verify_optional_addition_of_contract_abi() {
    // "mine" two blocks
    //  - one contract deploy (which should have a contract abi) and
    //  - one contract call (which should not)
    let new_blocks = vec![
        StacksBlockUpdate {
            block: fixtures::build_stacks_testnet_block_with_contract_deployment(),
            parent_microblocks_to_apply: vec![],
            parent_microblocks_to_rollback: vec![],
        },
        StacksBlockUpdate {
            block: fixtures::build_stacks_testnet_block_with_contract_call(),
            parent_microblocks_to_apply: vec![],
            parent_microblocks_to_rollback: vec![],
        },
    ];
    let event: StacksChainEvent =
        StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
            new_blocks,
            confirmed_blocks: vec![],
        });
    let mut contract_deploy_chainhook = StacksChainhookInstance {
        uuid: "contract-deploy".to_string(),
        owner_uuid: None,
        name: "".to_string(),
        network: StacksNetwork::Testnet,
        version: 1,
        blocks: None,
        start_block: None,
        end_block: None,
        expire_after_occurrence: None,
        capture_all_events: None,
        decode_clarity_values: None,
        include_contract_abi: Some(true),
        predicate: StacksPredicate::ContractDeployment(
            StacksContractDeploymentPredicate::Deployer("*".to_string()),
        ),
        action: HookAction::Noop,
        enabled: true,
        expired_at: None,
    };
    let contract_call_chainhook = StacksChainhookInstance {
        uuid: "contract-call".to_string(),
        owner_uuid: None,
        name: "".to_string(),
        network: StacksNetwork::Testnet,
        version: 1,
        blocks: None,
        start_block: None,
        end_block: None,
        expire_after_occurrence: None,
        capture_all_events: None,
        decode_clarity_values: None,
        include_contract_abi: Some(true),
        predicate: StacksPredicate::ContractCall(StacksContractCallBasedPredicate {
            contract_identifier: "ST13F481SBR0R7Z6NMMH8YV2FJJYXA5JPA0AD3HP9.subnet-v1".to_string(),
            method: "commit-block".to_string(),
        }),
        action: HookAction::Noop,
        enabled: true,
        expired_at: None,
    };

    let predicates = vec![&contract_deploy_chainhook, &contract_call_chainhook];
    let (triggered, _blocks, _) =
        evaluate_stacks_chainhooks_on_chain_event(&event, predicates, &Context::empty());
    assert_eq!(triggered.len(), 2);

    for t in triggered.into_iter() {
        let result = serialize_stacks_payload_to_json(t, &HashMap::new(), &Context::empty());
        let result = result.as_object().unwrap();
        let uuid = result.get("chainhook").unwrap().get("uuid").unwrap();
        let apply_blocks = result.get("apply").unwrap();
        for block in apply_blocks.as_array().unwrap() {
            let transactions = block.get("transactions").unwrap();
            for transaction in transactions.as_array().unwrap() {
                let contract_abi = transaction.get("metadata").unwrap().get("contract_abi");
                if uuid == "contract-call" {
                    assert_eq!(contract_abi, None);
                } else if uuid == "contract-deploy" {
                    assert!(contract_abi.is_some())
                } else {
                    unreachable!()
                }
            }
        }
    }
    contract_deploy_chainhook.include_contract_abi = Some(false);
    let predicates = vec![&contract_deploy_chainhook, &contract_call_chainhook];
    let (triggered, _blocks, _) =
        evaluate_stacks_chainhooks_on_chain_event(&event, predicates, &Context::empty());
    assert_eq!(triggered.len(), 2);

    for t in triggered.into_iter() {
        let result = serialize_stacks_payload_to_json(t, &HashMap::new(), &Context::empty());
        let result = result.as_object().unwrap();
        let apply_blocks = result.get("apply").unwrap();
        for block in apply_blocks.as_array().unwrap() {
            let transactions = block.get("transactions").unwrap();
            for transaction in transactions.as_array().unwrap() {
                let contract_abi = transaction.get("metadata").unwrap().get("contract_abi");
                assert_eq!(contract_abi, None);
            }
        }
    }
}

#[test_case(
    StacksPredicate::ContractCall(StacksContractCallBasedPredicate {
        contract_identifier: "ST13F481SBR0R7Z6NMMH8YV2FJJYXA5JPA0AD3HP9.subnet-v1".to_string(),
        method: "commit-block".to_string()
    }),
    1;
    "ContractCall predicate matches by contract identifier and method"
)]
#[test_case(
    StacksPredicate::ContractCall(StacksContractCallBasedPredicate {
        contract_identifier: "ST13F481SBR0R7Z6NMMH8YV2FJJYXA5JPA0AD3HP9.subnet-v1".to_string(),
        method: "wrong-method".to_string()
    }),
    0;
    "ContractCall predicate does not match for wrong method"
)]
#[test_case(
    StacksPredicate::ContractCall(StacksContractCallBasedPredicate {
        contract_identifier: "wrong-id".to_string(),
        method: "commit-block".to_string()
    }),
    0;
    "ContractCall predicate does not match for wrong contract identifier"
)]
#[test_case(
    StacksPredicate::Txid(ExactMatchingRule::Equals("0xb92c2ade84a8b85f4c72170680ae42e65438aea4db72ba4b2d6a6960f4141ce8".to_string())),
    1;
    "Txid predicate matches by a transaction's id"
)]
#[test_case(
    StacksPredicate::Txid(ExactMatchingRule::Equals("wrong-id".to_string())),
    0;
    "Txid predicate rejects non matching id"
)]
fn test_stacks_predicate_contract_call(predicate: StacksPredicate, expected_applies: u64) {
    // Prepare block
    let new_blocks = vec![
        StacksBlockUpdate {
            block: fixtures::build_stacks_testnet_block_with_contract_call(),
            parent_microblocks_to_apply: vec![],
            parent_microblocks_to_rollback: vec![],
        },
        StacksBlockUpdate {
            block: fixtures::build_stacks_testnet_block_with_contract_deployment(),
            parent_microblocks_to_apply: vec![],
            parent_microblocks_to_rollback: vec![],
        },
    ];
    let event = StacksChainEvent::ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData {
        new_blocks,
        confirmed_blocks: vec![],
    });
    // Prepare predicate
    let chainhook = StacksChainhookInstance {
        uuid: "".to_string(),
        owner_uuid: None,
        name: "".to_string(),
        network: StacksNetwork::Testnet,
        version: 1,
        blocks: None,
        start_block: None,
        end_block: None,
        expire_after_occurrence: None,
        capture_all_events: None,
        decode_clarity_values: None,
        include_contract_abi: None,
        predicate,
        action: HookAction::Noop,
        enabled: true,
        expired_at: None,
    };

    let predicates = vec![&chainhook];
    let (triggered, _predicates_evaluated, _predicates_expired) =
        evaluate_stacks_chainhooks_on_chain_event(&event, predicates, &Context::empty());

    if expected_applies == 0 {
        assert_eq!(triggered.len(), 0)
    } else if triggered.is_empty() {
        panic!("expected more than one block to be applied, but no predicates were triggered")
    } else {
        let actual_applies: u64 = triggered[0].apply.len().try_into().unwrap();
        assert_eq!(actual_applies, expected_applies);
    }
}

#[test]
fn test_stacks_hook_action_noop() {
    let chainhook = StacksChainhookInstance {
        uuid: "".to_string(),
        owner_uuid: None,
        name: "".to_string(),
        network: StacksNetwork::Testnet,
        version: 1,
        blocks: None,
        start_block: None,
        end_block: None,
        expire_after_occurrence: None,
        capture_all_events: None,
        decode_clarity_values: None,
        include_contract_abi: None,
        predicate: StacksPredicate::Txid(ExactMatchingRule::Equals(
            "0xb92c2ade84a8b85f4c72170680ae42e65438aea4db72ba4b2d6a6960f4141ce8".to_string(),
        )),
        action: HookAction::Noop,
        enabled: true,
        expired_at: None,
    };

    let apply_block_data = fixtures::build_stacks_testnet_block_with_contract_call();
    let apply_transactions = apply_block_data.transactions.iter().collect();
    let apply_blocks: &dyn AbstractStacksBlock = &apply_block_data;

    let rollback_block_data = fixtures::build_stacks_testnet_block_with_contract_deployment();
    let rollback_transactions = rollback_block_data.transactions.iter().collect();
    let rollback_blocks: &dyn AbstractStacksBlock = &apply_block_data;
    let trigger = StacksTriggerChainhook {
        chainhook: &chainhook,
        apply: vec![(apply_transactions, apply_blocks)],
        rollback: vec![(rollback_transactions, rollback_blocks)],
        events: vec![],
    };

    let proofs = HashMap::new();
    let ctx = Context {
        logger: None,
        tracer: false,
    };
    let occurrence =
        handle_stacks_hook_action(trigger, &proofs, &EventObserverConfig::default(), &ctx).unwrap();
    if let StacksChainhookOccurrence::Data(data) = occurrence {
        assert_eq!(data.apply.len(), 1);
        assert_eq!(
            data.apply[0].block_identifier.hash,
            apply_block_data.block_identifier.hash
        );
        assert_eq!(data.rollback.len(), 1);
        assert_eq!(
            data.rollback[0].block_identifier.hash,
            rollback_block_data.block_identifier.hash
        );
    } else {
        panic!("wrong occurrence type");
    }
}

#[test]
fn test_stacks_hook_action_file_append() {
    let chainhook = StacksChainhookInstance {
        uuid: "".to_string(),
        owner_uuid: None,
        name: "".to_string(),
        network: StacksNetwork::Testnet,
        version: 1,
        blocks: None,
        start_block: None,
        end_block: None,
        expire_after_occurrence: None,
        capture_all_events: None,
        decode_clarity_values: Some(true),
        include_contract_abi: None,
        predicate: StacksPredicate::Txid(ExactMatchingRule::Equals(
            "0xb92c2ade84a8b85f4c72170680ae42e65438aea4db72ba4b2d6a6960f4141ce8".to_string(),
        )),
        action: HookAction::FileAppend(FileHook {
            path: "./".to_string(),
        }),
        enabled: true,
        expired_at: None,
    };
    let payloads = get_all_event_payload_types();
    let apply_blocks = payloads
        .into_iter()
        .map(|payload| {
            fixtures::build_stacks_testnet_block_from_smart_contract_event_data(&[
                StacksTransactionEvent {
                    event_payload: payload,
                    position: StacksTransactionEventPosition { index: 0 },
                },
            ])
        })
        .collect::<Vec<_>>();
    let apply: Vec<(Vec<&StacksTransactionData>, &dyn AbstractStacksBlock)> = apply_blocks
        .iter()
        .map(|b| {
            (
                b.transactions.iter().collect(),
                b as &dyn AbstractStacksBlock,
            )
        })
        .collect();

    let rollback_block_data = fixtures::build_stacks_testnet_block_with_contract_deployment();
    let rollback_transactions = rollback_block_data.transactions.iter().collect();
    let rollback_block: &dyn AbstractStacksBlock = &rollback_block_data;
    let trigger = StacksTriggerChainhook {
        chainhook: &chainhook,
        apply,
        rollback: vec![(rollback_transactions, rollback_block)],
        events: vec![],
    };

    let proofs = HashMap::new();
    let ctx = Context {
        logger: None,
        tracer: false,
    };
    let occurrence =
        handle_stacks_hook_action(trigger, &proofs, &EventObserverConfig::default(), &ctx).unwrap();
    if let StacksChainhookOccurrence::File(path, bytes) = occurrence {
        assert_eq!(path, "./".to_string());
        let actual: JsonValue = serde_json::from_slice(&bytes).unwrap();
        let expected: JsonValue = serde_json::from_str(&get_expected_occurrence()).unwrap();
        assert_json_eq!(expected, actual);
    } else {
        panic!("wrong occurrence type");
    }
}
