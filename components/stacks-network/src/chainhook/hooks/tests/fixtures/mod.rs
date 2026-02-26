use std::collections::HashMap;
use std::sync::LazyLock;

use crate::chainhook::types::{
    FTBurnEventData, FTMintEventData, FTTransferEventData, NFTBurnEventData, NFTMintEventData,
    NFTTransferEventData, STXBurnEventData, STXLockEventData, STXMintEventData,
    STXTransferEventData, SmartContractEventData, StacksBlockData, StacksTransactionData,
    StacksTransactionEvent, StacksTransactionEventPayload,
};

static TESTNET_STACKS_BLOCK_FIXTURES: LazyLock<HashMap<u64, StacksBlockData>> =
    LazyLock::new(|| {
        let mut fixtures: HashMap<u64, StacksBlockData> = HashMap::new();
        fixtures.insert(
            107605,
            load_stacks_block_fixture(std::include_str!("stacks/testnet/107605.json")),
        );
        fixtures
    });

pub fn load_stacks_block_fixture(json_str: &str) -> StacksBlockData {
    serde_json::from_str(json_str).unwrap()
}

pub fn get_stacks_testnet_block(block_height: u64) -> &'static StacksBlockData {
    TESTNET_STACKS_BLOCK_FIXTURES.get(&block_height).unwrap()
}

pub fn build_stacks_testnet_block_from_smart_contract_event_data(
    events: &[StacksTransactionEvent],
) -> StacksBlockData {
    let mut base_block =
        load_stacks_block_fixture(std::include_str!("stacks/testnet/base/block.json"));
    let mut base_transaction = get_contract_call_transaction();

    base_transaction.metadata.receipt.events = events.to_vec();
    base_block.transactions.push(base_transaction);
    base_block
}

pub fn build_stacks_testnet_block_with_contract_deployment() -> StacksBlockData {
    let mut base_block =
        load_stacks_block_fixture(std::include_str!("stacks/testnet/base/block.json"));
    let base_transaction = get_contract_deploy_transaction();

    base_block.transactions.push(base_transaction);
    base_block
}
pub fn build_stacks_testnet_block_with_contract_call() -> StacksBlockData {
    let mut base_block =
        load_stacks_block_fixture(std::include_str!("stacks/testnet/base/block.json"));
    let base_transaction = get_contract_call_transaction();

    base_block.transactions.push(base_transaction);
    base_block
}

pub fn get_contract_call_transaction() -> StacksTransactionData {
    serde_json::from_str(std::include_str!(
        "stacks/testnet/base/transaction_contract_call.json"
    ))
    .unwrap()
}
pub fn get_contract_deploy_transaction() -> StacksTransactionData {
    serde_json::from_str(std::include_str!(
        "stacks/testnet/base/transaction_contract_deploy.json"
    ))
    .unwrap()
}

pub fn get_expected_occurrence() -> String {
    std::include_str!("stacks/testnet/occurrence.json").to_owned()
}

pub fn get_all_event_payload_types() -> Vec<StacksTransactionEventPayload> {
    vec![
        get_test_event_payload_by_type("stx_transfer"),
        get_test_event_payload_by_type("stx_mint"),
        get_test_event_payload_by_type("stx_lock"),
        get_test_event_payload_by_type("stx_burn"),
        get_test_event_payload_by_type("nft_transfer"),
        get_test_event_payload_by_type("nft_mint"),
        get_test_event_payload_by_type("nft_burn"),
        get_test_event_payload_by_type("ft_transfer"),
        get_test_event_payload_by_type("ft_mint"),
        get_test_event_payload_by_type("ft_burn"),
        get_test_event_payload_by_type("smart_contract_print_event"),
        get_test_event_payload_by_type("smart_contract_print_event_empty"),
        get_test_event_payload_by_type("smart_contract_not_print_event"),
    ]
}
pub fn get_test_event_payload_by_type(event_type: &str) -> StacksTransactionEventPayload {
    match event_type {
        "stx_transfer" => StacksTransactionEventPayload::STXTransferEvent(STXTransferEventData {
            sender: "".to_string(),
            recipient: "".to_string(),
            amount: "".to_string(),
        }),
        "stx_mint" => StacksTransactionEventPayload::STXMintEvent(STXMintEventData {
            recipient: "".to_string(),
            amount: "".to_string(),
        }),
        "stx_lock" => StacksTransactionEventPayload::STXLockEvent(STXLockEventData {
            locked_amount: "".to_string(),
            unlock_height: "".to_string(),
            locked_address: "".to_string(),
        }),
        "stx_burn" => StacksTransactionEventPayload::STXBurnEvent(STXBurnEventData {
            sender: "".to_string(),
            amount: "".to_string(),
        }),
        "nft_transfer" => StacksTransactionEventPayload::NFTTransferEvent(NFTTransferEventData {
            sender: "".to_string(),
            asset_class_identifier: "asset-id".to_string(),
            hex_asset_identifier: "asset-id".to_string(),
            recipient: "".to_string(),
        }),
        "nft_mint" => StacksTransactionEventPayload::NFTMintEvent(NFTMintEventData {
            asset_class_identifier: "asset-id".to_string(),
            hex_asset_identifier: "asset-id".to_string(),
            recipient: "".to_string(),
        }),
        "nft_burn" => StacksTransactionEventPayload::NFTBurnEvent(NFTBurnEventData {
            asset_class_identifier: "asset-id".to_string(),
            hex_asset_identifier: "asset-id".to_string(),
            sender: "".to_string(),
        }),
        "ft_transfer" => StacksTransactionEventPayload::FTTransferEvent(FTTransferEventData {
            sender: "".to_string(),
            asset_class_identifier: "asset-id".to_string(),
            amount: "".to_string(),
            recipient: "".to_string(),
        }),
        "ft_mint" => StacksTransactionEventPayload::FTMintEvent(FTMintEventData {
            asset_class_identifier: "asset-id".to_string(),
            recipient: "".to_string(),
            amount: "".to_string(),
        }),
        "ft_burn" => StacksTransactionEventPayload::FTBurnEvent(FTBurnEventData {
            asset_class_identifier: "asset-id".to_string(),
            sender: "".to_string(),
            amount: "".to_string(),
        }),
        "data_var_set" => todo!(),
        "data_map_insert" => todo!(),
        "data_map_update" => todo!(),
        "data_map_delete" => todo!(),
        "smart_contract_print_event" => {
            StacksTransactionEventPayload::SmartContractEvent(SmartContractEventData {
                topic: "print".to_string(),
                contract_identifier: "ST3AXH4EBHD63FCFPTZ8GR29TNTVWDYPGY0KDY5E5.loan-data"
                    .to_string(),
                hex_value: PRINT_EVENT_HEX.to_string(),
            })
        }
        "smart_contract_print_event_empty" => {
            StacksTransactionEventPayload::SmartContractEvent(SmartContractEventData {
                topic: "print".to_string(),
                contract_identifier: "some-id".to_string(),
                hex_value: EMPTY_EVENT_HEX.to_string(),
            })
        }
        "smart_contract_not_print_event" => {
            StacksTransactionEventPayload::SmartContractEvent(SmartContractEventData {
                topic: "not-print".to_string(),
                contract_identifier: "ST3AXH4EBHD63FCFPTZ8GR29TNTVWDYPGY0KDY5E5.loan-data"
                    .to_string(),
                hex_value: PRINT_EVENT_HEX.to_string(),
            })
        }
        _ => unimplemented!(),
    }
}

static PRINT_EVENT_HEX: &str = "0x0d00000010616263736f6d652d76616c7565616263"; // "abcsome-valueabc"

static EMPTY_EVENT_HEX: &str = "0x0d00000000";
