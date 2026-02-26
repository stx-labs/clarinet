use test_case::test_case;

use super::super::tests::{helpers, process_stacks_blocks_and_check_expectations};
use super::NewEvent;
use crate::chainhook::indexer::tests::helpers::stacks_events::create_new_event_from_stacks_event;
use crate::chainhook::types::{
    DataMapDeleteEventData, DataMapInsertEventData, DataMapUpdateEventData, DataVarSetEventData,
    FTBurnEventData, FTMintEventData, FTTransferEventData, NFTBurnEventData, NFTMintEventData,
    NFTTransferEventData, STXBurnEventData, STXLockEventData, STXMintEventData,
    STXTransferEventData, SmartContractEventData, StacksTransactionEventPayload,
};

#[test]
fn test_stacks_vector_001() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_001(), None));
}

#[test]
fn test_stacks_vector_002() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_002(), None));
}

#[test]
fn test_stacks_vector_003() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_003(), None));
}

#[test]
fn test_stacks_vector_004() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_004(), None));
}

#[test]
fn test_stacks_vector_005() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_005(), None));
}

#[test]
fn test_stacks_vector_006() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_006(), None));
}

#[test]
fn test_stacks_vector_007() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_007(), None));
}

#[test]
fn test_stacks_vector_008() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_008(), None));
}

#[test]
fn test_stacks_vector_009() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_009(), None));
}

#[test]
fn test_stacks_vector_010() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_010(), None));
}

#[test]
fn test_stacks_vector_011() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_011(), None));
}

#[test]
fn test_stacks_vector_012() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_012(), None));
}

#[test]
fn test_stacks_vector_013() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_013(), None));
}

#[test]
fn test_stacks_vector_014() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_014(), None));
}

#[test]
fn test_stacks_vector_015() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_015(), None));
}

#[test]
fn test_stacks_vector_016() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_016(), None));
}

#[test]
fn test_stacks_vector_017() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_017(), None));
}

#[test]
fn test_stacks_vector_018() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_018(), None));
}

#[test]
fn test_stacks_vector_019() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_019(), None));
}

#[test]
fn test_stacks_vector_020() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_020(), None));
}

#[test]
fn test_stacks_vector_021() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_021(), None));
}

#[test]
fn test_stacks_vector_022() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_022(), None));
}

#[test]
fn test_stacks_vector_023() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_023(), None));
}

#[test]
fn test_stacks_vector_024() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_024(), None));
}

#[test]
fn test_stacks_vector_025() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_025(), None));
}

#[test]
fn test_stacks_vector_026() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_026(), None));
}

#[test]
fn test_stacks_vector_027() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_027(), None));
}

#[test]
fn test_stacks_vector_028() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_028(), None));
}

#[test]
fn test_stacks_vector_029() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_029(), None));
}

#[test]
fn test_stacks_vector_030() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_030(), None));
}

#[test]
fn test_stacks_vector_031() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_031(), None));
}

#[test]
fn test_stacks_vector_032() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_032(), None));
}

#[test]
fn test_stacks_vector_033() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_033(), None));
}

#[test]
fn test_stacks_vector_034() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_034(), None));
}

#[test]
fn test_stacks_vector_035() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_035(), None));
}

#[test]
fn test_stacks_vector_036() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_036(), None));
}

#[test]
fn test_stacks_vector_037() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_037(), None));
}

#[test]
fn test_stacks_vector_038() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_038(), None));
}

#[test]
fn test_stacks_vector_039() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_039(), None));
}

#[test]
fn test_stacks_vector_040() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_040(), None));
}

// #[test]
// fn test_stacks_vector_041() {
//     process_stacks_blocks_and_check_expectations((helpers::shapes::get_vector_041(), None));
// }

#[test]
fn test_stacks_vector_042() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_042(), None));
}

#[test]
fn test_stacks_vector_043() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_043(), None));
}

#[test]
fn test_stacks_vector_044() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_044(), None));
}

#[test]
fn test_stacks_vector_045() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_045(), None));
}

#[test]
fn test_stacks_vector_046() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_046(), None));
}

#[test]
fn test_stacks_vector_047() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_047(), None));
}

#[test]
fn test_stacks_vector_048() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_048(), None));
}

#[test]
fn test_stacks_vector_049() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_049(), None));
}

#[test]
fn test_stacks_vector_050() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_050(), None));
}

#[test]
fn test_stacks_vector_051() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_051(), None));
}

#[test]
fn test_stacks_vector_052() {
    process_stacks_blocks_and_check_expectations((helpers::stacks_shapes::get_vector_052(), None));
}

#[test]
fn test_stacks_vector_053() {
    process_stacks_blocks_and_check_expectations(helpers::stacks_shapes::get_vector_053());
}
#[test]
fn test_stacks_vector_054() {
    process_stacks_blocks_and_check_expectations(helpers::stacks_shapes::get_vector_054());
}
#[test]
fn test_stacks_vector_055() {
    process_stacks_blocks_and_check_expectations(helpers::stacks_shapes::get_vector_055());
}

#[test_case(StacksTransactionEventPayload::STXTransferEvent(STXTransferEventData {
    sender: String::new(),
    recipient: String::new(),
    amount: "1".to_string(),
}); "stx_transfer")]
#[test_case(StacksTransactionEventPayload::STXMintEvent(STXMintEventData {
    recipient: String::new(),
    amount: "1".to_string(),
}); "stx_mint")]
#[test_case(StacksTransactionEventPayload::STXBurnEvent(STXBurnEventData {
    sender: String::new(),
    amount: "1".to_string(),
}); "stx_burn")]
#[test_case(StacksTransactionEventPayload::STXLockEvent(STXLockEventData {
    locked_amount: "1".to_string(),
    unlock_height: String::new(),
    locked_address: String::new(),
}); "stx_lock")]
#[test_case(StacksTransactionEventPayload::NFTTransferEvent(NFTTransferEventData {
    asset_class_identifier: String::new(),
    hex_asset_identifier: String::new(),
    sender: String::new(),
    recipient: String::new(),
}); "nft_transfer")]
#[test_case(StacksTransactionEventPayload::NFTMintEvent(NFTMintEventData {
    asset_class_identifier: String::new(),
    hex_asset_identifier: String::new(),
    recipient: String::new(),
}); "nft_mint")]
#[test_case(StacksTransactionEventPayload::NFTBurnEvent(NFTBurnEventData {
    asset_class_identifier: String::new(),
    hex_asset_identifier: String::new(),
    sender: String::new(),
}); "nft_burn")]
#[test_case(StacksTransactionEventPayload::FTTransferEvent(FTTransferEventData {
    asset_class_identifier: String::new(),
    sender: String::new(),
    recipient: String::new(),
    amount: "1".to_string(),
}); "ft_transfer")]
#[test_case(StacksTransactionEventPayload::FTMintEvent(FTMintEventData {
    asset_class_identifier: String::new(),
    recipient: String::new(),
    amount: "1".to_string(),
}); "ft_mint")]
#[test_case(StacksTransactionEventPayload::FTBurnEvent(FTBurnEventData {
    asset_class_identifier: String::new(),
    sender: String::new(),
    amount: "1".to_string(),
}); "ft_burn")]
#[test_case(StacksTransactionEventPayload::DataVarSetEvent(DataVarSetEventData {
    contract_identifier: String::new(),
    var: String::new(),
    hex_new_value: String::new(),
}); "data_var_set")]
#[test_case(StacksTransactionEventPayload::DataMapInsertEvent(DataMapInsertEventData {
    contract_identifier: String::new(),
    hex_inserted_key: String::new(),
    hex_inserted_value: String::new(),
    map: String::new()
}); "data_map_insert")]
#[test_case(StacksTransactionEventPayload::DataMapUpdateEvent(DataMapUpdateEventData {
    contract_identifier: String::new(),
    hex_new_value: String::new(),
    hex_key: String::new(),
    map: String::new()
}); "data_map_update")]
#[test_case(StacksTransactionEventPayload::DataMapDeleteEvent(DataMapDeleteEventData {
    contract_identifier: String::new(),
    hex_deleted_key: String::new(),
    map: String::new()
}); "data_map_delete")]
#[test_case(StacksTransactionEventPayload::SmartContractEvent(SmartContractEventData {
    contract_identifier: String::new(),
    topic: "print".to_string(),
    hex_value: String::new(),
}); "smart_contract_print_event")]
fn new_events_can_be_converted_into_chainhook_event(original_event: StacksTransactionEventPayload) {
    let new_event = create_new_event_from_stacks_event(original_event.clone());
    let event = new_event.into_chainhook_event().unwrap();
    let original_event_serialized = serde_json::to_string(&original_event).unwrap();
    let event_serialized = serde_json::to_string(&event.event_payload).unwrap();
    assert_eq!(original_event_serialized, event_serialized);
}

#[test]
fn into_chainhook_event_rejects_invalid_missing_event() {
    let new_event = NewEvent {
        txid: String::new(),
        committed: false,
        event_index: 0,
        event_type: String::new(),
        stx_transfer_event: None,
        stx_mint_event: None,
        stx_burn_event: None,
        stx_lock_event: None,
        nft_transfer_event: None,
        nft_mint_event: None,
        nft_burn_event: None,
        ft_transfer_event: None,
        ft_mint_event: None,
        ft_burn_event: None,
        data_var_set_event: None,
        data_map_insert_event: None,
        data_map_update_event: None,
        data_map_delete_event: None,
        contract_event: None,
    };
    new_event
        .into_chainhook_event()
        .expect_err("expected error on missing event");
}

#[test]
#[cfg(feature = "stacks-signers")]
fn parses_block_response_signer_message() {
    use super::standardize_stacks_stackerdb_chunks;
    use crate::chainhook::indexer::stacks::{
        NewSignerModifiedSlot, NewStackerDbChunkIssuerId, NewStackerDbChunkIssuerSlots,
        NewStackerDbChunks, NewStackerDbChunksContractId,
    };
    use crate::chainhook::types::{BlockResponseData, StacksSignerMessage};
    use crate::chainhook::utils::Context;

    let new_chunks = NewStackerDbChunks {
        contract_id: NewStackerDbChunksContractId {
            name: "signers-0-1".to_string(),
            issuer: (
                NewStackerDbChunkIssuerId(26),
                NewStackerDbChunkIssuerSlots(vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
            ),
        },
        modified_slots: vec![NewSignerModifiedSlot {
            sig: "01060cc1bef9ccfe7139f5240ff5c33c44c83206e851e21b63234a996654f70d750b44d9c76466a5c45515b63183dfcfaefe5877fbd3593859e50d5df39cd469a1".to_string(),
            data: "01008f913dd2bcc2cfbd1c82166e0ad99230f76de098a5ba6ee1b15b042c8f67c6f000a1c66742e665e981d10f7a70a5df312c9cba729331129ff1b510e71133d79c0122b25266bf47e8c1c923b4fde0464756ced884030e9983f797c902961fc9b0b10000005d737461636b732d7369676e657220302e302e3120283a646431656265363436303366353464616534383535386135643832643962643838356539376130312c206465627567206275696c642c206c696e7578205b616172636836345d29".to_string(),
            slot_id: 1,
            slot_version: 11,
        }],
    };
    let ctx = &Context::empty();
    let parsed_chunk = standardize_stacks_stackerdb_chunks(&new_chunks, ctx).unwrap();

    assert_eq!(parsed_chunk.len(), 1);
    let message = &parsed_chunk[0];
    assert_eq!(message.contract, "signers-0-1");
    assert_eq!(
        message.pubkey,
        "0x028efa20fa5706567008ebaf48f7ae891342eeb944d96392f719c505c89f84ed8d"
    );
    assert_eq!(message.sig, "0x01060cc1bef9ccfe7139f5240ff5c33c44c83206e851e21b63234a996654f70d750b44d9c76466a5c45515b63183dfcfaefe5877fbd3593859e50d5df39cd469a1");

    match &message.message {
        StacksSignerMessage::BlockResponse(BlockResponseData::Accepted(accepted)) => {
            assert_eq!(accepted.signature, "0x00a1c66742e665e981d10f7a70a5df312c9cba729331129ff1b510e71133d79c0122b25266bf47e8c1c923b4fde0464756ced884030e9983f797c902961fc9b0b1");
            assert_eq!(
                accepted.signer_signature_hash,
                "0x8f913dd2bcc2cfbd1c82166e0ad99230f76de098a5ba6ee1b15b042c8f67c6f0"
            );
        }
        _ => panic!(),
    }
}
