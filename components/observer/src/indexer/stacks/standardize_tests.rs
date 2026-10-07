use crate::indexer::stacks::{
    get_tx_description, get_value_description, standardize_stacks_serialized_block,
    standardize_stacks_serialized_block_header, NewBlock, NewEvent, NewTransaction,
};
use crate::indexer::{IndexerConfig, StacksChainContext};
use crate::types::{
    BitcoinNetwork, OperationIdentifier, OperationType, StacksBlockData, StacksNetwork,
    StacksNodeConfig, StacksTransactionKind,
};
use crate::utils::Context;

fn test_indexer_config() -> IndexerConfig {
    IndexerConfig {
        bitcoin_network: BitcoinNetwork::Regtest,
        stacks_network: StacksNetwork::Devnet,
        bitcoind_rpc_url: "http://localhost:18443".to_string(),
        bitcoind_rpc_username: "devnet".to_string(),
        bitcoind_rpc_password: "devnet".to_string(),
        stacks_node_config: StacksNodeConfig::new("http://localhost:20443".to_string(), 20445),
    }
}

/// A signed STX-transfer transaction from the canonical devnet wallet_1 to
/// wallet_2, serialized by `stacks-rpc-client::crypto` at a fixed nonce/fee.
/// Holding the wire format constant is what makes the standardization
/// expectations below golden vectors rather than tautologies.
const WALLET_1_STX_TRANSFER_RAW_TX: &str = "0x808000000004007321b74e2b6a7e949e6c4ad313035b1665095017000000000000000000000000000001f40000bd05fe0e990ba43cd9bb10a03826d9670534accefa1f54949c28515239d4fea54b1f9e755c3af482ae4542b243f8526b0f0b49a1258b6f023924ce4742a889c103010000000000051a99e2ec69ac5b6e67b4e26edd0e2c1c1a6b9bbd2300000000000f424068656c6c6f0000000000000000000000000000000000000000000000000000000000";
const WALLET_1_TXID: &str = "a5364f209406bce0205052fd2333836e61b7b277122b578e1baa58adbada8edd";

fn empty_block() -> NewBlock {
    NewBlock {
        block_height: 1,
        block_hash: "0x1111".to_string(),
        index_block_hash: "0x2222".to_string(),
        burn_block_height: 101,
        burn_block_hash: "0x3333".to_string(),
        parent_block_hash: "0x0000".to_string(),
        parent_index_block_hash: "0x4444".to_string(),
        parent_microblock: None,
        parent_microblock_sequence: None,
        parent_burn_block_hash: "0x5555".to_string(),
        parent_burn_block_height: 100,
        parent_burn_block_timestamp: 12345,
        transactions: vec![],
        events: vec![],
        matured_miner_rewards: vec![],
        tenure_height: None,
        block_time: None,
        signer_bitvec: None,
        signer_signature_hash: None,
        signer_signature: None,
        cycle_number: None,
        reward_set: None,
    }
}

fn transfer_tx() -> NewTransaction {
    NewTransaction {
        txid: WALLET_1_TXID.to_string(),
        tx_index: 0,
        status: "success".to_string(),
        raw_result: "0x0703".to_string(),
        raw_tx: WALLET_1_STX_TRANSFER_RAW_TX.to_string(),
        execution_cost: None,
        contract_interface: None,
        contract_abi: None,
    }
}

fn transfer_event() -> NewEvent {
    NewEvent {
        txid: WALLET_1_TXID.to_string(),
        committed: true,
        event_index: 0,
        event_type: "stx_transfer".to_string(),
        stx_transfer_event: Some(serde_json::json!({
            "sender": "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5",
            "recipient": "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG",
            "amount": "1000000",
        })),
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
    }
}

fn standardize_block(block: &NewBlock) -> StacksBlockData {
    let indexer_config = test_indexer_config();
    let mut chain_ctx = StacksChainContext::new(&StacksNetwork::Devnet);
    let ctx = Context::empty();
    standardize_stacks_serialized_block(
        &indexer_config,
        &serde_json::to_string(block).unwrap(),
        &mut chain_ctx,
        &ctx,
    )
    .unwrap()
}

#[test]
fn block_header_identifiers_are_derived_from_the_serialized_header() {
    let header = r#"{
        "block_height": 42,
        "index_block_hash": "0xabc",
        "parent_index_block_hash": "0xdef"
    }"#;
    let (block, parent) = standardize_stacks_serialized_block_header(header).unwrap();
    assert_eq!(block.index, 42);
    assert_eq!(block.hash, "0xabc");
    assert_eq!(parent.index, 41);
    assert_eq!(parent.hash, "0xdef");
}

#[test]
fn block_header_requires_the_index_hash() {
    let header = r#"{ "block_height": 42, "parent_index_block_hash": "0xdef" }"#;
    assert!(standardize_stacks_serialized_block_header(header).is_err());
}

#[test]
fn stx_transfer_tx_describes_the_transfer() {
    let (description, kind, fee, nonce, sender, sponsor) =
        get_tx_description(WALLET_1_STX_TRANSFER_RAW_TX, &[]).unwrap();
    assert_eq!(
        description,
        "transfered: 1000000 µSTX from ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 to ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG"
    );
    assert_eq!(kind, StacksTransactionKind::NativeTokenTransfer);
    assert_eq!(fee, 500);
    assert_eq!(nonce, 0);
    assert_eq!(sender, "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5");
    assert_eq!(sponsor, None);
}

#[test]
fn raw_tx_without_prefix_is_rejected() {
    assert!(get_tx_description("no-prefix-here", &[]).is_err());
}

#[test]
fn bitcoin_op_transfer_is_standardized_from_events() {
    // `0x00` raw_tx marks a Bitcoin-originated transaction; the transfer is
    // described by its STX transfer event.
    let event = transfer_event();
    let (description, kind, fee, nonce, sender, sponsor) =
        get_tx_description("0x00", &[&event]).unwrap();
    assert_eq!(
        description,
        "transfered: 1000000 µSTX from ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 to ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG through Bitcoin transaction"
    );
    assert_eq!(kind, StacksTransactionKind::NativeTokenTransfer);
    assert_eq!(fee, 0);
    assert_eq!(nonce, 0);
    assert_eq!(sender, "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5");
    assert_eq!(sponsor, None);
}

#[test]
fn bitcoin_op_with_no_events_is_rejected() {
    assert!(get_tx_description("0x00", &[]).is_err());
}

#[test]
fn serialized_block_standardizes_into_rosetta_shape() {
    let mut block = empty_block();
    block.transactions = vec![transfer_tx()];
    block.events = vec![transfer_event()];
    let block_data = standardize_block(&block);

    assert_eq!(block_data.block_identifier.index, 1);
    assert_eq!(block_data.block_identifier.hash, "0x2222");
    assert_eq!(block_data.parent_block_identifier.hash, "0x4444");
    assert_eq!(block_data.parent_block_identifier.index, 0);
    assert_eq!(block_data.timestamp, 12345);
    assert_eq!(
        block_data.metadata.bitcoin_anchor_block_identifier.index,
        101
    );
    assert_eq!(block_data.metadata.stacks_block_hash, "0x1111");
    // Devnet PoX config: 20-burn-block cycles (5 prepare + 15 reward), first
    // burn block height 100 → burn height 101 is cycle 0, position 0.
    assert_eq!(block_data.metadata.pox_cycle_length, 20);
    assert_eq!(block_data.metadata.pox_cycle_index, 0);
    assert_eq!(block_data.metadata.pox_cycle_position, 0);
    assert_eq!(block_data.metadata.signer_public_keys, None);
    assert_eq!(block_data.metadata.confirm_microblock_identifier, None);

    assert_eq!(block_data.transactions.len(), 1);
    let tx = &block_data.transactions[0];
    assert_eq!(tx.transaction_identifier.hash, WALLET_1_TXID);
    assert!(tx.metadata.success);
    assert_eq!(tx.metadata.fee, 500);
    assert_eq!(tx.metadata.nonce, 0);
    assert_eq!(
        tx.metadata.sender,
        "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5"
    );
    assert_eq!(tx.metadata.sponsor, None);
    assert_eq!(
        tx.metadata.description,
        "transfered: 1000000 µSTX from ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 to ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG"
    );
    // The STX transfer event must produce exactly two operations: a Debit
    // from the sender at index 0 and a Credit to the recipient at index 1,
    // each pointing at the other via related_operations.
    assert_eq!(tx.operations.len(), 2);

    let debit = &tx.operations[0];
    assert_eq!(debit.type_, OperationType::Debit);
    assert_eq!(
        debit.account.address,
        "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5"
    );
    assert_eq!(debit.amount.as_ref().map(|a| a.value), Some(1_000_000));
    assert_eq!(
        debit.related_operations,
        Some(vec![OperationIdentifier {
            index: 1,
            network_index: None,
        }])
    );

    let credit = &tx.operations[1];
    assert_eq!(credit.type_, OperationType::Credit);
    assert_eq!(
        credit.account.address,
        "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG"
    );
    assert_eq!(credit.amount.as_ref().map(|a| a.value), Some(1_000_000));
    assert_eq!(
        credit.related_operations,
        Some(vec![OperationIdentifier {
            index: 0,
            network_index: None,
        }])
    );
}

#[test]
fn abort_by_response_transactions_are_skipped() {
    let mut block = empty_block();
    let mut tx = transfer_tx();
    tx.status = "abort_by_response".to_string();
    tx.raw_result = "0x".to_string();
    block.transactions = vec![tx];
    let block_data = standardize_block(&block);
    assert!(block_data.transactions.is_empty());
}

#[test]
fn value_description_formats_clarity_values() {
    let ctx = Context::empty();
    // `(ok true)` consensus-encoded: 0x07 = ResponseTrue, 0x03 = Bool(true).
    assert_eq!(get_value_description("0x0703", &ctx), "(ok true)");
    // The function returns non-hexadecimal input without changes.
    assert_eq!(get_value_description("plain", &ctx), "plain");
    // The function returns invalid hexadecimal text without the prefix.
    assert_eq!(get_value_description("0xzz", &ctx), "zz");
    // The function also returns incomplete Clarity value bytes without the prefix.
    assert_eq!(get_value_description("0x00", &ctx), "00");
}
