use crate::chainhook::types::StacksTransactionEventPayload;

use crate::chainhook::indexer::stacks::NewEvent;

pub fn create_new_event_from_stacks_event(event: StacksTransactionEventPayload) -> NewEvent {
    let mut event_type = String::new();
    let stx_transfer_event = if let StacksTransactionEventPayload::STXTransferEvent(data) = &event {
        event_type = "stx_transfer".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let stx_mint_event = if let StacksTransactionEventPayload::STXMintEvent(data) = &event {
        event_type = "stx_mint".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let stx_burn_event = if let StacksTransactionEventPayload::STXBurnEvent(data) = &event {
        event_type = "stx_burn".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let stx_lock_event = if let StacksTransactionEventPayload::STXLockEvent(data) = &event {
        event_type = "stx_lock".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let nft_transfer_event = if let StacksTransactionEventPayload::NFTTransferEvent(data) = &event {
        event_type = "nft_transfer".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let nft_mint_event = if let StacksTransactionEventPayload::NFTMintEvent(data) = &event {
        event_type = "nft_mint".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let nft_burn_event = if let StacksTransactionEventPayload::NFTBurnEvent(data) = &event {
        event_type = "nft_burn".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let ft_transfer_event = if let StacksTransactionEventPayload::FTTransferEvent(data) = &event {
        event_type = "ft_transfer".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let ft_mint_event = if let StacksTransactionEventPayload::FTMintEvent(data) = &event {
        event_type = "ft_mint".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let ft_burn_event = if let StacksTransactionEventPayload::FTBurnEvent(data) = &event {
        event_type = "ft_burn".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let data_var_set_event = if let StacksTransactionEventPayload::DataVarSetEvent(data) = &event {
        event_type = "data_var_set".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    let data_map_insert_event =
        if let StacksTransactionEventPayload::DataMapInsertEvent(data) = &event {
            event_type = "data_map_insert".to_string();
            Some(serde_json::to_value(data).unwrap())
        } else {
            None
        };
    let data_map_update_event =
        if let StacksTransactionEventPayload::DataMapUpdateEvent(data) = &event {
            event_type = "data_map_update".to_string();
            Some(serde_json::to_value(data).unwrap())
        } else {
            None
        };
    let data_map_delete_event =
        if let StacksTransactionEventPayload::DataMapDeleteEvent(data) = &event {
            event_type = "data_map_delete".to_string();
            Some(serde_json::to_value(data).unwrap())
        } else {
            None
        };
    let contract_event = if let StacksTransactionEventPayload::SmartContractEvent(data) = &event {
        event_type = "smart_contract_print_event".to_string();
        Some(serde_json::to_value(data).unwrap())
    } else {
        None
    };
    NewEvent {
        txid: String::new(),
        committed: false,
        event_index: 0,
        event_type,
        stx_transfer_event,
        stx_mint_event,
        stx_burn_event,
        stx_lock_event,
        nft_transfer_event,
        nft_mint_event,
        nft_burn_event,
        ft_transfer_event,
        ft_mint_event,
        ft_burn_event,
        data_var_set_event,
        data_map_insert_event,
        data_map_update_event,
        data_map_delete_event,
        contract_event,
    }
}
