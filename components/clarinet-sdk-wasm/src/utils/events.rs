use clarity::vm::events::{FTEventType, NFTEventType, STXEventType, StacksTransactionEvent};
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
pub struct StacksEvent {
    pub event: String,
    pub data: serde_json::Value,
}

pub fn serialize_event(event: &StacksTransactionEvent) -> StacksEvent {
    match event {
        StacksTransactionEvent::SmartContractEvent(data) => StacksEvent {
            event: "print_event".into(),
            data: data
                .json_serialize()
                .expect("failed to serialize smart contract event"),
        },
        StacksTransactionEvent::STXEvent(STXEventType::STXTransferEvent(data)) => StacksEvent {
            event: "stx_transfer_event".into(),
            data: data.json_serialize(),
        },
        StacksTransactionEvent::STXEvent(STXEventType::STXMintEvent(data)) => StacksEvent {
            event: "stx_mint_event".into(),
            data: data.json_serialize(),
        },
        StacksTransactionEvent::STXEvent(STXEventType::STXBurnEvent(data)) => StacksEvent {
            event: "stx_burn_event".into(),
            data: data.json_serialize(),
        },
        StacksTransactionEvent::STXEvent(STXEventType::STXLockEvent(data)) => StacksEvent {
            event: "stx_lock_event".into(),
            data: data.json_serialize(),
        },
        StacksTransactionEvent::NFTEvent(NFTEventType::NFTTransferEvent(data)) => StacksEvent {
            event: "nft_transfer_event".into(),
            data: data
                .json_serialize()
                .expect("failed to serialize nft transfer event"),
        },
        StacksTransactionEvent::NFTEvent(NFTEventType::NFTMintEvent(data)) => StacksEvent {
            event: "nft_mint_event".into(),
            data: data
                .json_serialize()
                .expect("failed to serialize nft mint event"),
        },
        StacksTransactionEvent::NFTEvent(NFTEventType::NFTBurnEvent(data)) => StacksEvent {
            event: "nft_burn_event".into(),
            data: data
                .json_serialize()
                .expect("failed to serialize nft burn event"),
        },
        StacksTransactionEvent::FTEvent(FTEventType::FTTransferEvent(data)) => StacksEvent {
            event: "ft_transfer_event".into(),
            data: data.json_serialize(),
        },
        StacksTransactionEvent::FTEvent(FTEventType::FTMintEvent(data)) => StacksEvent {
            event: "ft_mint_event".into(),
            data: data.json_serialize(),
        },
        StacksTransactionEvent::FTEvent(FTEventType::FTBurnEvent(data)) => StacksEvent {
            event: "ft_burn_event".into(),
            data: data.json_serialize(),
        },
    }
}
