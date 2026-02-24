use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct STXTransferEventData {
    pub sender: String,
    pub recipient: String,
    pub amount: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct STXMintEventData {
    pub recipient: String,
    pub amount: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct STXLockEventData {
    pub locked_amount: String,
    pub unlock_height: String,
    pub locked_address: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct STXBurnEventData {
    pub sender: String,
    pub amount: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct NFTTransferEventData {
    #[serde(rename = "asset_identifier")]
    pub asset_class_identifier: String,
    #[serde(rename = "raw_value")]
    pub hex_asset_identifier: String,
    pub sender: String,
    pub recipient: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct NFTMintEventData {
    #[serde(rename = "asset_identifier")]
    pub asset_class_identifier: String,
    #[serde(rename = "raw_value")]
    pub hex_asset_identifier: String,
    pub recipient: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct NFTBurnEventData {
    #[serde(rename = "asset_identifier")]
    pub asset_class_identifier: String,
    #[serde(rename = "raw_value")]
    pub hex_asset_identifier: String,
    pub sender: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FTTransferEventData {
    #[serde(rename = "asset_identifier")]
    pub asset_class_identifier: String,
    pub sender: String,
    pub recipient: String,
    pub amount: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FTMintEventData {
    #[serde(rename = "asset_identifier")]
    pub asset_class_identifier: String,
    pub recipient: String,
    pub amount: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct FTBurnEventData {
    #[serde(rename = "asset_identifier")]
    pub asset_class_identifier: String,
    pub sender: String,
    pub amount: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct DataVarSetEventData {
    pub contract_identifier: String,
    pub var: String,
    #[serde(rename = "raw_new_value")]
    pub hex_new_value: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct DataMapInsertEventData {
    pub contract_identifier: String,
    pub map: String,
    #[serde(rename = "raw_inserted_key")]
    pub hex_inserted_key: String,
    #[serde(rename = "raw_inserted_value")]
    pub hex_inserted_value: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct DataMapUpdateEventData {
    pub contract_identifier: String,
    pub map: String,
    #[serde(rename = "raw_key")]
    pub hex_key: String,
    #[serde(rename = "raw_new_value")]
    pub hex_new_value: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct DataMapDeleteEventData {
    pub contract_identifier: String,
    pub map: String,
    #[serde(rename = "raw_deleted_key")]
    pub hex_deleted_key: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct SmartContractEventData {
    pub contract_identifier: String,
    pub topic: String,
    #[serde(rename = "raw_value")]
    pub hex_value: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type", content = "data")]
pub enum StacksTransactionEventPayload {
    STXTransferEvent(STXTransferEventData),
    STXMintEvent(STXMintEventData),
    STXLockEvent(STXLockEventData),
    STXBurnEvent(STXBurnEventData),
    NFTTransferEvent(NFTTransferEventData),
    NFTMintEvent(NFTMintEventData),
    NFTBurnEvent(NFTBurnEventData),
    FTTransferEvent(FTTransferEventData),
    FTMintEvent(FTMintEventData),
    FTBurnEvent(FTBurnEventData),
    DataVarSetEvent(DataVarSetEventData),
    DataMapInsertEvent(DataMapInsertEventData),
    DataMapUpdateEvent(DataMapUpdateEventData),
    DataMapDeleteEvent(DataMapDeleteEventData),
    SmartContractEvent(SmartContractEventData),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksTransactionEvent {
    #[serde(flatten)]
    pub event_payload: StacksTransactionEventPayload,
    #[serde(default)]
    pub position: StacksTransactionEventPosition,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize, Default)]
pub struct StacksTransactionEventPosition {
    pub index: u32,
}
