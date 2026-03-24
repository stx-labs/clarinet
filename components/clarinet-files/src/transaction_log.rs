use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TransactionLog {
    pub manifest_path: String,
    pub transactions: Vec<RecordedTx>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum RecordedTx {
    CallPublicFn {
        contract: String,
        method: String,
        args: Vec<Vec<u8>>,
        sender: String,
    },
    CallPrivateFn {
        contract: String,
        method: String,
        args: Vec<Vec<u8>>,
        sender: String,
    },
    CallReadOnlyFn {
        contract: String,
        method: String,
        args: Vec<Vec<u8>>,
        sender: String,
    },
    DeployContract {
        name: String,
        content: String,
        sender: String,
        clarity_version: Option<u32>,
    },
    #[serde(rename_all = "camelCase")]
    TransferSTX {
        amount: u64,
        recipient: String,
        sender: String,
    },
    AdvanceChainTip {
        count: u32,
    },
}
