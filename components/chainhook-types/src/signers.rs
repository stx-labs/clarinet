use serde::{Deserialize, Serialize};

use crate::StacksTransactionData;

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct NakamotoBlockHeaderData {
    pub version: u8,
    pub chain_length: u64,
    pub burn_spent: u64,
    pub consensus_hash: String,
    pub parent_block_id: String,
    pub tx_merkle_root: String,
    pub state_index_root: String,
    pub timestamp: u64,
    pub miner_signature: String,
    pub signer_signature: Vec<String>,
    pub pox_treatment: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct NakamotoBlockData {
    pub header: NakamotoBlockHeaderData,
    pub block_hash: String,
    pub index_block_hash: String,
    pub transactions: Vec<StacksTransactionData>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct BlockProposalData {
    pub block: NakamotoBlockData,
    pub burn_height: u64,
    pub reward_cycle: u64,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct BlockAcceptedResponse {
    pub signer_signature_hash: String,
    pub signature: String,
    pub metadata: SignerMessageMetadata,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct SignerMessageMetadata {
    pub server_version: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum BlockValidationFailedCode {
    BadBlockHash,
    BadTransaction,
    InvalidBlock,
    ChainstateError,
    UnknownParent,
    NonCanonicalTenure,
    NoSuchTenure,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum BlockRejectReasonCode {
    #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
    ValidationFailed {
        #[serde(rename = "VALIDATION_FAILED")]
        validation_failed: BlockValidationFailedCode,
    },
    ConnectivityIssues,
    RejectedInPriorRound,
    NoSortitionView,
    SortitionViewMismatch,
    TestingDirective,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct BlockRejectedResponse {
    pub reason: String,
    pub reason_code: BlockRejectReasonCode,
    pub signer_signature_hash: String,
    pub chain_id: u32,
    pub signature: String,
    pub metadata: SignerMessageMetadata,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type", content = "data")]
pub enum BlockResponseData {
    Accepted(BlockAcceptedResponse),
    Rejected(BlockRejectedResponse),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct BlockPushedData {
    pub block: NakamotoBlockData,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct PeerInfoData {
    pub burn_block_height: u64,
    pub stacks_tip_consensus_hash: String,
    pub stacks_tip: String,
    pub stacks_tip_height: u64,
    pub pox_consensus: String,
    pub server_version: String,
    pub network_id: u32,
    pub index_block_hash: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct MockProposalData {
    pub peer_info: PeerInfoData,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct MockSignatureData {
    pub mock_proposal: MockProposalData,
    pub metadata: SignerMessageMetadata,
    pub signature: String,
    pub pubkey: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct MockBlockData {
    pub mock_proposal: MockProposalData,
    pub mock_signatures: Vec<MockSignatureData>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type", content = "data")]
pub enum StacksSignerMessage {
    BlockProposal(BlockProposalData),
    BlockResponse(BlockResponseData),
    BlockPushed(BlockPushedData),
    MockSignature(MockSignatureData),
    MockProposal(PeerInfoData),
    MockBlock(MockBlockData),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksStackerDbChunk {
    pub contract: String,
    pub sig: String,
    pub pubkey: String,
    pub message: StacksSignerMessage,
}
