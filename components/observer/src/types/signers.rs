use serde::{Deserialize, Serialize};

use super::StacksTransactionData;

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
    InvalidTransactionReplay,
    InvalidParentBlock,
    InvalidTimestamp,
    NetworkChainMismatch,
    NotFoundError,
    ProblematicTransaction,
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

#[cfg(feature = "stacks-signers")]
mod stacks_signers_conversions {
    use libsigner::v0::messages::RejectCode;
    use stackslib::net::api::postblock_proposal::ValidateRejectCode;

    use super::{BlockRejectReasonCode, BlockValidationFailedCode};

    impl From<ValidateRejectCode> for BlockValidationFailedCode {
        fn from(code: ValidateRejectCode) -> Self {
            match code {
                ValidateRejectCode::BadBlockHash => Self::BadBlockHash,
                ValidateRejectCode::BadTransaction => Self::BadTransaction,
                ValidateRejectCode::InvalidBlock => Self::InvalidBlock,
                ValidateRejectCode::ChainstateError => Self::ChainstateError,
                ValidateRejectCode::UnknownParent => Self::UnknownParent,
                ValidateRejectCode::NonCanonicalTenure => Self::NonCanonicalTenure,
                ValidateRejectCode::NoSuchTenure => Self::NoSuchTenure,
                ValidateRejectCode::InvalidTransactionReplay => Self::InvalidTransactionReplay,
                ValidateRejectCode::InvalidParentBlock => Self::InvalidParentBlock,
                ValidateRejectCode::InvalidTimestamp => Self::InvalidTimestamp,
                ValidateRejectCode::NetworkChainMismatch => Self::NetworkChainMismatch,
                ValidateRejectCode::NotFoundError => Self::NotFoundError,
                ValidateRejectCode::ProblematicTransaction => Self::ProblematicTransaction,
            }
        }
    }

    impl From<RejectCode> for BlockRejectReasonCode {
        fn from(code: RejectCode) -> Self {
            match code {
                RejectCode::ValidationFailed(c) => Self::ValidationFailed {
                    validation_failed: c.into(),
                },
                RejectCode::NoSortitionView => Self::NoSortitionView,
                // The upstream `String` payload is dropped here; downstream consumers
                // see only the unit variant that the observer's wire format defines.
                RejectCode::ConnectivityIssues(_) => Self::ConnectivityIssues,
                RejectCode::RejectedInPriorRound => Self::RejectedInPriorRound,
                RejectCode::SortitionViewMismatch => Self::SortitionViewMismatch,
                RejectCode::TestingDirective => Self::TestingDirective,
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn validate_reject_code_to_block_validation_failed_code() {
            // Spot-check old + newly-added variants. The match in the From impl is
            // exhaustive, so any future upstream variant fails to compile until handled.
            assert_eq!(
                BlockValidationFailedCode::from(ValidateRejectCode::BadBlockHash),
                BlockValidationFailedCode::BadBlockHash
            );
            assert_eq!(
                BlockValidationFailedCode::from(ValidateRejectCode::NoSuchTenure),
                BlockValidationFailedCode::NoSuchTenure
            );
            assert_eq!(
                BlockValidationFailedCode::from(ValidateRejectCode::InvalidTransactionReplay),
                BlockValidationFailedCode::InvalidTransactionReplay
            );
            assert_eq!(
                BlockValidationFailedCode::from(ValidateRejectCode::ProblematicTransaction),
                BlockValidationFailedCode::ProblematicTransaction
            );
        }

        #[test]
        fn reject_code_to_block_reject_reason_code() {
            assert_eq!(
                BlockRejectReasonCode::from(RejectCode::ValidationFailed(
                    ValidateRejectCode::BadBlockHash
                )),
                BlockRejectReasonCode::ValidationFailed {
                    validation_failed: BlockValidationFailedCode::BadBlockHash,
                }
            );
            assert_eq!(
                BlockRejectReasonCode::from(RejectCode::NoSortitionView),
                BlockRejectReasonCode::NoSortitionView
            );
            // Upstream's `String` payload is dropped on the way to the unit variant.
            assert_eq!(
                BlockRejectReasonCode::from(RejectCode::ConnectivityIssues(
                    "peer unreachable".to_string()
                )),
                BlockRejectReasonCode::ConnectivityIssues
            );
            assert_eq!(
                BlockRejectReasonCode::from(RejectCode::RejectedInPriorRound),
                BlockRejectReasonCode::RejectedInPriorRound
            );
            assert_eq!(
                BlockRejectReasonCode::from(RejectCode::SortitionViewMismatch),
                BlockRejectReasonCode::SortitionViewMismatch
            );
            assert_eq!(
                BlockRejectReasonCode::from(RejectCode::TestingDirective),
                BlockRejectReasonCode::TestingDirective
            );
        }
    }
}
