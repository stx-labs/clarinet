use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::{Hash, Hasher};

use serde::{Deserialize, Serialize};

use super::bitcoin::{TxIn, TxOut};
use super::contract_interface::ContractInterface;
use super::events::*;
use super::{StacksStackerDbChunk, DEFAULT_STACKS_NODE_RPC};

/// BlockIdentifier uniquely identifies a block in a particular network.
#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct BlockIdentifier {
    /// Also known as the block height.
    pub index: u64,
    pub hash: String,
}

impl BlockIdentifier {
    pub fn get_hash_bytes_str(&self) -> &str {
        &self.hash[2..]
    }

    pub fn get_hash_bytes(&self) -> Vec<u8> {
        hex::decode(self.get_hash_bytes_str()).unwrap()
    }
}

impl Display for BlockIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "Block #{} ({}...{})",
            self.index,
            &self.hash.as_str()[0..6],
            &self.hash.as_str()[62..]
        )
    }
}

impl Hash for BlockIdentifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl Ord for BlockIdentifier {
    fn cmp(&self, other: &Self) -> Ordering {
        (other.index, &other.hash).cmp(&(self.index, &self.hash))
    }
}

impl PartialOrd for BlockIdentifier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for BlockIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
    }
}

impl Eq for BlockIdentifier {}

/// StacksBlock contain an array of Transactions that occurred at a particular
/// BlockIdentifier. A hard requirement for blocks returned by Rosetta
/// implementations is that they MUST be _inalterable_: once a client has
/// requested and received a block identified by a specific BlockIndentifier,
/// all future calls for that same BlockIdentifier must return the same block
/// contents.
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksBlockData {
    pub block_identifier: BlockIdentifier,
    pub parent_block_identifier: BlockIdentifier,
    /// The timestamp of the block in milliseconds since the Unix Epoch. The
    /// timestamp is stored in milliseconds because some blockchains produce
    /// blocks more often than once a second.
    pub timestamp: i64,
    pub transactions: Vec<StacksTransactionData>,
    pub metadata: StacksBlockMetadata,
}

/// StacksMicroblock contain an array of Transactions that occurred at a particular
/// BlockIdentifier.
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksMicroblockData {
    pub block_identifier: BlockIdentifier,
    pub parent_block_identifier: BlockIdentifier,
    /// The timestamp of the block in milliseconds since the Unix Epoch. The
    /// timestamp is stored in milliseconds because some blockchains produce
    /// blocks more often than once a second.
    pub timestamp: i64,
    pub transactions: Vec<StacksTransactionData>,
    pub metadata: StacksMicroblockMetadata,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksMicroblockMetadata {
    pub anchor_block_identifier: BlockIdentifier,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksMicroblocksTrail {
    pub microblocks: Vec<StacksMicroblockData>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksBlockMetadata {
    pub bitcoin_anchor_block_identifier: BlockIdentifier,
    pub pox_cycle_index: u32,
    pub pox_cycle_position: u32,
    pub pox_cycle_length: u32,
    pub confirm_microblock_identifier: Option<BlockIdentifier>,
    pub stacks_block_hash: String,

    // Fields included in Nakamoto block headers
    pub block_time: Option<u64>,
    pub signer_bitvec: Option<String>,
    pub signer_signature: Option<Vec<String>>,
    pub signer_public_keys: Option<Vec<String>>,

    // Available starting in epoch3, only included in blocks where the pox cycle rewards are first calculated
    pub cycle_number: Option<u64>,
    pub reward_set: Option<StacksBlockMetadataRewardSet>,

    // Available in /new_block messages sent from stacks-core v3.0 and newer
    pub tenure_height: Option<u64>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksBlockMetadataRewardSet {
    pub pox_ustx_threshold: String,
    pub rewarded_addresses: Vec<String>,
    pub signers: Option<Vec<StacksBlockMetadataRewardSetSigner>>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksBlockMetadataRewardSetSigner {
    pub signing_key: String,
    pub weight: u32,
    pub stacked_amt: String,
}

/// BitcoinBlock contain an array of Transactions that occurred at a particular
/// BlockIdentifier. A hard requirement for blocks returned by Rosetta
/// implementations is that they MUST be _inalterable_: once a client has
/// requested and received a block identified by a specific BlockIndentifier,
/// all future calls for that same BlockIdentifier must return the same block
/// contents.
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct BitcoinBlockData {
    pub block_identifier: BlockIdentifier,
    pub parent_block_identifier: BlockIdentifier,
    /// The timestamp of the block in milliseconds since the Unix Epoch. The
    /// timestamp is stored in milliseconds because some blockchains produce
    /// blocks more often than once a second.
    pub timestamp: u32,
    pub transactions: Vec<BitcoinTransactionData>,
    pub metadata: BitcoinBlockMetadata,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct BitcoinBlockMetadata {
    pub network: BitcoinNetwork,
}

/// The timestamp of the block in milliseconds since the Unix Epoch. The
/// timestamp is stored in milliseconds because some blockchains produce blocks
/// more often than once a second.
#[derive(Debug, Clone, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct Timestamp(i64);

/// Transactions contain an array of Operations that are attributable to the
/// same TransactionIdentifier.
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksTransactionData {
    pub transaction_identifier: TransactionIdentifier,
    pub operations: Vec<Operation>,
    /// Transactions that are related to other transactions should include the
    /// transaction_identifier of these transactions in the metadata.
    pub metadata: StacksTransactionMetadata,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type", content = "data")]
pub enum StacksTransactionKind {
    ContractCall(StacksContractCallData),
    ContractDeployment(StacksContractDeploymentData),
    NativeTokenTransfer,
    Coinbase,
    TenureChange,
    BitcoinOp(BitcoinOpData),
    Unsupported,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(tag = "type", content = "data")]
pub enum BitcoinOpData {
    StackSTX(StackSTXData),
    DelegateStackSTX(DelegateStackSTXData),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StackSTXData {
    pub locked_amount: String,
    pub unlock_height: String,
    pub stacking_address: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct DelegateStackSTXData {
    pub stacking_address: String,
    pub amount: String,
    pub delegate: String,
    pub pox_address: Option<String>,
    pub unlock_height: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksContractCallData {
    pub contract_identifier: String,
    pub method: String,
    pub args: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksContractDeploymentData {
    pub contract_identifier: String,
    pub code: String,
}

/// Extra data for Transaction
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksTransactionMetadata {
    pub success: bool,
    pub raw_tx: String,
    pub result: String,
    pub sender: String,
    pub nonce: u64,
    pub fee: u64,
    pub kind: StacksTransactionKind,
    pub receipt: StacksTransactionReceipt,
    pub description: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sponsor: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub execution_cost: Option<StacksTransactionExecutionCost>,
    pub position: StacksTransactionPosition,
    pub proof: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub contract_abi: Option<ContractInterface>,
}

/// TODO
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(untagged)]
pub enum StacksTransactionPosition {
    AnchorBlock(AnchorBlockPosition),
    MicroBlock(MicroBlockPosition),
}

impl StacksTransactionPosition {
    pub fn anchor_block(index: usize) -> StacksTransactionPosition {
        StacksTransactionPosition::AnchorBlock(AnchorBlockPosition { index })
    }

    pub fn micro_block(
        micro_block_identifier: BlockIdentifier,
        index: usize,
    ) -> StacksTransactionPosition {
        StacksTransactionPosition::MicroBlock(MicroBlockPosition {
            micro_block_identifier,
            index,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct AnchorBlockPosition {
    index: usize,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct MicroBlockPosition {
    micro_block_identifier: BlockIdentifier,
    index: usize,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct StacksTransactionExecutionCost {
    pub write_length: u64,
    pub write_count: u64,
    pub read_length: u64,
    pub read_count: u64,
    pub runtime: u64,
}

/// Extra event data for Transaction
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize, Default)]
pub struct StacksTransactionReceipt {
    pub mutated_contracts_radius: HashSet<String>,
    pub mutated_assets_radius: HashSet<String>,
    pub contract_calls_stack: HashSet<String>,
    pub events: Vec<StacksTransactionEvent>,
}

impl StacksTransactionReceipt {
    pub fn new(
        mutated_contracts_radius: HashSet<String>,
        mutated_assets_radius: HashSet<String>,
        events: Vec<StacksTransactionEvent>,
    ) -> StacksTransactionReceipt {
        StacksTransactionReceipt {
            mutated_contracts_radius,
            mutated_assets_radius,
            contract_calls_stack: HashSet::new(),
            events,
        }
    }
}

/// Transactions contain an array of Operations that are attributable to the
/// same TransactionIdentifier.
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct BitcoinTransactionData {
    pub transaction_identifier: TransactionIdentifier,
    pub operations: Vec<Operation>,
    /// Transactions that are related to other transactions should include the
    /// transaction_identifier of these transactions in the metadata.
    pub metadata: BitcoinTransactionMetadata,
}

/// Extra data for Transaction
#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct BitcoinTransactionMetadata {
    pub inputs: Vec<TxIn>,
    pub outputs: Vec<TxOut>,
    pub stacks_operations: Vec<StacksBaseChainOperation>,
    pub proof: Option<String>,
    pub fee: u64,
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum StacksBaseChainOperation {
    BlockCommitted(StacksBlockCommitmentData),
    LeaderRegistered(KeyRegistrationData),
    StxTransferred(TransferSTXData),
    StxLocked(LockSTXData),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub struct StacksBlockCommitmentData {
    pub block_hash: String,
    pub pox_cycle_index: u64,
    pub pox_cycle_length: u64,
    pub pox_cycle_position: u64,
    pub pox_sats_burnt: u64,
    pub pox_sats_transferred: Vec<PoxReward>,
    // pub mining_address_pre_commit: Option<String>,
    pub mining_address_post_commit: Option<String>,
    pub mining_sats_left: u64,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub struct PoxReward {
    pub recipient_address: String,
    pub amount: u64,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct KeyRegistrationData;

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct PobBlockCommitmentData {
    pub signers: Vec<String>,
    pub stacks_block_hash: String,
    pub amount: u64,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct BlockCommitmentData {
    pub stacks_block_hash: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct TransferSTXData {
    pub sender: String,
    pub recipient: String,
    pub amount: String,
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct LockSTXData {
    pub sender: String,
    pub amount: String,
    pub duration: u64,
}

/// The transaction_identifier uniquely identifies a transaction in a particular
/// network and block or in the mempool.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Hash, PartialOrd, Ord)]
pub struct TransactionIdentifier {
    /// Any transactions that are attributable only to a block (ex: a block
    /// event) should use the hash of the block as the identifier.
    pub hash: String,
}

impl TransactionIdentifier {
    pub fn new(txid: &str) -> Self {
        let lowercased_txid = txid.to_lowercase();
        Self {
            hash: match lowercased_txid.starts_with("0x") {
                true => lowercased_txid,
                false => format!("0x{}", lowercased_txid),
            },
        }
    }

    pub fn get_hash_bytes_str(&self) -> &str {
        &self.hash[2..]
    }

    pub fn get_hash_bytes(&self) -> Vec<u8> {
        hex::decode(self.get_hash_bytes_str()).unwrap()
    }

    pub fn get_8_hash_bytes(&self) -> [u8; 8] {
        let bytes = self.get_hash_bytes();
        [
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, strum::EnumIter, strum::IntoStaticStr,
)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum OperationType {
    Credit,
    Debit,
    Lock,
}

#[derive(Debug, Clone, Default, PartialEq, Deserialize, Serialize)]
pub struct OperationMetadata {
    /// Has to be specified for ADD_KEY, REMOVE_KEY, and STAKE operations
    #[serde(skip_serializing_if = "Option::is_none")]
    pub public_key: Option<PublicKey>,
    // TODO(lgalabru): ???
    //#[serde(skip_serializing_if = "Option::is_none")]
    // pub access_key: Option<TODO>,
    /// Has to be specified for DEPLOY_CONTRACT operation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub code: Option<String>,
    /// Has to be specified for FUNCTION_CALL operation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub method_name: Option<String>,
    /// Has to be specified for FUNCTION_CALL operation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub args: Option<String>,
}

/// PublicKey contains a public key byte array for a particular CurveType
/// encoded in hex. Note that there is no PrivateKey struct as this is NEVER the
/// concern of an implementation.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PublicKey {
    /// Hex-encoded public key bytes in the format specified by the CurveType.
    pub hex_bytes: Option<String>,
    pub curve_type: CurveType,
}

/// CurveType is the type of cryptographic curve associated with a PublicKey.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CurveType {
    /// `y (255-bits) || x-sign-bit (1-bit)` - `32 bytes` (<https://ed25519.cr.yp.to/ed25519-20110926.pdf>)
    Edwards25519,
    /// SEC compressed - `33 bytes` (<https://secg.org/sec1-v2.pdf#subsubsection.2.3.3>)
    Secp256k1,
}

/// Operations contain all balance-changing information within a transaction.
/// They are always one-sided (only affect 1 AccountIdentifier) and can
/// succeed or fail independently from a Transaction.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Operation {
    pub operation_identifier: OperationIdentifier,

    /// Restrict referenced related_operations to identifier indexes < the
    /// current operation_identifier.index. This ensures there exists a clear
    /// DAG-structure of relations. Since operations are one-sided, one could
    /// imagine relating operations in a single transfer or linking operations
    /// in a call tree.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub related_operations: Option<Vec<OperationIdentifier>>,

    /// The network-specific type of the operation. Ensure that any type that
    /// can be returned here is also specified in the NetworkStatus. This can
    /// be very useful to downstream consumers that parse all block data.
    #[serde(rename = "type")]
    pub type_: OperationType,

    /// The network-specific status of the operation. Status is not defined on
    /// the transaction object because blockchains with smart contracts may have
    /// transactions that partially apply. Blockchains with atomic transactions
    /// (all operations succeed or all operations fail) will have the same
    /// status for each operation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status: Option<OperationStatusKind>,

    pub account: AccountIdentifier,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub amount: Option<Amount>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<OperationMetadata>,
}

/// The operation_identifier uniquely identifies an operation within a
/// transaction.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OperationIdentifier {
    /// The operation index is used to ensure each operation has a unique
    /// identifier within a transaction. This index is only relative to the
    /// transaction and NOT GLOBAL. The operations in each transaction should
    /// start from index 0. To clarify, there may not be any notion of an
    /// operation index in the blockchain being described.
    pub index: u32,

    /// Some blockchains specify an operation index that is essential for
    /// client use. For example, Bitcoin uses a network_index to identify
    /// which UTXO was used in a transaction.  network_index should not be
    /// populated if there is no notion of an operation index in a blockchain
    /// (typically most account-based blockchains).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub network_index: Option<i64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize, strum::EnumIter)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum OperationStatusKind {
    Success,
}

/// The account_identifier uniquely identifies an account within a network. All
/// fields in the account_identifier are utilized to determine this uniqueness
/// (including the metadata field, if populated).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct AccountIdentifier {
    /// The address may be a cryptographic public key (or some encoding of it)
    /// or a provided username.
    pub address: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub sub_account: Option<SubAccountIdentifier>,
    /* Rosetta Spec also optionally provides:
     *
     * /// Blockchains that utilize a username model (where the address is not a
     * /// derivative of a cryptographic public key) should specify the public
     * /// key(s) owned by the address in metadata.
     * #[serde(skip_serializing_if = "Option::is_none")]
     * pub metadata: Option<serde_json::Value>, */
}

/// An account may have state specific to a contract address (ERC-20 token)
/// and/or a stake (delegated balance). The sub_account_identifier should
/// specify which state (if applicable) an account instantiation refers to.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct SubAccountIdentifier {
    /// The SubAccount address may be a cryptographic value or some other
    /// identifier (ex: bonded) that uniquely specifies a SubAccount.
    pub address: SubAccount,
    /* Rosetta Spec also optionally provides:
     *
     * /// If the SubAccount address is not sufficient to uniquely specify a
     * /// SubAccount, any other identifying information can be stored here.  It is
     * /// important to note that two SubAccounts with identical addresses but
     * /// differing metadata will not be considered equal by clients.
     * #[serde(skip_serializing_if = "Option::is_none")]
     * pub metadata: Option<serde_json::Value>, */
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum SubAccount {
    LiquidBalanceForStorage,
    Locked,
}

/// Amount is some Value of a Currency. It is considered invalid to specify a
/// Value without a Currency.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Amount {
    /// Value of the transaction in atomic units represented as an
    /// arbitrary-sized signed integer.  For example, 1 BTC would be represented
    /// by a value of 100000000.
    pub value: u128,

    pub currency: Currency,
    /* Rosetta Spec also optionally provides:
     *
     * #[serde(skip_serializing_if = "Option::is_none")]
     * pub metadata: Option<serde_json::Value>, */
}

/// Currency is composed of a canonical Symbol and Decimals. This Decimals value
/// is used to convert an Amount.Value from atomic units (Satoshis) to standard
/// units (Bitcoins).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Currency {
    /// Canonical symbol associated with a currency.
    pub symbol: String,

    /// Number of decimal places in the standard unit representation of the
    /// amount.  For example, BTC has 8 decimals. Note that it is not possible
    /// to represent the value of some currency in atomic units that is not base
    /// 10.
    pub decimals: u32,

    /// Any additional information related to the currency itself.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<CurrencyMetadata>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum CurrencyStandard {
    Sip09,
    Sip10,
    None,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CurrencyMetadata {
    pub asset_class_identifier: String,
    pub asset_identifier: Option<String>,
    pub standard: CurrencyStandard,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum BlockchainEvent {
    BlockchainUpdatedWithHeaders(BlockchainUpdatedWithHeaders),
    BlockchainUpdatedWithReorg(BlockchainUpdatedWithReorg),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BlockchainUpdatedWithHeaders {
    pub new_headers: Vec<BlockHeader>,
    pub confirmed_headers: Vec<BlockHeader>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BlockchainUpdatedWithReorg {
    pub headers_to_rollback: Vec<BlockHeader>,
    pub headers_to_apply: Vec<BlockHeader>,
    pub confirmed_headers: Vec<BlockHeader>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "type", content = "data")]
pub enum StacksNonConsensusEventPayloadData {
    SignerMessage(StacksStackerDbChunk),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct StacksNonConsensusEventData {
    pub payload: StacksNonConsensusEventPayloadData,
    pub received_at_ms: u64,
    pub received_at_block: BlockIdentifier,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BlockHeader {
    pub block_identifier: BlockIdentifier,
    pub parent_block_identifier: BlockIdentifier,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum BitcoinChainEvent {
    ChainUpdatedWithBlocks(BitcoinChainUpdatedWithBlocksData),
    ChainUpdatedWithReorg(BitcoinChainUpdatedWithReorgData),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BitcoinChainUpdatedWithBlocksData {
    pub new_blocks: Vec<BitcoinBlockData>,
    pub confirmed_blocks: Vec<BitcoinBlockData>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BitcoinChainUpdatedWithReorgData {
    pub blocks_to_rollback: Vec<BitcoinBlockData>,
    pub blocks_to_apply: Vec<BitcoinBlockData>,
    pub confirmed_blocks: Vec<BitcoinBlockData>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StacksChainUpdatedWithNonConsensusEventsData {
    pub events: Vec<StacksNonConsensusEventData>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum StacksChainEvent {
    ChainUpdatedWithBlocks(StacksChainUpdatedWithBlocksData),
    ChainUpdatedWithReorg(StacksChainUpdatedWithReorgData),
    ChainUpdatedWithMicroblocks(StacksChainUpdatedWithMicroblocksData),
    ChainUpdatedWithMicroblocksReorg(StacksChainUpdatedWithMicroblocksReorgData),
    ChainUpdatedWithNonConsensusEvents(StacksChainUpdatedWithNonConsensusEventsData),
}

impl StacksChainEvent {
    pub fn get_confirmed_blocks(self) -> Vec<StacksBlockData> {
        match self {
            StacksChainEvent::ChainUpdatedWithBlocks(event) => event.confirmed_blocks,
            StacksChainEvent::ChainUpdatedWithReorg(event) => event.confirmed_blocks,
            _ => vec![],
        }
    }

    pub fn get_latest_block_identifier(&self) -> Option<&BlockIdentifier> {
        match self {
            StacksChainEvent::ChainUpdatedWithBlocks(event) => {
                event.new_blocks.last().map(|b| &b.block.block_identifier)
            }
            StacksChainEvent::ChainUpdatedWithReorg(event) => event
                .blocks_to_apply
                .last()
                .map(|b| &b.block.block_identifier),
            StacksChainEvent::ChainUpdatedWithMicroblocks(event) => event
                .new_microblocks
                .first()
                .map(|b| &b.metadata.anchor_block_identifier),
            StacksChainEvent::ChainUpdatedWithMicroblocksReorg(event) => event
                .microblocks_to_apply
                .first()
                .map(|b| &b.metadata.anchor_block_identifier),
            StacksChainEvent::ChainUpdatedWithNonConsensusEvents(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StacksBlockUpdate {
    pub block: StacksBlockData,
    pub parent_microblocks_to_rollback: Vec<StacksMicroblockData>,
    pub parent_microblocks_to_apply: Vec<StacksMicroblockData>,
}

impl StacksBlockUpdate {
    pub fn new(block: StacksBlockData) -> StacksBlockUpdate {
        StacksBlockUpdate {
            block,
            parent_microblocks_to_rollback: vec![],
            parent_microblocks_to_apply: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StacksChainUpdatedWithBlocksData {
    pub new_blocks: Vec<StacksBlockUpdate>,
    pub confirmed_blocks: Vec<StacksBlockData>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StacksChainUpdatedWithReorgData {
    pub blocks_to_rollback: Vec<StacksBlockUpdate>,
    pub blocks_to_apply: Vec<StacksBlockUpdate>,
    pub confirmed_blocks: Vec<StacksBlockData>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StacksChainUpdatedWithMicroblocksData {
    pub new_microblocks: Vec<StacksMicroblockData>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StacksChainUpdatedWithMicroblocksReorgData {
    pub microblocks_to_rollback: Vec<StacksMicroblockData>,
    pub microblocks_to_apply: Vec<StacksMicroblockData>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StacksNetwork {
    Simnet,
    Devnet,
    Testnet,
    Mainnet,
}

impl std::fmt::Display for StacksNetwork {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl TryFrom<&str> for StacksNetwork {
    type Error = String;

    fn try_from(network: &str) -> Result<Self, Self::Error> {
        match network {
            "devnet" => Ok(StacksNetwork::Devnet),
            "testnet" => Ok(StacksNetwork::Testnet),
            "mainnet" => Ok(StacksNetwork::Mainnet),
            "simnet" => Ok(StacksNetwork::Simnet),
            _ => Err(format!(
                "network '{network}' unsupported (mainnet, testnet, devnet, simnet)",
            )),
        }
    }
}

impl StacksNetwork {
    pub fn as_str(&self) -> &str {
        match self {
            StacksNetwork::Devnet => "devnet",
            StacksNetwork::Testnet => "testnet",
            StacksNetwork::Mainnet => "mainnet",
            StacksNetwork::Simnet => "simnet",
        }
    }

    pub fn is_simnet(&self) -> bool {
        matches!(self, StacksNetwork::Simnet)
    }

    pub fn is_testnet(&self) -> bool {
        matches!(self, StacksNetwork::Testnet)
    }

    pub fn either_devnet_or_testnet(&self) -> bool {
        matches!(self, StacksNetwork::Devnet | StacksNetwork::Testnet)
    }

    pub fn either_testnet_or_mainnet(&self) -> bool {
        matches!(self, StacksNetwork::Mainnet | StacksNetwork::Testnet)
    }

    pub fn is_devnet(&self) -> bool {
        matches!(self, StacksNetwork::Devnet)
    }

    pub fn is_mainnet(&self) -> bool {
        matches!(self, StacksNetwork::Mainnet)
    }

    pub fn get_networks(&self) -> (BitcoinNetwork, StacksNetwork) {
        match &self {
            StacksNetwork::Simnet => (BitcoinNetwork::Regtest, StacksNetwork::Simnet),
            StacksNetwork::Devnet => (BitcoinNetwork::Testnet, StacksNetwork::Devnet),
            StacksNetwork::Testnet => (BitcoinNetwork::Testnet, StacksNetwork::Testnet),
            StacksNetwork::Mainnet => (BitcoinNetwork::Mainnet, StacksNetwork::Mainnet),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BitcoinNetwork {
    Regtest,
    Testnet,
    Signet,
    Mainnet,
}

impl std::fmt::Display for BitcoinNetwork {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl TryFrom<&str> for BitcoinNetwork {
    type Error = String;

    fn try_from(network: &str) -> Result<Self, Self::Error> {
        match network {
            "regtest" => Ok(BitcoinNetwork::Regtest),
            "testnet" => Ok(BitcoinNetwork::Testnet),
            "mainnet" => Ok(BitcoinNetwork::Mainnet),
            "signet" => Ok(BitcoinNetwork::Signet),
            _ => Err(format!(
                "network '{network}' unsupported (mainnet, testnet, regtest, signet)",
            )),
        }
    }
}

impl BitcoinNetwork {
    pub fn as_str(&self) -> &str {
        match self {
            BitcoinNetwork::Regtest => "regtest",
            BitcoinNetwork::Testnet => "testnet",
            BitcoinNetwork::Mainnet => "mainnet",
            BitcoinNetwork::Signet => "signet",
        }
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
pub enum BitcoinBlockSignaling {
    Stacks(StacksNodeConfig),
    ZeroMQ(String),
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
pub struct StacksNodeConfig {
    pub rpc_url: String,
    pub ingestion_port: u16,
}

impl StacksNodeConfig {
    pub fn new(rpc_url: String, ingestion_port: u16) -> StacksNodeConfig {
        StacksNodeConfig {
            rpc_url,
            ingestion_port,
        }
    }

    pub fn default_localhost(ingestion_port: u16) -> StacksNodeConfig {
        StacksNodeConfig {
            rpc_url: DEFAULT_STACKS_NODE_RPC.to_string(),
            ingestion_port,
        }
    }
}

impl BitcoinBlockSignaling {
    pub fn should_ignore_bitcoin_block_signaling_through_stacks(&self) -> bool {
        !matches!(self, BitcoinBlockSignaling::Stacks(_))
    }

    pub fn is_bitcoind_zmq_block_signaling_expected(&self) -> bool {
        !matches!(self, BitcoinBlockSignaling::ZeroMQ(_))
    }
}
