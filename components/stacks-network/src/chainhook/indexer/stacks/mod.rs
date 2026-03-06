mod blocks_pool;

use std::collections::{BTreeMap, HashMap, HashSet};
use std::convert::TryInto;
use std::io::Cursor;
use std::str;

pub use blocks_pool::StacksBlockPool;
use clarity::codec::StacksMessageCodec;
use clarity::vm::types::{SequenceData, Value as ClarityValue};
use hiro_system_kit::slog;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use stacks_codec::codec::{StacksTransaction, TransactionAuth, TransactionPayload};

use crate::chainhook::hooks::stacks::try_decode_clarity_value;
use crate::chainhook::indexer::{AssetClassCache, IndexerConfig, StacksChainContext};
use crate::chainhook::types::*;
use crate::chainhook::utils::Context;

#[derive(Deserialize, Serialize)]
pub struct NewBlock {
    pub block_height: u64,
    pub block_hash: String,
    pub index_block_hash: String,
    pub burn_block_height: u64,
    pub burn_block_hash: String,
    pub parent_block_hash: String,
    pub parent_index_block_hash: String,
    pub parent_microblock: String,
    pub parent_microblock_sequence: u64,
    pub parent_burn_block_hash: String,
    pub parent_burn_block_height: u64,
    pub parent_burn_block_timestamp: i64,
    pub transactions: Vec<NewTransaction>,
    pub events: Vec<NewEvent>,
    pub matured_miner_rewards: Vec<MaturedMinerReward>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub tenure_height: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub block_time: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub signer_bitvec: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub signer_signature_hash: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub signer_signature: Option<Vec<String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub cycle_number: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub reward_set: Option<RewardSet>,
}

#[derive(Deserialize, Serialize)]
pub struct RewardSet {
    pub pox_ustx_threshold: String,
    pub rewarded_addresses: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signers: Option<Vec<RewardSetSigner>>,
}

#[derive(Deserialize, Serialize)]
pub struct RewardSetSigner {
    pub signing_key: String,
    pub weight: u32,
    pub stacked_amt: String,
}

#[derive(Deserialize, Serialize, Default, Clone)]
pub struct NewBlockHeader {
    pub block_height: u64,
    pub index_block_hash: Option<String>,
    pub parent_index_block_hash: Option<String>,
}

#[derive(Deserialize, Serialize)]
pub struct MaturedMinerReward {
    pub from_index_consensus_hash: String,
    pub from_stacks_block_hash: String,
    pub recipient: String,
    pub coinbase_amount: String,
    /// micro-STX amount
    pub tx_fees_anchored: String,
    /// micro-STX amount
    pub tx_fees_streamed_confirmed: String,
    /// micro-STX amount
    pub tx_fees_streamed_produced: String,
}

#[derive(Deserialize, Debug)]
pub struct NewMicroblockTrail {
    pub parent_index_block_hash: String,
    pub burn_block_hash: String,
    pub burn_block_height: u64,
    pub burn_block_timestamp: i64,
    pub transactions: Vec<NewMicroblockTransaction>,
    pub events: Vec<NewEvent>,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct NewTransaction {
    pub txid: String,
    pub tx_index: usize,
    pub status: String,
    pub raw_result: String,
    pub raw_tx: String,
    pub execution_cost: Option<StacksTransactionExecutionCost>,
    pub contract_interface: Option<ContractInterface>,
    /// @deprecated Use `contract_interface` instead
    pub contract_abi: Option<ContractInterface>,
}

#[derive(Deserialize, Debug)]
pub struct NewMicroblockTransaction {
    pub txid: String,
    pub tx_index: usize,
    pub status: String,
    pub raw_result: String,
    pub raw_tx: String,
    pub execution_cost: Option<StacksTransactionExecutionCost>,
    pub microblock_sequence: usize,
    pub microblock_hash: String,
    pub microblock_parent_hash: String,
    pub contract_abi: Option<ContractInterface>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct NewEvent {
    pub txid: String,
    pub committed: bool,
    pub event_index: u32,
    #[serde(rename = "type")]
    pub event_type: String,
    pub stx_transfer_event: Option<JsonValue>,
    pub stx_mint_event: Option<JsonValue>,
    pub stx_burn_event: Option<JsonValue>,
    pub stx_lock_event: Option<JsonValue>,
    pub nft_transfer_event: Option<JsonValue>,
    pub nft_mint_event: Option<JsonValue>,
    pub nft_burn_event: Option<JsonValue>,
    pub ft_transfer_event: Option<JsonValue>,
    pub ft_mint_event: Option<JsonValue>,
    pub ft_burn_event: Option<JsonValue>,
    pub data_var_set_event: Option<JsonValue>,
    pub data_map_insert_event: Option<JsonValue>,
    pub data_map_update_event: Option<JsonValue>,
    pub data_map_delete_event: Option<JsonValue>,
    pub contract_event: Option<JsonValue>,
}

impl NewEvent {
    pub fn into_chainhook_event(&self) -> Result<StacksTransactionEvent, String> {
        if let Some(ref event_data) = self.stx_mint_event {
            let data: STXMintEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: (StacksTransactionEventPayload::STXMintEvent(data)),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.stx_lock_event {
            let data: STXLockEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: (StacksTransactionEventPayload::STXLockEvent(data)),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.stx_burn_event {
            let data: STXBurnEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: (StacksTransactionEventPayload::STXBurnEvent(data)),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.stx_transfer_event {
            let data: STXTransferEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: StacksTransactionEventPayload::STXTransferEvent(data.clone()),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.nft_mint_event {
            let data: NFTMintEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: (StacksTransactionEventPayload::NFTMintEvent(data)),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.nft_burn_event {
            let data: NFTBurnEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: (StacksTransactionEventPayload::NFTBurnEvent(data)),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.nft_transfer_event {
            let data: NFTTransferEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: StacksTransactionEventPayload::NFTTransferEvent(data.clone()),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.ft_mint_event {
            let data: FTMintEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: (StacksTransactionEventPayload::FTMintEvent(data)),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.ft_burn_event {
            let data: FTBurnEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: (StacksTransactionEventPayload::FTBurnEvent(data)),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.ft_transfer_event {
            let data: FTTransferEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: (StacksTransactionEventPayload::FTTransferEvent(data)),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.data_var_set_event {
            let data: DataVarSetEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: (StacksTransactionEventPayload::DataVarSetEvent(data)),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.data_map_insert_event {
            let data: DataMapInsertEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: StacksTransactionEventPayload::DataMapInsertEvent(data.clone()),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.data_map_update_event {
            let data: DataMapUpdateEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: StacksTransactionEventPayload::DataMapUpdateEvent(data.clone()),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.data_map_delete_event {
            let data: DataMapDeleteEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: StacksTransactionEventPayload::DataMapDeleteEvent(data.clone()),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        } else if let Some(ref event_data) = self.contract_event {
            let data: SmartContractEventData =
                serde_json::from_value(event_data.clone()).expect("Unable to decode event_data");
            return Ok(StacksTransactionEvent {
                event_payload: StacksTransactionEventPayload::SmartContractEvent(data.clone()),
                position: StacksTransactionEventPosition {
                    index: self.event_index,
                },
            });
        }
        Err("unable to support event type".to_string())
    }
}

pub fn get_stacks_currency() -> Currency {
    Currency {
        symbol: "STX".into(),
        decimals: 6,
        metadata: None,
    }
}

#[derive(Deserialize, Debug)]
pub struct ContractReadonlyCall {
    pub okay: bool,
    pub result: String,
}

#[cfg(feature = "stacks-signers")]
#[derive(Deserialize, Debug)]
pub struct NewStackerDbChunkIssuerId(pub u32);

#[cfg(feature = "stacks-signers")]
#[derive(Deserialize, Debug)]
pub struct NewStackerDbChunkIssuerSlots(pub Vec<u32>);

#[cfg(feature = "stacks-signers")]
#[derive(Deserialize, Debug)]
pub struct NewStackerDbChunksContractId {
    pub name: String,
    pub issuer: (NewStackerDbChunkIssuerId, NewStackerDbChunkIssuerSlots),
}

#[cfg(feature = "stacks-signers")]
#[derive(Deserialize, Debug)]
pub struct NewSignerModifiedSlot {
    pub sig: String,
    pub data: String,
    pub slot_id: u32,
    pub slot_version: u32,
}

#[cfg(feature = "stacks-signers")]
#[derive(Deserialize, Debug)]
pub struct NewStackerDbChunks {
    pub contract_id: NewStackerDbChunksContractId,
    pub modified_slots: Vec<NewSignerModifiedSlot>,
}

pub fn standardize_stacks_serialized_block_header(
    serialized_block: &str,
) -> Result<(BlockIdentifier, BlockIdentifier), String> {
    let mut block_header: NewBlockHeader = serde_json::from_str(serialized_block)
        .map_err(|e| format!("unable to parse stacks block_header {}", e))?;
    let hash = block_header
        .index_block_hash
        .take()
        .ok_or("unable to retrieve index_block_hash".to_string())?;
    let block_identifier = BlockIdentifier {
        hash,
        index: block_header.block_height,
    };
    let parent_hash = block_header
        .parent_index_block_hash
        .take()
        .ok_or("unable to retrieve parent_index_block_hash".to_string())?;

    let parent_height = block_identifier.index.saturating_sub(1);
    let parent_block_identifier = BlockIdentifier {
        hash: parent_hash,
        index: parent_height,
    };
    Ok((block_identifier, parent_block_identifier))
}

pub fn standardize_stacks_serialized_block(
    indexer_config: &IndexerConfig,
    serialized_block: &str,
    chain_ctx: &mut StacksChainContext,
    ctx: &Context,
) -> Result<StacksBlockData, String> {
    let mut block: NewBlock = serde_json::from_str(serialized_block)
        .map_err(|e| format!("unable to parse stacks block_header {}", e))?;
    standardize_stacks_block(indexer_config, &mut block, chain_ctx, ctx)
}

pub fn standardize_stacks_marshalled_block(
    indexer_config: &IndexerConfig,
    marshalled_block: JsonValue,
    chain_ctx: &mut StacksChainContext,
    ctx: &Context,
) -> Result<StacksBlockData, String> {
    let mut block: NewBlock = serde_json::from_value(marshalled_block)
        .map_err(|e| format!("unable to parse stacks block {}", e))?;
    standardize_stacks_block(indexer_config, &mut block, chain_ctx, ctx)
}

pub fn standardize_stacks_block(
    indexer_config: &IndexerConfig,
    block: &mut NewBlock,
    chain_ctx: &mut StacksChainContext,
    ctx: &Context,
) -> Result<StacksBlockData, String> {
    let pox_cycle_length: u64 = chain_ctx.pox_config.get_pox_cycle_len();
    let current_len = u64::saturating_sub(
        block.burn_block_height,
        1 + chain_ctx.pox_config.first_burnchain_block_height,
    );
    let pox_cycle_id: u32 = (current_len / pox_cycle_length).try_into().unwrap_or(0);
    let mut events: HashMap<&String, Vec<&NewEvent>> = HashMap::new();
    for event in block.events.iter() {
        events
            .entry(&event.txid)
            .and_modify(|events| events.push(event))
            .or_insert(vec![&event]);
    }

    let mut transactions = vec![];
    for tx in block.transactions.iter() {
        let tx_events = events.remove(&tx.txid).unwrap_or_default();
        let (description, tx_type, fee, nonce, sender, sponsor) =
            match get_tx_description(&tx.raw_tx, &tx_events) {
                Ok(desc) => desc,
                Err(e) => {
                    if tx.status.eq("abort_by_response") {
                        // We should probably revisit this approach
                        continue;
                    }
                    return Err(format!(
                        "unable to standardize block #{} ({})",
                        block.block_height, e
                    ));
                }
            };
        let events = tx_events
            .iter()
            .map(|e| e.into_chainhook_event())
            .collect::<Result<Vec<StacksTransactionEvent>, String>>()?;
        let (receipt, operations) = get_standardized_stacks_receipt(
            &tx.txid,
            events,
            &mut chain_ctx.asset_class_map,
            &indexer_config.get_stacks_node_config().rpc_url.clone(),
            true,
        );

        transactions.push(StacksTransactionData {
            transaction_identifier: TransactionIdentifier {
                hash: tx.txid.clone(),
            },
            operations,
            metadata: StacksTransactionMetadata {
                success: tx.status == "success",
                result: get_value_description(&tx.raw_result, ctx),
                raw_tx: tx.raw_tx.clone(),
                sender,
                nonce,
                fee,
                sponsor,
                kind: tx_type,
                execution_cost: tx.execution_cost.clone(),
                receipt,
                description,
                position: StacksTransactionPosition::anchor_block(tx.tx_index),
                proof: None,
                contract_abi: tx
                    .contract_interface
                    .clone()
                    .or_else(|| tx.contract_abi.clone()),
            },
        });
    }

    let confirm_microblock_identifier = if block.parent_microblock
        == "0x0000000000000000000000000000000000000000000000000000000000000000"
    {
        None
    } else {
        Some(BlockIdentifier {
            index: block.parent_microblock_sequence,
            hash: block.parent_microblock.clone(),
        })
    };

    let signer_sig_hash = block
        .signer_signature_hash
        .as_ref()
        .map(|hash| hex::decode(&hash[2..]).expect("unable to decode signer_signature hex"));

    let block = StacksBlockData {
        block_identifier: BlockIdentifier {
            hash: block.index_block_hash.clone(),
            index: block.block_height,
        },
        parent_block_identifier: BlockIdentifier {
            hash: block.parent_index_block_hash.clone(),
            index: match block.block_height {
                0 => 0,
                _ => block.block_height - 1,
            },
        },
        timestamp: block.parent_burn_block_timestamp,
        metadata: StacksBlockMetadata {
            bitcoin_anchor_block_identifier: BlockIdentifier {
                hash: block.burn_block_hash.clone(),
                index: block.burn_block_height,
            },
            pox_cycle_index: pox_cycle_id,
            pox_cycle_position: (current_len % pox_cycle_length) as u32,
            pox_cycle_length: pox_cycle_length.try_into().unwrap(),
            confirm_microblock_identifier,
            stacks_block_hash: block.block_hash.clone(),

            block_time: block.block_time,
            tenure_height: block.tenure_height,
            // TODO: decode `signer_bitvec` into an easy to use bit string representation (e.g. "01010101")
            signer_bitvec: block.signer_bitvec.clone(),
            signer_signature: block.signer_signature.clone(),

            signer_public_keys: match (signer_sig_hash, &block.signer_signature) {
                (Some(signer_sig_hash), Some(signatures)) => Some(
                    signatures
                        .iter()
                        .map(|sig_hex| {
                            let sig_msg =
                                clarity::util::secp256k1::MessageSignature::from_hex(sig_hex)
                                    .map_err(|e| {
                                        format!("unable to parse signer signature message: {}", e)
                                    })?;
                            let pubkey =
                                get_signer_pubkey_from_message_hash(&signer_sig_hash, &sig_msg)
                                    .map_err(|e| {
                                        format!("unable to recover signer sig pubkey: {}", e)
                                    })?;
                            Ok(format!("0x{}", hex::encode(pubkey)))
                        })
                        .collect::<Result<Vec<_>, String>>()?,
                ),
                _ => None,
            },

            cycle_number: block.cycle_number,
            reward_set: block
                .reward_set
                .as_ref()
                .map(|r| StacksBlockMetadataRewardSet {
                    pox_ustx_threshold: r.pox_ustx_threshold.clone(),
                    rewarded_addresses: r.rewarded_addresses.clone(),
                    signers: r.signers.as_ref().map(|signers| {
                        signers
                            .iter()
                            .map(|signer| StacksBlockMetadataRewardSetSigner {
                                signing_key: signer.signing_key.clone(),
                                weight: signer.weight,
                                stacked_amt: signer.stacked_amt.clone(),
                            })
                            .collect()
                    }),
                }),
        },
        transactions,
    };
    Ok(block)
}

pub fn standardize_stacks_serialized_microblock_trail(
    indexer_config: &IndexerConfig,
    serialized_microblock_trail: &str,
    chain_ctx: &mut StacksChainContext,
    ctx: &Context,
) -> Result<Vec<StacksMicroblockData>, String> {
    let mut microblock_trail: NewMicroblockTrail =
        serde_json::from_str(serialized_microblock_trail)
            .map_err(|e| format!("unable to parse microblock trail {}", e))?;
    standardize_stacks_microblock_trail(indexer_config, &mut microblock_trail, chain_ctx, ctx)
}

pub fn standardize_stacks_marshalled_microblock_trail(
    indexer_config: &IndexerConfig,
    marshalled_microblock_trail: JsonValue,
    chain_ctx: &mut StacksChainContext,
    ctx: &Context,
) -> Result<Vec<StacksMicroblockData>, String> {
    let mut microblock_trail: NewMicroblockTrail =
        serde_json::from_value(marshalled_microblock_trail)
            .map_err(|e| format!("unable to parse microblock trail {}", e))?;
    standardize_stacks_microblock_trail(indexer_config, &mut microblock_trail, chain_ctx, ctx)
}

pub fn standardize_stacks_microblock_trail(
    indexer_config: &IndexerConfig,
    microblock_trail: &mut NewMicroblockTrail,
    chain_ctx: &mut StacksChainContext,
    ctx: &Context,
) -> Result<Vec<StacksMicroblockData>, String> {
    let mut events: HashMap<&String, Vec<&NewEvent>> = HashMap::new();
    for event in microblock_trail.events.iter() {
        events
            .entry(&event.txid)
            .and_modify(|events| events.push(event))
            .or_insert(vec![&event]);
    }
    let mut microblocks_set: BTreeMap<
        (BlockIdentifier, BlockIdentifier),
        Vec<StacksTransactionData>,
    > = BTreeMap::new();
    for tx in microblock_trail.transactions.iter() {
        let tx_events = events.remove(&tx.txid).unwrap_or_default();
        let (description, tx_type, fee, nonce, sender, sponsor) =
            get_tx_description(&tx.raw_tx, &tx_events).expect("unable to parse transaction");

        let events = tx_events
            .iter()
            .map(|e| e.into_chainhook_event())
            .collect::<Result<Vec<StacksTransactionEvent>, String>>()?;
        let (receipt, operations) = get_standardized_stacks_receipt(
            &tx.txid,
            events,
            &mut chain_ctx.asset_class_map,
            &indexer_config.get_stacks_node_config().rpc_url.clone(),
            true,
        );

        let microblock_identifier = BlockIdentifier {
            hash: tx.microblock_hash.clone(),
            index: u64::try_from(tx.microblock_sequence).unwrap(),
        };

        let parent_microblock_identifier = if tx.microblock_sequence > 0 {
            BlockIdentifier {
                hash: tx.microblock_parent_hash.clone(),
                index: microblock_identifier.index.saturating_sub(1),
            }
        } else {
            microblock_identifier.clone()
        };

        let transaction = StacksTransactionData {
            transaction_identifier: TransactionIdentifier {
                hash: tx.txid.clone(),
            },
            operations,
            metadata: StacksTransactionMetadata {
                success: tx.status == "success",
                result: get_value_description(&tx.raw_result, ctx),
                raw_tx: tx.raw_tx.clone(),
                sender,
                fee,
                nonce,
                sponsor,
                kind: tx_type,
                execution_cost: tx.execution_cost.clone(),
                receipt,
                description,
                position: StacksTransactionPosition::micro_block(
                    microblock_identifier.clone(),
                    tx.tx_index,
                ),
                proof: None,
                contract_abi: tx.contract_abi.clone(),
            },
        };

        microblocks_set
            .entry((microblock_identifier, parent_microblock_identifier))
            .and_modify(|transactions| transactions.push(transaction.clone()))
            .or_insert(vec![transaction]);
    }

    let mut microblocks = vec![];
    for ((block_identifier, parent_block_identifier), transactions) in microblocks_set.into_iter() {
        microblocks.push(StacksMicroblockData {
            block_identifier,
            parent_block_identifier,
            timestamp: microblock_trail.burn_block_timestamp,
            transactions,
            metadata: StacksMicroblockMetadata {
                anchor_block_identifier: BlockIdentifier {
                    hash: microblock_trail.parent_index_block_hash.clone(),
                    index: 0,
                },
            },
        })
    }
    microblocks.sort_by(|a, b| b.block_identifier.cmp(&a.block_identifier));

    Ok(microblocks)
}

#[cfg(feature = "stacks-signers")]
pub fn standardize_stacks_marshalled_stackerdb_chunks(
    marshalled_stackerdb_chunks: JsonValue,
    ctx: &Context,
) -> Result<Vec<StacksStackerDbChunk>, String> {
    let stackerdb_chunks: NewStackerDbChunks = serde_json::from_value(marshalled_stackerdb_chunks)
        .map_err(|e| format!("unable to parse stackerdb chunks {e}"))?;
    standardize_stacks_stackerdb_chunks(&stackerdb_chunks, ctx)
}

#[cfg(feature = "stacks-signers")]
pub fn standardize_stacks_stackerdb_chunks(
    stackerdb_chunks: &NewStackerDbChunks,
    _ctx: &Context,
) -> Result<Vec<StacksStackerDbChunk>, String> {
    use stacks_codec::codec::{BlockResponse, RejectCode, SignerMessage, ValidateRejectCode};

    let contract_id = &stackerdb_chunks.contract_id.name;
    let mut parsed_chunks: Vec<StacksStackerDbChunk> = vec![];
    for slot in stackerdb_chunks.modified_slots.iter() {
        let data_bytes = hex::decode(&slot.data)
            .map_err(|e| format!("unable to decode signer slot hex data: {e}"))?;
        let signer_message = SignerMessage::consensus_deserialize(&mut Cursor::new(&data_bytes))
            .map_err(|e| format!("unable to deserialize SignerMessage: {e}"))?;
        let message = match signer_message {
            SignerMessage::BlockProposal(block_proposal) => {
                StacksSignerMessage::BlockProposal(BlockProposalData {
                    block: standardize_stacks_nakamoto_block(&block_proposal.block)?,
                    burn_height: block_proposal.burn_height,
                    reward_cycle: block_proposal.reward_cycle,
                })
            }
            SignerMessage::BlockResponse(block_response) => match block_response {
                BlockResponse::Accepted(block_accepted) => StacksSignerMessage::BlockResponse(
                    BlockResponseData::Accepted(BlockAcceptedResponse {
                        signer_signature_hash: format!(
                            "0x{}",
                            block_accepted.signer_signature_hash.to_hex()
                        ),
                        signature: format!("0x{}", block_accepted.signature.to_hex()),
                        metadata: SignerMessageMetadata {
                            server_version: block_accepted.metadata.server_version,
                        },
                    }),
                ),
                BlockResponse::Rejected(block_rejection) => StacksSignerMessage::BlockResponse(
                    BlockResponseData::Rejected(BlockRejectedResponse {
                        reason: block_rejection.reason,
                        reason_code: match block_rejection.reason_code {
                            RejectCode::ValidationFailed(validate_reject_code) => {
                                BlockRejectReasonCode::ValidationFailed {
                                    validation_failed: match validate_reject_code {
                                        ValidateRejectCode::BadBlockHash => {
                                            BlockValidationFailedCode::BadBlockHash
                                        }
                                        ValidateRejectCode::BadTransaction => {
                                            BlockValidationFailedCode::BadTransaction
                                        }
                                        ValidateRejectCode::InvalidBlock => {
                                            BlockValidationFailedCode::InvalidBlock
                                        }
                                        ValidateRejectCode::ChainstateError => {
                                            BlockValidationFailedCode::ChainstateError
                                        }
                                        ValidateRejectCode::UnknownParent => {
                                            BlockValidationFailedCode::UnknownParent
                                        }
                                        ValidateRejectCode::NonCanonicalTenure => {
                                            BlockValidationFailedCode::NonCanonicalTenure
                                        }
                                        ValidateRejectCode::NoSuchTenure => {
                                            BlockValidationFailedCode::NoSuchTenure
                                        }
                                    },
                                }
                            }
                            RejectCode::NoSortitionView => BlockRejectReasonCode::NoSortitionView,
                            RejectCode::ConnectivityIssues => {
                                BlockRejectReasonCode::ConnectivityIssues
                            }
                            RejectCode::RejectedInPriorRound => {
                                BlockRejectReasonCode::RejectedInPriorRound
                            }
                            RejectCode::SortitionViewMismatch => {
                                BlockRejectReasonCode::SortitionViewMismatch
                            }
                            RejectCode::TestingDirective => BlockRejectReasonCode::TestingDirective,
                        },
                        signer_signature_hash: format!(
                            "0x{}",
                            block_rejection.signer_signature_hash.to_hex()
                        ),
                        chain_id: block_rejection.chain_id,
                        signature: format!("0x{}", block_rejection.signature.to_hex()),
                        metadata: SignerMessageMetadata {
                            server_version: block_rejection.metadata.server_version,
                        },
                    }),
                ),
            },
            SignerMessage::BlockPushed(nakamoto_block) => {
                StacksSignerMessage::BlockPushed(BlockPushedData {
                    block: standardize_stacks_nakamoto_block(&nakamoto_block)?,
                })
            }
            SignerMessage::MockSignature(signature) => StacksSignerMessage::MockSignature(
                standardize_stacks_signer_mock_signature(&signature)?,
            ),
            SignerMessage::MockProposal(data) => StacksSignerMessage::MockProposal(
                standardize_stacks_signer_peer_info(&data.peer_info)?,
            ),
            SignerMessage::MockBlock(data) => StacksSignerMessage::MockBlock(MockBlockData {
                mock_proposal: MockProposalData {
                    peer_info: standardize_stacks_signer_peer_info(&data.mock_proposal.peer_info)?,
                },
                mock_signatures: data
                    .mock_signatures
                    .iter()
                    .map(standardize_stacks_signer_mock_signature)
                    .try_fold(
                        Vec::new(),
                        |mut acc, item| -> Result<Vec<MockSignatureData>, String> {
                            item.map(|val| {
                                acc.push(val);
                            })?;
                            Ok(acc)
                        },
                    )?,
            }),
        };
        parsed_chunks.push(StacksStackerDbChunk {
            contract: contract_id.clone(),
            sig: format!("0x{}", slot.sig),
            pubkey: format!(
                "0x{}",
                get_signer_pubkey_from_stackerdb_chunk_slot(slot, &data_bytes)?
            ),
            message,
        });
    }

    Ok(parsed_chunks)
}

#[cfg(feature = "stacks-signers")]
pub fn standardize_stacks_signer_mock_signature(
    signature: &stacks_codec::codec::MockSignature,
) -> Result<MockSignatureData, String> {
    let pubkey = get_signer_pubkey_from_message_hash(
        signature
            .mock_proposal
            .signer_signature_hash()
            .as_bytes()
            .as_ref(),
        &signature.signature,
    )?;
    Ok(MockSignatureData {
        mock_proposal: MockProposalData {
            peer_info: standardize_stacks_signer_peer_info(&signature.mock_proposal.peer_info)?,
        },
        metadata: SignerMessageMetadata {
            server_version: signature.metadata.server_version.clone(),
        },
        signature: format!("0x{}", signature.signature.to_hex()),
        pubkey: format!("0x{}", hex::encode(pubkey)),
    })
}

#[cfg(feature = "stacks-signers")]
pub fn standardize_stacks_signer_peer_info(
    peer_info: &stacks_codec::codec::PeerInfo,
) -> Result<PeerInfoData, String> {
    let block_hash = format!("0x{}", peer_info.stacks_tip.to_hex());
    Ok(PeerInfoData {
        burn_block_height: peer_info.burn_block_height,
        stacks_tip_consensus_hash: format!("0x{}", peer_info.stacks_tip_consensus_hash.to_hex()),
        stacks_tip: block_hash.clone(),
        stacks_tip_height: peer_info.stacks_tip_height,
        pox_consensus: format!("0x{}", peer_info.pox_consensus.to_hex()),
        server_version: peer_info.server_version.clone(),
        network_id: peer_info.network_id,
        index_block_hash: get_nakamoto_index_block_hash(
            &block_hash,
            &peer_info.stacks_tip_consensus_hash,
        )?,
    })
}

#[cfg(feature = "stacks-signers")]
pub fn standardize_stacks_nakamoto_block(
    block: &stacks_codec::codec::NakamotoBlock,
) -> Result<NakamotoBlockData, String> {
    use miniscript::bitcoin::hex::{Case, DisplayHex};

    let block_hash = get_nakamoto_block_hash(block)?;
    Ok(NakamotoBlockData {
        header: NakamotoBlockHeaderData {
            version: block.header.version,
            chain_length: block.header.chain_length,
            burn_spent: block.header.burn_spent,
            consensus_hash: format!("0x{}", block.header.consensus_hash.to_hex()),
            parent_block_id: format!("0x{}", block.header.parent_block_id.to_hex()),
            tx_merkle_root: format!("0x{}", block.header.tx_merkle_root.to_hex()),
            state_index_root: format!("0x{}", block.header.state_index_root.to_hex()),
            timestamp: block.header.timestamp,
            miner_signature: format!("0x{}", block.header.miner_signature.to_hex()),
            signer_signature: block
                .header
                .signer_signature
                .iter()
                .map(|s| format!("0x{}", s.to_hex()))
                .collect(),
            pox_treatment: format!(
                "0x{}",
                block
                    .header
                    .pox_treatment
                    .serialize_to_vec()
                    .to_hex_string(Case::Lower)
            ),
        },
        block_hash: block_hash.clone(),
        index_block_hash: get_nakamoto_index_block_hash(&block_hash, &block.header.consensus_hash)?,
        // TODO(rafaelcr): Parse and return transactions.
        transactions: vec![],
    })
}

#[cfg(feature = "stacks-signers")]
fn get_nakamoto_block_hash(block: &stacks_codec::codec::NakamotoBlock) -> Result<String, String> {
    use clarity::util::hash::Sha512Trunc256Sum;

    let mut block_header_bytes = vec![block.header.version];
    block_header_bytes.extend(block.header.chain_length.to_be_bytes());
    block_header_bytes.extend(block.header.burn_spent.to_be_bytes());
    block_header_bytes.extend(block.header.consensus_hash.as_bytes());
    block_header_bytes.extend(block.header.parent_block_id.as_bytes());
    block_header_bytes.extend(block.header.tx_merkle_root.as_bytes());
    block_header_bytes.extend(block.header.state_index_root.as_bytes());
    block_header_bytes.extend(block.header.timestamp.to_be_bytes());
    block_header_bytes.extend(block.header.miner_signature.as_bytes());
    block_header_bytes.extend(block.header.pox_treatment.serialize_to_vec());

    let hash = Sha512Trunc256Sum::from_data(&block_header_bytes).to_bytes();
    Ok(format!("0x{}", hex::encode(hash)))
}

#[cfg(feature = "stacks-signers")]
fn get_nakamoto_index_block_hash(
    block_hash: &str,
    consensus_hash: &clarity::types::chainstate::ConsensusHash,
) -> Result<String, String> {
    use clarity::util::hash::Sha512Trunc256Sum;

    let mut bytes =
        hex::decode(&block_hash[2..]).map_err(|e| format!("unable to decode block hash: {e}"))?;
    bytes.extend(consensus_hash.as_bytes());

    let hash = Sha512Trunc256Sum::from_data(&bytes).to_bytes();
    Ok(format!("0x{}", hex::encode(hash)))
}

pub fn get_signer_pubkey_from_message_hash(
    message_hash: &[u8],
    signature: &clarity::util::secp256k1::MessageSignature,
) -> Result<[u8; 33], String> {
    use miniscript::bitcoin::key::Secp256k1;
    use miniscript::bitcoin::secp256k1::ecdsa::{RecoverableSignature, RecoveryId};
    use miniscript::bitcoin::secp256k1::Message;

    let (first, sig) = signature.0.split_at(1);
    let rec_id = first[0];

    let secp = Secp256k1::new();
    let recovery_id =
        RecoveryId::from_i32(rec_id as i32).map_err(|e| format!("invalid recovery id: {e}"))?;
    let signature = RecoverableSignature::from_compact(sig, recovery_id)
        .map_err(|e| format!("invalid signature: {e}"))?;
    let message = Message::from_digest_slice(message_hash)
        .map_err(|e| format!("invalid digest message: {e}"))?;

    let pubkey = secp
        .recover_ecdsa(&message, &signature)
        .map_err(|e| format!("unable to recover pubkey: {e}"))?;

    Ok(pubkey.serialize())
}

#[cfg(feature = "stacks-signers")]
pub fn get_signer_pubkey_from_stackerdb_chunk_slot(
    slot: &NewSignerModifiedSlot,
    data_bytes: &[u8],
) -> Result<String, String> {
    use clarity::util::hash::Sha512Trunc256Sum;
    use miniscript::bitcoin::key::Secp256k1;
    use miniscript::bitcoin::secp256k1::ecdsa::{RecoverableSignature, RecoveryId};
    use miniscript::bitcoin::secp256k1::Message;

    let mut digest_bytes = slot.slot_id.to_be_bytes().to_vec();
    digest_bytes.extend(slot.slot_version.to_be_bytes());
    let data_bytes_hashed = Sha512Trunc256Sum::from_data(data_bytes).to_bytes();
    digest_bytes.extend(data_bytes_hashed);
    let digest = Sha512Trunc256Sum::from_data(&digest_bytes).to_bytes();

    let sig_bytes =
        hex::decode(&slot.sig).map_err(|e| format!("unable to decode signer slot sig: {e}"))?;
    let (first, sig) = sig_bytes.split_at(1);
    let rec_id = first[0];

    let secp = Secp256k1::new();
    let recovery_id =
        RecoveryId::from_i32(rec_id as i32).map_err(|e| format!("invalid recovery id: {e}"))?;
    let signature = RecoverableSignature::from_compact(sig, recovery_id)
        .map_err(|e| format!("invalid signature: {e}"))?;
    let message =
        Message::from_digest_slice(&digest).map_err(|e| format!("invalid digest message: {e}"))?;

    let pubkey = secp
        .recover_ecdsa(&message, &signature)
        .map_err(|e| format!("unable to recover signer pubkey: {e}"))?;

    Ok(hex::encode(pubkey.serialize()))
}

pub fn get_value_description(raw_value: &str, ctx: &Context) -> String {
    let raw_value = match raw_value.strip_prefix("0x") {
        Some(raw_value) => raw_value,
        _ => return raw_value.to_string(),
    };
    let value_bytes = match hex::decode(raw_value) {
        Ok(bytes) => bytes,
        _ => return raw_value.to_string(),
    };

    match ClarityValue::consensus_deserialize(&mut Cursor::new(&value_bytes)) {
        Ok(value) => format!("{}", value),
        Err(e) => {
            ctx.try_log(|logger| {
                slog::error!(logger, "unable to deserialize clarity value {:?}", e)
            });
            raw_value.to_string()
        }
    }
}

pub fn get_tx_description(
    raw_tx: &str,
    tx_events: &Vec<&NewEvent>,
) -> Result<
    (
        String, // Human readable transaction's description (contract-call, publish, ...)
        StacksTransactionKind, // Transaction kind
        u64,    // Transaction fee
        u64,    // Transaction nonce
        String, // Sender's address
        Option<String>, // Sponsor's address (optional)
    ),
    String,
> {
    let raw_tx = match raw_tx.strip_prefix("0x") {
        Some(raw_tx) => raw_tx,
        _ => return Err("unable to read txid".into()),
    };
    let tx_bytes = match hex::decode(raw_tx) {
        Ok(bytes) => bytes,
        Err(e) => return Err(format!("unable to read txid {}", e)),
    };

    // Handle Stacks transitions operated through Bitcoin transactions
    if tx_bytes.eq(&[0]) {
        if tx_events.is_empty() {
            return Err("received block with transaction '0x00' and no events".to_string());
        };
        for event in tx_events.iter() {
            if let Some(ref event_data) = event.stx_transfer_event {
                let data: STXTransferEventData = serde_json::from_value(event_data.clone())
                    .map_err(|e| format!("unable to decode event_data {}", e))?;
                let description = format!(
                    "transfered: {} µSTX from {} to {} through Bitcoin transaction",
                    data.amount, data.sender, data.recipient
                );
                let tx_type = StacksTransactionKind::NativeTokenTransfer;
                return Ok((description, tx_type, 0, 0, data.sender, None));
            } else if let Some(ref event_data) = event.stx_lock_event {
                let data: STXLockEventData = serde_json::from_value(event_data.clone())
                    .map_err(|e| format!("unable to decode event_data {}", e))?;
                let description = format!(
                    "stacked: {} µSTX by {} through Bitcoin transaction",
                    data.locked_amount, data.locked_address,
                );
                let tx_type =
                    StacksTransactionKind::BitcoinOp(BitcoinOpData::StackSTX(StackSTXData {
                        locked_amount: data.locked_amount,
                        unlock_height: data.unlock_height,
                        stacking_address: data.locked_address.clone(),
                    }));
                return Ok((description, tx_type, 0, 0, data.locked_address, None));
            } else if let Some(ref event_data) = event.contract_event {
                let data: SmartContractEventData = serde_json::from_value(event_data.clone())
                    .map_err(|e| format!("unable to decode event_data {}", e))?;
                if let Some(ClarityValue::Response(data)) =
                    try_decode_clarity_value(&data.hex_value)
                {
                    if data.committed {
                        if let ClarityValue::Tuple(outter) = *data.data {
                            if let Some(ClarityValue::Tuple(inner)) = outter.data_map.get("data") {
                                if let (
                                    Some(ClarityValue::Principal(stacking_address)),
                                    Some(ClarityValue::UInt(amount_ustx)),
                                    Some(ClarityValue::Principal(delegate)),
                                    Some(ClarityValue::Optional(pox_addr)),
                                    Some(ClarityValue::Optional(unlock_burn_height)),
                                ) = (
                                    &outter.data_map.get("stacker"),
                                    &inner.data_map.get("amount-ustx"),
                                    &inner.data_map.get("delegate-to"),
                                    &inner.data_map.get("pox-addr"),
                                    &inner.data_map.get("unlock-burn-height"),
                                ) {
                                    let description = format!(
                                    "stacked: {} µSTX delegated to {} through Bitcoin transaction",
                                    amount_ustx, delegate,
                                );
                                    let tx_type = StacksTransactionKind::BitcoinOp(
                                        BitcoinOpData::DelegateStackSTX(DelegateStackSTXData {
                                            stacking_address: stacking_address.to_string(),
                                            amount: amount_ustx.to_string(),
                                            delegate: delegate.to_string(),
                                            pox_address: match &pox_addr.data {
                                                Some(value) => match &**value {
                                                    ClarityValue::Tuple(address_comps) => {
                                                        match (
                                                            &address_comps.data_map.get("version"),
                                                            &address_comps
                                                                .data_map
                                                                .get("hashbytes"),
                                                        ) {
                                                            (
                                                                Some(ClarityValue::UInt(_version)),
                                                                Some(ClarityValue::Sequence(
                                                                    SequenceData::Buffer(
                                                                        _hashbytes,
                                                                    ),
                                                                )),
                                                            ) => None,
                                                            _ => None,
                                                        }
                                                    }
                                                    _ => None,
                                                },
                                                _ => None,
                                            },
                                            unlock_height: match &unlock_burn_height.data {
                                                Some(value) => match &**value {
                                                    ClarityValue::UInt(value) => {
                                                        Some(value.to_string())
                                                    }
                                                    _ => None,
                                                },
                                                _ => None,
                                            },
                                        }),
                                    );
                                    return Ok((description, tx_type, 0, 0, "".to_string(), None));
                                }
                            }
                        }
                    }
                }
            } else {
                return Ok((
                    "unsupported transaction".into(),
                    StacksTransactionKind::Unsupported,
                    0,
                    0,
                    "".to_string(),
                    None,
                ));
            }
        }
        return Err(format!(
            "unable to parse transaction {raw_tx} with events {:?}",
            tx_events
        ));
    }

    let tx = StacksTransaction::consensus_deserialize(&mut Cursor::new(&tx_bytes))
        .map_err(|e| format!("unable to consensus decode transaction {}", e))?;

    let (fee, nonce, sender, sponsor) = match tx.auth {
        TransactionAuth::Standard(ref conditions) => (
            conditions.tx_fee(),
            conditions.nonce(),
            if tx.is_mainnet() {
                conditions.address_mainnet().to_string()
            } else {
                conditions.address_testnet().to_string()
            },
            None,
        ),
        TransactionAuth::Sponsored(ref sender_conditions, ref sponsor_conditions) => (
            sponsor_conditions.tx_fee(),
            sender_conditions.nonce(),
            if tx.is_mainnet() {
                sender_conditions.address_mainnet().to_string()
            } else {
                sender_conditions.address_testnet().to_string()
            },
            Some(if tx.is_mainnet() {
                sponsor_conditions.address_mainnet().to_string()
            } else {
                sponsor_conditions.address_testnet().to_string()
            }),
        ),
    };

    let (description, tx_type) = match tx.payload {
        TransactionPayload::TokenTransfer(ref addr, ref amount, ref _memo) => (
            format!(
                "transfered: {} µSTX from {} to {}",
                amount,
                tx.origin_address(),
                addr
            ),
            StacksTransactionKind::NativeTokenTransfer,
        ),
        TransactionPayload::ContractCall(ref contract_call) => {
            let formatted_args = contract_call
                .function_args
                .iter()
                .map(|v| format!("{}", v))
                .collect::<Vec<String>>();
            (
                format!(
                    "invoked: {}.{}::{}({})",
                    contract_call.address,
                    contract_call.contract_name,
                    contract_call.function_name,
                    formatted_args.join(", ")
                ),
                StacksTransactionKind::ContractCall(StacksContractCallData {
                    contract_identifier: format!(
                        "{}.{}",
                        contract_call.address, contract_call.contract_name
                    ),
                    method: contract_call.function_name.to_string(),
                    args: formatted_args,
                }),
            )
        }
        TransactionPayload::SmartContract(ref smart_contract, ref _clarity_version) => {
            let contract_identifier = format!("{}.{}", tx.origin_address(), smart_contract.name);
            let data = StacksContractDeploymentData {
                contract_identifier: contract_identifier.clone(),
                code: smart_contract.code_body.to_string(),
            };
            (
                format!("deployed: {}", contract_identifier),
                StacksTransactionKind::ContractDeployment(data),
            )
        }
        TransactionPayload::Coinbase(_, _, _) => {
            ("coinbase".to_string(), StacksTransactionKind::Coinbase)
        }
        TransactionPayload::TenureChange(_) => (
            "tenure change".to_string(),
            StacksTransactionKind::TenureChange,
        ),
        TransactionPayload::PoisonMicroblock(_, _) => {
            ("other".to_string(), StacksTransactionKind::Unsupported)
        }
    };
    Ok((description, tx_type, fee, nonce, sender, sponsor))
}

pub fn get_standardized_fungible_currency_from_asset_class_id(
    asset_class_id: &str,
    asset_class_cache: &mut HashMap<String, AssetClassCache>,
    _node_url: &str,
) -> Currency {
    match asset_class_cache.get(asset_class_id) {
        None => {
            // TODO(lgalabru): re-approach this, with an adequate runtime strategy.
            // let comps = asset_class_id.split("::").collect::<Vec<&str>>();
            // let principal = comps[0].split(".").collect::<Vec<&str>>();
            // let contract_address = principal[0];
            // let contract_name = principal[1];
            // let stacks_rpc = StacksRpc::new(&node_url);
            // let value = stacks_rpc
            //     .call_read_only_fn(
            //         &contract_address,
            //         &contract_name,
            //         "get-symbol",
            //         vec![],
            //         contract_address,
            //     )
            //     .expect("Unable to retrieve symbol");
            let symbol = "TOKEN".into(); //value.expect_result_ok().expect_ascii();

            // let value = stacks_rpc
            //     .call_read_only_fn(
            //         &contract_address,
            //         &contract_name,
            //         "get-decimals",
            //         vec![],
            //         &contract_address,
            //     )
            //     .expect("Unable to retrieve decimals");
            let decimals = 6; // value.expect_result_ok().expect_u128() as u8;

            let entry = AssetClassCache { symbol, decimals };

            let currency = Currency {
                symbol: entry.symbol.clone(),
                decimals: entry.decimals.into(),
                metadata: Some(CurrencyMetadata {
                    asset_class_identifier: asset_class_id.into(),
                    asset_identifier: None,
                    standard: CurrencyStandard::Sip10,
                }),
            };

            asset_class_cache.insert(asset_class_id.into(), entry);

            currency
        }
        Some(entry) => Currency {
            symbol: entry.symbol.clone(),
            decimals: entry.decimals.into(),
            metadata: Some(CurrencyMetadata {
                asset_class_identifier: asset_class_id.into(),
                asset_identifier: None,
                standard: CurrencyStandard::Sip10,
            }),
        },
    }
}

pub fn get_standardized_non_fungible_currency_from_asset_class_id(
    asset_class_id: &str,
    asset_id: &str,
    _asset_class_cache: &mut HashMap<String, AssetClassCache>,
) -> Currency {
    Currency {
        symbol: asset_class_id.into(),
        decimals: 0,
        metadata: Some(CurrencyMetadata {
            asset_class_identifier: asset_class_id.into(),
            asset_identifier: Some(asset_id.into()),
            standard: CurrencyStandard::Sip09,
        }),
    }
}
//todo: this function has a lot of expects/panics. should return result instead
pub fn get_standardized_stacks_receipt(
    _txid: &str,
    events: Vec<StacksTransactionEvent>,
    asset_class_cache: &mut HashMap<String, AssetClassCache>,
    node_url: &str,
    include_operations: bool,
) -> (StacksTransactionReceipt, Vec<Operation>) {
    let mut mutated_contracts_radius = HashSet::new();
    let mut mutated_assets_radius = HashSet::new();
    let mut operations = vec![];

    if include_operations {
        let mut operation_id = 0;
        for event in events.iter() {
            match &event.event_payload {
                StacksTransactionEventPayload::STXMintEvent(data) => {
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: None,
                        type_: OperationType::Credit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.recipient.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount {
                            value: data.amount.parse::<u128>().expect("Unable to parse u64"),
                            currency: get_stacks_currency(),
                        }),
                        metadata: None,
                    });
                    operation_id += 1;
                }
                StacksTransactionEventPayload::STXLockEvent(data) => {
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: None,
                        type_: OperationType::Lock,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.locked_address.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount {
                            value: data
                                .locked_amount
                                .parse::<u128>()
                                .expect("Unable to parse u64"),
                            currency: get_stacks_currency(),
                        }),
                        metadata: None,
                    });
                    operation_id += 1;
                }
                StacksTransactionEventPayload::STXBurnEvent(data) => {
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: None,
                        type_: OperationType::Debit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.sender.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount {
                            value: data.amount.parse::<u128>().expect("Unable to parse u64"),
                            currency: get_stacks_currency(),
                        }),
                        metadata: None,
                    });
                    operation_id += 1;
                }
                StacksTransactionEventPayload::STXTransferEvent(data) => {
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: Some(vec![OperationIdentifier {
                            index: operation_id + 1,
                            network_index: None,
                        }]),
                        type_: OperationType::Debit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.sender.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount {
                            value: data.amount.parse::<u128>().expect("Unable to parse u64"),
                            currency: get_stacks_currency(),
                        }),
                        metadata: None,
                    });
                    operation_id += 1;
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: Some(vec![OperationIdentifier {
                            index: operation_id - 1,
                            network_index: None,
                        }]),
                        type_: OperationType::Credit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.recipient.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount {
                            value: data.amount.parse::<u128>().expect("Unable to parse u64"),
                            currency: get_stacks_currency(),
                        }),
                        metadata: None,
                    });
                    operation_id += 1;
                }
                StacksTransactionEventPayload::NFTMintEvent(data) => {
                    let (asset_class_identifier, contract_identifier) =
                        get_mutated_ids(&data.asset_class_identifier);
                    mutated_assets_radius.insert(asset_class_identifier);
                    mutated_contracts_radius.insert(contract_identifier);

                    let currency = get_standardized_non_fungible_currency_from_asset_class_id(
                        &data.asset_class_identifier,
                        &data.hex_asset_identifier,
                        asset_class_cache,
                    );
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: None,
                        type_: OperationType::Credit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.recipient.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount { value: 1, currency }),
                        metadata: None,
                    });
                    operation_id += 1;
                }
                StacksTransactionEventPayload::NFTBurnEvent(data) => {
                    let (asset_class_identifier, contract_identifier) =
                        get_mutated_ids(&data.asset_class_identifier);
                    mutated_assets_radius.insert(asset_class_identifier);
                    mutated_contracts_radius.insert(contract_identifier);

                    let currency = get_standardized_non_fungible_currency_from_asset_class_id(
                        &data.asset_class_identifier,
                        &data.hex_asset_identifier,
                        asset_class_cache,
                    );
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: None,
                        type_: OperationType::Debit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.sender.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount { value: 1, currency }),
                        metadata: None,
                    });
                    operation_id += 1;
                }
                StacksTransactionEventPayload::NFTTransferEvent(data) => {
                    let (asset_class_identifier, contract_identifier) =
                        get_mutated_ids(&data.asset_class_identifier);
                    mutated_assets_radius.insert(asset_class_identifier);
                    mutated_contracts_radius.insert(contract_identifier);

                    let currency = get_standardized_non_fungible_currency_from_asset_class_id(
                        &data.asset_class_identifier,
                        &data.hex_asset_identifier,
                        asset_class_cache,
                    );
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: Some(vec![OperationIdentifier {
                            index: operation_id + 1,
                            network_index: None,
                        }]),
                        type_: OperationType::Debit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.sender.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount {
                            value: 1,
                            currency: currency.clone(),
                        }),
                        metadata: None,
                    });
                    operation_id += 1;
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: Some(vec![OperationIdentifier {
                            index: operation_id - 1,
                            network_index: None,
                        }]),
                        type_: OperationType::Credit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.recipient.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount { value: 1, currency }),
                        metadata: None,
                    });
                    operation_id += 1;
                }
                StacksTransactionEventPayload::FTMintEvent(data) => {
                    let (asset_class_identifier, contract_identifier) =
                        get_mutated_ids(&data.asset_class_identifier);
                    mutated_assets_radius.insert(asset_class_identifier);
                    mutated_contracts_radius.insert(contract_identifier);

                    let currency = get_standardized_fungible_currency_from_asset_class_id(
                        &data.asset_class_identifier,
                        asset_class_cache,
                        node_url,
                    );

                    let value = match data.amount.parse::<u128>() {
                        Ok(value) => value,
                        Err(e) => {
                            panic!("unable to parse u64 {:?}: {:?}", data, e);
                        }
                    };

                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: None,
                        type_: OperationType::Credit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.recipient.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount { value, currency }),
                        metadata: None,
                    });
                    operation_id += 1;
                }
                StacksTransactionEventPayload::FTBurnEvent(data) => {
                    let (asset_class_identifier, contract_identifier) =
                        get_mutated_ids(&data.asset_class_identifier);
                    mutated_assets_radius.insert(asset_class_identifier);
                    mutated_contracts_radius.insert(contract_identifier);

                    let currency = get_standardized_fungible_currency_from_asset_class_id(
                        &data.asset_class_identifier,
                        asset_class_cache,
                        node_url,
                    );
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: None,
                        type_: OperationType::Debit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.sender.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount {
                            value: data.amount.parse::<u128>().expect("Unable to parse u64"),
                            currency,
                        }),
                        metadata: None,
                    });
                    operation_id += 1;
                }
                StacksTransactionEventPayload::FTTransferEvent(data) => {
                    let (asset_class_identifier, contract_identifier) =
                        get_mutated_ids(&data.asset_class_identifier);
                    mutated_assets_radius.insert(asset_class_identifier);
                    mutated_contracts_radius.insert(contract_identifier);

                    let currency = get_standardized_fungible_currency_from_asset_class_id(
                        &data.asset_class_identifier,
                        asset_class_cache,
                        node_url,
                    );
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: Some(vec![OperationIdentifier {
                            index: operation_id + 1,
                            network_index: None,
                        }]),
                        type_: OperationType::Debit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.sender.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount {
                            value: data.amount.parse::<u128>().expect("Unable to parse u64"),
                            currency: currency.clone(),
                        }),
                        metadata: None,
                    });
                    operation_id += 1;
                    operations.push(Operation {
                        operation_identifier: OperationIdentifier {
                            index: operation_id,
                            network_index: None,
                        },
                        related_operations: Some(vec![OperationIdentifier {
                            index: operation_id - 1,
                            network_index: None,
                        }]),
                        type_: OperationType::Credit,
                        status: Some(OperationStatusKind::Success),
                        account: AccountIdentifier {
                            address: data.recipient.clone(),
                            sub_account: None,
                        },
                        amount: Some(Amount {
                            value: data.amount.parse::<u128>().expect("Unable to parse u64"),
                            currency,
                        }),
                        metadata: None,
                    });
                    operation_id += 1;
                }
                StacksTransactionEventPayload::DataVarSetEvent(_data) => {}
                StacksTransactionEventPayload::DataMapInsertEvent(_data) => {}
                StacksTransactionEventPayload::DataMapUpdateEvent(_data) => {}
                StacksTransactionEventPayload::DataMapDeleteEvent(_data) => {}
                StacksTransactionEventPayload::SmartContractEvent(data) => {
                    mutated_contracts_radius.insert(data.contract_identifier.clone());
                }
            }
        }
    }

    let receipt =
        StacksTransactionReceipt::new(mutated_contracts_radius, mutated_assets_radius, events);
    (receipt, operations)
}

fn get_mutated_ids(asset_class_id: &str) -> (String, String) {
    let contract_id = asset_class_id.split("::").collect::<Vec<_>>()[0];
    (asset_class_id.into(), contract_id.into())
}

#[cfg(test)]
pub mod tests;
