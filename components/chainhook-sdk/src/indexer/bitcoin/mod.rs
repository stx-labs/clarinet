use std::time::Duration;

use bitcoincore_rpc::bitcoin::hashes::Hash;
use bitcoincore_rpc::bitcoin::{self, Address, Amount, BlockHash};
use bitcoincore_rpc::jsonrpc::error::RpcError;
use bitcoincore_rpc_json::GetRawTransactionResultVoutScriptPubKey;
use chainhook_types::bitcoin::{OutPoint, TxIn, TxOut};
use chainhook_types::{
    BitcoinBlockData, BitcoinBlockMetadata, BitcoinNetwork, BitcoinTransactionData,
    BitcoinTransactionMetadata, BlockCommitmentData, BlockHeader, BlockIdentifier,
    KeyRegistrationData, LockSTXData, PoxReward, StacksBaseChainOperation,
    StacksBlockCommitmentData, TransactionIdentifier, TransferSTXData,
};
use hiro_system_kit::slog;
use reqwest::Client as HttpClient;
use serde::{Deserialize, Serialize};
use serde_json::json;

use super::fork_scratch_pad::CONFIRMED_SEGMENT_MINIMUM_LENGTH;
use crate::chainhooks::bitcoin::{
    get_canonical_pox_config, get_stacks_canonical_magic_bytes, StacksOpcodes,
};
use crate::chainhooks::types::PoxConfig;
use crate::observer::BitcoinConfig;
use crate::utils::Context;

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BitcoinBlockFullBreakdown {
    pub hash: String,
    pub height: usize,
    pub tx: Vec<BitcoinTransactionFullBreakdown>,
    pub time: usize,
    pub nonce: u32,
    pub previousblockhash: Option<String>,
    pub confirmations: i32,
}

impl BitcoinBlockFullBreakdown {
    pub fn get_block_header(&self) -> BlockHeader {
        // Block id
        let hash = format!("0x{}", self.hash);
        let block_identifier = BlockIdentifier {
            index: self.height as u64,
            hash,
        };
        // Parent block id
        let parent_block_hash = match self.previousblockhash {
            Some(ref value) => format!("0x{}", value),
            None => format!("0x{}", BlockHash::all_zeros()),
        };
        let parent_block_identifier = BlockIdentifier {
            index: (self.height - 1) as u64,
            hash: parent_block_hash,
        };
        BlockHeader {
            block_identifier,
            parent_block_identifier,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BitcoinTransactionFullBreakdown {
    pub txid: String,
    pub vin: Vec<BitcoinTransactionInputFullBreakdown>,
    pub vout: Vec<BitcoinTransactionOutputFullBreakdown>,
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BitcoinTransactionInputFullBreakdown {
    pub sequence: u32,
    /// The raw scriptSig in case of a coinbase tx.
    // #[serde(default, with = "bitcoincore_rpc_json::serde_hex::opt")]
    // pub coinbase: Option<Vec<u8>>,
    /// Not provided for coinbase txs.
    pub txid: Option<String>,
    /// Not provided for coinbase txs.
    pub vout: Option<u32>,
    /// The scriptSig in case of a non-coinbase tx.
    pub script_sig: Option<GetRawTransactionResultVinScriptSig>,
    /// Not provided for coinbase txs.
    pub txinwitness: Option<Vec<String>>,
    pub prevout: Option<BitcoinTransactionInputPrevoutFullBreakdown>,
}

#[derive(Clone, PartialEq, Eq, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GetRawTransactionResultVinScriptSig {
    pub hex: String,
}

impl BitcoinTransactionInputFullBreakdown {
    /// Whether this input is from a coinbase tx. If there is not a [BitcoinTransactionInputFullBreakdown::txid] field, the transaction is a coinbase transaction.
    // Note: vout and script_sig fields are also not provided for coinbase transactions.
    pub fn is_coinbase(&self) -> bool {
        self.txid.is_none()
    }
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BitcoinTransactionInputPrevoutFullBreakdown {
    pub height: u64,
    #[serde(with = "bitcoin::amount::serde::as_btc")]
    pub value: Amount,
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct BitcoinTransactionOutputFullBreakdown {
    #[serde(with = "bitcoin::amount::serde::as_btc")]
    pub value: Amount,
    pub n: u32,
    pub script_pub_key: GetRawTransactionResultVoutScriptPubKey,
}

#[derive(Deserialize, Serialize)]
pub struct NewBitcoinBlock {
    pub burn_block_hash: String,
    pub burn_block_height: u64,
    pub reward_slot_holders: Vec<String>,
    pub reward_recipients: Vec<RewardParticipant>,
    pub burn_amount: u64,
}

#[allow(dead_code)]
#[derive(Deserialize, Serialize)]
pub struct RewardParticipant {
    recipient: String,
    amt: u64,
}

pub fn build_http_client() -> HttpClient {
    HttpClient::builder()
        .timeout(Duration::from_secs(15))
        .http1_only()
        .no_hickory_dns()
        .connect_timeout(Duration::from_secs(15))
        .tcp_keepalive(Some(Duration::from_secs(15)))
        .no_proxy()
        .danger_accept_invalid_certs(true)
        .build()
        .expect("Unable to build http client")
}

pub async fn download_and_parse_block_with_retry(
    http_client: &HttpClient,
    block_hash: &str,
    bitcoin_config: &BitcoinConfig,
    ctx: &Context,
) -> Result<BitcoinBlockFullBreakdown, String> {
    let mut errors_count = 0;
    let max_retries = 20;
    let block = loop {
        match download_and_parse_block(http_client, block_hash, bitcoin_config, ctx).await {
            Ok(result) => break result,
            Err(e) => {
                errors_count += 1;
                if errors_count > 3 && errors_count < max_retries {
                    ctx.try_log(|logger| {
                        slog::warn!(
                            logger,
                            "unable to fetch and parse block #{block_hash}: will retry in a few seconds (attempt #{errors_count}). Error: {e}",
                        )
                    });
                } else if errors_count == max_retries {
                    return Err(format!("unable to fetch and parse block #{block_hash} after {errors_count} attempts. Error: {e}"));
                }
                std::thread::sleep(std::time::Duration::from_secs(1));
            }
        }
    };
    Ok(block)
}

pub async fn retrieve_block_hash_with_retry(
    http_client: &HttpClient,
    block_height: &u64,
    bitcoin_config: &BitcoinConfig,
    ctx: &Context,
) -> Result<String, String> {
    let mut errors_count = 0;
    let max_retries = 10;
    let block_hash = loop {
        match retrieve_block_hash(http_client, block_height, bitcoin_config, ctx).await {
            Ok(result) => break result,
            Err(e) => {
                errors_count += 1;
                if errors_count > 3 && errors_count < max_retries {
                    ctx.try_log(|logger| {
                        slog::warn!(
                            logger,
                            "unable to retrieve block hash #{block_height}: will retry in a few seconds (attempt #{errors_count}). Error: {e}",
                        )
                    });
                } else if errors_count == max_retries {
                    return Err(format!("unable to retrieve block hash #{block_height} after {errors_count} attempts. Error: {e}"));
                }
                std::thread::sleep(std::time::Duration::from_secs(2));
            }
        }
    };
    Ok(block_hash)
}

pub async fn retrieve_block_hash(
    http_client: &HttpClient,
    block_height: &u64,
    bitcoin_config: &BitcoinConfig,
    _ctx: &Context,
) -> Result<String, String> {
    let body = json!({
        "jsonrpc": "1.0",
        "id": "chainhook-cli",
        "method": "getblockhash",
        "params": [block_height]
    });
    let block_hash = http_client
        .post(&bitcoin_config.rpc_url)
        .basic_auth(&bitcoin_config.username, Some(&bitcoin_config.password))
        .header("Content-Type", "application/json")
        .header("Host", &bitcoin_config.rpc_url[7..])
        .json(&body)
        .send()
        .await
        .map_err(|e| format!("unable to send request ({})", e))?
        .json::<bitcoincore_rpc::jsonrpc::Response>()
        .await
        .map_err(|e| format!("unable to parse response ({})", e))?
        .result::<String>()
        .map_err(|e| format!("unable to parse response ({})", e))?;

    Ok(block_hash)
}

// not used internally by chainhook; exported for ordhook
pub async fn try_download_block_bytes_with_retry(
    http_client: HttpClient,
    block_height: u64,
    bitcoin_config: BitcoinConfig,
    ctx: Context,
) -> Result<Vec<u8>, String> {
    let block_hash =
        retrieve_block_hash_with_retry(&http_client, &block_height, &bitcoin_config, &ctx)
            .await
            .unwrap();

    let mut errors_count = 0;

    let response = loop {
        match download_block(&http_client, &block_hash, &bitcoin_config, &ctx).await {
            Ok(result) => break result,
            Err(_e) => {
                errors_count += 1;
                if errors_count > 1 {
                    ctx.try_log(|logger| {
                        slog::warn!(
                            logger,
                            "unable to fetch block #{block_hash}: will retry in a few seconds (attempt #{errors_count}).",
                        )
                    });
                }
                std::thread::sleep(std::time::Duration::from_millis(1500));
                continue;
            }
        }
    };
    Ok(response)
}

#[derive(Debug, Clone, Deserialize)]
pub struct RpcErrorResponse {
    pub error: RpcError,
}

pub async fn download_block(
    http_client: &HttpClient,
    block_hash: &str,
    bitcoin_config: &BitcoinConfig,
    _ctx: &Context,
) -> Result<Vec<u8>, String> {
    let body = json!({
        "jsonrpc": "1.0",
        "id": "chainhook-cli",
        "method": "getblock",
        "params": [block_hash, 3]
    });
    let res = http_client
        .post(&bitcoin_config.rpc_url)
        .basic_auth(&bitcoin_config.username, Some(&bitcoin_config.password))
        .header("Content-Type", "application/json")
        .header("Host", &bitcoin_config.rpc_url[7..])
        .json(&body)
        .send()
        .await
        .map_err(|e| format!("unable to send request ({})", e))?;

    // Check status code
    if !res.status().is_success() {
        return Err(format!(
            "http request unsuccessful ({:?})",
            res.error_for_status()
        ));
    }

    let rpc_response_bytes = res
        .bytes()
        .await
        .map_err(|e| format!("unable to get bytes ({})", e))?
        .to_vec();

    // Check rpc error presence
    if let Ok(rpc_error) = serde_json::from_slice::<RpcErrorResponse>(&rpc_response_bytes[..]) {
        return Err(format!(
            "rpc request unsuccessful ({})",
            rpc_error.error.message
        ));
    }

    Ok(rpc_response_bytes)
}

pub fn parse_downloaded_block(
    downloaded_block: Vec<u8>,
) -> Result<BitcoinBlockFullBreakdown, String> {
    let block = serde_json::from_slice::<bitcoincore_rpc::jsonrpc::Response>(&downloaded_block[..])
        .map_err(|e| format!("unable to parse jsonrpc payload ({})", e))?
        .result::<BitcoinBlockFullBreakdown>()
        .map_err(|e| format!("unable to parse block ({})", e))?;
    Ok(block)
}

pub async fn download_and_parse_block(
    http_client: &HttpClient,
    block_hash: &str,
    bitcoin_config: &BitcoinConfig,
    _ctx: &Context,
) -> Result<BitcoinBlockFullBreakdown, String> {
    let response = download_block(http_client, block_hash, bitcoin_config, _ctx).await?;
    parse_downloaded_block(response)
}

pub fn standardize_bitcoin_block(
    block: BitcoinBlockFullBreakdown,
    network: &BitcoinNetwork,
    ctx: &Context,
) -> Result<BitcoinBlockData, (String, bool)> {
    let mut transactions = vec![];
    let block_height = block.height as u64;
    let expected_magic_bytes = get_stacks_canonical_magic_bytes(network);
    let pox_config = get_canonical_pox_config(network);

    ctx.try_log(|logger| slog::debug!(logger, "Standardizing Bitcoin block {}", block.hash,));

    for (tx_index, mut tx) in block.tx.into_iter().enumerate() {
        let txid = tx.txid.to_string();

        let mut stacks_operations = vec![];
        if let Some(op) = try_parse_stacks_operation(
            block_height,
            &tx.vin,
            &tx.vout,
            &pox_config,
            &expected_magic_bytes,
            ctx,
        ) {
            stacks_operations.push(op);
        }

        let mut inputs = vec![];
        let mut sats_in = 0;
        for (index, input) in tx.vin.drain(..).enumerate() {
            if input.is_coinbase() {
                continue;
            }
            let prevout = input.prevout.as_ref().ok_or((
                format!(
                    "error retrieving prevout for transaction {}, input #{} (block #{})",
                    tx.txid, index, block.height
                ),
                true,
            ))?;

            let txid = input.txid.as_ref().ok_or((
                format!(
                    "error retrieving txid for transaction {}, input #{} (block #{})",
                    tx.txid, index, block.height
                ),
                true,
            ))?;

            let vout = input.vout.ok_or((
                format!(
                    "error retrieving vout for transaction {}, input #{} (block #{})",
                    tx.txid, index, block.height
                ),
                true,
            ))?;

            let script_sig = input.script_sig.ok_or((
                format!(
                    "error retrieving script_sig for transaction {}, input #{} (block #{})",
                    tx.txid, index, block.height
                ),
                true,
            ))?;

            sats_in += prevout.value.to_sat();

            inputs.push(TxIn {
                previous_output: OutPoint {
                    txid: TransactionIdentifier::new(&txid.to_string()),
                    vout,
                    block_height: prevout.height,
                    value: prevout.value.to_sat(),
                },
                script_sig: format!("0x{}", script_sig.hex),
                sequence: input.sequence,
                witness: input
                    .txinwitness
                    .unwrap_or(vec![])
                    .to_vec()
                    .iter()
                    .map(|w| format!("0x{}", w))
                    .collect::<Vec<_>>(),
            });
        }

        let mut outputs = vec![];
        let mut sats_out = 0;
        for output in tx.vout.drain(..) {
            let value = output.value.to_sat();
            sats_out += value;
            outputs.push(TxOut {
                value,
                script_pubkey: format!("0x{}", hex::encode(&output.script_pub_key.hex)),
            });
        }

        let tx = BitcoinTransactionData {
            transaction_identifier: TransactionIdentifier {
                hash: format!("0x{}", txid),
            },
            operations: vec![],
            metadata: BitcoinTransactionMetadata {
                inputs,
                outputs,
                stacks_operations,
                proof: None,
                fee: sats_in.saturating_sub(sats_out),
                index: tx_index as u32,
            },
        };
        transactions.push(tx);
    }

    Ok(BitcoinBlockData {
        block_identifier: BlockIdentifier {
            hash: format!("0x{}", block.hash),
            index: block_height,
        },
        parent_block_identifier: BlockIdentifier {
            hash: format!(
                "0x{}",
                block
                    .previousblockhash
                    .unwrap_or(BlockHash::all_zeros().to_string())
            ),
            index: match block_height {
                0 => 0,
                _ => block_height - 1,
            },
        },
        timestamp: block.time as u32,
        metadata: BitcoinBlockMetadata {
            network: network.clone(),
        },
        transactions,
    })
}

fn try_parse_stacks_operation(
    block_height: u64,
    _inputs: &Vec<BitcoinTransactionInputFullBreakdown>,
    outputs: &[BitcoinTransactionOutputFullBreakdown],
    pox_config: &PoxConfig,
    expected_magic_bytes: &[u8; 2],
    ctx: &Context,
) -> Option<StacksBaseChainOperation> {
    if outputs.is_empty() {
        return None;
    }

    // Safely parsing the first 2 bytes (following OP_RETURN + PUSH_DATA)
    let op_return_output = &outputs[0].script_pub_key.hex;
    if op_return_output.len() < CONFIRMED_SEGMENT_MINIMUM_LENGTH as usize {
        return None;
    }
    if op_return_output[3] != expected_magic_bytes[0]
        || op_return_output[4] != expected_magic_bytes[1]
    {
        return None;
    }
    // Safely classifying the Stacks operation;
    let op_type: StacksOpcodes = match op_return_output[5].try_into() {
        Ok(op) => op,
        Err(_) => {
            ctx.try_log(|logger| {
                slog::debug!(
                    logger,
                    "Stacks operation parsing - opcode {} is not a stacks operation; skipping transaction",
                    op_return_output[5]
                )
            });
            return None;
        }
    };
    let op = match op_type {
        StacksOpcodes::KeyRegister => {
            let res = try_parse_key_register_op(&op_return_output[6..])?;
            StacksBaseChainOperation::LeaderRegistered(res)
        }
        StacksOpcodes::PreStx => {
            try_parse_pre_stx_op(&op_return_output[6..])?;
            return None;
        }
        StacksOpcodes::TransferStx => {
            let res = try_parse_transfer_stx_op(&op_return_output[6..])?;
            StacksBaseChainOperation::StxTransferred(res)
        }
        StacksOpcodes::StackStx => {
            let res = try_parse_stacks_stx_op(&op_return_output[6..])?;
            StacksBaseChainOperation::StxLocked(res)
        }
        StacksOpcodes::BlockCommit => {
            let res = try_parse_block_commit_op(&op_return_output[5..])?;
            let mut pox_sats_burnt = 0;
            let mut pox_sats_transferred = vec![];

            let output_1 = outputs
                .get(1)
                .ok_or("expected output 1 not found".to_string())
                .ok()?;
            let script_1 = output_1
                .script_pub_key
                .script()
                .map_err(|_e| "expected output 1 corrupted".to_string())
                .ok()?;
            let address_1 = Address::from_script(&script_1, bitcoin::Network::Bitcoin)
                .map_err(|_e| "expected output 1 corrupted".to_string())
                .ok()?;

            let output_2 = outputs
                .get(2)
                .ok_or("expected output 2 not found".to_string())
                .ok()?;
            let script_2 = output_2
                .script_pub_key
                .script()
                .map_err(|_e| "expected output 2 corrupted".to_string())
                .ok()?;
            let address_2 = Address::from_script(&script_2, bitcoin::Network::Bitcoin)
                .map_err(|_e| "expected output 2 corrupted".to_string())
                .ok()?;

            let output_1_is_burn = address_1.to_string().eq(pox_config.get_burn_address());
            let output_2_is_burn = address_2.to_string().eq(pox_config.get_burn_address());

            // PoX commitments have the following outputs:
            //  - 0: OP_RETURN
            //  - 1: rewarding address (could be a reward address, could be burn address in some rare cases)
            //  - 2: rewarding address (could be a reward address, could be burn address in some rare cases; always burn address if 1 was burn address)
            //  - [3-n]: change outputs
            //
            // PoB commitments have:
            //  - 0: OP_RETURN
            //  - 1: Burn address
            //  - [3-n]: change outputs
            //
            // So, to determine if PoX vs PoB, we check if output 1 is the burn address
            //  - If not, we definitely have a PoX block commitment
            //  - If it is, we need to check if output 2 is the burn address
            //    - If it is, we have a PoX block commitment
            //    - If not, we have a PoB block commitment
            //
            // The only assumption we're making in this logic is that the first change output doesn't
            // get sent to the burn address, in which case we'd incorrectly label a PoB block commitment as Pox.
            let mining_output_index = if !output_1_is_burn || output_2_is_burn {
                // We have a PoX Block Commitment
                // Output 0 is OP_RETURN
                // Output 1 is rewarding Address 1
                if output_1_is_burn {
                    pox_sats_burnt += output_1.value.to_sat();
                } else {
                    pox_sats_transferred.push(PoxReward {
                        recipient_address: address_1.to_string(),
                        amount: output_1.value.to_sat(),
                    });
                }
                // Output 2 is rewarding Address 2
                if output_2_is_burn {
                    pox_sats_burnt += output_2.value.to_sat();
                } else {
                    pox_sats_transferred.push(PoxReward {
                        recipient_address: address_2.to_string(),
                        amount: output_2.value.to_sat(),
                    });
                }
                // Output 3 is used for miner chained commitments
                3
            } else {
                // We have a PoB Block Commitment
                // Output 0 is OP_RETURN
                // Output 1 is be a Burn Address
                pox_sats_burnt += output_1.value.to_sat();
                // Output 2 is used for miner chained commitments
                2
            };

            let mut mining_sats_left = 0;
            let mut mining_address_post_commit = None;
            if let Some(mining_post_commit) = outputs.get(mining_output_index) {
                mining_sats_left = mining_post_commit.value.to_sat();
                mining_address_post_commit = match mining_post_commit.script_pub_key.script() {
                    Ok(script) => Address::from_script(&script, bitcoin::Network::Bitcoin)
                        .map(|a| a.to_string())
                        .ok(),
                    Err(_) => None,
                };
            }

            // let mining_address_pre_commit = match inputs[0].script_sig {
            //     Some(script) => match script.script() {

            //     }

            // }
            // mining_address_post_commit = match inputs.first().and_then(|i| i.script_sig).and_then(|s| s.script()).script_pub_key.script() {
            //     Ok(script) => Address::from_script(&script, bitcoin::Network::Bitcoin).and_then(|a| Ok(a.to_string())).ok(),
            //     Err(_) => None
            // };

            let pox_cycle_index = pox_config.get_pox_cycle_id(block_height);
            let pox_cycle_length = pox_config.get_pox_cycle_len();
            let pox_cycle_position = pox_config.get_pos_in_pox_cycle(block_height);

            StacksBaseChainOperation::BlockCommitted(StacksBlockCommitmentData {
                block_hash: res.stacks_block_hash,
                pox_cycle_index,
                pox_cycle_length,
                pox_cycle_position,
                pox_sats_burnt,
                pox_sats_transferred,
                // mining_address_pre_commit: None,
                mining_address_post_commit,
                mining_sats_left,
            })
        }
    };

    Some(op)
}

fn try_parse_block_commit_op(bytes: &[u8]) -> Option<BlockCommitmentData> {
    if bytes.len() < 32 {
        return None;
    }

    Some(BlockCommitmentData {
        stacks_block_hash: format!("0x{}", hex::encode(&bytes[0..32])),
    })
}

fn try_parse_key_register_op(_bytes: &[u8]) -> Option<KeyRegistrationData> {
    Some(KeyRegistrationData {})
}

fn try_parse_pre_stx_op(_bytes: &[u8]) -> Option<()> {
    None
}

fn try_parse_transfer_stx_op(bytes: &[u8]) -> Option<TransferSTXData> {
    if bytes.len() < 16 {
        return None;
    }

    // todo(lgalabru)
    Some(TransferSTXData {
        sender: "".into(),
        recipient: "".into(),
        amount: "".into(),
    })
}

fn try_parse_stacks_stx_op(bytes: &[u8]) -> Option<LockSTXData> {
    if bytes.len() < 16 {
        return None;
    }

    // todo(lgalabru)
    Some(LockSTXData {
        sender: "".into(),
        amount: "".into(),
        duration: 1,
    })
}

#[cfg(test)]
pub mod tests;

// Test vectors
// 1) Devnet PoB
// 2022-10-26T03:06:17.376341Z  INFO chainhook_event_observer::indexer: BitcoinBlockData { block_identifier: BlockIdentifier { index: 104, hash: "0x210d0d095a75d88fc059cb97f453eee33b1833153fb1f81b9c3c031c26bb106b" }, parent_block_identifier: BlockIdentifier { index: 103, hash: "0x5d5a4b8113c35f20fb0b69b1fb1ae1b88461ea57e2a2e4c036f97fae70ca1abb" }, timestamp: 1666753576, transactions: [BitcoinTransactionData { transaction_identifier: TransactionIdentifier { hash: "0xfaaac1833dc4883e7ec28f61e35b41f896c395f8d288b1a177155de2abd6052f" }, operations: [], metadata: BitcoinTransactionMetadata { inputs: [TxIn { previous_output: OutPoint { txid: "0000000000000000000000000000000000000000000000000000000000000000", vout: 4294967295 }, script_sig: "01680101", sequence: 4294967295, witness: [] }], outputs: [TxOut { value: 5000017550, script_pubkey: "76a914ee9369fb719c0ba43ddf4d94638a970b84775f4788ac" }, TxOut { value: 0, script_pubkey: "6a24aa21a9ed4a190dfdc77e260409c2a693e6d3b8eca43afbc4bffb79ddcdcc9516df804d9b" }], stacks_operations: [] } }, BitcoinTransactionData { transaction_identifier: TransactionIdentifier { hash: "0x59193c24cb2325cd2271b89f790f958dcd4065088680ffbc201a0ebb2f3cbf25" }, operations: [], metadata: BitcoinTransactionMetadata { inputs: [TxIn { previous_output: OutPoint { txid: "9eebe848baaf8dd4810e4e4a91168e2e471c949439faf5d768750ca21d067689", vout: 3 }, script_sig: "483045022100a20f90e9e3c3bb7e558ad4fa65902d8cf6ce4bff1f5af0ac0a323b547385069c022021b9877abbc9d1eef175c7f712ac1b2d8f5ce566be542714effe42711e75b83801210239810ebf35e6f6c26062c99f3e183708d377720617c90a986859ec9c95d00be9", sequence: 4294967293, witness: [] }], outputs: [TxOut { value: 0, script_pubkey: "6a4c5069645b1681995f8e568287e0e4f5cbc1d6727dafb5e3a7822a77c69bd04208265aca9424d0337dac7d9e84371a2c91ece1891d67d3554bd9fdbe60afc6924d4b0773d90000006700010000006600012b" }, TxOut { value: 10000, script_pubkey: "76a914000000000000000000000000000000000000000088ac" }, TxOut { value: 10000, script_pubkey: "76a914000000000000000000000000000000000000000088ac" }, TxOut { value: 4999904850, script_pubkey: "76a914ee9369fb719c0ba43ddf4d94638a970b84775f4788ac" }], stacks_operations: [PobBlockCommitment(PobBlockCommitmentData { signers: [], stacks_block_hash: "0x5b1681995f8e568287e0e4f5cbc1d6727dafb5e3a7822a77c69bd04208265aca", amount: 10000 })] } }], metadata: BitcoinBlockMetadata }
// 2022-10-26T03:06:21.929157Z  INFO chainhook_event_observer::indexer: BitcoinBlockData { block_identifier: BlockIdentifier { index: 105, hash: "0x0302c4c6063eb7199d3a565351bceeea9df4cb4aa09293194dbab277e46c2979" }, parent_block_identifier: BlockIdentifier { index: 104, hash: "0x210d0d095a75d88fc059cb97f453eee33b1833153fb1f81b9c3c031c26bb106b" }, timestamp: 1666753581, transactions: [BitcoinTransactionData { transaction_identifier: TransactionIdentifier { hash: "0xe7de433aa89c1f946f89133b0463b6cfebb26ad73b0771a79fd66c6acbfe3fb9" }, operations: [], metadata: BitcoinTransactionMetadata { inputs: [TxIn { previous_output: OutPoint { txid: "0000000000000000000000000000000000000000000000000000000000000000", vout: 4294967295 }, script_sig: "01690101", sequence: 4294967295, witness: [] }], outputs: [TxOut { value: 5000017600, script_pubkey: "76a914ee9369fb719c0ba43ddf4d94638a970b84775f4788ac" }, TxOut { value: 0, script_pubkey: "6a24aa21a9ed98ac3bc4e0c9ed53e3418a3bf3aa511dcd76088cf0e1c4fc71fb9755840d7a08" }], stacks_operations: [] } }, BitcoinTransactionData { transaction_identifier: TransactionIdentifier { hash: "0xe654501805d80d59ef0d95b57ad7a924f3be4a4dc0db5a785dfebe1f70c4e23e" }, operations: [], metadata: BitcoinTransactionMetadata { inputs: [TxIn { previous_output: OutPoint { txid: "59193c24cb2325cd2271b89f790f958dcd4065088680ffbc201a0ebb2f3cbf25", vout: 3 }, script_sig: "483045022100b59d2d07f68ea3a4f27a49979080a07b2432cfad9fc90e1edd0241496f0fd83f02205ac233f4cb68ada487f16339abedb7093948b683ba7d76b3b4058b2c0181a68901210239810ebf35e6f6c26062c99f3e183708d377720617c90a986859ec9c95d00be9", sequence: 4294967293, witness: [] }], outputs: [TxOut { value: 0, script_pubkey: "6a4c5069645b351bb015ef4f7dcdce4c9d95cbf157f85a3714626252cfc9078f3f1591ccdb13c3c7e22b34c4ffc2f6064a41df6fcd7f1b759d4f28b2f7cb6b27f283c868406e0000006800010000006600012c" }, TxOut { value: 10000, script_pubkey: "76a914000000000000000000000000000000000000000088ac" }, TxOut { value: 10000, script_pubkey: "76a914000000000000000000000000000000000000000088ac" }, TxOut { value: 4999867250, script_pubkey: "76a914ee9369fb719c0ba43ddf4d94638a970b84775f4788ac" }], stacks_operations: [PobBlockCommitment(PobBlockCommitmentData { signers: [], stacks_block_hash: "0x5b351bb015ef4f7dcdce4c9d95cbf157f85a3714626252cfc9078f3f1591ccdb", amount: 10000 })] } }], metadata: BitcoinBlockMetadata }
// 2022-10-26T03:07:53.298531Z  INFO chainhook_event_observer::indexer: BitcoinBlockData { block_identifier: BlockIdentifier { index: 106, hash: "0x52eb2aa15aa99afc4b918a552cef13e8b6eed84b257be097ad954b4f37a7e98d" }, parent_block_identifier: BlockIdentifier { index: 105, hash: "0x0302c4c6063eb7199d3a565351bceeea9df4cb4aa09293194dbab277e46c2979" }, timestamp: 1666753672, transactions: [BitcoinTransactionData { transaction_identifier: TransactionIdentifier { hash: "0xd28d7f5411416f94b95e9f999d5ee8ded5543ba9daae9f612b80f01c5107862d" }, operations: [], metadata: BitcoinTransactionMetadata { inputs: [TxIn { previous_output: OutPoint { txid: "0000000000000000000000000000000000000000000000000000000000000000", vout: 4294967295 }, script_sig: "016a0101", sequence: 4294967295, witness: [] }], outputs: [TxOut { value: 5000017500, script_pubkey: "76a914ee9369fb719c0ba43ddf4d94638a970b84775f4788ac" }, TxOut { value: 0, script_pubkey: "6a24aa21a9ed71aaf7e5384879a1b112bf623ac8b46dd88b39c3d2c6f8a1d264fc4463e6356a" }], stacks_operations: [] } }, BitcoinTransactionData { transaction_identifier: TransactionIdentifier { hash: "0x72e8e43afc4362cf921ccc57fde3e07b4cb6fac5f306525c86d38234c18e21d1" }, operations: [], metadata: BitcoinTransactionMetadata { inputs: [TxIn { previous_output: OutPoint { txid: "e654501805d80d59ef0d95b57ad7a924f3be4a4dc0db5a785dfebe1f70c4e23e", vout: 3 }, script_sig: "4730440220798bb7d7fb14df35610db2ef04e5d5b6588440b7c429bf650a96f8570904052b02204a817e13e7296a24a8f6cc8737bddb55d1835e513ec2b9dcb03424e4536ae34c01210239810ebf35e6f6c26062c99f3e183708d377720617c90a986859ec9c95d00be9", sequence: 4294967293, witness: [] }], outputs: [TxOut { value: 0, script_pubkey: "6a4c5069645b504d310fc27c86a6b65d0b0e0297db1e185d3432fdab9fa96a1053407ed07b537b8b7d23c6309dfd24340e85b75cff11ad685f8b310c1d2098748a0fffb146ec00000069000100000066000128" }, TxOut { value: 20000, script_pubkey: "76a914000000000000000000000000000000000000000088ac" }, TxOut { value: 4999829750, script_pubkey: "76a914ee9369fb719c0ba43ddf4d94638a970b84775f4788ac" }], stacks_operations: [PobBlockCommitment(PobBlockCommitmentData { signers: [], stacks_block_hash: "0x5b504d310fc27c86a6b65d0b0e0297db1e185d3432fdab9fa96a1053407ed07b", amount: 20000 })] } }], metadata: BitcoinBlockMetadata }
