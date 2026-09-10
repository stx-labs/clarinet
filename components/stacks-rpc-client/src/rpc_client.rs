use std::io::Cursor;

use clarity::codec::StacksMessageCodec;
use clarity::util::hash::{bytes_to_hex, hex_bytes, to_hex};
use clarity::vm::types::Value;
use reqwest::blocking::Client;
use serde::de::DeserializeOwned;
use serde::Deserialize;
#[cfg(any(test, feature = "mock"))]
use serde::Serialize;
use serde_json::json;
use stacks_codec::transaction::{StacksTransaction, TransactionPayload};

#[derive(Clone, Debug)]
pub enum RpcError {
    Generic,
    StatusCode(u16),
    Message(String),
}

impl From<reqwest::Error> for RpcError {
    fn from(e: reqwest::Error) -> Self {
        RpcError::Message(e.to_string())
    }
}

impl std::fmt::Display for RpcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            RpcError::Message(e) => write!(f, "{e}"),
            RpcError::StatusCode(e) => write!(f, "error status code {e}"),
            RpcError::Generic => write!(f, "unknown error"),
        }
    }
}

pub struct StacksRpc {
    pub url: String,
    pub client: Client,
}

pub struct PostTransactionResult {
    pub txid: String,
}

pub struct CallReadOnlyFnResult {
    pub result: Value,
}

#[cfg_attr(feature = "mock", derive(Serialize))]
#[derive(Deserialize, Debug, Clone, Default)]
pub struct NodeInfo {
    pub peer_version: u64,
    pub pox_consensus: String,
    pub burn_block_height: u64,
    pub stable_pox_consensus: String,
    pub stable_burn_block_height: u64,
    pub server_version: String,
    pub network_id: u32,
    pub parent_network_id: u32,
    pub stacks_tip_height: u64,
    pub stacks_tip: String,
    pub stacks_tip_consensus_hash: String,
    pub genesis_chainstate_hash: String,
}

#[cfg_attr(feature = "mock", derive(Serialize))]
#[derive(Deserialize, Debug, Clone)]
pub struct PoxInfo {
    pub contract_id: String,
    pub pox_activation_threshold_ustx: u64,
    pub first_burnchain_block_height: u32,
    pub current_burnchain_block_height: u32,
    pub prepare_phase_block_length: u32,
    pub reward_phase_block_length: u32,
    pub reward_slots: u32,
    pub reward_cycle_id: u32,
    pub reward_cycle_length: u32,
    pub total_liquid_supply_ustx: u64,
    pub current_cycle: CurrentPoxCycle,
    pub next_cycle: NextPoxCycle,
}

#[cfg_attr(feature = "mock", derive(Serialize))]
#[derive(Deserialize, Debug, Clone, Default)]
pub struct CurrentPoxCycle {
    pub id: u64,
    pub min_threshold_ustx: u64,
    pub stacked_ustx: u64,
    pub is_pox_active: bool,
}

#[cfg_attr(feature = "mock", derive(Serialize))]
#[derive(Deserialize, Debug, Clone, Default)]
pub struct NextPoxCycle {
    pub min_threshold_ustx: u64,
    pub stacked_ustx: u64,
    pub blocks_until_prepare_phase: i16,
    pub blocks_until_reward_phase: i16,
}

#[derive(Deserialize, Debug)]
pub struct Balance {
    pub balance: String,
    pub nonce: u64,
    pub balance_proof: String,
    pub nonce_proof: String,
}

#[derive(Deserialize, Debug, Clone)]
pub struct Contract {
    pub source: String,
    pub publish_height: u64,
}

#[derive(Deserialize, Debug)]
pub struct FeeEstimationReport {
    pub estimations: Vec<FeeEstimation>,
}

#[derive(Deserialize, Debug)]
pub struct FeeEstimation {
    pub fee: u64,
}

#[derive(Deserialize, Debug, Clone)]
pub struct BurnBlock {
    pub burn_block_time: u64,
    pub burn_block_hash: String,
    pub burn_block_height: u32,
}

impl StacksRpc {
    pub fn new(url: &str) -> Self {
        Self {
            url: url.into(),
            client: Client::builder().build().unwrap(),
        }
    }

    pub fn estimate_transaction_fee(
        &self,
        transaction_payload: &TransactionPayload,
        priority: usize,
    ) -> Result<u64, RpcError> {
        let tx = transaction_payload.serialize_to_vec();
        let payload = json!({ "transaction_payload": to_hex(&tx) });
        let path = format!("{}/v2/fees/transaction", self.url);
        let res: FeeEstimationReport = self.client.post(path).json(&payload).send()?.json()?;

        Ok(res.estimations[priority].fee)
    }

    pub fn post_transaction(
        &self,
        transaction: &StacksTransaction,
    ) -> Result<PostTransactionResult, RpcError> {
        let tx = transaction.serialize_to_vec();
        let path = format!("{}/v2/transactions", self.url);
        let res = self
            .client
            .post(path)
            .header("Content-Type", "application/octet-stream")
            .body(tx)
            .send()?;

        if !res.status().is_success() {
            let err = match res.text() {
                Ok(message) => RpcError::Message(message),
                Err(e) => RpcError::Message(e.to_string()),
            };
            return Err(err);
        }

        let txid: String = res.json().unwrap();
        let res = PostTransactionResult { txid };
        Ok(res)
    }

    fn get_json<T: DeserializeOwned>(&self, request_url: String) -> Result<T, RpcError> {
        let response = self.client.get(request_url).send()?;

        if !response.status().is_success() {
            return Err(RpcError::StatusCode(response.status().as_u16()));
        }

        response
            .json()
            .map_err(|e| RpcError::Message(e.to_string()))
    }

    pub fn get_nonce(&self, address: &str) -> Result<u64, RpcError> {
        let balance: Balance = self.get_json(format!("{}/v2/accounts/{address}", self.url))?;
        Ok(balance.nonce)
    }

    pub fn get_pox_info(&self) -> Result<PoxInfo, RpcError> {
        self.get_json(format!("{}/v2/pox", self.url))
    }

    pub fn get_info(&self) -> Result<NodeInfo, RpcError> {
        self.get_json(format!("{}/v2/info", self.url))
    }

    /// `None` when the node has no contract at that identifier.
    pub fn get_contract_source(
        &self,
        principal: &str,
        contract_name: &str,
    ) -> Result<Option<Contract>, RpcError> {
        let request_url = format!(
            "{}/v2/contracts/source/{}/{}",
            self.url, principal, contract_name
        );

        match self.get_json(request_url) {
            Ok(contract) => Ok(Some(contract)),
            Err(RpcError::StatusCode(404)) => Ok(None),
            Err(e) => Err(e),
        }
    }

    pub fn call_read_only_fn(
        &self,
        contract_addr: &str,
        contract_name: &str,
        method: &str,
        args: Vec<Value>,
        sender: &str,
    ) -> Result<Value, RpcError> {
        let path = format!(
            "{}/v2/contracts/call-read/{}/{}/{}",
            self.url, contract_addr, contract_name, method
        );

        let arguments = args
            .iter()
            .map(|a| bytes_to_hex(&a.serialize_to_vec().expect("failed to serialize value")))
            .collect::<Vec<_>>();
        let res = self
            .client
            .post(path)
            .json(&json!({
                "sender": sender,
                "arguments": arguments,
            }))
            .send()?;

        if !res.status().is_success() {
            let error = match res.text() {
                Ok(message) => RpcError::Message(message),
                _ => RpcError::Generic,
            };
            return Err(error);
        }

        #[derive(Deserialize, Debug)]
        struct ReadOnlyCallResult {
            okay: bool,
            result: String,
        }

        let response: ReadOnlyCallResult = res.json()?;
        if response.okay {
            let raw_value = response
                .result
                .strip_prefix("0x")
                .ok_or_else(|| RpcError::Message("result is not 0x-prefixed".into()))?;
            let bytes = hex_bytes(raw_value).map_err(|e| RpcError::Message(e.to_string()))?;
            let value = Value::consensus_deserialize(&mut Cursor::new(&bytes))
                .map_err(|e| RpcError::Message(e.to_string()))?;
            Ok(value)
        } else {
            Err(RpcError::Generic)
        }
    }

    pub fn get_burn_block(&self, height: u32) -> Result<BurnBlock, RpcError> {
        self.get_json(format!("{}/extended/v2/burn-blocks/{height}", self.url))
    }

    pub fn call_with_retry<T, F>(&self, mut func: F, retries: usize) -> Result<T, RpcError>
    where
        F: FnMut(&Self) -> Result<T, RpcError>,
    {
        for attempt in 0..=retries {
            match func(self) {
                Ok(result) => return Ok(result),
                Err(_err) if attempt < retries => {
                    std::thread::sleep(std::time::Duration::from_secs(1));
                }
                Err(err) => return Err(err),
            }
        }
        Err(RpcError::Generic)
    }
}
