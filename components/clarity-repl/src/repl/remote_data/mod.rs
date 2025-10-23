use clarity::types::chainstate::{
    BlockHeaderHash, BurnchainHeaderHash, ConsensusHash, SortitionId, StacksBlockId, VRFSeed,
};
use clarity::types::StacksEpochId;
use clarity::vm::errors::InterpreterResult;
use clarity_types::types::QualifiedContractIdentifier;
use serde::de::{DeserializeOwned, Error as SerdeError};
use serde::{Deserialize, Deserializer};

use crate::repl::settings::ApiUrl;

pub mod context;
pub mod fs;
mod http_request;
pub const MAINNET_20_START_HEIGHT: u32 = 1;
pub const MAINNET_2_05_START_HEIGHT: u32 = 40_607;
pub const MAINNET_21_START_HEIGHT: u32 = 99_113;
pub const MAINNET_22_START_HEIGHT: u32 = 103_900;
pub const MAINNET_23_START_HEIGHT: u32 = 104_359;
pub const MAINNET_24_START_HEIGHT: u32 = 107_055;
pub const MAINNET_25_START_HEIGHT: u32 = 147_290;
pub const MAINNET_30_START_HEIGHT: u32 = 171_833;
pub const MAINNET_31_START_HEIGHT: u32 = 340_555;
pub const MAINNET_32_START_HEIGHT: u32 = 2_401_415;
pub const MAINNET_33_START_HEIGHT: u32 = u32::MAX;

// the current primary testnet starts directly in epoch 2.5 (pox-4 deployment)
pub const TESTNET_20_START_HEIGHT: u32 = 1;
pub const TESTNET_2_05_START_HEIGHT: u32 = 1;
pub const TESTNET_21_START_HEIGHT: u32 = 1;
pub const TESTNET_22_START_HEIGHT: u32 = 1;
pub const TESTNET_23_START_HEIGHT: u32 = 1;
pub const TESTNET_24_START_HEIGHT: u32 = 1;
pub const TESTNET_25_START_HEIGHT: u32 = 1;
pub const TESTNET_30_START_HEIGHT: u32 = 320;
pub const TESTNET_31_START_HEIGHT: u32 = 814;
pub const TESTNET_32_START_HEIGHT: u32 = 3_140_887;
pub const TESTNET_33_START_HEIGHT: u32 = u32::MAX;

pub fn epoch_for_height(is_mainnet: bool, height: u32) -> StacksEpochId {
    if is_mainnet {
        epoch_for_mainnet_height(height)
    } else {
        epoch_for_testnet_height(height)
    }
}

fn epoch_for_mainnet_height(height: u32) -> StacksEpochId {
    if height < MAINNET_2_05_START_HEIGHT {
        StacksEpochId::Epoch20
    } else if height < MAINNET_21_START_HEIGHT {
        StacksEpochId::Epoch2_05
    } else if height < MAINNET_22_START_HEIGHT {
        StacksEpochId::Epoch21
    } else if height < MAINNET_23_START_HEIGHT {
        StacksEpochId::Epoch22
    } else if height < MAINNET_24_START_HEIGHT {
        StacksEpochId::Epoch23
    } else if height < MAINNET_25_START_HEIGHT {
        StacksEpochId::Epoch24
    } else if height < MAINNET_30_START_HEIGHT {
        StacksEpochId::Epoch25
    } else if height < MAINNET_31_START_HEIGHT {
        StacksEpochId::Epoch30
    } else if height < MAINNET_32_START_HEIGHT {
        StacksEpochId::Epoch31
    } else if height < MAINNET_33_START_HEIGHT {
        StacksEpochId::Epoch32
    } else {
        StacksEpochId::Epoch33
    }
}

fn epoch_for_testnet_height(height: u32) -> StacksEpochId {
    if height < TESTNET_2_05_START_HEIGHT {
        StacksEpochId::Epoch20
    } else if height < TESTNET_21_START_HEIGHT {
        StacksEpochId::Epoch2_05
    } else if height < TESTNET_22_START_HEIGHT {
        StacksEpochId::Epoch21
    } else if height < TESTNET_23_START_HEIGHT {
        StacksEpochId::Epoch22
    } else if height < TESTNET_24_START_HEIGHT {
        StacksEpochId::Epoch23
    } else if height < TESTNET_25_START_HEIGHT {
        StacksEpochId::Epoch24
    } else if height < TESTNET_30_START_HEIGHT {
        StacksEpochId::Epoch25
    } else if height < TESTNET_31_START_HEIGHT {
        StacksEpochId::Epoch30
    } else if height < TESTNET_32_START_HEIGHT {
        StacksEpochId::Epoch31
    } else if height < TESTNET_33_START_HEIGHT {
        StacksEpochId::Epoch32
    } else {
        StacksEpochId::Epoch33
    }
}

#[derive(Deserialize)]
pub struct ClarityDataResponse {
    pub data: String,
}

#[derive(Deserialize)]
pub struct ContractSource {
    pub source: String,
    pub publish_height: u32,
}

#[derive(Clone, Debug, Deserialize)]
pub struct Info {
    pub network_id: u32,
    pub stacks_tip_height: u32,
}

fn deserialize_burnchain_header_hash<'de, D>(
    deserializer: D,
) -> Result<BurnchainHeaderHash, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = String::deserialize(deserializer)?;
    BurnchainHeaderHash::from_hex(s.trim_start_matches("0x")).map_err(SerdeError::custom)
}

fn deserialize_consensus_hash<'de, D>(deserializer: D) -> Result<ConsensusHash, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = String::deserialize(deserializer)?;
    ConsensusHash::from_hex(s.trim_start_matches("0x")).map_err(SerdeError::custom)
}

fn deserialize_sortition_id<'de, D>(deserializer: D) -> Result<SortitionId, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = String::deserialize(deserializer)?;
    SortitionId::from_hex(s.trim_start_matches("0x")).map_err(SerdeError::custom)
}

fn deserialize_stacks_block_id<'de, D>(deserializer: D) -> Result<StacksBlockId, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = String::deserialize(deserializer)?;
    StacksBlockId::from_hex(s.trim_start_matches("0x")).map_err(SerdeError::custom)
}

fn deserialize_block_header_hash<'de, D>(deserializer: D) -> Result<BlockHeaderHash, D::Error>
where
    D: Deserializer<'de>,
{
    let s: String = String::deserialize(deserializer)?;
    BlockHeaderHash::from_hex(s.trim_start_matches("0x")).map_err(SerdeError::custom)
}

fn deserialize_vrf_seed<'de, D>(deserializer: D) -> Result<Option<VRFSeed>, D::Error>
where
    D: Deserializer<'de>,
{
    let s: Option<String> = Option::deserialize(deserializer)?;
    match s {
        Some(s) => VRFSeed::from_hex(s.trim_start_matches("0x"))
            .map_err(SerdeError::custom)
            .map(Some),
        None => Ok(None),
    }
}

#[derive(Clone, Debug, Deserialize)]
pub struct Sortition {
    #[serde(deserialize_with = "deserialize_burnchain_header_hash")]
    pub burn_block_hash: BurnchainHeaderHash,
    pub burn_block_height: u32,
    #[serde(deserialize_with = "deserialize_consensus_hash")]
    pub consensus_hash: ConsensusHash,
    #[serde(deserialize_with = "deserialize_sortition_id")]
    pub sortition_id: SortitionId,
    #[serde(deserialize_with = "deserialize_sortition_id")]
    pub parent_sortition_id: SortitionId,
    // @todo: remove serde(default) and Option<> when stacks-network#5772 is merged
    #[serde(default, deserialize_with = "deserialize_vrf_seed")]
    pub vrf_seed: Option<VRFSeed>,
}

#[derive(Clone, Debug, Deserialize)]
#[allow(dead_code)]
pub struct Block {
    pub height: u32,
    pub burn_block_height: u32,
    pub tenure_height: u32,
    pub block_time: u64,
    pub burn_block_time: u64,
    #[serde(deserialize_with = "deserialize_block_header_hash")]
    pub hash: BlockHeaderHash,
    #[serde(deserialize_with = "deserialize_stacks_block_id")]
    pub index_block_hash: StacksBlockId,
    #[serde(deserialize_with = "deserialize_burnchain_header_hash")]
    pub burn_block_hash: BurnchainHeaderHash,
}

#[derive(Clone, Debug)]
pub struct HttpClient {
    url: ApiUrl,
}

impl HttpClient {
    pub fn new(url: ApiUrl) -> Self {
        HttpClient { url }
    }

    fn get<T: DeserializeOwned>(&self, path: &str) -> Result<T, String> {
        let url = format!("{}{}", self.url, path);
        http_request::http_request(url.as_str())
    }

    pub fn fetch_info(&self) -> Info {
        self.get::<Info>("/v2/info").unwrap()
    }

    pub fn fetch_sortition(&self, height: u32) -> Sortition {
        let url = format!("/v3/sortitions/burn_height/{height}");
        let sortitions = self.get::<Vec<Sortition>>(&url).unwrap();
        sortitions.into_iter().next().unwrap()
    }

    pub fn fetch_block(&self, url: &str) -> Block {
        self.get::<Block>(url).unwrap()
    }

    pub fn fetch_clarity_data(&self, path: &str) -> InterpreterResult<Option<String>> {
        match self.get::<ClarityDataResponse>(path) {
            Ok(data) => Ok(Some(data.data.trim_start_matches("0x").to_string())),
            Err(_) => Ok(None),
        }
    }

    pub fn fetch_contract(
        &self,
        contract: &QualifiedContractIdentifier,
    ) -> Result<ContractSource, String> {
        self.get::<ContractSource>(&format!(
            "/v2/contracts/source/{}/{}?proof=false",
            contract.issuer, contract.name
        ))
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_http_client_fetch_info() {
        let mut server = mockito::Server::new();
        let _ = server
            .mock("GET", "/v2/info")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{
                "peer_version": 402653196,
                "pox_consensus": "0ce291b675bb0148b435a884e250aafc3fd6bc86",
                "burn_block_height": 882262,
                "stable_pox_consensus": "f517f5aced5be836f9fe10980ff06108a6a2acec",
                "stable_burn_block_height": 882255,
                "network_id": 1,
                "parent_network_id": 3652501241,
                "stacks_tip_height": 556946,
                "stacks_tip": "70526983b920b31d5e0d65750033a4dc2f328f31a3ffeb1f8780bfb164d50502",
                "stacks_tip_consensus_hash": "0ce291b675bb0148b435a884e250aafc3fd6bc86",
                "genesis_chainstate_hash": "74237aa39aa50a83de11a4f53e9d3bb7d43461d1de9873f402e5453ae60bc59b",
                "unanchored_tip": null,
                "unanchored_seq": null,
                "tenure_height": 184037,
                "is_fully_synced": true,
                "node_public_key": "02e0ce39375d699d164f90cc815427943c5acccca02069e394f9ed28d2c2bca317",
                "node_public_key_hash": "d5b1f3c7f9b2ffa8ac610170d1352550d240197c",
                "stackerdbs": []
            }"#)
            .create();

        let client = HttpClient::new(ApiUrl(server.url()));
        let info = client.fetch_info();
        assert_eq!(info.network_id, 1);
        assert_eq!(info.stacks_tip_height, 556946);
    }

    #[test]
    fn test_http_client_fetch_sortition() {
        let mut server = mockito::Server::new();

        let _ = server
            .mock("GET", "/v3/sortitions/burn_height/882262")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                r#"[{
                    "burn_block_hash": "0x000000000000000000012f34a6727bf7dc9ceae203022cb14a3b37fe8de0e6ad",
                    "burn_block_height": 882262,
                    "burn_header_timestamp": 1738666756,
                    "sortition_id": "0x6e79b604db6d97b9289f04f446e78ec871a6b16972b02674bc3ea2bdec200fb9",
                    "parent_sortition_id": "0x8b2dedebf5b8c72c1e8ede00abd1f417d755ac7f513dbf3c3d007494404115d3",
                    "consensus_hash": "0x0ce291b675bb0148b435a884e250aafc3fd6bc86",
                    "was_sortition": true,
                    "miner_pk_hash160": "0x37e79a837b4071a1fc6c1b49208e7d2141a25905",
                    "stacks_parent_ch": "0xcd18600459e4da24ede6662cc4df6bcece61b5f9",
                    "last_sortition_ch": "0xcd18600459e4da24ede6662cc4df6bcece61b5f9",
                    "committed_block_hash": "0xbf7e26ee22b18461dfed70cc114372a0f8a61249de2f20b120e6fe63da5a45e4"
                }]"#,
            )
            .create();

        let client = HttpClient::new(ApiUrl(server.url()));
        let sortition = client.fetch_sortition(882262);

        assert_eq!(sortition.burn_block_height, 882262);
        assert_eq!(
            sortition.consensus_hash,
            ConsensusHash::from_hex("0ce291b675bb0148b435a884e250aafc3fd6bc86").unwrap()
        );
        assert_eq!(
            sortition.sortition_id,
            SortitionId::from_hex(
                "6e79b604db6d97b9289f04f446e78ec871a6b16972b02674bc3ea2bdec200fb9"
            )
            .unwrap()
        );
        assert_eq!(
            sortition.parent_sortition_id,
            SortitionId::from_hex(
                "8b2dedebf5b8c72c1e8ede00abd1f417d755ac7f513dbf3c3d007494404115d3"
            )
            .unwrap()
        );
    }

    #[test]
    fn test_http_client_fetch_block() {
        let mut server = mockito::Server::new();
        let _ = server
            .mock("GET", "/extended/v2/blocks/556946")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                r#"{
                    "canonical": true,
                    "height": 556946,
                    "hash": "0x70526983b920b31d5e0d65750033a4dc2f328f31a3ffeb1f8780bfb164d50502",
                    "block_time": 1738667305,
                    "block_time_iso": "2025-02-04T11:08:25.000Z",
                    "tenure_height": 184037,
                    "index_block_hash": "0xa246be7256de49aa6923074a53507a839b2ba356f8809f8e7448c87b5c1891e9",
                    "parent_block_hash": "0x06dd38d5315c133b08cefdedb5c51f2e91fd8a0474e07b3d1a740c19bc21842e",
                    "parent_index_block_hash": "0x1d39f5eb45aa0e78cc256ea6ed180dcb9e8c87bea11ecf8102ef9bced5f3f73b",
                    "burn_block_time": 1738666756,
                    "burn_block_time_iso": "2025-02-04T10:59:16.000Z",
                    "burn_block_hash": "0x000000000000000000012f34a6727bf7dc9ceae203022cb14a3b37fe8de0e6ad",
                    "burn_block_height": 882262,
                    "miner_txid": "0x2ca4c7f6d36f32f3c2f1c5ebae3816690a9ba2258c38ef6a4494d315873a0448",
                    "tx_count": 1,
                    "execution_cost_read_count": 0,
                    "execution_cost_read_length": 0,
                    "execution_cost_runtime": 0,
                    "execution_cost_write_count": 0,
                    "execution_cost_write_length": 0
                }"#,
            )
            .create();

        let client = HttpClient::new(ApiUrl(server.url()));
        let block = client.fetch_block("/extended/v2/blocks/556946");
        assert_eq!(block.height, 556946);
        assert_eq!(block.burn_block_height, 882262);
        assert_eq!(block.tenure_height, 184037);
        assert_eq!(block.block_time, 1738667305);
        assert_eq!(block.burn_block_time, 1738666756);
        assert_eq!(
            block.hash,
            BlockHeaderHash::from_hex(
                "70526983b920b31d5e0d65750033a4dc2f328f31a3ffeb1f8780bfb164d50502"
            )
            .unwrap()
        );
        assert_eq!(
            block.index_block_hash,
            StacksBlockId::from_hex(
                "a246be7256de49aa6923074a53507a839b2ba356f8809f8e7448c87b5c1891e9"
            )
            .unwrap()
        );
        assert_eq!(
            block.burn_block_hash,
            BurnchainHeaderHash::from_hex(
                "000000000000000000012f34a6727bf7dc9ceae203022cb14a3b37fe8de0e6ad"
            )
            .unwrap()
        );
    }

    #[test]
    fn it_crashes_if_rate_limit_is_reached() {
        let mut server = mockito::Server::new();

        let _rate_limit_mock = server
            .mock("GET", "/v2/info")
            .with_status(429)
            .with_header("content-type", "application/json")
            .with_header("ratelimit-remaining", "0")
            .with_header("retry-after", "1")
            .with_body(r#"{"error":"Rate limit exceeded","message":"Too many requests"}"#)
            .create();

        let client = HttpClient::new(ApiUrl(server.url()));
        let info_result = std::panic::catch_unwind(|| client.fetch_info());
        assert!(
            info_result.is_err(),
            "Expected session creation to succeed after rate limit retries"
        );
    }

    #[test]
    fn it_retries_when_reaching_rate_limit() {
        let mut server = mockito::Server::new();

        let _rate_limit_mock = server
            .mock("GET", "/v2/info")
            .with_status(429)
            .with_header("content-type", "application/json")
            .with_header("ratelimit-remaining", "0")
            .with_header("retry-after", "1")
            .with_body(r#"{"error":"Rate limit exceeded","message":"Too many requests"}"#)
            .expect(2)
            .create();

        // The third call returns a 200
        let _ = server
            .mock("GET", "/v2/info")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{
                "peer_version": 402653196,
                "pox_consensus": "0ce291b675bb0148b435a884e250aafc3fd6bc86",
                "burn_block_height": 882262,
                "stable_pox_consensus": "f517f5aced5be836f9fe10980ff06108a6a2acec",
                "stable_burn_block_height": 882255,
                "network_id": 1,
                "parent_network_id": 3652501241,
                "stacks_tip_height": 556946,
                "stacks_tip": "70526983b920b31d5e0d65750033a4dc2f328f31a3ffeb1f8780bfb164d50502",
                "stacks_tip_consensus_hash": "0ce291b675bb0148b435a884e250aafc3fd6bc86",
                "genesis_chainstate_hash": "74237aa39aa50a83de11a4f53e9d3bb7d43461d1de9873f402e5453ae60bc59b",
                "unanchored_tip": null,
                "unanchored_seq": null,
                "tenure_height": 184037,
                "is_fully_synced": true,
                "node_public_key": "02e0ce39375d699d164f90cc815427943c5acccca02069e394f9ed28d2c2bca317",
                "node_public_key_hash": "d5b1f3c7f9b2ffa8ac610170d1352550d240197c",
                "stackerdbs": []
            }"#)
            .create();

        let client = HttpClient::new(ApiUrl(server.url()));
        let info = client.fetch_info();
        assert_eq!(info.network_id, 1);
        assert_eq!(info.stacks_tip_height, 556946);
    }

    // we should better handle network errors. tracked in #1646
    #[test]
    #[should_panic]
    fn test_http_client_error() {
        let mut server = mockito::Server::new();
        let _ = server
            .mock("GET", "/extended/v2/blocks/9999999999999")
            .with_status(404)
            .with_header("content-type", "application/json")
            .with_body(r#"{"statusCode":404,"error":"Not Found","message":"Block not found"}"#)
            .create();

        let client = HttpClient::new(ApiUrl(server.url()));

        let _block = client.fetch_block("/extended/v2/blocks/9999999999999");
    }
}
