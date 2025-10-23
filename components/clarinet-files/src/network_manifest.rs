use std::collections::BTreeMap;
use std::sync::LazyLock;

use clarinet_utils::{get_bip32_keys_from_mnemonic, mnemonic_from_phrase, random_mnemonic};
use clarity::types::chainstate::{StacksAddress, StacksPrivateKey};
use clarity::util::hash::bytes_to_hex;
use clarity::util::secp256k1::Secp256k1PublicKey;
use libsecp256k1::PublicKey;
use serde::Serialize;
use toml::value::Value;

use super::{FileAccessor, FileLocation};

pub const DEFAULT_DERIVATION_PATH: &str = "m/44'/5757'/0'/0/0";

pub const DEFAULT_STACKS_NODE_IMAGE: &str = "blockstack/stacks-blockchain:3.2.0.0.2-alpine";
pub const DEFAULT_STACKS_SIGNER_IMAGE: &str = "blockstack/stacks-signer:3.2.0.0.2.0-alpine";
pub const DEFAULT_STACKS_API_IMAGE: &str = "hirosystems/stacks-blockchain-api:latest";

pub const DEFAULT_POSTGRES_IMAGE: &str = "postgres:alpine";

pub const DEFAULT_BITCOIN_NODE_IMAGE: &str = "lncm/bitcoind:v27.2";
pub const DEFAULT_BITCOIN_EXPLORER_IMAGE: &str = "quay.io/hirosystems/bitcoin-explorer:devnet";

pub const DEFAULT_STACKS_EXPLORER_IMAGE: &str = "hirosystems/explorer:latest";

pub const DEFAULT_STACKS_MINER_MNEMONIC: &str = "fragile loan twenty basic net assault jazz absorb diet talk art shock innocent float punch travel gadget embrace caught blossom hockey surround initial reduce";
pub const DEFAULT_FAUCET_MNEMONIC: &str = "shadow private easily thought say logic fault paddle word top book during ignore notable orange flight clock image wealth health outside kitten belt reform";
pub const DEFAULT_STACKER_MNEMONIC: &str = "empty lens any direct brother then drop fury rule pole win claim scissors list rescue horn rent inform relief jump sword weekend half legend";
#[cfg(unix)]
pub const DEFAULT_DOCKER_SOCKET: &str = "unix:///var/run/docker.sock";
#[cfg(windows)]
pub const DEFAULT_DOCKER_SOCKET: &str = "npipe:////./pipe/docker_engine";
#[cfg(target_arch = "wasm32")]
pub const DEFAULT_DOCKER_SOCKET: &str = "/var/run/docker.sock";
pub const DEFAULT_DOCKER_PLATFORM: &str = "linux/amd64";

pub const DEFAULT_EPOCH_2_0: u64 = 100;
pub const DEFAULT_EPOCH_2_05: u64 = 100;
pub const DEFAULT_EPOCH_2_1: u64 = 101;
pub const DEFAULT_EPOCH_2_2: u64 = 102;
pub const DEFAULT_EPOCH_2_3: u64 = 103;
pub const DEFAULT_EPOCH_2_4: u64 = 104;
pub const DEFAULT_EPOCH_2_5: u64 = 108;
pub const DEFAULT_EPOCH_3_0: u64 = 142;
pub const DEFAULT_EPOCH_3_1: u64 = 144;
pub const DEFAULT_EPOCH_3_2: u64 = 146;
// pub const DEFAULT_EPOCH_3_3: u64 = 100_000_000;

// Currently, the pox-4 contract has these values hardcoded:
// https://github.com/stacks-network/stacks-core/blob/e09ab931e2f15ff70f3bb5c2f4d7afb[…]42bd7bec6/stackslib/src/chainstate/stacks/boot/pox-testnet.clar
// but they may be configurable in the future.
pub const DEFAULT_POX_PREPARE_LENGTH: u64 = 4;
pub const DEFAULT_POX_REWARD_LENGTH: u64 = 10;
pub const DEFAULT_FIRST_BURN_HEADER_HEIGHT: u64 = 100;

pub static DEFAULT_PRIVATE_KEYS: LazyLock<[StacksPrivateKey; 1]> = LazyLock::new(|| {
    [StacksPrivateKey::from_hex(
        "7287ba251d44a4d3fd9276c88ce34c5c52a038955511cccaf77e61068649c17801",
    )
    .unwrap()]
});

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StacksNetwork {
    Simnet,
    Devnet,
    Testnet,
    Mainnet,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BitcoinNetwork {
    Regtest,
    Testnet,
    Signet,
    Mainnet,
}

impl StacksNetwork {
    pub fn get_networks(&self) -> (BitcoinNetwork, StacksNetwork) {
        match &self {
            StacksNetwork::Simnet => (BitcoinNetwork::Regtest, StacksNetwork::Simnet),
            StacksNetwork::Devnet => (BitcoinNetwork::Testnet, StacksNetwork::Devnet),
            StacksNetwork::Testnet => (BitcoinNetwork::Testnet, StacksNetwork::Testnet),
            StacksNetwork::Mainnet => (BitcoinNetwork::Mainnet, StacksNetwork::Mainnet),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct NetworkManifestFile {
    network: NetworkConfigFile,
    accounts: Option<Value>,
    devnet: Option<DevnetConfigFile>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct NetworkConfigFile {
    name: String,
    node_rpc_address: Option<String>,
    stacks_node_rpc_address: Option<String>,
    bitcoin_node_rpc_address: Option<String>,
    deployment_fee_rate: Option<u64>,
    sats_per_bytes: Option<u64>,
}

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct DevnetConfigFile {
    pub name: Option<String>,
    pub network_id: Option<u16>,
    pub orchestrator_port: Option<u16>,
    pub orchestrator_control_port: Option<u16>,
    pub bitcoin_node_p2p_port: Option<u16>,
    pub bitcoin_node_rpc_port: Option<u16>,
    pub stacks_node_p2p_port: Option<u16>,
    pub stacks_node_rpc_port: Option<u16>,
    pub stacks_node_events_observers: Option<Vec<String>>,
    pub stacks_node_wait_time_for_microblocks: Option<u32>,
    pub stacks_node_first_attempt_time_ms: Option<u32>,
    pub stacks_node_env_vars: Option<Vec<String>>,
    pub stacks_node_next_initiative_delay: Option<u16>,
    pub stacks_signers_keys: Option<Vec<String>>,
    pub stacks_signers_env_vars: Option<Vec<String>>,
    pub stacks_api_env_vars: Option<Vec<String>>,
    pub stacks_explorer_env_vars: Option<Vec<String>>,
    pub stacks_api_port: Option<u16>,
    pub stacks_api_events_port: Option<u16>,
    pub bitcoin_explorer_port: Option<u16>,
    pub stacks_explorer_port: Option<u16>,
    pub bitcoin_node_username: Option<String>,
    pub bitcoin_node_password: Option<String>,
    pub miner_mnemonic: Option<String>,
    pub miner_derivation_path: Option<String>,
    pub miner_coinbase_recipient: Option<String>,
    pub miner_wallet_name: Option<String>,
    pub faucet_mnemonic: Option<String>,
    pub faucet_derivation_path: Option<String>,
    pub stacker_mnemonic: Option<String>,
    pub stacker_derivation_path: Option<String>,
    pub bitcoin_controller_block_time: Option<u32>,
    pub bitcoin_controller_automining_disabled: Option<bool>,
    pub pre_nakamoto_mock_signing: Option<bool>,
    pub working_dir: Option<String>,
    pub postgres_port: Option<u16>,
    pub postgres_username: Option<String>,
    pub postgres_password: Option<String>,
    pub stacks_api_postgres_database: Option<String>,
    pub pox_stacking_orders: Option<Vec<PoxStackingOrder>>,
    pub execute_script: Option<Vec<ExecuteScript>>,
    pub bitcoin_node_image_url: Option<String>,
    pub bitcoin_explorer_image_url: Option<String>,
    pub stacks_node_image_url: Option<String>,
    pub stacks_signer_image_url: Option<String>,
    pub stacks_api_image_url: Option<String>,
    pub stacks_explorer_image_url: Option<String>,
    pub postgres_image_url: Option<String>,
    pub disable_bitcoin_explorer: Option<bool>,
    pub disable_stacks_explorer: Option<bool>,
    pub disable_stacks_api: Option<bool>,
    pub disable_postgres: Option<bool>,
    pub bind_containers_volumes: Option<bool>,
    pub docker_host: Option<String>,
    pub components_host: Option<String>,
    pub epoch_2_0: Option<u64>,
    pub epoch_2_05: Option<u64>,
    pub epoch_2_1: Option<u64>,
    pub epoch_2_2: Option<u64>,
    pub epoch_2_3: Option<u64>,
    pub epoch_2_4: Option<u64>,
    pub epoch_2_5: Option<u64>,
    pub epoch_3_0: Option<u64>,
    pub epoch_3_1: Option<u64>,
    pub epoch_3_2: Option<u64>,
    pub epoch_3_3: Option<u64>,
    pub use_docker_gateway_routing: Option<bool>,
    pub docker_platform: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ExecuteScript {
    pub script: String,
    pub allow_wallets: bool,
    pub allow_write: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct NetworkManifest {
    pub network: NetworkConfig,
    #[serde(with = "accounts_serde")]
    pub accounts: BTreeMap<String, AccountConfig>,
    #[serde(rename = "devnet_settings")]
    pub devnet: Option<DevnetConfig>,
}

pub mod accounts_serde {
    use std::collections::BTreeMap;

    use serde::ser::SerializeSeq;
    use serde::{Deserializer, Serializer};

    use crate::AccountConfig;

    pub fn serialize<S>(
        target: &BTreeMap<String, AccountConfig>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(target.len()))?;
        for account in target.values() {
            seq.serialize_element(account)?;
        }
        seq.end()
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<BTreeMap<String, AccountConfig>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let mut res: BTreeMap<String, AccountConfig> = BTreeMap::new();
        let container: Vec<AccountConfig> = serde::Deserialize::deserialize(deserializer)?;
        for account in container {
            res.insert(account.label.clone(), account);
        }
        Ok(res)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct NetworkConfig {
    name: String,
    pub stacks_node_rpc_address: Option<String>,
    pub bitcoin_node_rpc_address: Option<String>,
    pub deployment_fee_rate: u64,
    pub sats_per_bytes: u64,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct DevnetConfig {
    pub name: String,
    pub network_id: Option<u16>,
    pub orchestrator_ingestion_port: u16,
    pub orchestrator_control_port: u16,
    pub bitcoin_node_p2p_port: u16,
    pub bitcoin_node_rpc_port: u16,
    pub bitcoin_node_username: String,
    pub bitcoin_node_password: String,
    pub stacks_node_p2p_port: u16,
    pub stacks_node_rpc_port: u16,
    pub stacks_node_wait_time_for_microblocks: u32,
    pub stacks_node_first_attempt_time_ms: u32,
    pub stacks_node_events_observers: Vec<String>,
    pub stacks_node_env_vars: Vec<String>,
    pub stacks_node_next_initiative_delay: u16,
    pub stacks_api_port: u16,
    pub stacks_api_events_port: u16,
    pub stacks_api_env_vars: Vec<String>,
    pub stacks_signers_keys: Vec<StacksPrivateKey>,
    pub stacks_signers_env_vars: Vec<String>,
    pub stacks_explorer_port: u16,
    pub stacks_explorer_env_vars: Vec<String>,
    pub bitcoin_explorer_port: u16,
    pub bitcoin_controller_block_time: u32,
    pub bitcoin_controller_automining_disabled: bool,
    pub miner_stx_address: String,
    pub miner_secret_key_hex: String,
    pub miner_btc_address: String,
    pub miner_mnemonic: String,
    pub miner_derivation_path: String,
    pub miner_coinbase_recipient: String,
    pub miner_wallet_name: String,
    pub faucet_stx_address: String,
    pub faucet_secret_key_hex: String,
    pub faucet_btc_address: String,
    pub faucet_mnemonic: String,
    pub faucet_derivation_path: String,
    pub stacker_mnemonic: String,
    pub stacker_derivation_path: String,
    pub pre_nakamoto_mock_signing: bool,
    pub working_dir: String,
    pub postgres_port: u16,
    pub postgres_username: String,
    pub postgres_password: String,
    pub stacks_api_postgres_database: String,
    pub pox_stacking_orders: Vec<PoxStackingOrder>,
    pub execute_script: Vec<ExecuteScript>,
    pub bitcoin_node_image_url: String,
    pub stacks_node_image_url: String,
    pub stacks_signer_image_url: String,
    pub stacks_api_image_url: String,
    pub stacks_explorer_image_url: String,
    pub postgres_image_url: String,
    pub bitcoin_explorer_image_url: String,
    pub disable_bitcoin_explorer: bool,
    pub disable_stacks_explorer: bool,
    pub disable_stacks_api: bool,
    pub disable_postgres: bool,
    pub bind_containers_volumes: bool,
    pub docker_host: String,
    pub components_host: String,
    pub epoch_2_0: u64,
    pub epoch_2_05: u64,
    pub epoch_2_1: u64,
    pub epoch_2_2: u64,
    pub epoch_2_3: u64,
    pub epoch_2_4: u64,
    pub epoch_2_5: u64,
    pub epoch_3_0: u64,
    pub epoch_3_1: u64,
    pub epoch_3_2: u64,
    // keep epoch 3.3 optional for now while it's not enabled on the default image
    #[serde(skip_serializing_if = "Option::is_none")]
    pub epoch_3_3: Option<u64>,
    pub use_docker_gateway_routing: bool,
    pub docker_platform: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PoxStackingOrder {
    pub start_at_cycle: u32,
    pub duration: u32,
    pub wallet: String,
    pub slots: u64,
    pub btc_address: String,
    pub auto_extend: Option<bool>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct AccountConfig {
    pub label: String,
    pub mnemonic: String,
    pub derivation: String,
    pub balance: u64,
    pub sbtc_balance: u64,
    pub stx_address: String,
    pub btc_address: String,
    pub is_mainnet: bool,
}

impl NetworkManifest {
    pub fn from_project_manifest_location(
        project_manifest_location: &FileLocation,
        networks: &(BitcoinNetwork, StacksNetwork),
        use_mainnet_wallets: bool,
        cache_location: Option<&FileLocation>,
        devnet_override: Option<DevnetConfigFile>,
    ) -> Result<NetworkManifest, String> {
        let network_manifest_location =
            project_manifest_location.get_network_manifest_location(&networks.1)?;
        NetworkManifest::from_location(
            &network_manifest_location,
            networks,
            use_mainnet_wallets,
            cache_location,
            devnet_override,
        )
    }

    pub async fn from_project_manifest_location_using_file_accessor(
        location: &FileLocation,
        networks: &(BitcoinNetwork, StacksNetwork),
        use_mainnet_wallets: bool,
        file_accessor: &dyn FileAccessor,
    ) -> Result<NetworkManifest, String> {
        let mut network_manifest_location = location.get_parent_location()?;
        network_manifest_location.append_path("settings/Devnet.toml")?;
        let content = file_accessor
            .read_file(network_manifest_location.to_string())
            .await?;

        let mut network_manifest_file: NetworkManifestFile =
            toml::from_slice(content.as_bytes()).unwrap();
        NetworkManifest::from_network_manifest_file(
            &mut network_manifest_file,
            networks,
            use_mainnet_wallets,
            None,
            None,
        )
    }

    pub fn from_location(
        location: &FileLocation,
        networks: &(BitcoinNetwork, StacksNetwork),
        use_mainnet_wallets: bool,
        cache_location: Option<&FileLocation>,
        devnet_override: Option<DevnetConfigFile>,
    ) -> Result<NetworkManifest, String> {
        let network_manifest_file_content = location.read_content()?;
        let mut network_manifest_file: NetworkManifestFile =
            toml::from_slice(&network_manifest_file_content[..]).unwrap();
        NetworkManifest::from_network_manifest_file(
            &mut network_manifest_file,
            networks,
            use_mainnet_wallets,
            cache_location,
            devnet_override,
        )
    }

    pub fn from_network_manifest_file(
        network_manifest_file: &mut NetworkManifestFile,
        networks: &(BitcoinNetwork, StacksNetwork),
        use_mainnet_wallets: bool,
        cache_location: Option<&FileLocation>,
        devnet_override: Option<DevnetConfigFile>,
    ) -> Result<NetworkManifest, String> {
        let stacks_node_rpc_address = match (
            &network_manifest_file.network.node_rpc_address,
            &network_manifest_file.network.stacks_node_rpc_address,
        ) {
            (Some(_), Some(url)) | (None, Some(url)) | (Some(url), None) => Some(url.clone()),
            _ => None,
        };
        let network = NetworkConfig {
            name: network_manifest_file.network.name.clone(),
            stacks_node_rpc_address,
            bitcoin_node_rpc_address: network_manifest_file
                .network
                .bitcoin_node_rpc_address
                .clone(),
            deployment_fee_rate: network_manifest_file
                .network
                .deployment_fee_rate
                .unwrap_or(10),
            sats_per_bytes: network_manifest_file.network.sats_per_bytes.unwrap_or(10),
        };

        let mut accounts = BTreeMap::new();
        let is_mainnet = matches!(networks.1, StacksNetwork::Mainnet);

        if let Some(Value::Table(entries)) = &network_manifest_file.accounts {
            for (account_name, account_settings) in entries.iter() {
                if let Value::Table(account_settings) = account_settings {
                    let balance = match account_settings.get("balance") {
                        Some(Value::Integer(balance)) => *balance as u64,
                        _ => 0,
                    };
                    let sbtc_balance = match account_settings.get("sbtc_balance") {
                        Some(Value::Integer(balance)) => *balance as u64,
                        _ => 1_000_000_000, // mint 10 sBTC by default
                    };

                    let mnemonic = match account_settings.get("mnemonic") {
                        Some(Value::String(phrase)) => match mnemonic_from_phrase(phrase) {
                            Ok(result) => result.to_string(),
                            Err(e) => {
                                return Err(format!(
                                        "mnemonic (located in ./settings/{:?}.toml) for deploying address is invalid: {}",
                                        networks.1 , e
                                    ));
                            }
                        },
                        _ => random_mnemonic().to_string(),
                    };

                    let derivation = match account_settings.get("derivation") {
                        Some(Value::String(derivation)) => derivation.to_string(),
                        _ => DEFAULT_DERIVATION_PATH.to_string(),
                    };

                    let addresses_network = if use_mainnet_wallets {
                        (networks.0.clone(), StacksNetwork::Mainnet)
                    } else {
                        networks.clone()
                    };
                    let (stx_address, btc_address, _) =
                        compute_addresses(&mnemonic, &derivation, &addresses_network);

                    accounts.insert(
                        account_name.to_string(),
                        AccountConfig {
                            label: account_name.to_string(),
                            mnemonic: mnemonic.to_string(),
                            derivation,
                            balance,
                            sbtc_balance,
                            stx_address,
                            btc_address,
                            is_mainnet,
                        },
                    );
                }
            }
        };

        let devnet = if matches!(networks.1, StacksNetwork::Devnet) {
            let mut devnet_config = network_manifest_file.devnet.take().unwrap_or_default();

            if let Some(ref devnet_override) = devnet_override {
                if let Some(ref val) = devnet_override.name {
                    devnet_config.name = Some(val.clone());
                }

                if let Some(val) = devnet_override.orchestrator_port {
                    devnet_config.orchestrator_port = Some(val);
                }

                if let Some(val) = devnet_override.orchestrator_control_port {
                    devnet_config.orchestrator_control_port = Some(val);
                }

                if let Some(val) = devnet_override.bitcoin_node_p2p_port {
                    devnet_config.bitcoin_node_p2p_port = Some(val);
                }

                if let Some(val) = devnet_override.bitcoin_node_rpc_port {
                    devnet_config.bitcoin_node_rpc_port = Some(val);
                }

                if let Some(val) = devnet_override.stacks_node_p2p_port {
                    devnet_config.stacks_node_p2p_port = Some(val);
                }

                if let Some(val) = devnet_override.stacks_node_rpc_port {
                    devnet_config.stacks_node_rpc_port = Some(val);
                }

                if let Some(ref val) = devnet_override.stacks_node_events_observers {
                    devnet_config.stacks_node_events_observers = Some(val.clone());
                }

                if let Some(val) = devnet_override.stacks_node_next_initiative_delay {
                    devnet_config.stacks_node_next_initiative_delay = Some(val);
                }

                if let Some(val) = devnet_override.stacks_api_port {
                    devnet_config.stacks_api_port = Some(val);
                }

                if let Some(val) = devnet_override.stacks_api_events_port {
                    devnet_config.stacks_api_events_port = Some(val);
                }

                if let Some(val) = devnet_override.bitcoin_explorer_port {
                    devnet_config.bitcoin_explorer_port = Some(val);
                }

                if let Some(val) = devnet_override.stacks_explorer_port {
                    devnet_config.stacks_explorer_port = Some(val);
                }

                if let Some(ref val) = devnet_override.bitcoin_node_username {
                    devnet_config.bitcoin_node_username = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.bitcoin_node_password {
                    devnet_config.bitcoin_node_password = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.miner_mnemonic {
                    devnet_config.miner_mnemonic = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.miner_derivation_path {
                    devnet_config.miner_derivation_path = Some(val.clone());
                }

                if let Some(val) = devnet_override.bitcoin_controller_block_time {
                    devnet_config.bitcoin_controller_block_time = Some(val);
                }

                if let Some(ref val) = devnet_override.working_dir {
                    devnet_config.working_dir = Some(val.clone());
                }

                if let Some(val) = devnet_override.postgres_port {
                    devnet_config.postgres_port = Some(val);
                }

                if let Some(ref val) = devnet_override.postgres_username {
                    devnet_config.postgres_username = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.postgres_password {
                    devnet_config.postgres_password = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.stacks_api_postgres_database {
                    devnet_config.stacks_api_postgres_database = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.pox_stacking_orders {
                    devnet_config.pox_stacking_orders = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.execute_script {
                    devnet_config.execute_script = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.bitcoin_node_image_url {
                    devnet_config.bitcoin_node_image_url = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.bitcoin_explorer_image_url {
                    devnet_config.bitcoin_explorer_image_url = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.stacks_node_image_url {
                    devnet_config.stacks_node_image_url = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.stacks_api_image_url {
                    devnet_config.stacks_api_image_url = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.stacks_explorer_image_url {
                    devnet_config.stacks_explorer_image_url = Some(val.clone());
                }

                if let Some(ref val) = devnet_override.postgres_image_url {
                    devnet_config.postgres_image_url = Some(val.clone());
                }

                if let Some(val) = devnet_override.disable_bitcoin_explorer {
                    devnet_config.disable_bitcoin_explorer = Some(val);
                }

                if let Some(val) = devnet_override.disable_stacks_explorer {
                    devnet_config.disable_stacks_explorer = Some(val);
                }

                if let Some(val) = devnet_override.disable_stacks_api {
                    devnet_config.disable_stacks_api = Some(val);
                }

                if let Some(val) = devnet_override.disable_postgres {
                    devnet_config.disable_postgres = Some(val);
                }

                if let Some(val) = devnet_override.bitcoin_controller_automining_disabled {
                    devnet_config.bitcoin_controller_automining_disabled = Some(val);
                }

                if let Some(ref val) = devnet_override.epoch_2_0 {
                    devnet_config.epoch_2_0 = Some(*val);
                }

                if let Some(ref val) = devnet_override.epoch_2_05 {
                    devnet_config.epoch_2_05 = Some(*val);
                }

                if let Some(ref val) = devnet_override.epoch_2_1 {
                    devnet_config.epoch_2_1 = Some(*val);
                }

                if let Some(ref val) = devnet_override.epoch_2_2 {
                    devnet_config.epoch_2_2 = Some(*val);
                }

                if let Some(ref val) = devnet_override.epoch_2_3 {
                    devnet_config.epoch_2_3 = Some(*val);
                }

                if let Some(ref val) = devnet_override.epoch_2_4 {
                    devnet_config.epoch_2_4 = Some(*val);
                }

                if let Some(ref val) = devnet_override.epoch_2_5 {
                    devnet_config.epoch_2_5 = Some(*val);
                }

                if let Some(ref val) = devnet_override.epoch_3_0 {
                    devnet_config.epoch_3_0 = Some(*val);
                }

                if let Some(ref val) = devnet_override.epoch_3_1 {
                    devnet_config.epoch_3_1 = Some(*val);
                }

                if let Some(ref val) = devnet_override.epoch_3_2 {
                    devnet_config.epoch_3_2 = Some(*val);
                }

                if let Some(val) = devnet_override.network_id {
                    devnet_config.network_id = Some(val);
                }

                if let Some(val) = devnet_override.use_docker_gateway_routing {
                    devnet_config.use_docker_gateway_routing = Some(val);
                }
            };

            let now = clarity::util::get_epoch_time_secs();
            let devnet_dir = if let Some(network_id) = devnet_config.network_id {
                format!("stacks-devnet-{now}-{network_id}/")
            } else {
                format!("stacks-devnet-{now}/")
            };
            let default_working_dir = match cache_location {
                Some(cache_location) => {
                    let mut devnet_location = cache_location.clone();
                    let _ = devnet_location.append_path(&devnet_dir);
                    devnet_location.to_string()
                }
                None => {
                    let mut dir = std::env::temp_dir();
                    dir.push(devnet_dir);
                    dir.display().to_string()
                }
            };

            let miner_mnemonic = devnet_config
                .miner_mnemonic
                .take()
                .unwrap_or(DEFAULT_STACKS_MINER_MNEMONIC.to_string());
            let miner_derivation_path = devnet_config
                .miner_derivation_path
                .take()
                .unwrap_or(DEFAULT_DERIVATION_PATH.to_string());
            let (miner_stx_address, miner_btc_address, miner_secret_key_hex) =
                compute_addresses(&miner_mnemonic, &miner_derivation_path, networks);

            let faucet_mnemonic = devnet_config
                .faucet_mnemonic
                .take()
                .unwrap_or(DEFAULT_FAUCET_MNEMONIC.to_string());
            let faucet_derivation_path = devnet_config
                .faucet_derivation_path
                .take()
                .unwrap_or(DEFAULT_DERIVATION_PATH.to_string());
            let (faucet_stx_address, faucet_btc_address, faucet_secret_key_hex) =
                compute_addresses(&faucet_mnemonic, &faucet_derivation_path, networks);

            let stacks_node_events_observers = devnet_config
                .stacks_node_events_observers
                .take()
                .unwrap_or_default();

            // validate that epoch 3.0 is started in a reward phase
            let epoch_3_0 = devnet_config.epoch_3_0.unwrap_or(DEFAULT_EPOCH_3_0);
            if !is_in_reward_phase(
                DEFAULT_FIRST_BURN_HEADER_HEIGHT,
                DEFAULT_POX_REWARD_LENGTH,
                DEFAULT_POX_PREPARE_LENGTH,
                &epoch_3_0,
            ) {
                return Err(format!(
                    "Epoch 3.0 must start *during* a reward phase, not a prepare phase. Epoch 3.0 start set to: {epoch_3_0}. Reward Cycle Length: {DEFAULT_POX_REWARD_LENGTH}. Prepare Phase Length: {DEFAULT_POX_PREPARE_LENGTH}"
                ));
            }

            let stacker_mnemonic = devnet_config
                .stacker_mnemonic
                .take()
                .unwrap_or(DEFAULT_STACKER_MNEMONIC.to_string());
            let stacker_derivation_path = devnet_config
                .stacker_derivation_path
                .take()
                .unwrap_or(DEFAULT_DERIVATION_PATH.to_string());
            let (stx_address, btc_address, _) =
                compute_addresses(&stacker_mnemonic, &stacker_derivation_path, networks);

            accounts.insert(
                "stacker".to_string(),
                AccountConfig {
                    label: "stacker".to_string(),
                    mnemonic: stacker_mnemonic.clone(),
                    derivation: stacker_derivation_path.clone(),
                    balance: 100_000_000_000_000,
                    sbtc_balance: 1_000_000_000,
                    stx_address,
                    btc_address,
                    is_mainnet: false,
                },
            );

            let mut stacking_orders = vec![];
            let mut add_default_stacking_order = true;
            // for stacking orders, we validate that wallet names match one of the provided accounts
            if let Some(mut val) = devnet_config.pox_stacking_orders {
                for (i, stacking_order) in val.iter().enumerate() {
                    let wallet_name = &stacking_order.wallet;

                    // if the project already set a stacking order for the stacker, do not override it
                    if wallet_name == "stacker" {
                        add_default_stacking_order = false;
                    }

                    let wallet_is_in_accounts = accounts
                        .iter()
                        .any(|(account_name, _)| wallet_name == account_name);
                    if !wallet_is_in_accounts {
                        return Err(format!("Account data was not provided for the wallet ({}) listed in stacking order {}.", wallet_name, i + 1));
                    };
                }

                stacking_orders.append(&mut val);
            }

            // to ensure that the network stacks enough STXs to reach epoch 3.0
            // add a default stacking order for deployer wallet in cycle 1
            if add_default_stacking_order {
                if let Some((_, account_config)) = accounts
                    .iter()
                    .find(|(account_name, _)| *account_name == "stacker")
                {
                    stacking_orders.push(PoxStackingOrder {
                        auto_extend: Some(true),
                        duration: 10,
                        start_at_cycle: 1,
                        wallet: "stacker".into(),
                        slots: 10,
                        btc_address: account_config.btc_address.clone(),
                    })
                }
            }

            let config = DevnetConfig {
                name: devnet_config.name.take().unwrap_or("devnet".into()),
                network_id: devnet_config.network_id,
                orchestrator_ingestion_port: devnet_config.orchestrator_port.unwrap_or(20445),
                orchestrator_control_port: devnet_config.orchestrator_control_port.unwrap_or(20446),
                bitcoin_node_p2p_port: devnet_config.bitcoin_node_p2p_port.unwrap_or(18444),
                bitcoin_node_rpc_port: devnet_config.bitcoin_node_rpc_port.unwrap_or(18443),
                bitcoin_node_username: devnet_config
                    .bitcoin_node_username
                    .take()
                    .unwrap_or("devnet".to_string()),
                bitcoin_node_password: devnet_config
                    .bitcoin_node_password
                    .take()
                    .unwrap_or("devnet".to_string()),
                bitcoin_controller_block_time: devnet_config
                    .bitcoin_controller_block_time
                    .unwrap_or(60_000),
                bitcoin_controller_automining_disabled: devnet_config
                    .bitcoin_controller_automining_disabled
                    .unwrap_or(false),
                stacks_node_p2p_port: devnet_config.stacks_node_p2p_port.unwrap_or(20444),
                stacks_node_rpc_port: devnet_config.stacks_node_rpc_port.unwrap_or(20443),
                stacks_node_events_observers,
                stacks_node_wait_time_for_microblocks: devnet_config
                    .stacks_node_wait_time_for_microblocks
                    .unwrap_or(50),
                stacks_node_first_attempt_time_ms: devnet_config
                    .stacks_node_first_attempt_time_ms
                    .unwrap_or(500),
                stacks_node_next_initiative_delay: devnet_config
                    .stacks_node_next_initiative_delay
                    .unwrap_or(3000),
                stacks_api_port: devnet_config.stacks_api_port.unwrap_or(3999),
                stacks_api_events_port: devnet_config.stacks_api_events_port.unwrap_or(3700),
                stacks_explorer_port: devnet_config.stacks_explorer_port.unwrap_or(8000),
                bitcoin_explorer_port: devnet_config.bitcoin_explorer_port.unwrap_or(8001),
                miner_btc_address,
                miner_stx_address: miner_stx_address.clone(),
                miner_mnemonic,
                miner_secret_key_hex,
                miner_derivation_path,
                miner_coinbase_recipient: devnet_config
                    .miner_coinbase_recipient
                    .unwrap_or(miner_stx_address),
                miner_wallet_name: devnet_config.miner_wallet_name.unwrap_or("".to_string()),
                pre_nakamoto_mock_signing: devnet_config
                    .pre_nakamoto_mock_signing
                    .unwrap_or_default(),
                faucet_btc_address,
                faucet_stx_address,
                faucet_mnemonic,
                faucet_secret_key_hex,
                faucet_derivation_path,
                stacker_mnemonic,
                stacker_derivation_path,
                working_dir: devnet_config
                    .working_dir
                    .take()
                    .unwrap_or(default_working_dir),
                postgres_port: devnet_config.postgres_port.unwrap_or(5432),
                postgres_username: devnet_config
                    .postgres_username
                    .take()
                    .unwrap_or("postgres".to_string()),
                postgres_password: devnet_config
                    .postgres_password
                    .take()
                    .unwrap_or("postgres".to_string()),
                stacks_api_postgres_database: devnet_config
                    .stacks_api_postgres_database
                    .take()
                    .unwrap_or("stacks_api".to_string()),
                execute_script: devnet_config.execute_script.take().unwrap_or_default(),
                bitcoin_node_image_url: devnet_config
                    .bitcoin_node_image_url
                    .take()
                    .unwrap_or(DEFAULT_BITCOIN_NODE_IMAGE.to_string()),
                stacks_node_image_url: devnet_config
                    .stacks_node_image_url
                    .take()
                    .unwrap_or(DEFAULT_STACKS_NODE_IMAGE.to_string()),
                stacks_signer_image_url: devnet_config
                    .stacks_signer_image_url
                    .take()
                    .unwrap_or(DEFAULT_STACKS_SIGNER_IMAGE.to_string()),
                stacks_api_image_url: devnet_config
                    .stacks_api_image_url
                    .take()
                    .unwrap_or(DEFAULT_STACKS_API_IMAGE.to_string()),
                postgres_image_url: devnet_config
                    .postgres_image_url
                    .take()
                    .unwrap_or(DEFAULT_POSTGRES_IMAGE.to_string()),
                stacks_explorer_image_url: devnet_config
                    .stacks_explorer_image_url
                    .take()
                    .unwrap_or(DEFAULT_STACKS_EXPLORER_IMAGE.to_string()),
                bitcoin_explorer_image_url: devnet_config
                    .bitcoin_explorer_image_url
                    .take()
                    .unwrap_or(DEFAULT_BITCOIN_EXPLORER_IMAGE.to_string()),
                pox_stacking_orders: stacking_orders,
                disable_bitcoin_explorer: devnet_config.disable_bitcoin_explorer.unwrap_or(false),
                disable_stacks_api: devnet_config.disable_stacks_api.unwrap_or(false),
                disable_postgres: devnet_config.disable_postgres.unwrap_or(false),
                disable_stacks_explorer: devnet_config.disable_stacks_explorer.unwrap_or(false),
                bind_containers_volumes: devnet_config.bind_containers_volumes.unwrap_or(true),
                docker_host: devnet_config
                    .docker_host
                    .unwrap_or(DEFAULT_DOCKER_SOCKET.into()),
                components_host: devnet_config.components_host.unwrap_or("127.0.0.1".into()),
                epoch_2_0: devnet_config.epoch_2_0.unwrap_or(DEFAULT_EPOCH_2_0),
                epoch_2_05: devnet_config.epoch_2_05.unwrap_or(DEFAULT_EPOCH_2_05),
                epoch_2_1: devnet_config.epoch_2_1.unwrap_or(DEFAULT_EPOCH_2_1),
                epoch_2_2: devnet_config.epoch_2_2.unwrap_or(DEFAULT_EPOCH_2_2),
                epoch_2_3: devnet_config.epoch_2_3.unwrap_or(DEFAULT_EPOCH_2_3),
                epoch_2_4: devnet_config.epoch_2_4.unwrap_or(DEFAULT_EPOCH_2_4),
                epoch_2_5: devnet_config.epoch_2_5.unwrap_or(DEFAULT_EPOCH_2_5),
                epoch_3_0: devnet_config.epoch_3_0.unwrap_or(DEFAULT_EPOCH_3_0),
                epoch_3_1: devnet_config.epoch_3_1.unwrap_or(DEFAULT_EPOCH_3_1),
                epoch_3_2: devnet_config.epoch_3_2.unwrap_or(DEFAULT_EPOCH_3_2),
                epoch_3_3: devnet_config.epoch_3_3,
                stacks_node_env_vars: devnet_config
                    .stacks_node_env_vars
                    .take()
                    .unwrap_or_default(),
                stacks_signers_keys: devnet_config
                    .stacks_signers_keys
                    .take()
                    .map(|keys| {
                        keys.into_iter()
                            .map(|key| StacksPrivateKey::from_hex(&key).unwrap())
                            .collect::<Vec<clarity::util::secp256k1::Secp256k1PrivateKey>>()
                    })
                    .unwrap_or(DEFAULT_PRIVATE_KEYS.to_vec()),
                stacks_signers_env_vars: devnet_config
                    .stacks_signers_env_vars
                    .take()
                    .unwrap_or_default(),
                stacks_api_env_vars: devnet_config.stacks_api_env_vars.take().unwrap_or_default(),
                stacks_explorer_env_vars: devnet_config
                    .stacks_explorer_env_vars
                    .take()
                    .unwrap_or_default(),
                use_docker_gateway_routing: devnet_config
                    .use_docker_gateway_routing
                    .unwrap_or(false),
                docker_platform: devnet_config.docker_platform,
            };
            Some(config)
        } else {
            None
        };
        let config = NetworkManifest {
            network,
            accounts,
            devnet,
        };

        Ok(config)
    }
}

impl Default for DevnetConfig {
    fn default() -> Self {
        // Compute default addresses using the default mnemonics
        let networks = (BitcoinNetwork::Regtest, StacksNetwork::Devnet);

        let (miner_stx_address, miner_btc_address, miner_secret_key_hex) = compute_addresses(
            DEFAULT_STACKS_MINER_MNEMONIC,
            DEFAULT_DERIVATION_PATH,
            &networks,
        );

        let (faucet_stx_address, faucet_btc_address, faucet_secret_key_hex) =
            compute_addresses(DEFAULT_FAUCET_MNEMONIC, DEFAULT_DERIVATION_PATH, &networks);

        Self {
            name: "devnet".to_string(),
            network_id: None,
            orchestrator_ingestion_port: 20445,
            orchestrator_control_port: 20446,
            bitcoin_node_p2p_port: 18444,
            bitcoin_node_rpc_port: 18443,
            bitcoin_node_username: "devnet".to_string(),
            bitcoin_node_password: "devnet".to_string(),
            stacks_node_p2p_port: 20444,
            stacks_node_rpc_port: 20443,
            stacks_node_wait_time_for_microblocks: 50,
            stacks_node_first_attempt_time_ms: 500,
            stacks_node_events_observers: vec![],
            stacks_node_env_vars: vec![],
            stacks_node_next_initiative_delay: 3000,
            stacks_api_port: 3999,
            stacks_api_events_port: 3700,
            stacks_api_env_vars: vec![],
            stacks_signers_keys: DEFAULT_PRIVATE_KEYS.to_vec(),
            stacks_signers_env_vars: vec![],
            stacks_explorer_port: 8000,
            stacks_explorer_env_vars: vec![],
            bitcoin_explorer_port: 8001,
            bitcoin_controller_block_time: 60_000,
            bitcoin_controller_automining_disabled: false,
            miner_stx_address: miner_stx_address.clone(),
            miner_secret_key_hex,
            miner_btc_address,
            miner_mnemonic: DEFAULT_STACKS_MINER_MNEMONIC.to_string(),
            miner_derivation_path: DEFAULT_DERIVATION_PATH.to_string(),
            miner_coinbase_recipient: miner_stx_address,
            miner_wallet_name: String::new(),
            faucet_stx_address,
            faucet_secret_key_hex,
            faucet_btc_address,
            faucet_mnemonic: DEFAULT_FAUCET_MNEMONIC.to_string(),
            faucet_derivation_path: DEFAULT_DERIVATION_PATH.to_string(),
            stacker_mnemonic: DEFAULT_STACKER_MNEMONIC.to_string(),
            stacker_derivation_path: DEFAULT_DERIVATION_PATH.to_string(),
            pre_nakamoto_mock_signing: false,
            working_dir: "/tmp".to_string(),
            postgres_port: 5432,
            postgres_username: "postgres".to_string(),
            postgres_password: "postgres".to_string(),
            stacks_api_postgres_database: "stacks_api".to_string(),
            pox_stacking_orders: get_default_stacking_orders(),
            execute_script: vec![],
            bitcoin_node_image_url: DEFAULT_BITCOIN_NODE_IMAGE.to_string(),
            stacks_node_image_url: DEFAULT_STACKS_NODE_IMAGE.to_string(),
            stacks_signer_image_url: DEFAULT_STACKS_SIGNER_IMAGE.to_string(),
            stacks_api_image_url: DEFAULT_STACKS_API_IMAGE.to_string(),
            stacks_explorer_image_url: DEFAULT_STACKS_EXPLORER_IMAGE.to_string(),
            postgres_image_url: DEFAULT_POSTGRES_IMAGE.to_string(),
            bitcoin_explorer_image_url: DEFAULT_BITCOIN_EXPLORER_IMAGE.to_string(),
            disable_bitcoin_explorer: false,
            disable_stacks_explorer: false,
            disable_stacks_api: false,
            disable_postgres: false,
            bind_containers_volumes: true,
            docker_host: DEFAULT_DOCKER_SOCKET.to_string(),
            components_host: "127.0.0.1".to_string(),
            epoch_2_0: DEFAULT_EPOCH_2_0,
            epoch_2_05: DEFAULT_EPOCH_2_05,
            epoch_2_1: DEFAULT_EPOCH_2_1,
            epoch_2_2: DEFAULT_EPOCH_2_2,
            epoch_2_3: DEFAULT_EPOCH_2_3,
            epoch_2_4: DEFAULT_EPOCH_2_4,
            epoch_2_5: DEFAULT_EPOCH_2_5,
            epoch_3_0: DEFAULT_EPOCH_3_0,
            epoch_3_1: DEFAULT_EPOCH_3_1,
            epoch_3_2: DEFAULT_EPOCH_3_2,
            epoch_3_3: None,
            use_docker_gateway_routing: false,
            docker_platform: None,
        }
    }
}

pub fn get_default_stacking_orders() -> Vec<PoxStackingOrder> {
    let accounts = [
        ("stacker", "n3r661yy817HN3BZvec67XM1smryNTaizX", 10),
        ("wallet_1", "mr1iPkD9N3RJZZxXRk7xF9d36gffa6exNC", 2),
        ("wallet_2", "muYdXKmX9bByAueDe6KFfHd5Ff1gdN9ErG", 2),
        ("wallet_3", "mvZtbibDAAA3WLpY7zXXFqRa3T4XSknBX7", 2),
    ];
    let mut stacking_orders = vec![];
    for (name, btc_address, slots) in accounts {
        stacking_orders.push(PoxStackingOrder {
            auto_extend: Some(true),
            duration: 10,
            start_at_cycle: 1,
            wallet: name.into(),
            slots,
            btc_address: btc_address.to_owned(),
        })
    }
    stacking_orders
}

pub fn compute_addresses(
    mnemonic: &str,
    derivation_path: &str,
    networks: &(BitcoinNetwork, StacksNetwork),
) -> (String, String, String) {
    let (secret_bytes, public_key) =
        get_bip32_keys_from_mnemonic(mnemonic, "", derivation_path).unwrap();

    // Enforce a 33 bytes secret key format, expected by Stacks
    let mut secret_key_bytes = secret_bytes;
    secret_key_bytes.push(1);
    let miner_secret_key_hex = bytes_to_hex(&secret_key_bytes);

    let pub_key = Secp256k1PublicKey::from_slice(&public_key.serialize_compressed()).unwrap();
    let version = if matches!(networks.1, StacksNetwork::Mainnet) {
        stacks_common::address::C32_ADDRESS_VERSION_MAINNET_SINGLESIG
    } else {
        stacks_common::address::C32_ADDRESS_VERSION_TESTNET_SINGLESIG
    };

    let stx_address = StacksAddress::from_public_keys(
        version,
        &stacks_common::address::AddressHashMode::SerializeP2PKH,
        1,
        &vec![pub_key],
    )
    .unwrap();

    let btc_address = compute_btc_address(&public_key, &networks.0);

    (stx_address.to_string(), btc_address, miner_secret_key_hex)
}

#[cfg(not(target_arch = "wasm32"))]
fn compute_btc_address(public_key: &PublicKey, network: &BitcoinNetwork) -> String {
    let public_key = bitcoin::PublicKey::from_slice(&public_key.serialize_compressed())
        .expect("Unable to recreate public key");
    let btc_address = bitcoin::Address::p2pkh(
        &public_key,
        match network {
            BitcoinNetwork::Signet => bitcoin::Network::Signet,
            BitcoinNetwork::Regtest => bitcoin::Network::Regtest,
            BitcoinNetwork::Testnet => bitcoin::Network::Testnet,
            BitcoinNetwork::Mainnet => bitcoin::Network::Bitcoin,
        },
    );
    btc_address.to_string()
}

// This logic was taken from stacks-core:
// https://github.com/stacks-network/stacks-core/blob/524b0e1ae9ad3c8d2d2ac37e72be4aee2c045ef8/src/burnchains/mod.rs#L513C30-L530
pub fn is_in_reward_phase(
    first_block_height: u64,
    reward_cycle_length: u64,
    prepare_length: u64,
    block_height: &u64,
) -> bool {
    if block_height <= &first_block_height {
        // not a reward cycle start if we're the first block after genesis.
        false
    } else {
        let effective_height = block_height - first_block_height;
        let reward_index = effective_height % reward_cycle_length;

        // NOTE: first block in reward cycle is mod 1, so mod 0 is the last block in the
        // prepare phase.
        !(reward_index == 0 || reward_index > (reward_cycle_length - prepare_length))
    }
}

#[cfg(target_arch = "wasm32")]
fn compute_btc_address(_public_key: &PublicKey, _network: &BitcoinNetwork) -> String {
    "__not_implemented__".to_string()
}

#[cfg(test)]
mod tests {
    use clarity_repl::repl::DEFAULT_EPOCH;

    use crate::{DEFAULT_STACKS_NODE_IMAGE, DEFAULT_STACKS_SIGNER_IMAGE};

    #[test]
    fn test_default_stacks_docker_images_version() {
        let epoch = DEFAULT_EPOCH.to_string();

        let default_node_version = DEFAULT_STACKS_NODE_IMAGE
            .split(':')
            .nth(1)
            .expect("Default Stacks node image should contain a version tag");
        assert!(
            default_node_version.starts_with(epoch.as_str()),
            "The default Stacks node image {DEFAULT_STACKS_NODE_IMAGE} does not start with {epoch}"
        );

        let default_signer_version = DEFAULT_STACKS_NODE_IMAGE
            .split(':')
            .nth(1)
            .expect("Default Stacks signer image should contain a version tag");
        assert!(default_signer_version.starts_with(epoch.as_str()),
            "The default Stacks signer image {DEFAULT_STACKS_SIGNER_IMAGE} does not start with {epoch}"
        );
    }
}
