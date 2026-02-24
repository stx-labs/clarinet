mod http;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::error::Error;
use std::net::SocketAddr;
use std::str;
use std::str::FromStr;
use std::sync::mpsc::{Receiver, Sender};
use std::sync::{Arc, Mutex, RwLock};

use bitcoincore_rpc::bitcoin::{BlockHash, Txid};
use bitcoincore_rpc::{Auth, Client, RpcApi};
use crate::chainhook::types::{
    BitcoinBlockData, BitcoinBlockSignaling, BitcoinChainEvent, BitcoinChainUpdatedWithBlocksData,
    BitcoinChainUpdatedWithReorgData, BitcoinNetwork, BlockIdentifier, BlockchainEvent, Chain,
    StacksBlockData, StacksChainEvent, StacksNetwork, StacksNodeConfig, TransactionIdentifier,
    DEFAULT_STACKS_NODE_RPC,
};
use hiro_system_kit::slog;
use serde::{Deserialize, Serialize};

// Custom shutdown wrapper to replace Rocket's Shutdown
pub struct Shutdown(tokio::sync::oneshot::Sender<()>);

impl Shutdown {
    pub fn notify(self) {
        let _ = self.0.send(());
    }
}

// LogLevel enum to replace Rocket's LogLevel
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum LogLevel {
    Off,
    Critical,
    #[default]
    Normal,
    Debug,
}

use crate::chainhook::hooks::bitcoin::{
    evaluate_bitcoin_chainhooks_on_chain_event, handle_bitcoin_hook_action,
    BitcoinChainhookInstance, BitcoinChainhookOccurrence, BitcoinChainhookOccurrencePayload,
    BitcoinTriggerChainhook,
};
use crate::chainhook::hooks::stacks::{
    evaluate_stacks_chainhooks_on_chain_event, handle_stacks_hook_action, StacksChainhookInstance,
    StacksChainhookOccurrence, StacksChainhookOccurrencePayload,
};
use crate::chainhook::hooks::types::{
    ChainhookInstance, ChainhookSpecificationNetworkMap, ChainhookStore,
};
use crate::chainhook::indexer::bitcoin::{
    build_http_client, download_and_parse_block_with_retry, standardize_bitcoin_block,
    BitcoinBlockFullBreakdown,
};
use crate::chainhook::indexer::{Indexer, IndexerConfig};
use crate::chainhook::utils::{send_request, Context};

pub const DEFAULT_INGESTION_PORT: u16 = 20445;

#[derive(Deserialize)]
pub struct NewTransaction {
    pub txid: String,
    pub status: String,
    pub raw_result: String,
    pub raw_tx: String,
}

#[derive(Clone, Debug)]
pub enum Event {
    BitcoinChainEvent(BitcoinChainEvent),
    StacksChainEvent(StacksChainEvent),
}

pub enum DataHandlerEvent {
    Process(BitcoinChainhookOccurrencePayload),
    Terminate,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PredicatesConfig {
    pub payload_http_request_timeout_ms: Option<u64>,
}

impl PredicatesConfig {
    pub fn new() -> Self {
        PredicatesConfig {
            payload_http_request_timeout_ms: None,
        }
    }
}

impl Default for PredicatesConfig {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct EventObserverConfig {
    pub registered_chainhooks: ChainhookStore,
    pub predicates_config: PredicatesConfig,
    pub bitcoin_rpc_proxy_enabled: bool,
    pub bitcoind_rpc_username: String,
    pub bitcoind_rpc_password: String,
    pub bitcoind_rpc_url: String,
    pub bitcoin_block_signaling: BitcoinBlockSignaling,
    pub display_stacks_ingestion_logs: bool,
    pub bitcoin_network: BitcoinNetwork,
    pub stacks_network: StacksNetwork,
    pub prometheus_monitoring_port: Option<u16>,
}

/// A builder that is used to create a general purpose [EventObserverConfig].
///
/// ## Examples
/// ```
/// use chainhook_sdk::observer::EventObserverConfig;
/// use chainhook_sdk::observer::EventObserverConfigBuilder;
///
/// fn get_config() -> Result<EventObserverConfig, String> {
///     EventObserverConfigBuilder::new()
///         .bitcoind_rpc_password("my_password")
///         .bitcoin_network("mainnet")
///         .stacks_network("mainnet")
///         .finish()
/// }
/// ```
#[derive(Deserialize, Debug, Clone)]
pub struct EventObserverConfigBuilder {
    pub bitcoind_rpc_username: Option<String>,
    pub bitcoind_rpc_password: Option<String>,
    pub bitcoind_rpc_url: Option<String>,
    pub bitcoind_zmq_url: Option<String>,
    pub chainhook_stacks_block_ingestion_port: Option<u16>,
    pub stacks_node_rpc_url: Option<String>,
    pub display_stacks_ingestion_logs: Option<bool>,
    pub bitcoin_network: Option<String>,
    pub stacks_network: Option<String>,
    pub prometheus_monitoring_port: Option<u16>,
}

impl Default for EventObserverConfigBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl EventObserverConfigBuilder {
    pub fn new() -> Self {
        EventObserverConfigBuilder {
            bitcoind_rpc_username: None,
            bitcoind_rpc_password: None,
            bitcoind_rpc_url: None,
            bitcoind_zmq_url: None,
            chainhook_stacks_block_ingestion_port: None,
            stacks_node_rpc_url: None,
            display_stacks_ingestion_logs: None,
            bitcoin_network: None,
            stacks_network: None,
            prometheus_monitoring_port: None,
        }
    }

    /// Sets the bitcoind node's RPC username.
    pub fn bitcoind_rpc_username(&mut self, username: &str) -> &mut Self {
        self.bitcoind_rpc_username = Some(username.to_string());
        self
    }

    /// Sets the bitcoind node's RPC password.
    pub fn bitcoind_rpc_password(&mut self, password: &str) -> &mut Self {
        self.bitcoind_rpc_password = Some(password.to_string());
        self
    }

    /// Sets the bitcoind node's RPC url.
    pub fn bitcoind_rpc_url(&mut self, url: &str) -> &mut Self {
        self.bitcoind_rpc_url = Some(url.to_string());
        self
    }

    /// Sets the bitcoind node's ZMQ url, used by the observer to receive new block events from bitcoind.
    pub fn bitcoind_zmq_url(&mut self, url: &str) -> &mut Self {
        self.bitcoind_zmq_url = Some(url.to_string());
        self
    }

    /// Sets the Bitcoin network. Must be a valid bitcoin network string according to [BitcoinNetwork::from_str].
    pub fn bitcoin_network(&mut self, network: &str) -> &mut Self {
        self.bitcoin_network = Some(network.to_string());
        self
    }

    /// Sets the Stacks network. Must be a valid bitcoin network string according to [StacksNetwork::from_str].
    pub fn stacks_network(&mut self, network: &str) -> &mut Self {
        self.stacks_network = Some(network.to_string());
        self
    }

    /// Sets the Stacks node's RPC url.
    pub fn stacks_node_rpc_url(&mut self, url: &str) -> &mut Self {
        self.stacks_node_rpc_url = Some(url.to_string());
        self
    }

    /// Sets the port at which Chainhook will observer Stacks blockchain events. The Stacks node's config should have an events_observer
    /// entry matching this port in order to send block events the Chainhook.
    pub fn chainhook_stacks_block_ingestion_port(&mut self, port: u16) -> &mut Self {
        self.chainhook_stacks_block_ingestion_port = Some(port);
        self
    }

    /// Sets whether Chainhook should display Stacks ingestion logs.
    pub fn display_stacks_ingestion_logs(&mut self, display_logs: bool) -> &mut Self {
        self.display_stacks_ingestion_logs = Some(display_logs);
        self
    }

    /// Sets the Prometheus monitoring port.
    pub fn prometheus_monitoring_port(&mut self, port: u16) -> &mut Self {
        self.prometheus_monitoring_port = Some(port);
        self
    }

    /// Attempts to convert a [EventObserverConfigBuilder] instance into an [EventObserverConfig], filling in
    /// defaults as necessary according to [EventObserverConfig::default].
    ///
    /// This function will return an error if the `bitcoin_network` or `stacks_network` strings are set and are not a valid [BitcoinNetwork] or [StacksNetwork].
    ///
    pub fn finish(&self) -> Result<EventObserverConfig, String> {
        EventObserverConfig::new_using_overrides(Some(self))
    }
}

/// A builder that is used to create an [EventObserverConfig] that is tailored for use with a bitcoind node emitting events via the ZMQ interface.
///
/// ## Examples
/// ```
/// use chainhook_sdk::observer::EventObserverConfig;
/// use chainhook_sdk::observer::BitcoinEventObserverConfigBuilder;
///
/// fn get_config() -> Result<EventObserverConfig, String> {
///     BitcoinEventObserverConfigBuilder::new()
///         .rpc_password("my_password")
///         .network("mainnet")
///         .finish()
/// }
/// ```
pub struct BitcoinEventObserverConfigBuilder {
    pub bitcoind_rpc_username: Option<String>,
    pub bitcoind_rpc_password: Option<String>,
    pub bitcoind_rpc_url: Option<String>,
    pub bitcoin_network: Option<String>,
    pub bitcoind_zmq_url: Option<String>,
    pub prometheus_monitoring_port: Option<u16>,
}
impl Default for BitcoinEventObserverConfigBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl BitcoinEventObserverConfigBuilder {
    pub fn new() -> Self {
        BitcoinEventObserverConfigBuilder {
            bitcoind_rpc_username: None,
            bitcoind_rpc_password: None,
            bitcoind_rpc_url: None,
            bitcoin_network: None,
            bitcoind_zmq_url: None,
            prometheus_monitoring_port: None,
        }
    }

    /// Sets the bitcoind node's RPC username.
    pub fn rpc_username(&mut self, username: &str) -> &mut Self {
        self.bitcoind_rpc_username = Some(username.to_string());
        self
    }

    /// Sets the bitcoind node's RPC password.
    pub fn rpc_password(&mut self, password: &str) -> &mut Self {
        self.bitcoind_rpc_password = Some(password.to_string());
        self
    }

    /// Sets the bitcoind node's RPC url.
    pub fn rpc_url(&mut self, url: &str) -> &mut Self {
        self.bitcoind_rpc_url = Some(url.to_string());
        self
    }

    /// Sets the bitcoind node's ZMQ url, used by the observer to receive new block events from bitcoind.
    pub fn zmq_url(&mut self, url: &str) -> &mut Self {
        self.bitcoind_zmq_url = Some(url.to_string());
        self
    }

    /// Sets the Bitcoin network. Must be a valid bitcoin network string according to [BitcoinNetwork::from_str].
    pub fn network(&mut self, network: &str) -> &mut Self {
        self.bitcoin_network = Some(network.to_string());
        self
    }

    /// Sets the Prometheus monitoring port.
    pub fn prometheus_monitoring_port(&mut self, port: u16) -> &mut Self {
        self.prometheus_monitoring_port = Some(port);
        self
    }

    /// Attempts to convert a [BitcoinEventObserverConfigBuilder] instance into an [EventObserverConfig], filling in
    /// defaults as necessary according to [EventObserverConfig::default].
    ///
    /// This function will return an error if the `bitcoin_network` string is set and is not a valid [BitcoinNetwork].
    pub fn finish(&self) -> Result<EventObserverConfig, String> {
        let bitcoin_network = if let Some(network) = self.bitcoin_network.as_ref() {
            BitcoinNetwork::try_from(network.as_str())?
        } else {
            BitcoinNetwork::Regtest
        };
        Ok(EventObserverConfig {
            registered_chainhooks: ChainhookStore::new(),
            predicates_config: PredicatesConfig {
                payload_http_request_timeout_ms: None,
            },
            bitcoin_rpc_proxy_enabled: false,
            bitcoind_rpc_username: self
                .bitcoind_rpc_username
                .clone()
                .unwrap_or_else(|| "devnet".into()),
            bitcoind_rpc_password: self
                .bitcoind_rpc_password
                .clone()
                .unwrap_or_else(|| "devnet".into()),
            bitcoind_rpc_url: self
                .bitcoind_rpc_url
                .clone()
                .unwrap_or_else(|| "http://localhost:18443".into()),
            bitcoin_block_signaling: BitcoinBlockSignaling::ZeroMQ(
                self.bitcoind_zmq_url
                    .clone()
                    .unwrap_or_else(|| "tcp://0.0.0.0:18543".into()),
            ),
            display_stacks_ingestion_logs: false,
            bitcoin_network,
            stacks_network: StacksNetwork::Devnet,
            prometheus_monitoring_port: self.prometheus_monitoring_port,
        })
    }
}

impl Default for EventObserverConfig {
    fn default() -> Self {
        Self {
            registered_chainhooks: ChainhookStore::new(),
            predicates_config: PredicatesConfig {
                payload_http_request_timeout_ms: None,
            },
            bitcoin_rpc_proxy_enabled: false,
            bitcoind_rpc_username: "devnet".into(),
            bitcoind_rpc_password: "devnet".into(),
            bitcoind_rpc_url: "http://localhost:18443".into(),
            bitcoin_block_signaling: BitcoinBlockSignaling::Stacks(StacksNodeConfig::new(
                DEFAULT_STACKS_NODE_RPC.to_string(),
                DEFAULT_INGESTION_PORT,
            )),
            display_stacks_ingestion_logs: false,
            bitcoin_network: BitcoinNetwork::Regtest,
            stacks_network: StacksNetwork::Devnet,
            prometheus_monitoring_port: None,
        }
    }
}

impl EventObserverConfig {
    /// Adds a [ChainhookInstance] to config's the registered chainhook store, returning the updated config.
    pub fn register_chainhook_instance(
        &mut self,
        spec: ChainhookInstance,
    ) -> Result<&mut Self, String> {
        let mut chainhook_config = ChainhookStore::new();
        chainhook_config.register_instance(spec)?;
        self.registered_chainhooks = chainhook_config;

        Ok(self)
    }

    /// Adds a [BitcoinChainhookInstance] to the config's registered chainhook store, returning the updated config.
    pub fn register_bitcoin_chainhook_instance(
        &mut self,
        spec: BitcoinChainhookInstance,
    ) -> Result<&mut Self, String> {
        self.register_chainhook_instance(ChainhookInstance::Bitcoin(spec))
    }
    /// Adds a [StacksChainhookInstance] to the config's registered chainhook store, returning the updated config.
    pub fn register_stacks_chainhook_instance(
        &mut self,
        spec: StacksChainhookInstance,
    ) -> Result<&mut Self, String> {
        self.register_chainhook_instance(ChainhookInstance::Stacks(spec))
    }

    pub fn get_bitcoin_config(&self) -> BitcoinConfig {
        BitcoinConfig {
            username: self.bitcoind_rpc_username.clone(),
            password: self.bitcoind_rpc_password.clone(),
            rpc_url: self.bitcoind_rpc_url.clone(),
            network: self.bitcoin_network.clone(),
            bitcoin_block_signaling: self.bitcoin_block_signaling.clone(),
        }
    }

    pub fn get_stacks_node_config(&self) -> &StacksNodeConfig {
        match self.bitcoin_block_signaling {
            BitcoinBlockSignaling::Stacks(ref config) => config,
            _ => unreachable!(),
        }
    }

    /// Helper to allow overriding some default fields in creating a new EventObserverConfig.
    ///
    /// *Note: This is used by external crates, so it should not be removed, even if not used internally by Chainhook.*
    pub fn new_using_overrides(
        overrides: Option<&EventObserverConfigBuilder>,
    ) -> Result<EventObserverConfig, String> {
        let bitcoin_network =
            if let Some(network) = overrides.and_then(|c| c.bitcoin_network.as_ref()) {
                BitcoinNetwork::try_from(network.as_str())?
            } else {
                BitcoinNetwork::Regtest
            };

        let stacks_network =
            if let Some(network) = overrides.and_then(|c| c.stacks_network.as_ref()) {
                StacksNetwork::try_from(network.as_str())?
            } else {
                StacksNetwork::Devnet
            };

        let config = EventObserverConfig {
            bitcoin_rpc_proxy_enabled: false,
            registered_chainhooks: ChainhookStore::new(),
            predicates_config: PredicatesConfig {
                payload_http_request_timeout_ms: None,
            },
            bitcoind_rpc_username: overrides
                .and_then(|c| c.bitcoind_rpc_username.clone())
                .unwrap_or_else(|| "devnet".to_string()),
            bitcoind_rpc_password: overrides
                .and_then(|c| c.bitcoind_rpc_password.clone())
                .unwrap_or_else(|| "devnet".to_string()),
            bitcoind_rpc_url: overrides
                .and_then(|c| c.bitcoind_rpc_url.clone())
                .unwrap_or_else(|| "http://localhost:18443".to_string()),
            bitcoin_block_signaling: overrides
                .and_then(|c| c.bitcoind_zmq_url.as_ref())
                .map(|url| BitcoinBlockSignaling::ZeroMQ(url.clone()))
                .unwrap_or_else(|| {
                    BitcoinBlockSignaling::Stacks(StacksNodeConfig::new(
                        overrides
                            .and_then(|c| c.stacks_node_rpc_url.clone())
                            .unwrap_or_else(|| DEFAULT_STACKS_NODE_RPC.to_string()),
                        overrides
                            .and_then(|c| c.chainhook_stacks_block_ingestion_port)
                            .unwrap_or(DEFAULT_INGESTION_PORT),
                    ))
                }),
            display_stacks_ingestion_logs: overrides
                .and_then(|c| c.display_stacks_ingestion_logs)
                .unwrap_or(false),
            bitcoin_network,
            stacks_network,
            prometheus_monitoring_port: overrides.and_then(|c| c.prometheus_monitoring_port),
        };
        Ok(config)
    }
}

#[derive(Deserialize, Debug)]
pub struct ContractReadonlyCall {
    pub okay: bool,
    pub result: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ObserverCommand {
    ProcessBitcoinBlock(BitcoinBlockFullBreakdown),
    CacheBitcoinBlock(BitcoinBlockData),
    PropagateBitcoinChainEvent(BlockchainEvent),
    PropagateStacksChainEvent(StacksChainEvent),
    PropagateStacksMempoolEvent(StacksChainMempoolEvent),
    RegisterPredicate(ChainhookSpecificationNetworkMap),
    EnablePredicate(ChainhookInstance),
    DeregisterBitcoinPredicate(String),
    DeregisterStacksPredicate(String),
    ExpireBitcoinPredicate(HookExpirationData),
    ExpireStacksPredicate(HookExpirationData),
    NotifyBitcoinTransactionProxied,
    Terminate,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HookExpirationData {
    pub hook_uuid: String,
    pub block_height: u64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StacksChainMempoolEvent {
    TransactionsAdmitted(Vec<MempoolAdmissionData>),
    TransactionDropped(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct MempoolAdmissionData {
    pub tx_data: String,
    pub tx_description: String,
}

#[derive(Clone, Debug)]
pub struct PredicateEvaluationReport {
    pub predicates_evaluated: BTreeMap<String, BTreeSet<BlockIdentifier>>,
    pub predicates_triggered: BTreeMap<String, BTreeSet<BlockIdentifier>>,
    pub predicates_expired: BTreeMap<String, BTreeSet<BlockIdentifier>>,
}

impl Default for PredicateEvaluationReport {
    fn default() -> Self {
        Self::new()
    }
}

impl PredicateEvaluationReport {
    pub fn new() -> PredicateEvaluationReport {
        PredicateEvaluationReport {
            predicates_evaluated: BTreeMap::new(),
            predicates_triggered: BTreeMap::new(),
            predicates_expired: BTreeMap::new(),
        }
    }

    pub fn track_evaluation(&mut self, uuid: &str, block_identifier: &BlockIdentifier) {
        self.predicates_evaluated
            .entry(uuid.to_string())
            .and_modify(|e| {
                e.insert(block_identifier.clone());
            })
            .or_insert_with(|| {
                let mut set = BTreeSet::new();
                set.insert(block_identifier.clone());
                set
            });
    }

    pub fn track_trigger(&mut self, uuid: &str, blocks: &Vec<&BlockIdentifier>) {
        for block_id in blocks.iter() {
            self.predicates_triggered
                .entry(uuid.to_string())
                .and_modify(|e| {
                    e.insert((*block_id).clone());
                })
                .or_insert_with(|| {
                    let mut set = BTreeSet::new();
                    set.insert((*block_id).clone());
                    set
                });
        }
    }

    pub fn track_expiration(&mut self, uuid: &str, block_identifier: &BlockIdentifier) {
        self.predicates_expired
            .entry(uuid.to_string())
            .and_modify(|e| {
                e.insert(block_identifier.clone());
            })
            .or_insert_with(|| {
                let mut set = BTreeSet::new();
                set.insert(block_identifier.clone());
                set
            });
    }
}

#[derive(Clone, Debug)]
pub struct PredicateInterruptedData {
    pub predicate_key: String,
    pub error: String,
}

#[derive(Clone, Debug)]
pub enum ObserverEvent {
    Error(String),
    Fatal(String),
    Info(String),
    BitcoinChainEvent((BitcoinChainEvent, PredicateEvaluationReport)),
    StacksChainEvent((StacksChainEvent, PredicateEvaluationReport)),
    NotifyBitcoinTransactionProxied,
    PredicateRegistered(ChainhookInstance),
    PredicateDeregistered(PredicateDeregisteredEvent),
    PredicateEnabled(ChainhookInstance),
    BitcoinPredicateTriggered(BitcoinChainhookOccurrencePayload),
    StacksPredicateTriggered(StacksChainhookOccurrencePayload),
    PredicatesTriggered(usize),
    PredicateInterrupted(PredicateInterruptedData),
    Terminate,
    StacksChainMempoolEvent(StacksChainMempoolEvent),
}

#[derive(Clone, Debug)]
pub struct PredicateDeregisteredEvent {
    pub predicate_uuid: String,
    pub chain: Chain,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
/// JSONRPC Request
pub struct BitcoinRPCRequest {
    /// The name of the RPC call
    pub method: String,
    /// Parameters to the RPC call
    pub params: serde_json::Value,
    /// Identifier for this Request, which should appear in the response
    pub id: serde_json::Value,
    /// jsonrpc field, MUST be "2.0"
    pub jsonrpc: serde_json::Value,
}

#[derive(Debug, Clone)]
pub struct BitcoinConfig {
    pub username: String,
    pub password: String,
    pub rpc_url: String,
    pub network: BitcoinNetwork,
    pub bitcoin_block_signaling: BitcoinBlockSignaling,
}

#[derive(Debug, Clone)]
pub struct BitcoinBlockDataCached {
    pub block: BitcoinBlockData,
    pub processed_by_sidecar: bool,
}

pub struct ObserverSidecar {
    pub bitcoin_blocks_mutator: Option<(
        crossbeam_channel::Sender<(Vec<BitcoinBlockDataCached>, Vec<BlockIdentifier>)>,
        crossbeam_channel::Receiver<Vec<BitcoinBlockDataCached>>,
    )>,
    pub bitcoin_chain_event_notifier: Option<crossbeam_channel::Sender<HandleBlock>>,
}

#[derive(Debug, Clone, Default)]
pub struct StacksObserverStartupContext {
    pub block_pool_seed: Vec<StacksBlockData>,
    pub last_block_height_appended: u64,
}

impl ObserverSidecar {
    fn perform_bitcoin_sidecar_mutations(
        &self,
        blocks: Vec<BitcoinBlockDataCached>,
        blocks_ids_to_rollback: Vec<BlockIdentifier>,
        ctx: &Context,
    ) -> Vec<BitcoinBlockDataCached> {
        if let Some(ref block_mutator) = self.bitcoin_blocks_mutator {
            ctx.try_log(|logger| slog::info!(logger, "Sending blocks to pre-processor",));
            let _ = block_mutator
                .0
                .send((blocks.clone(), blocks_ids_to_rollback));
            ctx.try_log(|logger| slog::info!(logger, "Waiting for blocks from pre-processor",));
            match block_mutator.1.recv() {
                Ok(updated_blocks) => {
                    ctx.try_log(|logger| slog::info!(logger, "Block received from pre-processor",));
                    updated_blocks
                }
                Err(e) => {
                    ctx.try_log(|logger| {
                        slog::error!(
                            logger,
                            "Unable to receive block from pre-processor {}",
                            e.to_string()
                        )
                    });
                    blocks
                }
            }
        } else {
            blocks
        }
    }

    fn notify_chain_event(&self, chain_event: &BitcoinChainEvent, _ctx: &Context) {
        if let Some(ref notifier) = self.bitcoin_chain_event_notifier {
            match chain_event {
                BitcoinChainEvent::ChainUpdatedWithBlocks(data) => {
                    for block in data.new_blocks.iter() {
                        let _ = notifier.send(HandleBlock::ApplyBlock(block.clone()));
                    }
                }
                BitcoinChainEvent::ChainUpdatedWithReorg(data) => {
                    for block in data.blocks_to_rollback.iter() {
                        let _ = notifier.send(HandleBlock::UndoBlock(block.clone()));
                    }
                    for block in data.blocks_to_apply.iter() {
                        let _ = notifier.send(HandleBlock::ApplyBlock(block.clone()));
                    }
                }
            }
        }
    }
}

/// A helper struct used to configure and call [start_event_observer], which spawns a thread to observer chain events.
///
/// ### Examples
/// ```
/// use chainhook_sdk::observer::EventObserverBuilder;
/// use chainhook_sdk::observer::EventObserverConfig;
/// use chainhook_sdk::observer::ObserverCommand;
/// use chainhook_sdk::utils::Context;
/// use std::error::Error;
/// use std::sync::mpsc::{Receiver, Sender};
///
/// fn start_event_observer(
///     config: EventObserverConfig,
///     observer_commands_tx: &Sender<ObserverCommand>,
///     observer_commands_rx: Receiver<ObserverCommand>,
///     ctx: &Context,
/// )-> Result<(), Box<dyn Error>> {
///     EventObserverBuilder::new(
///         config,
///         &observer_commands_tx,
///         observer_commands_rx,
///         &ctx
///     )
///     .start()
/// }
/// ```
pub struct EventObserverBuilder {
    config: EventObserverConfig,
    observer_commands_tx: Sender<ObserverCommand>,
    observer_commands_rx: Receiver<ObserverCommand>,
    ctx: Context,
    observer_events_tx: Option<crossbeam_channel::Sender<ObserverEvent>>,
    observer_sidecar: Option<ObserverSidecar>,
    stacks_startup_context: Option<StacksObserverStartupContext>,
}

impl EventObserverBuilder {
    pub fn new(
        config: EventObserverConfig,
        observer_commands_tx: &Sender<ObserverCommand>,
        observer_commands_rx: Receiver<ObserverCommand>,
        ctx: &Context,
    ) -> Self {
        EventObserverBuilder {
            config,
            observer_commands_tx: observer_commands_tx.clone(),
            observer_commands_rx,
            ctx: ctx.clone(),
            observer_events_tx: None,
            observer_sidecar: None,
            stacks_startup_context: None,
        }
    }

    /// Sets the `observer_events_tx` Sender. Set this and listen on the corresponding
    /// Receiver to be notified of every [ObserverEvent].
    pub fn events_tx(
        &mut self,
        observer_events_tx: crossbeam_channel::Sender<ObserverEvent>,
    ) -> &mut Self {
        self.observer_events_tx = Some(observer_events_tx);
        self
    }

    /// Sets a sidecar for the observer. See [ObserverSidecar].
    pub fn sidecar(&mut self, sidecar: ObserverSidecar) -> &mut Self {
        self.observer_sidecar = Some(sidecar);
        self
    }

    /// Sets the Stacks startup context. See [StacksObserverStartupContext].
    pub fn stacks_startup_context(&mut self, context: StacksObserverStartupContext) -> &mut Self {
        self.stacks_startup_context = Some(context);
        self
    }

    /// Starts the event observer, calling [start_event_observer]. This function consumes the
    /// [EventObserverBuilder] and spawns a new thread to run the observer.
    pub fn start(self) -> Result<(), Box<dyn Error>> {
        start_event_observer(
            self.config,
            self.observer_commands_tx,
            self.observer_commands_rx,
            self.observer_events_tx,
            self.observer_sidecar,
            self.stacks_startup_context,
            self.ctx,
        )
    }
}

/// Spawns a thread to observe blockchain events. Use [EventObserverBuilder] to configure easily.
pub fn start_event_observer(
    config: EventObserverConfig,
    observer_commands_tx: Sender<ObserverCommand>,
    observer_commands_rx: Receiver<ObserverCommand>,
    observer_events_tx: Option<crossbeam_channel::Sender<ObserverEvent>>,
    observer_sidecar: Option<ObserverSidecar>,
    stacks_startup_context: Option<StacksObserverStartupContext>,
    ctx: Context,
) -> Result<(), Box<dyn Error>> {
    match config.bitcoin_block_signaling {
        BitcoinBlockSignaling::ZeroMQ(ref url) => {
            ctx.try_log(|logger| {
                slog::info!(logger, "Observing Bitcoin chain events via ZeroMQ: {}", url)
            });
            let context_cloned = ctx.clone();
            let event_observer_config_moved = config.clone();
            let observer_commands_tx_moved = observer_commands_tx.clone();
            let _ = std::thread::Builder::new()
                .name("Chainhook event observer".to_string())
                .spawn(move || {
                    let rt = tokio::runtime::Runtime::new().unwrap();
                    rt.block_on(start_bitcoin_event_observer(
                        event_observer_config_moved,
                        observer_commands_tx_moved,
                        observer_commands_rx,
                        observer_events_tx.clone(),
                        observer_sidecar,
                        context_cloned.clone(),
                    ))
                    .unwrap_or_else(|e| {
                        if let Some(tx) = observer_events_tx {
                            context_cloned.try_log(|logger| {
                                slog::crit!(
                                    logger,
                                    "Chainhook event observer thread failed with error: {e}",
                                )
                            });
                            let _ = tx.send(ObserverEvent::Terminate);
                        }
                    });
                })
                .expect("unable to spawn thread");
        }
        BitcoinBlockSignaling::Stacks(ref _url) => {
            // Start chainhook event observer
            let context_cloned = ctx.clone();
            let event_observer_config_moved = config.clone();
            let observer_commands_tx_moved = observer_commands_tx.clone();

            let _ = std::thread::Builder::new()
                .name("Chainhook event observer".to_string())
                .spawn(move || {
                    let rt = tokio::runtime::Runtime::new().unwrap();
                    rt.block_on(start_stacks_event_observer(
                        event_observer_config_moved,
                        observer_commands_tx_moved,
                        observer_commands_rx,
                        observer_events_tx.clone(),
                        observer_sidecar,
                        stacks_startup_context.unwrap_or_default(),
                        context_cloned.clone(),
                    ))
                    .unwrap_or_else(|e| {
                        if let Some(tx) = observer_events_tx {
                            context_cloned.try_log(|logger| {
                                slog::crit!(
                                    logger,
                                    "Chainhook event observer thread failed with error: {e}",
                                )
                            });
                            let _ = tx.send(ObserverEvent::Terminate);
                        }
                    });
                })
                .expect("unable to spawn thread");

            ctx.try_log(|logger| {
                slog::info!(
                    logger,
                    "Listening on port {} for Stacks chain events",
                    config.get_stacks_node_config().ingestion_port
                )
            });

            ctx.try_log(|logger| {
                slog::info!(logger, "Observing Bitcoin chain events via Stacks node")
            });
        }
    }
    Ok(())
}

pub async fn start_bitcoin_event_observer(
    config: EventObserverConfig,
    _observer_commands_tx: Sender<ObserverCommand>,
    observer_commands_rx: Receiver<ObserverCommand>,
    observer_events_tx: Option<crossbeam_channel::Sender<ObserverEvent>>,
    observer_sidecar: Option<ObserverSidecar>,
    ctx: Context,
) -> Result<(), Box<dyn Error>> {
    let chainhook_store = config.registered_chainhooks.clone();

    // This loop is used for handling background jobs, emitted by HTTP calls.
    start_observer_commands_handler(
        config,
        chainhook_store,
        observer_commands_rx,
        observer_events_tx,
        None,
        observer_sidecar,
        ctx,
    )
    .await
}

pub async fn start_stacks_event_observer(
    config: EventObserverConfig,
    observer_commands_tx: Sender<ObserverCommand>,
    observer_commands_rx: Receiver<ObserverCommand>,
    observer_events_tx: Option<crossbeam_channel::Sender<ObserverEvent>>,
    observer_sidecar: Option<ObserverSidecar>,
    stacks_startup_context: StacksObserverStartupContext,
    ctx: Context,
) -> Result<(), Box<dyn Error>> {
    let indexer_config = IndexerConfig {
        bitcoind_rpc_url: config.bitcoind_rpc_url.clone(),
        bitcoind_rpc_username: config.bitcoind_rpc_username.clone(),
        bitcoind_rpc_password: config.bitcoind_rpc_password.clone(),
        stacks_network: StacksNetwork::Devnet,
        bitcoin_network: BitcoinNetwork::Regtest,
        bitcoin_block_signaling: config.bitcoin_block_signaling.clone(),
    };

    let mut indexer = Indexer::new(indexer_config.clone());

    indexer.seed_stacks_block_pool(stacks_startup_context.block_pool_seed, &ctx);

    let _log_level = if config.display_stacks_ingestion_logs {
        LogLevel::Debug
    } else {
        LogLevel::Off
    };

    let ingestion_port = config.get_stacks_node_config().ingestion_port;
    let bitcoin_rpc_proxy_enabled = config.bitcoin_rpc_proxy_enabled;
    let bitcoin_config = config.get_bitcoin_config();

    let chainhook_store = config.registered_chainhooks.clone();

    let indexer_rw_lock = Arc::new(RwLock::new(indexer));

    let background_job_tx_mutex = Arc::new(Mutex::new(observer_commands_tx.clone()));

    // Create Axum router
    let ctx_cloned = ctx.clone();
    let app = http::create_router(
        indexer_rw_lock,
        background_job_tx_mutex,
        bitcoin_config,
        ctx_cloned,
        bitcoin_rpc_proxy_enabled,
    );

    // Create socket address
    let addr = SocketAddr::from(([0, 0, 0, 0], ingestion_port));

    // Create a tokio handle for shutdown
    let (shutdown_tx, shutdown_rx) = tokio::sync::oneshot::channel::<()>();
    let ingestion_shutdown = Some(Shutdown(shutdown_tx));

    // Spawn the server
    let _server = tokio::spawn(async move {
        let listener = match tokio::net::TcpListener::bind(&addr).await {
            Ok(listener) => listener,
            Err(e) => {
                eprintln!("Failed to bind to address {}: {}", addr, e);
                return;
            }
        };

        if let Err(e) = axum::serve(listener, app.into_make_service())
            .with_graceful_shutdown(async {
                let _ = shutdown_rx.await;
            })
            .await
        {
            eprintln!("Server error: {}", e);
        }
    });

    // This loop is used for handling background jobs, emitted by HTTP calls.
    start_observer_commands_handler(
        config,
        chainhook_store,
        observer_commands_rx,
        observer_events_tx,
        ingestion_shutdown,
        observer_sidecar,
        ctx,
    )
    .await
}

pub fn get_bitcoin_proof(
    bitcoin_client_rpc: &Client,
    transaction_identifier: &TransactionIdentifier,
    block_identifier: &BlockIdentifier,
) -> Result<String, String> {
    let txid =
        Txid::from_str(transaction_identifier.get_hash_bytes_str()).expect("unable to build txid");
    let block_hash =
        BlockHash::from_str(&block_identifier.hash[2..]).expect("unable to build block_hash");

    let res = bitcoin_client_rpc.get_tx_out_proof(&[txid], Some(&block_hash));
    match res {
        Ok(proof) => Ok(format!("0x{}", hex::encode(&proof))),
        Err(e) => Err(format!(
            "failed collecting proof for transaction {}: {}",
            transaction_identifier.hash, e
        )),
    }
}

pub fn gather_proofs<'a>(
    trigger: &BitcoinTriggerChainhook<'a>,
    proofs: &mut HashMap<&'a TransactionIdentifier, String>,
    config: &EventObserverConfig,
    ctx: &Context,
) {
    let bitcoin_client_rpc = Client::new(
        &config.bitcoind_rpc_url,
        Auth::UserPass(
            config.bitcoind_rpc_username.to_string(),
            config.bitcoind_rpc_password.to_string(),
        ),
    )
    .expect("unable to build http client");

    for (transactions, block) in trigger.apply.iter() {
        for transaction in transactions.iter() {
            if !proofs.contains_key(&transaction.transaction_identifier) {
                ctx.try_log(|logger| {
                    slog::debug!(
                        logger,
                        "Collecting proof for transaction {}",
                        transaction.transaction_identifier.hash
                    )
                });
                match get_bitcoin_proof(
                    &bitcoin_client_rpc,
                    &transaction.transaction_identifier,
                    &block.block_identifier,
                ) {
                    Ok(proof) => {
                        proofs.insert(&transaction.transaction_identifier, proof);
                    }
                    Err(e) => {
                        ctx.try_log(|logger| slog::warn!(logger, "{e}"));
                    }
                }
            }
        }
    }
}

pub enum HandleBlock {
    ApplyBlock(BitcoinBlockData),
    UndoBlock(BitcoinBlockData),
}

pub async fn start_observer_commands_handler(
    config: EventObserverConfig,
    mut chainhook_store: ChainhookStore,
    observer_commands_rx: Receiver<ObserverCommand>,
    observer_events_tx: Option<crossbeam_channel::Sender<ObserverEvent>>,
    ingestion_shutdown: Option<Shutdown>,
    observer_sidecar: Option<ObserverSidecar>,
    ctx: Context,
) -> Result<(), Box<dyn Error>> {
    let mut chainhooks_occurrences_tracker: HashMap<String, u64> = HashMap::new();
    let networks = (&config.bitcoin_network, &config.stacks_network);
    let mut bitcoin_block_store: HashMap<BlockIdentifier, BitcoinBlockDataCached> = HashMap::new();
    let http_client = build_http_client();
    let store_update_required = observer_sidecar
        .as_ref()
        .and_then(|s| s.bitcoin_blocks_mutator.as_ref())
        .is_some();

    loop {
        let command = match observer_commands_rx.recv() {
            Ok(cmd) => cmd,
            Err(e) => {
                ctx.try_log(|logger| {
                    slog::crit!(logger, "Error: broken channel {}", e.to_string())
                });
                break;
            }
        };
        match command {
            ObserverCommand::Terminate => {
                break;
            }
            ObserverCommand::ProcessBitcoinBlock(mut block_data) => {
                let block_hash = block_data.hash.to_string();
                let mut attempts = 0;
                let max_attempts = 10;
                let block = loop {
                    match standardize_bitcoin_block(
                        block_data.clone(),
                        &config.bitcoin_network,
                        &ctx,
                    ) {
                        Ok(block) => break Some(block),
                        Err((e, refetch_block)) => {
                            attempts += 1;
                            if attempts > max_attempts {
                                break None;
                            }
                            ctx.try_log(|logger| {
                                slog::warn!(logger, "Error standardizing block: {}", e)
                            });
                            if refetch_block {
                                block_data = match download_and_parse_block_with_retry(
                                    &http_client,
                                    &block_hash,
                                    &config.get_bitcoin_config(),
                                    &ctx,
                                )
                                .await
                                {
                                    Ok(block) => block,
                                    Err(e) => {
                                        ctx.try_log(|logger| {
                                            slog::warn!(
                                                logger,
                                                "unable to download_and_parse_block: {}",
                                                e.to_string()
                                            )
                                        });
                                        continue;
                                    }
                                };
                            }
                        }
                    };
                };
                let Some(block) = block else {
                    ctx.try_log(|logger| {
                        slog::crit!(
                            logger,
                            "Could not process bitcoin block after {} attempts.",
                            attempts
                        )
                    });
                    break;
                };

                bitcoin_block_store.insert(
                    block.block_identifier.clone(),
                    BitcoinBlockDataCached {
                        block,
                        processed_by_sidecar: false,
                    },
                );
            }
            ObserverCommand::CacheBitcoinBlock(block) => {
                bitcoin_block_store.insert(
                    block.block_identifier.clone(),
                    BitcoinBlockDataCached {
                        block,
                        processed_by_sidecar: false,
                    },
                );
            }
            ObserverCommand::PropagateBitcoinChainEvent(blockchain_event) => {
                ctx.try_log(|logger| {
                    slog::info!(logger, "Handling PropagateBitcoinChainEvent command")
                });
                let mut confirmed_blocks = vec![];

                // Update Chain event before propagation
                let (chain_event, _new_tip) = match blockchain_event {
                    BlockchainEvent::BlockchainUpdatedWithHeaders(data) => {
                        let mut blocks_to_mutate = vec![];
                        let mut new_blocks = vec![];
                        let mut new_tip = 0;

                        for header in data.new_headers.iter() {
                            if header.block_identifier.index > new_tip {
                                new_tip = header.block_identifier.index;
                            }

                            if store_update_required {
                                let Some(block) =
                                    bitcoin_block_store.remove(&header.block_identifier)
                                else {
                                    continue;
                                };
                                blocks_to_mutate.push(block);
                            } else {
                                let Some(cache) = bitcoin_block_store.get(&header.block_identifier)
                                else {
                                    continue;
                                };
                                new_blocks.push(cache.block.clone());
                            };
                        }

                        if let Some(ref sidecar) = observer_sidecar {
                            let updated_blocks = sidecar.perform_bitcoin_sidecar_mutations(
                                blocks_to_mutate,
                                vec![],
                                &ctx,
                            );
                            for cache in updated_blocks.into_iter() {
                                bitcoin_block_store
                                    .insert(cache.block.block_identifier.clone(), cache.clone());
                                new_blocks.push(cache.block);
                            }
                        }

                        for header in data.confirmed_headers.iter() {
                            match bitcoin_block_store.remove(&header.block_identifier) {
                                Some(res) => {
                                    confirmed_blocks.push(res.block);
                                }
                                None => {
                                    ctx.try_log(|logger| {
                                        slog::error!(
                                            logger,
                                            "Unable to retrieve confirmed bitcoin block {}",
                                            header.block_identifier
                                        )
                                    });
                                }
                            }
                        }

                        (
                            BitcoinChainEvent::ChainUpdatedWithBlocks(
                                BitcoinChainUpdatedWithBlocksData {
                                    new_blocks,
                                    confirmed_blocks: confirmed_blocks.clone(),
                                },
                            ),
                            new_tip,
                        )
                    }
                    BlockchainEvent::BlockchainUpdatedWithReorg(data) => {
                        let mut blocks_to_rollback = vec![];

                        let mut blocks_to_mutate = vec![];
                        let mut blocks_to_apply = vec![];
                        let mut new_tip = 0;

                        for header in data.headers_to_apply.iter() {
                            if header.block_identifier.index > new_tip {
                                new_tip = header.block_identifier.index;
                            }

                            if store_update_required {
                                let Some(block) =
                                    bitcoin_block_store.remove(&header.block_identifier)
                                else {
                                    continue;
                                };
                                blocks_to_mutate.push(block);
                            } else {
                                let Some(cache) = bitcoin_block_store.get(&header.block_identifier)
                                else {
                                    continue;
                                };
                                blocks_to_apply.push(cache.block.clone());
                            };
                        }

                        let mut blocks_ids_to_rollback: Vec<BlockIdentifier> = vec![];

                        for header in data.headers_to_rollback.iter() {
                            match bitcoin_block_store.get(&header.block_identifier) {
                                Some(cache) => {
                                    blocks_ids_to_rollback.push(header.block_identifier.clone());
                                    blocks_to_rollback.push(cache.block.clone());
                                }
                                None => {
                                    ctx.try_log(|logger| {
                                        slog::error!(
                                            logger,
                                            "Unable to retrieve bitcoin block {}",
                                            header.block_identifier
                                        )
                                    });
                                }
                            }
                        }

                        if let Some(ref sidecar) = observer_sidecar {
                            let updated_blocks = sidecar.perform_bitcoin_sidecar_mutations(
                                blocks_to_mutate,
                                blocks_ids_to_rollback,
                                &ctx,
                            );
                            for cache in updated_blocks.into_iter() {
                                bitcoin_block_store
                                    .insert(cache.block.block_identifier.clone(), cache.clone());
                                blocks_to_apply.push(cache.block);
                            }
                        }

                        for header in data.confirmed_headers.iter() {
                            match bitcoin_block_store.remove(&header.block_identifier) {
                                Some(res) => {
                                    confirmed_blocks.push(res.block);
                                }
                                None => {
                                    ctx.try_log(|logger| {
                                        slog::error!(
                                            logger,
                                            "Unable to retrieve confirmed bitcoin block {}",
                                            header.block_identifier
                                        )
                                    });
                                }
                            }
                        }

                        (
                            BitcoinChainEvent::ChainUpdatedWithReorg(
                                BitcoinChainUpdatedWithReorgData {
                                    blocks_to_apply,
                                    blocks_to_rollback,
                                    confirmed_blocks: confirmed_blocks.clone(),
                                },
                            ),
                            new_tip,
                        )
                    }
                };

                if let Some(ref sidecar) = observer_sidecar {
                    sidecar.notify_chain_event(&chain_event, &ctx)
                }
                // process hooks
                let mut hooks_ids_to_deregister = vec![];
                let mut requests = vec![];
                let mut report = PredicateEvaluationReport::new();

                let bitcoin_chainhooks = chainhook_store
                    .bitcoin_chainhooks
                    .iter()
                    .filter(|p| p.enabled)
                    .filter(|p| p.expired_at.is_none())
                    .collect::<Vec<_>>();
                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "Evaluating {} bitcoin chainhooks registered",
                        bitcoin_chainhooks.len()
                    )
                });

                let (predicates_triggered, predicates_evaluated, predicates_expired) =
                    evaluate_bitcoin_chainhooks_on_chain_event(
                        &chain_event,
                        &bitcoin_chainhooks,
                        &ctx,
                    );

                for (uuid, block_identifier) in predicates_evaluated.into_iter() {
                    report.track_evaluation(uuid, block_identifier);
                }
                for (uuid, block_identifier) in predicates_expired.into_iter() {
                    report.track_expiration(uuid, block_identifier);
                }
                for entry in predicates_triggered.iter() {
                    let blocks_ids = entry
                        .apply
                        .iter()
                        .map(|e| &e.1.block_identifier)
                        .collect::<Vec<&BlockIdentifier>>();
                    report.track_trigger(&entry.chainhook.uuid, &blocks_ids);
                }

                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "{} bitcoin chainhooks positive evaluations",
                        predicates_triggered.len()
                    )
                });

                let mut chainhooks_to_trigger = vec![];

                for trigger in predicates_triggered.into_iter() {
                    let mut total_occurrences: u64 = *chainhooks_occurrences_tracker
                        .get(&trigger.chainhook.uuid)
                        .unwrap_or(&0);
                    // todo: this currently is only additive, and an occurrence means we match a chain event,
                    // rather than the number of blocks. Should we instead add to the total occurrences for
                    // every apply block, and subtract for every rollback? If we did this, we could set the
                    // status to `Expired` when we go above `expire_after_occurrence` occurrences, rather than
                    // deregistering
                    total_occurrences += 1;

                    let limit = trigger.chainhook.expire_after_occurrence.unwrap_or(0);
                    if limit == 0 || total_occurrences <= limit {
                        chainhooks_occurrences_tracker
                            .insert(trigger.chainhook.uuid.clone(), total_occurrences);
                        chainhooks_to_trigger.push(trigger);
                    } else {
                        hooks_ids_to_deregister.push(trigger.chainhook.uuid.clone());
                    }
                }

                let mut proofs = HashMap::new();
                for trigger in chainhooks_to_trigger.iter() {
                    if trigger.chainhook.include_proof {
                        gather_proofs(trigger, &mut proofs, &config, &ctx);
                    }
                }

                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "{} bitcoin chainhooks will be triggered",
                        chainhooks_to_trigger.len()
                    )
                });

                if let Some(ref tx) = observer_events_tx {
                    let _ = tx.send(ObserverEvent::PredicatesTriggered(
                        chainhooks_to_trigger.len(),
                    ));
                }
                for chainhook_to_trigger in chainhooks_to_trigger.into_iter() {
                    let predicate_uuid = &chainhook_to_trigger.chainhook.uuid;
                    match handle_bitcoin_hook_action(chainhook_to_trigger, &proofs, &config) {
                        Err(e) => {
                            // todo: we may want to set predicates that reach this branch as interrupted,
                            // but for now we will error to see if this problem occurs.
                            ctx.try_log(|logger| {
                                slog::error!(
                                    logger,
                                    "unable to handle action for predicate {}: {}",
                                    predicate_uuid,
                                    e
                                )
                            });
                        }
                        Ok(BitcoinChainhookOccurrence::Http(request, data)) => {
                            requests.push((request, data));
                        }
                        Ok(BitcoinChainhookOccurrence::File(_path, _bytes)) => {
                            ctx.try_log(|logger| {
                                slog::warn!(logger, "Writing to disk not supported in server mode")
                            })
                        }
                        Ok(BitcoinChainhookOccurrence::Data(payload)) => {
                            if let Some(ref tx) = observer_events_tx {
                                let _ = tx.send(ObserverEvent::BitcoinPredicateTriggered(payload));
                            }
                        }
                    }
                }
                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "{} bitcoin chainhooks to deregister",
                        hooks_ids_to_deregister.len()
                    )
                });

                for hook_uuid in hooks_ids_to_deregister.iter() {
                    if let Some(ref tx) = observer_events_tx {
                        let _ = tx.send(ObserverEvent::PredicateDeregistered(
                            PredicateDeregisteredEvent {
                                predicate_uuid: hook_uuid.clone(),
                                chain: Chain::Bitcoin,
                            },
                        ));
                    }
                }

                for (request, data) in requests.into_iter() {
                    match send_request(*request, 3, 1, &ctx).await {
                        Ok(_) => {
                            if let Some(ref tx) = observer_events_tx {
                                let _ = tx.send(ObserverEvent::BitcoinPredicateTriggered(data));
                            }
                        }
                        Err(e) => {
                            chainhook_store.deregister_bitcoin_hook(data.chainhook.uuid.clone());
                            if let Some(ref tx) = observer_events_tx {
                                let _ = tx.send(ObserverEvent::PredicateInterrupted(PredicateInterruptedData {
                                    predicate_key: ChainhookInstance::bitcoin_key(&data.chainhook.uuid),
                                    error: format!("Unable to evaluate predicate on Bitcoin chainstate: {}", e)
                                }));
                            }
                        }
                    }
                }

                if let Some(ref tx) = observer_events_tx {
                    let _ = tx.send(ObserverEvent::BitcoinChainEvent((chain_event, report)));
                }
            }
            ObserverCommand::PropagateStacksChainEvent(chain_event) => {
                ctx.try_log(|logger| {
                    slog::info!(logger, "Handling PropagateStacksChainEvent command")
                });
                let mut hooks_ids_to_deregister = vec![];
                let mut requests = vec![];
                let mut report = PredicateEvaluationReport::new();

                let stacks_chainhooks = chainhook_store
                    .stacks_chainhooks
                    .iter()
                    .filter(|p| p.enabled)
                    .filter(|p| p.expired_at.is_none())
                    .collect::<Vec<_>>();
                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "Evaluating {} stacks chainhooks registered",
                        stacks_chainhooks.len()
                    )
                });

                // track stacks chain metrics
                match &chain_event {
                    StacksChainEvent::ChainUpdatedWithBlocks(update) => {
                        match update
                            .new_blocks
                            .iter()
                            .max_by_key(|b| b.block.block_identifier.index)
                        {
                            Some(highest_tip_update) => {
                                highest_tip_update.block.block_identifier.index
                            }
                            None => 0,
                        }
                    }
                    StacksChainEvent::ChainUpdatedWithReorg(update) => {
                        match update
                            .blocks_to_apply
                            .iter()
                            .max_by_key(|b| b.block.block_identifier.index)
                        {
                            Some(highest_tip_update) => {
                                highest_tip_update.block.block_identifier.index
                            }
                            None => 0,
                        }
                    }
                    _ => 0,
                };

                // process hooks
                let (predicates_triggered, predicates_evaluated, predicates_expired) =
                    evaluate_stacks_chainhooks_on_chain_event(
                        &chain_event,
                        stacks_chainhooks,
                        &ctx,
                    );
                for (uuid, block_identifier) in predicates_evaluated.into_iter() {
                    report.track_evaluation(uuid, block_identifier);
                }
                for (uuid, block_identifier) in predicates_expired.into_iter() {
                    report.track_expiration(uuid, block_identifier);
                }
                for entry in predicates_triggered.iter() {
                    let mut block_ids = entry
                        .apply
                        .iter()
                        .map(|e| e.1.get_identifier())
                        .collect::<Vec<&BlockIdentifier>>();
                    let mut event_block_ids = entry
                        .events
                        .iter()
                        .map(|e| &e.received_at_block)
                        .collect::<Vec<&BlockIdentifier>>();
                    if !event_block_ids.is_empty() {
                        block_ids.append(&mut event_block_ids);
                    }
                    report.track_trigger(&entry.chainhook.uuid, &block_ids);
                }
                ctx.try_log(|logger| {
                    slog::info!(
                        logger,
                        "{} stacks chainhooks positive evaluations",
                        predicates_triggered.len()
                    )
                });

                let mut chainhooks_to_trigger = vec![];

                for trigger in predicates_triggered.into_iter() {
                    let mut total_occurrences: u64 = *chainhooks_occurrences_tracker
                        .get(&trigger.chainhook.uuid)
                        .unwrap_or(&0);
                    total_occurrences += 1;

                    let limit = trigger.chainhook.expire_after_occurrence.unwrap_or(0);
                    if limit == 0 || total_occurrences <= limit {
                        chainhooks_occurrences_tracker
                            .insert(trigger.chainhook.uuid.clone(), total_occurrences);
                        chainhooks_to_trigger.push(trigger);
                    } else {
                        hooks_ids_to_deregister.push(trigger.chainhook.uuid.clone());
                    }
                }

                if let Some(ref tx) = observer_events_tx {
                    let _ = tx.send(ObserverEvent::PredicatesTriggered(
                        chainhooks_to_trigger.len(),
                    ));
                }
                let proofs = HashMap::new();
                for chainhook_to_trigger in chainhooks_to_trigger.into_iter() {
                    let predicate_uuid = &chainhook_to_trigger.chainhook.uuid;
                    match handle_stacks_hook_action(chainhook_to_trigger, &proofs, &config, &ctx) {
                        Err(e) => {
                            ctx.try_log(|logger| {
                                // todo: we may want to set predicates that reach this branch as interrupted,
                                // but for now we will error to see if this problem occurs.
                                slog::error!(
                                    logger,
                                    "unable to handle action for predicate {}: {}",
                                    predicate_uuid,
                                    e
                                )
                            });
                        }
                        Ok(StacksChainhookOccurrence::Http(request, data)) => {
                            requests.push((request, data));
                        }
                        Ok(StacksChainhookOccurrence::File(_path, _bytes)) => {
                            ctx.try_log(|logger| {
                                slog::warn!(logger, "Writing to disk not supported in server mode")
                            })
                        }
                        Ok(StacksChainhookOccurrence::Data(payload)) => {
                            if let Some(ref tx) = observer_events_tx {
                                let _ = tx.send(ObserverEvent::StacksPredicateTriggered(payload));
                            }
                        }
                    }
                }

                for hook_uuid in hooks_ids_to_deregister.iter() {
                    if let Some(ref tx) = observer_events_tx {
                        let _ = tx.send(ObserverEvent::PredicateDeregistered(
                            PredicateDeregisteredEvent {
                                predicate_uuid: hook_uuid.clone(),
                                chain: Chain::Stacks,
                            },
                        ));
                    }
                }

                for (request, data) in requests.into_iter() {
                    // todo(lgalabru): collect responses for reporting
                    ctx.try_log(|logger| {
                        slog::debug!(
                            logger,
                            "Dispatching request from stacks chainhook {:?}",
                            request
                        )
                    });
                    match send_request(*request, 3, 1, &ctx).await {
                        Ok(_) => {
                            if let Some(ref tx) = observer_events_tx {
                                let _ = tx.send(ObserverEvent::StacksPredicateTriggered(data));
                            }
                        }
                        Err(e) => {
                            chainhook_store.deregister_stacks_hook(data.chainhook.uuid.clone());
                            if let Some(ref tx) = observer_events_tx {
                                let _ = tx.send(ObserverEvent::PredicateInterrupted(PredicateInterruptedData {
                                    predicate_key: ChainhookInstance::stacks_key(&data.chainhook.uuid),
                                    error: format!("Unable to evaluate predicate on Bitcoin chainstate: {}", e)
                                }));
                            }
                        }
                    };
                }

                if let Some(ref tx) = observer_events_tx {
                    let _ = tx.send(ObserverEvent::StacksChainEvent((chain_event, report)));
                }
            }
            ObserverCommand::PropagateStacksMempoolEvent(mempool_event) => {
                ctx.try_log(|logger| {
                    slog::debug!(logger, "Handling PropagateStacksMempoolEvent command")
                });
                if let Some(ref tx) = observer_events_tx {
                    let _ = tx.send(ObserverEvent::StacksChainMempoolEvent(mempool_event));
                }
            }
            ObserverCommand::NotifyBitcoinTransactionProxied => {
                ctx.try_log(|logger| {
                    slog::debug!(logger, "Handling NotifyBitcoinTransactionProxied command")
                });
                if let Some(ref tx) = observer_events_tx {
                    let _ = tx.send(ObserverEvent::NotifyBitcoinTransactionProxied);
                }
            }
            ObserverCommand::RegisterPredicate(spec) => {
                ctx.try_log(|logger| slog::info!(logger, "Handling RegisterPredicate command"));

                let mut spec =
                    match chainhook_store.register_instance_from_network_map(networks, spec) {
                        Ok(spec) => spec,
                        Err(e) => {
                            ctx.try_log(|logger| {
                                slog::warn!(
                                    logger,
                                    "Unable to register new chainhook spec: {}",
                                    e.to_string()
                                )
                            });
                            continue;
                        }
                    };

                ctx.try_log(
                    |logger| slog::debug!(logger, "Registering chainhook {}", spec.uuid(),),
                );
                if let Some(ref tx) = observer_events_tx {
                    let _ = tx.send(ObserverEvent::PredicateRegistered(spec.clone()));
                } else {
                    ctx.try_log(|logger| {
                        slog::debug!(logger, "Enabling Predicate {}", spec.uuid())
                    });
                    chainhook_store.enable_instance(&mut spec);
                }
            }
            ObserverCommand::EnablePredicate(mut spec) => {
                ctx.try_log(|logger| slog::info!(logger, "Enabling Predicate {}", spec.uuid()));
                chainhook_store.enable_instance(&mut spec);
                if let Some(ref tx) = observer_events_tx {
                    let _ = tx.send(ObserverEvent::PredicateEnabled(spec));
                }
            }
            ObserverCommand::DeregisterStacksPredicate(hook_uuid) => {
                ctx.try_log(|logger| {
                    slog::info!(logger, "Handling DeregisterStacksPredicate command")
                });
                let _hook = chainhook_store.deregister_stacks_hook(hook_uuid.clone());

                // event if the predicate wasn't in the `chainhook_store`, propogate this event to delete from redis
                if let Some(tx) = &observer_events_tx {
                    let _ = tx.send(ObserverEvent::PredicateDeregistered(
                        PredicateDeregisteredEvent {
                            predicate_uuid: hook_uuid,
                            chain: Chain::Stacks,
                        },
                    ));
                };
            }
            ObserverCommand::DeregisterBitcoinPredicate(hook_uuid) => {
                ctx.try_log(|logger| {
                    slog::info!(logger, "Handling DeregisterBitcoinPredicate command")
                });
                let _hook = chainhook_store.deregister_bitcoin_hook(hook_uuid.clone());

                // even if the predicate wasn't in the `chainhook_store`, propogate this event to delete from redis
                if let Some(tx) = &observer_events_tx {
                    let _ = tx.send(ObserverEvent::PredicateDeregistered(
                        PredicateDeregisteredEvent {
                            predicate_uuid: hook_uuid.clone(),
                            chain: Chain::Bitcoin,
                        },
                    ));
                };
            }
            ObserverCommand::ExpireStacksPredicate(HookExpirationData {
                hook_uuid,
                block_height,
            }) => {
                ctx.try_log(|logger| slog::info!(logger, "Handling ExpireStacksPredicate command"));
                chainhook_store.expire_stacks_hook(hook_uuid, block_height);
            }
            ObserverCommand::ExpireBitcoinPredicate(HookExpirationData {
                hook_uuid,
                block_height,
            }) => {
                ctx.try_log(|logger| {
                    slog::info!(logger, "Handling ExpireBitcoinPredicate command")
                });
                chainhook_store.expire_bitcoin_hook(hook_uuid, block_height);
            }
        }
    }
    terminate(ingestion_shutdown, observer_events_tx, &ctx);
    Ok(())
}

fn terminate(
    ingestion_shutdown: Option<Shutdown>,
    observer_events_tx: Option<crossbeam_channel::Sender<ObserverEvent>>,
    ctx: &Context,
) {
    ctx.try_log(|logger| slog::info!(logger, "Handling Termination command"));
    if let Some(ingestion_shutdown) = ingestion_shutdown {
        ingestion_shutdown.notify();
    }
    if let Some(ref tx) = observer_events_tx {
        let _ = tx.send(ObserverEvent::Info("Terminating event observer".into()));
        let _ = tx.send(ObserverEvent::Terminate);
    }
}
#[cfg(test)]
pub mod tests;
