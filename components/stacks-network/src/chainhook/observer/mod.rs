mod http;

use std::collections::HashMap;
use std::error::Error;
use std::net::SocketAddr;
use std::str;
use std::sync::mpsc::{Receiver, Sender};
use std::sync::{Arc, Mutex, RwLock};

use hiro_system_kit::slog;
use serde::{Deserialize, Serialize};

use crate::chainhook::indexer::bitcoin::{
    build_http_client, download_and_parse_block_with_retry, standardize_bitcoin_block,
    BitcoinBlockFullBreakdown,
};
use crate::chainhook::indexer::{Indexer, IndexerConfig};
use crate::chainhook::types::{
    BitcoinBlockData, BitcoinChainEvent, BitcoinChainUpdatedWithBlocksData,
    BitcoinChainUpdatedWithReorgData, BitcoinNetwork, BlockIdentifier, BlockchainEvent,
    StacksBlockData, StacksChainEvent, StacksNetwork, StacksNodeConfig, DEFAULT_STACKS_NODE_RPC,
};
use crate::chainhook::utils::Context;

pub struct Shutdown(tokio::sync::oneshot::Sender<()>);

impl Shutdown {
    pub fn notify(self) {
        let _ = self.0.send(());
    }
}

pub const DEFAULT_INGESTION_PORT: u16 = 20445;

#[derive(Debug, Clone)]
pub struct EventObserverConfig {
    pub bitcoin_rpc_proxy_enabled: bool,
    pub bitcoind_rpc_username: String,
    pub bitcoind_rpc_password: String,
    pub bitcoind_rpc_url: String,
    pub stacks_node_config: StacksNodeConfig,
    pub display_stacks_ingestion_logs: bool,
    pub bitcoin_network: BitcoinNetwork,
    pub stacks_network: StacksNetwork,
    pub prometheus_monitoring_port: Option<u16>,
}

impl Default for EventObserverConfig {
    fn default() -> Self {
        Self {
            bitcoin_rpc_proxy_enabled: false,
            bitcoind_rpc_username: "devnet".into(),
            bitcoind_rpc_password: "devnet".into(),
            bitcoind_rpc_url: "http://localhost:18443".into(),
            stacks_node_config: StacksNodeConfig::new(
                DEFAULT_STACKS_NODE_RPC.to_string(),
                DEFAULT_INGESTION_PORT,
            ),
            display_stacks_ingestion_logs: false,
            bitcoin_network: BitcoinNetwork::Regtest,
            stacks_network: StacksNetwork::Devnet,
            prometheus_monitoring_port: None,
        }
    }
}

impl EventObserverConfig {
    pub fn get_bitcoin_config(&self) -> BitcoinConfig {
        BitcoinConfig {
            username: self.bitcoind_rpc_username.clone(),
            password: self.bitcoind_rpc_password.clone(),
            rpc_url: self.bitcoind_rpc_url.clone(),
            network: self.bitcoin_network.clone(),
        }
    }

    pub fn get_stacks_node_config(&self) -> &StacksNodeConfig {
        &self.stacks_node_config
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
    NotifyBitcoinTransactionProxied,
    Terminate,
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
pub enum ObserverEvent {
    Error(String),
    Fatal(String),
    Info(String),
    BitcoinChainEvent(BitcoinChainEvent),
    StacksChainEvent(StacksChainEvent),
    NotifyBitcoinTransactionProxied,
    Terminate,
    StacksChainMempoolEvent(StacksChainMempoolEvent),
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
}

#[derive(Debug, Clone)]
pub struct BitcoinBlockDataCached {
    pub block: BitcoinBlockData,
}

#[derive(Debug, Clone, Default)]
pub struct StacksObserverStartupContext {
    pub block_pool_seed: Vec<StacksBlockData>,
    pub last_block_height_appended: u64,
}

/// Spawns a thread to observe blockchain events.
pub fn start_event_observer(
    config: EventObserverConfig,
    observer_commands_tx: Sender<ObserverCommand>,
    observer_commands_rx: Receiver<ObserverCommand>,
    observer_events_tx: Option<crossbeam_channel::Sender<ObserverEvent>>,
    stacks_startup_context: Option<StacksObserverStartupContext>,
    ctx: Context,
) -> Result<(), Box<dyn Error>> {
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

    Ok(())
}

pub async fn start_stacks_event_observer(
    config: EventObserverConfig,
    observer_commands_tx: Sender<ObserverCommand>,
    observer_commands_rx: Receiver<ObserverCommand>,
    observer_events_tx: Option<crossbeam_channel::Sender<ObserverEvent>>,
    stacks_startup_context: StacksObserverStartupContext,
    ctx: Context,
) -> Result<(), Box<dyn Error>> {
    let indexer_config = IndexerConfig {
        bitcoind_rpc_url: config.bitcoind_rpc_url.clone(),
        bitcoind_rpc_username: config.bitcoind_rpc_username.clone(),
        bitcoind_rpc_password: config.bitcoind_rpc_password.clone(),
        stacks_network: StacksNetwork::Devnet,
        bitcoin_network: BitcoinNetwork::Regtest,
        stacks_node_config: config.stacks_node_config.clone(),
    };

    let mut indexer = Indexer::new(indexer_config.clone());
    indexer.seed_stacks_block_pool(stacks_startup_context.block_pool_seed, &ctx);

    let ingestion_port = config.get_stacks_node_config().ingestion_port;
    let bitcoin_rpc_proxy_enabled = config.bitcoin_rpc_proxy_enabled;
    let bitcoin_config = config.get_bitcoin_config();

    let indexer_rw_lock = Arc::new(RwLock::new(indexer));
    let background_job_tx_mutex = Arc::new(Mutex::new(observer_commands_tx.clone()));

    let ctx_cloned = ctx.clone();
    let app = http::create_router(
        indexer_rw_lock,
        background_job_tx_mutex,
        bitcoin_config,
        ctx_cloned,
        bitcoin_rpc_proxy_enabled,
    );

    let addr = SocketAddr::from(([0, 0, 0, 0], ingestion_port));
    let (shutdown_tx, shutdown_rx) = tokio::sync::oneshot::channel::<()>();
    let ingestion_shutdown = Some(Shutdown(shutdown_tx));

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

    start_observer_commands_handler(
        config,
        observer_commands_rx,
        observer_events_tx,
        ingestion_shutdown,
        ctx,
    )
    .await
}

pub async fn start_observer_commands_handler(
    config: EventObserverConfig,
    observer_commands_rx: Receiver<ObserverCommand>,
    observer_events_tx: Option<crossbeam_channel::Sender<ObserverEvent>>,
    ingestion_shutdown: Option<Shutdown>,
    ctx: Context,
) -> Result<(), Box<dyn Error>> {
    let mut bitcoin_block_store: HashMap<BlockIdentifier, BitcoinBlockDataCached> = HashMap::new();
    let http_client = build_http_client();

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
                    BitcoinBlockDataCached { block },
                );
            }
            ObserverCommand::CacheBitcoinBlock(block) => {
                bitcoin_block_store.insert(
                    block.block_identifier.clone(),
                    BitcoinBlockDataCached { block },
                );
            }
            ObserverCommand::PropagateBitcoinChainEvent(blockchain_event) => {
                ctx.try_log(|logger| {
                    slog::info!(logger, "Handling PropagateBitcoinChainEvent command")
                });

                let chain_event = match blockchain_event {
                    BlockchainEvent::BlockchainUpdatedWithHeaders(data) => {
                        let mut new_blocks = vec![];
                        let mut confirmed_blocks = vec![];

                        for header in data.new_headers.iter() {
                            if let Some(cache) = bitcoin_block_store.get(&header.block_identifier) {
                                new_blocks.push(cache.block.clone());
                            }
                        }

                        for header in data.confirmed_headers.iter() {
                            if let Some(res) = bitcoin_block_store.remove(&header.block_identifier)
                            {
                                confirmed_blocks.push(res.block);
                            } else {
                                ctx.try_log(|logger| {
                                    slog::error!(
                                        logger,
                                        "Unable to retrieve confirmed bitcoin block {}",
                                        header.block_identifier
                                    )
                                });
                            }
                        }

                        BitcoinChainEvent::ChainUpdatedWithBlocks(
                            BitcoinChainUpdatedWithBlocksData {
                                new_blocks,
                                confirmed_blocks,
                            },
                        )
                    }
                    BlockchainEvent::BlockchainUpdatedWithReorg(data) => {
                        let mut blocks_to_rollback = vec![];
                        let mut blocks_to_apply = vec![];
                        let mut confirmed_blocks = vec![];

                        for header in data.headers_to_rollback.iter() {
                            if let Some(cache) = bitcoin_block_store.get(&header.block_identifier) {
                                blocks_to_rollback.push(cache.block.clone());
                            } else {
                                ctx.try_log(|logger| {
                                    slog::error!(
                                        logger,
                                        "Unable to retrieve bitcoin block {}",
                                        header.block_identifier
                                    )
                                });
                            }
                        }

                        for header in data.headers_to_apply.iter() {
                            if let Some(cache) = bitcoin_block_store.get(&header.block_identifier) {
                                blocks_to_apply.push(cache.block.clone());
                            }
                        }

                        for header in data.confirmed_headers.iter() {
                            if let Some(res) = bitcoin_block_store.remove(&header.block_identifier)
                            {
                                confirmed_blocks.push(res.block);
                            } else {
                                ctx.try_log(|logger| {
                                    slog::error!(
                                        logger,
                                        "Unable to retrieve confirmed bitcoin block {}",
                                        header.block_identifier
                                    )
                                });
                            }
                        }

                        BitcoinChainEvent::ChainUpdatedWithReorg(BitcoinChainUpdatedWithReorgData {
                            blocks_to_apply,
                            blocks_to_rollback,
                            confirmed_blocks,
                        })
                    }
                };

                if let Some(ref tx) = observer_events_tx {
                    let _ = tx.send(ObserverEvent::BitcoinChainEvent(chain_event));
                }
            }
            ObserverCommand::PropagateStacksChainEvent(chain_event) => {
                ctx.try_log(|logger| {
                    slog::info!(logger, "Handling PropagateStacksChainEvent command")
                });
                if let Some(ref tx) = observer_events_tx {
                    let _ = tx.send(ObserverEvent::StacksChainEvent(chain_event));
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
