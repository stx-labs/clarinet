use std::convert::TryFrom;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::Arc;
use std::time::Duration;
use std::{fs, str};

use base58::FromBase58;
use bitcoincore_rpc::bitcoin::Address;
use chainhook_sdk::chainhooks::types::ChainhookStore;
use chainhook_sdk::indexer::stacks::standardize_stacks_serialized_block;
use chainhook_sdk::indexer::StacksChainContext;
use chainhook_sdk::observer::{
    start_event_observer, EventObserverConfig, ObserverCommand, ObserverEvent, PredicatesConfig,
    StacksChainMempoolEvent, StacksObserverStartupContext,
};
use chainhook_sdk::types::{
    BitcoinBlockSignaling, BitcoinChainEvent, StacksChainEvent, StacksNodeConfig,
};
use chainhook_sdk::utils::Context;
use chainhook_types::{StacksBlockData, StacksTransactionKind};
use clarinet_deployments::onchain::{
    apply_on_chain_deployment, DeploymentCommand, DeploymentEvent, TransactionStatus,
};
use clarinet_deployments::types::DeploymentSpecification;
use clarinet_files::{
    self, AccountConfig, DevnetConfig, NetworkManifest, PoxStackingOrder, ProjectManifest,
    StacksNetwork, DEFAULT_FIRST_BURN_HEADER_HEIGHT,
};
use clarity::consts::CHAIN_ID_TESTNET;
use clarity::types::PublicKey;
use clarity::util::hash::{hex_bytes, Hash160};
use clarity::vm::types::{BuffData, PrincipalData, SequenceData, TupleData};
use clarity::vm::{ClarityName, Value as ClarityValue};
use hiro_system_kit::{self, slog, yellow};
use serde_json::json;
use stacks_common::address::AddressHashMode;
use stacks_common::types::chainstate::{StacksPrivateKey, StacksPublicKey};
use stacks_rpc_client::rpc_client::PoxInfo;
use stacks_rpc_client::StacksRpc;
use stackslib::chainstate::stacks::address::PoxAddress;
use stackslib::util_lib::signed_structured_data::pox4::{
    make_pox_4_signer_key_signature, Pox4SignatureTopic,
};

use super::ChainsCoordinatorCommand;
use crate::command::run_command;
use crate::event::{send_status_update, DevnetEvent, Status};
use crate::orchestrator::{
    copy_directory, get_global_snapshot_dir, get_project_snapshot_dir, ServicesMapHosts,
    EXCLUDED_STACKS_SNAPSHOT_FILES,
};

const SNAPSHOT_STACKS_START_HEIGHT: u64 = 38;
const SNAPSHOT_BURN_START_HEIGHT: u64 = 143;

#[derive(Deserialize)]
pub struct NewTransaction {
    pub txid: String,
    pub status: String,
    pub raw_result: String,
    pub raw_tx: String,
}

#[derive(Clone, Debug)]
pub struct DevnetEventObserverConfig {
    pub devnet_config: DevnetConfig,
    pub event_observer_config: EventObserverConfig,
    pub accounts: Vec<AccountConfig>,
    pub deployment: DeploymentSpecification,
    pub manifest: ProjectManifest,
    pub deployment_fee_rate: u64,
    pub services_map_hosts: ServicesMapHosts,
    pub network_manifest: NetworkManifest,
}

impl DevnetEventObserverConfig {
    pub fn consolidated_stacks_rpc_url(&self) -> String {
        format!("http://{}", self.services_map_hosts.stacks_node_host)
    }

    pub fn consolidated_bitcoin_rpc_url(&self) -> String {
        format!("http://{}", self.services_map_hosts.bitcoin_node_host)
    }

    pub fn get_deployer(&self) -> AccountConfig {
        self.accounts
            .iter()
            .find(|account| account.label == "deployer")
            .expect("deployer not found")
            .clone()
    }
}

#[derive(Clone, Debug)]
pub struct DevnetInitializationStatus {
    pub should_deploy_protocol: bool,
}

#[derive(Deserialize, Debug)]
pub struct ContractReadonlyCall {
    pub okay: bool,
    pub result: String,
}

#[derive(Debug)]
pub enum BitcoinMiningCommand {
    Start,
    Pause,
    Mine,
    InvalidateChainTip,
}

impl DevnetEventObserverConfig {
    pub fn new(
        devnet_config: DevnetConfig,
        manifest: ProjectManifest,
        network_manifest: Option<NetworkManifest>,
        deployment: DeploymentSpecification,
        chainhooks: ChainhookStore,
        ctx: &Context,
        services_map_hosts: ServicesMapHosts,
    ) -> Self {
        ctx.try_log(|logger| slog::info!(logger, "Checking contracts"));
        let network_manifest = match network_manifest {
            Some(n) => n,
            None => NetworkManifest::from_project_manifest_location(
                &manifest.location,
                &StacksNetwork::Devnet.get_networks(),
                false,
                Some(&manifest.project.cache_location),
                None,
            )
            .expect("unable to load network manifest"),
        };
        let event_observer_config = EventObserverConfig {
            bitcoin_rpc_proxy_enabled: true,
            registered_chainhooks: chainhooks,
            bitcoind_rpc_username: devnet_config.bitcoin_node_username.clone(),
            bitcoind_rpc_password: devnet_config.bitcoin_node_password.clone(),
            bitcoind_rpc_url: format!("http://{}", services_map_hosts.bitcoin_node_host),
            bitcoin_block_signaling: BitcoinBlockSignaling::Stacks(StacksNodeConfig {
                rpc_url: format!("http://{}", services_map_hosts.stacks_node_host),
                ingestion_port: devnet_config.orchestrator_ingestion_port,
            }),

            display_stacks_ingestion_logs: true,
            bitcoin_network: chainhook_types::BitcoinNetwork::Regtest,
            stacks_network: chainhook_types::StacksNetwork::Devnet,
            prometheus_monitoring_port: None,
            predicates_config: PredicatesConfig::default(),
        };

        DevnetEventObserverConfig {
            devnet_config,
            event_observer_config,
            accounts: network_manifest
                .accounts
                .clone()
                .into_values()
                .collect::<Vec<_>>(),
            manifest,
            deployment,
            deployment_fee_rate: network_manifest.network.deployment_fee_rate,
            services_map_hosts,
            network_manifest,
        }
    }
}
pub async fn start_chains_coordinator(
    config: DevnetEventObserverConfig,
    devnet_event_tx: Sender<DevnetEvent>,
    chains_coordinator_commands_rx: crossbeam_channel::Receiver<ChainsCoordinatorCommand>,
    _chains_coordinator_commands_tx: crossbeam_channel::Sender<ChainsCoordinatorCommand>,
    orchestrator_terminator_tx: Sender<bool>,
    observer_command_tx: Sender<ObserverCommand>,
    observer_command_rx: Receiver<ObserverCommand>,
    mining_command_tx: Sender<BitcoinMiningCommand>,
    mining_command_rx: Receiver<BitcoinMiningCommand>,
    using_snapshot: bool,
    create_new_snapshot: bool,
    ctx: Context,
) -> Result<(), String> {
    let mut should_deploy_protocol = true; // Will change when `stacks-network` components becomes compatible with Testnet / Mainnet setups
    let boot_completed = Arc::new(AtomicBool::new(false));
    let mut current_burn_height = if using_snapshot {
        SNAPSHOT_BURN_START_HEIGHT
    } else {
        0
    };
    let starting_block_height = if using_snapshot {
        SNAPSHOT_STACKS_START_HEIGHT
    } else {
        0
    };

    let global_snapshot_dir = get_global_snapshot_dir();
    let project_snapshot_dir = get_project_snapshot_dir(&config.devnet_config);
    // Ensure directories exist
    fs::create_dir_all(&global_snapshot_dir)
        .map_err(|e| format!("unable to create global snapshot directory: {e:?}"))?;
    fs::create_dir_all(&project_snapshot_dir)
        .map_err(|e| format!("unable to create project snapshot directory: {e:?}"))?;

    let (deployment_commands_tx, deployments_command_rx) = channel();
    let (deployment_events_tx, deployment_events_rx) = channel();

    // Set-up the background task in charge of serializing / signing / publishing the contracts.
    // This tasks can take several seconds to minutes, depending on the complexity of the project.
    // We start this process as soon as possible, as a background task.
    // This thread becomes dormant once the encoding is done, and proceed to the actual deployment once
    // the event DeploymentCommand::Start is received.
    perform_protocol_deployment(
        &config.network_manifest,
        &config.deployment,
        deployment_events_tx,
        deployments_command_rx,
        Some(config.consolidated_bitcoin_rpc_url()),
        Some(config.consolidated_stacks_rpc_url()),
    );

    // Set-up the background task in charge of monitoring contracts deployments.
    // This thread will be waiting and relaying events emitted by the thread above.
    relay_devnet_protocol_deployment(
        deployment_events_rx,
        &devnet_event_tx,
        Some(mining_command_tx.clone()),
        &boot_completed,
    );

    let chainhooks_count = config
        .event_observer_config
        .registered_chainhooks
        .stacks_chainhooks
        .len()
        + config
            .event_observer_config
            .registered_chainhooks
            .bitcoin_chainhooks
            .len();
    if chainhooks_count > 0 {
        devnet_event_tx
            .send(DevnetEvent::info(format!(
                "{chainhooks_count} chainhooks registered",
            )))
            .expect("Unable to terminate event observer");
    }

    // Spawn event observer
    let (observer_event_tx, observer_event_rx) = crossbeam_channel::unbounded();
    let event_observer_config = config.event_observer_config.clone();
    let observer_event_tx_moved = observer_event_tx.clone();
    let observer_command_tx_moved = observer_command_tx.clone();
    let ctx_moved = ctx.clone();

    let stacks_startup_context = if using_snapshot {
        // Load events from snapshot if available
        let event_pool: Vec<StacksBlockData> = {
            let mut events = vec![];
            let events_cache_path = get_global_snapshot_dir()
                .join("events_export")
                .join("events_cache.tsv");

            let mut chain_ctx = StacksChainContext::new(&chainhook_types::StacksNetwork::Devnet);
            if let Ok(file_content) = fs::read_to_string(&events_cache_path) {
                for line in file_content.lines() {
                    let parts: Vec<&str> = line.split('\t').collect();
                    if parts.get(2).unwrap_or(&"") == &"/new_block" {
                        let maybe_block = standardize_stacks_serialized_block(
                            &chainhook_sdk::indexer::IndexerConfig {
                                bitcoin_network: chainhook_types::BitcoinNetwork::Regtest,
                                stacks_network: chainhook_types::StacksNetwork::Devnet,
                                bitcoind_rpc_url: config
                                    .devnet_config
                                    .bitcoin_node_image_url
                                    .clone(),
                                bitcoind_rpc_username: config
                                    .devnet_config
                                    .bitcoin_node_username
                                    .clone(),
                                bitcoind_rpc_password: config
                                    .devnet_config
                                    .bitcoin_node_password
                                    .clone(),
                                bitcoin_block_signaling: BitcoinBlockSignaling::Stacks(
                                    StacksNodeConfig {
                                        rpc_url: config.devnet_config.stacks_node_image_url.clone(),
                                        ingestion_port: 3999,
                                    },
                                ),
                            },
                            parts.get(3).unwrap_or(&""),
                            &mut chain_ctx,
                            &ctx,
                        );
                        match maybe_block {
                            Ok(block) => {
                                events.push(block);
                            }
                            Err(e) => {
                                let _ =
                                    devnet_event_tx.send(DevnetEvent::debug(format!("Error: {e}")));
                            }
                        }
                    }
                }
            }
            events
        };
        Some(StacksObserverStartupContext {
            block_pool_seed: event_pool,
            last_block_height_appended: starting_block_height,
        })
    } else {
        None
    };
    let _ = hiro_system_kit::thread_named("Event observer").spawn(move || {
        let _ = start_event_observer(
            event_observer_config,
            observer_command_tx_moved,
            observer_command_rx,
            Some(observer_event_tx_moved),
            None,
            stacks_startup_context,
            ctx_moved,
        );
    });

    // Spawn bitcoin miner controller
    let devnet_event_tx_moved = devnet_event_tx.clone();
    let devnet_config = config.clone();
    let _ = hiro_system_kit::thread_named("Bitcoin mining").spawn(move || {
        let future =
            handle_bitcoin_mining(mining_command_rx, &devnet_config, &devnet_event_tx_moved);
        hiro_system_kit::nestable_block_on(future);
    });

    // Loop over events being received from Bitcoin and Stacks,
    // and orchestrate the 2 chains + protocol.
    let mut deployment_commands_tx = Some(deployment_commands_tx);

    let mut sel = crossbeam_channel::Select::new();
    let chains_coordinator_commands_oper = sel.recv(&chains_coordinator_commands_rx);
    let observer_event_oper = sel.recv(&observer_event_rx);

    let stacks_signers_keys = config.devnet_config.stacks_signers_keys.clone();

    loop {
        let oper = sel.select();
        let command = match oper.index() {
            i if i == chains_coordinator_commands_oper => {
                match oper.recv(&chains_coordinator_commands_rx) {
                    Ok(ChainsCoordinatorCommand::Terminate) => {
                        let _ = orchestrator_terminator_tx.send(true);
                        let _ = observer_command_tx.send(ObserverCommand::Terminate);
                        let _ = mining_command_tx.send(BitcoinMiningCommand::Pause);
                        break;
                    }
                    Err(_e) => {
                        continue;
                    }
                }
            }
            i if i == observer_event_oper => match oper.recv(&observer_event_rx) {
                Ok(cmd) => cmd,
                Err(_e) => {
                    continue;
                }
            },
            _ => unreachable!(),
        };

        match command {
            ObserverEvent::Fatal(msg) => {
                devnet_event_tx
                    .send(DevnetEvent::error(msg))
                    .expect("Unable to terminate event observer");
                // Terminate
            }
            ObserverEvent::PredicateInterrupted(_data) => {
                devnet_event_tx
                    .send(DevnetEvent::error("predicate interrupt".to_string())) // need to use data for the message
                    .expect("Event observer received predicate interrupt");
            }
            ObserverEvent::Error(msg) => {
                devnet_event_tx
                    .send(DevnetEvent::error(msg))
                    .expect("Unable to terminate event observer");
            }
            ObserverEvent::Info(msg) => {
                devnet_event_tx
                    .send(DevnetEvent::info(msg))
                    .expect("Unable to terminate event observer");
            }
            ObserverEvent::BitcoinChainEvent((chain_update, _)) => {
                // Contextual shortcut: Devnet is an environment under control,
                // with 1 miner. As such we will ignore Reorgs handling.
                let (log, comment) = match &chain_update {
                    BitcoinChainEvent::ChainUpdatedWithBlocks(event) => {
                        let tip = event.new_blocks.last().unwrap();
                        let bitcoin_block_height = tip.block_identifier.index;
                        current_burn_height = bitcoin_block_height;
                        let log = format!("Bitcoin block #{bitcoin_block_height} received");
                        let comment =
                            format!("mining blocks (chain_tip = #{bitcoin_block_height})");

                        // Check if we've reached the target height for database export (142)
                        // If we've reached epoch 3.0, create the global snapshot
                        if create_new_snapshot
                            && bitcoin_block_height == config.devnet_config.epoch_3_0
                        {
                            let _ = create_global_snapshot(
                                &config,
                                &devnet_event_tx,
                                mining_command_tx.clone(),
                            )
                            .await;
                        }
                        // Stacking orders can't be published until devnet is ready
                        if !stacks_signers_keys.is_empty()
                            && bitcoin_block_height >= DEFAULT_FIRST_BURN_HEADER_HEIGHT + 10
                        {
                            let res = publish_stacking_orders(
                                &config.devnet_config,
                                &devnet_event_tx,
                                &config.accounts,
                                &config.services_map_hosts,
                                config.deployment_fee_rate,
                                bitcoin_block_height as u32,
                            )
                            .await;
                            if let Some(tx_count) = res {
                                let _ = devnet_event_tx.send(DevnetEvent::success(format!(
                                    "Broadcasted {tx_count} stacking orders"
                                )));
                            }
                        }

                        (log, comment)
                    }
                    BitcoinChainEvent::ChainUpdatedWithReorg(events) => {
                        let tip = events.blocks_to_apply.last().unwrap();
                        let bitcoin_block_height = tip.block_identifier.index;
                        current_burn_height = bitcoin_block_height;
                        let log = format!(
                            "Bitcoin reorg received (new height: {})",
                            tip.block_identifier.index
                        );
                        let status = format!(
                            "mining blocks (chain_tip = #{})",
                            tip.block_identifier.index
                        );
                        (log, status)
                    }
                };

                let _ = devnet_event_tx.send(DevnetEvent::debug(log));

                send_status_update(
                    &devnet_event_tx,
                    &None,
                    "bitcoin-node",
                    Status::Green,
                    &comment,
                );
                let _ = devnet_event_tx.send(DevnetEvent::BitcoinChainEvent(chain_update.clone()));
            }
            ObserverEvent::StacksChainEvent((chain_event, _)) => {
                if should_deploy_protocol {
                    if let Some(block_identifier) = chain_event.get_latest_block_identifier() {
                        if block_identifier.index == starting_block_height {
                            should_deploy_protocol = false;
                            if let Some(deployment_commands_tx) = deployment_commands_tx.take() {
                                deployment_commands_tx
                                    .send(DeploymentCommand::Start)
                                    .map_err(|e| format!("unable to start deployment: {e}"))
                                    .unwrap();
                            }
                        }
                    }
                }

                let stacks_block_update = match &chain_event {
                    StacksChainEvent::ChainUpdatedWithBlocks(block) => {
                        match block.new_blocks.last() {
                            Some(block) => block.clone(),
                            None => unreachable!(),
                        }
                    }
                    StacksChainEvent::ChainUpdatedWithMicroblocks(_) => {
                        let _ = devnet_event_tx.send(DevnetEvent::StacksChainEvent(chain_event));
                        continue;
                        // TODO(lgalabru): good enough for now
                    }
                    StacksChainEvent::ChainUpdatedWithMicroblocksReorg(_) => {
                        unreachable!() // TODO(lgalabru): good enough for now - code path unreachable in the context of Devnet
                    }
                    StacksChainEvent::ChainUpdatedWithReorg(data) => {
                        // reorgs should not happen in devnet
                        // tests showed that it can happen in epoch 3.0 but should not
                        // this patch allows to handle it, but further investigation will be done
                        // with blockchain team in order to avoid this
                        devnet_event_tx
                            .send(DevnetEvent::warning("Stacks reorg received".to_string()))
                            .expect("Unable to send reorg event");
                        match data.blocks_to_apply.last() {
                            Some(block) => block.clone(),
                            None => unreachable!(),
                        }
                    }
                    StacksChainEvent::ChainUpdatedWithNonConsensusEvents(_) => {
                        continue;
                    }
                };

                // if the sbtc-deposit contract is detected, fund the accounts with sBTC
                stacks_block_update
                    .block
                    .transactions
                    .iter()
                    .for_each(|tx| {
                        if let StacksTransactionKind::ContractDeployment(data) = &tx.metadata.kind {
                            let contract_identifier = data.contract_identifier.clone();
                            let deployer = config.get_deployer();
                            if contract_identifier
                                == format!("{}.sbtc-deposit", deployer.stx_address)
                            {
                                fund_genesis_account(
                                    &devnet_event_tx,
                                    &config.services_map_hosts,
                                    &config.accounts,
                                    config.deployment_fee_rate,
                                    &boot_completed,
                                );
                            }
                        }
                    });

                let _ = devnet_event_tx.send(DevnetEvent::StacksChainEvent(chain_event));

                // Partially update the UI. With current approach a full update
                // would require either cloning the block, or passing ownership.
                send_status_update(
                    &devnet_event_tx,
                    &None,
                    "stacks-node",
                    Status::Green,
                    &format!(
                        "mining blocks (chain_tip = #{})",
                        stacks_block_update.block.block_identifier.index
                    ),
                );

                // devnet_event_tx.send(DevnetEvent::send_status_update(status_update_data));

                let message = if stacks_block_update.block.block_identifier.index == 0 {
                    format!(
                        "Genesis Stacks block anchored in Bitcoin block #{} includes {} transactions",
                        stacks_block_update
                            .block
                            .metadata
                            .bitcoin_anchor_block_identifier
                            .index,
                        stacks_block_update.block.transactions.len(),
                    )
                } else {
                    format!(
                        "Stacks block #{} mined including {} transaction{}",
                        stacks_block_update.block.block_identifier.index,
                        stacks_block_update.block.transactions.len(),
                        if stacks_block_update.block.transactions.len() <= 1 {
                            ""
                        } else {
                            "s"
                        },
                    )
                };
                let _ = devnet_event_tx.send(DevnetEvent::info(message));
            }
            ObserverEvent::NotifyBitcoinTransactionProxied => {
                if !boot_completed.load(Ordering::SeqCst) {
                    if config
                        .devnet_config
                        .epoch_3_0
                        .saturating_sub(current_burn_height)
                        > 6
                    {
                        std::thread::sleep(std::time::Duration::from_millis(750));
                    } else {
                        // as epoch 3.0 gets closer, bitcoin blocks need to slow down
                        std::thread::sleep(std::time::Duration::from_millis(4000));
                    }
                    let res = mine_bitcoin_block(
                        &config.services_map_hosts.bitcoin_node_host,
                        config.devnet_config.bitcoin_node_username.as_str(),
                        config.devnet_config.bitcoin_node_password.as_str(),
                        config.devnet_config.miner_btc_address.as_str(),
                    )
                    .await;
                    if let Err(e) = res {
                        let _ = devnet_event_tx.send(DevnetEvent::error(e));
                    }
                }
            }
            ObserverEvent::PredicateRegistered(hook) => {
                let message = format!("New hook \"{}\" registered", hook.key());
                let _ = devnet_event_tx.send(DevnetEvent::info(message));
            }
            ObserverEvent::PredicateDeregistered(_hook) => {}
            ObserverEvent::PredicatesTriggered(count) => {
                if count > 0 {
                    let _ =
                        devnet_event_tx.send(DevnetEvent::info(format!("{count} hooks triggered")));
                }
            }
            ObserverEvent::Terminate => {
                break;
            }
            ObserverEvent::StacksChainMempoolEvent(mempool_event) => match mempool_event {
                StacksChainMempoolEvent::TransactionsAdmitted(transactions) => {
                    for tx in transactions.into_iter() {
                        let _ = devnet_event_tx.send(DevnetEvent::MempoolAdmission(tx));
                    }
                }
                StacksChainMempoolEvent::TransactionDropped(ref _transactions) => {}
            },
            ObserverEvent::BitcoinPredicateTriggered(_) => {}
            ObserverEvent::StacksPredicateTriggered(_) => {}
            ObserverEvent::PredicateEnabled(_) => {}
        }
    }
    Ok(())
}

pub fn perform_protocol_deployment(
    network_manifest: &NetworkManifest,
    deployment: &DeploymentSpecification,
    deployment_event_tx: Sender<DeploymentEvent>,
    deployment_command_rx: Receiver<DeploymentCommand>,
    override_bitcoin_rpc_url: Option<String>,
    override_stacks_rpc_url: Option<String>,
) {
    let deployment = deployment.clone();
    let network_manifest = network_manifest.clone();
    let _ = hiro_system_kit::thread_named("Deployment execution").spawn(move || {
        apply_on_chain_deployment(
            network_manifest,
            deployment,
            deployment_event_tx,
            deployment_command_rx,
            false,
            override_bitcoin_rpc_url,
            override_stacks_rpc_url,
        );
    });
}

pub fn relay_devnet_protocol_deployment(
    deployment_events_rx: Receiver<DeploymentEvent>,
    devnet_event_tx: &Sender<DevnetEvent>,
    bitcoin_mining_tx: Option<Sender<BitcoinMiningCommand>>,
    boot_completed: &Arc<AtomicBool>,
) {
    let devnet_event_tx = devnet_event_tx.clone();
    let boot_completed = boot_completed.clone();
    let _ = hiro_system_kit::thread_named("Deployment monitoring").spawn(move || {
        loop {
            let event = match deployment_events_rx.recv() {
                Ok(event) => event,
                Err(_e) => break,
            };
            match event {
                DeploymentEvent::TransactionUpdate(tracker) => {
                    if let TransactionStatus::Error(ref message) = tracker.status {
                        let _ = devnet_event_tx.send(DevnetEvent::error(message.into()));
                        break;
                    }
                }
                DeploymentEvent::Interrupted(_) => {
                    // Terminate
                    break;
                }
                DeploymentEvent::DeploymentCompleted => {
                    boot_completed.store(true, Ordering::SeqCst);
                    if let Some(bitcoin_mining_tx) = bitcoin_mining_tx {
                        let _ = devnet_event_tx.send(DevnetEvent::BootCompleted(bitcoin_mining_tx));
                    }
                    break;
                }
            }
        }
    });
}

fn should_publish_stacking_orders(
    current_cycle: &u32,
    pox_stacking_order: &PoxStackingOrder,
) -> bool {
    let PoxStackingOrder {
        duration,
        start_at_cycle,
        ..
    } = pox_stacking_order;

    let is_higher_than_start_cycle = *current_cycle >= (start_at_cycle - 1);
    if !is_higher_than_start_cycle {
        return false;
    }

    let offset = (current_cycle + duration).saturating_sub(*start_at_cycle);
    let should_stack = (offset % duration) == (duration - 1);
    if !should_stack {
        return false;
    }

    true
}

async fn remove_global_snapshot(devnet_event_tx: &Sender<DevnetEvent>) -> Result<(), String> {
    let global_snapshot_dir = get_global_snapshot_dir();

    // Check if the global snapshot directory exists
    if !global_snapshot_dir.exists() {
        let _ = devnet_event_tx.send(DevnetEvent::info(
            "No existing global snapshot found to remove".to_string(),
        ));
        return Ok(());
    }

    let _ = devnet_event_tx.send(DevnetEvent::info(
        "Removing existing global snapshot...".to_string(),
    ));

    // Remove the entire global snapshot directory
    fs::remove_dir_all(&global_snapshot_dir)
        .map_err(|e| format!("unable to remove global snapshot directory: {e:?}"))?;

    let _ = devnet_event_tx.send(DevnetEvent::success(
        "Existing global snapshot removed successfully".to_string(),
    ));

    Ok(())
}

pub async fn create_global_snapshot(
    devnet_event_observer_config: &DevnetEventObserverConfig,
    devnet_event_tx: &Sender<DevnetEvent>,
    mining_command_tx: Sender<BitcoinMiningCommand>,
) {
    // First, remove the existing global snapshot if it exists
    if let Err(e) = remove_global_snapshot(devnet_event_tx).await {
        let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
            "Failed to remove existing global snapshot: {e}. Continuing with new snapshot creation."
        )));
    }

    let devnet_config = &devnet_event_observer_config.devnet_config;
    let global_snapshot_dir = get_global_snapshot_dir();
    let project_snapshot_dir = get_project_snapshot_dir(devnet_config);

    // Project snapshot marker
    let project_marker = project_snapshot_dir.join("epoch_3_ready");
    if !project_marker.exists() {
        match std::fs::File::create(&project_marker) {
            Ok(_) => {
                let _ = devnet_event_tx.send(DevnetEvent::success(
                    "Project snapshot data prepared up to epoch 3.0. Future project starts will be faster.".to_string(),
                ));
            }
            Err(e) => {
                let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                    "Failed to create project snapshot marker file: {e}"
                )));
            }
        }
    }

    let global_marker = global_snapshot_dir.join("epoch_3_ready");
    if !global_marker.exists() {
        // Copy project snapshot to global snapshot as a template
        if project_snapshot_dir != global_snapshot_dir {
            // Copy bitcoin data
            let project_bitcoin_snapshot = project_snapshot_dir.join("bitcoin");
            let global_bitcoin_snapshot = global_snapshot_dir.join("bitcoin");
            if project_bitcoin_snapshot.exists() {
                let _ = copy_directory(&project_bitcoin_snapshot, &global_bitcoin_snapshot, None)
                    .inspect_err(|e| {
                        let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                            "Failed to copy bitcoin snapshot: {e}"
                        )));
                    });
            }

            // Copy stacks data
            let project_stacks_snapshot = project_snapshot_dir.join("stacks");
            let global_stacks_snapshot = global_snapshot_dir.join("stacks");
            if project_stacks_snapshot.exists() {
                let _ = copy_directory(
                    &project_stacks_snapshot,
                    &global_stacks_snapshot,
                    Some(EXCLUDED_STACKS_SNAPSHOT_FILES),
                )
                .inspect_err(|e| {
                    let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                        "Failed to copy stacks snapshot: {e}"
                    )));
                });
            }

            match std::fs::File::create(&global_marker) {
                Ok(_) => {
                    let _ = devnet_event_tx.send(DevnetEvent::success(
                        "Global template snapshot data prepared. Future project initializations will be faster.".to_string()
                    ));
                }
                Err(e) => {
                    let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                        "Failed to create global snapshot marker file: {e}"
                    )));
                }
            }
        }
    }
    let _ = devnet_event_tx.send(DevnetEvent::info(
        "Reached block height 142, preparing to export Stacks API events...".to_string(),
    ));

    // To properly export, we need to:
    // 1. Stop mining to prevent further blocks
    let _ = mining_command_tx.send(BitcoinMiningCommand::Pause);
    // 2. Wait a moment for pausing to complete
    std::thread::sleep(Duration::from_secs(3));

    // Export the events
    match export_stacks_api_events(devnet_event_observer_config, devnet_event_tx).await {
        Ok(_) => {
            let _ = devnet_event_tx.send(DevnetEvent::success(
                "Stacks API events exported successfully".to_string(),
            ));
        }
        Err(e) => {
            let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                "Failed to export Stacks API events: {e}. Continuing without export."
            )));
        }
    }
    // 3. Resume mining after presumed export completion
    let _ = mining_command_tx.send(BitcoinMiningCommand::Start);
}

pub async fn publish_stacking_orders(
    devnet_config: &DevnetConfig,
    devnet_event_tx: &Sender<DevnetEvent>,
    accounts: &[AccountConfig],
    services_map_hosts: &ServicesMapHosts,
    fee_rate: u64,
    bitcoin_block_height: u32,
) -> Option<usize> {
    let node_rpc_url = format!("http://{}", &services_map_hosts.stacks_node_host);
    let pox_info: PoxInfo = match reqwest::get(format!("{node_rpc_url}/v2/pox")).await {
        Ok(result) => match result.json().await {
            Ok(pox_info) => Some(pox_info),
            Err(e) => {
                let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                    "unable to parse pox info: {e}"
                )));
                None
            }
        },
        Err(e) => {
            let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                "unable to retrieve pox info: {e}"
            )));
            None
        }
    }?;

    let effective_height =
        u32::saturating_sub(bitcoin_block_height, pox_info.first_burnchain_block_height);

    let current_cycle = effective_height / pox_info.reward_cycle_length;
    let pox_cycle_length = pox_info.reward_cycle_length;
    let pox_cycle_position = effective_height % pox_cycle_length;

    if pox_cycle_position != 10 {
        return None;
    }

    let pox_contract_id = pox_info.contract_id;
    let pox_version = pox_contract_id
        .rsplit('-')
        .next()
        .and_then(|version| version.parse().ok())
        .unwrap_or(1); // pox 1 contract is `pox.clar`

    let mut transactions = 0;
    for (i, pox_stacking_order) in devnet_config.pox_stacking_orders.iter().enumerate() {
        if !should_publish_stacking_orders(&current_cycle, pox_stacking_order) {
            continue;
        }

        // if the is not the first cycle of this stacker, then stacking order will be extended
        let extend_stacking = current_cycle != pox_stacking_order.start_at_cycle - 1;
        if extend_stacking && !pox_stacking_order.auto_extend.unwrap_or_default() {
            continue;
        }

        let Some(account) = accounts
            .iter()
            .find(|e| e.label == pox_stacking_order.wallet)
            .cloned()
        else {
            continue;
        };

        transactions += 1;

        let stx_amount = pox_info.next_cycle.min_threshold_ustx * pox_stacking_order.slots;

        let node_rpc_url_moved = node_rpc_url.clone();
        let pox_contract_id_moved = pox_contract_id.clone();
        let btc_address_moved = pox_stacking_order.btc_address.clone();
        let duration = pox_stacking_order.duration;

        let signer_key =
            devnet_config.stacks_signers_keys[i % devnet_config.stacks_signers_keys.len()].clone();

        let stacking_result =
            hiro_system_kit::thread_named("Stacking orders handler").spawn(move || {
                let default_fee = fee_rate * 1000;
                let stacks_rpc = StacksRpc::new(&node_rpc_url_moved);
                let nonce = stacks_rpc.get_nonce(&account.stx_address)?;

                let (_, _, account_secret_key) = clarinet_files::compute_addresses(
                    &account.mnemonic,
                    &account.derivation,
                    &StacksNetwork::Devnet.get_networks(),
                );

                let (method, arguments) = get_stacking_tx_method_and_args(
                    pox_version,
                    bitcoin_block_height,
                    current_cycle.into(),
                    &signer_key,
                    extend_stacking,
                    &btc_address_moved,
                    stx_amount,
                    duration,
                    i.try_into().unwrap(),
                );

                let tx = stacks_codec::codec::build_contract_call_transaction(
                    pox_contract_id_moved,
                    method,
                    arguments,
                    nonce,
                    default_fee,
                    &hex_bytes(&account_secret_key).unwrap(),
                );

                stacks_rpc.post_transaction(&tx)
            });

        match stacking_result {
            Ok(result) => {
                if let Ok(result) = result.join() {
                    match result {
                        Ok(_) => {
                            let _ = devnet_event_tx.send(DevnetEvent::success(format!(
                                "Stacking order for {stx_amount} STX submitted"
                            )));
                        }
                        Err(e) => {
                            let _ = devnet_event_tx
                                .send(DevnetEvent::error(format!("Unable to stack: {e}")));
                        }
                    }
                };
            }
            Err(e) => {
                let _ = devnet_event_tx.send(DevnetEvent::error(format!("Unable to stack: {e}")));
            }
        }
    }
    if transactions > 0 {
        Some(transactions)
    } else {
        None
    }
}

fn fund_genesis_account(
    devnet_event_tx: &Sender<DevnetEvent>,
    services_map_hosts: &ServicesMapHosts,
    accounts: &[AccountConfig],
    fee_rate: u64,
    boot_completed: &Arc<AtomicBool>,
) {
    let deployer = accounts
        .iter()
        .find(|account| account.label == "deployer")
        .unwrap()
        .clone();
    let (_, _, deployer_secret_key) = clarinet_files::compute_addresses(
        &deployer.mnemonic,
        &deployer.derivation,
        &StacksNetwork::Devnet.get_networks(),
    );

    let accounts_moved = accounts.to_vec();
    let devnet_event_tx_moved = devnet_event_tx.clone();
    let stacks_api_host_moved = services_map_hosts.stacks_api_host.clone();
    let boot_completed_moved = Arc::clone(boot_completed);

    let _ = hiro_system_kit::thread_named("sBTC funding handler").spawn(move || {
        while !boot_completed_moved.load(Ordering::SeqCst) {
            std::thread::sleep(std::time::Duration::from_secs(3));
        }
        let node_rpc_url = format!("http://{}", &stacks_api_host_moved);
        let stacks_rpc = StacksRpc::new(&node_rpc_url);

        let info = match stacks_rpc.call_with_retry(|client| client.get_info(), 5) {
            Ok(info) => info,
            Err(e) => {
                let _ = devnet_event_tx_moved
                    .send(DevnetEvent::error(format!("Failed to retrieve info: {e}")));
                return;
            }
        };

        let burn_height_number = info.burn_block_height as u32;
        let burn_height = ClarityValue::UInt(burn_height_number.into());

        let burn_block = match stacks_rpc
            .call_with_retry(|client| client.get_burn_block(burn_height_number), 5)
        {
            Ok(b) => b,
            Err(e) => {
                let _ = devnet_event_tx_moved.send(DevnetEvent::error(format!(
                    "Failed to retrieve burn block: {e}"
                )));
                return;
            }
        };

        let mut deployer_nonce =
            match stacks_rpc.call_with_retry(|client| client.get_nonce(&deployer.stx_address), 5) {
                Ok(n) => n,
                Err(e) => {
                    let _ = devnet_event_tx_moved
                        .send(DevnetEvent::error(format!("Failed to retrieve nonce: {e}")));
                    return;
                }
            };

        let burn_block_hash = ClarityValue::buff_from(
            hex_bytes(&burn_block.burn_block_hash.replace("0x", "")).unwrap(),
        )
        .unwrap();

        let contract_id = format!("{}.sbtc-deposit", deployer.stx_address);
        let mut nb_of_founded_accounts = 0;

        for account in accounts_moved {
            if account.sbtc_balance == 0 {
                continue;
            }
            let txid_buffer = std::array::from_fn::<_, 32, _>(|_| rand::random());
            let txid = ClarityValue::buff_from(txid_buffer.to_vec()).unwrap();
            let vout_index = ClarityValue::UInt(1);
            let amount = ClarityValue::UInt(account.sbtc_balance.into());
            let recipient =
                ClarityValue::Principal(PrincipalData::parse(&account.stx_address).unwrap());
            let sweep_txid_buffer = std::array::from_fn::<_, 32, _>(|_| rand::random());
            let sweep_txid = ClarityValue::buff_from(sweep_txid_buffer.to_vec()).unwrap();
            let args = vec![
                txid,
                vout_index,
                amount,
                recipient,
                burn_block_hash.clone(),
                burn_height.clone(),
                sweep_txid,
            ];
            let tx = stacks_codec::codec::build_contract_call_transaction(
                contract_id.clone(),
                "complete-deposit-wrapper".to_string(),
                args,
                deployer_nonce,
                fee_rate * 1000,
                &hex_bytes(&deployer_secret_key).unwrap(),
            );
            let funding_result = stacks_rpc.post_transaction(&tx);
            deployer_nonce += 1;

            match funding_result {
                Ok(_) => nb_of_founded_accounts += 1,
                Err(e) => {
                    let _ = devnet_event_tx_moved.send(DevnetEvent::error(format!(
                        "Unable to fund {}: {}",
                        account.stx_address, e
                    )));
                }
            }
        }
        let _ = devnet_event_tx_moved.send(DevnetEvent::info(format!(
            "Funded {nb_of_founded_accounts} accounts with sBTC"
        )));
    });
}

pub fn invalidate_bitcoin_chain_tip(
    _bitcoin_node_host: &str,
    _bitcoin_node_username: &str,
    _bitcoin_node_password: &str,
) {
    unimplemented!()
}

pub async fn mine_bitcoin_block(
    bitcoin_node_host: &str,
    bitcoin_node_username: &str,
    bitcoin_node_password: &str,
    miner_btc_address: &str,
) -> Result<(), String> {
    let miner_address = Address::from_str(miner_btc_address).unwrap();
    let _ = reqwest::Client::builder()
        .timeout(Duration::from_secs(2))
        .build()
        .expect("Unable to build http client")
        .post(format!("http://{bitcoin_node_host}"))
        .basic_auth(bitcoin_node_username, Some(bitcoin_node_password))
        .header("Content-Type", "application/json")
        .header("Host", bitcoin_node_host)
        .json(&serde_json::json!({
            "jsonrpc": "1.0",
            "id": "stacks-network",
            "method": "generatetoaddress",
            "params": [json!(1), json!(miner_address)]
        }))
        .send()
        .await
        .map_err(|e| format!("unable to send request ({e})"))?
        .json::<bitcoincore_rpc::jsonrpc::Response>()
        .await
        .map_err(|e| format!("unable to generate bitcoin block: ({e})"))?;
    Ok(())
}

async fn handle_bitcoin_mining(
    mining_command_rx: Receiver<BitcoinMiningCommand>,
    config: &DevnetEventObserverConfig,
    devnet_event_tx: &Sender<DevnetEvent>,
) {
    let stop_miner = Arc::new(AtomicBool::new(false));
    loop {
        let command = match mining_command_rx.recv() {
            Ok(cmd) => cmd,
            Err(e) => {
                print!("{} {}", yellow!("unexpected error:"), e);
                break;
            }
        };
        match command {
            BitcoinMiningCommand::Start => {
                stop_miner.store(false, Ordering::SeqCst);
                let stop_miner_reader = stop_miner.clone();
                let devnet_event_tx_moved = devnet_event_tx.clone();
                let config_moved = config.clone();
                let _ =
                    hiro_system_kit::thread_named("Bitcoin mining runloop").spawn(move || loop {
                        std::thread::sleep(std::time::Duration::from_millis(
                            config_moved
                                .devnet_config
                                .bitcoin_controller_block_time
                                .into(),
                        ));
                        let future = mine_bitcoin_block(
                            &config_moved.services_map_hosts.bitcoin_node_host,
                            &config_moved.devnet_config.bitcoin_node_username,
                            &config_moved.devnet_config.bitcoin_node_password,
                            &config_moved.devnet_config.miner_btc_address,
                        );
                        let res = hiro_system_kit::nestable_block_on(future);
                        if stop_miner_reader.load(Ordering::SeqCst) {
                            break;
                        }
                        if let Err(e) = res {
                            let _ = devnet_event_tx_moved.send(DevnetEvent::error(e));
                        }
                    });
            }
            BitcoinMiningCommand::Pause => {
                stop_miner.store(true, Ordering::SeqCst);
            }
            BitcoinMiningCommand::Mine => {
                let res = mine_bitcoin_block(
                    &config.services_map_hosts.bitcoin_node_host,
                    config.devnet_config.bitcoin_node_username.as_str(),
                    config.devnet_config.bitcoin_node_password.as_str(),
                    config.devnet_config.miner_btc_address.as_str(),
                )
                .await;
                if let Err(e) = res {
                    let _ = devnet_event_tx.send(DevnetEvent::error(e));
                }
            }
            BitcoinMiningCommand::InvalidateChainTip => {
                invalidate_bitcoin_chain_tip(
                    &config.services_map_hosts.bitcoin_node_host,
                    config.devnet_config.bitcoin_node_username.as_str(),
                    config.devnet_config.bitcoin_node_password.as_str(),
                );
            }
        }
    }
}

fn get_stacking_tx_method_and_args(
    pox_version: u32,
    bitcoin_block_height: u32,
    cycle: u128,
    signer_key: &StacksPrivateKey,
    extend_stacking: bool,
    btc_address: &str,
    stx_amount: u64,
    duration: u32,
    auth_id: u128,
) -> (String, Vec<ClarityValue>) {
    let addr_bytes = btc_address
        .from_base58()
        .expect("Unable to get bytes from btc address");
    let pox_addr_tuple = ClarityValue::Tuple(
        TupleData::from_data(vec![
            (
                ClarityName::try_from("version".to_owned()).unwrap(),
                ClarityValue::buff_from_byte(AddressHashMode::SerializeP2PKH as u8),
            ),
            (
                ClarityName::try_from("hashbytes".to_owned()).unwrap(),
                ClarityValue::Sequence(SequenceData::Buffer(BuffData {
                    data: Hash160::from_bytes(&addr_bytes[1..21])
                        .unwrap()
                        .as_bytes()
                        .to_vec(),
                })),
            ),
        ])
        .unwrap(),
    );

    let burn_block_height: u128 = (bitcoin_block_height - 1).into();

    let method = if extend_stacking {
        "stack-extend"
    } else {
        "stack-stx"
    };

    let pox_addr = PoxAddress::try_from_pox_tuple(false, &pox_addr_tuple).unwrap();
    let mut arguments = if extend_stacking {
        vec![ClarityValue::UInt(duration.into()), pox_addr_tuple]
    } else {
        vec![
            ClarityValue::UInt(stx_amount.into()),
            pox_addr_tuple,
            ClarityValue::UInt(burn_block_height),
            ClarityValue::UInt(duration.into()),
        ]
    };

    if pox_version >= 4 {
        // extra arguments for pox-4 (for both stack-stx and stack-extend)
        //   (signer-sig (optional (buff 65)))
        //   (signer-key (buff 33))
        //   (max-amount uint)
        //   (auth-id uint)
        let topic = if extend_stacking {
            Pox4SignatureTopic::StackExtend
        } else {
            Pox4SignatureTopic::StackStx
        };

        let signature = make_pox_4_signer_key_signature(
            &pox_addr,
            signer_key,
            cycle,
            &topic,
            CHAIN_ID_TESTNET,
            duration.into(),
            stx_amount.into(),
            auth_id,
        )
        .expect("Unable to make pox 4 signature");

        let signer_sig = signature.to_rsv();

        let pub_key = StacksPublicKey::from_private(signer_key);
        arguments.push(ClarityValue::some(ClarityValue::buff_from(signer_sig).unwrap()).unwrap());
        arguments.push(ClarityValue::buff_from(pub_key.to_bytes()).unwrap());
        arguments.push(ClarityValue::UInt(stx_amount.into()));
        arguments.push(ClarityValue::UInt(auth_id));
    };

    (method.to_string(), arguments)
}

async fn export_stacks_api_events(
    config: &DevnetEventObserverConfig,
    devnet_event_tx: &Sender<DevnetEvent>,
) -> Result<(), String> {
    // Get container name
    let container_name = format!("stacks-api.{}.devnet", config.manifest.project.name);

    let _ = devnet_event_tx.send(DevnetEvent::info(
        "Exporting Stacks API events...".to_string(),
    ));

    // Create exec command to export events
    let export_command = format!(
        "docker exec {container_name} node /app/lib/index.js export-events --file /tmp/events_cache.tsv --overwrite-file"
    );
    let output = run_command(&export_command)
        .map_err(|e| format!("Failed to execute export command: {e}"))?;

    if !output.status.success() {
        return Err(format!(
            "Export command failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    // Wait for export to complete
    std::thread::sleep(Duration::from_secs(5));

    // Copy the exported file from container to host
    let export_path = PathBuf::from(&config.devnet_config.working_dir).join("events_export");
    fs::create_dir_all(&export_path)
        .map_err(|e| format!("unable to create events export directory: {e:?}"))?;

    let copy_command = format!(
        "docker cp {}:/tmp/events_cache.tsv {}",
        container_name,
        export_path.join("events_cache.tsv").display()
    );

    let output =
        run_command(&copy_command).map_err(|e| format!("Failed to copy events file: {e}"))?;

    if !output.status.success() {
        return Err(format!(
            "Copy command failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    // Also copy to global cache
    let global_snapshot_dir = get_global_snapshot_dir();
    let global_export_path = global_snapshot_dir.join("events_export");
    fs::create_dir_all(&global_export_path)
        .map_err(|e| format!("unable to create global events export directory: {e:?}"))?;

    fs::copy(
        export_path.join("events_cache.tsv"),
        global_export_path.join("events_cache.tsv"),
    )
    .map_err(|e| format!("unable to copy to global cache: {e:?}"))?;

    let _ = devnet_event_tx.send(DevnetEvent::success(
        "Successfully exported Stacks API events".to_string(),
    ));

    Ok(())
}

#[cfg(test)]
mod tests_stacking_orders {

    use super::*;

    fn build_pox_stacking_order(duration: u32, start_at_cycle: u32) -> PoxStackingOrder {
        PoxStackingOrder {
            duration,
            start_at_cycle,
            wallet: "wallet_1".to_string(),
            slots: 1,
            btc_address: "address_1".to_string(),
            auto_extend: Some(true),
        }
    }

    #[test]
    fn test_should_publish_stacking_orders_basic() {
        let pox_stacking_order = build_pox_stacking_order(12, 6);

        // cycle just before start_at_cycle
        assert!(should_publish_stacking_orders(&5, &pox_stacking_order));
        // cycle before start_at_cycle + duration
        assert!(should_publish_stacking_orders(&17, &pox_stacking_order),);
        // cycle before start_at_cycle + duration * 42
        assert!(should_publish_stacking_orders(&509, &pox_stacking_order));
        // cycle equal to start_at_cycle
        assert!(!should_publish_stacking_orders(&6, &pox_stacking_order));
        // cycle after start_at_cycle
        assert!(!should_publish_stacking_orders(&8, &pox_stacking_order));
    }

    #[test]
    fn test_should_publish_stacking_orders_edge_cases() {
        // duration is one cycle
        let pox_stacking_order = build_pox_stacking_order(1, 4);
        assert!(!should_publish_stacking_orders(&2, &pox_stacking_order));

        for i in 3..=20 {
            assert!(should_publish_stacking_orders(&i, &pox_stacking_order));
        }
        // duration is low and start_at_cycle is high
        let pox_stacking_order = build_pox_stacking_order(2, 100);
        for i in 0..=98 {
            assert!(!should_publish_stacking_orders(&i, &pox_stacking_order));
        }
        assert!(should_publish_stacking_orders(&99, &pox_stacking_order));
        assert!(!should_publish_stacking_orders(&100, &pox_stacking_order));
        assert!(should_publish_stacking_orders(&101, &pox_stacking_order));
    }
}

#[cfg(test)]
mod test_rpc_client {
    use clarinet_files::DEFAULT_DERIVATION_PATH;
    use stacks_rpc_client::mock_stacks_rpc::MockStacksRpc;
    use stacks_rpc_client::rpc_client::NodeInfo;

    use super::*;

    #[test]
    fn test_fund_genesis_account() {
        let mut stacks_rpc = MockStacksRpc::new();
        let info_mock = stacks_rpc.get_info_mock(NodeInfo {
            peer_version: 4207599116,
            pox_consensus: "4f4de3d4ab3246299c039084a12c801c9dc70323".to_string(),
            burn_block_height: 100,
            stable_pox_consensus: "a2c4972bf818f554809e25fa637b780c77c20b62".to_string(),
            stable_burn_block_height: 99,
            server_version: "stacks-node 0.0.1".to_string(),
            network_id: 2147483648,
            parent_network_id: 3669344250,
            stacks_tip_height: 47,
            stacks_tip: "6bb0e4706fdfb9624a23d9144f2161c61d5c58816643b48ffdb735887bdbf5fa"
                .to_string(),
            stacks_tip_consensus_hash: "4f4de3d4ab3246299c039084a12c801c9dc70323".to_string(),
            genesis_chainstate_hash:
                "74237aa39aa50a83de11a4f53e9d3bb7d43461d1de9873f402e5453ae60bc59b".to_string(),
        });
        let burn_block_mock = stacks_rpc.get_burn_block_mock(100);
        let nonce_mock = stacks_rpc.get_nonce_mock("ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC", 0);
        let tx_mock = stacks_rpc
            .get_tx_mock("0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef");

        let deployer = AccountConfig {
            label: "deployer".to_string(),
            mnemonic: "cycle puppy glare enroll cost improve round trend wrist mushroom scorpion tower claim oppose clever elephant dinosaur eight problem before frozen dune wagon high".to_string(),
            encrypted_mnemonic: "".to_string(),
            derivation: DEFAULT_DERIVATION_PATH.to_string(),
            balance: 10000000,
            sbtc_balance: 100000000,
            stx_address: "ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC".to_string(),
            btc_address: "mvZtbibDAAA3WLpY7zXXFqRa3T4XSknBX7".to_string(),
            is_mainnet: false,
        };

        let fee_rate = 10;
        let (devnet_event_tx, devnet_event_rx) = channel();
        let services_map_hosts = ServicesMapHosts {
            stacks_api_host: stacks_rpc.url.replace("http://", ""),
            // only stacks_node is called
            stacks_node_host: stacks_rpc.url.clone(),
            bitcoin_node_host: "localhost".to_string(),
            bitcoin_explorer_host: "localhost".to_string(),
            stacks_explorer_host: "localhost".to_string(),
            postgres_host: "localhost".to_string(),
        };
        let accounts = vec![deployer];

        let boot_completed = Arc::new(AtomicBool::new(true));

        fund_genesis_account(
            &devnet_event_tx,
            &services_map_hosts,
            &accounts,
            fee_rate,
            &boot_completed,
        );

        let timeout = Duration::from_secs(3);
        let start = std::time::Instant::now();

        let mut received_events = Vec::new();
        while start.elapsed() < timeout {
            match devnet_event_rx.recv_timeout(Duration::from_millis(100)) {
                Ok(event) => {
                    received_events.push(event);
                }
                Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {}
                Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                    break;
                }
            }
        }

        assert_eq!(received_events.len(), 1);
        assert!(received_events.iter().any(|event| {
            if let DevnetEvent::Log(msg) = event {
                msg.message.contains("Funded 1 accounts with sBTC")
            } else {
                false
            }
        }));

        info_mock.assert();
        nonce_mock.assert();
        burn_block_mock.assert();
        tx_mock.assert();
    }
}
