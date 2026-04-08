use std::collections::HashSet;
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
use clarinet_deployments::onchain::{
    apply_on_chain_deployment, DeploymentCommand, DeploymentEvent, TransactionStatus,
};
use clarinet_deployments::types::DeploymentSpecification;
use clarinet_files::{
    self, AccountConfig, DevnetConfig, NetworkManifest, PoxStackingOrder, ProjectManifest,
    StacksNetwork, DEFAULT_FIRST_BURN_HEADER_HEIGHT, DEFAULT_POX_REWARD_LENGTH,
};
use clarity::consts::CHAIN_ID_TESTNET;
use clarity::types::PublicKey;
use clarity::util::hash::{hex_bytes, Hash160};
use clarity::vm::types::{BuffData, PrincipalData, SequenceData, TupleData};
use clarity::vm::{ClarityName, Value as ClarityValue};
use hiro_system_kit::{self, slog, yellow};
use observer::event_handler::{
    start_event_observer, EventObserverConfig, ObserverCommand, ObserverEvent,
    StacksChainMempoolEvent, StacksObserverStartupContext,
};
use observer::indexer::stacks::standardize_stacks_serialized_block;
use observer::indexer::StacksChainContext;
use observer::types::{
    BitcoinChainEvent, StacksBlockData, StacksChainEvent, StacksNodeConfig, StacksTransactionKind,
};
use observer::utils::Context;
use serde::Deserialize;
use serde_json::json;
use stacks_common::address::AddressHashMode;
use stacks_common::types::chainstate::{StacksAddress, StacksPrivateKey, StacksPublicKey};
use stacks_rpc_client::rpc_client::PoxInfo;
use stacks_rpc_client::StacksRpc;
use stackslib::chainstate::stacks::address::PoxAddress;
use stackslib::util_lib::signed_structured_data::pox4::{
    make_pox_4_signer_key_signature, Pox4SignatureTopic,
};
use stackslib::util_lib::signed_structured_data::pox5::{
    make_pox_5_signer_grant_signature, make_pox_5_signer_key_signature, Pox5SignatureTopic,
};

use super::ChainsCoordinatorCommand;
use crate::command::run_docker_command;
use crate::event::{send_status_update, DevnetEvent, Status};
use crate::orchestrator::{
    copy_directory, get_global_snapshot_dir, get_project_snapshot_dir, ServicesMapHosts,
    EXCLUDED_STACKS_SNAPSHOT_FILES,
};

const SNAPSHOT_EPOCH3_5_STACKS_HEIGHT: u64 = 60;
const SNAPSHOT_EPOCH3_5_BURN_HEIGHT: u64 = 163;

/// Whether to start from a snapshot or from genesis.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SnapshotLevel {
    /// No snapshot — start from genesis.
    None,
    /// Snapshot at epoch 3.5 (burn height 163).
    Epoch3_5,
}

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
        ctx: &Context,
        services_map_hosts: ServicesMapHosts,
    ) -> Self {
        ctx.try_log(|logger| slog::info!(logger, "Checking contracts"));
        let network_manifest = match network_manifest {
            Some(n) => n,
            None => NetworkManifest::from_project_root(
                &manifest.root_dir,
                &StacksNetwork::Devnet.get_networks(),
                false,
                Some(&manifest.project.cache_location),
                None,
            )
            .expect("unable to load network manifest"),
        };
        let event_observer_config = EventObserverConfig {
            bitcoin_rpc_proxy_enabled: true,
            bitcoind_rpc_username: devnet_config.bitcoin_node_username.clone(),
            bitcoind_rpc_password: devnet_config.bitcoin_node_password.clone(),
            bitcoind_rpc_url: format!("http://{}", services_map_hosts.bitcoin_node_host),
            stacks_node_config: StacksNodeConfig {
                rpc_url: format!("http://{}", services_map_hosts.stacks_node_host),
                ingestion_port: devnet_config.orchestrator_ingestion_port,
            },

            display_stacks_ingestion_logs: true,
            bitcoin_network: observer::types::BitcoinNetwork::Regtest,
            stacks_network: observer::types::StacksNetwork::Devnet,
            prometheus_monitoring_port: None,
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
    snapshot_level: SnapshotLevel,
    create_new_snapshot: bool,
    ctx: Context,
) -> Result<(), String> {
    let mut should_deploy_protocol = true; // Will change when `stacks-network` components becomes compatible with Testnet / Mainnet setups
    let boot_completed = Arc::new(AtomicBool::new(false));
    let mut last_pox_version: Option<u32> = None;
    let mut current_burn_height = match snapshot_level {
        SnapshotLevel::Epoch3_5 => SNAPSHOT_EPOCH3_5_BURN_HEIGHT,
        SnapshotLevel::None => 0,
    };
    let starting_block_height = match snapshot_level {
        SnapshotLevel::Epoch3_5 => SNAPSHOT_EPOCH3_5_STACKS_HEIGHT,
        SnapshotLevel::None => 0,
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

    // Spawn event observer
    let (observer_event_tx, observer_event_rx) = crossbeam_channel::unbounded();
    let event_observer_config = config.event_observer_config.clone();
    let observer_event_tx_moved = observer_event_tx.clone();
    let observer_command_tx_moved = observer_command_tx.clone();
    let ctx_moved = ctx.clone();

    let stacks_startup_context = if snapshot_level != SnapshotLevel::None {
        // Load events from snapshot if available
        let event_pool: Vec<StacksBlockData> = {
            let mut events = vec![];
            let events_cache_path = snapshot_level
                .snapshot_dir()
                .join("events_export")
                .join("events_cache.tsv");

            let mut chain_ctx = StacksChainContext::new(&observer::types::StacksNetwork::Devnet);
            if let Ok(file_content) = fs::read_to_string(&events_cache_path) {
                for line in file_content.lines() {
                    let parts: Vec<&str> = line.split('\t').collect();
                    if parts.get(2).unwrap_or(&"") == &"/new_block" {
                        let maybe_block = standardize_stacks_serialized_block(
                            &observer::indexer::IndexerConfig {
                                bitcoin_network: observer::types::BitcoinNetwork::Regtest,
                                stacks_network: observer::types::StacksNetwork::Devnet,
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
                                stacks_node_config: StacksNodeConfig {
                                    rpc_url: config.devnet_config.stacks_node_image_url.clone(),
                                    ingestion_port: 3999,
                                },
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
            ObserverEvent::BitcoinChainEvent(chain_update) => {
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

                        // Create snapshot at epoch 3.5 milestone
                        if create_new_snapshot {
                            if let Some(epoch_3_5) = config.devnet_config.epoch_3_5 {
                                // Snapshot a few blocks after the next reward cycle
                                // boundary following epoch 3.5 activation, so pox-5
                                // stacking is confirmed and active.
                                let epoch_offset = (epoch_3_5 - DEFAULT_FIRST_BURN_HEADER_HEIGHT)
                                    % DEFAULT_POX_REWARD_LENGTH;
                                let next_cycle_start =
                                    epoch_3_5 + (DEFAULT_POX_REWARD_LENGTH - epoch_offset);
                                let snapshot_height = next_cycle_start + 3;

                                if bitcoin_block_height == snapshot_height {
                                    let _ = create_snapshot(
                                        &config,
                                        &devnet_event_tx,
                                        mining_command_tx.clone(),
                                    )
                                    .await;
                                }
                            }
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
                                &mut last_pox_version,
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

                send_status_update(&devnet_event_tx, "bitcoin-node", Status::Green, &comment);
                let _ = devnet_event_tx.send(DevnetEvent::BitcoinChainEvent(chain_update.clone()));
            }
            ObserverEvent::StacksChainEvent(chain_event) => {
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
        while let Ok(event) = deployment_events_rx.recv() {
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

impl SnapshotLevel {
    /// Returns the marker file name for this snapshot level.
    pub fn marker_name(self) -> &'static str {
        match self {
            SnapshotLevel::Epoch3_5 => "epoch_3_5_ready",
            SnapshotLevel::None => unreachable!(),
        }
    }

    /// Returns the snapshot directory for this level.
    pub fn snapshot_dir(self) -> PathBuf {
        get_global_snapshot_dir().join("epoch_3_5")
    }
}

fn remove_snapshot(devnet_event_tx: &Sender<DevnetEvent>) -> Result<(), String> {
    let snapshot_dir = SnapshotLevel::Epoch3_5.snapshot_dir();
    let marker = snapshot_dir.join(SnapshotLevel::Epoch3_5.marker_name());

    if !marker.exists() {
        return Ok(());
    }

    let _ = devnet_event_tx.send(DevnetEvent::info(
        "Removing existing snapshot...".to_string(),
    ));

    let _ = fs::remove_dir_all(&snapshot_dir);

    let _ = devnet_event_tx.send(DevnetEvent::success(
        "Snapshot removed successfully".to_string(),
    ));

    Ok(())
}

pub async fn create_snapshot(
    devnet_event_observer_config: &DevnetEventObserverConfig,
    devnet_event_tx: &Sender<DevnetEvent>,
    mining_command_tx: Sender<BitcoinMiningCommand>,
) {
    // Remove any existing snapshot
    let _ = remove_snapshot(devnet_event_tx);

    let devnet_config = &devnet_event_observer_config.devnet_config;
    let project_snapshot_dir = get_project_snapshot_dir(devnet_config);
    let snapshot_dir = SnapshotLevel::Epoch3_5.snapshot_dir();

    fs::create_dir_all(&snapshot_dir)
        .unwrap_or_else(|e| panic!("unable to create snapshot directory: {e:?}"));

    // Copy project data to snapshot directory
    let project_bitcoin = project_snapshot_dir.join("bitcoin");
    let snapshot_bitcoin = snapshot_dir.join("bitcoin");
    if project_bitcoin.exists() {
        let _ = copy_directory(&project_bitcoin, &snapshot_bitcoin, None).inspect_err(|e| {
            let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                "Failed to copy bitcoin snapshot: {e}"
            )));
        });
    }

    let project_stacks = project_snapshot_dir.join("stacks");
    let snapshot_stacks = snapshot_dir.join("stacks");
    if project_stacks.exists() {
        let _ = copy_directory(
            &project_stacks,
            &snapshot_stacks,
            Some(EXCLUDED_STACKS_SNAPSHOT_FILES),
        )
        .inspect_err(|e| {
            let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                "Failed to copy stacks snapshot: {e}"
            )));
        });
    }

    let _ = devnet_event_tx.send(DevnetEvent::info(
        "Creating snapshot, preparing to export Stacks API events...".to_string(),
    ));

    // 1. Stop mining to prevent further blocks
    let _ = mining_command_tx.send(BitcoinMiningCommand::Pause);
    // 2. Wait a moment for pausing to complete
    std::thread::sleep(Duration::from_secs(3));

    // Export the events
    match export_stacks_api_events(devnet_event_observer_config, devnet_event_tx).await {
        Ok(_) => {
            // Copy events to snapshot dir
            let project_events = project_snapshot_dir
                .join("events_export")
                .join("events_cache.tsv");
            let snapshot_events_dir = snapshot_dir.join("events_export");
            let _ = fs::create_dir_all(&snapshot_events_dir);
            if project_events.exists() {
                let _ = fs::copy(
                    &project_events,
                    snapshot_events_dir.join("events_cache.tsv"),
                );
            }
            let _ = devnet_event_tx.send(DevnetEvent::success(
                "Stacks API events exported for snapshot".to_string(),
            ));
        }
        Err(e) => {
            let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                "Failed to export Stacks API events: {e}. Continuing without export."
            )));
        }
    }

    // Write the marker file
    match std::fs::File::create(snapshot_dir.join(SnapshotLevel::Epoch3_5.marker_name())) {
        Ok(_) => {
            let _ = devnet_event_tx.send(DevnetEvent::success(
                "Snapshot created successfully".to_string(),
            ));
        }
        Err(e) => {
            let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                "Failed to create snapshot marker: {e}"
            )));
        }
    }

    // 3. Resume mining
    let _ = mining_command_tx.send(BitcoinMiningCommand::Start);
}

pub async fn publish_stacking_orders(
    devnet_config: &DevnetConfig,
    devnet_event_tx: &Sender<DevnetEvent>,
    accounts: &[AccountConfig],
    services_map_hosts: &ServicesMapHosts,
    fee_rate: u64,
    bitcoin_block_height: u32,
    last_pox_version: &mut Option<u32>,
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

    let pox_contract_id = pox_info.contract_id;
    let pox_version = pox_contract_id
        .rsplit('-')
        .next()
        .and_then(|version| version.parse().ok())
        .unwrap_or(1); // pox 1 contract is `pox.clar`

    // Detect pox change (pox-4 -> pox-5 after epoch 3.5)
    let pox_version_changed = match *last_pox_version {
        Some(prev) => prev != pox_version,
        None => false,
    };
    *last_pox_version = Some(pox_version);

    if !pox_version_changed && pox_cycle_position != 10 {
        return None;
    }

    if pox_version_changed {
        let _ = devnet_event_tx.send(DevnetEvent::info(format!(
            "PoX contract changed to pox-{pox_version}, re-submitting stacking orders"
        )));
    }

    let mut transactions = 0;
    let mut lockup_txids: Vec<String> = vec![];
    // In pox-5, the reward set validation uses a HashMap keyed by signer public key.
    // Multiple stacking entries with the same signer key create duplicate HashMap keys,
    // causing block signature validation to fail. Track used signer keys to prevent this.
    let mut used_signer_keys: HashSet<Vec<u8>> = HashSet::new();
    for (i, pox_stacking_order) in devnet_config.pox_stacking_orders.iter().enumerate() {
        if !pox_version_changed
            && !should_publish_stacking_orders(&current_cycle, pox_stacking_order)
        {
            continue;
        }

        // if the is not the first cycle of this stacker, then stacking order will be extended.
        // When the pox version changed, treat as a fresh stake since the new contract has no
        // existing stacking state for this stacker.
        let extend_stacking =
            !pox_version_changed && current_cycle != pox_stacking_order.start_at_cycle - 1;
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

        let signer_key =
            devnet_config.stacks_signers_keys[i % devnet_config.stacks_signers_keys.len()].clone();

        // For pox-5+, skip stacking orders that reuse a signer key already used by another
        // order. The reward set validation HashMap deduplicates entries by signer key, so
        // multiple entries with the same key causes signature validation to fail.
        if pox_version >= 5 {
            let pub_key_bytes = StacksPublicKey::from_private(&signer_key).to_bytes();
            if !used_signer_keys.insert(pub_key_bytes) {
                let _ = devnet_event_tx.send(DevnetEvent::warning(format!(
                    "Skipping stacking order for '{}': signer key already used by another order (pox-5 requires unique signer keys)",
                    pox_stacking_order.wallet
                )));
                continue;
            }
        }

        transactions += 1;

        let stx_amount = pox_info.next_cycle.min_threshold_ustx * pox_stacking_order.slots;

        let node_rpc_url_moved = node_rpc_url.clone();
        let pox_contract_id_moved = pox_contract_id.clone();
        let btc_address_moved = pox_stacking_order.btc_address.clone();
        let duration = pox_stacking_order.duration;

        let bitcoin_node_host = services_map_hosts.bitcoin_node_host.clone();
        let bitcoin_node_username = devnet_config.bitcoin_node_username.clone();
        let bitcoin_node_password = devnet_config.bitcoin_node_password.clone();
        let pox_reward_length = pox_info.reward_cycle_length;
        let first_burnchain_block_height = pox_info.first_burnchain_block_height;

        let stx_address_moved = account.stx_address.clone();
        let stacking_result =
            hiro_system_kit::thread_named("Stacking orders handler").spawn(move || {
                let default_fee = fee_rate * 1000;
                let stacks_rpc = StacksRpc::new(&node_rpc_url_moved);
                let mut nonce = stacks_rpc
                    .get_nonce(&account.stx_address)
                    .map_err(|e| format!("{e:?}"))?;

                let (_, _, account_secret_key) = clarinet_files::compute_addresses(
                    &account.mnemonic,
                    &account.derivation,
                    &StacksNetwork::Devnet.get_networks(),
                );

                let auth_id: u128 = i.try_into().unwrap();

                // For pox-5, submit a grant-signer-key transaction before staking
                if pox_version >= 5 {
                    let stacker_principal =
                        PrincipalData::parse(&stx_address_moved).expect("Invalid stx address");
                    let pox_addr_tuple = make_pox_addr_tuple(&btc_address_moved);
                    let pox_addr = PoxAddress::try_from_pox_tuple(false, &pox_addr_tuple).unwrap();

                    let grant_args = get_pox5_grant_signer_key_args(
                        &stacker_principal,
                        &pox_addr,
                        &signer_key,
                        auth_id,
                    );

                    let grant_tx = stacks_codec::codec::build_contract_call_transaction(
                        pox_contract_id_moved.clone(),
                        "grant-signer-key".to_string(),
                        grant_args,
                        nonce,
                        default_fee,
                        &hex_bytes(&account_secret_key).unwrap(),
                    );

                    stacks_rpc
                        .post_transaction(&grant_tx)
                        .map_err(|e| format!("{e:?}"))?;
                    nonce += 1;
                }

                let (method, arguments) = get_stacking_tx_method_and_args(
                    pox_version,
                    bitcoin_block_height,
                    current_cycle.into(),
                    &signer_key,
                    extend_stacking,
                    &btc_address_moved,
                    stx_amount,
                    duration,
                    auth_id,
                );

                let tx = stacks_codec::codec::build_contract_call_transaction(
                    pox_contract_id_moved,
                    method,
                    arguments,
                    nonce,
                    default_fee,
                    &hex_bytes(&account_secret_key).unwrap(),
                );

                stacks_rpc
                    .post_transaction(&tx)
                    .map_err(|e| format!("{e:?}"))?;

                // For pox-5, create and broadcast the Bitcoin lockup transaction
                let mut lockup_txid_result = String::new();
                if pox_version >= 5 {
                    let unlock_burn_height = compute_unlock_burn_height(
                        bitcoin_block_height,
                        first_burnchain_block_height,
                        pox_reward_length,
                        duration,
                    );

                    let full_key = hex_bytes(&account_secret_key).unwrap();
                    // Strip the trailing 0x01 compression flag appended by compute_addresses
                    let btc_secret_key = &full_key[..32];
                    let _ = std::fs::OpenOptions::new().create(true).append(true).open("/tmp/pox5-debug.log")
                        .and_then(|mut f| {
                            use std::io::Write;
                            writeln!(f, "pox-5 lockup: stacker={}, btc_addr={}, unlock_height={}, btc_block={}",
                                stx_address_moved, btc_address_moved, unlock_burn_height, bitcoin_block_height)
                        });
                    let lockup_txid = send_pox5_bitcoin_lockup(
                        &bitcoin_node_host,
                        &bitcoin_node_username,
                        &bitcoin_node_password,
                        &stx_address_moved,
                        &btc_address_moved,
                        btc_secret_key,
                        unlock_burn_height,
                    )?;
                    let _ = std::fs::OpenOptions::new().create(true).append(true).open("/tmp/pox5-debug.log")
                        .and_then(|mut f| {
                            use std::io::Write;
                            writeln!(f, "pox-5 lockup broadcast: stacker={}, lockup_txid={}", stx_address_moved, lockup_txid)
                        });
                    lockup_txid_result = lockup_txid;
                }

                Ok::<String, String>(lockup_txid_result)
            });

        match stacking_result {
            Ok(result) => {
                if let Ok(result) = result.join() {
                    match result {
                        Ok(txid) => {
                            if !txid.is_empty() {
                                lockup_txids.push(txid);
                            }
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

    // After broadcasting pox-5 Bitcoin lockup transactions, mine a block immediately
    // to ensure they are confirmed before the prepare phase begins.
    if pox_version >= 5 && !lockup_txids.is_empty() {
        std::thread::sleep(std::time::Duration::from_millis(500));
        let _ = mine_bitcoin_block(
            &services_map_hosts.bitcoin_node_host,
            &devnet_config.bitcoin_node_username,
            &devnet_config.bitcoin_node_password,
            &devnet_config.miner_btc_address,
        )
        .await;

        // Verify lockup transactions are confirmed
        std::thread::sleep(std::time::Duration::from_millis(500));
        let bitcoin_rpc_url = format!("http://{}", &services_map_hosts.bitcoin_node_host);
        let client = reqwest::Client::new();
        for txid in &lockup_txids {
            let resp = client
                .post(&bitcoin_rpc_url)
                .basic_auth(
                    &devnet_config.bitcoin_node_username,
                    Some(&devnet_config.bitcoin_node_password),
                )
                .json(&serde_json::json!({
                    "jsonrpc": "1.0",
                    "id": "clarinet",
                    "method": "getrawtransaction",
                    "params": [txid, true]
                }))
                .send()
                .await
                .ok();
            let resp_json = match resp {
                Some(r) => r.json::<serde_json::Value>().await.ok(),
                None => None,
            };
            let confirmations = resp_json
                .as_ref()
                .and_then(|v| v["result"]["confirmations"].as_i64())
                .unwrap_or(-1);
            let block_hash = resp_json
                .as_ref()
                .and_then(|v| v["result"]["blockhash"].as_str())
                .unwrap_or("none");
            let _ = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open("/tmp/pox5-debug.log")
                .and_then(|mut f| {
                    use std::io::Write;
                    writeln!(
                        f,
                        "pox-5 post-mine check: txid={txid}, confirmations={confirmations}, blockhash={block_hash}"
                    )
                });
        }
        let _ = devnet_event_tx.send(DevnetEvent::info(format!(
            "Mined Bitcoin block to confirm {} pox-5 lockup transactions",
            lockup_txids.len()
        )));
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

fn make_pox_addr_tuple(btc_address: &str) -> ClarityValue {
    let addr_bytes = btc_address
        .from_base58()
        .expect("Unable to get bytes from btc address");
    ClarityValue::Tuple(
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
    )
}

/// Build the unlock script bytes for a pox-5 lockup.
/// For devnet, this is a simple P2PKH script: OP_DUP OP_HASH160 <pubkey-hash> OP_EQUALVERIFY OP_CHECKSIG
fn build_unlock_script(btc_address: &str) -> Vec<u8> {
    let addr_bytes = btc_address
        .from_base58()
        .expect("Unable to get bytes from btc address");
    let pubkey_hash = &addr_bytes[1..21];

    // Standard P2PKH script
    vec![
        0x76, // OP_DUP
        0xa9, // OP_HASH160
        0x14, // Push 20 bytes
    ]
    .into_iter()
    .chain(pubkey_hash.iter().copied())
    .chain([
        0x88, // OP_EQUALVERIFY
        0xac, // OP_CHECKSIG
    ])
    .collect()
}

/// Build the full pox-5 locking script (witness redeem script).
///
/// Must match stacks-core's `RawPox5Entry::to_redeem_script()`, which uses
/// the Bitcoin `Script::Builder`:
///   Builder::new()
///     .push_slice(&principal_data)       // 22 bytes: 0x05 || version || hash160
///     .push_opcode(OP_DROP)
///     .push_scriptint(unlock_height)     // minimal-length CScriptNum encoding
///     .push_opcode(OP_CLTV)
///     .push_opcode(OP_DROP)
///     .push_slice(&unlock_bytes)         // length-prefixed data push
///     .into_script()
fn build_pox5_locking_script(
    stacker_stx_address: &str,
    unlock_burn_height: u32,
    unlock_bytes: &[u8],
) -> Vec<u8> {
    use bitcoincore_rpc::bitcoin::blockdata::opcodes;
    use bitcoincore_rpc::bitcoin::blockdata::script::Builder;
    use bitcoincore_rpc::bitcoin::script::PushBytes;

    let stx_addr =
        <StacksAddress as stacks_common::types::Address>::from_string(stacker_stx_address)
            .expect("Unable to parse stacks address");
    let addr_version = stx_addr.version();
    let addr_hash = stx_addr.bytes().as_bytes();

    let mut principal_data = vec![0x05, addr_version];
    principal_data.extend_from_slice(addr_hash);

    let builder = Builder::new()
        .push_slice(<&PushBytes>::try_from(principal_data.as_slice()).unwrap())
        .push_opcode(opcodes::all::OP_DROP)
        .push_int(unlock_burn_height.into())
        .push_opcode(opcodes::all::OP_CLTV)
        .push_opcode(opcodes::all::OP_DROP);

    let builder = if !unlock_bytes.is_empty() {
        builder.push_slice(<&PushBytes>::try_from(unlock_bytes).unwrap())
    } else {
        builder
    };

    builder.into_script().into_bytes()
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
    let pox_addr_tuple = make_pox_addr_tuple(btc_address);
    let burn_block_height: u128 = (bitcoin_block_height - 1).into();
    let pox_addr = PoxAddress::try_from_pox_tuple(false, &pox_addr_tuple).unwrap();

    if pox_version >= 5 {
        return get_pox5_stacking_tx_method_and_args(
            &pox_addr,
            pox_addr_tuple,
            burn_block_height,
            cycle,
            signer_key,
            extend_stacking,
            btc_address,
            stx_amount,
            duration,
            auth_id,
        );
    }

    let method = if extend_stacking {
        "stack-extend"
    } else {
        "stack-stx"
    };

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

/// Build the method and arguments for a pox-5 `stake` or `stake-extend` call.
///
/// pox-5 `stake` signature:
///   (amount-ustx uint) (pox-addr tuple) (start-burn-ht uint)
///   (signer-sig (optional (buff 65))) (signer-key (buff 33))
///   (max-amount uint) (auth-id uint) (num-cycles uint) (unlock-bytes (buff 683))
///
/// pox-5 `stake-extend` signature:
///   (amount-ustx uint) (pox-addr tuple)
///   (signer-sig (optional (buff 65))) (signer-key (buff 33))
///   (max-amount uint) (auth-id uint) (num-cycles uint) (unlock-bytes (buff 683))
fn get_pox5_stacking_tx_method_and_args(
    pox_addr: &PoxAddress,
    pox_addr_tuple: ClarityValue,
    burn_block_height: u128,
    cycle: u128,
    signer_key: &StacksPrivateKey,
    extend_stacking: bool,
    btc_address: &str,
    stx_amount: u64,
    duration: u32,
    auth_id: u128,
) -> (String, Vec<ClarityValue>) {
    let method = if extend_stacking {
        "stake-extend"
    } else {
        "stake"
    };

    let topic = if extend_stacking {
        Pox5SignatureTopic::StakeExtend
    } else {
        Pox5SignatureTopic::Stake
    };

    let signature = make_pox_5_signer_key_signature(
        pox_addr,
        signer_key,
        cycle,
        &topic,
        CHAIN_ID_TESTNET,
        duration.into(),
        stx_amount.into(),
        auth_id,
    )
    .expect("Unable to make pox 5 signature");

    let signer_sig = signature.to_rsv();
    let pub_key = StacksPublicKey::from_private(signer_key);
    let unlock_bytes = build_unlock_script(btc_address);

    let mut arguments = vec![ClarityValue::UInt(stx_amount.into()), pox_addr_tuple];

    if !extend_stacking {
        arguments.push(ClarityValue::UInt(burn_block_height));
    }

    arguments.extend([
        ClarityValue::some(ClarityValue::buff_from(signer_sig).unwrap()).unwrap(),
        ClarityValue::buff_from(pub_key.to_bytes()).unwrap(),
        ClarityValue::UInt(stx_amount.into()),
        ClarityValue::UInt(auth_id),
        ClarityValue::UInt(duration.into()),
        ClarityValue::buff_from(unlock_bytes).unwrap(),
    ]);

    (method.to_string(), arguments)
}

/// Build the arguments for a pox-5 `grant-signer-key` call.
///
/// grant-signer-key signature:
///   (signer-key (buff 33)) (staker principal) (pox-addr (optional tuple))
///   (auth-id uint) (signer-sig (buff 65))
fn get_pox5_grant_signer_key_args(
    stacker_principal: &PrincipalData,
    pox_addr: &PoxAddress,
    signer_key: &StacksPrivateKey,
    auth_id: u128,
) -> Vec<ClarityValue> {
    let pub_key = StacksPublicKey::from_private(signer_key);

    let grant_sig = make_pox_5_signer_grant_signature(
        stacker_principal,
        Some(pox_addr),
        auth_id,
        CHAIN_ID_TESTNET,
        signer_key,
    )
    .expect("Unable to make pox 5 grant signature");

    let pox_addr_clarity = pox_addr
        .clone()
        .as_clarity_tuple()
        .expect("Invalid pox address")
        .into();

    vec![
        ClarityValue::buff_from(pub_key.to_bytes()).unwrap(),
        ClarityValue::Principal(stacker_principal.clone()),
        ClarityValue::some(pox_addr_clarity).unwrap(),
        ClarityValue::UInt(auth_id),
        ClarityValue::buff_from(grant_sig.to_rsv()).unwrap(),
    ]
}

/// Compute the unlock burn height for a pox-5 stake.
/// Must match the stacks-node's internal computation for the expected P2WSH locking script.
///
/// The node computes unlock_height from first_reward_cycle and lock_period using
/// the burnchain's +1 genesis offset:
///   unlock_cycle = first_reward_cycle + duration = (current_cycle + 1) + duration
///   unlock_height = (first_burnchain_block_height + 1) + unlock_cycle * cycle_length + cycle_length / 2
fn compute_unlock_burn_height(
    bitcoin_block_height: u32,
    first_burnchain_block_height: u32,
    reward_cycle_length: u32,
    duration: u32,
) -> u32 {
    let effective_height = bitcoin_block_height.saturating_sub(first_burnchain_block_height);
    let current_cycle = effective_height / reward_cycle_length;
    let first_reward_cycle = current_cycle + 1;
    let unlock_cycle = first_reward_cycle + duration;
    (first_burnchain_block_height + 1)
        + unlock_cycle * reward_cycle_length
        + reward_cycle_length / 2
}

/// Create and broadcast a Bitcoin lockup transaction for pox-5 staking.
///
/// Sends BTC from the stacker's wallet to a P2WSH address derived from the pox-5 locking script.
fn send_pox5_bitcoin_lockup(
    bitcoin_node_host: &str,
    bitcoin_node_username: &str,
    bitcoin_node_password: &str,
    stacker_stx_address: &str,
    btc_address: &str,
    btc_secret_key: &[u8],
    unlock_burn_height: u32,
) -> Result<String, String> {
    use bitcoincore_rpc::bitcoin::blockdata::opcodes;
    use bitcoincore_rpc::bitcoin::blockdata::script::Builder;
    use bitcoincore_rpc::bitcoin::hashes::Hash as _;
    use bitcoincore_rpc::bitcoin::script::PushBytes;
    use bitcoincore_rpc::bitcoin::secp256k1::{Message, PublicKey, Secp256k1, SecretKey};
    use bitcoincore_rpc::bitcoin::sighash::SighashCache;
    use bitcoincore_rpc::bitcoin::{
        Amount, OutPoint, PubkeyHash, ScriptBuf, Sequence, Transaction, TxIn, TxOut, WScriptHash,
        Witness,
    };

    let dbg = |msg: String| {
        let _ = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open("/tmp/pox5-debug.log")
            .and_then(|mut f| {
                use std::io::Write;
                writeln!(f, "{msg}")
            });
    };

    let unlock_bytes = build_unlock_script(btc_address);
    let locking_script =
        build_pox5_locking_script(stacker_stx_address, unlock_burn_height, &unlock_bytes);

    // Create P2WSH output: the scriptPubKey is OP_0 <32-byte-sha256-of-script>
    let script_buf = ScriptBuf::from(locking_script);
    dbg(format!(
        "pox-5 locking_script hex: {}",
        script_buf
            .as_bytes()
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect::<String>(),
    ));
    let script_hash = WScriptHash::hash(script_buf.as_bytes());
    dbg(format!("pox-5 P2WSH script_hash: {script_hash}"));
    let p2wsh_script = ScriptBuf::new_p2wsh(&script_hash);
    dbg(format!(
        "pox-5 P2WSH scriptPubKey hex: {}",
        p2wsh_script
            .as_bytes()
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect::<String>(),
    ));

    let bitcoin_rpc_url = format!("http://{bitcoin_node_host}");
    let client = reqwest::blocking::Client::new();

    // List UTXOs for the stacker's BTC address (uses default wallet where descriptors are registered)
    let listunspent_resp = client
        .post(&bitcoin_rpc_url)
        .basic_auth(bitcoin_node_username, Some(bitcoin_node_password))
        .json(&serde_json::json!({
            "jsonrpc": "1.0",
            "id": "clarinet",
            "method": "listunspent",
            "params": [0, 9999999, [btc_address]]
        }))
        .send()
        .map_err(|e| format!("Failed to list unspent UTXOs: {e}"))?
        .json::<serde_json::Value>()
        .map_err(|e| format!("Failed to parse listunspent response: {e}"))?;

    let utxos = listunspent_resp["result"]
        .as_array()
        .ok_or_else(|| format!("No UTXOs available for pox-5 lockup (address: {btc_address})"))?;

    let lockup_sats: u64 = 5_000;
    let fee_sats: u64 = 1_000;

    let utxo = utxos
        .iter()
        .find(|u| {
            u["amount"]
                .as_f64()
                .map(|a| (a * 100_000_000.0) as u64 >= lockup_sats + fee_sats)
                .unwrap_or(false)
        })
        .ok_or_else(|| {
            format!("No UTXO with sufficient funds for pox-5 lockup (address: {btc_address})")
        })?;

    let txid_str = utxo["txid"].as_str().ok_or("Missing txid in UTXO")?;
    let vout = utxo["vout"].as_u64().ok_or("Missing vout in UTXO")? as u32;
    let utxo_amount_btc = utxo["amount"].as_f64().ok_or("Missing amount in UTXO")?;
    let utxo_amount_sats = (utxo_amount_btc * 100_000_000.0) as u64;
    dbg(format!(
        "pox-5 lockup UTXO: txid={txid_str}, vout={vout}, amount_sats={utxo_amount_sats}, addr={btc_address}",
    ));
    let utxo_script_hex = utxo["scriptPubKey"]
        .as_str()
        .ok_or("Missing scriptPubKey in UTXO")?;

    let txid = txid_str
        .parse()
        .map_err(|e| format!("Failed to parse txid: {e}"))?;

    // Build the transaction
    let input = TxIn {
        previous_output: OutPoint { txid, vout },
        script_sig: ScriptBuf::default(),
        sequence: Sequence(0xFFFFFFFD),
        witness: Witness::new(),
    };

    let lockup_output = TxOut {
        value: Amount::from_sat(lockup_sats),
        script_pubkey: p2wsh_script,
    };

    // Return change to the stacker's address
    let change_sats = utxo_amount_sats - lockup_sats - fee_sats;
    let addr_bytes = btc_address
        .from_base58()
        .expect("Unable to decode btc address");
    let pubkey_hash = PubkeyHash::from_slice(&addr_bytes[1..21]).expect("Invalid hash");
    let change_script = Builder::new()
        .push_opcode(opcodes::all::OP_DUP)
        .push_opcode(opcodes::all::OP_HASH160)
        .push_slice(pubkey_hash)
        .push_opcode(opcodes::all::OP_EQUALVERIFY)
        .push_opcode(opcodes::all::OP_CHECKSIG)
        .into_script();

    let change_output = TxOut {
        value: Amount::from_sat(change_sats),
        script_pubkey: change_script,
    };

    let mut raw_tx = Transaction {
        version: bitcoincore_rpc::bitcoin::transaction::Version::TWO,
        lock_time: bitcoincore_rpc::bitcoin::absolute::LockTime::ZERO,
        input: vec![input],
        output: vec![lockup_output, change_output],
    };

    // Sign the transaction manually using the stacker's private key
    let utxo_script = ScriptBuf::from(hex_bytes(utxo_script_hex).map_err(|e| format!("{e}"))?);
    let sig_hash_all = 0x01u32;
    let sig_hash = SighashCache::new(raw_tx.clone())
        .legacy_signature_hash(0, &utxo_script, sig_hash_all)
        .map_err(|e| format!("Failed to compute sighash: {e}"))?;

    let secp = Secp256k1::new();
    let secret_key =
        SecretKey::from_slice(btc_secret_key).map_err(|e| format!("Invalid secret key: {e}"))?;
    let message =
        Message::from_digest_slice(&sig_hash[..]).map_err(|e| format!("Invalid message: {e}"))?;
    let signature = secp.sign_ecdsa_recoverable(&message, &secret_key);
    let public_key = PublicKey::from_secret_key(&secp, &secret_key);
    let sig_der = signature.to_standard().serialize_der();
    let sig_bytes = [&*sig_der, &[sig_hash_all as u8][..]].concat();

    raw_tx.input[0].script_sig = Builder::new()
        .push_slice(<&PushBytes>::try_from(sig_bytes.as_slice()).unwrap())
        .push_slice(public_key.serialize())
        .into_script();

    // Broadcast the signed transaction
    let raw_tx_hex = bitcoincore_rpc::bitcoin::consensus::encode::serialize_hex(&raw_tx);
    dbg(format!("pox-5 lockup raw_tx_hex: {raw_tx_hex}"));

    let send_resp = client
        .post(&bitcoin_rpc_url)
        .basic_auth(bitcoin_node_username, Some(bitcoin_node_password))
        .json(&serde_json::json!({
            "jsonrpc": "1.0",
            "id": "clarinet",
            "method": "sendrawtransaction",
            "params": [raw_tx_hex]
        }))
        .send()
        .map_err(|e| format!("Failed to broadcast Bitcoin lockup transaction: {e}"))?
        .json::<serde_json::Value>()
        .map_err(|e| format!("Failed to parse sendrawtransaction response: {e}"))?;

    dbg(format!("pox-5 sendrawtransaction response: {send_resp}"));
    if let Some(error) = send_resp["error"].as_object() {
        return Err(format!(
            "Bitcoin lockup transaction failed: {}",
            error["message"].as_str().unwrap_or("unknown error")
        ));
    }

    let lockup_txid = send_resp["result"]
        .as_str()
        .unwrap_or("unknown")
        .to_string();

    // Verify the transaction is in the mempool
    let mempool_resp = client
        .post(&bitcoin_rpc_url)
        .basic_auth(bitcoin_node_username, Some(bitcoin_node_password))
        .json(&serde_json::json!({
            "jsonrpc": "1.0",
            "id": "clarinet",
            "method": "getmempoolentry",
            "params": [&lockup_txid]
        }))
        .send()
        .ok()
        .and_then(|r| r.json::<serde_json::Value>().ok());
    dbg(format!(
        "pox-5 mempool check for {lockup_txid}: {}",
        mempool_resp.map_or("request failed".to_string(), |v| {
            if v["error"].is_null() {
                "IN MEMPOOL".to_string()
            } else {
                format!("NOT IN MEMPOOL: {}", v["error"])
            }
        })
    ));

    Ok(lockup_txid)
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
    let docker_host = config.devnet_config.docker_host.as_deref();
    let output = run_docker_command(&export_command, docker_host)
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

    let output = run_docker_command(&copy_command, docker_host)
        .map_err(|e| format!("Failed to copy events file: {e}"))?;

    if !output.status.success() {
        return Err(format!(
            "Copy command failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

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
            btc_balance: 500_000_000,
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

    /// Verify that the P2WSH script hash computed by clarinet matches
    /// the stacks-node's computation (from RawPox5Entry::to_redeem_script).
    ///
    /// The node uses Bitcoin Script Builder:
    ///   Builder::new()
    ///     .push_slice(&[0x05, version, ...hash160...])
    ///     .push_opcode(OP_DROP)
    ///     .push_scriptint(unlock_height)
    ///     .push_opcode(OP_CLTV)
    ///     .push_opcode(OP_DROP)
    ///     .push_slice(&unlock_bytes)
    #[test]
    fn pox5_locking_script_hash_matches_node() {
        use bitcoincore_rpc::bitcoin::blockdata::opcodes;
        use bitcoincore_rpc::bitcoin::blockdata::script::Builder;
        use bitcoincore_rpc::bitcoin::hashes::Hash as _;
        use bitcoincore_rpc::bitcoin::script::PushBytes;
        use bitcoincore_rpc::bitcoin::WScriptHash;

        let stx_address = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";
        let btc_address = "muYdXKmX9bByAueDe6KFfHd5Ff1gdN9ErG";
        let unlock_height: u32 = 371;

        let unlock_bytes = build_unlock_script(btc_address);
        let locking_script = build_pox5_locking_script(stx_address, unlock_height, &unlock_bytes);

        // Clarinet's P2WSH hash
        let clarinet_hash = WScriptHash::hash(&locking_script);

        // Replicate stacks-node's RawPox5Entry::to_redeem_script() using the
        // same Bitcoin Script Builder approach the node uses
        let stx_addr = <stacks_common::types::chainstate::StacksAddress as stacks_common::types::Address>::from_string(stx_address).unwrap();
        let addr_version = stx_addr.version();
        let addr_hash = stx_addr.bytes().as_bytes();

        let mut principal_data = vec![0x05, addr_version];
        principal_data.extend_from_slice(addr_hash);

        let node_script = Builder::new()
            .push_slice(<&PushBytes>::try_from(principal_data.as_slice()).unwrap())
            .push_opcode(opcodes::all::OP_DROP)
            .push_int(unlock_height.into())
            .push_opcode(opcodes::all::OP_CLTV)
            .push_opcode(opcodes::all::OP_DROP)
            .push_slice(<&PushBytes>::try_from(unlock_bytes.as_slice()).unwrap())
            .into_script();

        let node_hash = WScriptHash::hash(node_script.as_bytes());

        assert_eq!(
            clarinet_hash, node_hash,
            "Clarinet P2WSH hash does not match node's expected hash",
        );

        assert_eq!(
            locking_script,
            node_script.as_bytes(),
            "Locking script bytes mismatch"
        );
    }
}
