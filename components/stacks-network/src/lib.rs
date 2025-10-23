extern crate serde;

#[macro_use]
extern crate serde_derive;

mod chainhooks;
pub mod chains_coordinator;
mod command;
mod event;
mod log;
mod orchestrator;
mod snapshot_extractor;
mod ui;

use std::sync::mpsc::{self, channel, Receiver, Sender};
use std::thread;
use std::thread::sleep;
use std::time::Duration;

use chainhook_sdk::chainhooks::types::ChainhookStore;
pub use chainhook_sdk::observer::MempoolAdmissionData;
use chainhook_sdk::observer::ObserverCommand;
pub use chainhook_sdk::utils::Context;
pub use chainhook_sdk::{self};
pub use chainhooks::{load_chainhooks, parse_chainhook_full_specification};
use chains_coordinator::{start_chains_coordinator, BitcoinMiningCommand};
use clarinet_deployments::types::DeploymentSpecification;
use clarinet_files::devnet_diff::DevnetDiffConfig;
use clarinet_files::NetworkManifest;
pub use event::DevnetEvent;
use hiro_system_kit::slog;
pub use log::{LogData, LogLevel};
pub use orchestrator::DevnetOrchestrator;
use orchestrator::ServicesMapHosts;

use self::chains_coordinator::DevnetEventObserverConfig;

#[allow(dead_code)]
#[derive(Debug)]
pub enum ChainsCoordinatorCommand {
    Terminate,
}

pub fn block_on<F, R>(future: F) -> R
where
    F: std::future::Future<Output = R>,
{
    let rt = hiro_system_kit::create_basic_runtime();
    rt.block_on(future)
}

/// Sets up signal handlers for SIGINT and SIGTERM on Unix systems
/// On Unix: Handles both SIGINT (Ctrl+C) and SIGTERM (kill) gracefully
/// On Windows: Handles Ctrl+C via ctrlc crate
fn setup_signal_handlers(
    orchestrator_terminator_tx: Sender<bool>,
    observer_command_tx: Sender<ObserverCommand>,
    mining_command_tx: Sender<BitcoinMiningCommand>,
    devnet_events_tx: Sender<DevnetEvent>,
) {
    #[cfg(unix)]
    {
        thread::spawn(move || {
            use signal_hook::consts::{SIGINT, SIGTERM};
            use signal_hook::iterator::Signals;

            let mut signals = match Signals::new([SIGINT, SIGTERM]) {
                Ok(signals) => signals,
                Err(e) => {
                    eprintln!("Failed to setup signal handlers: {}", e);
                    return;
                }
            };

            for sig in signals.forever() {
                match sig {
                    SIGINT | SIGTERM => {
                        let signal_name = if sig == SIGINT { "SIGINT" } else { "SIGTERM" };
                        let _ = devnet_events_tx.send(DevnetEvent::warning(format!(
                            "{} received, initiating graceful shutdown...",
                            signal_name
                        )));

                        // Trigger graceful shutdown
                        let _ = orchestrator_terminator_tx.send(true);
                        let _ = observer_command_tx.send(ObserverCommand::Terminate);
                        let _ = mining_command_tx.send(BitcoinMiningCommand::Pause);
                        sleep(Duration::from_secs(3));
                        let _ = devnet_events_tx.send(DevnetEvent::Terminate);
                        break;
                    }
                    _ => {}
                }
            }
        });
    }

    #[cfg(not(unix))]
    {
        // On Windows and other platforms, use ctrlc for SIGINT/Ctrl+C
        let _ = ctrlc::set_handler(move || {
            let _ = devnet_events_tx.send(DevnetEvent::warning(
                "Ctrl+C received, initiating graceful shutdown...".into(),
            ));
            let _ = orchestrator_terminator_tx.send(true);
            let _ = observer_command_tx.send(ObserverCommand::Terminate);
            let _ = mining_command_tx.send(BitcoinMiningCommand::Pause);
            sleep(Duration::from_secs(3));
            let _ = devnet_events_tx.send(DevnetEvent::Terminate);
        });
    }
}

pub struct DevnetRunConfig {
    pub deployment: DeploymentSpecification,
    pub chainhooks: Option<ChainhookStore>,
    pub log_tx: Option<Sender<LogData>>,
    pub display_dashboard: bool,
    pub no_snapshot: bool,
    pub create_new_snapshot: bool,
    pub ctx: Context,
    pub orchestrator_terminated_tx: Sender<bool>,
    pub orchestrator_terminated_rx: Option<Receiver<bool>>,
    pub ip_address_setup: ServicesMapHosts,
    pub start_local_devnet_services: bool,
    pub network_manifest: Option<NetworkManifest>,
    pub save_container_logs: bool,
}

async fn do_run_devnet(
    mut devnet: DevnetOrchestrator,
    config: DevnetRunConfig,
) -> Result<
    (
        Option<mpsc::Receiver<DevnetEvent>>,
        Option<mpsc::Sender<bool>>,
        Option<crossbeam_channel::Sender<ChainsCoordinatorCommand>>,
    ),
    String,
> {
    let (devnet_events_tx, devnet_events_rx) = channel();

    devnet.termination_success_tx = Some(config.orchestrator_terminated_tx);

    let devnet_config = match devnet.network_config {
        Some(ref network_config) => match &network_config.devnet {
            Some(devnet_config) => Ok(devnet_config.clone()),
            _ => Err("Unable to retrieve config"),
        },
        _ => Err("Unable to retrieve config"),
    }?;

    let differ = DevnetDiffConfig::new();
    let diff = differ.is_compatible(&devnet_config);
    if diff.is_err() {
        let _ = devnet_events_tx.send(DevnetEvent::warning(
            "Default snapshot can not be used".to_string(),
        ));
    }
    // Check for and potentially copy snapshot data
    if config.start_local_devnet_services && !config.no_snapshot && diff.is_ok() {
        let global_snapshot_dir = orchestrator::get_global_snapshot_dir();

        // First, try to extract embedded snapshot if it exists and we don't have snapshot yet
        let global_snapshot_ready = global_snapshot_dir.join("epoch_3_ready").exists();

        if !global_snapshot_ready {
            let _ = devnet_events_tx.send(DevnetEvent::info(
                "No existing snapshot found, extracting embedded snapshot data...".to_string(),
            ));

            match snapshot_extractor::extract_embedded_snapshot(
                &global_snapshot_dir,
                &devnet_events_tx,
            ) {
                Ok(true) => {
                    let _ = devnet_events_tx.send(DevnetEvent::success(
                        "Embedded snapshot extracted successfully".to_string(),
                    ));
                }
                Ok(false) => {
                    let _ = devnet_events_tx.send(DevnetEvent::warning(
                        "No embedded snapshot available".to_string(),
                    ));
                }
                Err(e) => {
                    let _ = devnet_events_tx.send(DevnetEvent::warning(format!(
                        "Failed to extract embedded snapshot: {e}. Continuing without snapshot."
                    )));
                }
            }
        }
    }
    // if we're starting all services, all trace logs go to networking.log
    if config.start_local_devnet_services {
        let file_appender =
            tracing_appender::rolling::never(&devnet_config.working_dir, "networking.log");
        let (non_blocking, _guard) = tracing_appender::non_blocking(file_appender);

        let _ = tracing_subscriber::fmt()
            .with_max_level(tracing::Level::INFO)
            .with_writer(non_blocking)
            .try_init();
    } else {
        // for the devnet, we can't write to a file, so we log everything to stdout, but we still want to set
        // the max trace level so we don't get too much information in the logs
        let _ = tracing_subscriber::fmt()
            .with_max_level(tracing::Level::INFO)
            .try_init();
    }

    // The event observer should be able to send some events to the UI thread,
    // and should be able to be terminated
    let hooks = config.chainhooks.unwrap_or_default();
    let devnet_path = devnet_config.working_dir.clone();
    let observer_config = DevnetEventObserverConfig::new(
        devnet_config.clone(),
        devnet.manifest.clone(),
        config.network_manifest,
        config.deployment,
        hooks,
        &config.ctx,
        config.ip_address_setup,
    );

    let chains_coordinator_tx = devnet_events_tx.clone();
    let (chains_coordinator_commands_tx, chains_coordinator_commands_rx) =
        crossbeam_channel::unbounded();
    let (orchestrator_terminator_tx, terminator_rx) = channel();
    let (observer_command_tx, observer_command_rx) = channel();
    let (mining_command_tx, mining_command_rx) = channel();
    let moved_mining_command_tx = mining_command_tx.clone();
    let moved_orchestrator_terminator_tx = orchestrator_terminator_tx.clone();
    let moved_chains_coordinator_commands_tx = chains_coordinator_commands_tx.clone();
    let moved_observer_command_tx = observer_command_tx.clone();

    let ctx_moved = config.ctx.clone();
    let chains_coordinator_handle = hiro_system_kit::thread_named("Chains coordinator")
        .spawn(move || {
            let future = start_chains_coordinator(
                observer_config,
                chains_coordinator_tx,
                chains_coordinator_commands_rx,
                moved_chains_coordinator_commands_tx,
                moved_orchestrator_terminator_tx,
                moved_observer_command_tx,
                observer_command_rx,
                moved_mining_command_tx,
                mining_command_rx,
                !config.no_snapshot,
                config.create_new_snapshot,
                ctx_moved,
            );
            let rt = hiro_system_kit::create_basic_runtime();
            rt.block_on(future)
        })
        .expect("unable to retrieve join handle");

    // Let's start the orchestration

    // The devnet orchestrator should be able to send some events to the UI thread,
    // and should be able to be restarted/terminated
    let orchestrator_event_tx = devnet_events_tx.clone();
    let chains_coordinator_commands_tx_moved = chains_coordinator_commands_tx.clone();
    let ctx_moved = config.ctx.clone();
    let orchestrator_handle = {
        hiro_system_kit::thread_named("Initializing bitcoin node")
            .spawn(move || {
                let moved_orchestrator_event_tx = orchestrator_event_tx.clone();
                let res = if config.start_local_devnet_services {
                    let future = devnet.start(
                        moved_orchestrator_event_tx,
                        terminator_rx,
                        &ctx_moved,
                        config.no_snapshot,
                        config.save_container_logs,
                    );
                    let rt = hiro_system_kit::create_basic_runtime();
                    rt.block_on(future)
                } else {
                    let future = devnet
                        .initialize_bitcoin_node(&moved_orchestrator_event_tx, config.no_snapshot);
                    let rt = hiro_system_kit::create_basic_runtime();
                    rt.block_on(future)
                };
                if let Err(ref e) = res {
                    let _ = orchestrator_event_tx.send(DevnetEvent::FatalError(e.clone()));
                    let _ = chains_coordinator_commands_tx_moved
                        .send(ChainsCoordinatorCommand::Terminate);
                }
                res
            })
            .expect("unable to retrieve join handle")
    };

    // Set up signal handlers for graceful shutdown (SIGINT and SIGTERM)
    setup_signal_handlers(
        orchestrator_terminator_tx.clone(),
        observer_command_tx,
        mining_command_tx,
        devnet_events_tx.clone(),
    );

    if config.display_dashboard {
        config
            .ctx
            .try_log(|logger| slog::info!(logger, "Starting Devnet"));

        let moved_chains_coordinator_commands_tx = chains_coordinator_commands_tx.clone();
        ui::start_ui(
            devnet_events_tx,
            devnet_events_rx,
            moved_chains_coordinator_commands_tx,
            config.orchestrator_terminated_rx.expect(
                "orchestrator_terminated_rx should be provided when display_dashboard set to true",
            ),
            &devnet_path,
            !devnet_config.bitcoin_controller_automining_disabled,
            &config.ctx,
        )?;

        if let Err(e) = chains_coordinator_handle.join() {
            if let Ok(message) = e.downcast::<String>() {
                return Err(*message);
            }
        }

        if let Err(e) = orchestrator_handle.join() {
            if let Ok(message) = e.downcast::<String>() {
                return Err(*message);
            }
        }
    } else if config.log_tx.is_none() {
        loop {
            match devnet_events_rx.recv() {
                Ok(DevnetEvent::Log(log)) => {
                    if let Some(ref log_tx) = config.log_tx {
                        let _ = log_tx.send(log.clone());
                    } else {
                        match log.level {
                            LogLevel::Debug => config
                                .ctx
                                .try_log(|logger| slog::debug!(logger, "{}", log.message)),
                            LogLevel::Info | LogLevel::Success => config
                                .ctx
                                .try_log(|logger| slog::info!(logger, "{}", log.message)),
                            LogLevel::Warning => config
                                .ctx
                                .try_log(|logger| slog::warn!(logger, "{}", log.message)),
                            LogLevel::Error => config
                                .ctx
                                .try_log(|logger| slog::error!(logger, "{}", log.message)),
                        }
                    }
                }
                Ok(DevnetEvent::BootCompleted(bitcoin_mining_tx)) => {
                    if !devnet_config.bitcoin_controller_automining_disabled {
                        let _ = bitcoin_mining_tx.send(BitcoinMiningCommand::Start);
                    }
                }
                Ok(DevnetEvent::FatalError(e)) => return Err(e),
                Ok(DevnetEvent::Terminate) => return Ok((None, None, None)),
                _ => {}
            }
        }
    } else {
        return Ok((
            Some(devnet_events_rx),
            Some(orchestrator_terminator_tx),
            Some(chains_coordinator_commands_tx),
        ));
    }

    Ok((None, None, Some(chains_coordinator_commands_tx)))
}

pub async fn do_run_chain_coordinator(
    mut devnet: DevnetOrchestrator,
    deployment: DeploymentSpecification,
    chainhooks: &mut Option<ChainhookStore>,
    log_tx: Option<Sender<LogData>>,
    no_snapshot: bool,
    ctx: Context,
    orchestrator_terminated_tx: Sender<bool>,
    namespace: &str,
    network_manifest: NetworkManifest,
) -> Result<
    (
        Option<mpsc::Receiver<DevnetEvent>>,
        Option<mpsc::Sender<bool>>,
        Option<crossbeam_channel::Sender<ChainsCoordinatorCommand>>,
    ),
    String,
> {
    let ip_address_setup = devnet.prepare_network_k8s_coordinator(namespace)?;
    let config = DevnetRunConfig {
        deployment,
        chainhooks: chainhooks.take(),
        log_tx,
        display_dashboard: false,
        no_snapshot,
        create_new_snapshot: false,
        ctx,
        orchestrator_terminated_tx,
        orchestrator_terminated_rx: None,
        ip_address_setup,
        start_local_devnet_services: false,
        network_manifest: Some(network_manifest),
        save_container_logs: false,
    };
    do_run_devnet(devnet, config).await
}

pub async fn do_run_local_devnet(
    mut devnet: DevnetOrchestrator,
    deployment: DeploymentSpecification,
    chainhooks: &mut Option<ChainhookStore>,
    log_tx: Option<Sender<LogData>>,
    display_dashboard: bool,
    no_snapshot: bool,
    create_new_snapshot: bool,
    ctx: Context,
    orchestrator_terminated_tx: Sender<bool>,
    orchestrator_terminated_rx: Option<Receiver<bool>>,
    save_container_logs: bool,
) -> Result<
    (
        Option<mpsc::Receiver<DevnetEvent>>,
        Option<mpsc::Sender<bool>>,
        Option<crossbeam_channel::Sender<ChainsCoordinatorCommand>>,
    ),
    String,
> {
    let ip_address_setup = devnet.prepare_local_network().await?;
    let config = DevnetRunConfig {
        deployment,
        chainhooks: chainhooks.take(),
        log_tx,
        display_dashboard,
        no_snapshot,
        create_new_snapshot,
        ctx,
        orchestrator_terminated_tx,
        orchestrator_terminated_rx,
        ip_address_setup,
        start_local_devnet_services: true,
        network_manifest: None,
        save_container_logs,
    };
    do_run_devnet(devnet, config).await
}
