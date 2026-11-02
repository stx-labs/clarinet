mod bitcoin_rpc_client;
pub mod chains_coordinator;
mod command;
mod event;
pub mod event_logger;
mod log;
pub use observer;
pub mod log_event_logger;
mod orchestrator;
mod snapshot_extractor;
mod ui;

use std::sync::mpsc::{channel, Sender};
use std::thread;
use std::thread::sleep;
use std::time::Duration;

use chains_coordinator::{start_chains_coordinator, BitcoinMiningCommand};
use clarinet_deployments::types::DeploymentSpecification;
use clarinet_files::devnet_diff::DevnetDiffConfig;
use clarinet_files::DevnetConfig;
pub use event::DevnetEvent;
use event_logger::DevnetEventLogger;
use hiro_system_kit::slog;
pub use log::{LogData, LogLevel};
pub use log_event_logger::LogEventLogger;
pub use observer::event_handler::MempoolAdmissionData;
use observer::event_handler::ObserverCommand;
pub use observer::types::{BitcoinNetwork, StacksNetwork};
pub use observer::utils::Context;
pub use orchestrator::DevnetOrchestrator;
use orchestrator::{
    get_global_snapshot_dir, get_snapshot_cache_root, DEVNET_SNAPSHOT_READY_MARKER,
};

use self::chains_coordinator::DevnetEventObserverConfig;
use self::snapshot_extractor::{SnapshotImages, SnapshotPlan};

#[allow(dead_code)]
#[derive(Debug)]
pub enum ChainsCoordinatorCommand {
    Terminate,
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

pub fn do_run_local_devnet(
    mut devnet: DevnetOrchestrator,
    deployment: DeploymentSpecification,
    display_dashboard: bool,
    no_snapshot: bool,
    create_new_snapshot: bool,
    ctx: Context,
    save_container_logs: bool,
) -> Result<(), String> {
    let ip_address_setup =
        hiro_system_kit::create_basic_runtime().block_on(devnet.prepare_local_network())?;
    let (devnet_events_tx, devnet_events_rx) = channel();

    let (orchestrator_terminated_tx, orchestrator_terminated_rx) = channel();
    devnet.termination_success_tx = Some(orchestrator_terminated_tx);

    let devnet_config = devnet.devnet_config().clone();

    let differ = DevnetDiffConfig::new();
    let diff = differ.is_compatible(&devnet_config);
    if diff.is_err() {
        let _ = devnet_events_tx.send(DevnetEvent::warning(
            "Default snapshot can not be used".to_string(),
        ));
    }
    // Snapshots are only loaded by projects with the default devnet settings,
    // so one built from other settings would be reused as if it had them.
    let should_create_snapshot = create_new_snapshot && diff.is_ok();
    if create_new_snapshot && !should_create_snapshot {
        let _ = devnet_events_tx.send(DevnetEvent::warning(
            "Ignoring --create-new-snapshot: the devnet settings differ from the defaults"
                .to_string(),
        ));
    }
    // Check for and potentially copy snapshot data.
    // --create-new-snapshot always runs from genesis so we never load an existing snapshot.
    let use_snapshot = !no_snapshot
        && !create_new_snapshot
        && diff.is_ok()
        && prepare_global_snapshot(&devnet_config, &devnet_events_tx);
    let file_appender =
        tracing_appender::rolling::never(&devnet_config.working_dir, "networking.log");
    let (non_blocking, _guard) = tracing_appender::non_blocking(file_appender);
    let _ = tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .with_writer(non_blocking)
        .try_init();

    // The event observer should be able to send some events to the UI thread,
    // and should be able to be terminated
    let devnet_path = devnet_config.working_dir.clone();
    let observer_config = DevnetEventObserverConfig::new(
        devnet_config.clone(),
        devnet.manifest.clone(),
        devnet.network_config().clone(),
        deployment,
        &ctx,
        ip_address_setup,
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

    let ctx_moved = ctx.clone();
    let chains_coordinator_handle = hiro_system_kit::thread_named("Chains coordinator")
        .spawn(move || {
            hiro_system_kit::create_basic_runtime().block_on(start_chains_coordinator(
                observer_config,
                chains_coordinator_tx,
                chains_coordinator_commands_rx,
                moved_chains_coordinator_commands_tx,
                moved_orchestrator_terminator_tx,
                moved_observer_command_tx,
                observer_command_rx,
                moved_mining_command_tx,
                mining_command_rx,
                use_snapshot,
                should_create_snapshot,
                ctx_moved,
            ))
        })
        .expect("unable to retrieve join handle");

    // Let's start the orchestration

    // The devnet orchestrator should be able to send some events to the UI thread,
    // and should be able to be restarted/terminated
    let orchestrator_event_tx = devnet_events_tx.clone();
    let chains_coordinator_commands_tx_moved = chains_coordinator_commands_tx.clone();
    let ctx_moved = ctx.clone();
    let orchestrator_handle = {
        hiro_system_kit::thread_named("Initializing bitcoin node")
            .spawn(move || {
                let moved_orchestrator_event_tx = orchestrator_event_tx.clone();
                let rt = hiro_system_kit::create_basic_runtime();
                let res = rt.block_on(devnet.start(
                    moved_orchestrator_event_tx,
                    terminator_rx,
                    &ctx_moved,
                    !use_snapshot,
                    save_container_logs,
                ));
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
        orchestrator_terminator_tx,
        observer_command_tx,
        mining_command_tx,
        devnet_events_tx.clone(),
    );

    if display_dashboard {
        ctx.try_log(|logger| slog::info!(logger, "Starting Devnet"));

        ui::start_ui(
            devnet_events_tx,
            devnet_events_rx,
            chains_coordinator_commands_tx,
            orchestrator_terminated_rx,
            &devnet_path,
            !devnet_config.bitcoin_controller_automining_disabled,
            &ctx,
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
    } else {
        let mut logger = LogEventLogger::new();
        loop {
            match devnet_events_rx.recv() {
                Ok(DevnetEvent::Log(log)) => {
                    logger.handle_log(log);
                }
                Ok(DevnetEvent::ServiceStatus(status)) => {
                    logger.handle_service_status(status);
                }
                Ok(DevnetEvent::StacksChainEvent(ref chain_event)) => {
                    logger.handle_stacks_chain_event(chain_event);
                }
                Ok(DevnetEvent::BitcoinChainEvent(ref chain_event)) => {
                    logger.handle_bitcoin_chain_event(chain_event);
                }
                Ok(DevnetEvent::MempoolAdmission(tx)) => {
                    logger.handle_mempool_admission(tx);
                }
                Ok(DevnetEvent::ProtocolDeployingProgress(data)) => {
                    logger.handle_protocol_deploying_progress(data);
                }
                Ok(DevnetEvent::BootCompleted(bitcoin_mining_tx)) => {
                    logger.handle_boot_completed();
                    if !devnet_config.bitcoin_controller_automining_disabled {
                        let _ = bitcoin_mining_tx.send(BitcoinMiningCommand::Start);
                    }
                }
                Ok(DevnetEvent::FatalError(e)) => {
                    logger.handle_fatal_error(&e);
                    return Err(e);
                }
                Ok(DevnetEvent::Terminate) => return Ok(()),
                Ok(DevnetEvent::Tick) | Ok(DevnetEvent::KeyEvent(_)) => {}
                Err(_) => return Ok(()),
            }
        }
    }

    Ok(())
}

/// Makes sure the global snapshot was built with the configured images.
/// Returns whether a snapshot can be used.
fn prepare_global_snapshot(
    devnet_config: &DevnetConfig,
    devnet_events_tx: &Sender<DevnetEvent>,
) -> bool {
    if let Err(e) = snapshot_extractor::remove_unkeyed_snapshot(&get_snapshot_cache_root()) {
        let _ = devnet_events_tx.send(DevnetEvent::warning(format!(
            "Failed to remove the devnet snapshot cached by an older Clarinet: {e}"
        )));
    }

    let global_snapshot_dir = get_global_snapshot_dir(devnet_config);
    let cached_marker =
        std::fs::read_to_string(global_snapshot_dir.join(DEVNET_SNAPSHOT_READY_MARKER)).ok();
    let images = SnapshotImages::from(devnet_config);

    match SnapshotPlan::new(cached_marker.as_deref(), &images) {
        SnapshotPlan::UseCached => true,
        SnapshotPlan::RunFromGenesis => {
            let _ = devnet_events_tx.send(DevnetEvent::warning(format!(
                "No devnet snapshot was built with these images, starting from genesis.\n{images}Run with --create-new-snapshot to build one."
            )));
            false
        }
        SnapshotPlan::ExtractEmbedded => {
            match snapshot_extractor::extract_embedded_snapshot(
                &global_snapshot_dir,
                devnet_events_tx,
            ) {
                Ok(()) => true,
                Err(e) => {
                    let _ = devnet_events_tx.send(DevnetEvent::warning(format!(
                        "Failed to extract embedded snapshot: {e}. Continuing without snapshot."
                    )));
                    false
                }
            }
        }
    }
}
