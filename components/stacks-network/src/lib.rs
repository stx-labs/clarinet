mod bitcoin_rpc_client;
pub mod chains_coordinator;
mod docker_files;
mod event;
pub mod event_logger;
mod log;
pub use observer;
pub mod log_event_logger;
mod orchestrator;
mod snapshot_extractor;
mod ui;

use std::sync::mpsc::{channel, Sender};

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
use tokio::sync::mpsc::{unbounded_channel, UnboundedSender};

use self::chains_coordinator::DevnetEventObserverConfig;
use self::snapshot_extractor::{SnapshotImages, SnapshotPlan};

#[allow(dead_code)]
#[derive(Debug)]
pub enum ChainsCoordinatorCommand {
    Terminate,
}

/// Shuts the devnet down gracefully on SIGINT (Ctrl+C) and SIGTERM (kill) on
/// Unix, and on Ctrl+C and Ctrl+Break on Windows.
/// A second signal exits right away, leaving the devnet containers behind.
fn setup_signal_handlers(
    orchestrator_terminator_tx: UnboundedSender<()>,
    observer_command_tx: Sender<ObserverCommand>,
    mining_command_tx: Sender<BitcoinMiningCommand>,
    devnet_events_tx: Sender<DevnetEvent>,
    display_dashboard: bool,
) {
    let mut shutting_down = false;
    let handle_signal = move |signal_name: &str, exit_code: i32| {
        if shutting_down {
            if display_dashboard {
                ui::restore_terminal();
            }
            eprintln!("{signal_name} received again, exiting without removing devnet containers");
            std::process::exit(exit_code);
        }
        shutting_down = true;
        let _ = devnet_events_tx.send(DevnetEvent::warning(format!(
            "{signal_name} received, initiating graceful shutdown..."
        )));
        // The orchestrator sends DevnetEvent::Terminate once the containers are removed
        let _ = orchestrator_terminator_tx.send(());
        let _ = observer_command_tx.send(ObserverCommand::Terminate);
        let _ = mining_command_tx.send(BitcoinMiningCommand::Pause);
    };

    hiro_system_kit::thread_named("Signal handler")
        .spawn(move || {
            let watching = hiro_system_kit::create_basic_runtime()
                .block_on(watch_shutdown_signals(handle_signal));
            if let Err(e) = watching {
                eprintln!("Failed to setup signal handlers: {e}");
            }
        })
        .expect("unable to retrieve join handle");
}

#[cfg(unix)]
async fn watch_shutdown_signals(mut handle_signal: impl FnMut(&str, i32)) -> std::io::Result<()> {
    use tokio::signal::unix::{signal, SignalKind};

    let mut sigint = signal(SignalKind::interrupt())?;
    let mut sigterm = signal(SignalKind::terminate())?;
    loop {
        // Exit codes are 128 + the signal number
        tokio::select! {
            Some(()) = sigint.recv() => handle_signal("SIGINT", 130),
            Some(()) = sigterm.recv() => handle_signal("SIGTERM", 143),
            else => return Ok(()),
        }
    }
}

#[cfg(windows)]
async fn watch_shutdown_signals(mut handle_signal: impl FnMut(&str, i32)) -> std::io::Result<()> {
    use tokio::signal::windows::{ctrl_break, ctrl_c};

    let mut ctrl_c = ctrl_c()?;
    let mut ctrl_break = ctrl_break()?;
    loop {
        tokio::select! {
            Some(()) = ctrl_c.recv() => handle_signal("Ctrl+C", 130),
            Some(()) = ctrl_break.recv() => handle_signal("Ctrl+Break", 130),
            else => return Ok(()),
        }
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
        devnet.docker()?.clone(),
        devnet.stacks_api_container_name(),
    );

    let chains_coordinator_tx = devnet_events_tx.clone();
    let (chains_coordinator_commands_tx, chains_coordinator_commands_rx) =
        crossbeam_channel::unbounded();
    let (orchestrator_terminator_tx, terminator_rx) = unbounded_channel();
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
    // and should be able to be terminated
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
                match &res {
                    Ok(()) => {
                        let _ = orchestrator_event_tx.send(DevnetEvent::Terminate);
                    }
                    Err(e) => {
                        let _ = orchestrator_event_tx.send(DevnetEvent::FatalError(e.clone()));
                        let _ = chains_coordinator_commands_tx_moved
                            .send(ChainsCoordinatorCommand::Terminate);
                    }
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
        display_dashboard,
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

        // The UI can return before seeing the error, e.g. when the user quits during teardown
        match orchestrator_handle.join() {
            Ok(result) => result?,
            Err(e) => {
                if let Ok(message) = e.downcast::<String>() {
                    return Err(*message);
                }
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
