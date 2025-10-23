use std::fs;
use std::fs::OpenOptions;
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::mpsc::{self, channel, Sender};

use clarinet_deployments::types::DeploymentSpecification;
use hiro_system_kit::{slog, slog_async, slog_term, Drain};
use stacks_network::chainhook_sdk::types::{BitcoinNetwork, StacksNetwork};
use stacks_network::chainhook_sdk::utils::Context;
use stacks_network::{
    do_run_local_devnet, load_chainhooks, ChainsCoordinatorCommand, DevnetEvent,
    DevnetOrchestrator, LogData,
};

pub struct StartConfig {
    pub devnet: DevnetOrchestrator,
    pub deployment: DeploymentSpecification,
    pub log_tx: Option<Sender<LogData>>,
    pub display_dashboard: bool,
    pub no_snapshot: bool,
    pub save_container_logs: bool,
    pub create_new_snapshot: bool,
}

pub fn start(
    config: StartConfig,
) -> Result<
    (
        Option<mpsc::Receiver<DevnetEvent>>,
        Option<mpsc::Sender<bool>>,
        Option<crossbeam_channel::Sender<ChainsCoordinatorCommand>>,
    ),
    String,
> {
    let hooks = match load_chainhooks(
        &config.devnet.manifest.location,
        &(BitcoinNetwork::Regtest, StacksNetwork::Devnet),
    ) {
        Ok(hooks) => hooks,
        Err(e) => {
            println!("{e}");
            std::process::exit(1);
        }
    };

    let working_dir = config
        .devnet
        .network_config
        .as_ref()
        .and_then(|c| c.devnet.as_ref())
        .map(|d| d.working_dir.to_string())
        .ok_or("unable to read settings/Devnet.toml")?;
    fs::create_dir_all(&working_dir).map_err(|_| format!("unable to create dir {working_dir}"))?;
    let mut log_path = PathBuf::from_str(&working_dir)
        .map_err(|e| format!("unable to working_dir {working_dir}\n{e}"))?;
    log_path.push("devnet.log");

    let file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(log_path)
        .map_err(|e| format!("unable to create log file {e}"))?;

    let decorator = slog_term::PlainDecorator::new(file);
    let drain = slog_term::FullFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();
    let logger = slog::Logger::root(drain, o!());

    let ctx = Context {
        logger: Some(logger),
        tracer: false,
    };

    let (orchestrator_terminated_tx, orchestrator_terminated_rx) = channel();

    let res = hiro_system_kit::nestable_block_on(do_run_local_devnet(
        config.devnet,
        config.deployment,
        &mut Some(hooks),
        config.log_tx,
        config.display_dashboard,
        config.no_snapshot,
        config.create_new_snapshot,
        ctx,
        orchestrator_terminated_tx,
        Some(orchestrator_terminated_rx),
        config.save_container_logs,
    ));
    println!(
        "{} logs and chainstate available at location {}",
        yellow!("\nterminating devnet network:"),
        working_dir
    );

    res
}
