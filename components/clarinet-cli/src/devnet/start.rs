use std::fs;
use std::fs::OpenOptions;
use std::path::PathBuf;
use std::str::FromStr;

use clarinet_deployments::types::DeploymentSpecification;
use hiro_system_kit::{slog, slog_async, slog_term, Drain};
use stacks_network::{do_run_local_devnet, Context, DevnetOrchestrator};

pub struct StartConfig {
    pub devnet: DevnetOrchestrator,
    pub deployment: DeploymentSpecification,
    pub display_dashboard: bool,
    pub no_snapshot: bool,
    pub save_container_logs: bool,
    pub create_new_snapshot: bool,
}

pub fn start(config: StartConfig) -> Result<(), String> {
    let working_dir = config.devnet.devnet_config().working_dir.clone();
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

    let res = do_run_local_devnet(
        config.devnet,
        config.deployment,
        config.display_dashboard,
        config.no_snapshot,
        config.create_new_snapshot,
        ctx,
        config.save_container_logs,
    );
    println!(
        "{} logs and chainstate available at location {}",
        yellow!("\nterminating devnet network:"),
        working_dir
    );

    res
}
