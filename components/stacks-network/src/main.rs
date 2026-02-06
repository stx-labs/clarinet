use std::path::PathBuf;
use std::sync::mpsc::channel;

use chainhook_sdk::types::{BitcoinNetwork, StacksNetwork};
use clap::Parser;
use clarinet_files::{FileLocation, NetworkManifest, ProjectManifest};
use hiro_system_kit::slog;
use stacks_network::{do_run_chain_coordinator, load_chainhooks, Context, DevnetOrchestrator};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Devnet namespace
    #[clap(short, long)]
    namespace: String,
    /// Path of the project manifest to load
    #[clap(short, long)]
    manifest_path: Option<String>,
    /// Path of the network manifest to load
    #[clap(short, long)]
    network_manifest_path: Option<String>,
    /// Path of the deployment plan
    #[clap(short, long)]
    deployment_plan_path: Option<String>,
    /// Path of the project's root
    #[clap(short, long)]
    project_root_path: Option<String>,
    no_snapshot: bool,
}

fn main() {
    let args = Args::parse();
    let manifest_location = get_config_location_from_path_or_exit(&args.manifest_path);
    let network_manifest_path = get_config_location_from_path_or_exit(&args.network_manifest_path);
    let deployment_location = get_config_location_from_path_or_exit(&args.deployment_plan_path);

    let project_manifest_file_content = manifest_location
        .read_content()
        .unwrap_or_else(|e| panic!("failed to read manifest data {e:?}"));

    let manifest: ProjectManifest = yaml_serde::from_slice(&project_manifest_file_content[..])
        .unwrap_or_else(|e| panic!("Clarinet.toml file malformatted {e:?}"));

    let network_manifest_file_content = network_manifest_path
        .read_content()
        .unwrap_or_else(|e| panic!("failed to read network manifest data {e:?}"));

    let network_manifest: NetworkManifest =
        yaml_serde::from_slice(&network_manifest_file_content[..])
            .unwrap_or_else(|e| panic!("Devnet.toml file malformatted {e:?}"));

    let orchestrator =
        DevnetOrchestrator::new(manifest, Some(network_manifest.clone()), None, false, false)
            .unwrap();

    let deployment_specification_file_content = deployment_location
        .read_content()
        .unwrap_or_else(|e| panic!("failed to read manifest data {e:?}"));
    let deployment = yaml_serde::from_slice(&deployment_specification_file_content)
        .unwrap_or_else(|e| panic!("deployment plan malformatted {e:?}"));

    let chainhooks = match load_chainhooks(
        &manifest_location,
        &(BitcoinNetwork::Regtest, StacksNetwork::Devnet),
    ) {
        Ok(hooks) => hooks,
        Err(e) => {
            panic!("failed to load chainhooks {e}");
        }
    };

    let logger = hiro_system_kit::log::setup_logger();
    let _guard = hiro_system_kit::log::setup_global_logger(logger.clone());
    let ctx = Context {
        logger: Some(logger),
        tracer: false,
    };
    ctx.try_log(|logger| slog::info!(logger, "starting devnet coordinator"));

    let (orchestrator_terminated_tx, _) = channel();
    let res = hiro_system_kit::nestable_block_on(do_run_chain_coordinator(
        orchestrator,
        deployment,
        &mut Some(chainhooks),
        None,
        args.no_snapshot,
        ctx,
        orchestrator_terminated_tx,
        &args.namespace,
        network_manifest,
    ));
    println!("{:?}", res.unwrap());
}

fn get_config_location_from_path_or_exit(path: &Option<String>) -> FileLocation {
    if let Some(path) = path {
        let path_buf = PathBuf::from(path);
        if !path_buf.exists() {
            std::process::exit(1);
        }
        FileLocation::from_path(path_buf)
    } else {
        std::process::exit(1);
    }
}
