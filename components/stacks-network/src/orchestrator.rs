use std::collections::HashMap;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{Receiver, Sender};
use std::time::Duration;

use bollard::container::{
    Config, CreateContainerOptions, KillContainerOptions, ListContainersOptions, LogsOptions,
    PruneContainersOptions, WaitContainerOptions,
};
use bollard::errors::Error as DockerError;
use bollard::image::CreateImageOptions;
use bollard::models::{HostConfig, PortBinding};
use bollard::network::{CreateNetworkOptions, PruneNetworksOptions};
use bollard::service::Ipam;
use bollard::Docker;
use chainhook_sdk::bitcoin::hex::DisplayHex;
use chainhook_sdk::utils::Context;
use clarinet_deployments::types::BurnchainEpochConfig;
use clarinet_files::{
    DevnetConfig, DevnetConfigFile, NetworkManifest, ProjectManifest, StacksNetwork,
    DEFAULT_DOCKER_PLATFORM,
};
use clarity::types::chainstate::StacksPrivateKey;
use clarity::types::PrivateKey;
use futures::stream::TryStreamExt;
use hiro_system_kit::{slog, slog_term, Drain};
use indoc::formatdoc;
use reqwest::RequestBuilder;
use serde_json::Value as JsonValue;

use crate::command::run_command;
use crate::event::{send_status_update, DevnetEvent, Status};

const BITCOIND_DATA_DIR: &str = "/home/bitcoin/.bitcoin";

const DOCKER_ERR_MSG: &str = "unable to get docker client";

#[derive(Debug)]
pub struct DevnetOrchestrator {
    pub name: String,
    network_name: String,
    pub manifest: ProjectManifest,
    pub network_config: Option<NetworkManifest>,
    pub termination_success_tx: Option<Sender<bool>>,
    pub can_exit: bool,
    pub logger: Option<slog::Logger>,
    stacks_node_container_id: Option<String>,
    stacks_signers_containers_ids: Vec<String>,
    stacks_api_container_id: Option<String>,
    stacks_explorer_container_id: Option<String>,
    bitcoin_node_container_id: Option<String>,
    bitcoin_explorer_container_id: Option<String>,
    postgres_container_id: Option<String>,
    docker_client: Option<Docker>,
    services_map_hosts: Option<ServicesMapHosts>,
    save_container_logs: bool,
}
#[derive(Clone, Debug)]
pub struct ServicesMapHosts {
    pub bitcoin_node_host: String,
    pub stacks_node_host: String,
    pub stacks_api_host: String,
    pub postgres_host: String,
    pub stacks_explorer_host: String,
    pub bitcoin_explorer_host: String,
}

pub static EXCLUDED_STACKS_SNAPSHOT_FILES: &[&str] =
    &["event_observers.sqlite", "event_observers.sqlite-journal"];

impl DevnetOrchestrator {
    pub fn new(
        manifest: ProjectManifest,
        network_manifest: Option<NetworkManifest>,
        devnet_override: Option<DevnetConfigFile>,
        should_use_docker: bool,
        log_to_stdout: bool,
    ) -> Result<DevnetOrchestrator, String> {
        let mut network_config = match network_manifest {
            Some(n) => Ok(n),
            None => NetworkManifest::from_project_manifest_location(
                &manifest.location,
                &StacksNetwork::Devnet.get_networks(),
                false,
                Some(&manifest.project.cache_location),
                devnet_override,
            ),
        }?;
        if let Some(ref mut devnet) = network_config.devnet {
            let working_dir = PathBuf::from(&devnet.working_dir);
            let devnet_path = if working_dir.is_absolute() {
                working_dir
            } else {
                let mut cwd = std::env::current_dir()
                    .map_err(|e| format!("unable to retrieve current dir ({e})"))?;
                cwd.push(&working_dir);
                let _ = fs::create_dir(&cwd);
                cwd.canonicalize().map_err(|e| {
                    format!(
                        "unable to canonicalize working_dir {} ({})",
                        working_dir.display(),
                        e
                    )
                })?
            };
            devnet.working_dir = format!("{}", devnet_path.display());
        }

        let name = manifest.project.name.to_string();
        let mut network_name = name.clone();
        if let Some(ref devnet) = network_config.devnet {
            if let Some(ref network_id) = devnet.network_id {
                network_name.push_str(&format!(".{network_id}"));
            }
            network_name.push_str(&format!(".{}", devnet.name));
        } else {
            network_name.push_str(".net");
        }

        let docker_client = match should_use_docker {
            true => match network_config.devnet {
                Some(ref devnet) => {
                    let client = Docker::connect_with_socket(
                        &devnet.docker_host,
                        120,
                        bollard::API_DEFAULT_VERSION,
                    )
                    .or_else(|_| Docker::connect_with_socket_defaults())
                    .or_else(|_| {
                        let mut user_space_docker_socket =
                            dirs::home_dir().expect("unable to retrieve homedir");
                        user_space_docker_socket.push(".docker");
                        user_space_docker_socket.push("run");
                        user_space_docker_socket.push("docker.sock");
                        Docker::connect_with_socket(
                            user_space_docker_socket.to_str().unwrap(),
                            120,
                            bollard::API_DEFAULT_VERSION,
                        )
                    })
                    .map_err(|e| format!("unable to connect to docker: {e:?}"))?;
                    Some(client)
                }
                None => unreachable!(),
            },
            false => None,
        };

        let logger = if log_to_stdout {
            let plain = slog_term::PlainSyncDecorator::new(std::io::stdout());
            let logger =
                slog::Logger::root(slog_term::FullFormat::new(plain).build().fuse(), slog::o!());
            Some(logger)
        } else {
            None
        };
        Ok(DevnetOrchestrator {
            name,
            network_name,
            manifest,
            network_config: Some(network_config),
            docker_client,
            can_exit: true,
            logger,
            termination_success_tx: None,
            stacks_node_container_id: None,
            stacks_signers_containers_ids: vec![],
            stacks_api_container_id: None,
            stacks_explorer_container_id: None,
            bitcoin_node_container_id: None,
            bitcoin_explorer_container_id: None,
            postgres_container_id: None,
            services_map_hosts: None,
            save_container_logs: false,
        })
    }

    fn get_devnet_config(&self) -> Result<&DevnetConfig, String> {
        self.network_config
            .as_ref()
            .and_then(|config| config.devnet.as_ref())
            .ok_or_else(|| "unable to get devnet configuration".to_string())
    }

    pub fn prepare_network_k8s_coordinator(
        &mut self,
        namespace: &str,
    ) -> Result<ServicesMapHosts, String> {
        let services_map_hosts = ServicesMapHosts {
            bitcoin_node_host: format!(
                "bitcoind-chain-coordinator.{namespace}.svc.cluster.local:18443"
            ),
            stacks_node_host: format!("stacks-blockchain.{namespace}.svc.cluster.local:20443"),
            postgres_host: format!("stacks-blockchain-api.{namespace}.svc.cluster.local:5432"),
            stacks_api_host: format!("stacks-blockchain-api.{namespace}.svc.cluster.local:3999"),
            stacks_explorer_host: "localhost".into(), // todo (micaiah)
            bitcoin_explorer_host: "localhost".into(), // todo (micaiah)
        };

        self.services_map_hosts = Some(services_map_hosts.clone());

        Ok(services_map_hosts)
    }

    pub async fn prepare_local_network(&mut self) -> Result<ServicesMapHosts, String> {
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.get_devnet_config()?;

        // prune any staled resources from previous sessions
        self.clean_previous_session().await?;

        let mut labels = HashMap::new();
        labels.insert("project", self.network_name.as_str());

        let mut options = HashMap::new();
        options.insert("enable_ip_masquerade", "true");
        options.insert("enable_icc", "true");
        options.insert("host_binding_ipv4", "0.0.0.0");
        options.insert("com.docker.network.bridge.enable_icc", "true");
        options.insert("com.docker.network.bridge.enable_ip_masquerade", "true");
        options.insert("com.docker.network.bridge.host_binding_ipv4", "0.0.0.0");

        let network_id = docker
            .create_network::<&str>(CreateNetworkOptions {
                name: &self.network_name,
                driver: "bridge",
                ipam: Ipam {
                    ..Default::default()
                },
                labels,
                options,
                ..Default::default()
            })
            .await
            .map_err(|e| {
                format!(
                    "clarinet was unable to create network. Is docker running locally? (error: {e})"
                )
            })?
            .id
            .ok_or("unable to retrieve network_id")?;

        let res = docker
            .inspect_network::<&str>(&network_id, None)
            .await
            .map_err(|e| format!("unable to retrieve network: {e}"))?;

        let gateway = res
            .ipam
            .as_ref()
            .and_then(|ipam| ipam.config.as_ref())
            .and_then(|config| config.first())
            .and_then(|map| map.gateway.clone())
            .ok_or("unable to retrieve gateway")?;

        let services_map_hosts = if devnet_config.use_docker_gateway_routing {
            ServicesMapHosts {
                bitcoin_node_host: format!("{}:{}", gateway, devnet_config.bitcoin_node_rpc_port),
                stacks_node_host: format!("{}:{}", gateway, devnet_config.stacks_node_rpc_port),
                postgres_host: format!("{}:{}", gateway, devnet_config.postgres_port),
                stacks_api_host: format!("{}:{}", gateway, devnet_config.stacks_api_port),
                stacks_explorer_host: format!("{}:{}", gateway, devnet_config.stacks_explorer_port),
                bitcoin_explorer_host: format!(
                    "{}:{}",
                    gateway, devnet_config.bitcoin_explorer_port
                ),
            }
        } else {
            ServicesMapHosts {
                bitcoin_node_host: format!("localhost:{}", devnet_config.bitcoin_node_rpc_port),
                stacks_node_host: format!("localhost:{}", devnet_config.stacks_node_rpc_port),
                postgres_host: format!("localhost:{}", devnet_config.postgres_port),
                stacks_api_host: format!("localhost:{}", devnet_config.stacks_api_port),
                stacks_explorer_host: format!("localhost:{}", devnet_config.stacks_explorer_port),
                bitcoin_explorer_host: format!("localhost:{}", devnet_config.bitcoin_explorer_port),
            }
        };

        self.services_map_hosts = Some(services_map_hosts.clone());

        Ok(services_map_hosts)
    }

    pub async fn start(
        &mut self,
        event_tx: Sender<DevnetEvent>,
        terminator_rx: Receiver<bool>,
        ctx: &Context,
        no_snapshot: bool,
        save_container_logs: bool,
    ) -> Result<(), String> {
        self.save_container_logs = save_container_logs;
        let devnet_config = self.get_devnet_config()?;

        let mut boot_index = 1;

        let _ = event_tx.send(DevnetEvent::info(format!(
            "Initiating Devnet boot sequence (working_dir: {})",
            devnet_config.working_dir
        )));
        let mut devnet_path = PathBuf::from(&devnet_config.working_dir);
        devnet_path.push("data");

        let signers_keys = devnet_config.stacks_signers_keys.clone();

        let disable_postgres = devnet_config.disable_postgres;
        let disable_stacks_api = devnet_config.disable_stacks_api;
        let disable_stacks_explorer = devnet_config.disable_stacks_explorer;
        let disable_bitcoin_explorer = devnet_config.disable_bitcoin_explorer;

        let _ = fs::create_dir(&devnet_config.working_dir);
        let _ = fs::create_dir(format!("{}/conf", devnet_config.working_dir));
        let _ = fs::create_dir(format!("{}/data", devnet_config.working_dir));

        let bitcoin_explorer_port = devnet_config.bitcoin_explorer_port;
        let stacks_explorer_port = devnet_config.stacks_explorer_port;
        let stacks_api_port = devnet_config.stacks_api_port;

        send_status_update(
            &event_tx,
            &self.logger,
            "bitcoin-node",
            Status::Red,
            "initializing",
        );

        send_status_update(
            &event_tx,
            &self.logger,
            "stacks-node",
            Status::Red,
            "initializing",
        );

        send_status_update(
            &event_tx,
            &self.logger,
            "stacks-signers",
            Status::Red,
            "initializing",
        );

        if !disable_stacks_api {
            send_status_update(
                &event_tx,
                &self.logger,
                "stacks-api",
                Status::Red,
                "initializing",
            );
        }
        if !disable_stacks_explorer {
            send_status_update(
                &event_tx,
                &self.logger,
                "stacks-explorer",
                Status::Red,
                "initializing",
            );
        }
        if !disable_bitcoin_explorer {
            send_status_update(
                &event_tx,
                &self.logger,
                "bitcoin-explorer",
                Status::Red,
                "initializing",
            );
        }

        let _ = event_tx.send(DevnetEvent::info(format!(
            "Creating network {}",
            self.network_name
        )));

        // Start bitcoind
        let _ = event_tx.send(DevnetEvent::info("Starting bitcoin-node".to_string()));
        send_status_update(
            &event_tx,
            &self.logger,
            "bitcoin-node",
            Status::Yellow,
            "preparing container",
        );
        match self.prepare_bitcoin_node_container(ctx, no_snapshot).await {
            Ok(_) => {}
            Err(message) => {
                let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                self.kill(ctx, Some(&message)).await;
                return Err(message);
            }
        };
        send_status_update(
            &event_tx,
            &self.logger,
            "bitcoin-node",
            Status::Yellow,
            "booting",
        );
        match self
            .boot_bitcoin_node_container(&event_tx, no_snapshot)
            .await
        {
            Ok(_) => {
                self.initialize_bitcoin_node(&event_tx, no_snapshot).await?;
            }
            Err(message) => {
                let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                self.kill(ctx, Some(&message)).await;
                return Err(message);
            }
        };

        // Start postgres container
        if !disable_postgres {
            let _ = event_tx.send(DevnetEvent::info("Starting postgres".to_string()));
            match self.prepare_postgres_container(ctx).await {
                Ok(_) => {}
                Err(message) => {
                    let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                    self.kill(ctx, Some(&message)).await;
                    return Err(message);
                }
            };
            match self.boot_postgres_container(ctx).await {
                Ok(_) => {}
                Err(message) => {
                    let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                    self.kill(ctx, Some(&message)).await;
                    return Err(message);
                }
            };
        };
        // Start stacks-api
        if !disable_stacks_api {
            send_status_update(
                &event_tx,
                &self.logger,
                "stacks-api",
                Status::Yellow,
                "preparing container",
            );
            let _ = event_tx.send(DevnetEvent::info("Starting stacks-api".to_string()));
            match self.prepare_stacks_api_container(ctx).await {
                Ok(_) => {}
                Err(message) => {
                    let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                    self.kill(ctx, Some(&message)).await;
                    return Err(message);
                }
            };

            send_status_update(
                &event_tx,
                &self.logger,
                "stacks-api",
                Status::Green,
                &format!("http://localhost:{stacks_api_port}/doc"),
            );

            match self.boot_stacks_api_container(ctx).await {
                Ok(_) => {}
                Err(message) => {
                    let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                    self.kill(ctx, Some(&message)).await;
                    return Err(message);
                }
            };
        }

        // Start stacks-node
        let _ = event_tx.send(DevnetEvent::info("Starting stacks-node".to_string()));
        send_status_update(
            &event_tx,
            &self.logger,
            "stacks-node",
            Status::Yellow,
            "updating image",
        );
        match self.prepare_stacks_node_container(boot_index, ctx).await {
            Ok(_) => {}
            Err(message) => {
                let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                self.kill(ctx, Some(&message)).await;
                return Err(message);
            }
        };
        send_status_update(
            &event_tx,
            &self.logger,
            "stacks-node",
            Status::Yellow,
            "booting",
        );
        match self
            .boot_stacks_node_container(&event_tx, no_snapshot)
            .await
        {
            Ok(_) => {}
            Err(message) => {
                let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                self.kill(ctx, Some(&message)).await;
                return Err(message);
            }
        };

        // Start streaming container logs if enabled
        let _ = self.start_container_logs_streaming(ctx).await;

        for (i, signer_key) in signers_keys.clone().iter().enumerate() {
            let _ = event_tx.send(DevnetEvent::info(format!("Starting stacks-signer-{i}")));
            send_status_update(
                &event_tx,
                &self.logger,
                "stacks-signers",
                Status::Yellow,
                "updating image",
            );
            match self
                .prepare_stacks_signer_container(boot_index, ctx, i as u32, signer_key)
                .await
            {
                Ok(_) => {}
                Err(message) => {
                    let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                    self.kill(ctx, Some(&message)).await;
                    return Err(message);
                }
            };
            send_status_update(
                &event_tx,
                &self.logger,
                "stacks-signers",
                Status::Yellow,
                &format!("booting signer {i}"),
            );
            match self.boot_stacks_signer_container(i as u32).await {
                Ok(_) => {}
                Err(message) => {
                    let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                    self.kill(ctx, Some(&message)).await;
                    return Err(message);
                }
            };
        }
        let signers_count = signers_keys.len();
        let message = format!(
            "{} signer{} running",
            signers_count,
            if signers_count > 1 { "s" } else { "" }
        );
        send_status_update(
            &event_tx,
            &self.logger,
            "stacks-signers",
            Status::Green,
            &message,
        );

        // Start stacks-explorer
        if !disable_stacks_explorer {
            send_status_update(
                &event_tx,
                &self.logger,
                "stacks-explorer",
                Status::Yellow,
                "preparing container",
            );
            match self.prepare_stacks_explorer_container(ctx).await {
                Ok(_) => {}
                Err(message) => {
                    let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                    self.kill(ctx, Some(&message)).await;
                    return Err(message);
                }
            };
            let _ = event_tx.send(DevnetEvent::info("Starting stacks-explorer".to_string()));
            match self.boot_stacks_explorer_container(ctx).await {
                Ok(_) => {}
                Err(message) => {
                    let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                    self.kill(ctx, Some(&message)).await;
                    return Err(message);
                }
            };
            send_status_update(
                &event_tx,
                &self.logger,
                "stacks-explorer",
                Status::Green,
                &format!("http://localhost:{stacks_explorer_port}"),
            );
        }

        // Start bitcoin-explorer
        if !disable_bitcoin_explorer {
            send_status_update(
                &event_tx,
                &self.logger,
                "bitcoin-explorer",
                Status::Yellow,
                "preparing container",
            );
            match self.prepare_bitcoin_explorer_container(ctx).await {
                Ok(_) => {}
                Err(message) => {
                    let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                    self.kill(ctx, Some(&message)).await;
                    return Err(message);
                }
            };
            let _ = event_tx.send(DevnetEvent::info("Starting bitcoin-explorer".to_string()));
            match self.boot_bitcoin_explorer_container(ctx).await {
                Ok(_) => {}
                Err(message) => {
                    let _ = event_tx.send(DevnetEvent::FatalError(message.clone()));
                    self.kill(ctx, Some(&message)).await;
                    return Err(message);
                }
            };
            send_status_update(
                &event_tx,
                &self.logger,
                "bitcoin-explorer",
                Status::Green,
                &format!("http://localhost:{bitcoin_explorer_port}"),
            );
        }

        loop {
            boot_index += 1;
            match terminator_rx.recv() {
                Ok(true) => {
                    self.kill(ctx, None).await;
                    break;
                }
                Ok(false) => {
                    send_status_update(
                        &event_tx,
                        &self.logger,
                        "bitcoin-node",
                        Status::Yellow,
                        "restarting",
                    );

                    send_status_update(
                        &event_tx,
                        &self.logger,
                        "stacks-node",
                        Status::Yellow,
                        "restarting",
                    );

                    let _ = event_tx.send(DevnetEvent::debug("Killing containers".into()));
                    let _ = self.stop_containers().await;

                    let _ = event_tx.send(DevnetEvent::debug("Restarting containers".into()));
                    let (bitcoin_node_c_id, stacks_node_c_id) = self
                        .start_containers(boot_index, no_snapshot)
                        .await
                        .map_err(|e| format!("unable to reboot: {e:?}"))?;
                    self.bitcoin_node_container_id = Some(bitcoin_node_c_id);
                    self.stacks_node_container_id = Some(stacks_node_c_id);

                    // Start streaming container logs for the new containers if enabled
                    let _ = self.start_container_logs_streaming(ctx).await;
                }
                Err(_) => {
                    break;
                }
            }
        }
        Ok(())
    }

    pub fn prepare_bitcoin_node_config(
        &self,
        boot_index: u32,
        no_snapshot: bool,
    ) -> Result<Config<String>, String> {
        let devnet_config = self.get_devnet_config()?;

        let mut port_bindings = HashMap::new();
        port_bindings.insert(
            format!("{}/tcp", devnet_config.bitcoin_node_rpc_port),
            Some(vec![PortBinding {
                host_ip: Some(String::from("0.0.0.0")),
                host_port: Some(format!("{}", devnet_config.bitcoin_node_rpc_port)),
            }]),
        );
        port_bindings.insert(
            format!("{}/tcp", devnet_config.bitcoin_node_p2p_port),
            Some(vec![PortBinding {
                host_ip: Some(String::from("0.0.0.0")),
                host_port: Some(format!("{}", devnet_config.bitcoin_node_p2p_port)),
            }]),
        );
        // ZMQ block notifications
        port_bindings.insert(
            format!("{}/tcp", 28332),
            Some(vec![PortBinding {
                host_ip: Some(String::from("0.0.0.0")),
                host_port: Some(format!("{}", 28332)),
            }]),
        );
        // ZMQ transaction notifications
        port_bindings.insert(
            format!("{}/tcp", 28333),
            Some(vec![PortBinding {
                host_ip: Some(String::from("0.0.0.0")),
                host_port: Some(format!("{}", 28333)),
            }]),
        );

        let bitcoind_conf = format!(
            r#"
server=1
regtest=1
rpcallowip=0.0.0.0/0
rpcallowip=::/0
rpcuser={bitcoin_node_username}
rpcpassword={bitcoin_node_password}
txindex=1
listen=1
discover=0
dns=0
dnsseed=0
listenonion=0
rpcworkqueue=100
rpcserialversion=1
disablewallet=0
fallbackfee=0.00001

[regtest]
bind=0.0.0.0:{bitcoin_node_p2p_port}
rpcbind=0.0.0.0:{bitcoin_node_rpc_port}
rpcport={bitcoin_node_rpc_port}
"#,
            bitcoin_node_username = devnet_config.bitcoin_node_username,
            bitcoin_node_password = devnet_config.bitcoin_node_password,
            bitcoin_node_p2p_port = devnet_config.bitcoin_node_p2p_port,
            bitcoin_node_rpc_port = devnet_config.bitcoin_node_rpc_port,
        );
        let mut bitcoind_conf_path = PathBuf::from(&devnet_config.working_dir);
        bitcoind_conf_path.push("conf");
        fs::create_dir_all(&bitcoind_conf_path)
            .map_err(|e| format!("unable to create bitcoin conf directory: {e}"))?;
        bitcoind_conf_path.push("bitcoin.conf");
        let mut file = File::create(bitcoind_conf_path)
            .map_err(|e| format!("unable to create bitcoin.conf: {e}"))?;

        file.write_all(bitcoind_conf.as_bytes())
            .map_err(|e| format!("unable to write bitcoin.conf: {e:?}"))?;

        let mut bitcoind_data_path = PathBuf::from(&devnet_config.working_dir);
        bitcoind_data_path.push("data");
        bitcoind_data_path.push(format!("{boot_index}"));
        bitcoind_data_path.push("bitcoin");
        fs::create_dir_all(bitcoind_data_path)
            .map_err(|e| format!("unable to create bitcoin directory: {e:?}"))?;

        let mut exposed_ports = HashMap::new();
        exposed_ports.insert(
            format!("{}/tcp", devnet_config.bitcoin_node_rpc_port),
            HashMap::new(),
        );
        exposed_ports.insert(
            format!("{}/tcp", devnet_config.bitcoin_node_p2p_port),
            HashMap::new(),
        );

        let mut labels = HashMap::new();
        labels.insert("project".to_string(), self.network_name.to_string());
        labels.insert("reset".to_string(), "true".to_string());

        let mut env = vec![];
        if devnet_config.bitcoin_controller_automining_disabled {
            env.push("STACKS_BITCOIN_AUTOMINING_DISABLED=1".to_string());
        }

        let working_dir = &devnet_config.working_dir;
        let mut binds = vec![format!("{working_dir}/conf:/etc/bitcoin")];

        if devnet_config.bind_containers_volumes {
            binds.push(format!(
                "{working_dir}/data/{boot_index}/bitcoin:{BITCOIND_DATA_DIR}"
            ));
        }

        let mut cmd_args = vec![
            "/usr/local/bin/bitcoind".into(),
            "-conf=/etc/bitcoin/bitcoin.conf".into(),
            "-nodebuglogfile".into(),
            format!("-pid={BITCOIND_DATA_DIR}/bitcoind.pid"),
            format!("-datadir={BITCOIND_DATA_DIR}"),
        ];
        if !no_snapshot {
            cmd_args.push("-reindex".into());
        }
        let config = Config {
            labels: Some(labels),
            user: Some("1000".to_string()), // Run as user 10000
            image: Some(devnet_config.bitcoin_node_image_url.clone()),
            domainname: Some(self.network_name.to_string()),
            tty: None,
            exposed_ports: Some(exposed_ports),
            entrypoint: Some(vec![]),
            env: Some(env),
            host_config: Some(HostConfig {
                auto_remove: Some(true),
                binds: Some(binds),
                network_mode: Some(self.network_name.clone()),
                port_bindings: Some(port_bindings),
                extra_hosts: Some(vec!["host.docker.internal:host-gateway".into()]),
                ..Default::default()
            }),
            cmd: Some(cmd_args),
            ..Default::default()
        };

        Ok(config)
    }

    pub async fn prepare_bitcoin_node_container(
        &mut self,
        ctx: &Context,
        no_snapshot: bool,
    ) -> Result<(), String> {
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.get_devnet_config()?;

        let _info = docker
            .create_image(
                Some(CreateImageOptions {
                    from_image: devnet_config.bitcoin_node_image_url.clone(),
                    platform: devnet_config.docker_platform.clone().unwrap_or_default(),
                    ..Default::default()
                }),
                None,
                None,
            )
            .try_collect::<Vec<_>>()
            .await
            .map_err(|e| formatted_docker_error("unable to create bitcoind image", e))?;
        let container_name = format!("bitcoin-node.{}", self.network_name);
        let options = CreateContainerOptions {
            name: container_name.as_str(),
            platform: devnet_config.docker_platform.as_deref(),
        };

        let config = self.prepare_bitcoin_node_config(1, no_snapshot)?;

        let container = match docker
            .create_container::<&str, String>(Some(options.clone()), config.clone())
            .await
            .map_err(|e| formatted_docker_error("unable to create bitcoind container", e))
        {
            Ok(container) => container.id,
            Err(_e) => {
                // Attempt to clean eventual subsequent artifacts
                let _ = docker.kill_container::<String>(&container_name, None).await;
                docker
                    .create_container::<&str, String>(Some(options), config)
                    .await
                    .map_err(|e| formatted_docker_error("unable to create bitcoind container", e))?
                    .id
            }
        };
        ctx.try_log(|logger| slog::info!(logger, "Created container bitcoin-node: {}", container));
        self.bitcoin_node_container_id = Some(container);

        Ok(())
    }

    pub async fn clean_previous_session(&self) -> Result<(), String> {
        let mut filters = HashMap::new();
        filters.insert(
            "label".to_string(),
            vec![format!("project={}", self.network_name)],
        );
        let options = Some(ListContainersOptions {
            all: true,
            filters,
            ..Default::default()
        });

        let Some(docker) = &self.docker_client else {
            panic!("unable to get Docker client");
        };
        let res = docker.list_containers(options).await;
        let url = "https://docs.hiro.so/tools/clarinet/local-blockchain-development#common-issues";
        let containers = res.map_err(|e| {
            formatdoc!(
                "
                unable to communicate with Docker: {e}
                visit {url} to resolve this issue.
            "
            )
        })?;

        let options = KillContainerOptions { signal: "SIGKILL" };

        for container in containers.iter() {
            let Some(container_id) = &container.id else {
                continue;
            };
            let _ = docker
                .kill_container(container_id, Some(options.clone()))
                .await;

            let _ = docker
                .wait_container(container_id, None::<WaitContainerOptions<String>>)
                .try_collect::<Vec<_>>()
                .await;
        }
        self.prune().await;
        Ok(())
    }

    pub async fn boot_bitcoin_node_container(
        &mut self,
        devnet_event_tx: &Sender<DevnetEvent>,
        no_snapshot: bool,
    ) -> Result<(), String> {
        let container = match &self.bitcoin_node_container_id {
            Some(container) => container.clone(),
            _ => return Err("unable to boot container".to_string()),
        };

        let Some(docker) = &self.docker_client else {
            return Err("unable to get Docker client".into());
        };
        docker
            .start_container::<String>(&container, None)
            .await
            .map_err(|e| formatted_docker_error("unable to start bitcoind container", e))?;
        // Copy snapshot if available
        let global_snapshot_dir = get_global_snapshot_dir();
        let bitcoin_snapshot = global_snapshot_dir.join("bitcoin").join("regtest");
        // XXX This shouldn't be needed
        let exec_config = bollard::exec::CreateExecOptions {
            cmd: Some(vec!["mkdir", "-p", BITCOIND_DATA_DIR]),
            attach_stdout: Some(false),
            attach_stderr: Some(false),
            ..Default::default()
        };

        let exec = docker
            .create_exec(&container, exec_config)
            .await
            .map_err(|e| format!("Failed to create exec for mkdir: {e}"))?;

        docker
            .start_exec(&exec.id, None)
            .await
            .map_err(|e| format!("Failed to create bitcoin directory: {e}"))?;
        if !no_snapshot {
            // Ensure the destination directory exists in the container

            copy_snapshot_to_container(
                &container,
                &bitcoin_snapshot,
                BITCOIND_DATA_DIR,
                devnet_event_tx,
                "Bitcoin",
            )
            .await?;
        }

        Ok(())
    }

    pub fn prepare_stacks_node_config(&self, boot_index: u32) -> Result<Config<String>, String> {
        let network_config = self
            .network_config
            .as_ref()
            .ok_or("unable to get network configuration")?;
        let devnet_config = self.get_devnet_config()?;

        let mut port_bindings = HashMap::new();
        port_bindings.insert(
            format!("{}/tcp", devnet_config.stacks_node_p2p_port),
            Some(vec![PortBinding {
                host_ip: Some(String::from("0.0.0.0")),
                host_port: Some(format!("{}", devnet_config.stacks_node_p2p_port)),
            }]),
        );
        port_bindings.insert(
            format!("{}/tcp", devnet_config.stacks_node_rpc_port),
            Some(vec![PortBinding {
                host_ip: Some(String::from("0.0.0.0")),
                host_port: Some(format!("{}", devnet_config.stacks_node_rpc_port)),
            }]),
        );

        let mut stacks_conf = format!(
            r#"
[node]
working_dir = "/devnet"
rpc_bind = "0.0.0.0:{stacks_node_rpc_port}"
p2p_bind = "0.0.0.0:{stacks_node_p2p_port}"
data_url = "http://127.0.0.1:{stacks_node_rpc_port}"
p2p_address = "127.0.0.1:{stacks_node_rpc_port}"
miner = true
stacker = true
seed = "{miner_secret_key_hex}"
local_peer_seed = "{miner_secret_key_hex}"
pox_sync_sample_secs = 0
wait_time_for_blocks = 0
wait_time_for_microblocks = 0
next_initiative_delay = {next_initiative_delay}
mine_microblocks = false
microblock_frequency = 1000

[connection_options]
# inv_sync_interval = 10
# download_interval = 10
# walk_interval = 10
disable_block_download = false
disable_inbound_handshakes = true
disable_inbound_walks = true
public_ip_address = "1.1.1.1:1234"
auth_token = "12345"

[miner]
first_attempt_time_ms = {first_attempt_time_ms}
block_reward_recipient = "{miner_coinbase_recipient}"
microblock_attempt_time_ms = 10
pre_nakamoto_mock_signing = {pre_nakamoto_mock_signing}
mining_key = "19ec1c3e31d139c989a23a27eac60d1abfad5277d3ae9604242514c738258efa01"
"#,
            stacks_node_rpc_port = devnet_config.stacks_node_rpc_port,
            stacks_node_p2p_port = devnet_config.stacks_node_p2p_port,
            miner_secret_key_hex = devnet_config.miner_secret_key_hex,
            next_initiative_delay = devnet_config.stacks_node_next_initiative_delay,
            first_attempt_time_ms = devnet_config.stacks_node_first_attempt_time_ms,
            miner_coinbase_recipient = devnet_config.miner_coinbase_recipient,
            pre_nakamoto_mock_signing = devnet_config.pre_nakamoto_mock_signing,
        );

        for (_, account) in network_config.accounts.iter() {
            stacks_conf.push_str(&format!(
                r#"
[[ustx_balance]]
address = "{}"
amount = {}
"#,
                account.stx_address, account.balance
            ));
        }

        for i in 0..devnet_config.stacks_signers_keys.len() {
            // the endpoints are
            // `stacks-signer-0.<network>:30000`
            // `stacks-signer-1.<network>:30001`
            // ...
            stacks_conf.push_str(&format!(
                r#"
[[events_observer]]
endpoint = "stacks-signer-{i}.{}:{}"
events_keys = ["stackerdb", "block_proposal", "burn_blocks"]
"#,
                self.network_name,
                30000 + i
            ));
        }

        stacks_conf.push_str(&format!(
            r#"
# Add orchestrator (docker-host) as an event observer
# Also used by the devnet chainhook instance
[[events_observer]]
endpoint = "host.docker.internal:{orchestrator_ingestion_port}"
events_keys = ["*"]
"#,
            orchestrator_ingestion_port = devnet_config.orchestrator_ingestion_port,
        ));

        if !devnet_config.disable_stacks_api {
            stacks_conf.push_str(&format!(
                r#"
# Add stacks-api as an event observer
[[events_observer]]
endpoint = "stacks-api.{}:{}"
events_keys = ["*"]
"#,
                self.network_name, devnet_config.stacks_api_events_port
            ));
        }

        for chains_coordinator in devnet_config.stacks_node_events_observers.iter() {
            stacks_conf.push_str(&format!(
                r#"
[[events_observer]]
endpoint = "{chains_coordinator}"
events_keys = ["*"]
"#,
            ));
        }

        stacks_conf.push_str(&format!(
            r#"
[burnchain]
chain = "bitcoin"
mode = "krypton"
magic_bytes = "T3"
first_burn_block_height = 100
pox_prepare_length = 5
pox_reward_length = 20
burn_fee_cap = 20_000
poll_time_secs = 1
timeout = 2
peer_host = "host.docker.internal"
rpc_ssl = false
wallet_name = "{miner_wallet_name}"
username = "{bitcoin_node_username}"
password = "{bitcoin_node_password}"
rpc_port = {orchestrator_ingestion_port}
peer_port = {bitcoin_node_p2p_port}

"#,
            bitcoin_node_username = devnet_config.bitcoin_node_username,
            bitcoin_node_password = devnet_config.bitcoin_node_password,
            bitcoin_node_p2p_port = devnet_config.bitcoin_node_p2p_port,
            orchestrator_ingestion_port = devnet_config.orchestrator_ingestion_port,
            miner_wallet_name = devnet_config.miner_wallet_name,
        ));

        stacks_conf.push_str(&formatdoc!(
            r#"
            [[burnchain.epochs]]
            epoch_name = "1.0"
            start_height = 0

        "#
        ));

        let epoch_config = BurnchainEpochConfig::from(devnet_config);
        let epoch_config_toml = toml::to_string(&epoch_config)
            .map_err(|e| format!("unable to serialize stacks epoch config: {e:?}"))?;
        stacks_conf.push_str(&epoch_config_toml);

        let mut stacks_conf_path = PathBuf::from(&devnet_config.working_dir);
        stacks_conf_path.push("conf/Stacks.toml");
        let mut file = File::create(stacks_conf_path)
            .map_err(|e| format!("unable to create Stacks.toml: {e:?}"))?;
        file.write_all(stacks_conf.as_bytes())
            .map_err(|e| format!("unable to write Stacks.toml: {e:?}"))?;

        let mut stacks_node_data_path = PathBuf::from(&devnet_config.working_dir);
        stacks_node_data_path.push("data");
        stacks_node_data_path.push(format!("{boot_index}"));
        stacks_node_data_path.push("stacks");
        fs::create_dir_all(stacks_node_data_path)
            .map_err(|e| format!("unable to create stacks directory: {e:?}"))?;

        let mut exposed_ports = HashMap::new();
        exposed_ports.insert(
            format!("{}/tcp", devnet_config.stacks_node_rpc_port),
            HashMap::new(),
        );
        exposed_ports.insert(
            format!("{}/tcp", devnet_config.stacks_node_p2p_port),
            HashMap::new(),
        );

        let mut labels = HashMap::new();
        labels.insert("project".to_string(), self.network_name.to_string());
        labels.insert("reset".to_string(), "true".to_string());

        let mut binds = vec![format!(
            "{}/conf:/src/stacks-node/",
            devnet_config.working_dir
        )];

        if devnet_config.bind_containers_volumes {
            binds.push(format!(
                "{}/data/{}/stacks:/devnet/",
                devnet_config.working_dir, boot_index
            ))
        }

        let mut env = vec![
            "STACKS_LOG_PP=1".to_string(),
            "BLOCKSTACK_USE_TEST_GENESIS_CHAINSTATE=1".to_string(),
        ];
        env.append(&mut devnet_config.stacks_node_env_vars.clone());

        let config = Config {
            labels: Some(labels),
            image: Some(devnet_config.stacks_node_image_url.clone()),
            // domainname: Some(self.network_name.to_string()),
            tty: None,
            exposed_ports: Some(exposed_ports),
            entrypoint: Some(vec![
                "stacks-node".into(),
                "start".into(),
                "--config".into(),
                "/src/stacks-node/Stacks.toml".into(),
            ]),
            env: Some(env),
            host_config: Some(HostConfig {
                auto_remove: Some(true),
                binds: Some(binds),
                network_mode: Some(self.network_name.clone()),
                port_bindings: Some(port_bindings),
                extra_hosts: Some(vec!["host.docker.internal:host-gateway".into()]),
                ..Default::default()
            }),
            ..Default::default()
        };

        Ok(config)
    }

    pub async fn prepare_stacks_node_container(
        &mut self,
        boot_index: u32,
        ctx: &Context,
    ) -> Result<(), String> {
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.get_devnet_config()?;

        let _info = docker
            .create_image(
                Some(CreateImageOptions {
                    from_image: devnet_config.stacks_node_image_url.clone(),
                    platform: devnet_config.docker_platform.clone().unwrap_or_default(),
                    ..Default::default()
                }),
                None,
                None,
            )
            .try_collect::<Vec<_>>()
            .await
            .map_err(|e| format!("unable to create image: {e}"))?;
        let options = CreateContainerOptions {
            name: format!("stacks-node.{}", self.network_name),
            platform: devnet_config.docker_platform.clone(),
        };

        let config = self.prepare_stacks_node_config(boot_index)?;

        let container = docker
            .create_container::<String, String>(Some(options), config)
            .await
            .map_err(|e| format!("unable to create container: {e}"))?
            .id;

        ctx.try_log(|logger| slog::info!(logger, "Created container stacks-node: {}", container));
        self.stacks_node_container_id = Some(container);

        Ok(())
    }

    pub async fn boot_stacks_node_container(
        &mut self,
        devnet_event_tx: &Sender<DevnetEvent>,
        no_snapshot: bool,
    ) -> Result<(), String> {
        let container = match &self.stacks_node_container_id {
            Some(container) => container.clone(),
            _ => return Err("unable to boot container".to_string()),
        };

        let Some(docker) = &self.docker_client else {
            return Err("unable to get Docker client".into());
        };
        let global_snapshot_dir = get_global_snapshot_dir();
        let stacks_snapshot = global_snapshot_dir.join("stacks").join("krypton");

        if !no_snapshot {
            copy_snapshot_to_container(
                &container,
                &stacks_snapshot,
                "/devnet",
                devnet_event_tx,
                "Stacks",
            )
            .await?;
        }

        docker
            .start_container::<String>(&container, None)
            .await
            .map_err(|e| formatted_docker_error("unable to start stacks-node container", e))?;

        Ok(())
    }

    pub fn prepare_stacks_signer_config(
        &self,
        boot_index: u32,
        signer_id: u32,
        signer_key: &StacksPrivateKey,
    ) -> Result<Config<String>, String> {
        let devnet_config = self.get_devnet_config()?;

        let signer_conf = format!(
            r#"
stacks_private_key = "{signer_private_key}"
node_host = "stacks-node.{network_name}:{stacks_node_rpc_port}" # eg "127.0.0.1:20443"
# must be added as event_observer in node config:
endpoint = "0.0.0.0:{port}"
network = "testnet"
auth_password = "12345"
db_path = "stacks-signer-{signer_id}.sqlite"
"#,
            signer_private_key = signer_key.to_bytes().to_lower_hex_string(),
            // signer_private_key = devnet_config.signer_private_key,
            network_name = self.network_name,
            stacks_node_rpc_port = devnet_config.stacks_node_rpc_port,
            port = 30000 + signer_id,
        );
        let mut signer_conf_path = PathBuf::from(&devnet_config.working_dir);
        signer_conf_path.push(format!("conf/Signer-{signer_id}.toml"));
        let mut file = File::create(signer_conf_path)
            .map_err(|e| format!("unable to create Signer.toml: {e:?}"))?;
        file.write_all(signer_conf.as_bytes())
            .map_err(|e| format!("unable to write Signer.toml: {e:?}"))?;

        let mut stacks_signer_data_path = PathBuf::from(&devnet_config.working_dir);
        stacks_signer_data_path.push("data");
        stacks_signer_data_path.push(boot_index.to_string());
        stacks_signer_data_path.push("signer");
        fs::create_dir_all(stacks_signer_data_path)
            .map_err(|e| format!("unable to create stacks directory: {e:?}"))?;

        let mut labels = HashMap::new();
        labels.insert("project".to_string(), self.network_name.to_string());
        labels.insert("reset".to_string(), "true".to_string());

        let mut binds = vec![format!(
            "{}/conf:/src/stacks-signer/",
            devnet_config.working_dir
        )];

        if devnet_config.bind_containers_volumes {
            binds.push(format!(
                "{}/data/{}/stacks:/devnet/",
                devnet_config.working_dir, boot_index
            ))
        }

        let env = devnet_config.stacks_signers_env_vars.clone();

        let config = Config {
            labels: Some(labels),
            image: Some(devnet_config.stacks_signer_image_url.clone()),
            // domainname: Some(self.network_name.to_string()),
            tty: None,
            exposed_ports: None,
            entrypoint: Some(vec![
                "stacks-signer".into(),
                "run".into(),
                "--config".into(),
                format!("/src/stacks-signer/Signer-{signer_id}.toml"),
            ]),
            env: Some(env),
            host_config: Some(HostConfig {
                auto_remove: Some(true),
                binds: Some(binds),
                network_mode: Some(self.network_name.clone()),
                port_bindings: None,
                extra_hosts: Some(vec!["host.docker.internal:host-gateway".into()]),
                ..Default::default()
            }),
            ..Default::default()
        };

        Ok(config)
    }

    pub async fn prepare_stacks_signer_container(
        &mut self,
        boot_index: u32,
        ctx: &Context,
        signer_id: u32,
        signer_key: &StacksPrivateKey,
    ) -> Result<(), String> {
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.get_devnet_config()?;

        let _info = docker
            .create_image(
                Some(CreateImageOptions {
                    from_image: devnet_config.stacks_signer_image_url.clone(),
                    platform: devnet_config.docker_platform.clone().unwrap_or_default(),
                    ..Default::default()
                }),
                None,
                None,
            )
            .try_collect::<Vec<_>>()
            .await
            .map_err(|e| format!("unable to create image: {e}"))?;
        let options = CreateContainerOptions {
            name: format!("stacks-signer-{signer_id}.{}", self.network_name),
            platform: devnet_config.docker_platform.clone(),
        };

        let config = self.prepare_stacks_signer_config(boot_index, signer_id, signer_key)?;

        let container = docker
            .create_container::<String, String>(Some(options), config)
            .await
            .map_err(|e| format!("unable to create container: {e}"))?
            .id;

        ctx.try_log(|logger| {
            slog::info!(
                logger,
                "Created container stacks-signer-{signer_id}: {}",
                container
            )
        });
        self.stacks_signers_containers_ids.push(container);

        Ok(())
    }

    pub async fn boot_stacks_signer_container(&mut self, signer_id: u32) -> Result<(), String> {
        let container = self.stacks_signers_containers_ids[signer_id as usize].clone();

        let Some(docker) = &self.docker_client else {
            return Err("unable to get Docker client".into());
        };

        docker
            .start_container::<String>(&container, None)
            .await
            .map_err(|e| formatted_docker_error("unable to start stacks-signer container", e))?;

        Ok(())
    }

    pub async fn prepare_stacks_api_container(&mut self, ctx: &Context) -> Result<(), String> {
        let docker = self
            .docker_client
            .as_ref()
            .ok_or_else(|| "unable to get Docker client".to_string())?;
        let devnet_config = self.get_devnet_config()?;

        let _info = docker
            .create_image(
                Some(CreateImageOptions {
                    from_image: devnet_config.stacks_api_image_url.clone(),
                    platform: devnet_config.docker_platform.clone().unwrap_or_default(),
                    ..Default::default()
                }),
                None,
                None,
            )
            .try_collect::<Vec<_>>()
            .await
            .map_err(|e| format!("unable to create image: {e}"))?;
        let options = CreateContainerOptions {
            name: format!("stacks-api.{}", self.network_name),
            platform: devnet_config.docker_platform.clone(),
        };

        let mut port_bindings = HashMap::new();
        port_bindings.insert(
            format!("{}/tcp", devnet_config.stacks_api_port),
            Some(vec![PortBinding {
                host_ip: Some(String::from("0.0.0.0")),
                host_port: Some(format!("{}", devnet_config.stacks_api_port)),
            }]),
        );

        let mut exposed_ports = HashMap::new();
        exposed_ports.insert(
            format!("{}/tcp", devnet_config.stacks_api_port),
            HashMap::new(),
        );

        let mut labels = HashMap::new();
        labels.insert("project".to_string(), self.network_name.to_string());

        let mut env = vec![
            format!("STACKS_CORE_RPC_HOST=stacks-node.{}", self.network_name),
            format!("STACKS_BLOCKCHAIN_API_DB=pg"),
            format!(
                "STACKS_CORE_RPC_PORT={}",
                devnet_config.stacks_node_rpc_port
            ),
            format!(
                "STACKS_BLOCKCHAIN_API_PORT={}",
                devnet_config.stacks_api_port
            ),
            format!("STACKS_BLOCKCHAIN_API_HOST=0.0.0.0"),
            format!(
                "STACKS_CORE_EVENT_PORT={}",
                devnet_config.stacks_api_events_port
            ),
            format!("STACKS_CORE_EVENT_HOST=0.0.0.0"),
            format!("STACKS_API_ENABLE_FT_METADATA=1"),
            format!("PG_HOST=postgres.{}", self.network_name),
            format!("PG_PORT=5432"),
            format!("PG_USER={}", devnet_config.postgres_username),
            format!("PG_PASSWORD={}", devnet_config.postgres_password),
            format!("PG_DATABASE={}", devnet_config.stacks_api_postgres_database),
            format!("STACKS_CHAIN_ID=2147483648"),
            format!("V2_POX_MIN_AMOUNT_USTX=90000000260"),
            format!("FAUCET_PRIVATE_KEY={}", devnet_config.faucet_secret_key_hex),
            "NODE_ENV=development".to_string(),
        ];
        env.append(&mut devnet_config.stacks_api_env_vars.clone());

        let config = Config {
            labels: Some(labels),
            image: Some(devnet_config.stacks_api_image_url.clone()),
            // domainname: Some(self.network_name.to_string()),
            tty: None,
            exposed_ports: Some(exposed_ports),
            env: Some(env),
            host_config: Some(HostConfig {
                auto_remove: Some(true),
                network_mode: Some(self.network_name.clone()),
                port_bindings: Some(port_bindings),
                extra_hosts: Some(vec!["host.docker.internal:host-gateway".into()]),
                ..Default::default()
            }),
            ..Default::default()
        };

        let container = docker
            .create_container::<String, String>(Some(options), config)
            .await
            .map_err(|e| format!("unable to create container: {e}"))?
            .id;

        ctx.try_log(|logger| slog::info!(logger, "Created container stacks-api: {}", container));
        self.stacks_api_container_id = Some(container);

        Ok(())
    }

    fn has_events_to_import(&self, devnet_config: &DevnetConfig) -> Option<PathBuf> {
        let project_events_path = PathBuf::from(&devnet_config.working_dir)
            .join("events_export")
            .join("events_cache.tsv");

        if project_events_path.exists() {
            Some(project_events_path)
        } else {
            let global_events_path = get_global_snapshot_dir()
                .join("events_export")
                .join("events_cache.tsv");

            if global_events_path.exists() {
                Some(global_events_path)
            } else {
                None
            }
        }
    }
    pub async fn boot_stacks_api_container(&self, ctx: &Context) -> Result<(), String> {
        let container = match &self.stacks_api_container_id {
            Some(container) => container.clone(),
            _ => return Err("unable to boot container".to_string()),
        };

        let Some(docker) = &self.docker_client else {
            return Err("unable to get Docker client".into());
        };

        docker
            .start_container::<String>(&container, None)
            .await
            .map_err(|e| formatted_docker_error("unable to start stacks-api container", e))?;

        let devnet_config = match &self.network_config {
            Some(ref network_config) => match network_config.devnet {
                Some(ref devnet_config) => devnet_config,
                _ => return Ok(()),
            },
            _ => return Ok(()),
        };

        // Check if we need to import events
        if let Some(events_path) = self.has_events_to_import(devnet_config) {
            // Wait for API to be ready
            // TODO: don't do this..
            std::thread::sleep(Duration::from_secs(8));

            ctx.try_log(|logger| {
                slog::info!(logger, "Importing events from {}", events_path.display())
            });

            // Copy the events file to the container
            let container_name = format!("stacks-api.{}", self.network_name);
            let copy_command = format!(
                "docker cp {} {}:/tmp/events_cache.tsv",
                events_path.display(),
                container_name
            );
            let output = run_command(&copy_command)
                .map_err(|e| format!("Failed to copy events file to container: {e}"))?;

            if !output.status.success() {
                return Err(format!(
                    "Copy command failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                ));
            }

            // Run the import command
            let import_command = format!(
                "docker exec {container_name} node /app/lib/index.js import-events --file /tmp/events_cache.tsv --wipe-db"
            );
            let output = run_command(&import_command)
                .map_err(|e| format!("Failed to import events: {e}"))?;

            if !output.status.success() {
                ctx.try_log(|logger| {
                    slog::warn!(
                        logger,
                        "Events import failed: {}",
                        String::from_utf8_lossy(&output.stderr)
                    )
                });
            } else {
                ctx.try_log(|logger| slog::info!(logger, "Events import completed successfully"));
            }
        }
        Ok(())
    }

    pub async fn prepare_postgres_container(&mut self, ctx: &Context) -> Result<(), String> {
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.get_devnet_config()?;

        let _info = docker
            .create_image(
                Some(CreateImageOptions {
                    from_image: devnet_config.postgres_image_url.clone(),
                    platform: devnet_config.docker_platform.clone().unwrap_or_default(),
                    ..Default::default()
                }),
                None,
                None,
            )
            .try_collect::<Vec<_>>()
            .await
            .map_err(|e| format!("unable to create image: {e}"))?;
        let options = CreateContainerOptions {
            name: format!("postgres.{}", self.network_name),
            platform: devnet_config.docker_platform.clone(),
        };

        let mut port_bindings = HashMap::new();
        port_bindings.insert(
            "5432/tcp".to_string(),
            Some(vec![PortBinding {
                host_ip: Some(String::from("0.0.0.0")),
                host_port: Some(format!("{}", devnet_config.postgres_port)),
            }]),
        );

        let exposed_ports = HashMap::new();

        let mut labels = HashMap::new();
        labels.insert("project".to_string(), self.network_name.to_string());

        let config = Config {
            labels: Some(labels),
            image: Some(devnet_config.postgres_image_url.clone()),
            // domainname: Some(self.network_name.to_string()),
            tty: None,
            exposed_ports: Some(exposed_ports),
            env: Some(vec![
                format!("POSTGRES_PASSWORD={}", devnet_config.postgres_password),
                format!("POSTGRES_DB={}", devnet_config.stacks_api_postgres_database),
            ]),
            host_config: Some(HostConfig {
                auto_remove: Some(true),
                network_mode: Some(self.network_name.clone()),
                port_bindings: Some(port_bindings),
                extra_hosts: Some(vec!["host.docker.internal:host-gateway".into()]),
                ..Default::default()
            }),
            ..Default::default()
        };

        let container = docker
            .create_container::<String, String>(Some(options), config)
            .await
            .map_err(|e| format!("unable to create container: {e}"))?
            .id;

        ctx.try_log(|logger| slog::info!(logger, "Created container postgres: {}", container));
        self.postgres_container_id = Some(container);

        Ok(())
    }

    pub async fn boot_postgres_container(&self, _ctx: &Context) -> Result<(), String> {
        let container = match &self.postgres_container_id {
            Some(container) => container.clone(),
            _ => return Err("unable to boot container".to_string()),
        };

        let Some(docker) = &self.docker_client else {
            return Err("unable to get Docker client".into());
        };

        docker
            .start_container::<String>(&container, None)
            .await
            .map_err(|e| formatted_docker_error("unable to start postgres container", e))?;

        Ok(())
    }

    pub async fn prepare_stacks_explorer_container(&mut self, ctx: &Context) -> Result<(), String> {
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.get_devnet_config()?;

        // default platform to linux/amd64 since the image is built for that
        let platform = devnet_config
            .docker_platform
            .clone()
            .unwrap_or(DEFAULT_DOCKER_PLATFORM.to_string());
        let _info = docker
            .create_image(
                Some(CreateImageOptions {
                    from_image: devnet_config.stacks_explorer_image_url.clone(),
                    platform: platform.clone(),
                    ..Default::default()
                }),
                None,
                None,
            )
            .try_collect::<Vec<_>>()
            .await
            .map_err(|e| format!("unable to create image: {e}"))?;
        let options = CreateContainerOptions {
            name: format!("stacks-explorer.{}", self.network_name),
            platform: Some(platform),
        };

        let explorer_guest_port = 3000;
        let mut port_bindings = HashMap::new();
        port_bindings.insert(
            format!("{explorer_guest_port}/tcp"),
            Some(vec![PortBinding {
                host_ip: Some(String::from("0.0.0.0")),
                host_port: Some(format!("{}", devnet_config.stacks_explorer_port)),
            }]),
        );

        let mut exposed_ports = HashMap::new();
        exposed_ports.insert(format!("{explorer_guest_port}/tcp"), HashMap::new());

        let mut labels = HashMap::new();
        labels.insert("project".to_string(), self.network_name.to_string());

        let mut env = vec![
            format!(
                "NEXT_PUBLIC_REGTEST_API_SERVER=http://localhost:{}",
                devnet_config.stacks_api_port
            ),
            format!(
                "NEXT_PUBLIC_TESTNET_API_SERVER=http://localhost:{}",
                devnet_config.stacks_api_port
            ),
            format!(
                "NEXT_PUBLIC_MAINNET_API_SERVER=http://localhost:{}",
                devnet_config.stacks_api_port
            ),
            format!("NEXT_PUBLIC_DEFAULT_POLLING_INTERVAL={}", 5000),
            "NODE_ENV=development".to_string(),
        ];
        env.append(&mut devnet_config.stacks_explorer_env_vars.clone());

        let config = Config {
            labels: Some(labels),
            image: Some(devnet_config.stacks_explorer_image_url.clone()),
            // domainname: Some(self.network_name.to_string()),
            tty: None,
            exposed_ports: Some(exposed_ports),
            env: Some(env),
            host_config: Some(HostConfig {
                auto_remove: Some(true),
                network_mode: Some(self.network_name.clone()),
                port_bindings: Some(port_bindings),
                extra_hosts: Some(vec!["host.docker.internal:host-gateway".into()]),
                ..Default::default()
            }),
            ..Default::default()
        };

        let container = docker
            .create_container::<String, String>(Some(options), config)
            .await
            .map_err(|e| format!("unable to create container: {e}"))?
            .id;

        ctx.try_log(|logger| {
            slog::info!(logger, "Created container stacks-explorer: {}", container)
        });
        self.stacks_explorer_container_id = Some(container);

        Ok(())
    }

    pub async fn boot_stacks_explorer_container(&self, _ctx: &Context) -> Result<(), String> {
        let container = match &self.stacks_explorer_container_id {
            Some(container) => container.clone(),
            _ => return Err("unable to boot container".to_string()),
        };

        let Some(docker) = &self.docker_client else {
            return Err("unable to get Docker client".into());
        };

        docker
            .start_container::<String>(&container, None)
            .await
            .map_err(|e| format!("unable to create container: {e}"))?;

        Ok(())
    }

    pub async fn prepare_bitcoin_explorer_container(
        &mut self,
        ctx: &Context,
    ) -> Result<(), String> {
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.get_devnet_config()?;

        // default platform to linux/amd64 since the image is built for that
        let platform = devnet_config
            .docker_platform
            .clone()
            .unwrap_or(DEFAULT_DOCKER_PLATFORM.to_string());
        let _info = docker
            .create_image(
                Some(CreateImageOptions {
                    from_image: devnet_config.bitcoin_explorer_image_url.clone(),
                    platform: platform.clone(),
                    ..Default::default()
                }),
                None,
                None,
            )
            .try_collect::<Vec<_>>()
            .await
            .map_err(|e| format!("unable to create image: {e}"))?;
        let options = CreateContainerOptions {
            name: format!("bitcoin-explorer.{}", self.network_name),
            platform: Some(platform),
        };

        let mut port_bindings = HashMap::new();
        port_bindings.insert(
            format!("{}/tcp", devnet_config.bitcoin_explorer_port),
            Some(vec![PortBinding {
                host_ip: Some(String::from("0.0.0.0")),
                host_port: Some(format!("{}", devnet_config.bitcoin_explorer_port)),
            }]),
        );

        let mut exposed_ports = HashMap::new();
        exposed_ports.insert(
            format!("{}/tcp", devnet_config.bitcoin_explorer_port),
            HashMap::new(),
        );

        let mut labels = HashMap::new();
        labels.insert("project".to_string(), self.network_name.to_string());

        let config = Config {
            labels: Some(labels),
            image: Some(devnet_config.bitcoin_explorer_image_url.clone()),
            // domainname: Some(self.network_name.to_string()),
            tty: None,
            exposed_ports: Some(exposed_ports),
            env: Some(vec![
                format!("BTCEXP_HOST=0.0.0.0",),
                format!("BTCEXP_PORT={}", devnet_config.bitcoin_explorer_port),
                format!("BTCEXP_BITCOIND_HOST=host.docker.internal",),
                format!(
                    "BTCEXP_BITCOIND_PORT={}",
                    devnet_config.bitcoin_node_rpc_port
                ),
                format!(
                    "BTCEXP_BITCOIND_USER={}",
                    devnet_config.bitcoin_node_username
                ),
                format!(
                    "BTCEXP_BITCOIND_PASS={}",
                    devnet_config.bitcoin_node_password
                ),
                format!(
                    "BTCEXP_BASIC_AUTH_PASSWORD={}",
                    devnet_config.bitcoin_node_password
                ),
                format!("BTCEXP_RPC_ALLOWALL=true",),
            ]),
            host_config: Some(HostConfig {
                auto_remove: Some(true),
                network_mode: Some(self.network_name.clone()),
                port_bindings: Some(port_bindings),
                extra_hosts: Some(vec!["host.docker.internal:host-gateway".into()]),
                ..Default::default()
            }),
            ..Default::default()
        };

        let container = docker
            .create_container::<String, String>(Some(options), config)
            .await
            .map_err(|e| format!("unable to create container: {e}"))?
            .id;

        ctx.try_log(|logger| {
            slog::info!(logger, "Created container bitcoin-explorer: {}", container)
        });
        self.bitcoin_explorer_container_id = Some(container);

        Ok(())
    }

    pub async fn boot_bitcoin_explorer_container(&self, _ctx: &Context) -> Result<(), String> {
        let container = match &self.bitcoin_explorer_container_id {
            Some(container) => container.clone(),
            _ => return Err("unable to boot container".to_string()),
        };

        let Some(docker) = &self.docker_client else {
            return Err("unable to get Docker client".into());
        };

        docker
            .start_container::<String>(&container, None)
            .await
            .map_err(|e| format!("unable to create container: {e}"))?;

        Ok(())
    }

    pub async fn stop_containers(&self) -> Result<(), String> {
        let containers_ids = match (
            &self.stacks_node_container_id,
            &self.stacks_api_container_id,
            &self.stacks_explorer_container_id,
            &self.bitcoin_node_container_id,
            &self.bitcoin_explorer_container_id,
            &self.postgres_container_id,
        ) {
            (Some(c1), Some(c2), Some(c3), Some(c4), Some(c5), Some(c6)) => {
                (c1, c2, c3, c4, c5, c6)
            }
            _ => return Err("unable to get containers".to_string()),
        };

        let (
            stacks_node_c_id,
            stacks_api_c_id,
            stacks_explorer_c_id,
            bitcoin_node_c_id,
            bitcoin_explorer_c_id,
            postgres_c_id,
        ) = containers_ids;

        let Some(docker) = &self.docker_client else {
            return Err("unable to get Docker client".into());
        };

        let options = KillContainerOptions { signal: "SIGKILL" };

        // kill all signers
        for container_id in &self.stacks_signers_containers_ids {
            let _ = docker
                .kill_container(container_id, Some(options.clone()))
                .await;
        }

        // kill other containers
        let _ = docker
            .kill_container(stacks_node_c_id, Some(options.clone()))
            .await;

        let _ = docker
            .kill_container(stacks_api_c_id, Some(options.clone()))
            .await;

        let _ = docker
            .kill_container(stacks_explorer_c_id, Some(options.clone()))
            .await;

        let _ = docker
            .kill_container(bitcoin_node_c_id, Some(options.clone()))
            .await;

        let _ = docker
            .kill_container(bitcoin_explorer_c_id, Some(options.clone()))
            .await;

        let _ = docker
            .kill_container(postgres_c_id, Some(options.clone()))
            .await;

        let _ = docker
            .wait_container(stacks_node_c_id, None::<WaitContainerOptions<String>>)
            .try_collect::<Vec<_>>()
            .await;

        Ok(())
    }

    pub async fn start_containers(
        &self,
        boot_index: u32,
        no_snapshot: bool,
    ) -> Result<(String, String), String> {
        let containers_ids = match (
            &self.stacks_api_container_id,
            &self.stacks_explorer_container_id,
            &self.bitcoin_explorer_container_id,
            &self.postgres_container_id,
        ) {
            (Some(c1), Some(c2), Some(c3), Some(c4)) => (c1, c2, c3, c4),
            _ => return Err("unable to boot container".to_string()),
        };
        let (stacks_api_c_id, stacks_explorer_c_id, bitcoin_explorer_c_id, postgres_c_id) =
            containers_ids;

        let Some(docker) = &self.docker_client else {
            return Err("unable to get Docker client".into());
        };

        // Prune
        let mut filters = HashMap::new();
        filters.insert(
            "label".to_string(),
            vec![
                format!("project={}", self.network_name),
                "reset=true".to_string(),
            ],
        );
        let _ = docker
            .prune_containers(Some(PruneContainersOptions { filters }))
            .await;

        let platform = self
            .network_config
            .as_ref()
            .and_then(|c| c.devnet.as_ref())
            .and_then(|c| c.docker_platform.clone());

        let options = CreateContainerOptions {
            name: format!("bitcoin-node.{}", self.network_name),
            platform: platform.clone(),
        };
        let bitcoin_node_config = self.prepare_bitcoin_node_config(boot_index, no_snapshot)?;
        let bitcoin_node_c_id = docker
            .create_container::<String, String>(Some(options), bitcoin_node_config)
            .await
            .map_err(|e| format!("unable to create container: {e}"))?
            .id;

        let options = CreateContainerOptions {
            name: format!("stacks-node.{}", self.network_name),
            platform,
        };
        let stacks_node_config = self.prepare_stacks_node_config(boot_index)?;
        let stacks_node_c_id = docker
            .create_container::<String, String>(Some(options), stacks_node_config)
            .await
            .map_err(|e| format!("unable to create container: {e}"))?
            .id;

        // Start all the containers
        let _ = docker
            .start_container::<String>(&bitcoin_node_c_id, None)
            .await;

        let _ = docker
            .start_container::<String>(bitcoin_explorer_c_id, None)
            .await;

        let _ = docker.start_container::<String>(postgres_c_id, None).await;

        let _ = docker
            .start_container::<String>(stacks_api_c_id, None)
            .await;

        let _ = docker
            .start_container::<String>(stacks_explorer_c_id, None)
            .await;

        let _ = docker
            .start_container::<String>(&stacks_node_c_id, None)
            .await;

        Ok((bitcoin_node_c_id, stacks_node_c_id))
    }

    pub async fn kill(&self, ctx: &Context, fatal_message: Option<&str>) {
        let Some(docker) = self.docker_client.as_ref() else {
            return;
        };
        let Ok(devnet_config) = self.get_devnet_config() else {
            return;
        };

        let options = Some(KillContainerOptions { signal: "SIGKILL" });

        // Terminate containers
        let container_ids = vec![
            self.bitcoin_explorer_container_id.clone(),
            self.stacks_explorer_container_id.clone(),
            self.bitcoin_node_container_id.clone(),
            self.stacks_api_container_id.clone(),
            self.postgres_container_id.clone(),
            self.stacks_node_container_id.clone(),
        ];

        let signers_container_ids = self.stacks_signers_containers_ids.clone();

        for container_id in container_ids
            .into_iter()
            .flatten()
            .chain(signers_container_ids)
        {
            let _ = docker.kill_container(&container_id, options.clone()).await;
            ctx.try_log(|logger| slog::info!(logger, "Terminating container: {}", &container_id));
            let _ = docker.remove_container(&container_id, None).await;
        }

        // Delete network
        let _ = docker.remove_network(&self.network_name).await;

        ctx.try_log(|logger| slog::info!(logger, "Pruning network and containers"));

        self.prune().await;
        if let Some(ref tx) = self.termination_success_tx {
            let _ = tx.send(true);
        }

        ctx.try_log(|logger| {
            slog::info!(
                logger,
                "Artifacts (logs, conf, chainstates) available here: {}",
                devnet_config.working_dir
            )
        });

        if let Some(message) = fatal_message {
            ctx.try_log(|logger| slog::info!(logger, "⚠️  fatal error - {}", message));
        } else {
            ctx.try_log(|logger| slog::info!(logger, "✌️"));
        }
    }

    pub async fn prune(&self) {
        let Some(docker) = &self.docker_client else {
            return;
        };

        let mut filters = HashMap::new();
        filters.insert(
            "label".to_string(),
            vec![format!("project={}", self.network_name)],
        );
        let _ = docker
            .prune_containers(Some(PruneContainersOptions {
                filters: filters.clone(),
            }))
            .await;

        let _ = docker
            .prune_networks(Some(PruneNetworksOptions { filters }))
            .await;
    }

    pub async fn initialize_bitcoin_node(
        &self,
        devnet_event_tx: &Sender<DevnetEvent>,
        no_snapshot: bool,
    ) -> Result<(), String> {
        use std::str::FromStr;

        use bitcoincore_rpc::bitcoin::Address;
        use reqwest::Client as HttpClient;
        use serde_json::json;

        let devnet_config = self.get_devnet_config()?;
        let accounts = self
            .network_config
            .as_ref()
            .map(|config| &config.accounts)
            .ok_or_else(|| "unable to initialize bitcoin node".to_string())?;

        let miner_address = Address::from_str(&devnet_config.miner_btc_address)
            .map_err(|e| format!("unable to create miner address: {e:?}"))?;

        let faucet_address = Address::from_str(&devnet_config.faucet_btc_address)
            .map_err(|e| format!("unable to create faucet address: {e:?}"))?;

        let bitcoin_node_url = format!(
            "http://{}/",
            self.services_map_hosts.as_ref().unwrap().bitcoin_node_host
        );

        fn base_builder(node_url: &str, username: &str, password: &str) -> RequestBuilder {
            let http_client = HttpClient::builder()
                .timeout(Duration::from_secs(30))
                .build()
                .expect("Unable to build http client");
            http_client
                .post(node_url)
                .timeout(Duration::from_secs(3))
                .basic_auth(username, Some(&password))
                .header("Content-Type", "application/json")
                .header("Host", &node_url[7..])
        }

        let _ = devnet_event_tx.send(DevnetEvent::info("Configuring bitcoin-node".to_string()));

        let max_errors = 30;

        let mut error_count = 0;
        // Wait for the bitcoin node to be responsive
        loop {
            let network_info = base_builder(
                &bitcoin_node_url,
                &devnet_config.bitcoin_node_username,
                &devnet_config.bitcoin_node_password,
            )
            .json(&json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": "getnetworkinfo",
                "params": []
            }))
            .send()
            .await
            .map_err(|e| format!("unable to send 'getnetworkinfo' request ({e})"));

            match network_info {
                Ok(_r) => break,
                Err(e) => {
                    error_count += 1;
                    if error_count > max_errors {
                        return Err(e);
                    } else if error_count > 1 {
                        let _ = devnet_event_tx.send(DevnetEvent::error(e));
                    }
                }
            }
            std::thread::sleep(std::time::Duration::from_secs(1));
            let _ = devnet_event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
        }

        // Only generate blocks if we're NOT using cached data
        if no_snapshot {
            let _ = devnet_event_tx.send(DevnetEvent::info(
                "Initializing blockchain with fresh blocks".to_string(),
            ));
            let mut error_count = 0;
            loop {
                let rpc_call = base_builder(
                    &bitcoin_node_url,
                    &devnet_config.bitcoin_node_username,
                    &devnet_config.bitcoin_node_password,
                )
                .json(&json!({
                    "jsonrpc": "1.0",
                    "id": "stacks-network",
                    "method": "generatetoaddress",
                    "params": [json!(3), json!(miner_address)]
                }))
                .send()
                .await
                .map_err(|e| format!("unable to send 'generatetoaddress' request ({e})"));

                match rpc_call {
                    Ok(_r) => break,
                    Err(e) => {
                        error_count += 1;
                        if error_count > max_errors {
                            return Err(e);
                        } else if error_count > 1 {
                            let _ = devnet_event_tx.send(DevnetEvent::error(e));
                        }
                    }
                }
                std::thread::sleep(std::time::Duration::from_secs(1));

                let _ =
                    devnet_event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
            }

            let mut error_count = 0;
            loop {
                let rpc_call = base_builder(
                    &bitcoin_node_url,
                    &devnet_config.bitcoin_node_username,
                    &devnet_config.bitcoin_node_password,
                )
                .json(&json!({
                    "jsonrpc": "1.0",
                    "id": "stacks-network",
                    "method": "generatetoaddress",
                    "params": [json!(97), json!(faucet_address)]
                }))
                .send()
                .await
                .map_err(|e| format!("unable to send 'generatetoaddress' request ({e})"));

                let Err(e) = rpc_call else {
                    break;
                };
                error_count += 1;
                if error_count > max_errors {
                    return Err(e);
                } else if error_count > 1 {
                    let _ = devnet_event_tx.send(DevnetEvent::error(e));
                }
                std::thread::sleep(std::time::Duration::from_secs(1));
                let _ =
                    devnet_event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
            }

            let mut error_count = 0;
            loop {
                let rpc_call = base_builder(
                    &bitcoin_node_url,
                    &devnet_config.bitcoin_node_username,
                    &devnet_config.bitcoin_node_password,
                )
                .json(&json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": "generatetoaddress",
                "params": [json!(1), json!(miner_address)]
                }))
                .send()
                .await
                .map_err(|e| format!("unable to send 'generatetoaddress' request ({e})"));

                match rpc_call {
                    Ok(_r) => break,
                    Err(e) => {
                        error_count += 1;
                        if error_count > max_errors {
                            return Err(e);
                        } else if error_count > 1 {
                            let _ = devnet_event_tx.send(DevnetEvent::error(e));
                        }
                    }
                }
                std::thread::sleep(std::time::Duration::from_secs(1));
                let _ =
                    devnet_event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
            }
        } else {
            let _ = devnet_event_tx.send(DevnetEvent::info(
                "Using snapshot - skipping initial address seeding".to_string(),
            ));
        }

        let mut error_count = 0;
        loop {
            let rpc_load_call = base_builder(
                &bitcoin_node_url,
                &devnet_config.bitcoin_node_username,
                &devnet_config.bitcoin_node_password,
            )
            .json(&json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": "loadwallet",
                "params": json!(vec![&devnet_config.miner_wallet_name])
            }))
            .send()
            .await
            .map_err(|e| format!("unable to send 'loadwallet' request ({e})"));

            let rpc_create_call = base_builder(
                &bitcoin_node_url,
                &devnet_config.bitcoin_node_username,
                &devnet_config.bitcoin_node_password,
            )
            .json(&json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": "createwallet",
                "params": json!({ "wallet_name": devnet_config.miner_wallet_name, "disable_private_keys": true })
            }))
            .send()
            .await
            .map_err(|e| format!("unable to send 'createwallet' request ({e})"));

            match rpc_create_call {
                Ok(r) => {
                    if r.status().is_success() {
                        break;
                    } else {
                        // if createwallet fails it likely means we need to load the existing wallet
                        match rpc_load_call {
                            Ok(r) => {
                                if r.status().is_success() {
                                    break;
                                } else {
                                    let err = r.text().await;
                                    let msg = format!("{err:?}");
                                    // if it returns "Wallet is already loaded" we break out
                                    match err {
                                        Ok(text) => {
                                            if text.contains("is already loaded") {
                                                break;
                                            } else {
                                                let _ =
                                                    devnet_event_tx.send(DevnetEvent::error(msg));
                                            }
                                        }
                                        Err(e) => {
                                            let _ = devnet_event_tx.send(DevnetEvent::error(
                                                format!("Failed to read error text: {e}"),
                                            ));
                                            return Err(format!("Failed to read error text: {e}"));
                                        }
                                    }
                                }
                            }
                            Err(e) => {
                                let err = r.text().await;
                                let msg = format!("{err:?}");
                                println!("msg: {msg}");
                                let _ = devnet_event_tx.send(DevnetEvent::error(msg));

                                error_count += 1;
                                if error_count > max_errors {
                                    return Err(e);
                                } else if error_count > 1 {
                                    let _ = devnet_event_tx.send(DevnetEvent::error(e));
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    error_count += 1;
                    if error_count > max_errors {
                        return Err(e);
                    } else if error_count > 1 {
                        let _ = devnet_event_tx.send(DevnetEvent::error(e));
                    }
                }
            }
            std::thread::sleep(std::time::Duration::from_secs(1));
            let _ = devnet_event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
        }

        let mut error_count = 0;
        loop {
            let descriptor = format!("addr({})", miner_address.assume_checked_ref());
            let rpc_result: JsonValue = base_builder(
                &bitcoin_node_url,
                &devnet_config.bitcoin_node_username,
                &devnet_config.bitcoin_node_password,
            )
            .json(&json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": "getdescriptorinfo",
                "params": [json!(descriptor)]

            }))
            .send()
            .await
            .map_err(|e| format!("unable to send 'getdescriptorinfo' request ({e})"))
            .map_err(|e| format!("unable to receive 'getdescriptorinfo' response: {e}"))?
            .json()
            .await
            .map_err(|e| format!("unable to parse 'getdescriptorinfo' result: {e}"))?;

            let checksum = rpc_result
                .as_object()
                .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                .get("result")
                .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                .as_object()
                .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                .get("checksum")
                .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                .as_str()
                .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                .to_string();

            let _ = devnet_event_tx.send(DevnetEvent::info(format!(
                "Registering {descriptor}#{checksum}"
            )));
            let payload = json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": "importdescriptors",
                "params": {
                    "requests": [{
                        "desc": format!("{}#{}", descriptor, checksum),
                        "timestamp": 0,
                    }]
                }
            });
            let rpc_call = base_builder(
                &bitcoin_node_url,
                &devnet_config.bitcoin_node_username,
                &devnet_config.bitcoin_node_password,
            )
            .json(&payload)
            .send()
            .await
            .map_err(|e| format!("unable to send 'importdescriptors' request ({e})"));

            match rpc_call {
                Ok(_r) => {
                    break;
                }
                Err(e) => {
                    error_count += 1;
                    if error_count > max_errors {
                        return Err(e);
                    } else if error_count > 1 {
                        let _ = devnet_event_tx.send(DevnetEvent::error(e));
                    }
                }
            }
            std::thread::sleep(std::time::Duration::from_secs(1));
            let _ = devnet_event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
        }

        let mut error_count = 0;
        loop {
            let descriptor = format!("addr({})", faucet_address.assume_checked_ref());
            let rpc_result: JsonValue = base_builder(
                &bitcoin_node_url,
                &devnet_config.bitcoin_node_username,
                &devnet_config.bitcoin_node_password,
            )
            .json(&json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": "getdescriptorinfo",
                "params": [json!(descriptor)]

            }))
            .send()
            .await
            .map_err(|e| format!("unable to send 'getdescriptorinfo' request ({e})"))
            .map_err(|e| format!("unable to receive 'getdescriptorinfo' response: {e}"))?
            .json()
            .await
            .map_err(|e| format!("unable to parse 'getdescriptorinfo' result: {e}"))?;

            let checksum = rpc_result
                .as_object()
                .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                .get("result")
                .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                .as_object()
                .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                .get("checksum")
                .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                .as_str()
                .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                .to_string();

            let _ = devnet_event_tx.send(DevnetEvent::info(format!(
                "Registering {descriptor}#{checksum}"
            )));
            let payload = json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": "importdescriptors",
                "params": {
                    "requests": [{
                        "desc": format!("{}#{}", descriptor, checksum),
                        "timestamp": 0,
                    }]
                }
            });
            let rpc_call = base_builder(
                &bitcoin_node_url,
                &devnet_config.bitcoin_node_username,
                &devnet_config.bitcoin_node_password,
            )
            .json(&payload)
            .send()
            .await
            .map_err(|e| format!("unable to send 'importdescriptors' request ({e})"));

            match rpc_call {
                Ok(_r) => {
                    break;
                }
                Err(e) => {
                    error_count += 1;
                    if error_count > max_errors {
                        return Err(e);
                    } else if error_count > 1 {
                        let _ = devnet_event_tx.send(DevnetEvent::error(e));
                    }
                }
            }
            std::thread::sleep(std::time::Duration::from_secs(1));
            let _ = devnet_event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
        }
        // Index devnet's wallets by default
        for (_, account) in accounts.iter() {
            let address = Address::from_str(&account.btc_address)
                .map_err(|e| format!("unable to create address: {e:?}"))?;

            let mut error_count = 0;
            loop {
                let descriptor = format!("addr({})", address.assume_checked_ref());
                let rpc_result: JsonValue = base_builder(
                    &bitcoin_node_url,
                    &devnet_config.bitcoin_node_username,
                    &devnet_config.bitcoin_node_password,
                )
                .json(&json!({
                    "jsonrpc": "1.0",
                    "id": "stacks-network",
                    "method": "getdescriptorinfo",
                    "params": [json!(descriptor)]

                }))
                .send()
                .await
                .map_err(|e| format!("unable to send 'getdescriptorinfo' request ({e})"))
                .map_err(|e| format!("unable to receive 'getdescriptorinfo' response: {e}"))?
                .json()
                .await
                .map_err(|e| format!("unable to parse 'getdescriptorinfo' result: {e}"))?;

                let checksum = rpc_result
                    .as_object()
                    .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                    .get("result")
                    .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                    .as_object()
                    .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                    .get("checksum")
                    .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                    .as_str()
                    .ok_or("unable to parse 'getdescriptorinfo'".to_string())?
                    .to_string();

                let _ = devnet_event_tx.send(DevnetEvent::info(format!(
                    "Registering {descriptor}#{checksum}"
                )));
                let payload = json!({
                    "jsonrpc": "1.0",
                    "id": "stacks-network",
                    "method": "importdescriptors",
                    "params": {
                        "requests": [{
                            "desc": format!("{}#{}", descriptor, checksum),
                            "timestamp": 0,
                        }]
                    }
                });
                let rpc_call = base_builder(
                    &bitcoin_node_url,
                    &devnet_config.bitcoin_node_username,
                    &devnet_config.bitcoin_node_password,
                )
                .json(&payload)
                .send()
                .await
                .map_err(|e| format!("unable to send 'importdescriptors' request ({e})"));

                match rpc_call {
                    Ok(_r) => {
                        break;
                    }
                    Err(e) => {
                        error_count += 1;
                        if error_count > max_errors {
                            return Err(e);
                        } else if error_count > 1 {
                            let _ = devnet_event_tx.send(DevnetEvent::error(e));
                        }
                    }
                }
                std::thread::sleep(std::time::Duration::from_secs(1));
                let _ =
                    devnet_event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
            }
        }

        // before generating a block, hit the getblockchaininfo and check that
        // verificationprogress == 1 before you generate the first block.
        let mut error_count = 0;
        loop {
            let rpc_result: JsonValue = base_builder(
                &bitcoin_node_url,
                &devnet_config.bitcoin_node_username,
                &devnet_config.bitcoin_node_password,
            )
            .json(&json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": "getblockchaininfo",
                "params": []
            }))
            .send()
            .await
            .map_err(|e| format!("unable to send 'getblockchaininfo' request ({e})"))?
            .json()
            .await
            .map_err(|e| format!("unable to parse 'getblockchaininfo' result: {e}"))?;

            let verification_progress = rpc_result
                .as_object()
                .ok_or("unable to parse 'getblockchaininfo'".to_string())?
                .get("result")
                .ok_or("unable to parse 'getblockchaininfo'".to_string())?
                .as_object()
                .ok_or("unable to parse 'getblockchaininfo'".to_string())?
                .get("verificationprogress")
                .ok_or("unable to parse 'getblockchaininfo'".to_string())?
                .as_f64()
                .ok_or("unable to parse verificationprogress".to_string())?;

            if verification_progress >= 1.0 {
                let _ = devnet_event_tx.send(DevnetEvent::info(
                    "Blockchain verification completed".to_string(),
                ));
                break;
            }

            error_count += 1;
            if error_count > max_errors {
                return Err("Blockchain verification timeout".to_string());
            }

            std::thread::sleep(std::time::Duration::from_millis(500));
            let _ = devnet_event_tx.send(DevnetEvent::info(format!(
                "Verification progress: {:.2}%",
                verification_progress * 100.0
            )));
        }

        if !no_snapshot {
            let _ = devnet_event_tx.send(DevnetEvent::info(
                "Using cached blockchain data - mining one block".to_string(),
            ));

            loop {
                let rpc_call = base_builder(
                    &bitcoin_node_url,
                    &devnet_config.bitcoin_node_username,
                    &devnet_config.bitcoin_node_password,
                )
                .json(&json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": "generatetoaddress",
                "params": [json!(1), json!(miner_address)]
                }))
                .send()
                .await
                .map_err(|e| format!("unable to send 'generatetoaddress' request ({e})"));

                match rpc_call {
                    Ok(_r) => break,
                    Err(e) => {
                        error_count += 1;
                        if error_count > max_errors {
                            return Err(e);
                        } else if error_count > 1 {
                            let _ = devnet_event_tx.send(DevnetEvent::error(e));
                        }
                    }
                }
                std::thread::sleep(std::time::Duration::from_secs(1));
                let _ =
                    devnet_event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
            }
        }
        Ok(())
    }

    pub async fn start_container_logs_streaming(&self, ctx: &Context) -> Result<(), String> {
        if !self.save_container_logs {
            return Ok(());
        }

        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.get_devnet_config()?;

        // Start streaming stacks-node logs
        if let Some(container_id) = &self.stacks_node_container_id {
            let log_path = PathBuf::from(&devnet_config.working_dir).join("stacks-node.log");
            let container_id = container_id.clone();
            let docker = docker.clone();
            let log_path_clone = log_path.clone();

            // Spawn a background thread to stream logs
            // TODO: use tokio::spawn instead
            // https://github.com/stx-labs/clarinet/issues/1905
            std::thread::spawn(move || {
                // Create a dedicated runtime for the Docker client to ensure stable connections
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .unwrap();
                rt.block_on(async {
                    let logs_options = LogsOptions::<String> {
                        stdout: true,
                        stderr: true,
                        follow: true,
                        ..Default::default()
                    };

                    let mut file = match File::create(&log_path_clone) {
                        Ok(file) => file,
                        Err(e) => {
                            eprintln!("Failed to create log file: {e}");
                            return;
                        }
                    };

                    match docker
                        .logs(&container_id, Some(logs_options))
                        .try_for_each(|log| {
                            match log {
                                bollard::container::LogOutput::StdOut { message }
                                | bollard::container::LogOutput::StdErr { message } => {
                                    if let Ok(log_line) = String::from_utf8(message.to_vec()) {
                                        let _ = writeln!(file, "{log_line}");
                                        let _ = file.flush(); // Ensure logs are written immediately
                                    }
                                }
                                bollard::container::LogOutput::StdIn { .. }
                                | bollard::container::LogOutput::Console { .. } => {
                                    // Skip these types as they're not relevant for logs
                                }
                            }
                            futures::future::ok(())
                        })
                        .await
                    {
                        Ok(_) => {
                            eprintln!("Container logs stream ended for {container_id}");
                        }
                        Err(e) => {
                            eprintln!("Error streaming container logs: {e}");
                        }
                    }
                });
            });

            ctx.try_log(|logger| {
                slog::info!(
                    logger,
                    "Started streaming stacks-node logs to: {}",
                    log_path.display()
                )
            });
        }

        Ok(())
    }
}

fn formatted_docker_error(message: &str, error: DockerError) -> String {
    let error = match &error {
        DockerError::DockerResponseServerError {
            status_code: _c,
            message: m,
        } => m.to_string(),
        _ => format!("{error:?}"),
    };
    format!("{message}: {error}")
}

pub fn get_global_snapshot_dir() -> std::path::PathBuf {
    let home_dir = dirs::home_dir().expect("Unable to retrieve home dir");
    home_dir.join(".clarinet").join("cache").join("devnet")
}

pub fn get_project_snapshot_dir(devnet_config: &DevnetConfig) -> std::path::PathBuf {
    PathBuf::from(&devnet_config.working_dir)
        .join("data")
        .join("1")
}

pub fn copy_directory(
    source: &PathBuf,
    destination: &PathBuf,
    exclude_patterns: Option<&[&str]>,
) -> Result<(), String> {
    fs::create_dir_all(destination).map_err(|e| {
        format!(
            "Failed to create directory {}: {}",
            destination.display(),
            e
        )
    })?;

    for entry in fs::read_dir(source)
        .map_err(|e| format!("Failed to read directory {}: {}", source.display(), e))?
    {
        let entry = entry.map_err(|e| format!("Failed to read directory entry: {e}"))?;
        let file_name = entry.file_name();
        let file_name_str = file_name.to_string_lossy();

        // Skip this file if it matches any exclude pattern
        if let Some(patterns) = &exclude_patterns {
            if patterns.iter().any(|pattern| file_name_str == *pattern) {
                continue;
            }
        }
        let entry_path = entry.path();
        let destination_path = destination.join(&file_name);

        if entry_path.is_dir() {
            copy_directory(&entry_path, &destination_path, exclude_patterns)?;
        } else {
            fs::copy(&entry_path, &destination_path).map_err(|e| {
                format!(
                    "Failed to copy {} to {}: {}",
                    entry_path.display(),
                    destination_path.display(),
                    e
                )
            })?;
        }
    }

    Ok(())
}

pub async fn copy_snapshot_to_container(
    container_id: &str,
    source_path: &Path,
    dest_path: &str,
    devnet_event_tx: &Sender<DevnetEvent>,
    service_name: &str,
) -> Result<(), String> {
    if !source_path.exists() {
        return Ok(()); // No snapshot to copy
    }

    let _ = devnet_event_tx.send(DevnetEvent::info(format!(
        "Copying {service_name} snapshot to container..."
    )));

    // Use docker cp command which handles directory creation better
    let copy_command = format!(
        "docker cp {}/ {}:{}",
        source_path.display(),
        container_id,
        dest_path
    );
    let output =
        run_command(&copy_command).map_err(|e| format!("Failed to execute docker cp: {e}"))?;

    if !output.status.success() {
        return Err(format!(
            "docker cp failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    let _ = devnet_event_tx.send(DevnetEvent::success(format!(
        "{service_name} snapshot copied to container successfully"
    )));

    Ok(())
}
