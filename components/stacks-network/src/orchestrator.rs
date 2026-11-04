use std::cell::Cell;
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::mpsc::Sender;
use std::time::{Duration, Instant};

use bitcoincore_rpc::bitcoin::hex::DisplayHex;
use bitcoincore_rpc::json::GetDescriptorInfoResult;
use bitcoincore_rpc::jsonrpc::Response as JsonRpcResponse;
use bollard::container::{
    Config, CreateContainerOptions, InspectContainerOptions, ListContainersOptions, LogsOptions,
    RemoveContainerOptions,
};
use bollard::errors::Error as DockerError;
use bollard::image::CreateImageOptions;
use bollard::models::{ContainerState, ContainerStateStatusEnum, HostConfig, PortBinding};
use bollard::network::{CreateNetworkOptions, PruneNetworksOptions};
use bollard::service::Ipam;
use bollard::Docker;
use clarinet_deployments::types::BurnchainEpochConfig;
use clarinet_files::{
    DevnetConfig, NetworkManifest, ProjectManifest, StacksNetwork, StacksNodeEventObserver,
    DEFAULT_DOCKER_PLATFORM, DEFAULT_POX_PREPARE_LENGTH, DEFAULT_POX_REWARD_LENGTH,
};
use clarity::types::chainstate::StacksPrivateKey;
use clarity::types::PrivateKey;
use futures::stream::{FuturesUnordered, StreamExt, TryStreamExt};
use hiro_system_kit::slog;
use indoc::formatdoc;
use observer::utils::Context;
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::UnboundedReceiver;

use crate::bitcoin_rpc_client::BitcoinRpcClient;
use crate::command::run_docker_command;
use crate::event::{send_status_update, DevnetEvent, Status};
use crate::snapshot_extractor::SnapshotImages;

const BITCOIND_DATA_DIR: &str = "/home/bitcoin/.bitcoin";

const DOCKER_ERR_MSG: &str = "unable to get docker client";

#[derive(Serialize)]
struct StacksNodeEventObserversConfig<'a> {
    events_observer: &'a [StacksNodeEventObserver],
}

fn serialize_stacks_node_event_observers(observers: &[StacksNodeEventObserver]) -> String {
    // Endpoints and event keys are plain strings, which always have a TOML form.
    toml::to_string(&StacksNodeEventObserversConfig {
        events_observer: observers,
    })
    .expect("event observers are strings and string arrays")
}

/// Every event observer the devnet stacks-node reports to: the signers, the orchestrator
/// (docker-host), the stacks-api when enabled, and those configured in Devnet.toml.
fn stacks_node_event_observers(
    network_name: &str,
    devnet_config: &DevnetConfig,
) -> Vec<StacksNodeEventObserver> {
    // the signer endpoints are
    // `stacks-signer-0.<network>:30000`
    // `stacks-signer-1.<network>:30001`
    // ...
    let mut observers: Vec<StacksNodeEventObserver> = (0..devnet_config.stacks_signers_keys.len())
        .map(|i| StacksNodeEventObserver {
            endpoint: format!("stacks-signer-{i}.{network_name}:{}", 30000 + i),
            events_keys: vec![
                "stackerdb".into(),
                "block_proposal".into(),
                "burn_blocks".into(),
            ],
        })
        .collect();

    // also used by the devnet observer instance
    observers.push(StacksNodeEventObserver {
        endpoint: format!(
            "host.docker.internal:{}",
            devnet_config.orchestrator_ingestion_port
        ),
        events_keys: vec!["*".into()],
    });

    if !devnet_config.disable_stacks_api {
        observers.push(StacksNodeEventObserver {
            endpoint: format!(
                "stacks-api.{network_name}:{}",
                devnet_config.stacks_api_events_port
            ),
            events_keys: vec!["*".into()],
        });
    }

    observers.extend(devnet_config.stacks_node_events_observers.iter().cloned());
    observers
}

#[derive(Debug)]
pub struct DevnetOrchestrator {
    pub name: String,
    network_name: String,
    pub manifest: ProjectManifest,
    network_config: NetworkManifest,
    pub termination_success_tx: Option<Sender<bool>>,
    pub can_exit: bool,
    stacks_node_container_id: Option<String>,
    stacks_signers_containers_ids: Vec<String>,
    docker_network_id: Option<String>,
    stacks_api_container_id: Option<String>,
    stacks_explorer_container_id: Option<String>,
    bitcoin_node_container_id: Option<String>,
    bitcoin_explorer_container_id: Option<String>,
    postgres_container_id: Option<String>,
    docker_client: Option<Docker>,
    services_map_hosts: Option<ServicesMapHosts>,
    save_container_logs: bool,
    container_index: Cell<u32>,
}
#[derive(Deserialize)]
struct MempoolLoadInfo {
    loaded: bool,
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
pub const DEVNET_SNAPSHOT_READY_MARKER: &str = "epoch_4_ready";

impl DevnetOrchestrator {
    pub fn new(
        manifest: ProjectManifest,
        should_use_docker: bool,
    ) -> Result<DevnetOrchestrator, String> {
        let mut network_config = NetworkManifest::from_project_root(
            &manifest.root_dir,
            &StacksNetwork::Devnet.get_networks(),
            false,
            Some(&manifest.project.cache_location),
        )?;
        let devnet = network_config
            .devnet
            .as_mut()
            .ok_or("unable to read the devnet configuration")?;

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

        let name = manifest.project.name.to_string();
        let mut network_name = name.clone();
        if let Some(ref network_id) = devnet.network_id {
            network_name.push_str(&format!(".{network_id}"));
        }
        network_name.push_str(&format!(".{}", devnet.name));

        let docker_client = if should_use_docker {
            let client = if let Some(ref host) = devnet.docker_host {
                Docker::connect_with_socket(host, 120, bollard::API_DEFAULT_VERSION)
            } else {
                Docker::connect_with_defaults()
            }
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
        } else {
            None
        };

        Ok(DevnetOrchestrator {
            name,
            network_name,
            manifest,
            network_config,
            docker_client,
            can_exit: true,
            termination_success_tx: None,
            stacks_node_container_id: None,
            stacks_signers_containers_ids: vec![],
            docker_network_id: None,
            stacks_api_container_id: None,
            stacks_explorer_container_id: None,
            bitcoin_node_container_id: None,
            bitcoin_explorer_container_id: None,
            postgres_container_id: None,
            services_map_hosts: None,
            save_container_logs: false,
            container_index: Cell::new(0),
        })
    }

    pub fn network_config(&self) -> &NetworkManifest {
        &self.network_config
    }

    pub fn devnet_config(&self) -> &DevnetConfig {
        self.network_config
            .devnet
            .as_ref()
            .expect("checked in DevnetOrchestrator::new")
    }

    pub fn stacks_api_container_name(&self) -> String {
        format!("stacks-api.{}", self.network_name)
    }

    async fn pull_image(&self, image_url: &str, platform: String) -> Result<(), String> {
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        docker
            .create_image(
                Some(CreateImageOptions {
                    from_image: image_url.to_string(),
                    platform,
                    ..Default::default()
                }),
                None,
                None,
            )
            .try_collect::<Vec<_>>()
            .await
            .map_err(|e| formatted_docker_error("unable to pull image", e))?;
        Ok(())
    }

    fn project_labels(&self, service: &str) -> HashMap<String, String> {
        self.container_index.set(self.container_index.get() + 1);
        HashMap::from([
            ("project".to_string(), self.network_name.clone()),
            // the docker.compose.* labels make the project behave like a docker compose one
            // so users can do `docker compose -p <self.name> logs -f`
            ("com.docker.compose.project".to_string(), self.name.clone()),
            ("com.docker.compose.config-hash".to_string(), "".to_string()),
            ("com.docker.compose.oneoff".to_string(), "False".to_string()),
            (
                "com.docker.compose.service".to_string(),
                service.to_string(),
            ),
            (
                "com.docker.compose.container-number".to_string(),
                self.container_index.get().to_string(),
            ),
        ])
    }

    pub async fn prepare_local_network(&mut self) -> Result<ServicesMapHosts, String> {
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.devnet_config();

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
                ipam: Ipam::default(),
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

        self.docker_network_id = Some(network_id);
        self.services_map_hosts = Some(services_map_hosts.clone());

        Ok(services_map_hosts)
    }

    /// Runs the devnet until shutdown or a container failure, then removes its containers.
    pub async fn start(
        &mut self,
        event_tx: Sender<DevnetEvent>,
        mut terminator_rx: UnboundedReceiver<()>,
        ctx: &Context,
        no_snapshot: bool,
        save_container_logs: bool,
    ) -> Result<(), String> {
        self.save_container_logs = save_container_logs;
        let result = tokio::select! {
            booted = self.boot(&event_tx, ctx, no_snapshot) => match booted {
                Ok(()) => self.wait_for_termination(&mut terminator_rx).await,
                Err(message) => Err(message),
            },
            _ = terminator_rx.recv() => Ok(()),
        };
        if let Err(message) = &result {
            let _ = event_tx.send(DevnetEvent::error(format!(
                "{message}, removing devnet containers"
            )));
        }
        self.kill(ctx, result.as_ref().err().map(String::as_str))
            .await;
        result
    }

    async fn boot(
        &mut self,
        event_tx: &Sender<DevnetEvent>,
        ctx: &Context,
        no_snapshot: bool,
    ) -> Result<(), String> {
        let devnet_config = self.devnet_config();

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

        send_status_update(event_tx, "bitcoin-node", Status::Red, "initializing");

        send_status_update(event_tx, "stacks-node", Status::Red, "initializing");

        send_status_update(event_tx, "stacks-signers", Status::Red, "initializing");

        if !disable_stacks_api {
            send_status_update(event_tx, "stacks-api", Status::Red, "initializing");
        }
        if !disable_stacks_explorer {
            send_status_update(event_tx, "stacks-explorer", Status::Red, "initializing");
        }
        if !disable_bitcoin_explorer {
            send_status_update(event_tx, "bitcoin-explorer", Status::Red, "initializing");
        }

        let _ = event_tx.send(DevnetEvent::info(format!(
            "Creating network {}",
            self.network_name
        )));

        // Start bitcoind
        let _ = event_tx.send(DevnetEvent::info("Starting bitcoin-node".to_string()));
        send_status_update(
            event_tx,
            "bitcoin-node",
            Status::Yellow,
            "preparing container",
        );
        self.prepare_bitcoin_node_container(ctx, no_snapshot)
            .await?;
        send_status_update(event_tx, "bitcoin-node", Status::Yellow, "booting");
        self.boot_bitcoin_node_container(event_tx, no_snapshot)
            .await?;
        self.initialize_bitcoin_node(event_tx, no_snapshot).await?;

        // Start postgres container
        if !disable_postgres {
            let _ = event_tx.send(DevnetEvent::info("Starting postgres".to_string()));
            self.prepare_postgres_container(ctx).await?;
            self.boot_postgres_container(ctx).await?;
        };

        // Start stacks-api
        if !disable_stacks_api {
            send_status_update(
                event_tx,
                "stacks-api",
                Status::Yellow,
                "preparing container",
            );
            let _ = event_tx.send(DevnetEvent::info("Starting stacks-api".to_string()));
            self.prepare_stacks_api_container(ctx).await?;
            self.boot_stacks_api_container(ctx, no_snapshot).await?;
            send_status_update(
                event_tx,
                "stacks-api",
                Status::Green,
                &format!("http://localhost:{stacks_api_port}/doc"),
            );
        }

        // Start stacks-node
        let _ = event_tx.send(DevnetEvent::info("Starting stacks-node".to_string()));
        send_status_update(event_tx, "stacks-node", Status::Yellow, "updating image");
        self.prepare_stacks_node_container(ctx).await?;
        send_status_update(event_tx, "stacks-node", Status::Yellow, "booting");
        self.boot_stacks_node_container(event_tx, no_snapshot)
            .await?;

        // Start streaming container logs if enabled
        let _ = self.start_container_logs_streaming(ctx).await;

        for (i, signer_key) in signers_keys.iter().enumerate() {
            let _ = event_tx.send(DevnetEvent::info(format!("Starting stacks-signer-{i}")));
            send_status_update(event_tx, "stacks-signers", Status::Yellow, "updating image");
            self.prepare_stacks_signer_container(ctx, i as u32, signer_key)
                .await?;
            send_status_update(
                event_tx,
                "stacks-signers",
                Status::Yellow,
                &format!("booting signer {i}"),
            );
            self.boot_stacks_signer_container(i as u32).await?;
        }
        let signers_count = signers_keys.len();
        let message = format!(
            "{} signer{} running",
            signers_count,
            if signers_count > 1 { "s" } else { "" }
        );
        send_status_update(event_tx, "stacks-signers", Status::Green, &message);

        // Start stacks-explorer
        if !disable_stacks_explorer {
            send_status_update(
                event_tx,
                "stacks-explorer",
                Status::Yellow,
                "preparing container",
            );
            self.prepare_stacks_explorer_container(ctx).await?;
            let _ = event_tx.send(DevnetEvent::info("Starting stacks-explorer".to_string()));
            self.boot_stacks_explorer_container(ctx).await?;
            send_status_update(
                event_tx,
                "stacks-explorer",
                Status::Green,
                &format!("http://localhost:{stacks_explorer_port}"),
            );
        }

        // Start bitcoin-explorer
        if !disable_bitcoin_explorer {
            send_status_update(
                event_tx,
                "bitcoin-explorer",
                Status::Yellow,
                "preparing container",
            );
            self.prepare_bitcoin_explorer_container(ctx).await?;
            let _ = event_tx.send(DevnetEvent::info("Starting bitcoin-explorer".to_string()));
            self.boot_bitcoin_explorer_container(ctx).await?;
            send_status_update(
                event_tx,
                "bitcoin-explorer",
                Status::Green,
                &format!("http://localhost:{bitcoin_explorer_port}"),
            );
        }

        Ok(())
    }

    async fn wait_for_termination(
        &self,
        terminator_rx: &mut UnboundedReceiver<()>,
    ) -> Result<(), String> {
        tokio::select! {
            _ = terminator_rx.recv() => Ok(()),
            Some(message) = self.wait_for_node_exit() => Err(message),
        }
    }

    /// Node and signer containers are not auto-removed, so a crash during boot keeps its exit code.
    async fn wait_for_node_exit(&self) -> Option<String> {
        let docker = self.docker_client.as_ref()?;
        let nodes = [
            ("bitcoin-node", &self.bitcoin_node_container_id),
            ("stacks-node", &self.stacks_node_container_id),
        ]
        .into_iter()
        .filter_map(|(name, id)| Some((name.to_string(), id.as_deref()?)));
        let signers = self
            .stacks_signers_containers_ids
            .iter()
            .enumerate()
            .map(|(i, id)| (format!("stacks-signer-{i}"), id.as_str()));
        let mut exits: FuturesUnordered<_> = nodes
            .chain(signers)
            .map(|(name, id)| async move {
                wait_for_container_exit(docker, &name, id, &WATCH_TIMINGS).await
            })
            .collect();
        exits.next().await
    }

    pub fn prepare_bitcoin_node_config(&self, no_snapshot: bool) -> Result<Config<String>, String> {
        let devnet_config = self.devnet_config();

        let port_bindings = HashMap::from([
            (
                format!("{}/tcp", devnet_config.bitcoin_node_rpc_port),
                port_binding(devnet_config.bitcoin_node_rpc_port),
            ),
            (
                format!("{}/tcp", devnet_config.bitcoin_node_p2p_port),
                port_binding(devnet_config.bitcoin_node_p2p_port),
            ),
            // ZMQ block notifications
            ("28332/tcp".to_string(), port_binding(28332)),
            // ZMQ transaction notifications
            ("28333/tcp".to_string(), port_binding(28333)),
        ]);

        let bitcoind_conf = formatdoc!(
            "server=1
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
            disablewallet=0
            fallbackfee=0.00001

            [regtest]
            bind=0.0.0.0:{bitcoin_node_p2p_port}
            rpcbind=0.0.0.0:{bitcoin_node_rpc_port}
            rpcport={bitcoin_node_rpc_port}
            ",
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

        let bitcoind_data_path = get_project_snapshot_dir(devnet_config).join("bitcoin");
        fs::create_dir_all(&bitcoind_data_path)
            .map_err(|e| format!("unable to create bitcoin directory: {e:?}"))?;

        let exposed_ports = HashMap::from([
            (
                format!("{}/tcp", devnet_config.bitcoin_node_rpc_port),
                HashMap::new(),
            ),
            (
                format!("{}/tcp", devnet_config.bitcoin_node_p2p_port),
                HashMap::new(),
            ),
        ]);

        let labels = self.project_labels("bitcoin-node");

        let mut env = vec![];
        if devnet_config.bitcoin_controller_automining_disabled {
            env.push("STACKS_BITCOIN_AUTOMINING_DISABLED=1".to_string());
        }
        // The bitcoin/bitcoin entrypoint runs bitcoind under this uid, so files in the
        // bind-mounted datadir and those restored by `docker cp` stay owned by the host user.
        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;
            let uid = fs::metadata(&bitcoind_data_path)
                .map_err(|e| format!("unable to read bitcoin directory metadata: {e:?}"))?
                .uid();
            env.push(format!("UID={uid}"));
        }

        let working_dir = &devnet_config.working_dir;
        let mut binds = vec![format!("{working_dir}/conf:/etc/bitcoin")];

        if devnet_config.bind_containers_volumes {
            binds.push(format!(
                "{}:{BITCOIND_DATA_DIR}",
                bitcoind_data_path.display()
            ));
        }

        let mut cmd_args = vec![
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
            image: Some(devnet_config.bitcoin_node_image_url.clone()),
            domainname: Some(self.network_name.to_string()),
            tty: None,
            exposed_ports: Some(exposed_ports),
            env: Some(env),
            host_config: Some(HostConfig {
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
        let devnet_config = self.devnet_config();
        self.pull_image(
            &devnet_config.bitcoin_node_image_url.clone(),
            devnet_config.docker_platform.clone().unwrap_or_default(),
        )
        .await?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.devnet_config();
        let container_name = format!("bitcoin-node.{}", self.network_name);
        let options = CreateContainerOptions {
            name: container_name.as_str(),
            platform: devnet_config.docker_platform.as_deref(),
        };

        let config = self.prepare_bitcoin_node_config(no_snapshot)?;

        let container = match docker
            .create_container::<&str, String>(Some(options.clone()), config.clone())
            .await
            .map_err(|e| formatted_docker_error("unable to create bitcoind container", e))
        {
            Ok(container) => container.id,
            Err(_e) => {
                // Only a leftover of this project is replaced; another same-named container is kept
                let leftover = docker
                    .inspect_container(&container_name, None::<InspectContainerOptions>)
                    .await
                    .is_ok_and(|container| {
                        container
                            .config
                            .and_then(|config| config.labels)
                            .is_some_and(|labels| labels.get("project") == Some(&self.network_name))
                    });
                if leftover {
                    let _ = docker
                        .remove_container(&container_name, Some(FORCE_REMOVE))
                        .await;
                }
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
        let filters = HashMap::from([(
            "label".to_string(),
            vec![format!("project={}", self.network_name)],
        )]);
        let options = Some(ListContainersOptions {
            all: true,
            filters: filters.clone(),
            ..Default::default()
        });

        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let res = docker.list_containers(options).await;
        let url = "https://docs.stacks.co/clarinet/local-blockchain-development#common-issues";
        let containers = res.map_err(|e| {
            format!("unable to communicate with Docker: {e}\nvisit {url} to resolve this issue.")
        })?;

        // Not pruned: a prune leaves the anonymous volumes behind
        for container_id in containers.iter().filter_map(|c| c.id.as_ref()) {
            let _ = docker
                .remove_container(container_id, Some(FORCE_REMOVE))
                .await;
        }
        let _ = docker
            .prune_networks(Some(PruneNetworksOptions { filters }))
            .await;
        Ok(())
    }

    pub async fn boot_bitcoin_node_container(
        &mut self,
        devnet_event_tx: &Sender<DevnetEvent>,
        no_snapshot: bool,
    ) -> Result<(), String> {
        let container = self
            .bitcoin_node_container_id
            .as_ref()
            .ok_or("unable to boot container")?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;

        // Copied before start: bitcoind would otherwise race the copy and reindex a partial
        // datadir, and the image entrypoint only chowns what is already there
        if !no_snapshot {
            let devnet_config = self.devnet_config();
            let bitcoin_snapshot = get_global_snapshot_dir(devnet_config)
                .join("bitcoin")
                .join("regtest");
            let docker_host = devnet_config.docker_host.as_deref();

            copy_snapshot_to_container(
                container,
                &bitcoin_snapshot,
                &format!("{BITCOIND_DATA_DIR}/regtest"),
                devnet_event_tx,
                "Bitcoin",
                docker_host,
            )
            .await?;
        }

        docker
            .start_container::<String>(container, None)
            .await
            .map_err(|e| formatted_docker_error("unable to start bitcoind container", e))?;

        Ok(())
    }

    pub fn prepare_stacks_node_config(&self) -> Result<Config<String>, String> {
        let network_config = &self.network_config;
        let devnet_config = self.devnet_config();

        let port_bindings = HashMap::from([
            (
                format!("{}/tcp", devnet_config.stacks_node_p2p_port),
                port_binding(devnet_config.stacks_node_p2p_port),
            ),
            (
                format!("{}/tcp", devnet_config.stacks_node_rpc_port),
                port_binding(devnet_config.stacks_node_rpc_port),
            ),
        ]);

        let mut stacks_conf = formatdoc!(
            r#"[node]
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

        // pox-5 needs sbtc-token and sbtc-registry contracts to be resolvable.
        // sBTC contracts are always deployed as boot dependencies, so always
        // tell the stacks-node where they'll be deployed.
        if let Some(deployer) = network_config
            .accounts
            .values()
            .find(|a| a.label == "deployer")
        {
            let deployer_addr = &deployer.stx_address;
            stacks_conf = stacks_conf.replacen(
                "[connection_options]",
                &formatdoc!(
                    r#"pox_5_sbtc_contract = "{deployer_addr}.sbtc-token"
                    pox_5_sbtc_registry_contract = "{deployer_addr}.sbtc-registry"
                    pox_5_bond_admin = "{deployer_addr}"

                    [connection_options]"#,
                ),
                1,
            );
        }

        for account in network_config.accounts.values() {
            stacks_conf.push_str(&formatdoc!(
                r#"
                [[ustx_balance]]
                address = "{address}"
                amount = {amount}
                "#,
                address = account.stx_address,
                amount = account.balance,
            ));
        }

        stacks_conf.push_str(&serialize_stacks_node_event_observers(
            &stacks_node_event_observers(&self.network_name, devnet_config),
        ));

        stacks_conf.push_str(&formatdoc!(
            r#"
            [burnchain]
            chain = "bitcoin"
            mode = "krypton"
            magic_bytes = "T3"
            first_burn_block_height = 100
            pox_prepare_length = {pox_prepare_length}
            pox_reward_length = {pox_reward_length}
            burn_fee_cap = 20_000
            poll_time_secs = 1
            timeout = 2
            peer_host = "host.docker.internal"
            wallet_name = "{miner_wallet_name}"
            username = "{bitcoin_node_username}"
            password = "{bitcoin_node_password}"
            rpc_port = {orchestrator_ingestion_port}
            peer_port = {bitcoin_node_p2p_port}
            "#,
            pox_prepare_length = DEFAULT_POX_PREPARE_LENGTH,
            pox_reward_length = DEFAULT_POX_REWARD_LENGTH,
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

        let stacks_node_data_path = get_project_snapshot_dir(devnet_config).join("stacks");
        fs::create_dir_all(&stacks_node_data_path)
            .map_err(|e| format!("unable to create stacks directory: {e:?}"))?;

        let exposed_ports = HashMap::from([
            (
                format!("{}/tcp", devnet_config.stacks_node_rpc_port),
                HashMap::new(),
            ),
            (
                format!("{}/tcp", devnet_config.stacks_node_p2p_port),
                HashMap::new(),
            ),
        ]);

        let labels = self.project_labels("stacks-node");

        let mut binds = vec![format!(
            "{}/conf:/src/stacks-node/",
            devnet_config.working_dir
        )];

        if devnet_config.bind_containers_volumes {
            binds.push(format!("{}:/devnet/", stacks_node_data_path.display()))
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

    pub async fn prepare_stacks_node_container(&mut self, ctx: &Context) -> Result<(), String> {
        let devnet_config = self.devnet_config();
        self.pull_image(
            &devnet_config.stacks_node_image_url.clone(),
            devnet_config.docker_platform.clone().unwrap_or_default(),
        )
        .await?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.devnet_config();
        let options = CreateContainerOptions {
            name: format!("stacks-node.{}", self.network_name),
            platform: devnet_config.docker_platform.clone(),
        };

        let config = self.prepare_stacks_node_config()?;

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
        let container = self
            .stacks_node_container_id
            .as_ref()
            .ok_or("unable to boot container")?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;

        if !no_snapshot {
            let devnet_config = self.devnet_config();
            let stacks_snapshot = get_global_snapshot_dir(devnet_config).join("stacks");
            let docker_host = devnet_config.docker_host.as_deref();

            copy_snapshot_to_container(
                container,
                &stacks_snapshot,
                "/devnet",
                devnet_event_tx,
                "Stacks",
                docker_host,
            )
            .await?;
        }

        docker
            .start_container::<String>(container, None)
            .await
            .map_err(|e| formatted_docker_error("unable to start stacks-node container", e))?;

        Ok(())
    }

    pub fn prepare_stacks_signer_config(
        &self,
        signer_id: u32,
        signer_key: &StacksPrivateKey,
    ) -> Result<Config<String>, String> {
        let devnet_config = self.devnet_config();

        let signer_conf = formatdoc!(
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

        let data_dir = get_project_snapshot_dir(devnet_config);
        fs::create_dir_all(data_dir.join("signer"))
            .map_err(|e| format!("unable to create stacks directory: {e:?}"))?;

        let labels = self.project_labels(&format!("stacks-signer-{signer_id}"));

        let mut binds = vec![format!(
            "{}/conf:/src/stacks-signer/",
            devnet_config.working_dir
        )];

        if devnet_config.bind_containers_volumes {
            binds.push(format!("{}:/devnet/", data_dir.join("stacks").display()))
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
        ctx: &Context,
        signer_id: u32,
        signer_key: &StacksPrivateKey,
    ) -> Result<(), String> {
        let devnet_config = self.devnet_config();
        self.pull_image(
            &devnet_config.stacks_signer_image_url.clone(),
            devnet_config.docker_platform.clone().unwrap_or_default(),
        )
        .await?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.devnet_config();
        let options = CreateContainerOptions {
            name: format!("stacks-signer-{signer_id}.{}", self.network_name),
            platform: devnet_config.docker_platform.clone(),
        };

        let config = self.prepare_stacks_signer_config(signer_id, signer_key)?;

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
        let container = &self.stacks_signers_containers_ids[signer_id as usize];
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;

        docker
            .start_container::<String>(container, None)
            .await
            .map_err(|e| formatted_docker_error("unable to start stacks-signer container", e))?;

        Ok(())
    }

    pub async fn prepare_stacks_api_container(&mut self, ctx: &Context) -> Result<(), String> {
        let devnet_config = self.devnet_config();
        self.pull_image(
            &devnet_config.stacks_api_image_url.clone(),
            devnet_config.docker_platform.clone().unwrap_or_default(),
        )
        .await?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.devnet_config();
        let options = CreateContainerOptions {
            name: self.stacks_api_container_name(),
            platform: devnet_config.docker_platform.clone(),
        };

        let port_bindings = HashMap::from([(
            format!("{}/tcp", devnet_config.stacks_api_port),
            port_binding(devnet_config.stacks_api_port),
        )]);

        let exposed_ports = HashMap::from([(
            format!("{}/tcp", devnet_config.stacks_api_port),
            HashMap::new(),
        )]);

        let labels = self.project_labels("stacks-api");

        let mut env = vec![
            format!("STACKS_CORE_RPC_HOST=stacks-node.{}", self.network_name),
            "STACKS_BLOCKCHAIN_API_DB=pg".to_string(),
            format!(
                "STACKS_CORE_RPC_PORT={}",
                devnet_config.stacks_node_rpc_port
            ),
            format!(
                "STACKS_BLOCKCHAIN_API_PORT={}",
                devnet_config.stacks_api_port
            ),
            "STACKS_BLOCKCHAIN_API_HOST=0.0.0.0".to_string(),
            format!(
                "STACKS_CORE_EVENT_PORT={}",
                devnet_config.stacks_api_events_port
            ),
            "STACKS_CORE_EVENT_HOST=0.0.0.0".to_string(),
            "STACKS_API_ENABLE_FT_METADATA=1".to_string(),
            format!("PG_HOST=postgres.{}", self.network_name),
            "PG_PORT=5432".to_string(),
            format!("PG_USER={}", devnet_config.postgres_username),
            format!("PG_PASSWORD={}", devnet_config.postgres_password),
            format!("PG_DATABASE={}", devnet_config.stacks_api_postgres_database),
            "STACKS_CHAIN_ID=2147483648".to_string(),
            "V2_POX_MIN_AMOUNT_USTX=90000000260".to_string(),
            format!("FAUCET_PRIVATE_KEY={}", devnet_config.faucet_secret_key_hex),
            "PG_SCHEMA=public".to_string(),
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
        let events_path = get_global_snapshot_dir(devnet_config)
            .join("events_export")
            .join("events_cache.tsv");
        events_path.exists().then_some(events_path)
    }
    pub async fn boot_stacks_api_container(
        &self,
        ctx: &Context,
        no_snapshot: bool,
    ) -> Result<(), String> {
        let container = self
            .stacks_api_container_id
            .as_ref()
            .ok_or("unable to boot container")?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;

        docker
            .start_container::<String>(container, None)
            .await
            .map_err(|e| formatted_docker_error("unable to start stacks-api container", e))?;

        let devnet_config = self.devnet_config();

        // Check if we need to import events
        if !no_snapshot {
            if let Some(events_path) = self.has_events_to_import(devnet_config) {
                // Wait for the container to be running
                loop {
                    let is_running = docker
                        .inspect_container(container, None::<InspectContainerOptions>)
                        .await
                        .ok()
                        .and_then(|info| info.state)
                        .and_then(|state| state.running)
                        .unwrap_or(false);
                    if is_running {
                        break;
                    }
                    tokio::time::sleep(Duration::from_millis(1000)).await;
                }
                // the container seems to need an extra second to be really ready
                tokio::time::sleep(Duration::from_millis(1000)).await;

                ctx.try_log(|logger| {
                    slog::info!(logger, "Importing events from {}", events_path.display())
                });

                // Copy the events file to the container
                let container_name = self.stacks_api_container_name();
                let copy_command = format!(
                    "docker cp {} {container_name}:/tmp/events_cache.tsv",
                    events_path.display()
                );
                let output =
                    run_docker_command(&copy_command, devnet_config.docker_host.as_deref())
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
                let output =
                    run_docker_command(&import_command, devnet_config.docker_host.as_deref())
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
                    ctx.try_log(|logger| {
                        slog::info!(logger, "Events import completed successfully")
                    });
                }
            }
        }
        Ok(())
    }

    pub async fn prepare_postgres_container(&mut self, ctx: &Context) -> Result<(), String> {
        let devnet_config = self.devnet_config();
        self.pull_image(
            &devnet_config.postgres_image_url.clone(),
            devnet_config.docker_platform.clone().unwrap_or_default(),
        )
        .await?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let options = CreateContainerOptions {
            name: format!("postgres.{}", self.network_name),
            platform: devnet_config.docker_platform.clone(),
        };

        let port_bindings = HashMap::from([(
            "5432/tcp".to_string(),
            port_binding(devnet_config.postgres_port),
        )]);

        let exposed_ports = HashMap::new();

        let labels = self.project_labels("postgres");

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
        let container = self
            .postgres_container_id
            .as_ref()
            .ok_or("unable to boot container")?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;

        docker
            .start_container::<String>(container, None)
            .await
            .map_err(|e| formatted_docker_error("unable to start postgres container", e))?;

        Ok(())
    }

    pub async fn prepare_stacks_explorer_container(&mut self, ctx: &Context) -> Result<(), String> {
        let devnet_config = self.devnet_config();
        // default platform to linux/amd64 since the image is built for that
        let platform = devnet_config
            .docker_platform
            .clone()
            .unwrap_or_else(|| DEFAULT_DOCKER_PLATFORM.to_string());
        self.pull_image(
            &devnet_config.stacks_explorer_image_url.clone(),
            platform.clone(),
        )
        .await?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.devnet_config();
        let options = CreateContainerOptions {
            name: format!("stacks-explorer.{}", self.network_name),
            platform: Some(platform),
        };

        let explorer_guest_port = 3000_u16;
        let port_bindings = HashMap::from([(
            format!("{explorer_guest_port}/tcp"),
            port_binding(devnet_config.stacks_explorer_port),
        )]);

        let exposed_ports = HashMap::from([(format!("{explorer_guest_port}/tcp"), HashMap::new())]);

        let labels = self.project_labels("stacks-explorer");

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
        let container = self
            .stacks_explorer_container_id
            .as_ref()
            .ok_or("unable to boot container")?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;

        docker
            .start_container::<String>(container, None)
            .await
            .map_err(|e| format!("unable to start container: {e}"))?;

        Ok(())
    }

    pub async fn prepare_bitcoin_explorer_container(
        &mut self,
        ctx: &Context,
    ) -> Result<(), String> {
        let devnet_config = self.devnet_config();
        // default platform to linux/amd64 since the image is built for that
        let platform = devnet_config
            .docker_platform
            .clone()
            .unwrap_or_else(|| DEFAULT_DOCKER_PLATFORM.to_string());
        self.pull_image(
            &devnet_config.bitcoin_explorer_image_url.clone(),
            platform.clone(),
        )
        .await?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.devnet_config();
        let options = CreateContainerOptions {
            name: format!("bitcoin-explorer.{}", self.network_name),
            platform: Some(platform),
        };

        let port_bindings = HashMap::from([(
            format!("{}/tcp", devnet_config.bitcoin_explorer_port),
            port_binding(devnet_config.bitcoin_explorer_port),
        )]);

        let exposed_ports = HashMap::from([(
            format!("{}/tcp", devnet_config.bitcoin_explorer_port),
            HashMap::new(),
        )]);

        let labels = self.project_labels("bitcoin-explorer");

        let config = Config {
            labels: Some(labels),
            image: Some(devnet_config.bitcoin_explorer_image_url.clone()),
            // domainname: Some(self.network_name.to_string()),
            tty: None,
            exposed_ports: Some(exposed_ports),
            env: Some(vec![
                "BTCEXP_HOST=0.0.0.0".to_string(),
                format!("BTCEXP_PORT={}", devnet_config.bitcoin_explorer_port),
                "BTCEXP_BITCOIND_HOST=host.docker.internal".to_string(),
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
                "BTCEXP_RPC_ALLOWALL=true".to_string(),
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
        let container = self
            .bitcoin_explorer_container_id
            .as_ref()
            .ok_or("unable to boot container")?;
        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;

        docker
            .start_container::<String>(container, None)
            .await
            .map_err(|e| format!("unable to start container: {e}"))?;

        Ok(())
    }

    pub async fn kill(&self, ctx: &Context, fatal_message: Option<&str>) {
        let Some(docker) = self.docker_client.as_ref() else {
            return;
        };
        let devnet_config = self.devnet_config();

        // Terminate containers
        let container_ids = [
            &self.bitcoin_explorer_container_id,
            &self.stacks_explorer_container_id,
            &self.bitcoin_node_container_id,
            &self.stacks_api_container_id,
            &self.postgres_container_id,
            &self.stacks_node_container_id,
        ];

        for container_id in container_ids
            .into_iter()
            .flatten()
            .chain(&self.stacks_signers_containers_ids)
        {
            ctx.try_log(|logger| slog::info!(logger, "Terminating container: {}", &container_id));
            let _ = docker
                .remove_container(container_id, Some(FORCE_REMOVE))
                .await;
        }

        // By id, not name or label: a newer session for the same project reuses both
        if let Some(network_id) = &self.docker_network_id {
            let _ = docker.remove_network(network_id).await;
        }
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

    async fn initialize_bitcoin_node(
        &self,
        devnet_event_tx: &Sender<DevnetEvent>,
        no_snapshot: bool,
    ) -> Result<(), String> {
        use std::str::FromStr;

        use bitcoincore_rpc::bitcoin::Address;
        use serde_json::json;

        let devnet_config = self.devnet_config();
        let accounts = &self.network_config.accounts;

        let miner_address = Address::from_str(&devnet_config.miner_btc_address)
            .map_err(|e| format!("unable to create miner address: {e:?}"))?;

        let faucet_address = Address::from_str(&devnet_config.faucet_btc_address)
            .map_err(|e| format!("unable to create faucet address: {e:?}"))?;

        let bitcoin_node_url = format!(
            "http://{}/",
            self.services_map_hosts.as_ref().unwrap().bitcoin_node_host
        );

        let btc_rpc = BitcoinRpcClient::new(
            bitcoin_node_url,
            devnet_config.bitcoin_node_username.clone(),
            devnet_config.bitcoin_node_password.clone(),
        );

        let _ = devnet_event_tx.send(DevnetEvent::info("Configuring bitcoin-node".to_string()));

        const MAX_ERRORS: u32 = 30;

        // Wait for the bitcoin node to be responsive
        btc_rpc
            .call_with_retry("getnetworkinfo", json!([]), MAX_ERRORS, devnet_event_tx)
            .await?;

        // Only generate blocks if we're NOT using cached data
        if no_snapshot {
            let _ = devnet_event_tx.send(DevnetEvent::info(
                "Initializing blockchain with fresh blocks".to_string(),
            ));
            btc_rpc
                .call_with_retry(
                    "generatetoaddress",
                    json!([3, miner_address]),
                    MAX_ERRORS,
                    devnet_event_tx,
                )
                .await?;
            btc_rpc
                .call_with_retry(
                    "generatetoaddress",
                    json!([97, faucet_address]),
                    MAX_ERRORS,
                    devnet_event_tx,
                )
                .await?;
            btc_rpc
                .call_with_retry(
                    "generatetoaddress",
                    json!([1, miner_address]),
                    MAX_ERRORS,
                    devnet_event_tx,
                )
                .await?;
        } else {
            let _ = devnet_event_tx.send(DevnetEvent::info(
                "Using snapshot - skipping initial address seeding".to_string(),
            ));
        }

        let mut error_count = 0;
        loop {
            let rpc_load_call = btc_rpc
                .call("loadwallet", json!([&devnet_config.miner_wallet_name]))
                .await
                .map_err(|e| format!("unable to send 'loadwallet' request ({e})"));

            let rpc_create_call = btc_rpc
                .call(
                    "createwallet",
                    json!({ "wallet_name": devnet_config.miner_wallet_name, "disable_private_keys": true }),
                )
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
                                if error_count > MAX_ERRORS {
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
                    if error_count > MAX_ERRORS {
                        return Err(e);
                    } else if error_count > 1 {
                        let _ = devnet_event_tx.send(DevnetEvent::error(e));
                    }
                }
            }
            tokio::time::sleep(Duration::from_millis(1000)).await;
            let _ = devnet_event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
        }

        // Register descriptors for miner, faucet, and all accounts
        register_descriptor(
            &miner_address.assume_checked_ref().to_string(),
            &btc_rpc,
            MAX_ERRORS,
            devnet_event_tx,
        )
        .await?;
        register_descriptor(
            &faucet_address.assume_checked_ref().to_string(),
            &btc_rpc,
            MAX_ERRORS,
            devnet_event_tx,
        )
        .await?;
        // Index devnet's wallets by default
        for account in accounts.values() {
            let address = Address::from_str(&account.btc_address)
                .map_err(|e| format!("unable to create address: {e:?}"))?;
            register_descriptor(
                &address.assume_checked_ref().to_string(),
                &btc_rpc,
                MAX_ERRORS,
                devnet_event_tx,
            )
            .await?;
        }

        // bitcoind loads the mempool only after the -reindex import finishes. Not
        // `verificationprogress`: on Bitcoin Core 31 regtest it stays below 1 while the tip is old.
        let mut error_count = 0;
        loop {
            let response: JsonRpcResponse = btc_rpc
                .call("getmempoolinfo", json!([]))
                .await
                .map_err(|e| format!("unable to send 'getmempoolinfo' request ({e})"))?
                .json()
                .await
                .map_err(|e| format!("unable to parse 'getmempoolinfo' result: {e}"))?;

            let MempoolLoadInfo { loaded } = response
                .result()
                .map_err(|e| format!("unable to parse 'getmempoolinfo' result: {e}"))?;

            if loaded {
                let _ = devnet_event_tx.send(DevnetEvent::info("Bitcoin chain loaded".to_string()));
                break;
            }

            error_count += 1;
            if error_count > MAX_ERRORS {
                return Err("Timed out waiting for bitcoind to load the chain".to_string());
            }

            tokio::time::sleep(Duration::from_millis(1000)).await;
            let _ = devnet_event_tx.send(DevnetEvent::info(
                "Waiting for bitcoind to load the chain".to_string(),
            ));
        }

        if !no_snapshot {
            let _ = devnet_event_tx.send(DevnetEvent::info(
                "Using cached blockchain data - mining one block".to_string(),
            ));
            btc_rpc
                .call_with_retry(
                    "generatetoaddress",
                    json!([1, miner_address]),
                    MAX_ERRORS,
                    devnet_event_tx,
                )
                .await?;
        }
        Ok(())
    }

    pub async fn start_container_logs_streaming(&self, ctx: &Context) -> Result<(), String> {
        if !self.save_container_logs {
            return Ok(());
        }

        let docker = self.docker_client.as_ref().ok_or(DOCKER_ERR_MSG)?;
        let devnet_config = self.devnet_config();

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

/// Kills the container if it runs, and removes its anonymous volumes.
const FORCE_REMOVE: RemoveContainerOptions = RemoveContainerOptions {
    v: true,
    force: true,
    link: false,
};

struct WatchTimings {
    poll_interval: Duration,
    /// Also bounds each inspect: bollard's client timeout ends at the response headers
    unreachable_timeout: Duration,
}

const WATCH_TIMINGS: WatchTimings = WatchTimings {
    poll_interval: Duration::from_secs(1),
    unreachable_timeout: Duration::from_secs(30),
};

async fn wait_for_container_exit(
    docker: &Docker,
    name: &str,
    container_id: &str,
    timings: &WatchTimings,
) -> String {
    let mut poll = tokio::time::interval(timings.poll_interval);
    let mut unreachable_since = None;
    loop {
        poll.tick().await;
        let polled_at = Instant::now();
        let inspect = docker.inspect_container(container_id, None::<InspectContainerOptions>);
        let error = match tokio::time::timeout(timings.unreachable_timeout, inspect).await {
            Ok(Ok(container)) => match container.state {
                Some(ContainerState {
                    status: Some(ContainerStateStatusEnum::EXITED | ContainerStateStatusEnum::DEAD),
                    exit_code: Some(code),
                    ..
                }) => return format!("{name} container exited unexpectedly (exit code {code})"),
                _ => {
                    unreachable_since = None;
                    continue;
                }
            },
            Ok(Err(DockerError::DockerResponseServerError {
                status_code: 404, ..
            })) => return format!("{name} container was removed unexpectedly"),
            Ok(Err(e)) => e.to_string(),
            Err(_) => "Docker stopped answering".to_string(),
        };
        if unreachable_since.get_or_insert(polled_at).elapsed() >= timings.unreachable_timeout {
            return format!("unable to watch the {name} container: {error}");
        }
    }
}

fn port_binding(port: u16) -> Option<Vec<PortBinding>> {
    Some(vec![PortBinding {
        host_ip: Some("0.0.0.0".to_string()),
        host_port: Some(port.to_string()),
    }])
}

async fn register_descriptor(
    address_str: &str,
    client: &BitcoinRpcClient,
    max_errors: u32,
    event_tx: &Sender<DevnetEvent>,
) -> Result<(), String> {
    use serde_json::json;

    let descriptor = format!("addr({address_str})");
    let response: JsonRpcResponse = client
        .call("getdescriptorinfo", json!([descriptor]))
        .await
        .map_err(|e| format!("unable to send 'getdescriptorinfo' request ({e})"))?
        .json()
        .await
        .map_err(|e| format!("unable to parse 'getdescriptorinfo' result: {e}"))?;

    let info: GetDescriptorInfoResult = response
        .result()
        .map_err(|e| format!("unable to parse 'getdescriptorinfo' result: {e}"))?;

    let checksum = info
        .checksum
        .ok_or("'getdescriptorinfo' returned no checksum")?;

    let _ = event_tx.send(DevnetEvent::info(format!(
        "Registering {descriptor}#{checksum}"
    )));

    client
        .call_with_retry(
            "importdescriptors",
            json!({
                "requests": [{
                    "desc": format!("{descriptor}#{checksum}"),
                    "timestamp": 0,
                }]
            }),
            max_errors,
            event_tx,
        )
        .await
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

pub fn get_snapshot_cache_root() -> PathBuf {
    let home_dir = dirs::home_dir().expect("Unable to retrieve home dir");
    home_dir.join(".clarinet").join("cache").join("devnet")
}

pub fn get_global_snapshot_dir(devnet_config: &DevnetConfig) -> PathBuf {
    SnapshotImages::from(devnet_config).cache_dir(&get_snapshot_cache_root())
}

pub fn get_project_snapshot_dir(devnet_config: &DevnetConfig) -> std::path::PathBuf {
    PathBuf::from(&devnet_config.working_dir)
        .join("data")
        .join("1")
}

pub fn copy_directory(
    source: &Path,
    destination: &Path,
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
    docker_host: Option<&str>,
) -> Result<(), String> {
    if !source_path.exists() {
        return Ok(()); // No snapshot to copy
    }

    let _ = devnet_event_tx.send(DevnetEvent::info(format!(
        "Copying {service_name} snapshot to container..."
    )));

    // `src/.` copies the contents and creates `dest_path` if it is missing (no bind mount)
    let copy_command = format!(
        "docker cp {}/. {container_id}:{dest_path}",
        source_path.display()
    );
    let output = run_docker_command(&copy_command, docker_host)
        .map_err(|e| format!("Failed to execute docker cp: {e}"))?;

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

#[cfg(test)]
mod tests {
    use clarinet_files::DEFAULT_BITCOIN_NODE_IMAGE;

    use super::*;

    fn test_orchestrator() -> (tempfile::TempDir, DevnetOrchestrator) {
        test_orchestrator_with_devnet_settings("")
    }

    fn test_orchestrator_with_devnet_settings(
        extra_settings: &str,
    ) -> (tempfile::TempDir, DevnetOrchestrator) {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        fs::create_dir_all(root.join("settings")).unwrap();
        fs::write(root.join("Clarinet.toml"), "[project]\nname = \"test\"\n").unwrap();
        let working_dir = root.join("devnet").display().to_string();
        let devnet_settings = format!(
            "[network]\nname = \"devnet\"\n\n[devnet]\nworking_dir = {working_dir:?}\n{extra_settings}"
        );
        fs::write(root.join("settings").join("Devnet.toml"), devnet_settings).unwrap();
        let manifest = ProjectManifest::from_location(&root.join("Clarinet.toml"), false).unwrap();
        let orchestrator = DevnetOrchestrator::new(manifest, false).unwrap();
        (dir, orchestrator)
    }

    #[test]
    fn stacks_api_container_name_follows_the_network_name() {
        let (_root, orchestrator) = test_orchestrator();
        assert_eq!(
            orchestrator.stacks_api_container_name(),
            "stacks-api.test.devnet"
        );

        let (_root, orchestrator) =
            test_orchestrator_with_devnet_settings("network_id = 7\nname = \"alt\"\n");
        assert_eq!(
            orchestrator.stacks_api_container_name(),
            "stacks-api.test.7.alt"
        );
    }

    #[test]
    fn bitcoin_conf_omits_options_removed_in_core_28() {
        let (root, orchestrator) = test_orchestrator();

        orchestrator.prepare_bitcoin_node_config(true).unwrap();
        let conf = fs::read_to_string(root.path().join("devnet").join("conf").join("bitcoin.conf"))
            .unwrap();
        assert!(!conf.contains("rpcserialversion"));
    }

    #[test]
    fn bitcoin_node_runs_through_the_image_entrypoint() {
        let (root, orchestrator) = test_orchestrator();

        let config = orchestrator.prepare_bitcoin_node_config(true).unwrap();
        assert_eq!(config.image.as_deref(), Some(DEFAULT_BITCOIN_NODE_IMAGE));
        // bitcoin/bitcoin drops privileges in its entrypoint, which needs to start as root
        assert_eq!(config.user, None);
        assert_eq!(config.entrypoint, None);
        let cmd = config.cmd.unwrap();
        assert!(cmd.iter().all(|arg| arg.starts_with('-')), "{cmd:?}");
        assert_eq!(cmd[0], "-conf=/etc/bitcoin/bitcoin.conf");
        assert!(cmd.contains(&format!("-datadir={BITCOIND_DATA_DIR}")));
        assert!(!cmd.contains(&"-reindex".to_string()));

        #[cfg(unix)]
        {
            use std::os::unix::fs::MetadataExt;
            let uid = fs::metadata(root.path()).unwrap().uid();
            assert_eq!(config.env.unwrap(), vec![format!("UID={uid}")]);
        }
        #[cfg(not(unix))]
        assert_eq!(config.env.unwrap(), Vec::<String>::new());

        let with_snapshot = orchestrator.prepare_bitcoin_node_config(false).unwrap();
        assert!(with_snapshot.cmd.unwrap().contains(&"-reindex".to_string()));
    }

    #[test]
    fn stacks_node_config_names_the_miner_wallet() {
        let (root, orchestrator) = test_orchestrator();

        let conf_dir = root.path().join("devnet").join("conf");
        fs::create_dir_all(&conf_dir).unwrap();
        orchestrator.prepare_stacks_node_config().unwrap();
        let conf = fs::read_to_string(conf_dir.join("Stacks.toml")).unwrap();
        assert!(conf.contains("wallet_name = \"miner\""));
        // stacks-node 4.0.5 rejects unknown burnchain fields
        assert!(!conf.contains("rpc_ssl"));
    }

    #[test]
    fn serializes_selective_stacks_node_event_observers() {
        let observers = vec![
            StacksNodeEventObserver {
                endpoint: "host.docker.internal:8787".into(),
                events_keys: vec!["burn_blocks".into(), "memtx".into()],
            },
            StacksNodeEventObserver {
                endpoint: "host.docker.internal:8788".into(),
                events_keys: vec![
                    "ST000000000000000000002AMW42H.token::mint".into(),
                    "ST000000000000000000002AMW42H.token.asset".into(),
                ],
            },
        ];

        let serialized = serialize_stacks_node_event_observers(&observers);
        let config = stackslib::config::ConfigFile::from_str(&serialized).unwrap();
        let parsed = config.events_observer.unwrap();

        assert_eq!(parsed.len(), 2);
        for observer in observers {
            let parsed_observer = parsed
                .iter()
                .find(|candidate| candidate.endpoint == observer.endpoint)
                .unwrap();
            assert_eq!(parsed_observer.events_keys, observer.events_keys);
        }
    }

    #[test]
    fn serializes_every_devnet_event_observer() {
        let devnet_config = DevnetConfig {
            stacks_signers_keys: vec![StacksPrivateKey::random(), StacksPrivateKey::random()],
            orchestrator_ingestion_port: 20445,
            stacks_api_events_port: 3700,
            stacks_node_events_observers: vec![StacksNodeEventObserver {
                endpoint: "host.docker.internal:8787".into(),
                events_keys: vec!["burn_blocks".into()],
            }],
            ..Default::default()
        };

        let serialized = serialize_stacks_node_event_observers(&stacks_node_event_observers(
            "devnet",
            &devnet_config,
        ));
        let config = stackslib::config::ConfigFile::from_str(&serialized).unwrap();
        // the node keeps its observers in a set, so compare them in a fixed order
        let mut parsed: Vec<_> = config
            .events_observer
            .unwrap()
            .into_iter()
            .map(|observer| (observer.endpoint, observer.events_keys))
            .collect();
        parsed.sort();

        let signer_keys = vec![
            "stackerdb".to_string(),
            "block_proposal".to_string(),
            "burn_blocks".to_string(),
        ];
        let mut expected = vec![
            (
                "stacks-signer-0.devnet:30000".to_string(),
                signer_keys.clone(),
            ),
            ("stacks-signer-1.devnet:30001".to_string(), signer_keys),
            (
                "host.docker.internal:20445".to_string(),
                vec!["*".to_string()],
            ),
            ("stacks-api.devnet:3700".to_string(), vec!["*".to_string()]),
            (
                "host.docker.internal:8787".to_string(),
                vec!["burn_blocks".to_string()],
            ),
        ];
        expected.sort();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn leaves_out_the_stacks_api_observer_when_the_api_is_disabled() {
        let devnet_config = DevnetConfig {
            disable_stacks_api: true,
            ..Default::default()
        };
        let observers = stacks_node_event_observers("devnet", &devnet_config);
        assert!(observers
            .iter()
            .all(|observer| !observer.endpoint.starts_with("stacks-api.")));
    }

    async fn mock_docker() -> (mockito::ServerGuard, Docker) {
        let server = mockito::Server::new_async().await;
        let docker =
            Docker::connect_with_http(&server.url(), 5, bollard::API_DEFAULT_VERSION).unwrap();
        (server, docker)
    }

    async fn orchestrator_with_mock_docker(
    ) -> (tempfile::TempDir, mockito::ServerGuard, DevnetOrchestrator) {
        let (root, mut orchestrator) = test_orchestrator();
        let (server, docker) = mock_docker().await;
        orchestrator.docker_client = Some(docker);
        (root, server, orchestrator)
    }

    fn mock(server: &mut mockito::ServerGuard, method: &str, path: &str) -> mockito::Mock {
        server
            .mock(method, mockito::Matcher::Regex(format!("{path}$")))
            .with_header("content-type", "application/json")
    }

    /// Sends the headers, then holds the body back, like a frozen daemon.
    fn stalled(mock: mockito::Mock) -> mockito::Mock {
        mock.with_chunked_body(|_| {
            std::thread::sleep(Duration::from_millis(300));
            Ok(())
        })
    }

    fn mock_inspect(server: &mut mockito::ServerGuard, status: usize, body: &str) -> mockito::Mock {
        mock(server, "GET", "/containers/abc/json")
            .with_status(status)
            .with_body(body)
    }

    fn exited(code: i64) -> String {
        format!(r#"{{"State":{{"Status":"exited","Running":false,"ExitCode":{code}}}}}"#)
    }

    const FAST: WatchTimings = WatchTimings {
        poll_interval: Duration::from_millis(10),
        unreachable_timeout: Duration::from_millis(200),
    };

    async fn watch(docker: &Docker, timings: &WatchTimings) -> String {
        wait_for_container_exit(docker, "stacks-node", "abc", timings).await
    }

    #[tokio::test]
    async fn reports_a_node_exit_code() {
        let (mut server, docker) = mock_docker().await;
        mock_inspect(&mut server, 200, &exited(137))
            .create_async()
            .await;

        assert_eq!(
            watch(&docker, &FAST).await,
            "stacks-node container exited unexpectedly (exit code 137)"
        );
    }

    #[tokio::test]
    async fn reports_a_removed_node() {
        let (mut server, docker) = mock_docker().await;
        mock_inspect(&mut server, 404, r#"{"message":"No such container"}"#)
            .create_async()
            .await;

        assert_eq!(
            watch(&docker, &FAST).await,
            "stacks-node container was removed unexpectedly"
        );
    }

    #[tokio::test]
    async fn keeps_watching_a_running_node() {
        let (mut server, docker) = mock_docker().await;
        let inspect = mock_inspect(&mut server, 200, r#"{"State":{"Status":"running"}}"#)
            .expect_at_least(2)
            .create_async()
            .await;

        let watch = tokio::time::timeout(Duration::from_millis(100), watch(&docker, &FAST));
        assert!(watch.await.is_err());
        inspect.assert_async().await;
    }

    #[tokio::test]
    async fn gives_up_once_docker_stays_unreachable() {
        let (mut server, docker) = mock_docker().await;
        let inspect = mock_inspect(&mut server, 500, "{}")
            .expect_at_least(2)
            .create_async()
            .await;

        let started = Instant::now();
        let message = watch(&docker, &FAST).await;
        assert!(message.starts_with("unable to watch the stacks-node container"));
        assert!(started.elapsed() >= FAST.unreachable_timeout);
        inspect.assert_async().await;
    }

    #[tokio::test]
    async fn gives_up_once_docker_stops_answering() {
        let (mut server, docker) = mock_docker().await;
        stalled(mock(&mut server, "GET", "/containers/abc/json"))
            .create_async()
            .await;
        let timings = WatchTimings {
            unreachable_timeout: Duration::from_millis(100),
            ..FAST
        };

        assert_eq!(
            watch(&docker, &timings).await,
            "unable to watch the stacks-node container: Docker stopped answering"
        );
    }

    #[tokio::test]
    async fn fails_when_a_node_exits_before_shutdown() {
        let (_root, mut server, mut orchestrator) = orchestrator_with_mock_docker().await;
        mock_inspect(&mut server, 200, &exited(101))
            .create_async()
            .await;
        orchestrator.stacks_node_container_id = Some("abc".into());
        let (_terminator_tx, mut terminator_rx) = tokio::sync::mpsc::unbounded_channel();

        let result = orchestrator.wait_for_termination(&mut terminator_rx).await;
        assert_eq!(
            result,
            Err("stacks-node container exited unexpectedly (exit code 101)".into())
        );
    }

    #[tokio::test]
    async fn fails_when_a_signer_exits() {
        let (_root, mut server, mut orchestrator) = orchestrator_with_mock_docker().await;
        mock_inspect(&mut server, 200, &exited(1))
            .create_async()
            .await;
        orchestrator.stacks_signers_containers_ids = vec!["abc".into()];

        assert_eq!(
            orchestrator.wait_for_node_exit().await,
            Some("stacks-signer-0 container exited unexpectedly (exit code 1)".into())
        );
    }

    /// Creating bitcoin-node fails because a container of this project, or of another one,
    /// holds its name.
    async fn assert_leftover_bitcoin_node_removed(same_project: bool) {
        let (_root, mut server, mut orchestrator) = orchestrator_with_mock_docker().await;
        let project = if same_project {
            orchestrator.network_name.clone()
        } else {
            "other.devnet".to_string()
        };
        let name = format!("bitcoin-node.{}", orchestrator.network_name);
        mock(&mut server, "POST", "/images/create")
            .match_query(mockito::Matcher::Any)
            .create_async()
            .await;
        mock(&mut server, "POST", "/containers/create")
            .match_query(mockito::Matcher::Any)
            .with_status(409)
            .with_body(r#"{"message":"Conflict"}"#)
            .create_async()
            .await;
        mock(&mut server, "GET", &format!("/containers/{name}/json"))
            .with_body(format!(
                r#"{{"Config":{{"Labels":{{"project":"{project}"}}}}}}"#
            ))
            .create_async()
            .await;
        let removed = mock(&mut server, "DELETE", &format!("/containers/{name}"))
            .match_query(mockito::Matcher::Any)
            .with_status(204)
            .expect(usize::from(same_project))
            .create_async()
            .await;

        let result = orchestrator
            .prepare_bitcoin_node_container(&Context::empty(), true)
            .await;
        assert!(result.is_err());
        removed.assert_async().await;
    }

    #[tokio::test]
    async fn replaces_a_leftover_bitcoin_node_of_this_project() {
        assert_leftover_bitcoin_node_removed(true).await;
    }

    #[tokio::test]
    async fn keeps_a_bitcoin_node_of_another_project() {
        assert_leftover_bitcoin_node_removed(false).await;
    }

    async fn start_with_mocked_docker(shutdown_while_booting: bool) -> Result<(), String> {
        let (_root, mut server, mut orchestrator) = orchestrator_with_mock_docker().await;
        orchestrator.bitcoin_node_container_id = Some("btc".into());
        orchestrator.docker_network_id = Some("net".into());
        let removed_with_volumes = mock(&mut server, "DELETE", "/containers/btc")
            .match_query(mockito::Matcher::AllOf(vec![
                mockito::Matcher::UrlEncoded("force".into(), "true".into()),
                mockito::Matcher::UrlEncoded("v".into(), "true".into()),
            ]))
            .with_status(204)
            .expect(1);
        let mut teardown = vec![removed_with_volumes.create_async().await];
        // a newer session of this project shares the network name and the labels
        let calls = [
            ("DELETE", "/networks/net".to_string(), 1),
            ("POST", "/containers/prune".to_string(), 0),
            ("POST", "/networks/prune".to_string(), 0),
            (
                "DELETE",
                format!("/networks/{}", orchestrator.network_name),
                0,
            ),
        ];
        for (method, path, expected) in calls {
            let mock = mock(&mut server, method, &path)
                .with_status(204)
                .expect(expected);
            teardown.push(mock.create_async().await);
        }
        let (event_tx, _event_rx) = std::sync::mpsc::channel();
        let (terminator_tx, terminator_rx) = tokio::sync::mpsc::unbounded_channel();
        if shutdown_while_booting {
            // the shutdown arrives while boot is pulling the bitcoin-node image
            let (pulling_tx, mut pulling_rx) = tokio::sync::mpsc::unbounded_channel();
            let pull = mock(&mut server, "POST", "/images/create")
                .match_query(mockito::Matcher::Any)
                .with_chunked_body(move |_| {
                    let _ = pulling_tx.send(());
                    std::thread::sleep(Duration::from_millis(300));
                    Ok(())
                })
                .expect(1);
            teardown.push(pull.create_async().await);
            tokio::spawn(async move {
                pulling_rx.recv().await;
                terminator_tx.send(()).unwrap();
            });
        }

        let result = orchestrator
            .start(event_tx, terminator_rx, &Context::empty(), true, false)
            .await;
        for mock in teardown {
            mock.assert_async().await;
        }
        result
    }

    #[tokio::test]
    async fn removes_only_its_own_resources_when_start_fails() {
        // the image pull is not mocked, so the first boot step fails
        let result = start_with_mocked_docker(false).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn removes_its_resources_on_a_shutdown_requested_while_booting() {
        let result = start_with_mocked_docker(true).await;
        assert_eq!(result, Ok(()));
    }
}
