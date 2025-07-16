use std::collections::HashMap;
use std::fs::{self, File};
use std::io::prelude::*;
use std::io::{self};
use std::{env, process};

use clap::{CommandFactory, Parser, Subcommand};
use clap_complete::{Generator, Shell};
use clarinet_deployments::diagnostic_digest::DiagnosticsDigest;
use clarinet_deployments::onchain::{
    apply_on_chain_deployment, get_initial_transactions_trackers, update_deployment_costs,
    DeploymentCommand, DeploymentEvent,
};
use clarinet_deployments::types::{DeploymentGenerationArtifacts, DeploymentSpecification};
use clarinet_deployments::{
    get_default_deployment_path, load_deployment, setup_session_with_deployment,
};
use clarinet_files::clarinetrc::ClarinetRC;
use clarinet_files::devnet_diff::DevnetDiffConfig;
use clarinet_files::{
    get_manifest_location, FileLocation, NetworkManifest, ProjectManifest, ProjectManifestFile,
    RequirementConfig, StacksNetwork,
};
use clarinet_format::formatter::{self, ClarityFormatter};
use clarity_repl::analysis::call_checker::ContractAnalysis;
use clarity_repl::clarity::vm::analysis::AnalysisDatabase;
use clarity_repl::clarity::vm::costs::LimitedCostTracker;
use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
use clarity_repl::clarity::ClarityVersion;
use clarity_repl::frontend::Terminal;
use clarity_repl::repl::diagnostic::output_diagnostic;
use clarity_repl::repl::settings::{ApiUrl, RemoteDataSettings};
use clarity_repl::repl::{ClarityCodeSource, ClarityContract, ContractDeployer, DEFAULT_EPOCH};
use clarity_repl::{analysis, repl};
use stacks_network::{self, DevnetOrchestrator};
use toml;

#[cfg(feature = "telemetry")]
use super::telemetry::{telemetry_report_event, DeveloperUsageDigest, DeveloperUsageEvent};
use crate::deployments::types::DeploymentSynthesis;
use crate::deployments::{
    self, check_deployments, generate_default_deployment, get_absolute_deployment_path,
    write_deployment,
};
use crate::devnet::package::{self as Package, ConfigurationPackage};
use crate::devnet::start::{start, StartConfig};
use crate::generate::changes::{Changes, TOMLEdition};
use crate::generate::{self};
use crate::lsp::run_lsp;
/// Clarinet is a command line tool for Clarity smart contract development.
///
/// For Clarinet documentation, refer to https://docs.hiro.so/clarinet/introduction.
/// Report any issues here https://github.com/hirosystems/clarinet/issues/new.
#[derive(Parser, PartialEq, Clone, Debug)]
#[clap(version = env!("CARGO_PKG_VERSION"), name = "clarinet", bin_name = "clarinet")]
struct Opts {
    #[clap(subcommand)]
    command: Command,
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Subcommand, PartialEq, Clone, Debug)]
enum Command {
    /// Generate shell completions scripts
    #[clap(name = "completions", bin_name = "completions", aliases = &["completion"])]
    Completions(Completions),
    /// Create and scaffold a new project
    #[clap(name = "new", bin_name = "new")]
    New(GenerateProject),
    /// Subcommands for working with contracts
    #[clap(subcommand, name = "contracts", aliases = &["contract"])]
    Contracts(Contracts),
    /// Interact with contracts deployed on Mainnet
    #[clap(subcommand, name = "requirements", aliases = &["requirement"])]
    Requirements(Requirements),
    /// Subcommands for working with chainhooks (deprecated)
    #[clap(name = "chainhooks", aliases = &["chainhook"])]
    Chainhooks,
    /// Manage contracts deployments on Simnet/Devnet/Testnet/Mainnet
    #[clap(subcommand, name = "deployments", aliases = &["deployment"])]
    Deployments(Deployments),
    /// Load contracts in a REPL for an interactive session
    #[clap(name = "console", aliases = &["poke"], bin_name = "console")]
    Console(Console),
    /// Check contracts syntax
    #[clap(name = "check", bin_name = "check")]
    Check(Check),
    /// Start a local Devnet network for interacting with your contracts from your browser
    #[clap(name = "integrate", bin_name = "integrate")]
    Integrate(DevnetStart),
    /// Subcommands for Devnet usage
    #[clap(subcommand, name = "devnet")]
    Devnet(Devnet),
    /// Subcommands ts-to-clar transpiler usage
    #[clap(name = "build", aliases = &["transpile"], bin_name = "build")]
    Build(Build),
    /// Get Clarity autocompletion and inline errors from your code editor (VSCode, vim, emacs, etc)
    #[clap(name = "lsp", bin_name = "lsp")]
    LSP,
    /// Format clarity code files
    #[clap(name = "format", aliases = &["fmt"], bin_name = "format")]
    Formatter(Formatter),
    /// Step by step debugging and breakpoints from your code editor (VSCode, vim, emacs, etc)
    #[clap(name = "dap", bin_name = "dap")]
    DAP,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct Build {
    #[clap(long = "path", short = 'p')]
    pub path: Option<String>,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct Formatter {
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,
    /// If specified, format only this file
    #[clap(long = "file", short = 'f')]
    pub file: Option<String>,
    #[clap(long = "max-line-length", short = 'l')]
    pub max_line_length: Option<usize>,
    #[clap(long = "indent", short = 'i', conflicts_with = "use_tabs")]
    /// indentation size, e.g. 2
    pub indentation: Option<usize>,
    #[clap(long = "tabs", short = 't', conflicts_with = "indentation", action = clap::ArgAction::SetTrue)]
    /// use tabs instead of spaces
    pub use_tabs: bool,
    #[clap(long = "dry-run", conflicts_with = "in_place")]
    /// Only echo the result of formatting
    pub dry_run: bool,
    #[clap(long = "in-place", conflicts_with = "dry_run")]
    /// Replace the contents of a file with the formatted code
    pub in_place: bool,
    #[clap(long = "stdin", conflicts_with_all = ["in_place", "file"])]
    /// Read from stdin (and output to stdout)
    pub stdin: bool,
    #[clap(long = "check", conflicts_with_all = ["in_place", "dry_run", "stdin"])]
    /// Check if files are properly formatted without modifying them
    pub check: bool,
}

impl Formatter {
    fn get_input_sources(&self) -> Vec<FormatterInputSource> {
        if self.stdin {
            vec![FormatterInputSource::Stdin]
        } else if let Some(file) = &self.file {
            vec![FormatterInputSource::File(file.clone())]
        } else {
            // look for files at the default code path (./contracts/) if
            // cmd.manifest_path is not specified OR if cmd.file is not specified
            let manifest = load_manifest_or_exit(self.manifest_path.clone(), true);
            let contracts = manifest.contracts.values().cloned();
            contracts
                .map(|c| from_code_source(c.code_source))
                .map(FormatterInputSource::File)
                .collect()
        }
    }
}

enum FormatterInputSource {
    File(String),
    Stdin,
}

impl FormatterInputSource {
    fn get_input(&self) -> String {
        match self {
            FormatterInputSource::File(path) => {
                fs::read_to_string(path).unwrap_or_else(|e| panic!("Failed to read file: {e}"))
            }
            FormatterInputSource::Stdin => io::stdin()
                .lines()
                .map_while(Result::ok)
                .collect::<Vec<_>>()
                .join("\n"),
        }
    }

    fn file_path(&self) -> Option<&str> {
        match self {
            FormatterInputSource::File(path) => Some(path),
            FormatterInputSource::Stdin => None,
        }
    }

    fn is_stdin(&self) -> bool {
        matches!(self, FormatterInputSource::Stdin)
    }
}

#[derive(Subcommand, PartialEq, Clone, Debug)]
enum Devnet {
    /// Generate package of all required devnet artifacts
    #[clap(name = "package", bin_name = "package")]
    Package(DevnetPackage),

    /// Start a local Devnet network for interacting with your contracts from your browser
    #[clap(name = "start", bin_name = "start")]
    DevnetStart(DevnetStart),
}

#[derive(Subcommand, PartialEq, Clone, Debug)]
enum Contracts {
    /// Generate files and settings for a new contract
    #[clap(name = "new", bin_name = "new")]
    NewContract(NewContract),
    /// Remove files and settings for a contract
    #[clap(name = "rm", bin_name = "rm")]
    RemoveContract(RemoveContract),
}

#[derive(Subcommand, PartialEq, Clone, Debug)]
enum Requirements {
    /// Interact with contracts published on Mainnet
    #[clap(name = "add", bin_name = "add")]
    AddRequirement(AddRequirement),
}

#[allow(clippy::enum_variant_names)]
#[derive(Subcommand, PartialEq, Clone, Debug)]
enum Deployments {
    /// Check deployments format
    #[clap(name = "check", bin_name = "check")]
    CheckDeployments(CheckDeployments),
    /// Generate new deployment
    #[clap(name = "generate", bin_name = "generate", aliases = &["new"])]
    GenerateDeployment(GenerateDeployment),
    /// Apply deployment
    #[clap(name = "apply", bin_name = "apply")]
    ApplyDeployment(ApplyDeployment),
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct DevnetPackage {
    /// Output json file name
    #[clap(long = "name", short = 'n')]
    pub package_file_name: Option<String>,
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct GenerateProject {
    /// Project's name
    pub name: String,
    /// Do not provide developer usage telemetry for this project
    #[clap(long = "disable-telemetry")]
    pub disable_telemetry: bool,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct NewContract {
    /// Contract's name
    pub name: String,
    /// Path to Clarinet.toml
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct RemoveContract {
    /// Contract's name
    pub name: String,
    /// Path to Clarinet.toml
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct AddRequirement {
    /// Contract id (ex. "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait")
    pub contract_id: String,
    /// Path to Clarinet.toml
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct CheckDeployments {
    /// Path to Clarinet.toml
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct GenerateDeployment {
    /// Generate a deployment file for simnet environments (console, tests)
    #[clap(
        long = "simnet",
        conflicts_with = "devnet",
        conflicts_with = "testnet",
        conflicts_with = "mainnet"
    )]
    pub simnet: bool,
    /// Generate a deployment file for devnet, using settings/Devnet.toml
    #[clap(
        long = "devnet",
        conflicts_with = "simnet",
        conflicts_with = "testnet",
        conflicts_with = "mainnet"
    )]
    pub devnet: bool,
    /// Generate a deployment file for devnet, using settings/Testnet.toml
    #[clap(
        long = "testnet",
        conflicts_with = "simnet",
        conflicts_with = "devnet",
        conflicts_with = "mainnet"
    )]
    pub testnet: bool,
    /// Generate a deployment file for devnet, using settings/Mainnet.toml
    #[clap(
        long = "mainnet",
        conflicts_with = "simnet",
        conflicts_with = "testnet",
        conflicts_with = "devnet"
    )]
    pub mainnet: bool,
    /// Path to Clarinet.toml
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,
    /// Generate a deployment file without trying to batch transactions (simnet only)
    #[clap(
        long = "no-batch",
        conflicts_with = "devnet",
        conflicts_with = "testnet",
        conflicts_with = "mainnet"
    )]
    pub no_batch: bool,
    /// Compute and set cost, using low priority (network connection required)
    #[clap(
        long = "low-cost",
        conflicts_with = "medium_cost",
        conflicts_with = "high_cost",
        conflicts_with = "manual_cost"
    )]
    pub low_cost: bool,
    /// Compute and set cost, using medium priority (network connection required)
    #[clap(
        conflicts_with = "low_cost",
        long = "medium-cost",
        conflicts_with = "high_cost",
        conflicts_with = "manual_cost"
    )]
    pub medium_cost: bool,
    /// Compute and set cost, using high priority (network connection required)
    #[clap(
        conflicts_with = "low_cost",
        conflicts_with = "medium_cost",
        long = "high-cost",
        conflicts_with = "manual_cost"
    )]
    pub high_cost: bool,
    /// Leave cost estimation manual
    #[clap(
        conflicts_with = "low_cost",
        conflicts_with = "medium_cost",
        conflicts_with = "high_cost",
        long = "manual-cost"
    )]
    pub manual_cost: bool,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct ApplyDeployment {
    /// Apply default deployment settings/default.devnet-plan.toml
    #[clap(
        long = "devnet",
        conflicts_with = "deployment_plan_path",
        conflicts_with = "testnet",
        conflicts_with = "mainnet"
    )]
    pub devnet: bool,
    /// Apply default deployment settings/default.testnet-plan.toml
    #[clap(
        long = "testnet",
        conflicts_with = "deployment_plan_path",
        conflicts_with = "devnet",
        conflicts_with = "mainnet"
    )]
    pub testnet: bool,
    /// Apply default deployment settings/default.mainnet-plan.toml
    #[clap(
        long = "mainnet",
        conflicts_with = "deployment_plan_path",
        conflicts_with = "testnet",
        conflicts_with = "devnet"
    )]
    pub mainnet: bool,
    /// Path to Clarinet.toml
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,
    /// Apply deployment plan specified
    #[clap(
        long = "deployment-plan-path",
        short = 'p',
        conflicts_with = "devnet",
        conflicts_with = "testnet",
        conflicts_with = "mainnet"
    )]
    pub deployment_plan_path: Option<String>,
    /// Display streams of logs instead of terminal UI dashboard
    #[clap(long = "no-dashboard")]
    pub no_dashboard: bool,
    /// Use on disk deployment plan (prevent updates computing)
    #[clap(
        long = "use-on-disk-deployment-plan",
        short = 'd',
        conflicts_with = "use_computed_deployment_plan"
    )]
    pub use_on_disk_deployment_plan: bool,
    /// Use computed deployment plan (will overwrite on disk version if any update)
    #[clap(
        long = "use-computed-deployment-plan",
        short = 'c',
        conflicts_with = "use_on_disk_deployment_plan"
    )]
    pub use_computed_deployment_plan: bool,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct Console {
    /// Path to Clarinet.toml
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,

    /// If specified, use this deployment file
    #[clap(long = "deployment-plan-path", short = 'p')]
    pub deployment_plan_path: Option<String>,
    /// Use on disk deployment plan (prevent updates computing)
    #[clap(
        long = "use-on-disk-deployment-plan",
        short = 'd',
        conflicts_with = "use_computed_deployment_plan"
    )]
    pub use_on_disk_deployment_plan: bool,
    /// Use computed deployment plan (will overwrite on disk version if any update)
    #[clap(
        long = "use-computed-deployment-plan",
        short = 'c',
        conflicts_with = "use_on_disk_deployment_plan"
    )]
    pub use_computed_deployment_plan: bool,

    /// Enable remote data fetching from mainnet or a testnet
    #[clap(long = "enable-remote-data", short = 'r')]
    pub enable_remote_data: bool,
    /// Set a custom Stacks Blockchain API URL for remote data fetching
    #[clap(long = "remote-data-api-url", short = 'a')]
    pub remote_data_api_url: Option<ApiUrl>,
    /// Initial remote Stacks block height
    #[clap(long = "remote-data-initial-height", short = 'b')]
    pub remote_data_initial_height: Option<u32>,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct DevnetStart {
    /// Path to Clarinet.toml
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,
    /// Display streams of logs instead of terminal UI dashboard
    #[clap(long = "no-dashboard")]
    pub no_dashboard: bool,
    /// Start from genesis rather than using snapshot
    #[clap(long = "from-genesis")]
    pub no_snapshot: bool,
    /// Save container logs locally
    #[clap(long = "save-container-logs")]
    pub save_container_logs: bool,
    /// Create a new global snapshot when reaching epoch 3.0
    #[clap(long = "create-new-snapshot")]
    pub create_new_snapshot: bool,
    /// If specified, use this deployment file
    #[clap(long = "deployment-plan-path", short = 'p')]
    pub deployment_plan_path: Option<String>,
    /// Use on disk deployment plan (prevent updates computing)
    #[clap(
        long = "use-on-disk-deployment-plan",
        short = 'd',
        conflicts_with = "use_computed_deployment_plan"
    )]
    pub use_on_disk_deployment_plan: bool,
    /// Use computed deployment plan (will overwrite on disk version if any update)
    #[clap(
        long = "use-computed-deployment-plan",
        short = 'c',
        conflicts_with = "use_on_disk_deployment_plan"
    )]
    pub use_computed_deployment_plan: bool,
    /// Path to Package.json produced by 'clarinet devnet package'
    #[clap(
        long = "package",
        conflicts_with = "use_computed_deployment_plan",
        conflicts_with = "manifest_path"
    )]
    pub package: Option<String>,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct Check {
    /// Path to Clarinet.toml
    #[clap(long = "manifest-path", short = 'm')]
    pub manifest_path: Option<String>,
    /// If specified, perform a simple syntax-check on just this one file
    pub file: Option<String>,
    /// If specified, use this deployment file
    #[clap(long = "deployment-plan-path", short = 'p')]
    pub deployment_plan_path: Option<String>,
    /// Use on disk deployment plan (prevent updates computing)
    #[clap(
        long = "use-on-disk-deployment-plan",
        short = 'd',
        conflicts_with = "use_computed_deployment_plan"
    )]
    pub use_on_disk_deployment_plan: bool,
    /// Use computed deployment plan (will overwrite on disk version if any update)
    #[clap(
        long = "use-computed-deployment-plan",
        short = 'c',
        conflicts_with = "use_on_disk_deployment_plan"
    )]
    pub use_computed_deployment_plan: bool,
}

#[derive(Parser, PartialEq, Clone, Debug)]
struct Completions {
    /// Specify which shell to generation completions script for
    #[clap(ignore_case = true)]
    pub shell: Shell,
}

pub fn main() {
    let opts: Opts = match Opts::try_parse() {
        Ok(opts) => opts,
        Err(e) => {
            if e.kind() == clap::error::ErrorKind::UnknownArgument {
                let manifest = load_manifest_or_exit(None, false);
                if manifest.project.telemetry {
                    #[cfg(feature = "telemetry")]
                    telemetry_report_event(DeveloperUsageEvent::UnknownCommand(
                        DeveloperUsageDigest::new(
                            &manifest.project.name,
                            &manifest.project.authors,
                        ),
                        e.to_string(),
                    ));
                }
            }

            // handle --version, --help, etc
            // see how to safely print clap errors here:
            // https://github.com/clap-rs/clap/blob/21b671f689bc0b8d790dc8c42902c22822bf6f82/clap_builder/src/error/mod.rs#L233
            let _ = e.print();
            let _ = std::io::stdout().lock().flush();
            let _ = std::io::stderr().lock().flush();
            std::process::exit(e.exit_code());
        }
    };

    let clarinetrc = ClarinetRC::from_rc_file();

    match opts.command {
        Command::Completions(cmd) => {
            let mut app = Opts::command();
            let file_name = cmd.shell.file_name("clarinet");
            let mut file = match File::create(file_name.clone()) {
                Ok(file) => file,
                Err(e) => {
                    eprintln!(
                        "{} Unable to create file {}: {}",
                        red!("error:"),
                        file_name,
                        e
                    );
                    std::process::exit(1);
                }
            };
            clap_complete::generate(cmd.shell, &mut app, "clarinet", &mut file);
            println!("{} {}", green!("Created file"), file_name);
            println!("Check your shell's documentation for details about using this file to enable completions for clarinet");
        }
        Command::New(project_opts) => {
            let current_path = std::env::current_dir().unwrap_or_else(|e| {
                eprintln!("{}{}", format_err!("unable to get current directory"), e);
                std::process::exit(1);
            });
            let current_dir_name = current_path.file_name().map(|s| s.to_string_lossy());
            let current_path = current_path.to_str().expect("Invalid path").to_owned();
            let use_current_dir = project_opts.name == ".";

            let (relative_dir, project_id) = if use_current_dir {
                if let Ok(entries) = std::fs::read_dir(&current_path) {
                    let is_empty = entries.count() == 0;
                    if !is_empty {
                        println!("{}", yellow!("Current directory is not empty"));
                        prompt_user_to_continue();
                    }
                };
                (
                    ".",
                    current_dir_name
                        .unwrap_or(std::borrow::Cow::Borrowed("project"))
                        .to_string(),
                )
            } else {
                if std::fs::read_dir(&project_opts.name).is_ok() {
                    println!("{}", yellow!("Directory already exists"));
                    prompt_user_to_continue();
                }
                let mut name_and_dir = project_opts.name.rsplitn(2, '/');
                let project_id = name_and_dir.next().unwrap();
                let relative_dir = name_and_dir.next().unwrap_or(".");
                (relative_dir, sanitize_project_name(project_id))
            };

            let project_path = if relative_dir == "." {
                current_path
            } else {
                format!("{current_path}/{relative_dir}")
            };

            let telemetry_enabled = if cfg!(feature = "telemetry") {
                if project_opts.disable_telemetry {
                    false
                } else {
                    match clarinetrc.enable_telemetry {
                        Some(enable) => enable,
                        _ => {
                            println!("{}", yellow!("Send usage data to Hiro."));
                            println!("{}", yellow!("Help Hiro improve its products and services by automatically sending diagnostics and usage data."));
                            println!("{}", yellow!("Only high level usage information, and no information identifying you or your project are collected."));
                            println!("{}",
                                yellow!("Enable or disable clarinet telemetry globally with this command:")
                            );
                            println!(
                                "{}",
                                blue!(
                                    "  $ mkdir -p ~/.clarinet; echo \"enable_telemetry = true\" >> {}",
                                    ClarinetRC::get_settings_file_path()
                                )
                            );
                            // TODO(lgalabru): once we have a privacy policy available, add a link
                            // println!("{}", yellow!("Visit http://hiro.so/clarinet-privacy for details."));
                            println!("{}", yellow!("Enable [Y/n]?"));
                            let mut buffer = String::new();
                            std::io::stdin().read_line(&mut buffer).unwrap();
                            !buffer.starts_with('n')
                        }
                    }
                }
            } else {
                false
            };
            if telemetry_enabled {
                println!(
                    "{}",
                    yellow!("Telemetry enabled. Thanks for helping to improve clarinet!")
                );
            } else {
                println!(
                    "{}",
                    yellow!(
                        "Telemetry disabled. Clarinet will not collect any data on this project."
                    )
                );
            }

            let changes = match generate::get_changes_for_new_project(
                project_path,
                project_id,
                use_current_dir,
                telemetry_enabled,
            ) {
                Ok(changes) => changes,
                Err(message) => {
                    eprintln!("{}", format_err!(message));
                    std::process::exit(1);
                }
            };

            if !execute_changes(changes) {
                std::process::exit(1);
            }
            if clarinetrc.enable_hints.unwrap_or(true) {
                display_contract_new_hint(Some(project_opts.name.as_str()));
            }
            if telemetry_enabled {
                #[cfg(feature = "telemetry")]
                telemetry_report_event(DeveloperUsageEvent::NewProject(DeveloperUsageDigest::new(
                    &project_opts.name,
                    &[],
                )));
            }
        }
        Command::Deployments(subcommand) => match subcommand {
            Deployments::CheckDeployments(cmd) => {
                let manifest = load_manifest_or_exit(cmd.manifest_path, true);
                // Ensure that all the deployments can correctly be deserialized.
                println!("Checking deployments");
                let res = check_deployments(&manifest);
                if let Err(message) = res {
                    eprintln!("{}", format_err!(message));
                    process::exit(1);
                }
            }
            Deployments::GenerateDeployment(cmd) => {
                let manifest = load_manifest_or_exit(cmd.manifest_path, true);

                let network = if cmd.devnet {
                    StacksNetwork::Devnet
                } else if cmd.testnet {
                    StacksNetwork::Testnet
                } else if cmd.mainnet {
                    StacksNetwork::Mainnet
                } else {
                    StacksNetwork::Simnet
                };

                let default_deployment_path =
                    get_default_deployment_path(&manifest, &network).unwrap();
                let (mut deployment, _) =
                    match generate_default_deployment(&manifest, &network, cmd.no_batch) {
                        Ok(deployment) => deployment,
                        Err(message) => {
                            eprintln!("{}", format_err!(message));
                            std::process::exit(1);
                        }
                    };

                if !cmd.manual_cost
                    && matches!(network, StacksNetwork::Testnet | StacksNetwork::Mainnet)
                {
                    let priority = match (cmd.low_cost, cmd.medium_cost, cmd.high_cost) {
                        (_, _, true) => 2,
                        (_, true, _) => 1,
                        (true, _, _) => 0,
                        (false, false, false) => {
                            eprintln!("{}", format_err!("cost strategy not specified (--low-cost, --medium-cost, --high-cost, --manual-cost)"));
                            std::process::exit(1);
                        }
                    };
                    match update_deployment_costs(&mut deployment, priority) {
                        Ok(_) => {}
                        Err(message) => {
                            eprintln!(
                                "{} unable to update costs\n{}",
                                yellow!("warning:"),
                                message
                            );
                        }
                    };
                }

                let write_plan = if default_deployment_path.exists() {
                    let existing_deployment = load_deployment(&manifest, &default_deployment_path)
                        .unwrap_or_else(|message| {
                            eprintln!(
                                "{}",
                                format_err!(format!(
                                    "unable to load {default_deployment_path}\n{message}",
                                ))
                            );
                            process::exit(1);
                        });
                    should_existing_plan_be_replaced(&existing_deployment, &deployment)
                } else {
                    true
                };

                if write_plan {
                    let res = write_deployment(&deployment, &default_deployment_path, false);
                    if let Err(message) = res {
                        eprintln!("{}", format_err!(message));
                        process::exit(1);
                    }

                    println!(
                        "{} {}",
                        green!("Generated file"),
                        default_deployment_path.get_relative_location().unwrap()
                    );
                }
            }
            Deployments::ApplyDeployment(cmd) => {
                let manifest = load_manifest_or_exit(cmd.manifest_path, true);

                let network = if cmd.devnet {
                    Some(StacksNetwork::Devnet)
                } else if cmd.testnet {
                    Some(StacksNetwork::Testnet)
                } else if cmd.mainnet {
                    Some(StacksNetwork::Mainnet)
                } else {
                    None
                };

                let result = match (&network, cmd.deployment_plan_path) {
                    (None, None) => {
                        Err(format!("{}: a flag `--devnet`, `--testnet`, `--mainnet` or `--deployment-plan-path=path/to/yaml` should be provided.", yellow!("Command usage")))
                    }
                    (Some(network), None) => {
                        let res = load_deployment_if_exists(&manifest, network, cmd.use_on_disk_deployment_plan, cmd.use_computed_deployment_plan);
                        match res {
                            Some(Ok(deployment)) => {
                                println!(
                                    "{} using existing deployments/default.{}-plan.yaml",
                                    yellow!("note:"),
                                    format!("{network:?}").to_lowercase(),
                                );
                                Ok(deployment)
                            }
                            Some(Err(e)) => Err(e),
                            None => {
                                let default_deployment_path = get_default_deployment_path(&manifest, network).unwrap();
                                let (deployment, _) = match generate_default_deployment(&manifest, network, false) {
                                    Ok(deployment) => deployment,
                                    Err(message) => {
                                        eprintln!("{}", red!(message));
                                        std::process::exit(1);
                                    }
                                };
                                let res = write_deployment(&deployment, &default_deployment_path, true);
                                if let Err(message) = res {
                                    Err(message)
                                } else {
                                    println!("{} {}", green!("Generated file"), default_deployment_path.get_relative_location().unwrap());
                                    Ok(deployment)
                                }
                            }
                        }
                    }
                    (None, Some(deployment_plan_path)) => {
                        let deployment_path = get_absolute_deployment_path(&manifest, &deployment_plan_path).expect("unable to retrieve deployment");
                        load_deployment(&manifest, &deployment_path)
                    }
                    (_, _) => unreachable!()
                };

                let deployment = match result {
                    Ok(deployment) => deployment,
                    Err(e) => {
                        eprintln!("{e}");
                        std::process::exit(1);
                    }
                };
                let network = deployment.network.clone();

                let node_url = deployment.stacks_node.clone().unwrap();

                println!(
                    "The following deployment plan will be applied:\n{}\n\n",
                    DeploymentSynthesis::from_deployment(&deployment)
                );

                if !cmd.use_on_disk_deployment_plan {
                    println!("{}", yellow!("Continue [Y/n]?"));
                    let mut buffer = String::new();
                    std::io::stdin().read_line(&mut buffer).unwrap();
                    if !buffer.starts_with('Y')
                        && !buffer.starts_with('y')
                        && !buffer.starts_with('\n')
                    {
                        eprintln!("Deployment aborted");
                        std::process::exit(1);
                    }
                }

                let (command_tx, command_rx) = std::sync::mpsc::channel();
                let (event_tx, event_rx) = std::sync::mpsc::channel();
                let manifest_moved = manifest.clone();

                if manifest.project.telemetry {
                    #[cfg(feature = "telemetry")]
                    telemetry_report_event(DeveloperUsageEvent::ProtocolPublished(
                        DeveloperUsageDigest::new(
                            &manifest.project.name,
                            &manifest.project.authors,
                        ),
                        network.clone(),
                    ));
                }

                let transaction_trackers = if cmd.no_dashboard {
                    vec![]
                } else {
                    get_initial_transactions_trackers(&deployment)
                };
                let network_moved = network.clone();
                std::thread::spawn(move || {
                    let manifest = manifest_moved;
                    let res = NetworkManifest::from_project_manifest_location(
                        &manifest.location,
                        &network_moved.get_networks(),
                        manifest.use_mainnet_wallets(),
                        Some(&manifest.project.cache_location),
                        None,
                    );
                    let network_manifest = match res {
                        Ok(network_manifest) => network_manifest,
                        Err(e) => {
                            let _ = event_tx.send(DeploymentEvent::Interrupted(e));
                            return;
                        }
                    };
                    apply_on_chain_deployment(
                        network_manifest,
                        deployment,
                        event_tx,
                        command_rx,
                        true,
                        None,
                        None,
                    );
                });

                let _ = command_tx.send(DeploymentCommand::Start);

                if cmd.no_dashboard {
                    loop {
                        let cmd = match event_rx.recv() {
                            Ok(cmd) => cmd,
                            Err(_e) => break,
                        };
                        match cmd {
                            DeploymentEvent::Interrupted(message) => {
                                eprintln!(
                                    "{} Error publishing transactions: {}",
                                    red!("x"),
                                    message
                                );
                                break;
                            }
                            DeploymentEvent::TransactionUpdate(update) => {
                                println!("{} {:?} {}", blue!("➡"), update.status, update.name);
                            }
                            DeploymentEvent::DeploymentCompleted => {
                                println!(
                                    "{} Transactions successfully confirmed on {:?}",
                                    green!("✔"),
                                    network
                                );
                                break;
                            }
                        }
                    }
                } else {
                    let res = deployments::start_ui(&node_url, event_rx, transaction_trackers);
                    match res {
                        Ok(()) => println!(
                            "{} Transactions successfully confirmed on {:?}",
                            green!("✔"),
                            network
                        ),
                        Err(message) => {
                            eprintln!("{} Error publishing transactions: {}", red!("x"), message)
                        }
                    }
                }
            }
        },
        Command::Chainhooks => {
            let message = "This command is deprecated. Use the chainhooks library instead (https://github.com/hirosystems/chainhook)";
            eprintln!("{}", format_err!(message));
            std::process::exit(1);
        }
        Command::Contracts(subcommand) => match subcommand {
            Contracts::NewContract(cmd) => {
                let manifest = load_manifest_or_exit(cmd.manifest_path, true);

                let changes = match generate::get_changes_for_new_contract(
                    &manifest.location,
                    cmd.name,
                    None,
                    true,
                ) {
                    Ok(changes) => changes,
                    Err(message) => {
                        eprintln!("{}", format_err!(message));
                        std::process::exit(1);
                    }
                };

                if !execute_changes(changes) {
                    std::process::exit(1);
                }
                if clarinetrc.enable_hints.unwrap_or(true) {
                    display_post_check_hint();
                }
            }
            Contracts::RemoveContract(cmd) => {
                let manifest = load_manifest_or_exit(cmd.manifest_path, true);
                let contract_name = cmd.name.clone();
                let changes =
                    match generate::get_changes_for_rm_contract(&manifest.location, cmd.name) {
                        Ok(changes) => changes,
                        Err(message) => {
                            eprintln!("{}", format_err!(message));
                            std::process::exit(1);
                        }
                    };

                let mut answer = String::new();
                println!(
                    "{} This command will delete the files {}.test.ts, {}.clar, and remove the contract from the manifest. Do you confirm? [y/N]",
                    yellow!("warning:"),
                    &contract_name,
                    &contract_name
                );
                std::io::stdin().read_line(&mut answer).unwrap();
                if !answer.trim().eq_ignore_ascii_case("y") {
                    eprintln!("{} Not deleting contract files", yellow!("warning:"));
                    std::process::exit(0);
                }
                if !execute_changes(changes) {
                    std::process::exit(1);
                }
                if clarinetrc.enable_hints.unwrap_or(true) {
                    display_post_check_hint();
                }
            }
        },
        Command::Requirements(subcommand) => match subcommand {
            Requirements::AddRequirement(cmd) => {
                let manifest = load_manifest_or_exit(cmd.manifest_path, true);

                let change = TOMLEdition {
                    comment: format!(
                        "{} with requirement {}",
                        yellow!("Updated Clarinet.toml"),
                        green!("{}", cmd.contract_id)
                    ),
                    manifest_location: manifest.location,
                    contracts_to_rm: vec![],
                    contracts_to_add: HashMap::new(),
                    requirements_to_add: vec![RequirementConfig {
                        contract_id: cmd.contract_id,
                    }],
                };
                if !execute_changes(vec![Changes::EditTOML(change)]) {
                    std::process::exit(1);
                }
                if clarinetrc.enable_hints.unwrap_or(true) {
                    display_post_check_hint();
                }
            }
        },
        Command::Console(cmd) => {
            // Loop to handle `::reload` command
            loop {
                let manifest = load_manifest_or_warn(cmd.manifest_path.clone());

                let mut terminal = match manifest {
                    Some(ref manifest) => {
                        let (deployment, _, artifacts) = load_deployment_and_artifacts_or_exit(
                            manifest,
                            &cmd.deployment_plan_path,
                            cmd.use_on_disk_deployment_plan,
                            cmd.use_computed_deployment_plan,
                        );

                        if !artifacts.success {
                            let diags_digest =
                                DiagnosticsDigest::new(&artifacts.diags, &deployment);
                            if diags_digest.has_feedbacks() {
                                println!("{}", diags_digest.message);
                            }
                            if diags_digest.errors > 0 {
                                println!(
                                    "{} {} detected",
                                    red!("x"),
                                    pluralize!(diags_digest.errors, "error")
                                );
                            }
                            std::process::exit(1);
                        }

                        Terminal::load(artifacts.session)
                    }
                    None => {
                        let remote_data_settings = RemoteDataSettings {
                            enabled: cmd.enable_remote_data,
                            api_url: cmd.remote_data_api_url.clone().unwrap_or_default(),
                            initial_height: cmd.remote_data_initial_height,
                            use_mainnet_wallets: false,
                        };
                        let mut settings = repl::SessionSettings::default();
                        settings.repl_settings.remote_data = remote_data_settings.clone();
                        Terminal::new(settings)
                    }
                };
                let reload = terminal.start();

                // Report telemetry
                if let Some(manifest) = manifest {
                    if manifest.project.telemetry {
                        #[cfg(feature = "telemetry")]
                        telemetry_report_event(DeveloperUsageEvent::PokeExecuted(
                            DeveloperUsageDigest::new(
                                &manifest.project.name,
                                &manifest.project.authors,
                            ),
                        ));

                        #[cfg(feature = "telemetry")]
                        let mut debug_count = 0;
                        for command in terminal.executed {
                            if command.starts_with("::debug") {
                                debug_count += 1;
                            }
                        }
                        if debug_count > 0 {
                            telemetry_report_event(DeveloperUsageEvent::DebugStarted(
                                DeveloperUsageDigest::new(
                                    &manifest.project.name,
                                    &manifest.project.authors,
                                ),
                                debug_count,
                            ));
                        }
                    }
                }

                if !reload {
                    break;
                }
            }

            if clarinetrc.enable_hints.unwrap_or(true) {
                display_contract_new_hint(None);
            }
        }
        Command::Check(cmd) if cmd.file.is_some() => {
            let file = cmd.file.unwrap();
            let mut settings = repl::SessionSettings::default();
            settings.repl_settings.analysis.enable_all_passes();

            let mut session = repl::Session::new(settings.clone());
            let Ok(code_source) = fs::read_to_string(&file) else {
                eprintln!("{} unable to read file: '{file}'", red!("error:"));
                std::process::exit(1);
            };
            let contract_id = QualifiedContractIdentifier::transient();
            let epoch = DEFAULT_EPOCH;
            let contract = ClarityContract {
                code_source: ClarityCodeSource::ContractInMemory(code_source),
                deployer: ContractDeployer::Transient,
                name: "transient".to_string(),
                clarity_version: ClarityVersion::default_for_epoch(epoch),
                epoch: clarity_repl::repl::Epoch::Specific(epoch),
            };
            let (ast, mut diagnostics, mut success) = session.interpreter.build_ast(&contract);
            let (annotations, mut annotation_diagnostics) = session
                .interpreter
                .collect_annotations(contract.expect_in_memory_code_source());
            diagnostics.append(&mut annotation_diagnostics);

            let mut contract_analysis = ContractAnalysis::new(
                contract_id,
                ast.expressions,
                LimitedCostTracker::new_free(),
                contract.epoch.resolve(),
                contract.clarity_version,
            );
            let mut analysis_db = AnalysisDatabase::new(&mut session.interpreter.clarity_datastore);
            let mut analysis_diagnostics = match analysis::run_analysis(
                &mut contract_analysis,
                &mut analysis_db,
                &annotations,
                &settings.repl_settings.analysis,
            ) {
                Ok(diagnostics) => diagnostics,
                Err(diagnostics) => {
                    success = false;
                    diagnostics
                }
            };
            diagnostics.append(&mut analysis_diagnostics);

            let lines = contract.expect_in_memory_code_source().lines();
            let formatted_lines: Vec<String> = lines.map(|l| l.to_string()).collect();
            for d in diagnostics {
                for line in output_diagnostic(&d, &file, &formatted_lines) {
                    println!("{line}");
                }
            }

            if success {
                println!("{} Syntax of contract successfully checked", green!("✔"))
            } else {
                std::process::exit(1);
            }
        }
        Command::Check(cmd) => {
            let manifest = load_manifest_or_exit(cmd.manifest_path, false);
            let (deployment, _, artifacts) = load_deployment_and_artifacts_or_exit(
                &manifest,
                &cmd.deployment_plan_path,
                cmd.use_on_disk_deployment_plan,
                cmd.use_computed_deployment_plan,
            );

            let diags_digest = DiagnosticsDigest::new(&artifacts.diags, &deployment);
            if diags_digest.has_feedbacks() {
                println!("{}", diags_digest.message);
            }

            if diags_digest.warnings > 0 {
                println!(
                    "{} {} detected",
                    yellow!("!"),
                    pluralize!(diags_digest.warnings, "warning")
                );
            }
            if diags_digest.errors > 0 {
                println!(
                    "{} {} detected",
                    red!("x"),
                    pluralize!(diags_digest.errors, "error")
                );
            } else {
                println!(
                    "{} {} checked",
                    green!("✔"),
                    pluralize!(diags_digest.contracts_checked, "contract"),
                );
            }
            let exit_code = match artifacts.success {
                true => 0,
                false => 1,
            };

            if clarinetrc.enable_hints.unwrap_or(true) {
                display_post_check_hint();
            }
            if manifest.project.telemetry {
                #[cfg(feature = "telemetry")]
                telemetry_report_event(DeveloperUsageEvent::CheckExecuted(
                    DeveloperUsageDigest::new(&manifest.project.name, &manifest.project.authors),
                ));
            }
            std::process::exit(exit_code);
        }
        Command::Integrate(cmd) => {
            eprintln!(
                "{}",
                format_warn!("This command is deprecated. Use 'clarinet devnet start' instead"),
            );
            devnet_start(cmd, clarinetrc)
        }
        Command::LSP => run_lsp(),
        Command::DAP => match super::dap::run_dap() {
            Ok(_) => (),
            Err(e) => {
                eprintln!("{}", red!(e));
                process::exit(1);
            }
        },
        Command::Formatter(cmd) => {
            eprintln!(
                "{}",
                format_warn!("This command is in beta. Feedback is welcome!"),
            );
            let sources = cmd.get_input_sources();
            let mut settings = formatter::Settings::default();

            if let Some(max_line_length) = cmd.max_line_length {
                settings.max_line_length = max_line_length;
            }

            if let Some(indentation) = cmd.indentation {
                settings.indentation = formatter::Indentation::Space(indentation);
            }
            if cmd.use_tabs {
                settings.indentation = formatter::Indentation::Tab;
            }
            let formatter = ClarityFormatter::new(settings);

            let mut all_files_formatted = true;
            let mut unformatted_files = Vec::new();

            for source in sources {
                let input = source.get_input();
                let output = formatter.format(&input);

                if cmd.check {
                    if let Some(file_path) = source.file_path() {
                        if input != output {
                            all_files_formatted = false;
                            unformatted_files.push(file_path.to_string());
                        }
                    }
                } else if cmd.in_place {
                    let file_path = source
                        .file_path()
                        .expect("No file path for in-place formatting");
                    let _ = overwrite_formatted(file_path, &output);
                } else if cmd.dry_run || source.is_stdin() {
                    println!("{output}");
                } else {
                    eprintln!("required flags: in-place, dry-run, or check");
                    std::process::exit(1);
                }
            }

            if cmd.check {
                if all_files_formatted {
                    println!("{} All files are properly formatted", green!("✔"));
                    std::process::exit(0);
                } else {
                    eprintln!(
                        "{} {} {} formatting",
                        red!("✗"),
                        unformatted_files.len(),
                        if unformatted_files.len() == 1 {
                            "file needs"
                        } else {
                            "files need"
                        }
                    );
                    for file in unformatted_files {
                        eprintln!("  {file}");
                    }
                    std::process::exit(1);
                }
            }
        }
        Command::Devnet(subcommand) => match subcommand {
            Devnet::Package(cmd) => {
                let manifest = load_manifest_or_exit(cmd.manifest_path, false);
                if let Err(e) = Package::pack(cmd.package_file_name, manifest) {
                    eprintln!("Could not execute the package command. {}", format_err!(e));
                    process::exit(1);
                }
            }
            Devnet::DevnetStart(cmd) => devnet_start(cmd, clarinetrc),
        },
        Command::Build(Build { path }) => {
            let file_name = path.unwrap_or_else(|| {
                eprintln!("No file path provided. Please specify a .clar.ts file.");
                process::exit(1);
            });
            let file_path = std::path::Path::new(&file_name);
            let extension = file_path.extension().unwrap_or_default();
            assert_eq!(extension, "ts");
            assert!(file_path.is_file());
            assert!(file_name.ends_with(".clar.ts"));

            let output_path = file_name.strip_suffix(".ts").unwrap();

            let src = std::fs::read_to_string(file_path).unwrap();
            let clarity_code = ts_to_clar::transpile(&file_name, &src).unwrap();
            std::fs::write(output_path, clarity_code).unwrap();
            println!("Transpiled {file_name} to {output_path}");
        }
    };
}

fn overwrite_formatted(file_path: &str, output: &str) -> io::Result<()> {
    let mut file = fs::File::create(file_path)?;

    file.write_all(output.as_bytes())?;
    Ok(())
}

fn from_code_source(src: ClarityCodeSource) -> String {
    match src {
        ClarityCodeSource::ContractOnDisk(path_buf) => {
            path_buf.as_path().to_str().unwrap().to_owned()
        }
        _ => panic!("invalid code source"), // TODO
    }
}

fn get_manifest_location_or_exit(path: Option<String>) -> FileLocation {
    get_manifest_location(path).unwrap_or_else(|| {
        eprintln!("Could not find Clarinet.toml");
        process::exit(1);
    })
}

fn get_manifest_location_or_warn(path: Option<String>) -> Option<FileLocation> {
    match get_manifest_location(path) {
        Some(manifest_location) => Some(manifest_location),
        None => {
            eprintln!(
                "{} no manifest found, starting with default settings.",
                yellow!("note:")
            );
            None
        }
    }
}

pub fn load_manifest_or_exit(
    path: Option<String>,
    allow_remote_data_fetching: bool,
) -> ProjectManifest {
    let manifest_location = get_manifest_location_or_exit(path);
    ProjectManifest::from_location(&manifest_location, allow_remote_data_fetching).unwrap_or_else(
        |message| {
            eprintln!("{} syntax errors in Clarinet.toml", red!("error:"));
            eprintln!("{message}");
            process::exit(1);
        },
    )
}

fn load_manifest_or_warn(path: Option<String>) -> Option<ProjectManifest> {
    let manifest_location = get_manifest_location_or_warn(path)?;

    ProjectManifest::from_location(&manifest_location, true)
        .inspect_err(|message| {
            eprintln!("{} syntax errors in Clarinet.toml", red!("error:"));
            eprintln!("{message}");
            process::exit(1);
        })
        .ok()
}

pub fn load_deployment_and_artifacts_or_exit(
    manifest: &ProjectManifest,
    deployment_plan_path: &Option<String>,
    force_on_disk: bool,
    force_computed: bool,
) -> (
    DeploymentSpecification,
    Option<String>,
    DeploymentGenerationArtifacts,
) {
    let result = match deployment_plan_path {
        None => {
            let res = load_deployment_if_exists(
                manifest,
                &StacksNetwork::Simnet,
                force_on_disk,
                force_computed,
            );
            match res {
                Some(Ok(deployment)) => {
                    println!(
                        "{} using deployments/default.simnet-plan.yaml",
                        yellow!("note:")
                    );
                    let artifacts = setup_session_with_deployment(manifest, &deployment, None);
                    Ok((deployment, None, artifacts))
                }
                Some(Err(e)) => Err(format!(
                    "loading deployments/default.simnet-plan.yaml failed with error: {e}"
                )),
                None => {
                    match generate_default_deployment(manifest, &StacksNetwork::Simnet, false) {
                        Ok((deployment, ast_artifacts)) if ast_artifacts.success => {
                            let mut artifacts = setup_session_with_deployment(
                                manifest,
                                &deployment,
                                Some(&ast_artifacts.asts),
                            );
                            for (contract_id, mut parser_diags) in ast_artifacts.diags.into_iter() {
                                // Merge parser's diags with analysis' diags.
                                if let Some(ref mut diags) = artifacts.diags.remove(&contract_id) {
                                    parser_diags.append(diags);
                                }
                                artifacts.diags.insert(contract_id, parser_diags);
                            }
                            Ok((deployment, None, artifacts))
                        }
                        Ok((deployment, ast_artifacts)) => Ok((deployment, None, ast_artifacts)),
                        Err(e) => Err(e),
                    }
                }
            }
        }
        Some(path) => {
            let deployment_location = get_absolute_deployment_path(manifest, path)
                .expect("unable to retrieve deployment");
            match load_deployment(manifest, &deployment_location) {
                Ok(deployment) => {
                    let artifacts = setup_session_with_deployment(manifest, &deployment, None);
                    Ok((deployment, Some(deployment_location.to_string()), artifacts))
                }
                Err(e) => Err(format!("loading {path} failed with error: {e}")),
            }
        }
    };

    match result {
        Ok(deployment) => deployment,
        Err(e) => {
            eprintln!("{}", format_err!(e));
            process::exit(1);
        }
    }
}

fn should_existing_plan_be_replaced(
    existing_plan: &DeploymentSpecification,
    new_plan: &DeploymentSpecification,
) -> bool {
    use similar::{ChangeTag, TextDiff};

    let existing_file = existing_plan
        .to_file_content()
        .expect("unable to serialize deployment");
    let new_file = new_plan
        .to_file_content()
        .expect("unable to serialize deployment");

    if existing_file == new_file {
        return false;
    }

    println!("{}", blue!("A new deployment plan was computed and differs from the default deployment plan currently saved on disk:"));

    let diffs = TextDiff::from_lines(
        std::str::from_utf8(&existing_file).unwrap(),
        std::str::from_utf8(&new_file).unwrap(),
    );

    for change in diffs.iter_all_changes() {
        let formatted_change = match change.tag() {
            ChangeTag::Delete => {
                format!("{} {}", red!("-"), red!("{change}"))
            }
            ChangeTag::Insert => {
                format!("{} {}", green!("+"), green!("{change}"))
            }
            ChangeTag::Equal => format!("  {change}"),
        };
        print!("{formatted_change}");
    }

    println!("{}", yellow!("Overwrite? [Y/n]"));
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer).unwrap();

    !buffer.starts_with('n')
}

fn load_deployment_if_exists(
    manifest: &ProjectManifest,
    network: &StacksNetwork,
    force_on_disk: bool,
    force_computed: bool,
) -> Option<Result<DeploymentSpecification, String>> {
    let default_deployment_location = match get_default_deployment_path(manifest, network) {
        Ok(location) => location,
        Err(e) => return Some(Err(e)),
    };
    if !default_deployment_location.exists() {
        return None;
    }

    if !force_on_disk {
        match generate_default_deployment(manifest, network, true) {
            Ok((deployment, _)) => {
                use similar::{ChangeTag, TextDiff};

                let current_version = match default_deployment_location.read_content() {
                    Ok(content) => content,
                    Err(message) => return Some(Err(message)),
                };

                let updated_version = match deployment.to_file_content() {
                    Ok(res) => res,
                    Err(err) => return Some(Err(format!("failed serializing deployment\n{err}"))),
                };

                if updated_version == current_version {
                    return Some(load_deployment(manifest, &default_deployment_location));
                }

                if !force_computed {
                    println!("{}", blue!("A new deployment plan was computed and differs from the default deployment plan currently saved on disk:"));

                    let diffs = TextDiff::from_lines(
                        std::str::from_utf8(&current_version).unwrap(),
                        std::str::from_utf8(&updated_version).unwrap(),
                    );

                    for change in diffs.iter_all_changes() {
                        let formatted_change = match change.tag() {
                            ChangeTag::Delete => {
                                format!("{} {}", red!("-"), red!("{change}"))
                            }
                            ChangeTag::Insert => {
                                format!("{} {}", green!("+"), green!("{change}"))
                            }
                            ChangeTag::Equal => format!("  {change}"),
                        };
                        print!("{formatted_change}");
                    }

                    println!("{}", yellow!("Overwrite? [Y/n]"));
                    let mut buffer = String::new();
                    std::io::stdin().read_line(&mut buffer).unwrap();
                    if buffer.starts_with('n') {
                        Some(load_deployment(manifest, &default_deployment_location))
                    } else {
                        default_deployment_location
                            .write_content(&updated_version)
                            .ok()?;
                        Some(Ok(deployment))
                    }
                } else {
                    default_deployment_location
                        .write_content(&updated_version)
                        .ok()?;
                    Some(Ok(deployment))
                }
            }
            Err(message) => {
                eprintln!(
                    "{} unable to compute an updated plan\n{}",
                    red!("error:"),
                    message
                );
                Some(load_deployment(manifest, &default_deployment_location))
            }
        }
    } else {
        Some(load_deployment(manifest, &default_deployment_location))
    }
}

fn sanitize_project_name(name: &str) -> String {
    let sanitized: String = name
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '/' {
                c
            } else {
                '_'
            }
        })
        .collect();
    if sanitized.is_empty() || sanitized.chars().all(|c| c == '_' || c == '/') {
        eprintln!("{} Invalid project name", red!("error:"));
        process::exit(1)
    }
    sanitized
}

fn execute_changes(changes: Vec<Changes>) -> bool {
    let mut shared_config = None;

    for mut change in changes.into_iter() {
        match change {
            Changes::AddFile(options) => {
                if let Ok(entry) = fs::metadata(&options.path) {
                    if entry.is_file() {
                        println!(
                            "{} file already exists at path {}",
                            yellow!("warning:"),
                            options.path
                        );
                        continue;
                    }
                }
                let mut file = match File::create(options.path.clone()) {
                    Ok(file) => file,
                    Err(e) => {
                        eprintln!(
                            "{} Unable to create file {}: {}",
                            red!("error:"),
                            options.path,
                            e
                        );
                        return false;
                    }
                };
                match file.write_all(options.content.as_bytes()) {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!(
                            "{} Unable to write file {}: {}",
                            red!("error:"),
                            options.path,
                            e
                        );
                        return false;
                    }
                };
                println!("{}", options.comment);
            }
            Changes::AddDirectory(options) => {
                match fs::create_dir_all(options.path.clone()) {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!(
                            "{} Unable to create directory {}: {}",
                            red!("error:"),
                            options.path,
                            e
                        );
                        return false;
                    }
                };
                println!("{}", options.comment);
            }
            Changes::EditTOML(ref mut options) => {
                let mut config = match shared_config.take() {
                    Some(config) => config,
                    None => {
                        let manifest_location = options.manifest_location.clone();
                        let project_manifest_content = match manifest_location.read_content() {
                            Ok(content) => content,
                            Err(message) => {
                                eprintln!("{}", format_err!(message));
                                return false;
                            }
                        };

                        let project_manifest_file: ProjectManifestFile =
                            match toml::from_slice(&project_manifest_content[..]) {
                                Ok(manifest) => manifest,
                                Err(message) => {
                                    eprintln!(
                                        "{} Failed to process manifest file: {}",
                                        red!("error:"),
                                        message
                                    );
                                    return false;
                                }
                            };
                        match ProjectManifest::from_project_manifest_file(
                            project_manifest_file,
                            &manifest_location,
                            true,
                        ) {
                            Ok(content) => content,
                            Err(message) => {
                                eprintln!("{}", format_err!(message));
                                return false;
                            }
                        }
                    }
                };

                let mut requirements = config.project.requirements.take().unwrap_or_default();
                for requirement in options.requirements_to_add.drain(..) {
                    if !requirements.contains(&requirement) {
                        requirements.push(requirement);
                    }
                }
                config.project.requirements = Some(requirements);

                for (contract_name, contract_config) in options.contracts_to_add.drain() {
                    config.contracts.insert(contract_name, contract_config);
                }
                for contract_name in options.contracts_to_rm.iter() {
                    config.contracts.remove(contract_name);
                }

                shared_config = Some(config);
                println!("{}", options.comment);
            }
            Changes::RemoveFile(options) => {
                if let Ok(entry) = fs::metadata(&options.path) {
                    if !entry.is_file() {
                        eprintln!(
                            "{} file doesn't exist at path {}",
                            yellow!("warning:"),
                            options.path
                        );
                        continue;
                    }
                }
                match fs::remove_file(&options.path) {
                    Ok(_) => println!("{}", options.comment),
                    Err(e) => eprintln!("error {e}"),
                }
            }
        }
    }

    if let Some(project_manifest) = shared_config {
        let toml_value = match toml::Value::try_from(&project_manifest) {
            Ok(value) => value,
            Err(e) => {
                eprintln!("{} failed encoding config file ({})", red!("error:"), e);
                return false;
            }
        };

        let pretty_toml = match toml::ser::to_string_pretty(&toml_value) {
            Ok(value) => value,
            Err(e) => {
                eprintln!("{} failed formatting config file ({})", red!("error:"), e);
                return false;
            }
        };

        if let Err(message) = project_manifest
            .location
            .write_content(pretty_toml.as_bytes())
        {
            eprintln!(
                "{} Unable to update manifest file - {}",
                red!("error:"),
                message
            );
            return false;
        }
    }

    true
}

fn prompt_user_to_continue() {
    println!("{}", yellow!("Do you want to continue? (y/N)"));
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer).unwrap();
    if !buffer.trim().eq_ignore_ascii_case("y") {
        std::process::exit(0);
    }
}

fn display_separator() {
    println!("{}", yellow!("----------------------------"));
}

fn display_hint_header() {
    display_separator();
    println!("{}", yellow!("Hint: what's next?"));
}

fn display_hint_footer() {
    println!(
        "{}",
        yellow!(
            "These hints can be disabled in the {} file.",
            ClarinetRC::get_settings_file_path()
        )
    );
    println!(
        "{}",
        blue!(
            "  $ mkdir -p ~/.clarinet; echo \"enable_hints = false\" >> {}",
            ClarinetRC::get_settings_file_path()
        )
    );
    display_separator();
}

fn display_devnet_incompatibilities(incompatibles: Vec<(String, String, String)>) {
    println!(
        "{}",
        yellow!("The default snapshot can not be used because the following Devnet.toml fields are incompatible with the snapshot:")
    );
    for (field, user_value, default_value) in incompatibles {
        println!(
            "{}",
            yellow!("{}:\n{}\n{} (default)", field, user_value, default_value)
        );
    }
}

fn display_devnet_incompatibilities_continue() {
    println!(
        "{}",
        yellow!("Continuing with startup, no snapshot will be used")
    );
    println!(
        "{}",
        yellow!("You can use the --from-genesis flag to skip the snapshot")
    );
}

fn display_post_check_hint() {
    println!();
    display_hint_header();
    println!(
        "{}",
        yellow!("Once you are ready to write TypeScript unit tests for your contract, run the following command:\n")
    );
    println!("{}", blue!("  $ npm install"));
    println!("{}", blue!("  $ npm test"));
    println!(
        "{}",
        yellow!("    Run all run tests in the ./tests folder.\n")
    );
    println!("{}", yellow!("Find more information on testing with Clarinet here: https://docs.hiro.so/stacks/clarinet-js-sdk"));
    display_hint_footer();
}

fn display_contract_new_hint(project_name: Option<&str>) {
    println!();
    display_hint_header();
    if let Some(project_name) = project_name {
        println!(
            "{}",
            yellow!("Switch to the newly created directory with:\n")
        );
        println!("{}", blue!("  $ cd {project_name}\n"));
    }
    println!(
        "{}",
        yellow!("Once you are ready to write your contracts, run the following commands:\n")
    );
    println!("{}", blue!("  $ clarinet contract new <contract-name>"));
    println!(
        "{}",
        yellow!("    Create new contract scaffolding, including test files.\n")
    );

    println!("{}", blue!("  $ clarinet check"));
    println!(
        "{}",
        yellow!("    Check contract syntax for all files in ./contracts.\n")
    );

    println!("{}", yellow!("Find more information on writing contracts with Clarinet here: https://docs.hiro.so/clarinet/how-to-guides/how-to-set-up-local-development-environment#developing-a-clarity-smart-contract"));
    display_hint_footer();
}

fn display_deploy_hint() {
    println!();
    display_hint_header();
    println!(
        "{}",
        yellow!("Once your contracts are ready to be deployed, you can run the following:")
    );

    println!("{}", blue!("  $ clarinet deployments apply --testnet"));
    println!(
        "{}",
        yellow!("    Deploy all contracts to the testnet network.\n")
    );

    println!("{}", blue!("  $ clarinet deployments apply --mainnet"));
    println!(
        "{}",
        yellow!("    Deploy all contracts to the mainnet network.\n")
    );

    println!(
        "{}",
        yellow!("Keep in mind, you can configure networks by editing the TOML files in the ./settings folder")
    );
    println!(
        "{}",
        yellow!("Find more information on the devnet here: https://docs.hiro.so/clarinet/guides/how-to-run-integration-environment")
    );
    display_hint_footer();
}

fn devnet_start(cmd: DevnetStart, clarinetrc: ClarinetRC) {
    let manifest = load_manifest_or_exit(cmd.manifest_path, false);
    println!("Computing deployment plan");
    let result = match cmd.deployment_plan_path {
        None => {
            let res = if let Some(package) = cmd.package {
                let Ok(package_file) = File::open(package) else {
                    eprintln!("{} package file not found", red!("error:"));
                    std::process::exit(1);
                };
                let deployment: ConfigurationPackage = serde_json::from_reader(package_file)
                    .expect("error while reading deployment specification");
                Some(Ok(deployment.deployment_plan))
            } else {
                load_deployment_if_exists(
                    &manifest,
                    &StacksNetwork::Devnet,
                    cmd.use_on_disk_deployment_plan,
                    cmd.use_computed_deployment_plan,
                )
            };
            match res {
                Some(Ok(deployment)) => {
                    println!(
                        "{} using existing deployments/default.devnet-plan.yaml",
                        yellow!("note:")
                    );
                    // TODO(lgalabru): Think more about the desired DX.
                    // Compute the latest version, display differences and propose overwrite?
                    Ok(deployment)
                }
                Some(Err(e)) => Err(e),
                None => {
                    let default_deployment_path =
                        get_default_deployment_path(&manifest, &StacksNetwork::Devnet).unwrap();
                    let (deployment, _) =
                        match generate_default_deployment(&manifest, &StacksNetwork::Devnet, false)
                        {
                            Ok(deployment) => deployment,
                            Err(message) => {
                                eprintln!("{}", red!(message));
                                std::process::exit(1);
                            }
                        };
                    let res = write_deployment(&deployment, &default_deployment_path, true);
                    if let Err(message) = res {
                        Err(message)
                    } else {
                        println!(
                            "{} {}",
                            green!("Generated file"),
                            default_deployment_path.get_relative_location().unwrap()
                        );
                        Ok(deployment)
                    }
                }
            }
        }
        Some(deployment_plan_path) => {
            let deployment_path = get_absolute_deployment_path(&manifest, &deployment_plan_path)
                .expect("unable to retrieve deployment");
            load_deployment(&manifest, &deployment_path)
        }
    };

    let deployment = match result {
        Ok(deployment) => deployment,
        Err(e) => {
            eprintln!("{}", format_err!(e));
            std::process::exit(1);
        }
    };

    let orchestrator = match DevnetOrchestrator::new(manifest, None, None, true, cmd.no_dashboard) {
        Ok(orchestrator) => orchestrator,
        Err(e) => {
            eprintln!("{}", format_err!(e));
            process::exit(1);
        }
    };

    if orchestrator.manifest.project.telemetry {
        #[cfg(feature = "telemetry")]
        telemetry_report_event(DeveloperUsageEvent::DevnetExecuted(
            DeveloperUsageDigest::new(
                &orchestrator.manifest.project.name,
                &orchestrator.manifest.project.authors,
            ),
        ));
    }

    let devnet_config = match orchestrator.network_config {
        Some(ref network_config) => match &network_config.devnet {
            Some(devnet_config) => Ok(devnet_config.clone()),
            _ => Err("Unable to retrieve config"),
        },
        _ => Err("Unable to retrieve config"),
    };
    let differ = DevnetDiffConfig::new();
    let compatible = differ.is_compatible(&devnet_config.unwrap());
    if let Err(incompatibles) = compatible {
        display_devnet_incompatibilities(incompatibles);
        prompt_user_to_continue();
        display_devnet_incompatibilities_continue()
    }
    let config = StartConfig {
        devnet: orchestrator,
        deployment,
        log_tx: None,
        display_dashboard: !cmd.no_dashboard,
        no_snapshot: cmd.no_snapshot,
        save_container_logs: cmd.save_container_logs,
        create_new_snapshot: cmd.create_new_snapshot,
    };
    match start(config) {
        Err(e) => {
            eprintln!("{}", format_err!(e));
            process::exit(1);
        }
        Ok(_) => {
            if clarinetrc.enable_hints.unwrap_or(true) {
                display_deploy_hint();
            }
            process::exit(0);
        }
    }
}

#[cfg(test)]
mod tests {
    use clap_complete::generate;

    use super::*;

    #[test]
    fn test_completion_for_shells() {
        for shell in [
            Shell::Bash,
            Shell::Elvish,
            Shell::Fish,
            Shell::PowerShell,
            Shell::Zsh,
        ] {
            let result = std::panic::catch_unwind(move || {
                let mut output_buffer = Vec::new();
                let mut cmd = Opts::command();
                generate(shell, &mut cmd, "clarinet", &mut output_buffer);
                assert!(
                    !output_buffer.is_empty(),
                    "failed to generate completion for {shell}",
                );
            });
            assert!(result.is_ok(), "failed to generate completion for {shell}",);
        }
    }

    #[test]
    fn test_sanitize_project_name() {
        let sanitized = sanitize_project_name("hello_world");
        assert_eq!(sanitized, "hello_world");

        let sanitized = sanitize_project_name("Hello_World");
        assert_eq!(sanitized, "Hello_World");

        let sanitized = sanitize_project_name("Hello-World");
        assert_eq!(sanitized, "Hello-World");

        let sanitized = sanitize_project_name("hello/world");
        assert_eq!(sanitized, "hello/world");

        let sanitized = sanitize_project_name("H€llo/world");
        assert_eq!(sanitized, "H_llo/world");

        let sanitized = sanitize_project_name("H€llo/world");
        assert_eq!(sanitized, "H_llo/world");
    }
}
