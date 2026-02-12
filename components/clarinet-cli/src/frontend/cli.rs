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
    get_manifest_location, FileLocation, NetworkManifest, ProjectManifest, RequirementConfig,
    StacksNetwork,
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
use clarity_repl::repl::{
    clarity_version_to_u8, ClarityCodeSource, ClarityContract, ContractDeployer, Epoch,
    DEFAULT_EPOCH,
};
use clarity_repl::{analysis, repl};
use stacks_network::{self, DevnetOrchestrator};
use toml_edit::DocumentMut;

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
    /// Encrypt deployment mnemonic
    #[clap(name = "encrypt", bin_name = "encrypt")]
    EncryptDeployment,
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
                            println!("{}", yellow!("Send usage data to Stacks Labs."));
                            println!("{}", yellow!("Help Stacks Labs improve its products and services by automatically sending diagnostics and usage data."));
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

                std::thread::spawn(move || {
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
                    while let Ok(cmd) = event_rx.recv() {
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
            Deployments::EncryptDeployment => {
                println!("{}", yellow!("Enter mnemonic to encrypt:"));
                let mut buffer = String::new();
                std::io::stdin().read_line(&mut buffer).unwrap();
                let phrase = buffer.trim();
                let password = rpassword::prompt_password("Enter password: ").unwrap();
                let encrypted_mnemonic =
                    clarinet_utils::encrypt_mnemonic_phrase(phrase, &password).unwrap();
                println!("encrypted_mnemonic = \"{encrypted_mnemonic}\"");
                std::process::exit(0);
            }
        },
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
                        let ((deployment, _, artifacts), _) = load_deployment_and_artifacts_or_exit(
                            manifest,
                            &cmd.deployment_plan_path,
                            cmd.use_on_disk_deployment_plan,
                            cmd.use_computed_deployment_plan,
                            false,
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
            let mut exit_codes = Vec::new();
            let mut global_found_env_simnet = false;
            for force_remove_env_simnet in [true, false] {
                let ((deployment, _, artifacts), found_env_simnet) =
                    load_deployment_and_artifacts_or_exit(
                        &manifest,
                        &cmd.deployment_plan_path,
                        cmd.use_on_disk_deployment_plan,
                        cmd.use_computed_deployment_plan,
                        force_remove_env_simnet,
                    );
                global_found_env_simnet |= found_env_simnet;
                if global_found_env_simnet {
                    if !force_remove_env_simnet {
                        println!("Checking contracts with #[env(simnet)] code");
                    } else {
                        println!("Checking contracts without #[env(simnet)] code");
                    }
                }
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
                exit_codes.push(exit_code);

                if clarinetrc.enable_hints.unwrap_or(true) {
                    display_post_check_hint();
                }
                if manifest.project.telemetry {
                    #[cfg(feature = "telemetry")]
                    telemetry_report_event(DeveloperUsageEvent::CheckExecuted(
                        DeveloperUsageDigest::new(
                            &manifest.project.name,
                            &manifest.project.authors,
                        ),
                    ));
                }
                if !global_found_env_simnet {
                    break;
                }
            }
            if exit_codes.contains(&1) {
                std::process::exit(1);
            } else {
                std::process::exit(0);
            }
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
    force_remove_env_simnet: bool,
) -> (
    (
        DeploymentSpecification,
        Option<String>,
        DeploymentGenerationArtifacts,
    ),
    bool,
) {
    let mut found_env_simnet = false;
    let result = match deployment_plan_path {
        None => {
            let res = load_deployment_if_exists(
                manifest,
                &StacksNetwork::Simnet,
                force_on_disk,
                force_computed,
            );
            match res {
                Some(Ok(mut deployment)) => {
                    println!(
                        "{} using deployments/default.simnet-plan.yaml",
                        yellow!("note:")
                    );
                    if force_remove_env_simnet {
                        found_env_simnet |= deployment.remove_env_simnet();
                    }
                    let artifacts = setup_session_with_deployment(manifest, &deployment, None);
                    Ok((deployment, None, artifacts))
                }
                Some(Err(e)) => Err(format!(
                    "loading deployments/default.simnet-plan.yaml failed with error: {e}"
                )),
                None => {
                    match generate_default_deployment(manifest, &StacksNetwork::Simnet, false) {
                        Ok((mut deployment, ast_artifacts)) if ast_artifacts.success => {
                            if force_remove_env_simnet {
                                found_env_simnet |= deployment.remove_env_simnet();
                            }
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
                Ok(mut deployment) => {
                    if force_remove_env_simnet {
                        found_env_simnet |= deployment.remove_env_simnet();
                    }
                    let artifacts = setup_session_with_deployment(manifest, &deployment, None);
                    Ok((deployment, Some(deployment_location.to_string()), artifacts))
                }
                Err(e) => Err(format!("loading {path} failed with error: {e}")),
            }
        }
    };

    match result {
        Ok(deployment) => (deployment, found_env_simnet),
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
    let mut shared_doc: Option<(FileLocation, DocumentMut)> = None;

    for mut change in changes {
        match change {
            Changes::AddFile(options) => {
                if fs::metadata(&options.path).is_ok_and(|e| e.is_file()) {
                    println!(
                        "{} file already exists at path {}",
                        yellow!("warning:"),
                        options.path
                    );
                    continue;
                }

                let write_result = File::create(&options.path)
                    .and_then(|mut file| file.write_all(options.content.as_bytes()));

                if let Err(e) = write_result {
                    eprintln!(
                        "{} Unable to write file {}: {e}",
                        red!("error:"),
                        options.path
                    );
                    return false;
                }

                println!("{}", options.comment);
            }

            Changes::AddDirectory(options) => {
                if let Err(e) = fs::create_dir_all(&options.path) {
                    eprintln!(
                        "{} Unable to create directory {}: {e}",
                        red!("error:"),
                        options.path
                    );
                    return false;
                }
                println!("{}", options.comment);
            }

            Changes::EditTOML(ref mut options) => {
                let (location, doc) = match shared_doc.take() {
                    Some(cached) => cached,
                    None => {
                        let Some(doc) = load_manifest(&options.manifest_location) else {
                            return false;
                        };
                        (options.manifest_location.clone(), doc)
                    }
                };

                let doc = edit_toml_document(doc, options);
                shared_doc = Some((location, doc));
                println!("{}", options.comment);
            }

            Changes::RemoveFile(options) => {
                if !fs::metadata(&options.path).is_ok_and(|e| e.is_file()) {
                    eprintln!(
                        "{} file doesn't exist at path {}",
                        yellow!("warning:"),
                        options.path
                    );
                    continue;
                }

                if let Err(e) = fs::remove_file(&options.path) {
                    eprintln!(
                        "{} Unable to remove file {}: {e}",
                        red!("error:"),
                        options.path
                    );
                    return false;
                }

                println!("{}", options.comment);
            }
        }
    }

    if let Some((location, doc)) = shared_doc {
        if let Err(e) = location.write_content(doc.to_string().as_bytes()) {
            eprintln!("{} Unable to update manifest file: {e}", red!("error:"));
            return false;
        }
    }

    true
}

/// Load and parse a manifest file, printing errors on failure.
fn load_manifest(location: &FileLocation) -> Option<DocumentMut> {
    let content = location
        .read_content()
        .map_err(|e| {
            eprintln!("{}", format_err!(e));
        })
        .ok()?;

    let content_str = std::str::from_utf8(&content)
        .map_err(|e| {
            eprintln!("{} Invalid UTF-8 in manifest file: {e}", red!("error:"));
        })
        .ok()?;

    content_str
        .parse()
        .map_err(|e| {
            eprintln!("{} Failed to parse manifest file: {e}", red!("error:"));
        })
        .ok()
}

/// Edit a TOML document directly, preserving comments and structure.
fn edit_toml_document(mut doc: DocumentMut, options: &mut TOMLEdition) -> DocumentMut {
    for req in options.requirements_to_add.drain(..) {
        add_requirement_to_doc(&mut doc, &req.contract_id);
    }

    for (name, contract) in options.contracts_to_add.drain() {
        add_contract_to_doc(&mut doc, &name, &contract);
    }

    for name in &options.contracts_to_rm {
        remove_contract_from_doc(&mut doc, name);
    }

    doc
}

/// Add a requirement to the [[project.requirements]] array in the document.
fn add_requirement_to_doc(doc: &mut DocumentMut, contract_id: &str) {
    use toml_edit::{ArrayOfTables, Item, Table};

    // Ensure [project] table exists
    let project = doc
        .entry("project")
        .or_insert(Item::Table(Table::new()))
        .as_table_mut()
        .expect("[project] should be a table");

    // Ensure [[project.requirements]] array exists.
    // If requirements = [] (an empty inline array), replace it with an array of tables.
    if project
        .get("requirements")
        .is_some_and(|v| v.as_array().is_some_and(|a| a.is_empty()))
    {
        project["requirements"] = Item::ArrayOfTables(ArrayOfTables::new());
    }

    let requirements = project
        .entry("requirements")
        .or_insert(Item::ArrayOfTables(ArrayOfTables::new()))
        .as_array_of_tables_mut()
        .expect("[[project.requirements]] should be an array of tables");

    // Check for duplicates
    let already_exists = requirements
        .iter()
        .filter_map(|req| req.get("contract_id")?.as_str())
        .any(|id| id == contract_id);

    if !already_exists {
        let mut new_req = Table::new();
        new_req["contract_id"] = toml_edit::value(contract_id);
        requirements.push(new_req);
    }
}

/// Add a contract to the [contracts.<name>] section in the document.
fn add_contract_to_doc(doc: &mut DocumentMut, name: &str, contract: &ClarityContract) {
    use toml_edit::{Item, Table};

    let contracts = doc
        .entry("contracts")
        .or_insert(Item::Table(Table::new()))
        .as_table_mut()
        .expect("[contracts] should be a table");

    let mut entry = Table::new();

    if let ClarityCodeSource::ContractOnDisk(path) = &contract.code_source {
        entry["path"] = toml_edit::value(path.display().to_string());
    }

    if let ContractDeployer::LabeledDeployer(label) = &contract.deployer {
        entry["deployer"] = toml_edit::value(label.as_str());
    }

    let clarity_version = clarity_version_to_u8(contract.clarity_version) as i64;
    entry["clarity_version"] = toml_edit::value(clarity_version);

    let epoch = match &contract.epoch {
        Epoch::Latest => "latest".to_string(),
        Epoch::Specific(e) => e.to_string(),
    };
    entry["epoch"] = toml_edit::value(epoch);

    contracts[name] = Item::Table(entry);
}

/// Remove a contract from the [contracts] section in the document.
fn remove_contract_from_doc(doc: &mut DocumentMut, name: &str) {
    if let Some(contracts) = doc.get_mut("contracts").and_then(|c| c.as_table_mut()) {
        contracts.remove(name);
    }
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
    println!("{}", yellow!("Find more information on testing with Clarinet here: https://docs.stacks.co/clarinet/testing-with-clarinet-sdk"));
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

    println!("{}", yellow!("Find more information on writing contracts with Clarinet here: https://docs.stacks.co/clarinet"));
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
        yellow!("Find more information on the devnet here: https://docs.stacks.co/clarinet/local-blockchain-development")
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

    mod toml_editing_tests {
        use std::path::PathBuf;

        use indoc::indoc;

        use super::*;

        /// Helper to create a ClarityContract for testing
        fn make_test_contract(name: &str) -> ClarityContract {
            ClarityContract {
                code_source: ClarityCodeSource::ContractOnDisk(PathBuf::from(format!(
                    "contracts/{}.clar",
                    name
                ))),
                name: name.to_string(),
                deployer: ContractDeployer::DefaultDeployer,
                clarity_version: ClarityVersion::Clarity2,
                epoch: Epoch::Latest,
            }
        }

        /// Helper to check if a string contains a comment (line starting with #)
        fn contains_comment(content: &str, comment_text: &str) -> bool {
            content.lines().any(|line| {
                let trimmed = line.trim();
                trimmed.starts_with('#') && trimmed.contains(comment_text)
            })
        }

        /// Helper to check if a TOML has a specific key-value in a section
        fn has_toml_value(content: &str, key_path: &str, expected_value: &str) -> bool {
            let doc: DocumentMut = content.parse().expect("Failed to parse TOML");
            let parts: Vec<&str> = key_path.split('.').collect();

            let mut current = doc.as_item();
            for (i, part) in parts.iter().enumerate() {
                if i == parts.len() - 1 {
                    // Last part - check the value
                    if let Some(table) = current.as_table() {
                        if let Some(item) = table.get(part) {
                            let value_str = item.to_string();
                            // Handle quoted strings
                            let cleaned = value_str.trim().trim_matches('"').trim_matches('\'');
                            return cleaned == expected_value;
                        }
                    }
                    return false;
                } else {
                    // Navigate to the next level
                    if let Some(table) = current.as_table() {
                        if let Some(item) = table.get(part) {
                            current = item;
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
            }
            false
        }

        /// Helper to check if a contract exists in the TOML
        fn has_contract(content: &str, contract_name: &str) -> bool {
            let doc: DocumentMut = content.parse().expect("Failed to parse TOML");
            if let Some(contracts) = doc.get("contracts").and_then(|c| c.as_table()) {
                return contracts.contains_key(contract_name);
            }
            false
        }

        /// Helper to check if a requirement exists in the TOML
        fn has_requirement(content: &str, contract_id: &str) -> bool {
            let doc: DocumentMut = content.parse().expect("Failed to parse TOML");
            if let Some(project) = doc.get("project").and_then(|p| p.as_table()) {
                if let Some(requirements) = project.get("requirements") {
                    if let Some(arr) = requirements.as_array_of_tables() {
                        return arr.iter().any(|req| {
                            req.get("contract_id")
                                .and_then(|v| v.as_str())
                                .map(|s| s == contract_id)
                                .unwrap_or(false)
                        });
                    }
                }
            }
            false
        }

        #[test]
        fn test_add_contract_preserves_comments() {
            let input = indoc! {r#"
                [project]
                name = "test-project"
                description = ""

                # This is an important comment that should be preserved!
                # Another comment line

                [repl.analysis]
                passes = ["check_checker"]

                # Check-checker settings comment
                [repl.analysis.check_checker]
                strict = false
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");
            add_contract_to_doc(&mut doc, "my-contract", &make_test_contract("my-contract"));

            let output = doc.to_string();

            // Verify comments are preserved
            assert!(
                contains_comment(&output, "important comment"),
                "Important comment should be preserved"
            );
            assert!(
                contains_comment(&output, "Another comment"),
                "Second comment should be preserved"
            );
            assert!(
                contains_comment(&output, "Check-checker settings"),
                "Check-checker comment should be preserved"
            );

            // Verify the contract was added
            assert!(
                has_contract(&output, "my-contract"),
                "Contract should be added"
            );

            // Verify existing settings are preserved
            assert!(
                has_toml_value(&output, "project.name", "test-project"),
                "Project name should be preserved"
            );
            assert!(
                has_toml_value(&output, "repl.analysis.check_checker.strict", "false"),
                "Repl settings should be preserved"
            );
        }

        #[test]
        fn test_remove_contract_preserves_comments_and_other_contracts() {
            let input = indoc! {r#"
                [project]
                name = "test-project"

                # Comment about contracts
                [contracts.keep-me]
                path = "contracts/keep-me.clar"
                clarity_version = 2
                epoch = "2.4"

                # Comment about remove-me
                [contracts.remove-me]
                path = "contracts/remove-me.clar"
                clarity_version = 1
                epoch = "2.0"

                # Final comment
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");
            remove_contract_from_doc(&mut doc, "remove-me");

            let output = doc.to_string();

            // Verify the contract was removed
            assert!(
                !has_contract(&output, "remove-me"),
                "Contract should be removed"
            );

            // Verify the other contract is still there
            assert!(
                has_contract(&output, "keep-me"),
                "Other contract should be preserved"
            );

            // Verify comments are preserved (at least the ones not directly attached to removed section)
            assert!(
                contains_comment(&output, "Comment about contracts"),
                "Contract section comment should be preserved"
            );
        }

        #[test]
        fn test_add_requirement_preserves_comments() {
            let input = indoc! {r#"
                [project]
                name = "test-project"
                description = "A test project"
                authors = ["Test Author"]
                telemetry = false

                # This comment should survive

                [contracts.my-contract]
                path = "contracts/my-contract.clar"
                clarity_version = 2
                epoch = "latest"
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");
            add_requirement_to_doc(
                &mut doc,
                "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait",
            );

            let output = doc.to_string();

            // Verify the requirement was added
            assert!(
                has_requirement(
                    &output,
                    "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait"
                ),
                "Requirement should be added"
            );

            // Verify comment is preserved
            assert!(
                contains_comment(&output, "This comment should survive"),
                "Comment should be preserved"
            );

            // Verify existing settings are preserved
            assert!(
                has_toml_value(&output, "project.name", "test-project"),
                "Project name should be preserved"
            );
            assert!(
                has_toml_value(&output, "project.telemetry", "false"),
                "Telemetry setting should be preserved"
            );

            // Verify contract is still there
            assert!(
                has_contract(&output, "my-contract"),
                "Contract should be preserved"
            );
        }

        #[test]
        fn test_add_requirement_with_empty_requirements_array() {
            let input = indoc! {r#"
                [project]
                name = 'project-template'
                requirements = []
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");
            add_requirement_to_doc(
                &mut doc,
                "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait",
            );

            let output = doc.to_string();

            assert!(
                has_requirement(
                    &output,
                    "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait"
                ),
                "Requirement should be added when requirements was an empty array"
            );

            assert!(
                has_toml_value(&output, "project.name", "project-template"),
                "Project name should be preserved"
            );
        }

        #[test]
        fn test_add_requirement_does_not_duplicate() {
            let input = indoc! {r#"
                [project]
                name = "test-project"

                [[project.requirements]]
                contract_id = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait"
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");

            // Try to add the same requirement again
            add_requirement_to_doc(
                &mut doc,
                "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait",
            );

            let output = doc.to_string();

            // Count how many times the contract_id appears
            let count = output.matches("nft-trait").count();
            assert_eq!(count, 1, "Requirement should not be duplicated");
        }

        #[test]
        fn test_add_multiple_requirements() {
            let input = indoc! {r#"
                [project]
                name = "test-project"

                [[project.requirements]]
                contract_id = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait"
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");
            add_requirement_to_doc(
                &mut doc,
                "SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9.another-trait",
            );

            let output = doc.to_string();

            // Both requirements should exist
            assert!(
                has_requirement(
                    &output,
                    "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait"
                ),
                "Original requirement should be preserved"
            );
            assert!(
                has_requirement(
                    &output,
                    "SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9.another-trait"
                ),
                "New requirement should be added"
            );
        }

        #[test]
        fn test_edit_simple_nft_example() {
            // Test with the actual simple-nft example content
            let input = indoc! {r#"
                [project]
                name = 'simple-nft'
                description = ''
                authors = []
                telemetry = false
                cache_dir = './.cache'

                [[project.requirements]]
                contract_id = 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait'

                [contracts.simple-nft]
                path = 'contracts/simple-nft.clar'
                clarity_version = 2
                epoch = 2.4

                # the analysis errors are used as a test case
                [repl.analysis]
                passes = ["check_checker"]
                check_checker = { trusted_sender = false, trusted_caller = false, callee_filter = false }

                # We don't want linter diagnostics in this test
                [repl.analysis.lint_groups]
                all = false
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");

            // Add a new contract
            add_contract_to_doc(
                &mut doc,
                "new-contract",
                &make_test_contract("new-contract"),
            );

            let output = doc.to_string();

            // Verify comments are preserved
            assert!(
                contains_comment(&output, "analysis errors are used as a test case"),
                "Analysis comment should be preserved"
            );
            assert!(
                contains_comment(&output, "don't want linter diagnostics"),
                "Linter comment should be preserved"
            );

            // Verify existing content is preserved
            assert!(
                has_contract(&output, "simple-nft"),
                "Original contract should be preserved"
            );
            assert!(
                has_contract(&output, "new-contract"),
                "New contract should be added"
            );
            assert!(
                has_requirement(
                    &output,
                    "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait"
                ),
                "Requirement should be preserved"
            );
            assert!(
                has_toml_value(&output, "project.cache_dir", "./.cache"),
                "Cache dir should be preserved"
            );
        }

        #[test]
        fn test_edit_counter_example() {
            // Test with the actual counter example content
            let input = indoc! {r#"
                [project]
                name = "counter"
                authors = []
                description = ""
                telemetry = false

                [contracts.counter]
                path = "contracts/counter.clar"
                clarity_version = 1
                epoch = "2.0"

                [contracts.counter-2]
                path = "contracts/counter-v2.clar"
                clarity_version = 2
                epoch = "2.4"

                [repl.analysis]
                passes = ["check_checker"]

                [repl.analysis.check_checker]
                strict = false
                trusted_sender = true
                trusted_caller = false
                callee_filter = false
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");

            // Remove one contract, add another
            remove_contract_from_doc(&mut doc, "counter");
            add_contract_to_doc(&mut doc, "counter-3", &make_test_contract("counter-3"));

            let output = doc.to_string();

            // Verify the operations
            assert!(
                !has_contract(&output, "counter"),
                "counter should be removed"
            );
            assert!(
                has_contract(&output, "counter-2"),
                "counter-2 should be preserved"
            );
            assert!(
                has_contract(&output, "counter-3"),
                "counter-3 should be added"
            );

            // Verify settings are preserved
            assert!(
                has_toml_value(&output, "project.name", "counter"),
                "Project name should be preserved"
            );
            assert!(
                has_toml_value(
                    &output,
                    "repl.analysis.check_checker.trusted_sender",
                    "true"
                ),
                "Check checker settings should be preserved"
            );
        }

        #[test]
        fn test_contract_with_labeled_deployer() {
            let input = indoc! {r#"
                [project]
                name = "test-project"
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");

            // Create a contract with a labeled deployer
            let contract = ClarityContract {
                code_source: ClarityCodeSource::ContractOnDisk(PathBuf::from(
                    "contracts/special.clar",
                )),
                name: "special".to_string(),
                deployer: ContractDeployer::LabeledDeployer("wallet_1".to_string()),
                clarity_version: ClarityVersion::Clarity3,
                epoch: Epoch::Specific(clarity_repl::clarity::StacksEpochId::Epoch25),
            };

            add_contract_to_doc(&mut doc, "special", &contract);

            let output = doc.to_string();

            // Verify the contract was added with all properties
            assert!(has_contract(&output, "special"), "Contract should be added");

            // Parse and check the values
            let parsed: DocumentMut = output.parse().unwrap();
            let contracts = parsed.get("contracts").unwrap().as_table().unwrap();
            let special = contracts.get("special").unwrap().as_table().unwrap();

            assert_eq!(
                special.get("deployer").unwrap().as_str().unwrap(),
                "wallet_1"
            );
            assert_eq!(
                special
                    .get("clarity_version")
                    .unwrap()
                    .as_integer()
                    .unwrap(),
                3
            );
        }

        #[test]
        fn test_preserves_inline_tables() {
            let input = indoc! {r#"
                [project]
                name = "test-project"

                [repl.analysis]
                passes = ["check_checker"]
                check_checker = { trusted_sender = false, trusted_caller = false, callee_filter = false }
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");
            add_contract_to_doc(&mut doc, "my-contract", &make_test_contract("my-contract"));

            let output = doc.to_string();

            // The inline table format should be preserved
            assert!(
                output.contains("check_checker = {") || output.contains("check_checker = { "),
                "Inline table format should be preserved: {}",
                output
            );
        }

        #[test]
        fn test_preserves_array_format() {
            let input = indoc! {r#"
                [project]
                name = "test-project"
                authors = ["Alice", "Bob"]
                requirements = []

                [repl.analysis]
                passes = ["check_checker", "lint"]
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");
            add_contract_to_doc(&mut doc, "my-contract", &make_test_contract("my-contract"));

            let output = doc.to_string();

            // Verify arrays are preserved
            assert!(
                output.contains("authors = [") || output.contains("authors= ["),
                "Authors array should be preserved"
            );
        }

        #[test]
        fn test_preserves_lint_groups_and_lints_ordering() {
            // Test with all lint groups and lints in a deliberately random order
            // with random values to ensure ordering is preserved after edits
            //
            // LintGroups: All, Perf, Safety, Style, Unused
            // LintNames: Noop, UnusedConst, UnusedDataVar, UnusedBinding, UnusedMap,
            //            UnusedPrivateFn, UnusedToken, UnusedTrait, CaseConst
            // LintLevels: ignore/allow/off/none, notice/note, warning/warn/on, error/err
            let input = indoc! {r#"
                [project]
                name = "lint-ordering-test"
                authors = []
                telemetry = false

                [contracts.existing-contract]
                path = "contracts/existing.clar"

                # Custom comment about linting configuration
                [repl.analysis]
                passes = ["check_checker", "lint"]

                # Lint groups in random order with random values
                [repl.analysis.lint_groups]
                style = "warn"
                unused = "error"
                all = false
                safety = "notice"
                perf = "off"

                # Individual lints in random order with random values
                [repl.analysis.lints]
                unused_map = "error"
                case_const = "warn"
                unused_binding = "off"
                noop = "notice"
                unused_trait = "warning"
                unused_const = "allow"
                unused_private_fn = "err"
                unused_data_var = "note"
                unused_token = "none"
            "#};

            let mut doc: DocumentMut = input.parse().expect("Failed to parse TOML");

            // Perform an edit (add a contract) to simulate real usage
            add_contract_to_doc(
                &mut doc,
                "new-contract",
                &make_test_contract("new-contract"),
            );

            let output = doc.to_string();

            // Verify the comment is preserved
            assert!(
                contains_comment(&output, "Custom comment about linting configuration"),
                "Linting comment should be preserved"
            );

            // Helper to extract keys in order from a TOML section
            fn get_keys_in_order(toml_str: &str, section: &str) -> Vec<String> {
                let doc: DocumentMut = toml_str.parse().unwrap();

                // Navigate to the section (e.g., "repl.analysis.lint_groups")
                let parts: Vec<&str> = section.split('.').collect();
                let mut current = doc.as_item();

                for part in &parts {
                    current = current.as_table().unwrap().get(part).unwrap();
                }

                current
                    .as_table()
                    .unwrap()
                    .iter()
                    .map(|(k, _)| k.to_string())
                    .collect()
            }

            // Verify lint_groups ordering is preserved
            let lint_groups_order = get_keys_in_order(&output, "repl.analysis.lint_groups");
            assert_eq!(
                lint_groups_order,
                vec!["style", "unused", "all", "safety", "perf"],
                "Lint groups ordering should be preserved"
            );

            // Verify lints ordering is preserved
            let lints_order = get_keys_in_order(&output, "repl.analysis.lints");
            assert_eq!(
                lints_order,
                vec![
                    "unused_map",
                    "case_const",
                    "unused_binding",
                    "noop",
                    "unused_trait",
                    "unused_const",
                    "unused_private_fn",
                    "unused_data_var",
                    "unused_token"
                ],
                "Lints ordering should be preserved"
            );

            // Also verify the values are preserved
            let parsed: DocumentMut = output.parse().unwrap();
            let lint_groups = parsed["repl"]["analysis"]["lint_groups"]
                .as_table()
                .unwrap();
            let lints = parsed["repl"]["analysis"]["lints"].as_table().unwrap();

            // Check a few lint group values
            assert_eq!(lint_groups["style"].as_str().unwrap(), "warn");
            assert_eq!(lint_groups["unused"].as_str().unwrap(), "error");
            assert!(!lint_groups["all"].as_bool().unwrap());
            assert_eq!(lint_groups["safety"].as_str().unwrap(), "notice");
            assert_eq!(lint_groups["perf"].as_str().unwrap(), "off");

            // Check a few lint values
            assert_eq!(lints["unused_map"].as_str().unwrap(), "error");
            assert_eq!(lints["case_const"].as_str().unwrap(), "warn");
            assert_eq!(lints["noop"].as_str().unwrap(), "notice");
            assert_eq!(lints["unused_token"].as_str().unwrap(), "none");
        }
    }
}
