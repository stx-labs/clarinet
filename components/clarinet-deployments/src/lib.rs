use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::fmt::Write;

extern crate serde;

#[macro_use]
extern crate serde_derive;

pub mod diagnostic_digest;
#[cfg(not(target_arch = "wasm32"))]
pub mod onchain;
pub mod requirements;
pub mod types;

use clarinet_files::{FileAccessor, FileLocation, NetworkManifest, ProjectManifest, StacksNetwork};
use clarity_repl::analysis::ast_dependency_detector::{ASTDependencyDetector, DependencySet};
use clarity_repl::clarity::vm::ast::ContractAST;
use clarity_repl::clarity::vm::diagnostic::Diagnostic;
use clarity_repl::clarity::vm::types::{PrincipalData, QualifiedContractIdentifier};
use clarity_repl::clarity::vm::{
    ContractName, EvaluationResult, ExecutionResult, SymbolicExpression,
};
use clarity_repl::clarity::StacksEpochId;
use clarity_repl::repl::boot::{
    get_boot_contract_epoch_and_clarity_version, BOOT_CONTRACTS_DATA, SBTC_DEPOSIT_MAINNET_ADDRESS,
    SBTC_MAINNET_ADDRESS, SBTC_TESTNET_ADDRESS_PRINCIPAL, SBTC_TOKEN_MAINNET_ADDRESS,
};
use clarity_repl::repl::session::ExecutionResultMap;
use clarity_repl::repl::{
    ClarityCodeSource, ClarityContract, ClarityInterpreter, ContractDeployer, Session,
    SessionSettings, DEFAULT_EPOCH,
};
use types::{
    ContractPublishSpecification, DeploymentGenerationArtifacts, EmulatedContractCallSpecification,
    EpochSpec, RequirementPublishSpecification, StxTransferSpecification, TransactionSpecification,
};

use self::types::{
    DeploymentSpecification, EmulatedContractPublishSpecification, GenesisSpecification,
    TransactionPlanSpecification, TransactionsBatchSpecification, WalletSpecification,
};

pub fn setup_session_with_deployment(
    manifest: &ProjectManifest,
    deployment: &DeploymentSpecification,
    contracts_asts: Option<&BTreeMap<QualifiedContractIdentifier, ContractAST>>,
) -> DeploymentGenerationArtifacts {
    let mut session = initiate_session_from_manifest(manifest);
    let contracts =
        update_session_with_deployment_plan(&mut session, deployment, contracts_asts, None);

    let deps = BTreeMap::new();
    let mut diags = HashMap::new();
    let mut results_values = HashMap::new();
    let mut asts = BTreeMap::new();
    let mut contracts_analysis = HashMap::new();
    let mut success = true;
    for (contract_id, res) in contracts.into_iter() {
        match res {
            Ok(execution_result) => {
                diags.insert(contract_id.clone(), execution_result.diagnostics);
                if let EvaluationResult::Contract(contract_result) = execution_result.result {
                    results_values.insert(contract_id.clone(), contract_result.result);
                    asts.insert(contract_id.clone(), contract_result.contract.ast);
                    contracts_analysis.insert(contract_id, contract_result.contract.analysis);
                }
            }
            Err(errors) => {
                success = false;
                diags.insert(contract_id.clone(), errors);
            }
        }
    }

    DeploymentGenerationArtifacts {
        asts,
        deps,
        diags,
        results_values,
        success,
        session,
        analysis: contracts_analysis,
    }
}

pub fn initiate_session_from_manifest(manifest: &ProjectManifest) -> Session {
    let settings = SessionSettings {
        repl_settings: manifest.repl_settings.clone(),
        disk_cache_enabled: true,
        cache_location: Some(manifest.project.cache_location.to_path_buf()),
        override_boot_contracts_source: manifest.project.override_boot_contracts_source.clone(),
        ..Default::default()
    };
    Session::new(settings)
}

fn update_session_with_genesis_accounts(
    session: &mut Session,
    deployment: &DeploymentSpecification,
) {
    if let Some(ref spec) = deployment.genesis {
        let addresses: Vec<_> = spec.wallets.iter().map(|w| w.address.clone()).collect();
        session.interpreter.save_genesis_accounts(addresses);

        for wallet in spec.wallets.iter() {
            let _ = session.interpreter.mint_stx_balance(
                wallet.address.clone().into(),
                wallet.balance.try_into().unwrap(),
            );
            if wallet.name == "deployer" {
                session.set_tx_sender(&wallet.address.to_string());
            }
        }
    }
}

fn fund_genesis_account_with_sbtc(session: &mut Session, deployment: &DeploymentSpecification) {
    if let Some(ref spec) = deployment.genesis {
        let block_height = session.interpreter.get_burn_block_height() - 1;
        let height = session.eval_clarity_string(&format!("u{block_height}"));
        let hash = session.eval_clarity_string(&format!(
            "(unwrap-panic (get-burn-block-info? header-hash u{block_height}))"
        ));
        let vout_index = session.eval_clarity_string("u1");

        for wallet in spec.wallets.iter() {
            if wallet.sbtc_balance == 0 {
                continue;
            }

            let mut random_tx_id = String::with_capacity(64);
            for _ in 0..32 {
                write!(&mut random_tx_id, "{:02x}", rand::random::<u8>()).unwrap();
            }
            let tx_id = session.eval_clarity_string(&format!("0x{random_tx_id}"));
            let mut random_sweep_txid = String::with_capacity(64);
            for _ in 0..32 {
                write!(&mut random_sweep_txid, "{:02x}", rand::random::<u8>()).unwrap();
            }
            let sweep_tx_id = session.eval_clarity_string(&format!("0x{random_sweep_txid}"));
            let amount = session.eval_clarity_string(&format!("u{}", wallet.sbtc_balance));
            let recipient = session.eval_clarity_string(&format!("'{}", wallet.address));

            let args = vec![
                tx_id,
                vout_index.clone(),
                amount,
                recipient,
                hash.clone(),
                height.clone(),
                sweep_tx_id,
            ];
            let _ = session.call_contract_fn(
                &SBTC_DEPOSIT_MAINNET_ADDRESS.to_string(),
                "complete-deposit-wrapper",
                &args,
                SBTC_MAINNET_ADDRESS,
                false,
                false,
            );
        }
    }
}

pub fn update_session_with_deployment_plan(
    session: &mut Session,
    deployment: &DeploymentSpecification,
    contracts_asts: Option<&BTreeMap<QualifiedContractIdentifier, ContractAST>>,
    forced_min_epoch: Option<StacksEpochId>,
) -> ExecutionResultMap {
    update_session_with_genesis_accounts(session, deployment);

    let mut should_mint_sbtc = false;

    let mut contracts = BTreeMap::new();
    for batch in deployment.plan.batches.iter() {
        let epoch: StacksEpochId = match (batch.epoch, forced_min_epoch) {
            (Some(epoch), _) => epoch.into(),
            _ => DEFAULT_EPOCH,
        };
        session.advance_chain_tip(1);
        session.update_epoch(epoch);

        for transaction in batch.transactions.iter() {
            match transaction {
                TransactionSpecification::RequirementPublish(_)
                | TransactionSpecification::BtcTransfer(_)
                | TransactionSpecification::ContractCall(_)
                | TransactionSpecification::ContractPublish(_) => {
                    panic!("emulated-contract-call and emulated-contract-publish are the only operations admitted in simnet deployments")
                }
                TransactionSpecification::EmulatedContractPublish(tx) => {
                    let contract_id = QualifiedContractIdentifier::new(
                        tx.emulated_sender.clone(),
                        tx.contract_name.clone(),
                    );
                    if !should_mint_sbtc && contract_id == *SBTC_DEPOSIT_MAINNET_ADDRESS {
                        should_mint_sbtc = true;
                    }
                    let contract_ast = contracts_asts.as_ref().and_then(|m| m.get(&contract_id));
                    let result = handle_emulated_contract_publish(session, tx, contract_ast, epoch);
                    contracts.insert(contract_id, result);
                }
                TransactionSpecification::EmulatedContractCall(tx) => {
                    let _ = handle_emulated_contract_call(session, tx);
                }
                TransactionSpecification::StxTransfer(tx) => {
                    handle_stx_transfer(session, tx);
                }
            }
        }
    }

    if should_mint_sbtc {
        fund_genesis_account_with_sbtc(session, deployment);
    }

    contracts
}

fn handle_stx_transfer(session: &mut Session, tx: &StxTransferSpecification) {
    let default_tx_sender = session.get_tx_sender();
    session.set_tx_sender(&tx.expected_sender.to_string());

    let _ = session.stx_transfer(tx.mstx_amount, &tx.recipient.to_string());

    session.set_tx_sender(&default_tx_sender);
}

fn handle_emulated_contract_publish(
    session: &mut Session,
    tx: &EmulatedContractPublishSpecification,
    contract_ast: Option<&ContractAST>,
    epoch: StacksEpochId,
) -> Result<ExecutionResult, Vec<Diagnostic>> {
    let default_tx_sender = session.get_tx_sender();
    session.set_tx_sender(&tx.emulated_sender.to_string());

    let contract = ClarityContract {
        code_source: ClarityCodeSource::ContractInMemory(tx.source.clone()),
        deployer: ContractDeployer::Address(tx.emulated_sender.to_string()),
        name: tx.contract_name.to_string(),
        clarity_version: tx.clarity_version,
        epoch: clarity_repl::repl::Epoch::Specific(epoch),
    };

    let result = session.deploy_contract(&contract, false, contract_ast);

    session.set_tx_sender(&default_tx_sender);
    result
}

fn handle_emulated_contract_call(
    session: &mut Session,
    tx: &EmulatedContractCallSpecification,
) -> Result<ExecutionResult, Vec<Diagnostic>> {
    let default_tx_sender = session.get_tx_sender();
    session.set_tx_sender(&tx.emulated_sender.to_string());

    let params: Vec<SymbolicExpression> = tx
        .parameters
        .iter()
        .map(|p| session.eval_clarity_string(p))
        .collect();
    let result = session.call_contract_fn(
        &tx.contract_id.to_string(),
        &tx.method.to_string(),
        &params,
        &tx.emulated_sender.to_string(),
        true,
        false,
    );
    if let Err(errors) = &result {
        println!("error: {:?}", errors.first().unwrap().message);
    }

    session.set_tx_sender(&default_tx_sender);
    result
}

pub async fn generate_default_deployment(
    manifest: &ProjectManifest,
    network: &StacksNetwork,
    no_batch: bool,
    file_accessor: Option<&dyn FileAccessor>,
    forced_min_epoch: Option<StacksEpochId>,
) -> Result<(DeploymentSpecification, DeploymentGenerationArtifacts), String> {
    let network_manifest = match file_accessor {
        None => NetworkManifest::from_project_manifest_location(
            &manifest.location,
            &network.get_networks(),
            manifest.use_mainnet_wallets(),
            Some(&manifest.project.cache_location),
            None,
        )?,
        Some(file_accessor) => {
            NetworkManifest::from_project_manifest_location_using_file_accessor(
                &manifest.location,
                &network.get_networks(),
                manifest.use_mainnet_wallets(),
                file_accessor,
            )
            .await?
        }
    };

    let (stacks_node, bitcoin_node) = match network {
        StacksNetwork::Simnet => (None, None),
        StacksNetwork::Devnet => {
            let (stacks_node, bitcoin_node) = match network_manifest.devnet {
                Some(ref devnet) => {
                    let stacks_node = format!("http://localhost:{}", devnet.stacks_node_rpc_port);
                    let bitcoin_node = format!(
                        "http://{}:{}@localhost:{}",
                        devnet.bitcoin_node_username,
                        devnet.bitcoin_node_password,
                        devnet.bitcoin_node_rpc_port
                    );
                    (stacks_node, bitcoin_node)
                }
                None => {
                    let stacks_node = "http://localhost:20443".to_string();
                    let bitcoin_node = "http://devnet:devnet@localhost:18443".to_string();
                    (stacks_node, bitcoin_node)
                }
            };
            (Some(stacks_node), Some(bitcoin_node))
        }
        StacksNetwork::Testnet => {
            let stacks_node = network_manifest
                .network
                .stacks_node_rpc_address
                .unwrap_or("https://api.testnet.hiro.so".to_string());
            let bitcoin_node = network_manifest.network.bitcoin_node_rpc_address.unwrap_or(
                "http://blockstack:blockstacksystem@bitcoind.testnet.stacks.co:18332".to_string(),
            );
            (Some(stacks_node), Some(bitcoin_node))
        }
        StacksNetwork::Mainnet => {
            let stacks_node = network_manifest
                .network
                .stacks_node_rpc_address
                .unwrap_or("https://api.hiro.so".to_string());
            let bitcoin_node = network_manifest.network.bitcoin_node_rpc_address.unwrap_or(
                "http://blockstack:blockstacksystem@bitcoin.blockstack.com:8332".to_string(),
            );
            (Some(stacks_node), Some(bitcoin_node))
        }
    };

    let deployment_fee_rate = network_manifest.network.deployment_fee_rate;

    let Some(default_deployer) = network_manifest.accounts.get("deployer") else {
        return Err("unable to retrieve default deployer account".to_string());
    };
    let Ok(default_deployer_address) =
        PrincipalData::parse_standard_principal(&default_deployer.stx_address)
    else {
        return Err(format!(
            "unable to turn address {} as a valid Stacks address",
            default_deployer.stx_address
        ));
    };

    let mut transactions = BTreeMap::new();
    let mut contracts_map = BTreeMap::new();
    let mut requirements_data = BTreeMap::new();
    let mut requirements_deps = BTreeMap::new();

    let mut repl_settings = manifest.repl_settings.clone();
    repl_settings.remote_data.enabled = false;

    let override_boot_contracts_source = if matches!(network, StacksNetwork::Simnet) {
        manifest.project.override_boot_contracts_source.clone()
    } else {
        if !manifest.project.override_boot_contracts_source.is_empty() {
            eprintln!("Warning: Custom boot contracts are only supported on simnet. Ignoring override_boot_contracts_source configuration for {network:?} network.");
        }
        BTreeMap::new()
    };

    let settings = SessionSettings {
        repl_settings,
        override_boot_contracts_source,
        ..Default::default()
    };

    let simnet_remote_data =
        matches!(network, StacksNetwork::Simnet) && manifest.repl_settings.remote_data.enabled;

    let mut boot_contracts_ids = BTreeSet::new();

    if !simnet_remote_data {
        let boot_contracts_data = if settings.override_boot_contracts_source.is_empty() {
            BOOT_CONTRACTS_DATA.clone()
        } else {
            clarity_repl::repl::boot::get_boot_contracts_data_with_overrides(
                &settings.override_boot_contracts_source,
            )
        };
        let mut boot_contracts_asts = BTreeMap::new();
        for (id, (contract, ast)) in boot_contracts_data {
            boot_contracts_ids.insert(id.clone());
            boot_contracts_asts.insert(id, (contract.clarity_version, ast));
        }
        requirements_data.append(&mut boot_contracts_asts);
    }

    // this ephemeral interpreter is used to parse code and build ASTs
    let interpreter = ClarityInterpreter::new(
        settings.get_default_sender(),
        settings.repl_settings.clone(),
        settings.cache_location.clone(),
    );

    // Initialize diagnostics collection and success tracking early
    let mut contract_diags: HashMap<QualifiedContractIdentifier, Vec<Diagnostic>> = HashMap::new();
    let mut asts_success = true;

    // Validate custom boot contracts from override_boot_contracts_source
    if !settings.override_boot_contracts_source.is_empty() && !simnet_remote_data {
        for (contract_name, file_path) in &settings.override_boot_contracts_source {
            // Only validate existing boot contracts that are being overridden
            if !clarity_repl::repl::boot::BOOT_CONTRACTS_NAMES.contains(&contract_name.as_str()) {
                continue;
            }

            // Resolve the relative path to the project root
            let project_root = manifest
                .location
                .get_parent_location()
                .map_err(|e| format!("Failed to get project root: {e}"))?;
            let mut resolved_path = project_root.clone();
            resolved_path
                .append_path(file_path)
                .map_err(|e| format!("Failed to resolve boot contract path {file_path}: {e}"))?;
            let resolved_path_string = resolved_path.to_string();

            // Load and validate the custom boot contract
            let custom_source = match file_accessor {
                None => {
                    // Fallback to file system when no file_accessor is provided
                    std::fs::read_to_string(&resolved_path_string).map_err(|e| {
                        format!("Failed to read boot contract file {resolved_path_string}: {e}")
                    })
                }
                Some(file_accessor) => {
                    let sources = file_accessor
                        .read_files(vec![resolved_path_string.clone()])
                        .await
                        .map_err(|e| {
                            format!("Failed to read boot contract file {resolved_path_string}: {e}")
                        })?;
                    sources
                        .get(&resolved_path_string)
                        .ok_or_else(|| {
                            format!("Unable to read custom boot contract: {contract_name}")
                        })
                        .cloned()
                }
            }?;

            let (epoch, clarity_version) =
                get_boot_contract_epoch_and_clarity_version(contract_name.as_str());

            let temp_contract = ClarityContract {
                code_source: ClarityCodeSource::ContractInMemory(custom_source),
                deployer: ContractDeployer::Address(default_deployer_address.to_address()),
                name: contract_name.clone(),
                clarity_version,
                epoch: clarity_repl::repl::Epoch::Specific(epoch),
            };

            let (_, diagnostics, ast_success) = interpreter.build_ast(&temp_contract);

            if !ast_success {
                contract_diags.insert(
                    QualifiedContractIdentifier::new(
                        default_deployer_address.clone(),
                        ContractName::try_from(contract_name.clone())
                            .unwrap_or_else(|_| ContractName::from("unknown")),
                    ),
                    diagnostics,
                );
                asts_success = false;
            }
        }
    }

    let mut queue = VecDeque::new();

    let mut contract_epochs = HashMap::new();

    // Build the ASTs / DependencySet for requirements - step required for Simnet/Devnet/Testnet/Mainnet
    if let Some(ref requirements) = manifest.project.requirements {
        let cache_location = &manifest.project.cache_location;
        let mut emulated_contracts_publish = HashMap::new();
        let mut requirements_publish = HashMap::new();

        // automatically add sbtc-deposit if only sbtc-token is present
        if requirements
            .iter()
            .any(|r| r.contract_id == SBTC_TOKEN_MAINNET_ADDRESS.to_string())
            && !requirements
                .iter()
                .any(|r| r.contract_id == SBTC_DEPOSIT_MAINNET_ADDRESS.to_string())
        {
            queue.push_front((
                QualifiedContractIdentifier::parse(&SBTC_DEPOSIT_MAINNET_ADDRESS.to_string())
                    .unwrap(),
                None,
            ));
        }

        // Load all the requirements
        // Some requirements are explicitly listed, some are discovered as we compute the ASTs.
        for requirement in requirements.iter() {
            let contract_id = match QualifiedContractIdentifier::parse(&requirement.contract_id) {
                Ok(contract_id) => contract_id,
                Err(_e) => {
                    return Err(format!(
                        "malformatted contract_id: {}",
                        requirement.contract_id
                    ))
                }
            };
            queue.push_front((contract_id, None));
        }

        while let Some((contract_id, forced_clarity_version)) = queue.pop_front() {
            if requirements_deps.contains_key(&contract_id) {
                continue;
            }

            // Did we already get the source in a prior cycle?
            let requirement_data = match requirements_data.remove(&contract_id) {
                Some(requirement_data) => requirement_data,
                None => {
                    // Download the code
                    let (source, epoch, clarity_version, contract_location) =
                        requirements::retrieve_contract(
                            &contract_id,
                            cache_location,
                            &file_accessor,
                        )
                        .await?;

                    let epoch = match forced_min_epoch {
                        Some(min_epoch) => std::cmp::max(min_epoch, epoch),
                        None => epoch,
                    };

                    contract_epochs.insert(contract_id.clone(), epoch);

                    // Build the struct representing the requirement in the deployment
                    if matches!(network, StacksNetwork::Simnet) {
                        if !simnet_remote_data {
                            let data = EmulatedContractPublishSpecification {
                                contract_name: contract_id.name.clone(),
                                emulated_sender: contract_id.issuer.clone(),
                                source: source.clone(),
                                location: contract_location,
                                clarity_version,
                            };

                            emulated_contracts_publish.insert(contract_id.clone(), data);
                        }
                    } else if matches!(network, StacksNetwork::Devnet) {
                        let mut remap_principals = BTreeMap::new();
                        remap_principals
                            .insert(contract_id.issuer.clone(), default_deployer_address.clone());

                        let data = RequirementPublishSpecification {
                            contract_id: contract_id.clone(),
                            remap_sender: default_deployer_address.clone(),
                            source: source.clone(),
                            location: contract_location,
                            cost: deployment_fee_rate * source.len() as u64,
                            remap_principals,
                            clarity_version,
                        };
                        requirements_publish.insert(contract_id.clone(), data);
                    } else if matches!(network, StacksNetwork::Testnet) {
                        let mut remap_sender = default_deployer_address.clone();
                        let mut remap_principals = BTreeMap::new();
                        remap_principals
                            .insert(contract_id.issuer.clone(), default_deployer_address.clone());

                        // Remap sBTC mainnet address to testnet address
                        if contract_id.issuer.to_string() == SBTC_MAINNET_ADDRESS {
                            remap_sender = SBTC_TESTNET_ADDRESS_PRINCIPAL.clone();
                            remap_principals.insert(
                                contract_id.issuer.clone(),
                                SBTC_TESTNET_ADDRESS_PRINCIPAL.clone(),
                            );
                        }

                        let data = RequirementPublishSpecification {
                            contract_id: contract_id.clone(),
                            remap_sender,
                            source: source.clone(),
                            location: contract_location,
                            cost: deployment_fee_rate * source.len() as u64,
                            remap_principals,
                            clarity_version,
                        };
                        requirements_publish.insert(contract_id.clone(), data);
                    }

                    // Compute the AST
                    let contract = ClarityContract {
                        code_source: ClarityCodeSource::ContractInMemory(source),
                        name: contract_id.name.to_string(),
                        deployer: ContractDeployer::ContractIdentifier(contract_id.clone()),
                        clarity_version,
                        epoch: clarity_repl::repl::Epoch::Specific(epoch),
                    };
                    let (ast, _, _) = interpreter.build_ast(&contract);
                    (clarity_version, ast)
                }
            };

            // Detect the eventual dependencies for this AST
            let mut contract_data = BTreeMap::new();
            let (_, ast) = requirement_data;
            let clarity_version = match forced_clarity_version {
                Some(clarity_version) => clarity_version,
                None => {
                    let (_, _, clarity_version, _) = requirements::retrieve_contract(
                        &contract_id,
                        cache_location,
                        &file_accessor,
                    )
                    .await?;
                    clarity_version
                }
            };
            contract_data.insert(contract_id.clone(), (clarity_version, ast));
            let dependencies =
                ASTDependencyDetector::detect_dependencies(&contract_data, &requirements_data);
            let (_, ast) = contract_data
                .remove(&contract_id)
                .expect("unable to retrieve ast");

            // Extract the known / unknown dependencies
            match dependencies {
                Ok(inferable_dependencies) => {
                    if inferable_dependencies.len() > 1 {
                        println!("warning: inferable_dependencies contains more than one entry");
                    }
                    // We submitted a HashMap with one contract, so we have at most one result in the `inferable_dependencies` map.
                    // We will extract and keep the associated data (source, ast, deps).
                    if let Some((contract_id, dependencies)) =
                        inferable_dependencies.into_iter().next()
                    {
                        for dependency in dependencies.iter() {
                            queue.push_back((dependency.contract_id.clone(), None));
                        }
                        requirements_deps.insert(contract_id.clone(), dependencies);
                        requirements_data.insert(contract_id.clone(), (clarity_version, ast));
                    }
                }
                Err((inferable_dependencies, non_inferable_dependencies)) => {
                    // In the case of unknown dependencies, we were unable to construct an exhaustive list of dependencies.
                    // As such, we will re-enqueue the present (front) and push all the unknown contract_ids in front of it,
                    // and we will keep the source in memory to avoid useless disk access.
                    for (_, dependencies) in inferable_dependencies.iter() {
                        for dependency in dependencies.iter() {
                            queue.push_back((dependency.contract_id.clone(), None));
                        }
                    }
                    requirements_data.insert(contract_id.clone(), (clarity_version, ast));
                    queue.push_front((contract_id, None));

                    for non_inferable_contract_id in non_inferable_dependencies.into_iter() {
                        queue.push_front((non_inferable_contract_id, None));
                    }
                }
            };
        }

        // Avoid listing requirements as deployment transactions to the deployment specification on Mainnet
        if !matches!(network, StacksNetwork::Mainnet) && !simnet_remote_data {
            let mut ordered_contracts_ids = match ASTDependencyDetector::order_contracts(
                &requirements_deps,
                &contract_epochs,
            ) {
                Ok(ordered_contracts) => ordered_contracts,
                Err(e) => return Err(format!("unable to order requirements {e}")),
            };

            // Filter out boot contracts from requirement dependencies
            ordered_contracts_ids.retain(|contract_id| !boot_contracts_ids.contains(contract_id));

            if matches!(network, StacksNetwork::Simnet) {
                for contract_id in ordered_contracts_ids.iter() {
                    let data = emulated_contracts_publish
                        .remove(contract_id)
                        .unwrap_or_else(|| panic!("unable to retrieve contract: {contract_id}"));
                    let tx = TransactionSpecification::EmulatedContractPublish(data);
                    add_transaction_to_epoch(
                        &mut transactions,
                        tx,
                        &contract_epochs[contract_id].into(),
                    );
                }
            } else if matches!(network, StacksNetwork::Devnet | StacksNetwork::Testnet) {
                for contract_id in ordered_contracts_ids.iter() {
                    let data = requirements_publish
                        .remove(contract_id)
                        .unwrap_or_else(|| panic!("unable to retrieve contract: {contract_id}"));
                    let tx = TransactionSpecification::RequirementPublish(data);
                    add_transaction_to_epoch(
                        &mut transactions,
                        tx,
                        &contract_epochs[contract_id].into(),
                    );
                }
            }
        }
    }

    let mut contracts = HashMap::new();
    let mut contracts_sources = HashMap::new();

    let base_location = manifest.location.clone().get_parent_location()?;

    let sources: HashMap<String, String> = match file_accessor {
        None => {
            let mut sources = HashMap::new();
            for (_, contract_config) in manifest.contracts.iter() {
                let mut contract_location = base_location.clone();
                contract_location
                    .append_path(contract_config.expect_contract_path_as_str())
                    .map_err(|_| {
                        format!(
                            "unable to build path for contract {}",
                            contract_config.expect_contract_path_as_str()
                        )
                    })?;

                let source = contract_location
                    .read_content_as_utf8()
                    .map_err(|_| format!("unable to find contract at {contract_location}"))?;
                sources.insert(contract_location.to_string(), source);
            }
            sources
        }
        Some(file_accessor) => {
            let contracts_location = manifest
                .contracts
                .values()
                .map(|contract_config| {
                    let mut contract_location = base_location.clone();
                    contract_location
                        .append_path(contract_config.expect_contract_path_as_str())
                        .unwrap();
                    contract_location.to_string()
                })
                .collect();
            file_accessor.read_files(contracts_location).await?
        }
    };

    for (name, contract_config) in manifest.contracts.iter() {
        let Ok(contract_name) = ContractName::try_from(name.to_string()) else {
            return Err(format!("unable to use {name} as a valid contract name"));
        };

        let deployer = match &contract_config.deployer {
            ContractDeployer::DefaultDeployer => default_deployer,
            ContractDeployer::LabeledDeployer(deployer) => {
                let Some(deployer) = network_manifest.accounts.get(deployer) else {
                    return Err(format!("unable to retrieve account '{deployer}'"));
                };
                deployer
            }
            _ => unreachable!(),
        };

        let Ok(sender) = PrincipalData::parse_standard_principal(&deployer.stx_address) else {
            return Err(format!(
                "unable to turn emulated_sender {} as a valid Stacks address",
                deployer.stx_address
            ));
        };

        let mut contract_location = base_location.clone();
        contract_location.append_path(contract_config.expect_contract_path_as_str())?;
        let source = sources
            .get(&contract_location.to_string())
            .ok_or(format!(
                "Invalid Clarinet.toml, source file not found for: {}",
                &name
            ))?
            .clone();

        let contract_id = QualifiedContractIdentifier::new(sender.clone(), contract_name.clone());

        let epoch = match forced_min_epoch {
            Some(min_epoch) => std::cmp::max(min_epoch, contract_config.epoch.resolve()),
            None => contract_config.epoch.resolve(),
        };

        contracts_sources.insert(
            contract_id.clone(),
            ClarityContract {
                code_source: ClarityCodeSource::ContractInMemory(source.clone()),
                deployer: ContractDeployer::Address(sender.to_address()),
                name: contract_name.to_string(),
                clarity_version: contract_config.clarity_version,
                epoch: clarity_repl::repl::Epoch::Specific(epoch),
            },
        );

        let contract_spec = if matches!(network, StacksNetwork::Simnet) {
            TransactionSpecification::EmulatedContractPublish(
                EmulatedContractPublishSpecification {
                    contract_name,
                    emulated_sender: sender,
                    source,
                    location: contract_location,
                    clarity_version: contract_config.clarity_version,
                },
            )
        } else {
            TransactionSpecification::ContractPublish(ContractPublishSpecification {
                contract_name,
                expected_sender: sender,
                location: contract_location,
                cost: deployment_fee_rate.saturating_mul(source.len().try_into().unwrap()),
                source,
                anchor_block_only: true,
                clarity_version: contract_config.clarity_version,
            })
        };

        contracts.insert(contract_id, contract_spec);
    }

    let session = Session::new(settings);

    let mut contract_asts = BTreeMap::new();
    let mut contract_data = BTreeMap::new();

    for (contract_id, contract) in contracts_sources.into_iter() {
        let (ast, diags, ast_success) = session.interpreter.build_ast(&contract);
        contract_asts.insert(contract_id.clone(), ast.clone());
        contract_data.insert(contract_id.clone(), (contract.clarity_version, ast));
        contract_diags.insert(contract_id.clone(), diags);
        contract_epochs.insert(contract_id, contract.epoch.resolve());
        asts_success = asts_success && ast_success;
    }

    let dependencies =
        ASTDependencyDetector::detect_dependencies(&contract_data, &requirements_data);

    let mut dependencies = match dependencies {
        Ok(dependencies) => dependencies,
        Err((dependencies, _)) => {
            // No need to report an error here, it will be caught and reported
            // with proper location information by the later analyses.
            dependencies
        }
    };

    for contract_id in boot_contracts_ids.into_iter() {
        dependencies.insert(contract_id.clone(), DependencySet::new());
    }

    dependencies.extend(requirements_deps);

    let ordered_contracts_ids =
        ASTDependencyDetector::order_contracts(&dependencies, &contract_epochs)
            .map_err(|e| e.to_string())?;

    // Track the latest epoch that a contract is deployed in, so that we can
    // ensure that all contracts are deployed after their dependencies.
    for contract_id in ordered_contracts_ids.into_iter() {
        if requirements_data.contains_key(contract_id) {
            continue;
        }
        let tx = contracts
            .remove(contract_id)
            .expect("unable to retrieve contract");

        match tx {
            TransactionSpecification::EmulatedContractPublish(ref data) => {
                contracts_map.insert(
                    contract_id.clone(),
                    (data.source.clone(), data.location.clone()),
                );
            }
            TransactionSpecification::ContractPublish(ref data) => {
                contracts_map.insert(
                    contract_id.clone(),
                    (data.source.clone(), data.location.clone()),
                );
            }
            _ => unreachable!(),
        }
        add_transaction_to_epoch(&mut transactions, tx, &contract_epochs[contract_id].into());
    }

    let tx_chain_limit = match no_batch {
        true => 100_000,
        false => 25,
    };

    let mut batches = vec![];
    let mut batch_count = 0;
    for (epoch, epoch_transactions) in transactions {
        for txs in epoch_transactions.chunks(tx_chain_limit) {
            if !txs.is_empty() {
                batches.push(TransactionsBatchSpecification {
                    id: batch_count,
                    transactions: txs.to_vec(),
                    epoch: Some(epoch),
                });
                batch_count += 1;
            }
        }
    }

    let mut wallets = vec![];
    if matches!(network, StacksNetwork::Simnet) {
        for (name, account) in network_manifest.accounts {
            let address = PrincipalData::parse_standard_principal(&account.stx_address)
                .map_err(|_| format!("unable to parse address {}", account.stx_address))?;
            wallets.push(WalletSpecification {
                name,
                address,
                balance: account.balance.into(),
                sbtc_balance: account.sbtc_balance.into(),
            });
        }
    }

    let name = match network {
        StacksNetwork::Simnet => "Simulated deployment, used as a default for `clarinet console`, `clarinet test` and `clarinet check`".to_string(),
        _ => format!("{network:?} deployment")
    };

    let deployment = DeploymentSpecification {
        id: 0,
        name,
        stacks_node,
        bitcoin_node,
        network: network.clone(),
        genesis: if matches!(network, StacksNetwork::Simnet) {
            let genesis_contracts = manifest.project.boot_contracts.clone();

            Some(GenesisSpecification {
                wallets,
                contracts: genesis_contracts,
            })
        } else {
            None
        },
        plan: TransactionPlanSpecification { batches },
        contracts: contracts_map,
    };

    // Check for custom boot contract validation errors and return error if any
    if !asts_success && matches!(network, StacksNetwork::Simnet) {
        let mut error_messages = Vec::new();
        for (contract_id, diagnostics) in &contract_diags {
            if !diagnostics.is_empty() {
                let contract_name = contract_id.name.to_string();
                let error_details: Vec<String> = diagnostics
                    .iter()
                    .map(|d| format!("  - {}", d.message))
                    .collect();
                if manifest
                    .project
                    .override_boot_contracts_source
                    .contains_key(&contract_name)
                {
                    error_messages.push(format!(
                        "Custom boot contract validation failed:\n{}",
                        error_messages.join("\n\n")
                    ));
                } else {
                    error_messages.push(format!(
                        "'{}' has validation errors:\n{}",
                        contract_name,
                        error_details.join("\n")
                    ));
                }
            }
        }
    }

    let artifacts = DeploymentGenerationArtifacts {
        asts: contract_asts,
        deps: dependencies,
        diags: contract_diags,
        success: asts_success,
        results_values: HashMap::new(),
        analysis: HashMap::new(),
        session,
    };

    Ok((deployment, artifacts))
}

fn add_transaction_to_epoch(
    transactions: &mut BTreeMap<EpochSpec, Vec<TransactionSpecification>>,
    transaction: TransactionSpecification,
    epoch: &EpochSpec,
) {
    let epoch_transactions = match transactions.get_mut(epoch) {
        Some(v) => v,
        None => {
            transactions.insert(*epoch, vec![]);
            transactions.get_mut(epoch).unwrap()
        }
    };
    epoch_transactions.push(transaction);
}

pub fn get_default_deployment_path(
    manifest: &ProjectManifest,
    network: &StacksNetwork,
) -> Result<FileLocation, String> {
    let mut deployment_path = manifest.location.get_project_root_location()?;
    deployment_path.append_path("deployments")?;
    deployment_path.append_path(match network {
        StacksNetwork::Simnet => "default.simnet-plan.yaml",
        StacksNetwork::Devnet => "default.devnet-plan.yaml",
        StacksNetwork::Testnet => "default.testnet-plan.yaml",
        StacksNetwork::Mainnet => "default.mainnet-plan.yaml",
    })?;
    Ok(deployment_path)
}

pub fn load_deployment(
    manifest: &ProjectManifest,
    deployment_plan_location: &FileLocation,
) -> Result<DeploymentSpecification, String> {
    let project_root_location = manifest.location.get_project_root_location()?;
    let spec = match DeploymentSpecification::from_config_file(
        deployment_plan_location,
        &project_root_location,
    ) {
        Ok(spec) => spec,
        Err(msg) => {
            return Err(format!(
                "error: {deployment_plan_location} syntax incorrect\n{msg}"
            ));
        }
    };
    Ok(spec)
}

#[cfg(test)]
mod tests {
    use clarity::vm::types::TupleData;
    use clarity::vm::{ClarityName, ClarityVersion, Value};
    use clarity_repl::repl::clarity_values::to_raw_value;
    use clarity_repl::repl::SessionSettings;

    use super::*;

    static DEPLOYER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

    fn deploy_contract(
        session: &mut Session,
        name: &str,
        source: &str,
        epoch: StacksEpochId,
    ) -> Result<ExecutionResult, Vec<Diagnostic>> {
        let emulated_publish_spec = EmulatedContractPublishSpecification {
            contract_name: ContractName::from(name),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            source: source.to_string(),
            clarity_version: ClarityVersion::Clarity2,
            location: FileLocation::from_path_string("/contracts/contract_1.clar").unwrap(),
        };

        handle_emulated_contract_publish(session, &emulated_publish_spec, None, epoch)
    }

    #[test]
    fn test_handle_emulated_publish() {
        let mut session = Session::new(SessionSettings::default());
        let epoch = StacksEpochId::Epoch25;
        session.update_epoch(epoch);

        let snippet = "(define-public (plus-2 (n int)) (ok (+ n 2)))";
        let result = deploy_contract(&mut session, "contract_1", snippet, epoch);
        assert!(result.is_ok());
    }

    #[test]
    fn test_handle_emulated_contract_call_with_simple_params() {
        let mut session = Session::new(SessionSettings::default());
        let epoch = StacksEpochId::Epoch25;
        session.update_epoch(epoch);

        let snippet = [
            "(define-data-var x int 0)",
            "(define-public (add (n int)) (ok (var-set x (+ (var-get x) n))))",
        ]
        .join("\n");
        let result = deploy_contract(&mut session, "contract_1", &snippet, epoch);
        assert!(result.is_ok());

        let contract_id = QualifiedContractIdentifier::new(
            PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            ContractName::from("contract_1"),
        );

        let contract_call_spec = EmulatedContractCallSpecification {
            contract_id: contract_id.clone(),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            method: ClarityName::from("add"),
            parameters: vec!["1".to_string()],
        };
        let result = handle_emulated_contract_call(&mut session, &contract_call_spec);
        assert!(result.is_ok());

        let var_x = session.interpreter.get_data_var(&contract_id, "x");
        assert_eq!(var_x, Some(to_raw_value(&Value::Int(1))));
    }

    #[test]
    fn test_handle_emulated_contract_call_with_list() {
        let mut session = Session::new(SessionSettings::default());
        let epoch = StacksEpochId::Epoch25;
        session.update_epoch(epoch);

        let snippet = [
            "(define-data-var sum int 0)",
            "(define-public (set-sum (i int) (ns (list 10 int)))",
            "  (ok (var-set sum (fold + ns i)))",
            ")",
        ]
        .join("\n");
        let result = deploy_contract(&mut session, "contract_1", &snippet, epoch);
        assert!(result.is_ok());

        let contract_id = QualifiedContractIdentifier::new(
            PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            ContractName::from("contract_1"),
        );

        let contract_call_spec = EmulatedContractCallSpecification {
            contract_id: contract_id.clone(),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            method: ClarityName::from("set-sum"),
            parameters: vec!["2".to_string(), "(list 20 20)".to_string()],
        };
        let result = handle_emulated_contract_call(&mut session, &contract_call_spec);
        assert!(result.is_ok());

        let var_x = session.interpreter.get_data_var(&contract_id, "sum");
        assert_eq!(var_x, Some(to_raw_value(&Value::Int(42))));
    }

    #[test]
    fn test_handle_emulated_contract_call_with_tuple() {
        let mut session = Session::new(SessionSettings::default());
        let epoch = StacksEpochId::Epoch25;
        session.update_epoch(epoch);

        let snippet = [
            "(define-data-var data { a: int, b: uint } { a: 0, b: u0} )",
            "(define-public (set-data (l { a: int }) (r { b: uint }))",
            "  (ok (var-set data (merge l r)))",
            ")",
        ]
        .join("\n");
        let result = deploy_contract(&mut session, "contract_1", &snippet, epoch);
        assert!(result.is_ok());

        let contract_id = QualifiedContractIdentifier::new(
            PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            ContractName::from("contract_1"),
        );

        let contract_call_spec = EmulatedContractCallSpecification {
            contract_id: contract_id.clone(),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            method: ClarityName::from("set-data"),
            parameters: vec!["{ a: 2 }".to_string(), "{ b: u3 }".to_string()],
        };
        let result = handle_emulated_contract_call(&mut session, &contract_call_spec);
        assert!(result.is_ok());

        let data = session.interpreter.get_data_var(&contract_id, "data");
        assert_eq!(
            data,
            Some(to_raw_value(&Value::Tuple(
                TupleData::from_data(vec![
                    (
                        ClarityName::try_from("a".to_owned()).unwrap(),
                        Value::Int(2),
                    ),
                    (
                        ClarityName::try_from("b".to_owned()).unwrap(),
                        Value::UInt(3),
                    )
                ])
                .unwrap()
            )))
        );
    }

    #[test]
    fn test_stx_transfer() {
        let mut session = Session::new(SessionSettings::default());
        let epoch = StacksEpochId::Epoch25;
        session.update_epoch(epoch);

        let sender = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        let receiver = "ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG";
        let sender_principal = PrincipalData::parse_standard_principal(sender).unwrap();
        let receiver_principal = PrincipalData::parse_standard_principal(receiver).unwrap();

        let _ = session
            .interpreter
            .mint_stx_balance(PrincipalData::Standard(sender_principal.clone()), 1000000);

        let stx_transfer_spec = StxTransferSpecification {
            expected_sender: sender_principal,
            recipient: PrincipalData::Standard(receiver_principal),
            mstx_amount: 1000,
            cost: 0,
            anchor_block_only: true,
            memo: [0u8; 34],
        };

        handle_stx_transfer(&mut session, &stx_transfer_spec);

        let assets_maps = session.interpreter.get_assets_maps();
        let stx_maps = assets_maps.get("STX").unwrap();
        assert_eq!(*stx_maps.get(sender).unwrap(), 999000);
        assert_eq!(*stx_maps.get(receiver).unwrap(), 1000);
    }
}
