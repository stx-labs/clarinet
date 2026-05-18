use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};
use std::fmt::Write;

pub mod diagnostic_digest;
#[cfg(not(target_arch = "wasm32"))]
pub mod onchain;
pub mod requirements;
pub mod types;

use std::path::{Path, PathBuf};

use clarinet_defaults::DEFAULT_EPOCH;
use clarinet_files::{
    paths, AccountConfig, DevnetConfig, FileAccessor, NetworkConfig, NetworkManifest,
    ProjectManifest, StacksNetwork,
};
use clarity::types::StacksEpochId;
use clarity::util::hash::Sha256Sum;
use clarity::vm::ast::ContractAST;
use clarity::vm::diagnostic::Diagnostic;
use clarity::vm::types::{PrincipalData, QualifiedContractIdentifier, StandardPrincipalData};
use clarity::vm::{
    ClarityVersion, ContractName, EvaluationResult, ExecutionResult, SymbolicExpression,
};
use clarity_repl::analysis::ast_dependency_detector::{ASTDependencyDetector, DependencySet};
use clarity_repl::repl::boot::{
    get_boot_contract_epoch_and_clarity_version, BOOT_CONTRACTS_DATA, SBTC_DEPOSIT_MAINNET_ADDRESS,
    SBTC_MAINNET_ADDRESS, SBTC_TESTNET_ADDRESS_PRINCIPAL, SBTC_TOKEN_MAINNET_ADDRESS,
};
use clarity_repl::repl::session::{AnnotatedExecutionResult, ExecutionResultMap};
use clarity_repl::repl::{
    ClarityCodeSource, ClarityContract, ClarityInterpreter, ContractDeployer, Session,
    SessionSettings,
};
use clarity_repl::utils::{remove_env_simnet, Environment};
pub use types::CachedContractAST;
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
    deployment: &mut DeploymentSpecification,
    contracts_asts: Option<&BTreeMap<QualifiedContractIdentifier, ContractAST>>,
    enable_analysis: bool,
) -> DeploymentGenerationArtifacts {
    // Mark contracts not in the project manifest as requirements
    // so that REPL analysis (linting) is skipped for them.
    // This is needed when the deployment plan is loaded from YAML,
    // where `skip_analysis` is not serialized. We match by location
    // (absolute path) rather than contract name because requirements
    // can share a name with a project contract.
    for batch in deployment.plan.batches.iter_mut() {
        for tx in batch.transactions.iter_mut() {
            if let TransactionSpecification::EmulatedContractPublish(ref mut spec) = tx {
                if !enable_analysis || !manifest.contracts_settings.contains_key(&spec.location) {
                    spec.skip_analysis = true;
                }
            }
        }
    }

    let mut session = initiate_session_from_manifest(manifest);
    if !enable_analysis {
        session.interpreter.repl_settings.analysis.disable_all();
    }
    let contracts = update_session_with_deployment_plan(&mut session, deployment, contracts_asts);

    let deps = BTreeMap::new();
    let mut diags = HashMap::new();
    let mut lint_diags = HashMap::new();
    let mut results_values = HashMap::new();
    let mut asts = BTreeMap::new();
    let mut contracts_analysis = HashMap::new();
    let mut success = true;
    for (contract_id, res) in contracts.into_iter() {
        match res {
            Ok(annotated) => {
                let AnnotatedExecutionResult {
                    execution_result,
                    lint_diagnostics,
                } = annotated;
                lint_diags.insert(contract_id.clone(), lint_diagnostics);
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
        lint_diags,
        results_values,
        success,
        session,
        analysis: contracts_analysis,
        ast_cache_entries: None,
    }
}

pub fn initiate_session_from_manifest(manifest: &ProjectManifest) -> Session {
    let settings = SessionSettings {
        repl_settings: manifest.repl_settings.clone(),
        disk_cache_enabled: true,
        cache_location: Some(manifest.project.cache_location.clone()),
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
) -> ExecutionResultMap {
    update_session_with_genesis_accounts(session, deployment);

    let mut should_mint_sbtc = false;

    let mut contracts = BTreeMap::new();
    for batch in deployment.plan.batches.iter() {
        let epoch: StacksEpochId = match batch.epoch {
            Some(epoch) => epoch.into(),
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
) -> Result<AnnotatedExecutionResult, Vec<Diagnostic>> {
    let default_tx_sender = session.get_tx_sender();
    session.set_tx_sender(&tx.emulated_sender.to_string());

    let contract = ClarityContract {
        code_source: ClarityCodeSource::ContractInMemory(tx.source.clone()),
        deployer: ContractDeployer::Address(tx.emulated_sender.to_string()),
        name: tx.contract_name.to_string(),
        clarity_version: tx.clarity_version,
        epoch: clarity_repl::repl::Epoch::Specific(epoch),
        skip_analysis: tx.skip_analysis,
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

/// Hash contract source for cache validation. SHA-256 is hardware-accelerated
/// on modern x86 (SHA-NI) and ARMv8 via the `sha2` crate's runtime dispatch.
pub(crate) fn compute_content_hash(source: &str) -> Sha256Sum {
    Sha256Sum::from_data(source.as_bytes())
}

/// RAII guard that keeps AST cache state atomic with respect to function
/// success: on the happy path, call [`commit`](Self::commit) to hand the
/// accumulated entries to the caller's artifacts; on any error (including
/// panic), `Drop` pushes them back into the input cache so a transient
/// failure doesn't cost the caller a cold rebuild.
///
/// Only constructed when the caller actually opted into caching
/// (wrapped in `Option<Self>` at the call site) — "no cache" and
/// "no guard" are the same state, and this struct doesn't need to
/// model the cache-free path.
struct AstCacheRestoreGuard<'a> {
    input_cache: &'a mut HashMap<(PathBuf, Environment), CachedContractAST>,
    pending: HashMap<(PathBuf, Environment), CachedContractAST>,
    committed: bool,
}

impl<'a> AstCacheRestoreGuard<'a> {
    fn new(input_cache: &'a mut HashMap<(PathBuf, Environment), CachedContractAST>) -> Self {
        Self {
            input_cache,
            pending: HashMap::new(),
            committed: false,
        }
    }

    /// Remove a matching cache entry from the input cache, or return
    /// `None` if missing or mismatched.
    fn try_reuse(
        &mut self,
        key: &(PathBuf, Environment),
        content_hash: &Sha256Sum,
        clarity_version: ClarityVersion,
        epoch: StacksEpochId,
    ) -> Option<CachedContractAST> {
        self.input_cache
            .remove(key)
            .filter(|entry| entry.matches(content_hash, clarity_version, epoch))
    }

    /// Accumulate a freshly-built (or verbatim-reused) cache entry.
    fn insert(&mut self, key: (PathBuf, Environment), entry: CachedContractAST) {
        self.pending.insert(key, entry);
    }

    /// Happy path: disarm the guard and take ownership of the entries so
    /// they can flow into `DeploymentGenerationArtifacts`.
    fn commit(mut self) -> HashMap<(PathBuf, Environment), CachedContractAST> {
        self.committed = true;
        std::mem::take(&mut self.pending)
    }
}

impl Drop for AstCacheRestoreGuard<'_> {
    fn drop(&mut self) {
        if !self.committed {
            self.input_cache.extend(std::mem::take(&mut self.pending));
        }
    }
}

/// Build the AST for one project contract, reusing a cached entry
/// when present. Returns the same (ast, diags, success) shape on
/// either path so the caller doesn't have to branch.
fn build_or_reuse_project_ast(
    contract: &ClarityContract,
    contract_location: PathBuf,
    environment: Environment,
    interpreter: &ClarityInterpreter,
    cache_guard: Option<&mut AstCacheRestoreGuard<'_>>,
) -> (ContractAST, Vec<Diagnostic>, bool) {
    let Some(guard) = cache_guard else {
        return interpreter.build_ast(contract);
    };

    let clarity_version = contract.clarity_version;
    let resolved_epoch = contract.epoch.resolve();
    let source = contract.expect_in_memory_code_source();
    let content_hash = compute_content_hash(source);
    let cache_key = (contract_location, environment);

    let cache_entry = guard
        .try_reuse(&cache_key, &content_hash, clarity_version, resolved_epoch)
        .unwrap_or_else(|| {
            let (ast, diags, ok) = interpreter.build_ast(contract);
            CachedContractAST::new(source, ast, diags, ok, clarity_version, resolved_epoch)
        });

    // TODO: drop these clones. The AST + diags end up owned in both the
    // returned tuple and the guard's pending entry (~20ms/build on a
    // 20-contract project). Needs `Arc<ContractAST>`, `&ContractAST` in
    // `detect_dependencies`, or merging the two output maps — each a
    // cross-crate refactor.
    let ast = cache_entry.ast.clone();
    let diags = cache_entry.diags.clone();
    let ok = cache_entry.ast_success;
    guard.insert(cache_key, cache_entry);
    (ast, diags, ok)
}

/// How to group publish transactions into batches.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BatchingMode {
    /// 25-tx chunks (the chained-transaction limit per anchor).
    Chunked,
    /// One batch per epoch — used for diffing against on-disk plans.
    Single,
}

impl BatchingMode {
    fn chunks<T>(self, items: &[T]) -> std::slice::Chunks<'_, T> {
        match self {
            BatchingMode::Chunked => items.chunks(25),
            // `.max(1)` avoids the `chunks(0)` panic on empty epochs;
            // for non-empty input `chunks(len)` yields one chunk.
            BatchingMode::Single => items.chunks(items.len().max(1)),
        }
    }
}

/// Fetch + parse requirements, infer dependencies, and append publish
/// transactions in dependency order. `requirements_data` must come in
/// pre-seeded with boot-contract ASTs.
#[allow(clippy::too_many_arguments)]
async fn resolve_requirements(
    manifest: &ProjectManifest,
    network: &StacksNetwork,
    simnet_remote_data: bool,
    file_accessor: Option<&dyn FileAccessor>,
    api_base_url: Option<&str>,
    interpreter: &ClarityInterpreter,
    default_deployer_address: &StandardPrincipalData,
    deployment_fee_rate: u64,
    boot_contracts_ids: &BTreeSet<QualifiedContractIdentifier>,
    requirements_data: &mut BTreeMap<QualifiedContractIdentifier, (ClarityVersion, ContractAST)>,
    requirements_deps: &mut BTreeMap<QualifiedContractIdentifier, DependencySet>,
    contract_epochs: &mut HashMap<QualifiedContractIdentifier, StacksEpochId>,
    transactions: &mut BTreeMap<EpochSpec, Vec<TransactionSpecification>>,
) -> Result<(), String> {
    let Some(requirements) = manifest.project.requirements.as_ref() else {
        return Ok(());
    };

    let mut emulated_contracts_publish = HashMap::new();
    let mut requirements_publish = HashMap::new();
    let mut queue = VecDeque::new();

    // sbtc-deposit transitively depends on sbtc-token, so users tend to
    // list only sbtc-token. Pull sbtc-deposit in too when that's the case.
    if requirements
        .iter()
        .any(|r| r.contract_id == SBTC_TOKEN_MAINNET_ADDRESS.to_string())
        && !requirements
            .iter()
            .any(|r| r.contract_id == SBTC_DEPOSIT_MAINNET_ADDRESS.to_string())
    {
        queue.push_front(
            QualifiedContractIdentifier::parse(&SBTC_DEPOSIT_MAINNET_ADDRESS.to_string()).unwrap(),
        );
    }

    for requirement in requirements.iter() {
        let contract_id = QualifiedContractIdentifier::parse(&requirement.contract_id)
            .map_err(|_e| format!("malformatted contract_id: {}", requirement.contract_id))?;
        queue.push_front(contract_id);
    }

    while let Some(contract_id) = queue.pop_front() {
        if requirements_deps.contains_key(&contract_id) {
            continue;
        }

        // Reuse a pre-loaded or prior-cycle AST instead of re-fetching.
        let (clarity_version, ast) = match requirements_data.remove(&contract_id) {
            Some(requirement_data) => requirement_data,
            None => {
                let (source, epoch, clarity_version, contract_location) =
                    requirements::retrieve_contract(
                        &contract_id,
                        &manifest.project.cache_location,
                        &file_accessor,
                        api_base_url,
                    )
                    .await?;

                contract_epochs.insert(contract_id.clone(), epoch);

                match network {
                    StacksNetwork::Simnet if !simnet_remote_data => {
                        emulated_contracts_publish.insert(
                            contract_id.clone(),
                            EmulatedContractPublishSpecification {
                                contract_name: contract_id.name.clone(),
                                emulated_sender: contract_id.issuer.clone(),
                                source: source.clone(),
                                location: contract_location,
                                clarity_version,
                                skip_analysis: true,
                            },
                        );
                    }
                    StacksNetwork::Devnet | StacksNetwork::Testnet => {
                        // sBTC ships under a mainnet address; remap to testnet on testnet.
                        let remap_target = if matches!(network, StacksNetwork::Testnet)
                            && contract_id.issuer.to_string() == SBTC_MAINNET_ADDRESS
                        {
                            SBTC_TESTNET_ADDRESS_PRINCIPAL.clone()
                        } else {
                            default_deployer_address.clone()
                        };
                        let remap_principals =
                            BTreeMap::from([(contract_id.issuer.clone(), remap_target.clone())]);
                        requirements_publish.insert(
                            contract_id.clone(),
                            RequirementPublishSpecification {
                                contract_id: contract_id.clone(),
                                remap_sender: remap_target,
                                source: source.clone(),
                                location: contract_location,
                                cost: deployment_fee_rate * source.len() as u64,
                                remap_principals,
                                clarity_version,
                            },
                        );
                    }
                    _ => {}
                }

                let contract = ClarityContract {
                    code_source: ClarityCodeSource::ContractInMemory(source),
                    name: contract_id.name.to_string(),
                    deployer: ContractDeployer::ContractIdentifier(contract_id.clone()),
                    clarity_version,
                    epoch: clarity_repl::repl::Epoch::Specific(epoch),
                    skip_analysis: true,
                };
                let (ast, _, _) = interpreter.build_ast(&contract);
                (clarity_version, ast)
            }
        };

        // `detect_dependencies` takes a map; wrap-then-unwrap our one AST.
        let mut contract_data = BTreeMap::new();
        contract_data.insert(contract_id.clone(), (clarity_version, ast));
        let dependencies =
            ASTDependencyDetector::detect_dependencies(&contract_data, requirements_data);
        let (_, ast) = contract_data
            .remove(&contract_id)
            .expect("unable to retrieve ast");

        match dependencies {
            Ok(inferable_dependencies) => {
                if inferable_dependencies.len() > 1 {
                    println!("warning: inferable_dependencies contains more than one entry");
                }
                // We submitted one contract, so at most one result.
                if let Some((contract_id, dependencies)) = inferable_dependencies.into_iter().next()
                {
                    for dependency in dependencies.iter() {
                        queue.push_back(dependency.contract_id.clone());
                    }
                    requirements_deps.insert(contract_id.clone(), dependencies);
                    requirements_data.insert(contract_id.clone(), (clarity_version, ast));
                }
            }
            Err((inferable_dependencies, non_inferable_dependencies)) => {
                // Re-enqueue current contract behind its unresolved deps,
                // keep source in memory to avoid re-downloading.
                for (_, dependencies) in inferable_dependencies.iter() {
                    for dependency in dependencies.iter() {
                        queue.push_back(dependency.contract_id.clone());
                    }
                }
                requirements_data.insert(contract_id.clone(), (clarity_version, ast));
                queue.push_front(contract_id);

                for non_inferable_contract_id in non_inferable_dependencies.into_iter() {
                    queue.push_front(non_inferable_contract_id);
                }
            }
        };
    }

    // Mainnet and simnet-with-remote-data: requirements are already on-chain.
    if matches!(network, StacksNetwork::Mainnet) || simnet_remote_data {
        return Ok(());
    }

    let mut ordered_contracts_ids =
        ASTDependencyDetector::order_contracts(requirements_deps, contract_epochs)
            .map_err(|e| format!("unable to order requirements {e}"))?;
    ordered_contracts_ids.retain(|contract_id| !boot_contracts_ids.contains(contract_id));

    match network {
        StacksNetwork::Simnet => {
            for contract_id in ordered_contracts_ids.iter() {
                let data = emulated_contracts_publish
                    .remove(contract_id)
                    .unwrap_or_else(|| panic!("unable to retrieve contract: {contract_id}"));
                transactions
                    .entry(contract_epochs[contract_id].into())
                    .or_default()
                    .push(TransactionSpecification::EmulatedContractPublish(data));
            }
        }
        StacksNetwork::Devnet | StacksNetwork::Testnet => {
            for contract_id in ordered_contracts_ids.iter() {
                let data = requirements_publish
                    .remove(contract_id)
                    .unwrap_or_else(|| panic!("unable to retrieve contract: {contract_id}"));
                transactions
                    .entry(contract_epochs[contract_id].into())
                    .or_default()
                    .push(TransactionSpecification::RequirementPublish(data));
            }
        }
        StacksNetwork::Mainnet => unreachable!("returned above"),
    }

    Ok(())
}

/// Parse custom boot-contract overrides; return per-id diagnostics for
/// any that failed. Non-boot names in the override table are skipped
/// (the table can also contain additions, which don't need validation).
async fn validate_custom_boot_contracts(
    override_boot_contracts_source: &BTreeMap<String, String>,
    manifest: &ProjectManifest,
    file_accessor: Option<&dyn FileAccessor>,
    interpreter: &ClarityInterpreter,
    default_deployer_address: &StandardPrincipalData,
) -> Result<HashMap<QualifiedContractIdentifier, Vec<Diagnostic>>, String> {
    let mut failures = HashMap::new();
    for (contract_name, file_path) in override_boot_contracts_source {
        if !clarity_repl::repl::boot::BOOT_CONTRACTS_NAMES.contains(&contract_name.as_str()) {
            continue;
        }

        let resolved_path = manifest.root_dir.join(file_path);
        let resolved_path_string = resolved_path.to_string_lossy().into_owned();

        let custom_source = match file_accessor {
            None => std::fs::read_to_string(&resolved_path_string).map_err(|e| {
                format!("Failed to read boot contract file {resolved_path_string}: {e}")
            })?,
            Some(file_accessor) => {
                let sources = file_accessor
                    .read_files(vec![resolved_path_string.clone()])
                    .await
                    .map_err(|e| {
                        format!("Failed to read boot contract file {resolved_path_string}: {e}")
                    })?;
                sources.get(&resolved_path_string).cloned().ok_or_else(|| {
                    format!("Unable to read custom boot contract: {contract_name}")
                })?
            }
        };

        let (epoch, clarity_version) =
            get_boot_contract_epoch_and_clarity_version(contract_name.as_str());

        let temp_contract = ClarityContract {
            code_source: ClarityCodeSource::ContractInMemory(custom_source),
            deployer: ContractDeployer::Address(default_deployer_address.to_address()),
            name: contract_name.clone(),
            clarity_version,
            epoch: clarity_repl::repl::Epoch::Specific(epoch),
            skip_analysis: false,
        };

        let (_, diagnostics, ast_success) = interpreter.build_ast(&temp_contract);

        if !ast_success {
            let contract_id = QualifiedContractIdentifier::new(
                default_deployer_address.clone(),
                ContractName::try_from(contract_name.clone())
                    .unwrap_or_else(|_| ContractName::from_literal("unknown")),
            );
            failures.insert(contract_id, diagnostics);
        }
    }
    Ok(failures)
}

/// Turn the network-manifest accounts into wallet specs for the simnet
/// genesis block.
fn build_simnet_wallets(
    accounts: BTreeMap<String, AccountConfig>,
) -> Result<Vec<WalletSpecification>, String> {
    accounts
        .into_iter()
        .map(|(name, account)| {
            let address = PrincipalData::parse_standard_principal(&account.stx_address)
                .map_err(|_| format!("unable to parse address {}", account.stx_address))?;
            Ok(WalletSpecification {
                name,
                address,
                balance: account.balance.into(),
                sbtc_balance: account.sbtc_balance.into(),
            })
        })
        .collect()
}

/// `SessionSettings` for the ephemeral interpreter. Custom boot
/// contracts only apply on simnet; warned-and-ignored otherwise.
fn build_session_settings(manifest: &ProjectManifest, network: &StacksNetwork) -> SessionSettings {
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

    SessionSettings {
        repl_settings,
        override_boot_contracts_source,
        ..Default::default()
    }
}

/// Seed `requirements_data` with boot-contract ASTs and return their
/// id set. No-op under simnet-with-remote-data (the node has them).
fn load_boot_contracts(
    settings: &SessionSettings,
    simnet_remote_data: bool,
    requirements_data: &mut BTreeMap<QualifiedContractIdentifier, (ClarityVersion, ContractAST)>,
) -> BTreeSet<QualifiedContractIdentifier> {
    if simnet_remote_data {
        return BTreeSet::new();
    }

    let boot_contracts_data = if settings.override_boot_contracts_source.is_empty() {
        BOOT_CONTRACTS_DATA.clone()
    } else {
        clarity_repl::repl::boot::get_boot_contracts_data_with_overrides(
            &settings.override_boot_contracts_source,
        )
    };

    let mut boot_contracts_ids = BTreeSet::new();
    for (id, (contract, ast)) in boot_contracts_data {
        boot_contracts_ids.insert(id.clone());
        requirements_data.insert(id, (contract.clarity_version, ast));
    }
    boot_contracts_ids
}

/// Topologically order project contracts and emit each as a publish
/// transaction in its epoch bucket. Requirements are skipped (already
/// emitted by `resolve_requirements`).
fn order_and_schedule_project_contracts(
    dependencies: &BTreeMap<QualifiedContractIdentifier, DependencySet>,
    contract_epochs: &HashMap<QualifiedContractIdentifier, StacksEpochId>,
    requirements_data: &BTreeMap<QualifiedContractIdentifier, (ClarityVersion, ContractAST)>,
    contracts: &mut HashMap<QualifiedContractIdentifier, TransactionSpecification>,
    transactions: &mut BTreeMap<EpochSpec, Vec<TransactionSpecification>>,
    contracts_map: &mut BTreeMap<QualifiedContractIdentifier, (String, PathBuf)>,
) -> Result<(), String> {
    let ordered_contracts_ids =
        ASTDependencyDetector::order_contracts(dependencies, contract_epochs)
            .map_err(|e| e.to_string())?;

    for contract_id in ordered_contracts_ids.into_iter() {
        if requirements_data.contains_key(contract_id) {
            continue;
        }
        let tx = contracts
            .remove(contract_id)
            .expect("unable to retrieve contract");

        let (source, location) = match &tx {
            TransactionSpecification::EmulatedContractPublish(data) => {
                (data.source.clone(), data.location.clone())
            }
            TransactionSpecification::ContractPublish(data) => {
                (data.source.clone(), data.location.clone())
            }
            _ => unreachable!(),
        };
        contracts_map.insert(contract_id.clone(), (source, location));
        transactions
            .entry(contract_epochs[contract_id].into())
            .or_default()
            .push(tx);
    }
    Ok(())
}

/// Load the network manifest via the accessor when one was supplied
/// (LSP, SDK) and from the local filesystem otherwise (CLI).
async fn load_network_manifest(
    manifest: &ProjectManifest,
    network: &StacksNetwork,
    file_accessor: Option<&dyn FileAccessor>,
) -> Result<NetworkManifest, String> {
    match file_accessor {
        None => NetworkManifest::from_project_root(
            &manifest.root_dir,
            &network.get_networks(),
            manifest.use_mainnet_wallets(),
            Some(&manifest.project.cache_location),
            None,
        ),
        Some(accessor) => {
            NetworkManifest::from_project_root_using_file_accessor(
                &manifest.root_dir,
                &network.get_networks(),
                manifest.use_mainnet_wallets(),
                accessor,
            )
            .await
        }
    }
}

struct ProjectContractSpecs {
    /// Per-id (contract, path) pair consumed by the AST-cache loop.
    contracts_sources: HashMap<QualifiedContractIdentifier, (ClarityContract, PathBuf)>,
    /// Publish transactions that end up in the deployment plan.
    contracts: HashMap<QualifiedContractIdentifier, TransactionSpecification>,
    /// True if `#[env(simnet)]` markers were stripped from any source.
    env_simnet_stripped: bool,
}

/// Builds the per-contract specs the AST-cache loop and deployment plan
/// both consume.
fn build_project_contract_specs(
    manifest: &ProjectManifest,
    network: &StacksNetwork,
    environment: Environment,
    accounts: &BTreeMap<String, AccountConfig>,
    default_deployer: &AccountConfig,
    deployment_fee_rate: u64,
    sources: &HashMap<String, String>,
) -> Result<ProjectContractSpecs, String> {
    let project_root = &manifest.root_dir;
    let mut contracts_sources = HashMap::new();
    let mut contracts = HashMap::new();
    let mut env_simnet_stripped = false;

    for (name, contract_config) in manifest.contracts.iter() {
        let Ok(contract_name) = ContractName::try_from(name.to_string()) else {
            return Err(format!("unable to use {name} as a valid contract name"));
        };

        let deployer = match &contract_config.deployer {
            ContractDeployer::DefaultDeployer => default_deployer,
            ContractDeployer::LabeledDeployer(deployer) => accounts
                .get(deployer)
                .ok_or_else(|| format!("unable to retrieve account '{deployer}'"))?,
            _ => unreachable!(),
        };

        let Ok(sender) = PrincipalData::parse_standard_principal(&deployer.stx_address) else {
            return Err(format!(
                "unable to turn emulated_sender {} as a valid Stacks address",
                deployer.stx_address
            ));
        };

        let contract_location = project_root.join(contract_config.expect_contract_path_as_str());
        let mut source = sources
            .get(contract_location.to_string_lossy().as_ref())
            .ok_or_else(|| format!("Invalid Clarinet.toml, source file not found for: {name}"))?
            .clone();

        if environment == Environment::OnChain {
            // Best effort: parse errors fall through to the later AST
            // build, which surfaces them with proper location info.
            if let Ok(Some(clean)) = remove_env_simnet(&source) {
                source = clean;
                env_simnet_stripped = true;
            }
        }

        let contract_id = QualifiedContractIdentifier::new(sender.clone(), contract_name.clone());
        let epoch = contract_config.epoch.resolve();

        contracts_sources.insert(
            contract_id.clone(),
            (
                ClarityContract {
                    code_source: ClarityCodeSource::ContractInMemory(source.clone()),
                    deployer: ContractDeployer::Address(sender.to_address()),
                    name: contract_name.to_string(),
                    clarity_version: contract_config.clarity_version,
                    epoch: clarity_repl::repl::Epoch::Specific(epoch),
                    skip_analysis: false,
                },
                contract_location.clone(),
            ),
        );

        let contract_spec = if matches!(network, StacksNetwork::Simnet) {
            TransactionSpecification::EmulatedContractPublish(
                EmulatedContractPublishSpecification {
                    contract_name,
                    emulated_sender: sender,
                    source,
                    location: contract_location,
                    clarity_version: contract_config.clarity_version,
                    skip_analysis: false,
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

    Ok(ProjectContractSpecs {
        contracts_sources,
        contracts,
        env_simnet_stripped,
    })
}

/// Load every contract source listed in the manifest, keyed by
/// absolute path (as a string). Uses the file accessor when supplied
/// (LSP, SDK) and the local filesystem otherwise (CLI).
async fn load_project_contract_sources(
    manifest: &ProjectManifest,
    file_accessor: Option<&dyn FileAccessor>,
) -> Result<HashMap<String, String>, String> {
    let project_root = &manifest.root_dir;
    let contract_paths: Vec<String> = manifest
        .contracts
        .values()
        .map(|cfg| {
            project_root
                .join(cfg.expect_contract_path_as_str())
                .to_string_lossy()
                .into_owned()
        })
        .collect();

    match file_accessor {
        Some(accessor) => accessor.read_files(contract_paths).await,
        None => contract_paths
            .into_iter()
            .map(|path| {
                let source = paths::read_content_as_utf8(Path::new(&path))
                    .map_err(|_| format!("unable to find contract at {path}"))?;
                Ok((path, source))
            })
            .collect(),
    }
}

/// Flatten the per-epoch transaction map into a list of batches.
/// Batch ids are assigned in iteration order across epochs.
fn chunk_transactions_into_batches(
    transactions: BTreeMap<EpochSpec, Vec<TransactionSpecification>>,
    batching: BatchingMode,
) -> Vec<TransactionsBatchSpecification> {
    let mut batches = Vec::new();
    for (epoch, epoch_transactions) in transactions {
        for txs in batching.chunks(&epoch_transactions) {
            batches.push(TransactionsBatchSpecification {
                id: batches.len(),
                transactions: txs.to_vec(),
                epoch: Some(epoch),
            });
        }
    }
    batches
}

/// (stacks-node, bitcoin-node) RPC URLs for `network`. `(None, None)`
/// for simnet; otherwise the manifest value or a well-known default.
fn resolve_node_endpoints(
    network: &StacksNetwork,
    devnet: Option<&DevnetConfig>,
    net: NetworkConfig,
) -> (Option<String>, Option<String>) {
    let (stacks_default, bitcoin_default) = match network {
        StacksNetwork::Simnet => return (None, None),
        StacksNetwork::Devnet => {
            let (stacks, bitcoin) = match devnet {
                Some(d) => (
                    format!("http://localhost:{}", d.stacks_node_rpc_port),
                    format!(
                        "http://{}:{}@localhost:{}",
                        d.bitcoin_node_username, d.bitcoin_node_password, d.bitcoin_node_rpc_port
                    ),
                ),
                None => (
                    "http://localhost:20443".to_string(),
                    "http://devnet:devnet@localhost:18443".to_string(),
                ),
            };
            return (Some(stacks), Some(bitcoin));
        }
        StacksNetwork::Testnet => (
            "https://api.testnet.hiro.so",
            "http://blockstack:blockstacksystem@bitcoind.testnet.stacks.co:18332",
        ),
        StacksNetwork::Mainnet => (
            "https://api.hiro.so",
            "http://blockstack:blockstacksystem@bitcoin.blockstack.com:8332",
        ),
    };
    (
        Some(
            net.stacks_node_rpc_address
                .unwrap_or_else(|| stacks_default.to_string()),
        ),
        Some(
            net.bitcoin_node_rpc_address
                .unwrap_or_else(|| bitcoin_default.to_string()),
        ),
    )
}

pub async fn generate_default_deployment(
    manifest: &ProjectManifest,
    network: &StacksNetwork,
    batching: BatchingMode,
    file_accessor: Option<&dyn FileAccessor>,
    api_base_url: Option<&str>,
    environment: Environment,
) -> Result<(DeploymentSpecification, DeploymentGenerationArtifacts, bool), String> {
    generate_default_deployment_with_cache(
        manifest,
        network,
        batching,
        file_accessor,
        api_base_url,
        environment,
        None,
    )
    .await
}

pub async fn generate_default_deployment_with_cache(
    manifest: &ProjectManifest,
    network: &StacksNetwork,
    batching: BatchingMode,
    file_accessor: Option<&dyn FileAccessor>,
    api_base_url: Option<&str>,
    environment: Environment,
    // Taken by `&mut` so hit entries can be *removed* and reused directly in
    // the returned `ast_cache_entries`, avoiding a per-hit `ContractAST.clone()`.
    // Leftover entries (misses, hash mismatches, stale contracts) are dropped
    // by the caller when the HashMap goes out of scope.
    cached_asts: Option<&mut HashMap<(PathBuf, Environment), CachedContractAST>>,
) -> Result<(DeploymentSpecification, DeploymentGenerationArtifacts, bool), String> {
    let mut found_env_simnet = false;
    let NetworkManifest {
        network: net,
        accounts,
        devnet,
    } = load_network_manifest(manifest, network, file_accessor).await?;

    let deployment_fee_rate = net.deployment_fee_rate;
    let (stacks_node, bitcoin_node) = resolve_node_endpoints(network, devnet.as_ref(), net);

    let Some(default_deployer) = accounts.get("deployer") else {
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

    let mut transactions: BTreeMap<EpochSpec, Vec<TransactionSpecification>> = BTreeMap::new();
    let mut contracts_map = BTreeMap::new();
    let mut requirements_data = BTreeMap::new();
    let mut requirements_deps = BTreeMap::new();

    let settings = build_session_settings(manifest, network);
    let simnet_remote_data =
        matches!(network, StacksNetwork::Simnet) && manifest.repl_settings.remote_data.enabled;
    let boot_contracts_ids =
        load_boot_contracts(&settings, simnet_remote_data, &mut requirements_data);

    // this ephemeral interpreter is used to parse code and build ASTs
    let interpreter = ClarityInterpreter::new(
        settings.get_default_sender(),
        settings.repl_settings.clone(),
        settings.cache_location.clone(),
    );

    // Initialize diagnostics collection and success tracking early
    let mut contract_diags: HashMap<QualifiedContractIdentifier, Vec<Diagnostic>> = HashMap::new();
    let mut asts_success = true;

    if !settings.override_boot_contracts_source.is_empty() && !simnet_remote_data {
        let failures = validate_custom_boot_contracts(
            &settings.override_boot_contracts_source,
            manifest,
            file_accessor,
            &interpreter,
            &default_deployer_address,
        )
        .await?;
        if !failures.is_empty() {
            asts_success = false;
            contract_diags.extend(failures);
        }
    }

    let mut contract_epochs = HashMap::new();

    if manifest.project.requirements.is_some() {
        resolve_requirements(
            manifest,
            network,
            simnet_remote_data,
            file_accessor,
            api_base_url,
            &interpreter,
            &default_deployer_address,
            deployment_fee_rate,
            &boot_contracts_ids,
            &mut requirements_data,
            &mut requirements_deps,
            &mut contract_epochs,
            &mut transactions,
        )
        .await?;
    }

    let sources = load_project_contract_sources(manifest, file_accessor).await?;
    let ProjectContractSpecs {
        contracts_sources,
        mut contracts,
        env_simnet_stripped,
    } = build_project_contract_specs(
        manifest,
        network,
        environment,
        &accounts,
        default_deployer,
        deployment_fee_rate,
        &sources,
    )?;
    found_env_simnet |= env_simnet_stripped;

    let session = Session::new(settings);

    let mut contract_data = BTreeMap::new();
    // `Some` iff the caller opted into caching. On any early return /
    // `?` / panic below, dropping the `Some(guard)` pushes the entries
    // we've built back into the caller's input cache — so a cycle error
    // in `order_contracts` or a malformed wallet address can't cost us
    // a cold rebuild. We call `.commit()` on the happy path.
    let mut cache_guard = cached_asts.map(AstCacheRestoreGuard::new);

    for (contract_id, (contract, contract_location)) in contracts_sources {
        let clarity_version = contract.clarity_version;
        let resolved_epoch = contract.epoch.resolve();
        let (ast, diags, ok) = build_or_reuse_project_ast(
            &contract,
            contract_location,
            environment,
            &session.interpreter,
            cache_guard.as_mut(),
        );
        contract_diags.insert(contract_id.clone(), diags);
        asts_success = asts_success && ok;
        contract_data.insert(contract_id.clone(), (clarity_version, ast));
        contract_epochs.insert(contract_id, resolved_epoch);
    }

    // Failures are intentionally not reported here — later analyses
    // surface them with proper location information.
    let mut dependencies =
        ASTDependencyDetector::detect_dependencies(&contract_data, &requirements_data)
            .unwrap_or_else(|(deps, _)| deps);

    // `contract_data` is no longer borrowed; consume it for the final
    // `asts` output so we don't clone each AST a second time.
    let contract_asts: BTreeMap<_, _> = contract_data
        .into_iter()
        .map(|(id, (_version, ast))| (id, ast))
        .collect();

    for contract_id in boot_contracts_ids {
        dependencies.insert(contract_id, DependencySet::new());
    }
    dependencies.extend(requirements_deps);

    // Post-loop errors are safe to `?` through: `cache_guard` is still
    // live and its `Drop` will restore `pending` into `cached_asts`.
    order_and_schedule_project_contracts(
        &dependencies,
        &contract_epochs,
        &requirements_data,
        &mut contracts,
        &mut transactions,
        &mut contracts_map,
    )?;

    let batches = chunk_transactions_into_batches(transactions, batching);

    let wallets = if matches!(network, StacksNetwork::Simnet) {
        build_simnet_wallets(accounts)?
    } else {
        Vec::new()
    };

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

    // Disarm the guard (if any) — we're committed to returning Ok, so
    // the accumulated entries belong in the artifacts, not back in the
    // caller's input cache. `None` carries through for cache-free callers
    // so they're structurally distinguishable from "opted in, zero hits".
    let ast_cache_entries = cache_guard.map(AstCacheRestoreGuard::commit);

    let artifacts = DeploymentGenerationArtifacts {
        asts: contract_asts,
        deps: dependencies,
        diags: contract_diags,
        lint_diags: HashMap::new(),
        success: asts_success,
        results_values: HashMap::new(),
        analysis: HashMap::new(),
        session,
        ast_cache_entries,
    };

    Ok((deployment, artifacts, found_env_simnet))
}

pub fn get_default_deployment_path(network: &StacksNetwork) -> &'static str {
    match network {
        StacksNetwork::Simnet => "deployments/default.simnet-plan.yaml",
        StacksNetwork::Devnet => "deployments/default.devnet-plan.yaml",
        StacksNetwork::Testnet => "deployments/default.testnet-plan.yaml",
        StacksNetwork::Mainnet => "deployments/default.mainnet-plan.yaml",
    }
}

pub fn load_deployment(
    project_root: &Path,
    deployment_plan_path: &Path,
) -> Result<DeploymentSpecification, String> {
    DeploymentSpecification::from_config_file(deployment_plan_path, project_root).map_err(|err| {
        format!(
            "error: {} syntax incorrect\n{err}",
            deployment_plan_path.display()
        )
    })
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

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
    ) -> Result<AnnotatedExecutionResult, Vec<Diagnostic>> {
        let emulated_publish_spec = EmulatedContractPublishSpecification {
            contract_name: ContractName::try_from(name).unwrap(),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            source: source.to_string(),
            clarity_version: ClarityVersion::Clarity2,
            location: PathBuf::from("/contracts/contract_1.clar"),
            skip_analysis: false,
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
            ContractName::from_literal("contract_1"),
        );

        let contract_call_spec = EmulatedContractCallSpecification {
            contract_id: contract_id.clone(),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            method: ClarityName::from_literal("add"),
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
            ContractName::from_literal("contract_1"),
        );

        let contract_call_spec = EmulatedContractCallSpecification {
            contract_id: contract_id.clone(),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            method: ClarityName::from_literal("set-sum"),
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
            ContractName::from_literal("contract_1"),
        );

        let contract_call_spec = EmulatedContractCallSpecification {
            contract_id: contract_id.clone(),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            method: ClarityName::from_literal("set-data"),
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

    /// Contract source that triggers a lint warning (unused constant)
    /// when analysis is enabled with default lints.
    const LINT_TRIGGERING_SOURCE: &str = "(define-constant UNUSED_CONST u42)";

    /// Deploying a contract with `skip_analysis: false` and default lints enabled
    /// should produce lint diagnostics (e.g., "never used").
    #[test]
    fn deploy_with_analysis_produces_lint_diagnostics() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        let epoch = StacksEpochId::Epoch25;
        session.update_epoch(epoch);

        // Enable default lints on the session so analysis actually runs
        session.interpreter.repl_settings.analysis =
            clarity_repl::analysis::Settings::from(clarity_repl::analysis::SettingsFile::default());

        let spec = EmulatedContractPublishSpecification {
            contract_name: ContractName::from_literal("lint-test"),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            source: LINT_TRIGGERING_SOURCE.to_string(),
            clarity_version: ClarityVersion::Clarity2,
            location: PathBuf::from("/contracts/lint-test.clar"),
            skip_analysis: false,
        };

        let result = handle_emulated_contract_publish(&mut session, &spec, None, epoch);
        let annotated = result.expect("contract should deploy successfully");
        assert!(
            annotated
                .lint_diagnostics
                .iter()
                .any(|ld| ld.diagnostic.message.contains("never used")),
            "expected 'never used' lint warning, got: {:?}",
            annotated.lint_diagnostics
        );
    }

    /// Deploying a contract with `skip_analysis: true` should produce NO lint
    /// diagnostics even when the session has lints enabled.
    #[test]
    fn deploy_with_skip_analysis_produces_no_lint_diagnostics() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        let epoch = StacksEpochId::Epoch25;
        session.update_epoch(epoch);

        // Enable default lints — but skip_analysis on the contract overrides
        session.interpreter.repl_settings.analysis =
            clarity_repl::analysis::Settings::from(clarity_repl::analysis::SettingsFile::default());

        let spec = EmulatedContractPublishSpecification {
            contract_name: ContractName::from_literal("lint-test"),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            source: LINT_TRIGGERING_SOURCE.to_string(),
            clarity_version: ClarityVersion::Clarity2,
            location: PathBuf::from("/contracts/lint-test.clar"),
            skip_analysis: true,
        };

        let result = handle_emulated_contract_publish(&mut session, &spec, None, epoch);
        let execution_result = result.expect("contract should deploy successfully");
        let lint_diags: Vec<_> = execution_result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("never used"))
            .collect();
        assert!(
            lint_diags.is_empty(),
            "expected no lint diagnostics with skip_analysis=true, got: {lint_diags:?}"
        );
    }

    /// When analysis is globally disabled via `disable_all()` (as done by
    /// `setup_session_with_deployment` when `enable_analysis=false`),
    /// deploying a contract should produce NO lint diagnostics even when
    /// `skip_analysis` is false on the contract itself — because the session
    /// has no lints/passes configured.
    #[test]
    fn deploy_with_analysis_globally_disabled_produces_no_diagnostics() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        let epoch = StacksEpochId::Epoch25;
        session.update_epoch(epoch);

        // Enable default lints, then disable them globally
        // (this is what setup_session_with_deployment does when enable_analysis=false)
        session.interpreter.repl_settings.analysis =
            clarity_repl::analysis::Settings::from(clarity_repl::analysis::SettingsFile::default());
        session.interpreter.repl_settings.analysis.disable_all();

        let spec = EmulatedContractPublishSpecification {
            contract_name: ContractName::from_literal("lint-test"),
            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            source: LINT_TRIGGERING_SOURCE.to_string(),
            clarity_version: ClarityVersion::Clarity2,
            location: PathBuf::from("/contracts/lint-test.clar"),
            skip_analysis: false, // Even with skip_analysis=false, disable_all() prevents lints
        };

        let result = handle_emulated_contract_publish(&mut session, &spec, None, epoch);
        let execution_result = result.expect("contract should deploy successfully");
        let lint_diags: Vec<_> = execution_result
            .diagnostics
            .iter()
            .filter(|d| d.message.contains("never used"))
            .collect();
        assert!(
            lint_diags.is_empty(),
            "expected no lint diagnostics after disable_all(), got: {lint_diags:?}"
        );
    }

    /// Create a session with default lints enabled and a deployment containing
    /// a lint-triggering contract, then run `update_session_with_deployment_plan`.
    fn deploy_lint_triggering_contract_via_deployment_plan(
        disable_analysis: bool,
    ) -> ExecutionResultMap {
        let mut session = Session::new(SessionSettings::default());
        let epoch = StacksEpochId::Epoch25;
        session.update_epoch(epoch);

        session.interpreter.repl_settings.analysis =
            clarity_repl::analysis::Settings::from(clarity_repl::analysis::SettingsFile::default());
        if disable_analysis {
            session.interpreter.repl_settings.analysis.disable_all();
        }

        let deployment = DeploymentSpecification {
            id: 0,
            name: "test".to_string(),
            network: StacksNetwork::Simnet,
            stacks_node: None,
            bitcoin_node: None,
            genesis: None,
            contracts: BTreeMap::new(),
            plan: TransactionPlanSpecification {
                batches: vec![TransactionsBatchSpecification {
                    id: 0,
                    transactions: vec![TransactionSpecification::EmulatedContractPublish(
                        EmulatedContractPublishSpecification {
                            contract_name: ContractName::from_literal("lint-test"),
                            emulated_sender: PrincipalData::parse_standard_principal(DEPLOYER)
                                .unwrap(),
                            source: LINT_TRIGGERING_SOURCE.to_string(),
                            clarity_version: ClarityVersion::Clarity2,
                            location: PathBuf::from("/contracts/lint-test.clar"),
                            skip_analysis: false,
                        },
                    )],
                    epoch: Some(EpochSpec::Epoch2_4),
                }],
            },
        };

        update_session_with_deployment_plan(&mut session, &deployment, None)
    }

    /// End-to-end test: `update_session_with_deployment_plan` with analysis
    /// disabled should produce no lint diagnostics in execution results.
    /// This simulates the `clarinet console` and `npm test` codepaths.
    #[test]
    fn update_session_no_lint_diagnostics_when_analysis_disabled() {
        let results = deploy_lint_triggering_contract_via_deployment_plan(true);

        for (contract_id, result) in results {
            match result {
                Ok(execution_result) => {
                    let lint_diags: Vec<_> = execution_result
                        .diagnostics
                        .iter()
                        .filter(|d| d.message.contains("never used"))
                        .collect();
                    assert!(
                        lint_diags.is_empty(),
                        "contract {contract_id} should have no lint diagnostics, got: {lint_diags:?}"
                    );
                }
                Err(diags) => panic!("contract {contract_id} deployment failed: {diags:?}"),
            }
        }
    }

    /// Counterpart: with default lints enabled and skip_analysis=false,
    /// `update_session_with_deployment_plan` DOES produce lint diagnostics.
    #[test]
    fn update_session_produces_lint_diagnostics_when_analysis_enabled() {
        let results = deploy_lint_triggering_contract_via_deployment_plan(false);

        let has_lint_warning = results.into_iter().any(|(_, result)| {
            result
                .map(|r| {
                    r.lint_diagnostics
                        .iter()
                        .any(|ld| ld.diagnostic.message.contains("never used"))
                })
                .unwrap_or(false)
        });
        assert!(
            has_lint_warning,
            "expected lint diagnostics when analysis is enabled"
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

    // -------- AstCacheRestoreGuard unit tests --------
    //
    // Cover the two branches directly: an uncommitted `Drop` must push
    // `pending` back into the input cache, and `commit()` must hand the
    // entries to the caller while leaving the input cache empty.

    fn fake_cached_ast() -> CachedContractAST {
        let session = Session::new(SessionSettings::default());
        let source = "(define-data-var x uint u0)";
        let contract = ClarityContract {
            code_source: ClarityCodeSource::ContractInMemory(source.to_string()),
            deployer: ContractDeployer::Address(DEPLOYER.to_string()),
            name: "dummy".to_string(),
            clarity_version: ClarityVersion::Clarity3,
            epoch: clarity_repl::repl::Epoch::Specific(StacksEpochId::Epoch31),
            skip_analysis: false,
        };
        let (ast, diags, ast_success) = session.interpreter.build_ast(&contract);
        CachedContractAST::new(
            source,
            ast,
            diags,
            ast_success,
            ClarityVersion::Clarity3,
            StacksEpochId::Epoch31,
        )
    }

    #[test]
    fn ast_cache_restore_guard_drops_pending_into_input_on_uncommitted_drop() {
        let mut input = HashMap::new();
        let key = (PathBuf::from("/contracts/test.clar"), Environment::OnChain);

        {
            let mut guard = AstCacheRestoreGuard::new(&mut input);
            guard.insert(key.clone(), fake_cached_ast());
            // Intentionally drop without `.commit()`.
        }

        assert_eq!(
            input.len(),
            1,
            "Drop without commit should push pending entries back into input"
        );
        assert!(input.contains_key(&key));
    }

    #[test]
    fn ast_cache_restore_guard_commit_yields_pending_and_leaves_input_empty() {
        let mut input = HashMap::new();
        let key = (PathBuf::from("/contracts/test.clar"), Environment::OnChain);

        let committed = {
            let mut guard = AstCacheRestoreGuard::new(&mut input);
            guard.insert(key.clone(), fake_cached_ast());
            guard.commit()
        };

        assert_eq!(
            committed.len(),
            1,
            "commit() should hand the caller the accumulated entries"
        );
        assert!(committed.contains_key(&key));
        assert!(
            input.is_empty(),
            "committed entries must not leak back into the input cache"
        );
    }
}
