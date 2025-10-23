use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;

use clarity::consts::{CHAIN_ID_MAINNET, CHAIN_ID_TESTNET};
use clarity::types::StacksEpochId;
use clarity::vm::analysis::{AnalysisDatabase, ContractAnalysis};
use clarity::vm::ast::{build_ast_with_diagnostics, ContractAST};
use clarity::vm::contexts::{CallStack, ContractContext, Environment, GlobalContext, LocalContext};
use clarity::vm::contracts::Contract;
use clarity::vm::costs::{ExecutionCost, LimitedCostTracker};
use clarity::vm::database::{ClarityBackingStore, ClarityDatabase, StoreType};
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::events::*;
use clarity::vm::representations::SymbolicExpressionType::{Atom, List};
use clarity::vm::representations::{Span, SymbolicExpression};
use clarity::vm::{
    eval, eval_all, ClarityVersion, ContractEvaluationResult, CostSynthesis, EvalHook,
    EvaluationResult, ExecutionResult, ParsedContract, SnippetEvaluationResult,
};
use clarity_types::types::{PrincipalData, QualifiedContractIdentifier, StandardPrincipalData};
use clarity_types::Value;

use super::datastore::StacksConstants;
use super::remote_data::HttpClient;
use super::settings::{ApiUrl, RemoteNetworkInfo};
use super::{ClarityContract, DEFAULT_EPOCH};
use crate::analysis::annotation::{Annotation, AnnotationKind};
use crate::analysis::ast_dependency_detector::{ASTDependencyDetector, Dependency};
use crate::analysis::{self};
use crate::repl::datastore::{ClarityDatastore, Datastore};
use crate::repl::Settings;

#[derive(Debug, Clone)]
pub enum ContractCallError {
    NoSuchContract(String),
    NoSuchFunction(String),
    Uncategorized(String),
}

impl From<String> for ContractCallError {
    fn from(s: String) -> Self {
        if s.contains("UndefinedFunction") || s.contains("NoSuchPublicFunction") {
            ContractCallError::NoSuchFunction(s)
        } else if s.contains("Failed to read non-consensus contract metadata") {
            ContractCallError::NoSuchContract(s)
        } else {
            ContractCallError::Uncategorized(s)
        }
    }
}

impl std::fmt::Display for ContractCallError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ContractCallError::NoSuchContract(contract_id) => {
                write!(f, "NoSuchContract({contract_id})")
            }
            ContractCallError::NoSuchFunction(function_name) => {
                write!(f, "NoSuchFunction({function_name})")
            }
            ContractCallError::Uncategorized(message) => {
                write!(f, "Uncategorized({message})")
            }
        }
    }
}

impl std::error::Error for ContractCallError {}

pub const BLOCK_LIMIT_MAINNET: ExecutionCost = ExecutionCost {
    write_length: 15_000_000,
    write_count: 15_000,
    read_length: 100_000_000,
    read_count: 15_000,
    runtime: 5_000_000_000,
};

#[derive(Clone)]
pub struct ClarityInterpreter {
    pub clarity_datastore: ClarityDatastore,
    pub datastore: Datastore,
    pub repl_settings: Settings,
    remote_network_info: Option<RemoteNetworkInfo>,
    tx_sender: StandardPrincipalData,
    accounts: BTreeSet<String>,
    tokens: BTreeMap<String, BTreeMap<String, u128>>,
}

#[derive(Debug)]
pub struct Txid(pub [u8; 32]);

impl ClarityInterpreter {
    pub fn new(
        tx_sender: StandardPrincipalData,
        repl_settings: Settings,
        cache_location: Option<PathBuf>,
    ) -> Self {
        let remote_data_settings = repl_settings.remote_data.clone();

        let client = HttpClient::new(ApiUrl(remote_data_settings.api_url.to_string()));
        let remote_network_info = if remote_data_settings.enabled {
            Some(
                remote_data_settings
                    .get_initial_remote_network_info(&client, cache_location)
                    .unwrap(),
            )
        } else {
            None
        };

        let clarity_datastore = ClarityDatastore::new(remote_network_info.clone(), client.clone());
        let datastore = Datastore::new(&clarity_datastore, StacksConstants::default(), client);

        Self {
            tx_sender,
            repl_settings,
            remote_network_info,
            clarity_datastore,
            datastore,
            accounts: BTreeSet::new(),
            tokens: BTreeMap::new(),
        }
    }

    pub fn run(
        &mut self,
        contract: &ClarityContract,
        cached_ast: Option<&ContractAST>,
        cost_track: bool,
        eval_hooks: Option<Vec<&mut dyn EvalHook>>,
    ) -> Result<ExecutionResult, Vec<Diagnostic>> {
        let (ast, mut diagnostics, success) = match cached_ast {
            Some(ast) => (ast.clone(), vec![], true),
            None => self.build_ast(contract),
        };

        let code_source = contract.expect_in_memory_code_source();

        let (annotations, mut annotation_diagnostics) = self.collect_annotations(code_source);
        diagnostics.append(&mut annotation_diagnostics);

        let (analysis, mut analysis_diagnostics) =
            match self.run_analysis(contract, &ast, &annotations) {
                Ok((analysis, diagnostics)) => (analysis, diagnostics),
                Err(diagnostic) => {
                    diagnostics.push(diagnostic);
                    return Err(diagnostics.to_vec());
                }
            };
        diagnostics.append(&mut analysis_diagnostics);

        if !success {
            return Err(diagnostics.to_vec());
        }

        let mut result = match self.execute(contract, &ast, analysis, cost_track, eval_hooks) {
            Ok(result) => result,
            Err(e) => {
                diagnostics.push(Diagnostic {
                    level: Level::Error,
                    message: format!("Runtime Error: {e}"),
                    spans: vec![],
                    suggestion: None,
                });
                return Err(diagnostics.to_vec());
            }
        };

        result.diagnostics = diagnostics.to_vec();
        Ok(result)
    }

    pub fn detect_dependencies(
        &mut self,
        contract: &ClarityContract,
    ) -> Result<Vec<Dependency>, String> {
        let contract_id = contract.expect_resolved_contract_identifier(Some(&self.tx_sender));
        let (ast, _, success) = self.build_ast(contract);
        if !success {
            return Err("error parsing source".to_string());
        }

        let mut contract_map = BTreeMap::new();
        contract_map.insert(contract_id.clone(), (contract.clarity_version, ast));
        let mut all_dependencies =
            match ASTDependencyDetector::detect_dependencies(&contract_map, &BTreeMap::new()) {
                Ok(dependencies) => dependencies,
                Err((_, unresolved)) => {
                    return Err(format!(
                        "unresolved dependency(ies): {}",
                        unresolved
                            .iter()
                            .map(|contract_id| contract_id.to_string())
                            .collect::<Vec<String>>()
                            .join(",")
                    ));
                }
            };
        let mut dependencies = vec![];
        if let Some(dependencies_set) = all_dependencies.remove(&contract_id) {
            dependencies.extend(dependencies_set.set);
        }
        Ok(dependencies)
    }

    pub fn build_ast(&self, contract: &ClarityContract) -> (ContractAST, Vec<Diagnostic>, bool) {
        let source_code = contract.expect_in_memory_code_source();
        let contract_id = contract.expect_resolved_contract_identifier(Some(&self.tx_sender));
        build_ast_with_diagnostics(
            &contract_id,
            source_code,
            &mut (),
            contract.clarity_version,
            contract.epoch.resolve(),
        )
    }

    pub fn collect_annotations(&self, code_source: &str) -> (Vec<Annotation>, Vec<Diagnostic>) {
        let mut annotations = vec![];
        let mut diagnostics = vec![];
        for (n, line) in code_source.lines().enumerate() {
            if let Some(comment) = line.trim().strip_prefix(";;") {
                if let Some(annotation_string) = comment.trim().strip_prefix("#[") {
                    let span = Span {
                        start_line: (n + 1) as u32,
                        start_column: (line.find('#').unwrap_or(0) + 1) as u32,
                        end_line: (n + 1) as u32,
                        end_column: line.len() as u32,
                    };
                    if let Some(annotation_string) = annotation_string.strip_suffix(']') {
                        let kind: AnnotationKind = match annotation_string.trim().parse() {
                            Ok(kind) => kind,
                            Err(e) => {
                                diagnostics.push(Diagnostic {
                                    level: Level::Warning,
                                    message: e.to_string(),
                                    spans: vec![span.clone()],
                                    suggestion: None,
                                });
                                continue;
                            }
                        };
                        annotations.push(Annotation { kind, span });
                    } else {
                        diagnostics.push(Diagnostic {
                            level: Level::Warning,
                            message: "malformed annotation".to_string(),
                            spans: vec![span],
                            suggestion: None,
                        });
                    }
                }
            }
        }
        (annotations, diagnostics)
    }

    pub fn run_analysis(
        &mut self,
        contract: &ClarityContract,
        contract_ast: &ContractAST,
        annotations: &Vec<Annotation>,
    ) -> Result<(ContractAnalysis, Vec<Diagnostic>), Diagnostic> {
        let mut analysis_db = AnalysisDatabase::new(&mut self.clarity_datastore);

        // Run standard clarity analyses
        let mut contract_analysis = clarity::vm::analysis::run_analysis(
            &contract.expect_resolved_contract_identifier(Some(&self.tx_sender)),
            &contract_ast.expressions,
            &mut analysis_db,
            false,
            LimitedCostTracker::new_free(),
            contract.epoch.resolve(),
            contract.clarity_version,
            true,
        )
        .map_err(|boxed_error| boxed_error.0.diagnostic)?;

        // Run REPL-only analyses
        let diagnostics = analysis::run_analysis(
            &mut contract_analysis,
            &mut analysis_db,
            annotations,
            &self.repl_settings.analysis,
        )
        .map_err(|mut diagnostics| diagnostics.pop().unwrap())?;

        Ok((contract_analysis, diagnostics))
    }

    pub fn get_block_time(&mut self) -> u64 {
        let block_height = self.get_block_height();
        let mut conn = ClarityDatabase::new(
            &mut self.clarity_datastore,
            &self.datastore,
            &self.datastore,
        );
        conn.get_block_time(block_height)
            .expect("unable to get block time")
    }

    pub fn get_data_var(
        &mut self,
        contract_id: &QualifiedContractIdentifier,
        var_name: &str,
    ) -> Option<String> {
        let key = ClarityDatabase::make_key_for_trip(contract_id, StoreType::Variable, var_name);
        let value_hex = self
            .clarity_datastore
            .get_data(&key)
            .expect("failed to get key from datastore")?;
        Some(format!("0x{value_hex}"))
    }

    pub fn get_map_entry(
        &mut self,
        contract_id: &QualifiedContractIdentifier,
        map_name: &str,
        map_key: &Value,
    ) -> Option<String> {
        let key =
            ClarityDatabase::make_key_for_data_map_entry(contract_id, map_name, map_key).unwrap();
        let value_hex = self
            .clarity_datastore
            .get_data(&key)
            .expect("failed to get map entry from datastore")?;
        Some(format!("0x{value_hex}"))
    }

    fn get_global_context(
        &'_ mut self,
        epoch: StacksEpochId,
        cost_track: bool,
    ) -> Result<GlobalContext<'_, '_>, String> {
        let is_mainnet = self
            .remote_network_info
            .as_ref()
            .is_some_and(|data| data.is_mainnet);
        let chain_id = if is_mainnet {
            CHAIN_ID_MAINNET
        } else {
            CHAIN_ID_TESTNET
        };

        let mut conn = ClarityDatabase::new(
            &mut self.clarity_datastore,
            &self.datastore,
            &self.datastore,
        );
        conn.begin();
        conn.set_clarity_epoch_version(epoch)
            .map_err(|e| e.to_string())?;
        conn.commit().map_err(|e| e.to_string())?;
        let cost_tracker = if cost_track {
            LimitedCostTracker::new(
                is_mainnet,
                chain_id,
                BLOCK_LIMIT_MAINNET.clone(),
                &mut conn,
                epoch,
            )
            .map_err(|e| format!("failed to initialize cost tracker: {e}"))?
        } else {
            LimitedCostTracker::new_free()
        };

        Ok(GlobalContext::new(
            is_mainnet,
            chain_id,
            conn,
            cost_tracker,
            epoch,
        ))
    }

    fn execute(
        &mut self,
        contract: &ClarityContract,
        contract_ast: &ContractAST,
        analysis: ContractAnalysis,
        cost_track: bool,
        eval_hooks: Option<Vec<&mut dyn EvalHook>>,
    ) -> Result<ExecutionResult, String> {
        let contract_id = contract.expect_resolved_contract_identifier(Some(&self.tx_sender));
        let snippet = contract.expect_in_memory_code_source();
        let mut contract_context =
            ContractContext::new(contract_id.clone(), contract.clarity_version);

        #[cfg(not(target_arch = "wasm32"))]
        let show_timings = self.repl_settings.show_timings;

        let tx_sender: PrincipalData = self.tx_sender.clone().into();

        let mut global_context = self.get_global_context(contract.epoch.resolve(), cost_track)?;

        if let Some(in_hooks) = eval_hooks {
            let mut hooks: Vec<&mut dyn EvalHook> = Vec::new();
            for hook in in_hooks {
                hooks.push(hook);
            }
            global_context.eval_hooks = Some(hooks);
        }

        global_context.begin();
        let result = global_context.execute(|g| {
            if contract_ast.expressions.len() == 1 && !snippet.contains("(define-") {
                let context = LocalContext::new();
                let mut call_stack = CallStack::new();
                let mut env = Environment::new(
                    g,
                    &contract_context,
                    &mut call_stack,
                    Some(tx_sender.clone()),
                    Some(tx_sender.clone()),
                    None,
                );

                // call a function
                if let List(expression) = &contract_ast.expressions[0].expr {
                    if let Atom(name) = &expression[0].expr {
                        if name.to_string() == "contract-call?" {
                            let Ok(PrincipalData::Contract(contract_id)) = expression[1]
                                .match_literal_value()
                                .unwrap()
                                .clone()
                                .expect_principal()
                            else {
                                unreachable!();
                            };
                            let method = expression[2].match_atom().unwrap().to_string();
                            let mut args = vec![];
                            for arg in expression[3..].iter() {
                                let evaluated_arg = eval(arg, &mut env, &context)?;
                                args.push(evaluated_arg);
                            }

                            #[cfg(not(target_arch = "wasm32"))]
                            let start = std::time::Instant::now();

                            let args: Vec<SymbolicExpression> = args
                                .iter()
                                .map(|a| SymbolicExpression::atom_value(a.clone()))
                                .collect();
                            let res = env.execute_contract(&contract_id, &method, &args, false)?;

                            #[cfg(not(target_arch = "wasm32"))]
                            if show_timings {
                                println!("execution time: {:?}", start.elapsed());
                            }

                            return Ok(Some(res));
                        }
                    }
                };

                #[cfg(not(target_arch = "wasm32"))]
                let start = std::time::Instant::now();

                let result = eval(&contract_ast.expressions[0], &mut env, &context);

                #[cfg(not(target_arch = "wasm32"))]
                if show_timings {
                    println!("execution time: {:?}", start.elapsed());
                }

                return result.map(Some);
            }

            // deploy a contract
            eval_all(&contract_ast.expressions, &mut contract_context, g, None)
        });

        let value = result.map_err(|e| {
            let err = format!("Runtime error while interpreting {contract_id}: {e:?}");
            if let Some(mut eval_hooks) = global_context.eval_hooks.take() {
                for hook in eval_hooks.iter_mut() {
                    hook.did_complete(Err(err.clone()));
                }
                global_context.eval_hooks = Some(eval_hooks);
            }
            err
        })?;

        let mut cost = None;
        if cost_track {
            cost = Some(CostSynthesis::from_cost_tracker(&global_context.cost_track));
        }

        let mut emitted_events = global_context
            .event_batches
            .iter()
            .flat_map(|b| b.events.clone())
            .collect::<Vec<_>>();

        let contract_saved =
            !contract_context.functions.is_empty() || !contract_context.defined_traits.is_empty();

        let eval_result = if contract_saved {
            let mut functions = BTreeMap::new();
            for (name, defined_func) in contract_context.functions.iter() {
                if !defined_func.is_public() {
                    continue;
                }

                let args: Vec<_> = defined_func
                    .get_arguments()
                    .iter()
                    .zip(defined_func.get_arg_types().iter())
                    .map(|(n, t)| format!("({} {})", n.as_str(), t))
                    .collect();

                functions.insert(name.to_string(), args);
            }
            let parsed_contract = ParsedContract {
                contract_identifier: contract_id.to_string(),
                code: snippet.to_string(),
                function_args: functions,
                ast: contract_ast.clone(),
                analysis: analysis.clone(),
            };

            global_context
                .database
                .insert_contract_hash(&contract_id, snippet)
                .unwrap();
            let contract = Contract { contract_context };
            global_context
                .database
                .insert_contract(&contract_id, contract)
                .expect("failed to insert contract");
            global_context
                .database
                .set_contract_data_size(&contract_id, 0)
                .unwrap();

            EvaluationResult::Contract(ContractEvaluationResult {
                result: value,
                contract: parsed_contract,
            })
        } else {
            let result = value.unwrap_or(Value::none());
            EvaluationResult::Snippet(SnippetEvaluationResult { result })
        };

        global_context.commit().unwrap();

        let (events, accounts_to_credit, accounts_to_debit) =
            Self::process_events(&mut emitted_events);

        let mut execution_result = ExecutionResult {
            result: eval_result,
            events,
            cost,
            diagnostics: Vec::new(),
        };

        if let Some(mut eval_hooks) = global_context.eval_hooks {
            for hook in eval_hooks.iter_mut() {
                hook.did_complete(Ok(&mut execution_result));
            }
        }

        for (account, token, value) in accounts_to_credit {
            self.credit_token(account, token, value);
        }

        for (account, token, value) in accounts_to_debit {
            self.debit_token(account, token, value);
        }

        if contract_saved {
            let mut analysis_db = AnalysisDatabase::new(&mut self.clarity_datastore);
            analysis_db
                .execute(|db| db.insert_contract(&contract_id, &analysis))
                .expect("Unable to save data");
        }

        Ok(execution_result)
    }

    pub fn call_contract_fn(
        &mut self,
        contract_id: &QualifiedContractIdentifier,
        method: &str,
        args: &[SymbolicExpression],
        epoch: StacksEpochId,
        clarity_version: ClarityVersion,
        track_costs: bool,
        allow_private: bool,
        eval_hooks: Vec<&mut dyn EvalHook>,
    ) -> Result<ExecutionResult, ContractCallError> {
        let tx_sender: PrincipalData = self.tx_sender.clone().into();

        let mut global_context = self
            .get_global_context(epoch, track_costs)
            .map_err(ContractCallError::from)?;

        let mut hooks: Vec<&mut dyn EvalHook> = Vec::new();
        for hook in eval_hooks {
            hooks.push(hook);
        }
        if !hooks.is_empty() {
            global_context.eval_hooks = Some(hooks);
        }

        let contract_context = ContractContext::new(contract_id.clone(), clarity_version);

        global_context.begin();
        let result = global_context.execute(|g| {
            let mut call_stack = CallStack::new();
            let mut env = Environment::new(
                g,
                &contract_context,
                &mut call_stack,
                Some(tx_sender.clone()),
                Some(tx_sender.clone()),
                None,
            );

            match allow_private {
                true => env.execute_contract_allow_private(contract_id, method, args, false),
                false => env.execute_contract(contract_id, method, args, false),
            }
        });

        let value = result.map_err(|e| {
            let err = format!("Runtime error while interpreting {contract_id}: {e:?}");
            if let Some(mut eval_hooks) = global_context.eval_hooks.take() {
                for hook in eval_hooks.iter_mut() {
                    hook.did_complete(Err(err.clone()));
                }
                global_context.eval_hooks = Some(eval_hooks);
            }
            err
        });

        let mut cost = None;
        if track_costs {
            cost = Some(CostSynthesis::from_cost_tracker(&global_context.cost_track));
        }

        let mut emitted_events = global_context
            .event_batches
            .iter()
            .flat_map(|b| b.events.clone())
            .collect::<Vec<_>>();

        let eval_result = match value {
            Ok(result) => EvaluationResult::Snippet(SnippetEvaluationResult { result }),
            Err(e) => {
                return Err(ContractCallError::from(e));
            }
        };
        global_context.commit().unwrap();

        let (events, accounts_to_credit, accounts_to_debit) =
            Self::process_events(&mut emitted_events);

        let mut execution_result = ExecutionResult {
            result: eval_result,
            events,
            cost,
            diagnostics: Vec::new(),
        };

        if let Some(mut eval_hooks) = global_context.eval_hooks {
            for hook in eval_hooks.iter_mut() {
                hook.did_complete(Ok(&mut execution_result));
            }
        }

        for (account, token, value) in accounts_to_credit {
            self.credit_token(account, token, value);
        }

        for (account, token, value) in accounts_to_debit {
            self.debit_token(account, token, value);
        }

        Ok(execution_result)
    }

    fn process_events(
        emitted_events: &mut Vec<StacksTransactionEvent>,
    ) -> (
        Vec<StacksTransactionEvent>,
        Vec<(String, String, u128)>,
        Vec<(String, String, u128)>,
    ) {
        let mut events = vec![];
        let mut accounts_to_debit = vec![];
        let mut accounts_to_credit = vec![];
        for event in emitted_events.drain(..) {
            match event {
                StacksTransactionEvent::STXEvent(STXEventType::STXTransferEvent(
                    ref event_data,
                )) => {
                    accounts_to_debit.push((
                        event_data.sender.to_string(),
                        "STX".to_string(),
                        event_data.amount,
                    ));
                    accounts_to_credit.push((
                        event_data.recipient.to_string(),
                        "STX".to_string(),
                        event_data.amount,
                    ));
                }
                StacksTransactionEvent::STXEvent(STXEventType::STXMintEvent(ref event_data)) => {
                    accounts_to_credit.push((
                        event_data.recipient.to_string(),
                        "STX".to_string(),
                        event_data.amount,
                    ));
                }
                StacksTransactionEvent::STXEvent(STXEventType::STXBurnEvent(ref event_data)) => {
                    accounts_to_debit.push((
                        event_data.sender.to_string(),
                        "STX".to_string(),
                        event_data.amount,
                    ));
                }
                StacksTransactionEvent::FTEvent(FTEventType::FTTransferEvent(ref event_data)) => {
                    accounts_to_credit.push((
                        event_data.recipient.to_string(),
                        event_data.asset_identifier.sugared(),
                        event_data.amount,
                    ));
                    accounts_to_debit.push((
                        event_data.sender.to_string(),
                        event_data.asset_identifier.sugared(),
                        event_data.amount,
                    ));
                }
                StacksTransactionEvent::FTEvent(FTEventType::FTMintEvent(ref event_data)) => {
                    accounts_to_credit.push((
                        event_data.recipient.to_string(),
                        event_data.asset_identifier.sugared(),
                        event_data.amount,
                    ));
                }
                StacksTransactionEvent::FTEvent(FTEventType::FTBurnEvent(ref event_data)) => {
                    accounts_to_debit.push((
                        event_data.sender.to_string(),
                        event_data.asset_identifier.sugared(),
                        event_data.amount,
                    ));
                }
                StacksTransactionEvent::NFTEvent(NFTEventType::NFTTransferEvent(
                    ref event_data,
                )) => {
                    accounts_to_credit.push((
                        event_data.recipient.to_string(),
                        event_data.asset_identifier.sugared(),
                        1,
                    ));
                    accounts_to_debit.push((
                        event_data.sender.to_string(),
                        event_data.asset_identifier.sugared(),
                        1,
                    ));
                }
                StacksTransactionEvent::NFTEvent(NFTEventType::NFTMintEvent(ref event_data)) => {
                    accounts_to_credit.push((
                        event_data.recipient.to_string(),
                        event_data.asset_identifier.sugared(),
                        1,
                    ));
                }
                StacksTransactionEvent::NFTEvent(NFTEventType::NFTBurnEvent(ref event_data)) => {
                    accounts_to_debit.push((
                        event_data.sender.to_string(),
                        event_data.asset_identifier.sugared(),
                        1,
                    ));
                }
                _ => {}
            };
            events.push(event);
        }
        (events, accounts_to_credit, accounts_to_debit)
    }

    pub fn save_genesis_accounts(&mut self, addresses: Vec<StandardPrincipalData>) {
        self.clarity_datastore.save_local_account(addresses);
    }

    pub fn mint_stx_balance(
        &mut self,
        recipient: PrincipalData,
        amount: u64,
    ) -> Result<String, String> {
        let final_balance = {
            let mut global_context = self.get_global_context(DEFAULT_EPOCH, false)?;

            global_context.begin();
            let mut cur_balance = global_context
                .database
                .get_stx_balance_snapshot(&recipient)
                .expect("failed to get balance snapshot");
            cur_balance
                .credit(amount as u128)
                .expect("failed to credit balance");
            let final_balance = cur_balance
                .get_available_balance()
                .expect("failed to get balance");
            cur_balance.save().expect("failed to save balance");
            global_context
                .database
                .increment_ustx_liquid_supply(amount as u128)
                .unwrap();
            global_context.commit().unwrap();
            final_balance
        };
        self.credit_token(recipient.to_string(), "STX".to_string(), amount.into());
        Ok(format!("→ {recipient}: {final_balance} µSTX"))
    }

    pub fn set_tx_sender(&mut self, tx_sender: StandardPrincipalData) {
        self.tx_sender = tx_sender;
    }

    pub fn get_tx_sender(&self) -> StandardPrincipalData {
        self.tx_sender.clone()
    }

    pub fn set_current_epoch(&mut self, epoch: StacksEpochId) {
        self.datastore
            .set_current_epoch(&mut self.clarity_datastore, epoch);
    }

    pub fn advance_burn_chain_tip(&mut self, count: u32) -> u32 {
        let new_height = self
            .datastore
            .advance_burn_chain_tip(&mut self.clarity_datastore, count);
        self.set_tenure_height();
        new_height
    }

    pub fn advance_stacks_chain_tip(&mut self, count: u32) -> Result<u32, String> {
        let current_epoch = self.datastore.get_current_epoch();
        if current_epoch < StacksEpochId::Epoch30 {
            Err("only burn chain height can be advanced in epoch lower than 3.0".to_string())
        } else {
            Ok(self
                .datastore
                .advance_stacks_chain_tip(&mut self.clarity_datastore, count))
        }
    }

    pub fn set_tenure_height(&mut self) {
        let mut conn = ClarityDatabase::new(
            &mut self.clarity_datastore,
            &self.datastore,
            &self.datastore,
        );
        conn.begin();
        conn.put_data(
            "_stx-data::tenure_height",
            &self.datastore.get_current_tenure(),
        )
        .expect("failed set tenure height");
        conn.commit().expect("failed to commit");
    }

    pub fn get_block_height(&mut self) -> u32 {
        self.datastore.get_current_stacks_block_height()
    }

    pub fn get_burn_block_height(&mut self) -> u32 {
        self.datastore.get_current_burn_block_height()
    }

    fn credit_token(&mut self, account: String, token: String, value: u128) {
        self.accounts.insert(account.clone());
        match self.tokens.entry(token) {
            Entry::Occupied(balances) => {
                balances
                    .into_mut()
                    .entry(account)
                    .and_modify(|e| *e += value)
                    .or_insert(value);
            }
            Entry::Vacant(v) => {
                let mut balances = BTreeMap::new();
                balances.insert(account, value);
                v.insert(balances);
            }
        };
    }

    fn debit_token(&mut self, account: String, token: String, value: u128) {
        self.accounts.insert(account.clone());
        match self.tokens.entry(token) {
            Entry::Occupied(balances) => {
                balances
                    .into_mut()
                    .entry(account)
                    .and_modify(|e| *e -= value)
                    .or_insert(value);
            }
            Entry::Vacant(v) => {
                let mut balances = BTreeMap::new();
                balances.insert(account, value);
                v.insert(balances);
            }
        };
    }

    pub fn get_assets_maps(&self) -> BTreeMap<String, BTreeMap<String, u128>> {
        self.tokens.clone()
    }

    pub fn get_tokens(&self) -> Vec<String> {
        self.tokens.keys().cloned().collect()
    }

    pub fn get_accounts(&self) -> Vec<String> {
        self.accounts.clone().into_iter().collect::<Vec<_>>()
    }

    pub fn get_balance_for_account(&self, account: &str, token: &str) -> u128 {
        match self.tokens.get(token) {
            Some(balances) => match balances.get(account) {
                Some(value) => *value,
                _ => 0,
            },
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::types::chainstate::StacksAddress;
    use clarity::types::Address;
    use clarity::util::hash::hex_bytes;
    use clarity::vm::{self, ClarityVersion};
    use clarity_types::types::TupleData;
    use indoc::indoc;

    use super::*;
    use crate::analysis::Settings as AnalysisSettings;
    use crate::repl::settings::RemoteDataSettings;
    use crate::test_fixtures::clarity_contract::ClarityContractBuilder;

    #[track_caller]
    fn get_interpreter(settings: Option<Settings>) -> ClarityInterpreter {
        ClarityInterpreter::new(
            StandardPrincipalData::transient(),
            settings.unwrap_or_default(),
            None,
        )
    }

    #[track_caller]
    fn deploy_contract(
        interpreter: &mut ClarityInterpreter,
        contract: &ClarityContract,
    ) -> Result<ExecutionResult, ContractCallError> {
        let source = contract.expect_in_memory_code_source();
        let (ast, ..) = interpreter.build_ast(contract);
        let (annotations, _) = interpreter.collect_annotations(source);

        let (analysis, _) = interpreter
            .run_analysis(contract, &ast, &annotations)
            .unwrap();

        let result = interpreter
            .execute(contract, &ast, analysis, false, None)
            .map_err(ContractCallError::from);
        assert!(result.is_ok());
        result
    }

    #[track_caller]
    fn get_evaluation_result(result: Result<ExecutionResult, ContractCallError>) -> Value {
        assert!(result.is_ok());
        let result = result.unwrap();
        match result.result {
            EvaluationResult::Contract(_) => unreachable!(),
            EvaluationResult::Snippet(res) => res.result,
        }
    }

    #[track_caller]
    fn assert_execution_result_value(
        result: Result<ExecutionResult, ContractCallError>,
        expected_value: Value,
    ) {
        let result = get_evaluation_result(result);
        assert_eq!(result, expected_value);
    }

    #[track_caller]
    fn run_snippet(
        interpreter: &mut ClarityInterpreter,
        snippet: &str,
        clarity_version: ClarityVersion,
    ) -> Value {
        let contract = ClarityContractBuilder::new()
            .code_source(snippet.to_string())
            .epoch(interpreter.datastore.get_current_epoch())
            .clarity_version(clarity_version)
            .build();
        let deploy_result = deploy_contract(interpreter, &contract);
        assert!(deploy_result.is_ok());
        get_evaluation_result(deploy_result)
    }

    #[test]
    fn test_get_tx_sender() {
        let mut interpreter = get_interpreter(None);
        let tx_sender = StandardPrincipalData::transient();
        interpreter.set_tx_sender(tx_sender.clone());
        assert_eq!(interpreter.get_tx_sender(), tx_sender);
    }

    #[test]
    fn test_set_tx_sender() {
        let mut interpreter = get_interpreter(None);

        let addr = StacksAddress::from_string("ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5").unwrap();
        let tx_sender = StandardPrincipalData::from(addr);
        interpreter.set_tx_sender(tx_sender.clone());
        assert_eq!(interpreter.get_tx_sender(), tx_sender);
    }

    #[test]
    fn test_get_block_time() {
        let mut interpreter = get_interpreter(None);
        let bt = interpreter.get_block_time();
        assert_ne!(bt, 0); // TODO placeholder
    }

    #[test]
    fn test_get_block_height() {
        let mut interpreter = get_interpreter(None);
        assert_eq!(interpreter.get_block_height(), 0);
    }

    #[test]
    fn test_advance_stacks_chain_tip_pre_epoch_3() {
        let mut interpreter = get_interpreter(None);
        interpreter.set_current_epoch(StacksEpochId::Epoch2_05);
        let count = 5;
        let initial_block_height = interpreter.get_burn_block_height();
        assert_ne!(interpreter.advance_stacks_chain_tip(count), Ok(count));
        assert_eq!(interpreter.get_burn_block_height(), initial_block_height);
        assert_eq!(interpreter.get_block_height(), initial_block_height);
    }

    #[test]
    fn test_advance_stacks_chain_tip() {
        let wasm_settings = Settings {
            analysis: AnalysisSettings::default(),
            remote_data: RemoteDataSettings::default(),
            show_timings: false,
        };
        let mut interpreter = get_interpreter(Some(wasm_settings));
        interpreter.set_current_epoch(StacksEpochId::Epoch30);
        interpreter.advance_burn_chain_tip(1);
        let count = 5;
        let initial_block_height = interpreter.get_block_height();

        let result = interpreter.advance_stacks_chain_tip(count);
        assert_eq!(result, Ok(initial_block_height + count));

        assert_eq!(interpreter.get_burn_block_height(), initial_block_height);
        assert_eq!(interpreter.get_block_height(), initial_block_height + count);
    }

    #[test]
    fn test_advance_chain_tip_pre_epoch3() {
        let mut interpreter = get_interpreter(None);
        interpreter.set_current_epoch(StacksEpochId::Epoch2_05);
        let count = 5;
        let initial_block_height = interpreter.get_block_height();
        interpreter.advance_burn_chain_tip(count);
        assert_eq!(interpreter.get_block_height(), initial_block_height + count);
        assert_eq!(
            interpreter.get_burn_block_height(),
            initial_block_height + count
        );
    }

    #[test]
    fn test_advance_chain_tip() {
        let mut interpreter = get_interpreter(None);
        interpreter.set_current_epoch(StacksEpochId::Epoch30);
        let count = 5;
        let initial_block_height = interpreter.get_block_height();
        interpreter.advance_burn_chain_tip(count);
        assert_eq!(interpreter.get_block_height(), initial_block_height + count);
        assert_eq!(
            interpreter.get_burn_block_height(),
            initial_block_height + count
        );
    }

    #[test]
    fn test_get_assets_maps() {
        let mut interpreter = get_interpreter(None);
        let addr = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        let amount = 1000;
        interpreter.credit_token(addr.into(), "STX".into(), amount);

        let assets = interpreter.get_assets_maps();
        assert!(assets.contains_key("STX"));

        let stx = assets.get("STX").unwrap();
        assert!(stx.contains_key(addr));

        let balance = stx.get(addr).unwrap();
        assert_eq!(balance, &amount)
    }

    #[test]
    fn test_get_tokens() {
        let mut interpreter = get_interpreter(None);
        let addr = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        interpreter.credit_token(addr.into(), "STX".into(), 1000);

        let tokens = interpreter.get_tokens();
        assert_eq!(tokens, ["STX"]);
    }

    #[test]
    fn test_get_accounts() {
        let mut interpreter = get_interpreter(None);
        let addr = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        interpreter.credit_token(addr.into(), "STX".into(), 1000);

        let accounts = interpreter.get_accounts();
        assert_eq!(accounts, ["ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5"]);
    }

    #[test]
    fn test_get_balance_for_account() {
        let mut interpreter = get_interpreter(None);

        let addr = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        let amount = 1000;
        interpreter.credit_token(addr.into(), "STX".into(), amount);

        let balance = interpreter.get_balance_for_account(addr, "STX");
        assert_eq!(balance, amount);
    }

    #[test]
    fn test_credit_any_token() {
        let mut interpreter = get_interpreter(None);

        let addr = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        let amount = 1000;
        interpreter.credit_token(addr.into(), "MIA".into(), amount);

        let balance = interpreter.get_balance_for_account(addr, "MIA");
        assert_eq!(balance, amount);
    }

    #[test]
    fn test_mint_stx_balance() {
        let mut interpreter = get_interpreter(None);
        let recipient = PrincipalData::Standard(StandardPrincipalData::transient());
        let amount = 1000;

        let result = interpreter.mint_stx_balance(recipient.clone(), amount);
        assert!(result.is_ok());

        let balance = interpreter.get_balance_for_account(&recipient.to_string(), "STX");
        assert_eq!(balance, amount.into());
    }

    #[test]
    fn test_run_valid_contract() {
        let mut interpreter = get_interpreter(None);
        let contract = ClarityContract::fixture();
        let result = interpreter.run(&contract, None, false, None);
        assert!(result.is_ok());
        assert!(result.unwrap().diagnostics.is_empty());
    }

    #[test]
    fn test_run_invalid_contract() {
        let mut interpreter = get_interpreter(None);

        let snippet = "(define-public (add) (ok (+ u1 1)))";
        //                                            ^ should be uint
        let contract = ClarityContractBuilder::default()
            .code_source(snippet.into())
            .build();
        let result = interpreter.run(&contract, None, false, None);
        assert!(result.is_err());
        let diagnostics = result.unwrap_err();
        assert_eq!(diagnostics.len(), 1);
    }

    #[test]
    fn test_run_runtime_error() {
        let mut interpreter = get_interpreter(None);

        let snippet = "(/ u1 u0)";
        let contract = ClarityContractBuilder::default()
            .code_source(snippet.into())
            .build();
        let result = interpreter.run(&contract, None, false, None);
        assert!(result.is_err());

        let diagnostics = result.unwrap_err();
        assert_eq!(diagnostics.len(), 1);

        let message = format!("Runtime Error: Runtime error while interpreting {}.{}: Runtime(DivisionByZero, Some([FunctionIdentifier {{ identifier: \"_native_:native_div\" }}]))", StandardPrincipalData::transient(), contract.name);
        assert_eq!(
            diagnostics[0],
            Diagnostic {
                level: vm::diagnostic::Level::Error,
                message,
                spans: vec![],
                suggestion: None
            }
        );
    }

    #[test]
    fn test_build_ast() {
        let interpreter = get_interpreter(None);
        let contract = ClarityContract::fixture();
        let (_ast, diagnostics, success) = interpreter.build_ast(&contract);
        assert!(success);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_execute() {
        let mut interpreter = get_interpreter(None);

        let contract = ClarityContract::fixture();
        let result = deploy_contract(&mut interpreter, &contract);

        let ExecutionResult {
            diagnostics,
            events,
            ..
        } = result.unwrap();
        assert!(diagnostics.is_empty());
        assert!(events.is_empty());
    }

    #[test]
    fn test_call_contract_fn() {
        let mut interpreter = get_interpreter(None);

        let contract = ClarityContract::fixture();
        let source = contract.expect_in_memory_code_source();
        let (ast, ..) = interpreter.build_ast(&contract);
        let (annotations, _) = interpreter.collect_annotations(source);

        let (analysis, _) = interpreter
            .run_analysis(&contract, &ast, &annotations)
            .unwrap();

        let result = interpreter.execute(&contract, &ast, analysis, false, None);
        assert!(result.is_ok());
        let ExecutionResult {
            diagnostics,
            events,
            ..
        } = result.unwrap();
        assert!(diagnostics.is_empty());
        assert!(events.is_empty());
    }

    #[test]
    fn test_run_both() {
        let mut interpreter = get_interpreter(None);

        let contract = ClarityContract::fixture();
        let _ = deploy_contract(&mut interpreter, &contract);

        let call_contract = ClarityContractBuilder::default()
            .code_source("(contract-call? .contract incr)".to_owned())
            .build();
        let _ = interpreter.run(&call_contract, None, false, None);
    }

    #[test]
    fn test_get_data_var() {
        let mut interpreter = get_interpreter(None);
        let contract = ClarityContractBuilder::default()
            .code_source(["(define-data-var count uint u9)"].join("\n"))
            .build();

        let deploy = deploy_contract(&mut interpreter, &contract);
        assert!(deploy.is_ok());

        let contract_id = QualifiedContractIdentifier {
            issuer: StandardPrincipalData::transient(),
            name: "contract".into(),
        };
        let count = interpreter.get_data_var(&contract_id, "count");

        assert_eq!(
            count,
            Some("0x0100000000000000000000000000000009".to_owned())
        )
    }

    #[test]
    fn test_get_map_entry() {
        let mut interpreter = get_interpreter(None);
        let contract = ClarityContractBuilder::default()
            .code_source(
                [
                    "(define-map people uint (string-ascii 10))",
                    "(map-insert people u0 \"satoshi\")",
                ]
                .join("\n"),
            )
            .build();

        let deploy = deploy_contract(&mut interpreter, &contract);
        assert!(deploy.is_ok());

        let contract_id = QualifiedContractIdentifier {
            issuer: StandardPrincipalData::transient(),
            name: "contract".into(),
        };
        let name = interpreter.get_map_entry(&contract_id, "people", &Value::UInt(0));
        assert_eq!(name, Some("0x0a0d000000077361746f736869".to_owned()));
        let no_name = interpreter.get_map_entry(&contract_id, "people", &Value::UInt(404));
        assert_eq!(no_name, None);
    }

    #[test]
    fn test_execute_stx_events() {
        let mut interpreter = get_interpreter(None);
        let account = PrincipalData::parse("S1G2081040G2081040G2081040G208105NK8PE5").unwrap();
        let _ = interpreter.mint_stx_balance(account, 100000);

        let contract = ClarityContractBuilder::default()
            .code_source(
                [
                    "(define-public (test-transfer)",
                    "  (ok (stx-transfer? u10 tx-sender (as-contract tx-sender))))",
                    "(define-public (test-burn)",
                    "  (ok (stx-burn? u10 tx-sender)))",
                    "(test-transfer)",
                    "(test-burn)",
                ]
                .join("\n"),
            )
            .build();

        let account =
            PrincipalData::parse_standard_principal("S1G2081040G2081040G2081040G208105NK8PE5")
                .unwrap();
        let balance = interpreter.get_balance_for_account(&account.to_string(), "STX");
        assert_eq!(balance, 100000);

        let result = deploy_contract(&mut interpreter, &contract);
        assert!(result.is_ok());

        let ExecutionResult {
            diagnostics,
            events,
            ..
        } = result.unwrap();
        assert!(diagnostics.is_empty());
        assert_eq!(events.len(), 2);

        let balance = interpreter.get_balance_for_account(&account.to_string(), "STX");
        assert_eq!(balance, 99980);

        assert!(matches!(
            events[0],
            StacksTransactionEvent::STXEvent(STXEventType::STXTransferEvent(_))
        ));
        assert!(matches!(
            events[1],
            StacksTransactionEvent::STXEvent(STXEventType::STXBurnEvent(_))
        ));
    }

    #[test]
    fn test_execute_ft_events() {
        let mut interpreter = get_interpreter(None);

        let contract = ClarityContractBuilder::default()
            .code_source(
                [
                    "(define-fungible-token ctb)",
                    "(define-private (test-mint)",
                    "  (ft-mint? ctb u100 tx-sender))",
                    "(define-private (test-burn)",
                    "  (ft-burn? ctb u10 tx-sender))",
                    "(define-private (test-transfer)",
                    "  (ft-transfer? ctb u10 tx-sender (as-contract tx-sender)))",
                    "(test-mint)",
                    "(test-burn)",
                    "(test-transfer)",
                ]
                .join("\n"),
            )
            .build();

        let result = deploy_contract(&mut interpreter, &contract);
        assert!(result.is_ok());
        let ExecutionResult {
            diagnostics,
            events,
            ..
        } = result.unwrap();
        assert!(diagnostics.is_empty());
        assert_eq!(events.len(), 3);
        assert!(matches!(
            events[0],
            StacksTransactionEvent::FTEvent(FTEventType::FTMintEvent(_))
        ));
        assert!(matches!(
            events[1],
            StacksTransactionEvent::FTEvent(FTEventType::FTBurnEvent(_))
        ));
        assert!(matches!(
            events[2],
            StacksTransactionEvent::FTEvent(FTEventType::FTTransferEvent(_))
        ));
    }

    #[test]
    fn test_execute_nft_events() {
        let mut interpreter = get_interpreter(None);

        let contract = ClarityContractBuilder::default()
            .code_source(
                [
                    "(define-non-fungible-token nftest uint)",
                    "(nft-mint? nftest u1 tx-sender)",
                    "(nft-mint? nftest u2 tx-sender)",
                    "(define-private (test-burn)",
                    "  (nft-burn? nftest u1 tx-sender))",
                    "(define-private (test-transfer)",
                    "  (nft-transfer? nftest u2 tx-sender (as-contract  tx-sender)))",
                    "(test-burn)",
                    "(test-transfer)",
                ]
                .join("\n"),
            )
            .build();

        let result = deploy_contract(&mut interpreter, &contract);
        assert!(result.is_ok());
        let ExecutionResult {
            diagnostics,
            events,
            ..
        } = result.unwrap();
        assert!(diagnostics.is_empty());
        assert_eq!(events.len(), 4);
        assert!(matches!(
            events[0],
            StacksTransactionEvent::NFTEvent(NFTEventType::NFTMintEvent(_))
        ));
        assert!(matches!(
            events[1],
            StacksTransactionEvent::NFTEvent(NFTEventType::NFTMintEvent(_))
        ));
        assert!(matches!(
            events[2],
            StacksTransactionEvent::NFTEvent(NFTEventType::NFTBurnEvent(_))
        ));
        assert!(matches!(
            events[3],
            StacksTransactionEvent::NFTEvent(NFTEventType::NFTTransferEvent(_))
        ));
    }

    #[test]
    fn block_height_support_in_clarity2_epoch2() {
        let mut interpreter = get_interpreter(None);

        let snippet = [
            "(define-read-only (get-height)",
            "  { block-height: block-height }",
            ")",
            "(define-read-only (get-info (h uint))",
            "  { time: (get-block-info? time h) }",
            ")",
        ]
        .join("\n");
        let contract = ClarityContractBuilder::new()
            .code_source(snippet)
            .epoch(StacksEpochId::Epoch25)
            .clarity_version(ClarityVersion::Clarity2)
            .build();

        let deploy = deploy_contract(&mut interpreter, &contract);
        assert!(deploy.is_ok());

        let contract_id =
            contract.expect_resolved_contract_identifier(Some(&StandardPrincipalData::transient()));

        let result = interpreter.call_contract_fn(
            &contract_id,
            "get-height",
            &[],
            StacksEpochId::Epoch25,
            ClarityVersion::Clarity2,
            false,
            false,
            vec![],
        );
        assert_execution_result_value(
            result,
            Value::Tuple(
                TupleData::from_data(vec![("block-height".into(), Value::UInt(0))]).unwrap(),
            ),
        );

        interpreter.advance_burn_chain_tip(10);

        let result = interpreter.call_contract_fn(
            &contract_id,
            "get-height",
            &[],
            StacksEpochId::Epoch25,
            ClarityVersion::Clarity2,
            false,
            false,
            vec![],
        );
        assert_execution_result_value(
            result,
            Value::Tuple(
                TupleData::from_data(vec![("block-height".into(), Value::UInt(10))]).unwrap(),
            ),
        );

        let call_contract = ClarityContractBuilder::default()
            .code_source("(contract-call? .contract get-info u1)".into())
            .epoch(StacksEpochId::Epoch25)
            .clarity_version(ClarityVersion::Clarity2)
            .build();
        assert!(interpreter.run(&call_contract, None, false, None).is_ok());
    }

    #[test]
    fn block_height_support_in_clarity2_epoch3() {
        let mut interpreter = get_interpreter(None);

        interpreter.advance_burn_chain_tip(1);

        let snippet = [
            "(define-read-only (get-height)",
            "  { block-height: block-height }",
            ")",
            "(define-read-only (get-info (h uint))",
            "  { time: (get-block-info? time h) }",
            ")",
        ]
        .join("\n");
        let contract = ClarityContractBuilder::new()
            .code_source(snippet)
            .epoch(StacksEpochId::Epoch30)
            .clarity_version(ClarityVersion::Clarity2)
            .build();

        let deploy_result = deploy_contract(&mut interpreter, &contract);
        assert!(deploy_result.is_ok());

        let contract_id =
            contract.expect_resolved_contract_identifier(Some(&StandardPrincipalData::transient()));

        let result = interpreter.call_contract_fn(
            &contract_id,
            "get-height",
            &[],
            StacksEpochId::Epoch30,
            ClarityVersion::Clarity2,
            false,
            false,
            vec![],
        );
        assert_execution_result_value(
            result,
            Value::Tuple(
                TupleData::from_data(vec![("block-height".into(), Value::UInt(1))]).unwrap(),
            ),
        );

        let call_contract = ClarityContractBuilder::default()
            .code_source("(contract-call? .contract get-info u1)".into())
            .epoch(StacksEpochId::Epoch30)
            .clarity_version(ClarityVersion::Clarity3)
            .build();
        assert!(interpreter.run(&call_contract, None, false, None).is_ok());
    }

    #[test]
    fn block_height_support_in_clarity3_epoch3() {
        let mut interpreter = get_interpreter(None);

        interpreter.advance_burn_chain_tip(1);

        let snippet = [
            "(define-read-only (get-height)",
            "  {",
            "    stacks-block-height: stacks-block-height,",
            "    tenure-height: tenure-height,",
            "  }",
            ")",
            "(define-read-only (get-info (h uint))",
            "  {",
            "    stacks-time: (get-stacks-block-info? time h),",
            "    stacks-id-header-hash: (get-stacks-block-info? id-header-hash h),",
            "    stacks-header-hash: (get-stacks-block-info? header-hash h),",
            "    tenure-time: (get-tenure-info? time h),",
            "    tenure-miner-address: (get-tenure-info? miner-address h),",
            "  }",
            ")",
        ]
        .join("\n");
        let contract = ClarityContractBuilder::new()
            .code_source(snippet)
            .epoch(StacksEpochId::Epoch30)
            .clarity_version(ClarityVersion::Clarity3)
            .build();

        let deploy_result = deploy_contract(&mut interpreter, &contract);
        assert!(deploy_result.is_ok());

        let contract_id =
            contract.expect_resolved_contract_identifier(Some(&StandardPrincipalData::transient()));

        let result = interpreter.call_contract_fn(
            &contract_id,
            "get-height",
            &[],
            StacksEpochId::Epoch30,
            ClarityVersion::Clarity3,
            false,
            false,
            vec![],
        );
        assert_execution_result_value(
            result,
            Value::Tuple(
                TupleData::from_data(vec![
                    ("stacks-block-height".into(), Value::UInt(1)),
                    ("tenure-height".into(), Value::UInt(1)),
                ])
                .unwrap(),
            ),
        );

        interpreter.advance_burn_chain_tip(10);

        let result = interpreter.call_contract_fn(
            &contract_id,
            "get-height",
            &[],
            StacksEpochId::Epoch30,
            ClarityVersion::Clarity3,
            false,
            false,
            vec![],
        );
        assert_execution_result_value(
            result,
            Value::Tuple(
                TupleData::from_data(vec![
                    ("stacks-block-height".into(), Value::UInt(11)),
                    ("tenure-height".into(), Value::UInt(11)),
                ])
                .unwrap(),
            ),
        );

        let call_contract = ClarityContractBuilder::default()
            .code_source("(contract-call? .contract get-info u1)".into())
            .epoch(StacksEpochId::Epoch30)
            .clarity_version(ClarityVersion::Clarity3)
            .build();
        assert!(interpreter.run(&call_contract, None, false, None).is_ok());
    }

    #[test]
    fn burn_block_time_is_realistic_in_epoch_3_0() {
        let mut interpreter = get_interpreter(None);

        interpreter.set_current_epoch(StacksEpochId::Epoch30);
        interpreter.advance_burn_chain_tip(3);

        let snippet_1 = run_snippet(
            &mut interpreter,
            "(get-tenure-info? time u2)",
            ClarityVersion::Clarity3,
        );
        let Ok(Some(Value::UInt(time_block_1))) = snippet_1.expect_optional() else {
            panic!("Unexpected result");
        };

        let snippet_2 = run_snippet(
            &mut interpreter,
            "(get-tenure-info? time u3)",
            ClarityVersion::Clarity3,
        );
        let Ok(Some(Value::UInt(time_block_2))) = snippet_2.expect_optional() else {
            panic!("Unexpected result");
        };
        assert_eq!(time_block_2 - time_block_1, 600);
    }

    #[test]
    fn first_stacks_block_time_in_a_tenure() {
        let mut interpreter = get_interpreter(None);

        interpreter.set_current_epoch(StacksEpochId::Epoch30);
        let _ = interpreter.advance_burn_chain_tip(2);

        let snippet_1 = run_snippet(
            &mut interpreter,
            "(get-tenure-info? time (- stacks-block-height u1))",
            ClarityVersion::Clarity3,
        );
        let Ok(Some(Value::UInt(last_tenure_time))) = snippet_1.expect_optional() else {
            panic!("Unexpected result");
        };

        let snippet_2 = run_snippet(
            &mut interpreter,
            "(get-stacks-block-info? time (- stacks-block-height u1))",
            ClarityVersion::Clarity3,
        );
        let Ok(Some(Value::UInt(last_stacks_block_time))) = snippet_2.expect_optional() else {
            panic!("Unexpected result");
        };
        assert_eq!((last_stacks_block_time) - (last_tenure_time), 10);
    }

    #[test]
    fn stacks_block_time_is_realistic_in_epoch_3_0() {
        let mut interpreter = get_interpreter(None);

        interpreter.set_current_epoch(StacksEpochId::Epoch30);
        let _ = interpreter.advance_stacks_chain_tip(3);

        let snippet_1 = run_snippet(
            &mut interpreter,
            "(get-stacks-block-info? time u2)",
            ClarityVersion::Clarity3,
        );
        let Ok(Some(Value::UInt(time_block_1))) = snippet_1.expect_optional() else {
            panic!("Unexpected result");
        };

        let snippet_2 = run_snippet(
            &mut interpreter,
            "(get-stacks-block-info? time u3)",
            ClarityVersion::Clarity3,
        );
        let Ok(Some(Value::UInt(time_block_2))) = snippet_2.expect_optional() else {
            panic!("Unexpected result");
        };
        assert_eq!(time_block_2 - time_block_1, 10);
    }

    #[test]
    fn burn_block_time_after_many_stacks_blocks_is_realistic_in_epoch_3_0() {
        let mut interpreter = get_interpreter(None);

        interpreter.set_current_epoch(StacksEpochId::Epoch30);
        // by advancing stacks_chain_tip by 101, we are getting a tenure of more than 600 seconds
        // the next burn block should happen after the last stacks block
        let stacks_block_height = interpreter.advance_stacks_chain_tip(101).unwrap();
        assert_eq!(stacks_block_height, 102);

        let snippet_1 = run_snippet(
            &mut interpreter,
            "(get-stacks-block-info? time u1)",
            ClarityVersion::Clarity3,
        );
        let Ok(Some(Value::UInt(stacks_block_time_1))) = snippet_1.expect_optional() else {
            panic!("Unexpected result");
        };

        let snippet_2 = run_snippet(
            &mut interpreter,
            "(get-stacks-block-info? time u101)",
            ClarityVersion::Clarity3,
        );
        let Ok(Some(Value::UInt(stacks_block_time_2))) = snippet_2.expect_optional() else {
            panic!("Unexpected result");
        };
        assert_eq!(stacks_block_time_2 - stacks_block_time_1, 1000);

        let _ = interpreter.advance_burn_chain_tip(1);
        let _ = interpreter.advance_stacks_chain_tip(1);

        let snippet_3 = run_snippet(
            &mut interpreter,
            "(get-tenure-info? time u4)",
            ClarityVersion::Clarity3,
        );
        let Ok(Some(Value::UInt(tenure_height_1))) = snippet_3.expect_optional() else {
            panic!("Unexpected result");
        };

        let snippet_4 = run_snippet(
            &mut interpreter,
            "(get-tenure-info? time (- stacks-block-height u1))",
            ClarityVersion::Clarity3,
        );
        let Ok(Some(Value::UInt(tenure_height_2))) = snippet_4.expect_optional() else {
            panic!("Unexpected result");
        };

        assert_eq!(1030, tenure_height_2 - tenure_height_1);
    }

    #[test]
    fn can_call_a_public_function() {
        let mut interpreter = get_interpreter(None);

        let contract = ClarityContractBuilder::default()
            .code_source("(define-public (public-func) (ok true))".into())
            .build();
        let _ = deploy_contract(&mut interpreter, &contract);

        let allow_private = false;
        let result = interpreter.call_contract_fn(
            &contract
                .expect_resolved_contract_identifier(Some(&StandardPrincipalData::transient())),
            "public-func",
            &[],
            StacksEpochId::Epoch24,
            ClarityVersion::Clarity2,
            false,
            allow_private,
            vec![],
        );

        assert!(result.is_ok());
        let ExecutionResult { result, .. } = result.unwrap();

        assert!(
            matches!(result, EvaluationResult::Snippet(SnippetEvaluationResult { result }) if result == Value::okay_true())
        );
    }

    #[test]
    fn can_report_contract_call_errors() {
        let mut interpreter = get_interpreter(None);

        let contract = ClarityContractBuilder::default()
            .code_source("(define-public (public-func) (ok true))".into())
            .build();
        let _ = deploy_contract(&mut interpreter, &contract);

        let result = interpreter.call_contract_fn(
            &contract
                .expect_resolved_contract_identifier(Some(&StandardPrincipalData::transient())),
            "undefined-function",
            &[],
            StacksEpochId::Epoch24,
            ClarityVersion::Clarity2,
            false,
            false,
            vec![],
        );

        assert!(result.is_err());
        matches!(result.unwrap_err(), ContractCallError::NoSuchFunction(_));

        let result = interpreter.call_contract_fn(
            &QualifiedContractIdentifier {
                issuer: StandardPrincipalData::transient(),
                name: "unexisting".into(),
            },
            "undefined-function",
            &[],
            StacksEpochId::Epoch24,
            ClarityVersion::Clarity2,
            false,
            false,
            vec![],
        );

        assert!(result.is_err());
        matches!(result.unwrap_err(), ContractCallError::NoSuchContract(_));
    }

    #[test]
    fn can_call_a_private_function() {
        let mut interpreter = get_interpreter(None);

        let contract = ClarityContractBuilder::default()
            .code_source("(define-private (private-func) true)".into())
            .build();
        let _ = deploy_contract(&mut interpreter, &contract);

        let allow_private = true;
        let result = interpreter.call_contract_fn(
            &contract
                .expect_resolved_contract_identifier(Some(&StandardPrincipalData::transient())),
            "private-func",
            &[],
            StacksEpochId::Epoch24,
            ClarityVersion::Clarity2,
            false,
            allow_private,
            vec![],
        );

        assert!(result.is_ok());
        let ExecutionResult { result, .. } = result.unwrap();

        assert!(
            matches!(result, EvaluationResult::Snippet(SnippetEvaluationResult { result }) if result == Value::Bool(true))
        );
    }

    #[test]
    fn can_not_call_a_private_function_without_allow_private() {
        let mut interpreter = get_interpreter(None);

        let contract = ClarityContractBuilder::default()
            .code_source("(define-private (private-func) true)".into())
            .build();
        let _ = deploy_contract(&mut interpreter, &contract);

        let allow_private = false;
        let result = interpreter.call_contract_fn(
            &contract
                .expect_resolved_contract_identifier(Some(&StandardPrincipalData::transient())),
            "private-func",
            &[],
            StacksEpochId::Epoch24,
            ClarityVersion::Clarity2,
            false,
            allow_private,
            vec![],
        );

        assert!(result.is_err());
        let err = result.unwrap_err();
        match err {
            ContractCallError::NoSuchFunction(ref function_name) => {
                assert!(function_name.contains("private-func"));
            }
            _ => panic!("Expected NoSuchFunction error"),
        }
    }

    #[test]
    fn can_call_clarity_4_contract_hash() {
        let mut interpreter = get_interpreter(None);
        interpreter.set_current_epoch(StacksEpochId::Epoch33);

        let dummy_contract = ClarityContractBuilder::default()
            .name("dummy-contract")
            .code_source("(define-read-only (oktrue) (ok true))".into())
            .epoch(StacksEpochId::Epoch33)
            .build();
        let _ = deploy_contract(&mut interpreter, &dummy_contract);

        let contract = ClarityContractBuilder::default()
            .code_source("(define-read-only (get-hash) (contract-hash? .dummy-contract))".into())
            .epoch(StacksEpochId::Epoch33)
            .clarity_version(ClarityVersion::Clarity4)
            .build();
        let _ = deploy_contract(&mut interpreter, &contract);

        let get_hash_result = interpreter.call_contract_fn(
            &contract
                .expect_resolved_contract_identifier(Some(&StandardPrincipalData::transient())),
            "get-hash",
            &[],
            StacksEpochId::Epoch33,
            ClarityVersion::Clarity4,
            false,
            false,
            vec![],
        );

        // this hash changes if the contract id or the block height at which it is deployed changes
        let expected_hex = "ac2f50c3295fb458c70e141eaa36a3a4020fac270afd1504c475a48ab398545f";
        let expected_bytes = hex_bytes(expected_hex).unwrap();
        assert_execution_result_value(
            get_hash_result,
            Value::okay(Value::buff_from(expected_bytes).unwrap()).unwrap(),
        );
    }

    #[test]
    fn can_call_clarity_4_block_time() {
        let mut interpreter = get_interpreter(None);
        interpreter.set_current_epoch(StacksEpochId::Epoch33);
        let _ = interpreter.advance_stacks_chain_tip(2);

        let src = indoc!(
            r#"(define-read-only (get-time) {
                previous: (get-stacks-block-info? time (- stacks-block-height u1)),
                current: stacks-block-time,
            })"#
        );
        let contract = ClarityContractBuilder::default()
            .code_source(src.into())
            .epoch(StacksEpochId::Epoch33)
            .clarity_version(ClarityVersion::Clarity4)
            .build();
        let _ = deploy_contract(&mut interpreter, &contract);

        let get_time = interpreter.call_contract_fn(
            &contract
                .expect_resolved_contract_identifier(Some(&StandardPrincipalData::transient())),
            "get-time",
            &[],
            StacksEpochId::Epoch33,
            ClarityVersion::Clarity4,
            false,
            false,
            vec![],
        );

        let value = get_evaluation_result(get_time).expect_tuple().unwrap();
        let previous = value
            .data_map
            .get("previous")
            .and_then(|v| v.clone().expect_optional().unwrap())
            .and_then(|v| v.clone().expect_u128().ok())
            .expect("previous to be (some uint)");
        let current = value
            .data_map
            .get("current")
            .and_then(|v| v.clone().expect_u128().ok())
            .expect("current to be uint");
        assert_eq!(previous + 10, current);
    }

    #[test]
    fn can_call_clarity_4_current_contract() {
        let mut interpreter = get_interpreter(None);
        interpreter.set_current_epoch(StacksEpochId::Epoch33);

        let contract = ClarityContractBuilder::default()
            .name("my-contract")
            .code_source("(define-read-only (gcc) current-contract)".into())
            .epoch(StacksEpochId::Epoch33)
            .clarity_version(ClarityVersion::Clarity4)
            .build();
        let _ = deploy_contract(&mut interpreter, &contract);

        let result = interpreter.call_contract_fn(
            &contract
                .expect_resolved_contract_identifier(Some(&StandardPrincipalData::transient())),
            "gcc",
            &[],
            StacksEpochId::Epoch33,
            ClarityVersion::Clarity4,
            false,
            false,
            vec![],
        );

        assert_execution_result_value(
            result,
            Value::Principal(PrincipalData::Contract(QualifiedContractIdentifier {
                issuer: StandardPrincipalData::transient(),
                name: "my-contract".into(),
            })),
        );
    }
}
