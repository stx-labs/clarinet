use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Write as _;
use std::num::ParseIntError;

use clarity::codec::StacksMessageCodec;
use clarity::types::StacksEpochId;
use clarity::vm::ast::ContractAST;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::{
    ClarityVersion, CostSynthesis, EvalHook, EvaluationResult, ExecutionResult, ParsedContract,
    SymbolicExpression,
};
use clarity_types::types::{AssetIdentifier, PrincipalData, QualifiedContractIdentifier};
use clarity_types::Value;
use colored::Colorize;
use comfy_table::Table;

use super::diagnostic::output_diagnostic;
use super::hooks::logger::LoggerHook;
use super::hooks::perf::{CostField, PerfHook};
use super::interpreter::ContractCallError;
use super::{
    ClarityCodeSource, ClarityContract, ClarityInterpreter, ContractDeployer, Epoch,
    SessionSettings,
};
use crate::analysis::coverage::CoverageHook;
use crate::repl::boot;
use crate::repl::clarity_values::value_to_string;
use crate::repl::hooks::tracer::TracerHook;
use crate::repl::settings::Account;
use crate::utils::serialize_event;

pub type ExecutionResultMap =
    BTreeMap<QualifiedContractIdentifier, Result<ExecutionResult, Vec<Diagnostic>>>;

#[derive(Clone, Debug, Serialize)]
pub struct CostsReport {
    pub test_name: String,
    pub contract_id: String,
    pub method: String,
    pub args: Vec<String>,
    pub cost_result: CostSynthesis,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssetIdentifierParseError {
    NoPeriod,
    EndsWithPeriod,
    ContractIdentifierParseError,
    NoPrefixPeriod,
}

impl std::fmt::Display for AssetIdentifierParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoPeriod => write!(f, "no period"),
            Self::EndsWithPeriod => write!(f, "ends with period"),
            Self::ContractIdentifierParseError => write!(f, "error parsing ContractIdentifier"),
            Self::NoPrefixPeriod => write!(f, "no period in the prefix of the asset name"),
        }
    }
}

fn set_up_accounts(accounts: &[Account], interpreter: &mut ClarityInterpreter) {
    for account in accounts {
        let Ok(recipient) = PrincipalData::parse(&account.address) else {
            println!("{}", "Unable to parse address to credit".red());
            continue;
        };
        let _ = interpreter
            .mint_stx_balance(recipient, account.balance)
            .inspect_err(|e| println!("{}", e.red()));
    }
}

fn deploy_boot_contracts(
    settings: &SessionSettings,
    interpreter: &mut ClarityInterpreter,
) -> ExecutionResultMap {
    if settings.repl_settings.remote_data.enabled {
        return BTreeMap::new();
    }
    let mut boot_contracts = BTreeMap::new();

    // Load boot contracts (with custom overrides if specified)
    let boot_contracts_data = if settings.override_boot_contracts_source.is_empty() {
        boot::BOOT_CONTRACTS_DATA.clone()
    } else {
        boot::get_boot_contracts_data_with_overrides(&settings.override_boot_contracts_source)
    };

    for (contract_id, (contract, ast)) in boot_contracts_data {
        let result = interpreter.run(&contract, Some(&ast), false, None);
        if let Err(errs) = &result {
            for e in errs {
                ueprint!("Error deploying boot contract {contract_id}: {}", e.message);
            }
        }
        boot_contracts.insert(contract_id, result);
    }
    boot_contracts
}

#[derive(Clone)]
pub struct Session {
    pub settings: SessionSettings,
    pub boot_contracts: ExecutionResultMap,
    pub contracts: BTreeMap<QualifiedContractIdentifier, ParsedContract>,
    pub interpreter: ClarityInterpreter,
    pub show_costs: bool,
    pub last_contract_call_trace: Option<String>,

    coverage_hook: Option<CoverageHook>,
    logger_hook: Option<LoggerHook>,
    perf_hook: Option<PerfHook>,
}

impl Session {
    pub fn new(settings: SessionSettings) -> Self {
        let mut interpreter = ClarityInterpreter::new(
            settings.get_default_sender(),
            settings.repl_settings.clone(),
            settings.cache_location.clone(),
        );

        set_up_accounts(&settings.initial_accounts, &mut interpreter);
        let boot_contracts = deploy_boot_contracts(&settings, &mut interpreter);

        Self {
            interpreter,
            boot_contracts,
            contracts: BTreeMap::new(),
            show_costs: false,
            settings,
            last_contract_call_trace: None,

            coverage_hook: None,
            logger_hook: None,
            perf_hook: None,
        }
    }

    pub fn enable_coverage_hook(&mut self) {
        self.coverage_hook = Some(CoverageHook::new());
    }

    pub fn enable_logger_hook(&mut self) {
        self.logger_hook = Some(LoggerHook::new());
    }

    pub fn enable_performance(&mut self, cost_field: CostField) {
        self.perf_hook = Some(PerfHook::new(cost_field));
    }

    pub fn set_test_name(&mut self, name: String) {
        if let Some(coverage_hook) = &mut self.coverage_hook {
            coverage_hook.set_current_test_name(name);
        }
    }

    pub fn collect_lcov_content(
        &mut self,
        asts: &BTreeMap<QualifiedContractIdentifier, ContractAST>,
        contract_paths: &BTreeMap<String, String>,
    ) -> String {
        if let Some(coverage_hook) = &mut self.coverage_hook {
            println!("Collecting coverage data...");
            coverage_hook.collect_lcov_content(asts, contract_paths)
        } else {
            "".to_string()
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn process_console_input(&mut self, command: &str) -> (bool, Vec<String>) {
        let mut output = Vec::<String>::new();

        let mut reload = false;
        match command {
            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::reload") => reload = true,
            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::read") => self.read(&mut output, cmd),
            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::debug") => self.debug(&mut output, cmd),
            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::trace") => self.trace(&mut output, cmd),
            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::perf") => self.perf(&mut output, cmd),
            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::get_costs") => self.get_costs(&mut output, cmd),

            cmd if cmd.starts_with("::") => {
                output.push(self.handle_command(cmd));
            }

            snippet => {
                let _ = self.run_snippet(&mut output, self.show_costs, snippet);
                return (false, output);
            }
        }

        (reload, output)
    }

    pub fn handle_command(&mut self, command: &str) -> String {
        match command {
            "::help" => self.display_help(),

            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::functions") => self.display_functions(),
            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::keywords") => self.keywords(),
            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::describe") => self.display_doc(cmd),
            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::toggle_costs") => self.toggle_costs(),
            #[cfg(not(target_arch = "wasm32"))]
            cmd if cmd.starts_with("::toggle_timings") => self.toggle_timings(),

            cmd if cmd.starts_with("::mint_stx") => self.mint_stx(cmd),
            cmd if cmd.starts_with("::mint_ft") => self.mint_ft(cmd),
            cmd if cmd.starts_with("::set_tx_sender") => self.parse_and_set_tx_sender(cmd),
            cmd if cmd.starts_with("::get_assets_maps") => {
                self.get_accounts().unwrap_or("No account found".into())
            }
            cmd if cmd.starts_with("::get_contracts") => {
                self.get_contracts().unwrap_or("No contract found".into())
            }
            cmd if cmd.starts_with("::get_burn_block_height") => self.get_burn_block_height(),
            cmd if cmd.starts_with("::get_stacks_block_height") => self.get_block_height(),
            cmd if cmd.starts_with("::get_block_height") => self.get_block_height(),
            cmd if cmd.starts_with("::advance_chain_tip") => self.parse_and_advance_chain_tip(cmd),
            cmd if cmd.starts_with("::advance_stacks_chain_tip") => {
                self.parse_and_advance_stacks_chain_tip(cmd)
            }
            cmd if cmd.starts_with("::advance_burn_chain_tip") => {
                self.parse_and_advance_burn_chain_tip(cmd)
            }
            cmd if cmd.starts_with("::get_epoch") => self.get_epoch(),
            cmd if cmd.starts_with("::set_epoch") => self.set_epoch(cmd),
            cmd if cmd.starts_with("::encode") => self.encode(cmd),
            cmd if cmd.starts_with("::decode") => self.decode(cmd),
            cmd if cmd.starts_with("::get_constant") => self.get_constant(cmd),
            cmd if cmd.starts_with("::get_data_var") => self.get_data_var(cmd),
            cmd if cmd.starts_with("::get_map_val") => self.get_map_val(cmd),

            _ => "Invalid command. Try `::help`".yellow().to_string(),
        }
    }

    pub fn desugar_contract_id(
        default_deployer: &str,
        contract: &str,
    ) -> Result<QualifiedContractIdentifier, String> {
        let parts_count = contract.split('.').count();
        if parts_count > 2 {
            return Err(format!("Invalid contract identifier: {contract}"));
        }

        let is_qualified = parts_count == 2 && &contract[0..1] != ".";
        let contract_id = if is_qualified {
            contract.to_string()
        } else if &contract[0..1] != "." {
            format!("{}.{}", default_deployer, contract,)
        } else {
            format!("{}{}", default_deployer, contract,)
        };

        QualifiedContractIdentifier::parse(&contract_id).map_err(|e| e.to_string())
    }
    #[cfg(not(target_arch = "wasm32"))]
    fn contract_successfully_stored(
        &mut self,
        output: &mut Vec<String>,
        contract: &ParsedContract,
    ) {
        // Handle the successful storage of the contract
        let snippet = green!(
            "→ {} contract successfully stored.",
            contract.contract_identifier
        );
        output.push(snippet.to_string());
        let public_functions: Vec<_> = contract.analysis.public_function_types.keys().collect();
        let read_only_functions: Vec<_> =
            contract.analysis.read_only_function_types.keys().collect();
        if !(public_functions.is_empty() && read_only_functions.is_empty()) {
            output.push(
                "Use (contract-call? ...) for invoking the public and read-only functions:"
                    .to_string(),
            );
        }

        if !public_functions.is_empty() {
            output.push("  Public functions:".to_string());
            for func in &public_functions {
                output.push(format!("    - {func}"));
            }
        }
        if !read_only_functions.is_empty() {
            output.push("  Read-only functions:".to_string());
            for func in &read_only_functions {
                output.push(format!("    - {func}"));
            }
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn run_snippet(
        &mut self,
        output: &mut Vec<String>,
        cost_track: bool,
        cmd: &str,
    ) -> Result<ExecutionResult, Vec<Diagnostic>> {
        let (mut result, cost, execution_result) =
            match self.formatted_interpretation(cmd.to_string(), None, cost_track, None) {
                Ok((mut output, result)) => {
                    if let EvaluationResult::Contract(contract_result) = result.result.clone() {
                        self.contract_successfully_stored(&mut output, &contract_result.contract);
                    };
                    (output, result.cost.clone(), Ok(result))
                }
                Err((err_output, diagnostics)) => (err_output, None, Err(diagnostics)),
            };

        if let Some(cost) = cost {
            let headers = [
                "".to_string(),
                "Consumed".to_string(),
                "Limit".to_string(),
                "Percentage".to_string(),
            ];

            let mut table = comfy_table::Table::new();
            table.add_row(headers);
            table.add_row(vec![
                "Runtime",
                &cost.total.runtime.to_string(),
                &cost.limit.runtime.to_string(),
                &(Self::get_costs_percentage(&cost.total.runtime, &cost.limit.runtime)),
            ]);
            table.add_row(vec![
                "Read count",
                &cost.total.read_count.to_string(),
                &cost.limit.read_count.to_string(),
                &Self::get_costs_percentage(&cost.total.read_count, &cost.limit.read_count),
            ]);
            table.add_row(vec![
                "Read length (bytes)",
                &cost.total.read_length.to_string(),
                &cost.limit.read_length.to_string(),
                &Self::get_costs_percentage(&cost.total.read_length, &cost.limit.read_length),
            ]);
            table.add_row(vec![
                "Write count",
                &cost.total.write_count.to_string(),
                &cost.limit.write_count.to_string(),
                &Self::get_costs_percentage(&cost.total.write_count, &cost.limit.write_count),
            ]);
            table.add_row(vec![
                "Write length (bytes)",
                &cost.total.write_length.to_string(),
                &cost.limit.write_length.to_string(),
                &(Self::get_costs_percentage(&cost.total.write_length, &cost.limit.write_length)),
            ]);
            output.push(table.to_string());
        }
        output.append(&mut result);
        execution_result
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn get_costs_percentage(consumed: &u64, limit: &u64) -> String {
        let calc = (*consumed as f64 / *limit as f64) * 100_f64;

        format!("{calc:.2} %")
    }

    pub fn formatted_interpretation(
        &mut self,
        snippet: String,
        name: Option<String>,
        cost_track: bool,
        eval_hooks: Option<Vec<&mut dyn EvalHook>>,
    ) -> Result<(Vec<String>, ExecutionResult), (Vec<String>, Vec<Diagnostic>)> {
        let result = self.eval_with_hooks(snippet.to_string(), eval_hooks, cost_track);
        let mut output = Vec::<String>::new();
        let formatted_lines: Vec<String> = snippet.lines().map(|l| l.to_string()).collect();
        let contract_name = name.unwrap_or("<stdin>".to_string());

        match result {
            Ok(result) => {
                for diagnostic in &result.diagnostics {
                    output.append(&mut output_diagnostic(
                        diagnostic,
                        &contract_name,
                        &formatted_lines,
                    ));
                }
                if !result.events.is_empty() {
                    output.push(black!("Events emitted").to_string());
                    for event in result.events.iter() {
                        output.push(black!("{}", serialize_event(event)).to_string());
                    }
                }
                match &result.result {
                    EvaluationResult::Contract(contract_result) => {
                        if let Some(value) = &contract_result.result {
                            output.push(format!("{value}").green().to_string());
                        }
                    }
                    EvaluationResult::Snippet(snippet_result) => {
                        output.push(value_to_string(&snippet_result.result).green().to_string())
                    }
                }
                Ok((output, result))
            }
            Err(diagnostics) => {
                for d in &diagnostics {
                    output.append(&mut output_diagnostic(d, &contract_name, &formatted_lines));
                }
                Err((output, diagnostics))
            }
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn debug(&mut self, output: &mut Vec<String>, cmd: &str) {
        use crate::repl::debug::cli::CLIDebugger;

        let Some((_, snippet)) = cmd.split_once(' ') else {
            return output.push("Usage: ::debug <expr>".red().to_string());
        };

        let mut debugger = CLIDebugger::new(&QualifiedContractIdentifier::transient(), snippet);

        let mut result = match self.formatted_interpretation(
            snippet.to_string(),
            None,
            true,
            Some(vec![&mut debugger]),
        ) {
            Ok((mut output, result)) => {
                if let EvaluationResult::Contract(contract_result) = result.result {
                    self.contract_successfully_stored(&mut output, &contract_result.contract);
                };
                output
            }
            Err((result, _)) => result,
        };
        output.append(&mut result);
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn trace(&mut self, output: &mut Vec<String>, cmd: &str) {
        use super::hooks::tracer::TracerHook;

        let Some((_, snippet)) = cmd.split_once(' ') else {
            return output.push("Usage: ::trace <expr>".red().to_string());
        };

        let mut tracer = TracerHook::new();

        match self.eval_with_hooks(snippet.to_string(), Some(vec![&mut tracer]), false) {
            Ok(_) => (),
            Err(diagnostics) => {
                let lines = snippet.lines();
                let formatted_lines: Vec<String> = lines.map(|l| l.to_string()).collect();
                for d in diagnostics {
                    output.append(&mut output_diagnostic(&d, "<snippet>", &formatted_lines));
                }
            }
        };

        println!("{}  {}", snippet, black!("<console>"));
        println!("{}", tracer.output.join("\n"));
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn perf(&mut self, output: &mut Vec<String>, cmd: &str) {
        use super::hooks::perf::{CostField, PerfHook};

        let parts: Vec<&str> = cmd.split_whitespace().collect();
        if parts.len() < 3 {
            return output.push("Usage: ::perf <cost_field> <expr>".red().to_string());
        }

        let cost_field_str = parts[1];
        let snippet = parts[2..].join(" ");

        let cost_field = CostField::from(cost_field_str);

        let mut perf = PerfHook::new(cost_field);

        match self.eval_with_hooks(snippet.to_string(), Some(vec![&mut perf]), true) {
            Ok(_) => {
                output.push(
                    format!(
                        "Performance data written to perf.data (tracking {})",
                        cost_field.name()
                    )
                    .green()
                    .to_string(),
                );
            }
            Err(diagnostics) => {
                let lines = snippet.lines();
                let formatted_lines: Vec<String> = lines.map(|l| l.to_string()).collect();
                for d in diagnostics {
                    output.append(&mut output_diagnostic(&d, "<snippet>", &formatted_lines));
                }
            }
        };
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn read(&mut self, output: &mut Vec<String>, cmd: &str) {
        let Some((_, filename)) = cmd.split_once(' ') else {
            return output.push("Usage: ::read <filename>".red().to_string());
        };

        match std::fs::read_to_string(filename) {
            Ok(snippet) => {
                let _ = self.run_snippet(output, self.show_costs, &snippet);
            }
            Err(err) => output.push(
                format!("unable to read {filename}: {err}")
                    .red()
                    .to_string(),
            ),
        };
    }

    pub fn stx_transfer(
        &mut self,
        amount: u64,
        recipient: &str,
    ) -> Result<ExecutionResult, Vec<Diagnostic>> {
        let snippet = format!("(stx-transfer? u{amount} tx-sender '{recipient})");
        self.eval(snippet, false)
    }

    pub fn deploy_contract(
        &mut self,
        contract: &ClarityContract,
        cost_track: bool,
        ast: Option<&ContractAST>,
    ) -> Result<ExecutionResult, Vec<Diagnostic>> {
        let current_epoch = self.interpreter.datastore.get_current_epoch();
        if contract.epoch.resolve() != current_epoch {
            let diagnostic = Diagnostic {
                level: Level::Error,
                message: format!(
                    "contract epoch ({}) does not match current epoch ({})",
                    contract.epoch, current_epoch
                ),
                spans: vec![],
                suggestion: None,
            };
            return Err(vec![diagnostic]);
        }

        let mut hooks: Vec<&mut dyn EvalHook> = vec![];
        if let Some(ref mut coverage_hook) = self.coverage_hook {
            hooks.push(coverage_hook);
        }
        if let Some(ref mut logger_hook) = self.logger_hook {
            hooks.push(logger_hook);
        }
        if let Some(ref mut perf_hook) = self.perf_hook {
            hooks.push(perf_hook);
        }

        if contract.clarity_version > ClarityVersion::default_for_epoch(contract.epoch.resolve()) {
            let diagnostic = Diagnostic {
                level: Level::Error,
                message: format!(
                    "{} can not be used with {}",
                    contract.clarity_version, contract.epoch
                ),
                spans: vec![],
                suggestion: None,
            };
            return Err(vec![diagnostic]);
        }

        let contract_id =
            contract.expect_resolved_contract_identifier(Some(&self.interpreter.get_tx_sender()));

        let result = self.interpreter.run(contract, ast, cost_track, Some(hooks));

        result.inspect(|result| {
            if let EvaluationResult::Contract(contract_result) = &result.result {
                self.contracts
                    .insert(contract_id.clone(), contract_result.contract.clone());
            }
        })
    }

    pub fn call_contract_fn(
        &mut self,
        contract: &str,
        method: &str,
        args: &[SymbolicExpression],
        sender: &str,
        allow_private: bool,
        track_costs: bool,
    ) -> Result<ExecutionResult, Vec<Diagnostic>> {
        let initial_tx_sender = self.get_tx_sender();

        let contract_id = Self::desugar_contract_id(&initial_tx_sender, contract).map_err(|e| {
            vec![Diagnostic {
                level: Level::Error,
                message: e,
                spans: vec![],
                suggestion: None,
            }]
        })?;

        self.set_tx_sender(sender);

        let mut tracer_hook = TracerHook::new();
        let mut hooks: Vec<&mut dyn EvalHook> = vec![&mut tracer_hook];
        if let Some(ref mut coverage_hook) = self.coverage_hook {
            hooks.push(coverage_hook);
        }
        if let Some(ref mut logger_hook) = self.logger_hook {
            hooks.push(logger_hook);
        }
        if let Some(ref mut perf_hook) = self.perf_hook {
            hooks.push(perf_hook);
        }

        let current_epoch = self.interpreter.datastore.get_current_epoch();
        let contract_call_result = self.interpreter.call_contract_fn(
            &contract_id,
            method,
            args,
            current_epoch,
            ClarityVersion::default_for_epoch(current_epoch),
            track_costs,
            allow_private,
            hooks,
        );
        self.set_tx_sender(&initial_tx_sender);
        self.last_contract_call_trace = Some(tracer_hook.output.join("\n"));

        contract_call_result.map_err(|e| {
            ueprint!("{}", tracer_hook.output.join("\n"));
            if let Some(traced_error) = tracer_hook.error {
                ueprint!("{}", traced_error);
            }
            let contract_id_str = contract_id.to_string();
            let user_friendly_message = match e {
                ContractCallError::NoSuchContract(_) => {
                    format!("Contract '{contract_id_str}' does not exist")
                }
                ContractCallError::NoSuchFunction(_) => {
                    format!("Method '{method}' does not exist on contract '{contract_id_str}'")
                }
                ContractCallError::Uncategorized(message) => {
                    format!("Error calling contract function '{method}': {message}")
                }
            };
            vec![Diagnostic {
                level: Level::Error,
                message: user_friendly_message,
                spans: vec![],
                suggestion: None,
            }]
        })
    }

    /// Run a snippet as a contract deployment
    pub fn eval(
        &mut self,
        snippet: String,
        cost_track: bool,
    ) -> Result<ExecutionResult, Vec<Diagnostic>> {
        let current_epoch = self.interpreter.datastore.get_current_epoch();
        let contract = ClarityContract {
            code_source: ClarityCodeSource::ContractInMemory(snippet),
            name: format!("contract-{}", self.contracts.len()),
            deployer: ContractDeployer::DefaultDeployer,
            clarity_version: ClarityVersion::default_for_epoch(current_epoch),
            epoch: Epoch::Specific(current_epoch),
        };
        let contract_identifier =
            contract.expect_resolved_contract_identifier(Some(&self.interpreter.get_tx_sender()));

        let mut hooks: Vec<&mut dyn EvalHook> = vec![];
        if let Some(ref mut coverage_hook) = self.coverage_hook {
            hooks.push(coverage_hook);
        }
        if let Some(ref mut logger_hook) = self.logger_hook {
            hooks.push(logger_hook);
        }
        if let Some(ref mut perf_hook) = self.perf_hook {
            hooks.push(perf_hook);
        }

        let result = self
            .interpreter
            .run(&contract, None, cost_track, Some(hooks));

        result.inspect(|result| {
            if let EvaluationResult::Contract(contract_result) = &result.result {
                self.contracts
                    .insert(contract_identifier, contract_result.contract.clone());
            }
        })
    }

    /// Evaluate a Clarity snippet in order to use it as Clarity function arguments
    pub fn eval_clarity_string(&mut self, snippet: &str) -> SymbolicExpression {
        let eval_result = self.eval(snippet.to_string(), false);
        let value = match eval_result.unwrap().result {
            EvaluationResult::Contract(_) => unreachable!(),
            EvaluationResult::Snippet(snippet_result) => snippet_result.result,
        };
        SymbolicExpression::atom_value(value)
    }

    pub fn eval_with_hooks(
        &mut self,
        snippet: String,
        eval_hooks: Option<Vec<&mut dyn EvalHook>>,
        cost_track: bool,
    ) -> Result<ExecutionResult, Vec<Diagnostic>> {
        let current_epoch = self.interpreter.datastore.get_current_epoch();
        let contract = ClarityContract {
            code_source: ClarityCodeSource::ContractInMemory(snippet),
            name: format!("contract-{}", self.contracts.len()),
            deployer: ContractDeployer::DefaultDeployer,
            clarity_version: ClarityVersion::default_for_epoch(current_epoch),
            epoch: Epoch::Specific(current_epoch),
        };
        let contract_identifier =
            contract.expect_resolved_contract_identifier(Some(&self.interpreter.get_tx_sender()));

        let result = self
            .interpreter
            .run(&contract, None, cost_track, eval_hooks);

        match result {
            Ok(result) => {
                if let EvaluationResult::Contract(contract_result) = &result.result {
                    self.contracts
                        .insert(contract_identifier, contract_result.contract.clone());
                };
                Ok(result)
            }
            Err(res) => Err(res),
        }
    }

    fn display_help(&self) -> String {
        let mut output: Vec<String> = vec![];

        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
            "::functions\t\t\t\tDisplay all the native functions available in clarity".yellow()
        ));
        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
            "::keywords\t\t\t\tDisplay all the native keywords available in clarity".yellow()
        ));
        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
                "::describe <function> | <keyword>\tDisplay documentation for a given native function or keyword".yellow()
        ));
        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
            "::toggle_costs\t\t\t\tDisplay cost analysis after every expression".yellow()
        ));
        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
            "::toggle_timings\t\t\tDisplay the execution duration".yellow()
        ));

        output.push(format!(
            "{}",
            "::mint_stx <principal> <amount>\t\tMint STX balance for a given principal".yellow()
        ));
        output.push(format!(
            "{}",
            "::mint_ft <asset identifier> <principal> <amount>\t\tMint FT balance for a given asset (CONTRACT_IDENTIFIER.ASSET_NAME) and principal".yellow()
        ));
        output.push(format!(
            "{}",
            "::set_tx_sender <principal>\t\tSet tx-sender variable to principal".yellow()
        ));
        output.push(format!(
            "{}",
            "::get_assets_maps\t\t\tGet assets maps for active accounts".yellow()
        ));
        output.push(format!(
            "{}",
            "::get_contracts\t\t\t\tGet contracts".yellow()
        ));
        output.push(format!(
            "{}",
            "::get_block_height\t\t\tGet current block height".yellow()
        ));
        output.push(format!(
            "{}",
            "::advance_chain_tip <count>\t\tSimulate mining of <count> blocks".yellow()
        ));
        output.push(format!(
            "{}",
            "::advance_stacks_chain_tip <count>\tSimulate mining of <count> stacks blocks".yellow()
        ));
        output.push(format!(
            "{}",
            "::advance_burn_chain_tip <count>\tSimulate mining of <count> burnchain blocks"
                .yellow()
        ));
        output.push(format!(
            "{}",
            "::set_epoch <epoch>\t\t\tUpdate the current epoch".yellow()
        ));
        output.push(format!(
            "{}",
            "::get_epoch\t\t\t\tGet current epoch".yellow()
        ));

        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
            "::debug <expr>\t\t\t\tStart an interactive debug session executing <expr>".yellow()
        ));
        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
            "::trace <expr>\t\t\t\tGenerate an execution trace for <expr>".yellow()
        ));
        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
            "::perf <cost_field> <expr>\t\tGenerate performance data tracking specific cost field"
                .yellow()
        ));
        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
            "::get_costs <expr>\t\t\tDisplay the cost analysis".yellow()
        ));
        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
            "::reload \t\t\t\tReload the existing contract(s) in the session".yellow()
        ));
        #[cfg(not(target_arch = "wasm32"))]
        output.push(format!(
            "{}",
            "::read <filename>\t\t\tRead expressions from a file".yellow()
        ));

        output.push(format!(
            "{}",
            "::encode <expr>\t\t\t\tEncode an expression to a Clarity Value bytes representation"
                .yellow()
        ));
        output.push(format!(
            "{}",
            "::decode <bytes>\t\t\tDecode a Clarity Value bytes representation".yellow()
        ));
        output.push(format!(
            "{}",
            "::get_constant <contract> <constant>\tGet constant value from a contract".yellow()
        ));
        output.push(format!(
            "{}",
            "::get_data_var <contract> <var>\t\tGet data variable value from a contract".yellow()
        ));
        output.push(format!(
            "{}",
            "::get_map_val <contract> <map> <key>\tGet map value from a contract".yellow()
        ));

        output.join("\n")
    }

    fn parse_and_advance_chain_tip(&mut self, command: &str) -> String {
        let args: Vec<_> = command.split(' ').skip(1).collect();
        let Ok(count) = args.first().unwrap_or(&"1").parse::<u32>() else {
            return format!("{}", "Unable to parse count".red());
        };

        let _ = self.advance_chain_tip(count);
        format!(
            "new burn height: {}\nnew stacks height: {}",
            self.interpreter.datastore.get_current_burn_block_height(),
            self.interpreter.datastore.get_current_stacks_block_height(),
        )
        .green()
        .to_string()
    }

    fn parse_and_advance_burn_chain_tip(&mut self, command: &str) -> String {
        let args: Vec<_> = command.split(' ').skip(1).collect();
        let Ok(count) = args.first().unwrap_or(&"1").parse::<u32>() else {
            return format!("{}", "Unable to parse count".red());
        };

        let _ = self.advance_burn_chain_tip(count);
        format!(
            "new burn height: {}\nnew stacks height: {}",
            self.interpreter.datastore.get_current_burn_block_height(),
            self.interpreter.datastore.get_current_stacks_block_height(),
        )
        .green()
        .to_string()
    }

    fn parse_and_advance_stacks_chain_tip(&mut self, command: &str) -> String {
        let args: Vec<_> = command.split(' ').skip(1).collect();
        let Ok(count) = args.first().unwrap_or(&"1").parse::<u32>() else {
            return format!("{}", "Unable to parse count".red());
        };

        match self.advance_stacks_chain_tip(count) {
            Ok(_) => format!(
                "new burn height: {}\nnew stacks height: {}",
                self.interpreter.datastore.get_current_burn_block_height(),
                self.interpreter.datastore.get_current_stacks_block_height(),
            )
            .green()
            .to_string(),
            Err(_) => format!(
                "{}",
                "advance_stacks_chain_tip can't be called in epoch lower than 3.0".red()
            ),
        }
    }

    pub fn advance_chain_tip(&mut self, count: u32) -> u32 {
        let current_epoch = self.interpreter.datastore.get_current_epoch();
        if current_epoch < StacksEpochId::Epoch30 {
            self.advance_burn_chain_tip(count)
        } else {
            self.advance_stacks_chain_tip(count)
                .expect("Epoch checked already")
        }
    }

    pub fn advance_burn_chain_tip(&mut self, count: u32) -> u32 {
        self.interpreter.advance_burn_chain_tip(count)
    }

    pub fn advance_stacks_chain_tip(&mut self, count: u32) -> Result<u32, String> {
        self.interpreter.advance_stacks_chain_tip(count)
    }

    fn parse_and_set_tx_sender(&mut self, command: &str) -> String {
        let args: Vec<_> = command.split(' ').filter(|&s| !s.is_empty()).collect();

        if args.len() != 2 {
            return format!("{}", "Usage: ::set_tx_sender <address>".red());
        }

        let tx_sender = args[1];

        match PrincipalData::parse_standard_principal(args[1]) {
            Ok(address) => {
                self.interpreter.set_tx_sender(address);
                format!("tx-sender switched to {tx_sender}")
            }
            _ => format!("{}", "Unable to parse the address".red()),
        }
    }

    pub fn set_tx_sender(&mut self, address: &str) {
        let tx_sender =
            PrincipalData::parse_standard_principal(address).expect("Unable to parse address");
        self.interpreter.set_tx_sender(tx_sender)
    }

    pub fn get_tx_sender(&self) -> String {
        self.interpreter.get_tx_sender().to_address()
    }

    fn get_block_height(&mut self) -> String {
        let height = self.interpreter.get_block_height();
        format!("Current height: {height}")
    }

    fn get_burn_block_height(&mut self) -> String {
        let height = self.interpreter.get_burn_block_height();
        format!("Current height: {height}")
    }

    fn get_account_name(&self, address: &str) -> Option<&str> {
        for account in self.settings.initial_accounts.iter() {
            if account.address == address {
                return Some(&account.name);
            }
        }
        None
    }

    pub fn get_assets_maps(&self) -> BTreeMap<String, BTreeMap<String, u128>> {
        self.interpreter.get_assets_maps()
    }

    pub fn toggle_costs(&mut self) -> String {
        self.show_costs = !self.show_costs;
        format!("Always show costs: {}", self.show_costs)
    }

    pub fn toggle_timings(&mut self) -> String {
        self.interpreter.repl_settings.show_timings = !self.interpreter.repl_settings.show_timings;
        format!(
            "Always show timings: {}",
            self.interpreter.repl_settings.show_timings
        )
        .green()
        .to_string()
    }

    pub fn get_epoch(&mut self) -> String {
        let current_epoch = self.interpreter.datastore.get_current_epoch();
        format!("Current epoch: {current_epoch}")
    }

    pub fn set_epoch(&mut self, cmd: &str) -> String {
        let epoch = match cmd.split_once(' ').map(|(_, epoch)| epoch) {
            Some("2.0") => StacksEpochId::Epoch20,
            Some("2.05") => StacksEpochId::Epoch2_05,
            Some("2.1") => StacksEpochId::Epoch21,
            Some("2.2") => StacksEpochId::Epoch22,
            Some("2.3") => StacksEpochId::Epoch23,
            Some("2.4") => StacksEpochId::Epoch24,
            Some("2.5") => StacksEpochId::Epoch25,
            Some("3.0") => StacksEpochId::Epoch30,
            Some("3.1") => StacksEpochId::Epoch31,
            Some("3.2") => StacksEpochId::Epoch32,
            Some("3.3") => StacksEpochId::Epoch33,
            _ => {
                return "Usage: ::set_epoch 2.0 | 2.05 | 2.1 | 2.2 | 2.3 | 2.4 | 2.5 | 3.0 | 3.1 | 3.2 | 3.3"
                    .red()
                    .to_string()
            }
        };
        self.update_epoch(epoch);
        format!("Epoch updated to: {epoch}").green().to_string()
    }

    pub fn update_epoch(&mut self, epoch: StacksEpochId) {
        self.interpreter.set_current_epoch(epoch);
        if epoch >= StacksEpochId::Epoch30 {
            self.interpreter.set_tenure_height();
        }
    }

    pub fn encode(&mut self, cmd: &str) -> String {
        let Some((_, snippet)) = cmd.split_once(' ') else {
            return "Usage: ::encode <expr>".red().to_string();
        };

        let result = self.eval(snippet.to_string(), false);
        match result {
            Ok(result) => {
                let mut tx_bytes = vec![];
                let value = match result.result {
                    EvaluationResult::Contract(contract_result) => {
                        if let Some(value) = contract_result.result {
                            value
                        } else {
                            return "No value".to_string();
                        }
                    }
                    EvaluationResult::Snippet(snippet_result) => snippet_result.result,
                };
                if let Err(e) = value.consensus_serialize(&mut tx_bytes) {
                    return format!("{e}").red().to_string();
                };
                let mut s = String::with_capacity(2 * tx_bytes.len());
                for byte in tx_bytes {
                    write!(s, "{byte:02x}").unwrap();
                }
                s.green().to_string()
            }
            Err(diagnostics) => {
                let lines: Vec<String> = snippet.split('\n').map(String::from).collect();
                let mut output: Vec<String> = diagnostics
                    .iter()
                    .flat_map(|d| output_diagnostic(d, "encode", &lines))
                    .collect();
                output.push("encoding failed".into());
                output.join("\n")
            }
        }
    }

    pub fn decode(&mut self, cmd: &str) -> String {
        let Some((_, byte_string)) = cmd.split_once(' ') else {
            return "Usage: ::decode <hex-bytes>".red().to_string();
        };
        let tx_bytes = match decode_hex(byte_string) {
            Ok(tx_bytes) => tx_bytes,
            Err(e) => return format!("Parsing error: {e}").red().to_string(),
        };

        let value = match Value::consensus_deserialize(&mut &tx_bytes[..]) {
            Ok(value) => value,
            Err(e) => return format!("{e}").red().to_string(),
        };

        format!("{}", value_to_string(&value).green())
    }

    pub fn get_constant(&mut self, cmd: &str) -> String {
        let args: Vec<_> = cmd.split_whitespace().skip(1).collect();

        if args.len() != 2 {
            return format!("{}", "Usage: ::get_constant <contract> <constant>".red());
        }

        let contract_name = args[0];
        let constant_name = args[1];

        let default_deployer = match self.settings.initial_deployer.as_ref() {
            Some(account) => account.address.clone(),
            None => self.get_tx_sender(),
        };

        let contract_id = match Self::desugar_contract_id(&default_deployer, contract_name) {
            Ok(id) => id,
            Err(e) => return format!("{} {}", "Invalid contract identifier:".red(), e),
        };

        let contract = match self.contracts.get(&contract_id) {
            Some(contract) => contract,
            None => return format!("{} {}", "Contract not found:".red(), contract_id),
        };

        // Search for constant in the contract's AST
        for function_definition in &contract.ast.expressions {
            if let Some(expr) = function_definition.match_list() {
                if expr.len() >= 3 {
                    if let Some(name_expr) = expr.get(1) {
                        if let Some(name) = name_expr.match_atom() {
                            if name.as_str() == constant_name && expr.len() >= 3 {
                                let expr_str = format!("{}", expr[2]);
                                return format!("{} {}\n{} {}\n{} {}",
                                    "Contract:".yellow(),
                                    contract_id.to_string().green(),
                                    "Constant:".yellow(),
                                    name.green(),
                                    "Value:".yellow(),
                                    expr_str.green()
                                );
                            }
                        }
                    }
                }
            }
        }

        format!("{} {} {} {}",
            "Constant:".red(),
            constant_name.red(),
            "not found in contract:".red(),
            contract_id.to_string().red()
        )
    }

    pub fn get_data_var(&mut self, cmd: &str) -> String {
        let args: Vec<_> = cmd.split_whitespace().skip(1).collect();

        if args.len() != 2 {
            return format!("{}", "Usage: ::get_data_var <contract> <var>".red());
        }

        let contract_name = args[0];
        let var_name = args[1];

        let default_deployer = match self.settings.initial_deployer.as_ref() {
            Some(account) => account.address.clone(),
            None => self.get_tx_sender(),
        };

        let contract_id = match Self::desugar_contract_id(&default_deployer, contract_name) {
            Ok(id) => id,
            Err(e) => return format!("{} {}", "Invalid contract identifier:".red(), e),
        };

        match self.interpreter.get_data_var(&contract_id, var_name) {
            Some(value_hex) => {
                // Convert hex string back to Clarity Value for display
                let value_bytes = match decode_hex(&value_hex) {
                    Ok(bytes) => bytes,
                    Err(e) => return format!("{} {} {} {}",
                        "Failed to decode value:".red(),
                        e.to_string().red(),
                        "from contract:".red(),
                        contract_id.to_string().red()
                    )
                };

                let value = match Value::consensus_deserialize(&mut &value_bytes[..]) {
                    Ok(value) => value,
                    Err(e) => return format!("{} {} {} {}",
                        "Failed to deserialize value:".red(),
                        e.to_string().red(),
                        "from contract:".red(),
                        contract_id.to_string().red()
                    )
                };

                format!("{} {}\n{} {}\n{} {}",
                    "Contract:".yellow(),
                    contract_id.to_string().green(),
                    "Data var:".yellow(),
                    var_name.green(),
                    "Value:".yellow(),
                    value_to_string(&value).green()
                )
            }
            None => {
                format!("{} {} {} {}",
                    "Data var:".red(),
                    var_name.red(),
                    "not found in contract:".red(),
                    contract_id.to_string().red()
                )
            }
        }
    }

    pub fn get_map_val(&mut self, cmd: &str) -> String {
        let args: Vec<_> = cmd.split_whitespace().skip(1).collect();

        if args.len() < 3 {
            return format!("{}", "Usage: ::get_map_val <contract> <map> <key>".red());
        }

        let contract_name = args[0];
        let map_name = args[1];
        let key_expr = args[2..].join(" ");

        let default_deployer = match self.settings.initial_deployer.as_ref() {
            Some(account) => account.address.clone(),
            None => self.get_tx_sender(),
        };

        let contract_id = match Self::desugar_contract_id(&default_deployer, contract_name) {
            Ok(id) => id,
            Err(e) => return format!("{} {}", "Invalid contract identifier:".red(), e),
        };

        // Parse the key expression to get a Clarity Value
        let key_value = match self.eval_clarity_string(&key_expr) {
            value => value,
        };

        // Extract the actual Value from the SymbolicExpression
        let key_value = match key_value.match_atom_value() {
            Some(value) => value.clone(),
            None => {
                // For complex expressions, we need to evaluate them differently
                let result = match self.eval_with_hooks(format!("({})", key_expr), None, false) {
                    Ok(result) => {
                        match &result.result {
                            EvaluationResult::Snippet(snippet_result) => snippet_result.result.clone(),
                            _ => return format!("{} {} {} {}",
                                "Unable to evaluate key expression:".red(),
                                key_expr.red(),
                                "for contract:".red(),
                                contract_id.to_string().red()
                            )
                        }
                    }
                    Err(_) => return format!("{} {} {} {}",
                        "Unable to evaluate key expression:".red(),
                        key_expr.red(),
                        "for contract:".red(),
                        contract_id.to_string().red()
                    )
                };
                result
            }
        };

        match self.interpreter.get_map_entry(&contract_id, map_name, &key_value) {
            Some(value_hex) => {
                // Convert hex string back to Clarity Value for display
                let value_bytes = match decode_hex(&value_hex) {
                    Ok(bytes) => bytes,
                    Err(e) => return format!("{} {} {} {}",
                        "Failed to decode value:".red(),
                        e.to_string().red(),
                        "from contract:".red(),
                        contract_id.to_string().red()
                    )
                };

                let value = match Value::consensus_deserialize(&mut &value_bytes[..]) {
                    Ok(value) => value,
                    Err(e) => return format!("{} {} {} {}",
                        "Failed to deserialize value:".red(),
                        e.to_string().red(),
                        "from contract:".red(),
                        contract_id.to_string().red()
                    )
                };

                format!("{} {}\n{} {}\n{} {}\n{} {}",
                    "Contract:".yellow(),
                    contract_id.to_string().green(),
                    "Map:".yellow(),
                    map_name.green(),
                    "Key:".yellow(),
                    key_expr.green(),
                    "Value:".yellow(),
                    value_to_string(&value).green()
                )
            }
            None => {
                format!("{} {} {} {} {} {}",
                    "Map entry not found for key:".red(),
                    key_expr.red(),
                    "in map:".red(),
                    map_name.red(),
                    "of contract:".red(),
                    contract_id.to_string().red()
                )
            }
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn get_costs(&mut self, output: &mut Vec<String>, cmd: &str) {
        let Some((_, expr)) = cmd.split_once(' ') else {
            return output.push("Usage: ::get_costs <expr>".red().to_string());
        };

        let _ = self.run_snippet(output, true, expr);
    }

    pub fn get_accounts(&self) -> Option<String> {
        let accounts = self.interpreter.get_accounts();
        if accounts.is_empty() {
            return None;
        }

        let tokens = self.interpreter.get_tokens();
        let mut headers = vec!["Address".to_string()];
        for token in tokens.iter() {
            if token == "STX" {
                headers.push(String::from("uSTX"));
            } else {
                headers.push(String::from(token));
            }
        }

        let mut table = Table::new();
        table.add_row(headers);
        for account in accounts.iter() {
            let mut cells = vec![];

            if let Some(name) = self.get_account_name(account) {
                cells.push(format!("{account} ({name})"));
            } else {
                cells.push(account.to_string());
            }

            for token in tokens.iter() {
                let balance = self.interpreter.get_balance_for_account(account, token);
                cells.push(balance.to_string());
            }
            table.add_row(cells);
        }
        Some(table.to_string())
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn get_contracts(&self) -> Option<String> {
        use super::boot::{BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS, SBTC_MAINNET_ADDRESS};

        let contracts: Vec<_> = self
            .contracts
            .iter()
            .filter(|(contract_id, _)| {
                let contract_id_str = contract_id.to_string();
                !contract_id_str.starts_with(BOOT_TESTNET_ADDRESS)
                    && !contract_id_str.starts_with(BOOT_MAINNET_ADDRESS)
                    && !contract_id_str.starts_with(SBTC_MAINNET_ADDRESS)
            })
            .map(|(contract_id, contract)| {
                let contract_id_str = contract_id.to_string();
                let mut formatted_methods = vec![];
                for (method_name, method_args) in contract.function_args.iter() {
                    let formatted_args = if method_args.is_empty() {
                        String::new()
                    } else if method_args.len() == 1 {
                        format!(" {}", method_args.join(" "))
                    } else {
                        format!("\n    {}", method_args.join("\n    "))
                    };
                    formatted_methods.push(format!("({method_name}{formatted_args})"));
                }
                let formatted_spec = formatted_methods.join("\n").to_string();

                [contract_id_str, formatted_spec]
            })
            .collect();

        if contracts.is_empty() {
            return None;
        }

        let mut table = Table::new();
        table.add_row(["Contract identifier", "Public functions"]);
        table.add_rows(contracts);
        Some(table.to_string())
    }

    #[cfg(target_arch = "wasm32")]
    fn get_contracts(&self) -> Option<String> {
        if self.contracts.is_empty() {
            return None;
        }
        Some(
            self.contracts
                .keys()
                .map(|contract_id| contract_id.to_string())
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }

    fn mint_stx(&mut self, command: &str) -> String {
        let args: Vec<_> = command.split(' ').collect();

        if args.len() != 3 {
            return "Usage: ::mint_stx <recipient address> <amount>"
                .red()
                .to_string();
        }

        let Ok(recipient) = PrincipalData::parse(args[1]) else {
            return "Unable to parse the address".red().to_string();
        };

        let Ok(amount) = args[2].parse::<u64>() else {
            return "Unable to parse the balance".red().to_string();
        };

        match self.interpreter.mint_stx_balance(recipient, amount) {
            Ok(msg) => msg.green().to_string(),
            Err(err) => err.red().to_string(),
        }
    }

    pub fn parse_asset_identifier(
        default_deployer: &str,
        identifier: &str,
    ) -> Result<AssetIdentifier, AssetIdentifierParseError> {
        // parse asset identifier `contract_identifier.asset_name` into it's parts
        let Some(index) = identifier.rfind('.') else {
            return Err(AssetIdentifierParseError::NoPeriod);
        };

        if index == identifier.len() - 1 {
            return Err(AssetIdentifierParseError::EndsWithPeriod);
        }

        let (contract_identifier_str, asset_name_str) = identifier.split_at(index);

        let Ok(contract_identifier) =
            Self::desugar_contract_id(default_deployer, contract_identifier_str)
        else {
            return Err(AssetIdentifierParseError::ContractIdentifierParseError);
        };

        let Some(asset_name) = asset_name_str.strip_prefix('.') else {
            return Err(AssetIdentifierParseError::NoPrefixPeriod);
        };

        Ok(AssetIdentifier {
            contract_identifier,
            asset_name: asset_name.into(),
        })
    }

    fn mint_ft(&mut self, command: &str) -> String {
        let args: Vec<_> = command.split(' ').collect();

        if args.len() != 4 {
            return "Usage: ::mint_ft <asset identifier> <recipient address> <amount>"
                .red()
                .to_string();
        }

        let asset_identifier = match Self::parse_asset_identifier(&self.get_tx_sender(), args[1]) {
            Ok(asset_identifier) => asset_identifier,
            Err(err) => {
                return format!("Unable to parse the asset identifier: {err:?}")
                    .red()
                    .to_string()
            }
        };

        let Ok(recipient) = PrincipalData::parse(args[2]) else {
            return "Unable to parse the recipient address".red().to_string();
        };

        let Ok(amount) = args[3].parse::<u64>() else {
            return "Unable to parse the amount".red().to_string();
        };

        match self
            .interpreter
            .mint_ft_balance(&asset_identifier, &recipient, amount)
        {
            Ok(msg) => msg.green().to_string(),
            Err(err) => err.red().to_string(),
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn display_functions(&self) -> String {
        use crate::repl::docs::CLARITY_STD_INDEX;
        format!("{}", CLARITY_STD_INDEX.join("\n").yellow())
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn keywords(&self) -> String {
        use crate::repl::docs::CLARITY_KEYWORDS_INDEX;
        format!("{}", CLARITY_KEYWORDS_INDEX.join("\n").yellow())
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn lookup_functions_or_keywords_docs(&self, exp: &str) -> Option<&str> {
        use crate::repl::docs::{CLARITY_KEYWORDS_REF, CLARITY_STD_REF};
        CLARITY_STD_REF
            .get(exp)
            .or_else(|| CLARITY_KEYWORDS_REF.get(exp))
            .map(|s| s.as_str())
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn display_doc(&self, command: &str) -> String {
        let keyword = {
            let mut s = command.to_string();
            s = s.replace("::describe", "");
            s = s.replace(' ', "");
            s
        };

        match self.lookup_functions_or_keywords_docs(&keyword) {
            Some(doc) => format!("{}", doc.yellow()),
            None => format!(
                "{}",
                "It looks like there aren't matches for your search".red()
            ),
        }
    }

    pub fn get_performance_data(&self) -> Option<String> {
        if let Some(ref perf_hook) = self.perf_hook {
            perf_hook.get_buffer_data()
        } else {
            None
        }
    }

    pub fn clear_performance_buffer(&mut self) {
        if let Some(ref mut perf_hook) = self.perf_hook {
            perf_hook.clear_buffer();
        }
    }
}

#[derive(Debug, PartialEq)]
enum DecodeHexError {
    ParseError(ParseIntError),
    OddLength,
}

impl fmt::Display for DecodeHexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DecodeHexError::ParseError(e) => write!(f, "{e}"),
            DecodeHexError::OddLength => write!(f, "odd number of hex digits"),
        }
    }
}

impl From<ParseIntError> for DecodeHexError {
    fn from(err: ParseIntError) -> Self {
        DecodeHexError::ParseError(err)
    }
}

fn decode_hex(byte_string: &str) -> Result<Vec<u8>, DecodeHexError> {
    let byte_string_filtered: String = byte_string
        .strip_prefix("0x")
        .unwrap_or(byte_string)
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    if !byte_string_filtered.len().is_multiple_of(2) {
        return Err(DecodeHexError::OddLength);
    }
    let result: Result<Vec<u8>, ParseIntError> = (0..byte_string_filtered.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&byte_string_filtered[i..i + 2], 16))
        .collect();
    match result {
        Ok(result) => Ok(result),
        Err(e) => Err(DecodeHexError::ParseError(e)),
    }
}

#[allow(clippy::items_after_test_module)]
#[cfg(test)]
mod tests {
    use clarity::util::hash::hex_bytes;
    use clarity_types::types::TupleData;
    use indoc::formatdoc;

    use super::*;
    use crate::repl::boot::{BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS};
    use crate::repl::settings::Account;
    use crate::repl::DEFAULT_EPOCH;
    use crate::test_fixtures::clarity_contract::ClarityContractBuilder;

    #[track_caller]
    fn run_session_snippet(session: &mut Session, snippet: &str) -> Value {
        let execution_res = session.eval(snippet.to_string(), false).unwrap();
        let res = match execution_res.result {
            EvaluationResult::Contract(_) => unreachable!(),
            EvaluationResult::Snippet(res) => res,
        };
        res.result
    }

    #[track_caller]
    fn assert_execution_result_value(
        result: &Result<ExecutionResult, Vec<Diagnostic>>,
        expected_value: Value,
    ) {
        assert!(result.is_ok());
        let result = result.as_ref().unwrap();
        let result = match &result.result {
            EvaluationResult::Contract(_) => unreachable!(),
            EvaluationResult::Snippet(res) => res,
        };
        assert_eq!(result.result, expected_value);
    }

    #[test]
    fn initial_accounts() {
        let address = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        let session = Session::new(SessionSettings {
            initial_accounts: vec![Account {
                address: address.to_owned(),
                balance: 1000000,
                name: "wallet_1".to_owned(),
            }],
            ..Default::default()
        });
        let balance = session.interpreter.get_balance_for_account(address, "STX");
        assert_eq!(balance, 1000000);
    }

    #[test]
    fn epoch_switch() {
        let mut session = Session::new(SessionSettings::default());
        session.update_epoch(StacksEpochId::Epoch20);
        let diags = session
            .eval("(slice? \"blockstack\" u5 u10)".into(), false)
            .unwrap_err();
        assert_eq!(
            diags[0].message,
            format!("use of unresolved function 'slice?'",)
        );
        session.update_epoch(StacksEpochId::Epoch21);
        let res = session
            .eval("(slice? \"blockstack\" u5 u10)".into(), false)
            .unwrap();
        let res = match res.result {
            EvaluationResult::Contract(_) => unreachable!(),
            EvaluationResult::Snippet(res) => res,
        };
        assert_eq!(
            res.result,
            Value::some(Value::string_ascii_from_bytes("stack".as_bytes().to_vec()).unwrap())
                .unwrap()
        );
    }

    #[test]
    fn test_parse_and_advance_stacks_chain_tip() {
        let mut session = Session::new(SessionSettings::default());
        let result = session.handle_command("::advance_stacks_chain_tip 1");
        assert_eq!(
            result,
            "advance_stacks_chain_tip can't be called in epoch lower than 3.0"
                .to_string()
                .red()
                .to_string()
        );
        session.handle_command("::set_epoch 3.0");
        let _ = session.handle_command("::advance_stacks_chain_tip 1");
        let new_height = session.handle_command("::get_stacks_block_height");
        assert_eq!(new_height, "Current height: 2");
    }

    #[test]
    fn test_parse_and_advance_burn_chain_tip_pre_epoch3() {
        let mut session = Session::new(SessionSettings::default());
        let result = session.handle_command("::advance_burn_chain_tip 1");
        assert_eq!(
            result,
            "new burn height: 1\nnew stacks height: 1"
                .to_string()
                .green()
                .to_string()
        );
        // before epoch 3 this acts the same as burn_chain_tip
        let result = session.handle_command("::advance_chain_tip 1");
        assert_eq!(
            result,
            "new burn height: 2\nnew stacks height: 2"
                .to_string()
                .green()
                .to_string()
        );
    }

    #[test]
    fn test_parse_and_advance_burn_chain_tip_epoch3() {
        let mut session = Session::new(SessionSettings::default());
        session.handle_command("::set_epoch 3.0");
        let result = session.handle_command("::advance_burn_chain_tip 1");
        assert_eq!(
            result,
            "new burn height: 2\nnew stacks height: 2"
                .to_string()
                .green()
                .to_string()
        );
        let new_height = session.handle_command("::get_stacks_block_height");
        assert_eq!(new_height, "Current height: 2");
        // advance_chain_tip will only affect stacks height in epoch 3 or greater
        let _ = session.handle_command("::advance_chain_tip 1");
        let new_height = session.handle_command("::get_stacks_block_height");
        assert_eq!(new_height, "Current height: 3");
        let new_height = session.handle_command("::get_burn_block_height");
        assert_eq!(new_height, "Current height: 2");
    }

    #[test]
    fn set_epoch_command() {
        let mut session = Session::new(SessionSettings::default());
        let initial_block_height = session.interpreter.get_block_height();
        let initial_epoch = session.handle_command("::get_epoch");
        // initial epoch is 2.05
        assert_eq!(initial_epoch, "Current epoch: 2.05");

        // it can be lowered to 2.0
        // it's possible that in the feature we want to start from 2.0 and forbid lowering the epoch
        // this test would have to be updated
        session.handle_command("::set_epoch 2.0");
        let current_epoch = session.handle_command("::get_epoch");
        assert_eq!(current_epoch, "Current epoch: 2.0");

        session.handle_command("::set_epoch 2.4");
        let current_epoch = session.handle_command("::get_epoch");
        assert_eq!(current_epoch, "Current epoch: 2.4");

        // changing epoch in 2.x does not impact the block height
        assert_eq!(session.interpreter.get_block_height(), initial_block_height);

        session.handle_command("::set_epoch 3.0");
        let current_epoch = session.handle_command("::get_epoch");
        assert_eq!(current_epoch, "Current epoch: 3.0");

        // changing epoch in 3.x increments the block height
        assert_eq!(
            session.interpreter.get_block_height(),
            initial_block_height + 1
        );

        // ensure latest epoch is correctly handled
        let latest_epoch = StacksEpochId::latest().to_string();
        session.handle_command(&format!("::set_epoch {latest_epoch}"));
        let current_epoch = session.handle_command("::get_epoch");
        assert_eq!(current_epoch, format!("Current epoch: {latest_epoch}"));
    }

    #[test]
    fn encode_error() {
        let mut session = Session::new(SessionSettings::default());
        let result = session.encode("::encode { foo false }");
        assert_eq!(
            result,
            format_err!("Tuple literal construction expects a colon at index 1\nencoding failed")
        );

        let result = session.encode("::encode (foo 1)");
        assert_eq!(
            result.split('\n').next().unwrap(),
            format!(
                "encode:1:1: {} use of unresolved function 'foo'",
                "error:".red().bold()
            )
        );
    }

    #[test]
    fn decode_simple() {
        let mut session = Session::new(SessionSettings::default());

        let result = session.decode("::decode 0000000000000000 0000000000000000 2a");
        assert_eq!(result, "42".green().to_string());
    }

    #[test]
    fn decode_map() {
        let mut session = Session::new(SessionSettings::default());
        let result = session.decode("::decode 0x0c00000002036261720403666f6f0d0000000568656c6c6f");
        assert_eq!(result, "{ bar: false, foo: \"hello\" }".green().to_string());
    }

    #[test]
    fn decode_error() {
        let mut session = Session::new(SessionSettings::default());
        let result = session.decode("::decode 42");
        assert_eq!(
            result,
            "Failed to decode clarity value: DeserializationError(\"Bad type prefix\")"
                .red()
                .to_string()
        );

        let result = session.decode("::decode 4g");
        assert_eq!(
            result,
            "Parsing error: invalid digit found in string"
                .red()
                .to_string()
        );
    }

    #[test]
    fn clarity_epoch_mismatch() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        let snippet = "(define-data-var x uint u0)";

        // can not use ClarityContractBuilder to build an invalid contract
        let contract = ClarityContract {
            code_source: ClarityCodeSource::ContractInMemory(snippet.to_string()),
            name: "should_error".to_string(),
            deployer: ContractDeployer::Address("ST000000000000000000002AMW42H".into()),
            clarity_version: ClarityVersion::Clarity2,
            epoch: Epoch::Specific(StacksEpochId::Epoch2_05),
        };

        let result = session.deploy_contract(&contract, false, None);
        assert!(result.is_err(), "Expected error for clarity mismatch");
    }

    #[test]
    fn deploy_contract_with_wrong_epoch() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);

        session.update_epoch(StacksEpochId::Epoch24);

        let snippet = "(define-data-var x uint u0)";
        let contract = ClarityContractBuilder::new()
            .code_source(snippet.into())
            .epoch(StacksEpochId::Epoch25)
            .clarity_version(ClarityVersion::Clarity2)
            .build();

        let result = session.deploy_contract(&contract, false, None);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.len() == 1);
        assert_eq!(
            err.first().unwrap().message,
            "contract epoch (2.5) does not match current epoch (2.4)"
        );
    }

    #[test]
    fn test_eval_clarity_string() {
        let mut session = Session::new(SessionSettings::default());
        let epoch = StacksEpochId::Epoch25;
        session.update_epoch(epoch);

        let result = session.eval_clarity_string("u1");
        assert_eq!(result, SymbolicExpression::atom_value(Value::UInt(1)));

        let result = session.eval_clarity_string("(+ 1 2)");
        assert_eq!(result, SymbolicExpression::atom_value(Value::Int(3)));

        let result = session.eval_clarity_string("(list u1 u2)");
        assert_eq!(
            result,
            SymbolicExpression::atom_value(
                Value::cons_list_unsanitized(vec![Value::UInt(1), Value::UInt(2)]).unwrap()
            )
        );

        let result = session.eval_clarity_string("0x01");
        assert_eq!(
            result,
            SymbolicExpression::atom_value(Value::buff_from_byte(0x01))
        );
    }

    #[test]
    fn evaluate_at_block() {
        let settings = SessionSettings::default();

        let mut session = Session::new(settings);

        session.handle_command("::set_epoch 2.5");

        // setup contract state
        let snippet = "
            (define-data-var x uint u0)
            (define-read-only (get-x)
                (var-get x))
            (define-public (incr)
                (begin
                    (var-set x (+ (var-get x) u1))
                    (ok (var-get x))))";

        let contract = ClarityContract {
            code_source: ClarityCodeSource::ContractInMemory(snippet.to_string()),
            name: "contract".to_string(),
            deployer: ContractDeployer::Address("ST000000000000000000002AMW42H".into()),
            clarity_version: ClarityVersion::Clarity2,
            epoch: Epoch::Specific(StacksEpochId::Epoch25),
        };

        let _ = session.deploy_contract(&contract, false, None);

        // assert data-var is set to 0
        assert_eq!(
            session
                .process_console_input("(contract-call? .contract get-x)")
                .1[0],
            "u0".green().to_string()
        );

        // advance chain tip and test at-block
        let _ = session.advance_chain_tip(10000);
        assert_eq!(
            session
                .process_console_input("(contract-call? .contract get-x)")
                .1[0],
            "u0".green().to_string()
        );
        session.process_console_input("(contract-call? .contract incr)");
        assert_eq!(
            session
                .process_console_input("(contract-call? .contract get-x)")
                .1[0],
            "u1".green().to_string()
        );

        assert_eq!(session.process_console_input("(at-block (unwrap-panic (get-block-info? id-header-hash u0)) burn-block-height)").1[0], "u0".green().to_string());
        assert_eq!(session.process_console_input("(at-block (unwrap-panic (get-block-info? id-header-hash u5000)) burn-block-height)").1[0], "u4999".green().to_string());

        assert_eq!(session.process_console_input("(at-block (unwrap-panic (get-block-info? id-header-hash u0)) (contract-call? .contract get-x))").1[0], "u0".green().to_string());
        assert_eq!(session.process_console_input("(at-block (unwrap-panic (get-block-info? id-header-hash u5000)) (contract-call? .contract get-x))").1[0], "u0".green().to_string());

        // advance chain tip again and test at-block
        // do this twice to make sure that the lookup table is being updated properly
        session.advance_chain_tip(10);
        session.advance_chain_tip(10);

        assert_eq!(
            session
                .process_console_input("(contract-call? .contract get-x)")
                .1[0],
            "u1".green().to_string()
        );
        session.process_console_input("(contract-call? .contract incr)");
        assert_eq!(
            session
                .process_console_input("(contract-call? .contract get-x)")
                .1[0],
            "u2".green().to_string()
        );
        assert_eq!(session.process_console_input("(at-block (unwrap-panic (get-block-info? id-header-hash u10000)) (contract-call? .contract get-x))").1[0], "u1".green().to_string());

        session.handle_command("::set_epoch 3.1");

        let _ = session.advance_burn_chain_tip(10000);

        assert_eq!(session.process_console_input("(at-block (unwrap-panic (get-stacks-block-info? id-header-hash u19000)) burn-block-height)").1[0], "u20021".green().to_string());
    }

    #[test]
    fn can_deploy_a_contract() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(DEFAULT_EPOCH);

        // deploy default contract
        let contract = ClarityContractBuilder::default().build();
        let result = session.deploy_contract(&contract, false, None);
        assert!(result.is_ok());
    }

    #[test]
    fn can_call_boot_contract_fn() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(StacksEpochId::Epoch25);

        // call pox4 get-info
        let result = session.call_contract_fn(
            format!("{BOOT_MAINNET_ADDRESS}.pox-4").as_str(),
            "get-pox-info",
            &[],
            BOOT_TESTNET_ADDRESS,
            false,
            false,
        );
        assert_execution_result_value(
            &result,
            Value::okay(Value::Tuple(
                TupleData::from_data(vec![
                    ("min-amount-ustx".into(), Value::UInt(0)),
                    ("reward-cycle-id".into(), Value::UInt(0)),
                    ("prepare-cycle-length".into(), Value::UInt(50)),
                    ("first-burnchain-block-height".into(), Value::UInt(0)),
                    ("reward-cycle-length".into(), Value::UInt(1050)),
                    ("total-liquid-supply-ustx".into(), Value::UInt(0)),
                ])
                .unwrap(),
            ))
            .unwrap(),
        );
    }

    #[test]
    fn can_call_public_contract_fn() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(DEFAULT_EPOCH);

        // deploy default contract
        let contract = ClarityContractBuilder::default().build();
        let _ = session.deploy_contract(&contract, false, None);

        let result = session.call_contract_fn(
            "contract",
            "incr",
            &[],
            &session.get_tx_sender(),
            false,
            false,
        );
        assert_execution_result_value(&result, Value::okay(Value::UInt(1)).unwrap());

        let result = session.call_contract_fn(
            "contract",
            "get-x",
            &[],
            &session.get_tx_sender(),
            false,
            false,
        );
        assert_execution_result_value(&result, Value::UInt(1));
    }

    #[test]
    fn current_block_info_is_none() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(StacksEpochId::Epoch25);
        session.advance_chain_tip(5);
        let result = run_session_snippet(&mut session, "(get-block-info? time block-height)");
        assert_eq!(result, Value::none());
    }

    #[test]
    fn block_time_is_realistic_in_epoch_2_5() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(StacksEpochId::Epoch25);

        session.advance_chain_tip(4);

        let result = run_session_snippet(&mut session, "(get-block-info? time u2)");
        let Ok(Some(Value::UInt(time_block_1))) = result.expect_optional() else {
            panic!("Unexpected result");
        };

        let result = run_session_snippet(&mut session, "(get-block-info? time u3)");
        let Ok(Some(Value::UInt(time_block_2))) = result.expect_optional() else {
            panic!("Unexpected result");
        };

        assert!(time_block_2 - time_block_1 == 600);
    }

    #[test]
    fn burn_block_height_behavior_epoch2_5() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(StacksEpochId::Epoch25);

        let snippet = [
            "(define-read-only (get-burn (height uint))",
            "  (let ((id (unwrap-panic (get-block-info? id-header-hash height))))",
            "    (at-block id burn-block-height)))",
        ]
        .join("\n");

        let contract = ClarityContractBuilder::new()
            .code_source(snippet)
            .clarity_version(ClarityVersion::Clarity2)
            .epoch(StacksEpochId::Epoch25)
            .build();

        session.deploy_contract(&contract, false, None).unwrap();
        session.advance_burn_chain_tip(10);

        let result = run_session_snippet(&mut session, "block-height");
        assert_eq!(result, Value::UInt(10));
        let result = run_session_snippet(&mut session, "burn-block-height");
        assert_eq!(result, Value::UInt(9));
        let result = run_session_snippet(
            &mut session,
            format!("(contract-call? .{} get-burn u6)", contract.name).as_str(),
        );
        assert_eq!(result, Value::UInt(5));
    }

    #[test]
    fn get_burn_block_info_past() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(StacksEpochId::Epoch30);

        session.advance_burn_chain_tip(10);

        let result = run_session_snippet(&mut session, "(get-burn-block-info? header-hash u10)");
        let expected_header_hash =
            hex_bytes("02e51c71171ecd6467c472e11374c5a5ae882f8591c8d2b8ba24d916680f3e8a").unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(expected_header_hash).unwrap()).unwrap()
        );
        let result = run_session_snippet(&mut session, "(get-burn-block-info? header-hash u9)");
        let expected_header_hash =
            hex_bytes("02128940fbe65bd02156e79e09b8f84cf889c7353c9cd16e7f43a3f60902ca90").unwrap();
        assert_eq!(
            result,
            Value::some(Value::buff_from(expected_header_hash).unwrap()).unwrap()
        );
    }

    #[test]
    fn burn_block_height_behavior_epoch3_0() {
        // test that clarinet preserves the 3.0 and 3.1 special behavior of burn-block-height
        // https://github.com/stacks-network/stacks-core/pull/5524
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(StacksEpochId::Epoch30);

        let snippet = [
            "(define-read-only (get-burn (height uint))",
            "  (let ((id (unwrap-panic (get-stacks-block-info? id-header-hash height))))",
            "    (at-block id burn-block-height)))",
        ]
        .join("\n");

        let contract = ClarityContractBuilder::new()
            .code_source(snippet)
            .clarity_version(ClarityVersion::Clarity3)
            .epoch(StacksEpochId::Epoch30)
            .build();

        session.deploy_contract(&contract, false, None).unwrap();
        session.advance_burn_chain_tip(10);

        let result = run_session_snippet(&mut session, "stacks-block-height");
        assert_eq!(result, Value::UInt(11));
        let result = run_session_snippet(&mut session, "burn-block-height");
        assert_eq!(result, Value::UInt(11));
        let result = run_session_snippet(
            &mut session,
            format!("(contract-call? .{} get-burn u8)", contract.name).as_str(),
        );
        assert_eq!(result, Value::UInt(11));
    }

    #[test]
    fn burn_block_height_behavior_epoch3_0_contract_in_2_5() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(StacksEpochId::Epoch25);

        let snippet = [
            "(define-read-only (get-burn (height uint))",
            "  (let ((id (unwrap-panic (get-block-info? id-header-hash height))))",
            "    (at-block id burn-block-height)))",
        ]
        .join("\n");

        let contract = ClarityContractBuilder::new()
            .name("contract-2-5")
            .code_source(snippet)
            .clarity_version(ClarityVersion::Clarity2)
            .epoch(StacksEpochId::Epoch25)
            .build();

        session.deploy_contract(&contract, false, None).unwrap();
        session.advance_burn_chain_tip(10);

        session.update_epoch(StacksEpochId::Epoch30);

        session.advance_burn_chain_tip(10);

        let result = run_session_snippet(&mut session, "stacks-block-height");
        assert_eq!(result, Value::UInt(21));

        // calling a 2.5 contract in epoch 3.0 has the same behavior as epoch 2.5

        let result = run_session_snippet(
            &mut session,
            format!("(contract-call? .{} get-burn u8)", contract.name).as_str(),
        );
        assert_eq!(result, Value::UInt(7));

        let result = run_session_snippet(
            &mut session,
            format!("(contract-call? .{} get-burn u18)", contract.name).as_str(),
        );
        assert_eq!(result, Value::UInt(21));
    }

    #[test]
    fn test_parse_and_set_tx_sender() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        let sender = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        let result = session.process_console_input(&format!("::set_tx_sender    {sender}"));
        assert!(result.1[0].contains(sender));

        let result = session.process_console_input(&format!("::set_tx_sender {sender}     "));
        assert!(result.1[0].contains(sender));
    }

    #[test]
    fn test_call_contract_fn_undefined_function() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(DEFAULT_EPOCH);

        // deploy a simple contract
        let contract = ClarityContractBuilder::default().build();
        let _ = session.deploy_contract(&contract, false, None);

        // try to call a non-existent function
        let result = session.call_contract_fn(
            "contract",
            "doesnt-exist",
            &[],
            &session.get_tx_sender(),
            false,
            false,
        );

        assert!(result.is_err());
        let diagnostics = result.unwrap_err();
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].message,
            format!(
                "Method 'doesnt-exist' does not exist on contract '{}.contract'",
                session.get_tx_sender()
            )
        );
    }

    #[test]
    fn test_parse_asset_identifier() {
        // S1G2081040G2081040G2081040G208105NK8PE5.contract.ctb
        assert_eq!(
            Err(AssetIdentifierParseError::NoPeriod),
            Session::parse_asset_identifier("", "S1G2081040G2081040G2081040G208105NK8PE5")
        );

        assert_eq!(
            Err(AssetIdentifierParseError::ContractIdentifierParseError),
            Session::parse_asset_identifier("", "S1G2081040G2081040G2081040G208105NK8PE5.contract")
        );

        assert_eq!(
            Err(AssetIdentifierParseError::ContractIdentifierParseError),
            Session::parse_asset_identifier("", ".contract.ctb")
        );
        assert!(Session::parse_asset_identifier(
            "S1G2081040G2081040G2081040G208105NK8PE5",
            ".contract.ctb",
        )
        .is_ok());

        assert_eq!(
            Err(AssetIdentifierParseError::EndsWithPeriod),
            Session::parse_asset_identifier(
                "",
                "S1G2081040G2081040G2081040G208105NK8PE5.contract.ctb.",
            )
        );

        assert!(Session::parse_asset_identifier(
            "",
            "S1G2081040G2081040G2081040G208105NK8PE5.contract.ctb",
        )
        .is_ok());
    }

    #[test]
    fn test_mint_ft() {
        let settings = SessionSettings::default();
        let mut session = Session::new(settings);
        session.update_epoch(DEFAULT_EPOCH);

        let token_name = "ctb";
        let src = formatdoc!(
            r#"
            (define-fungible-token {token_name})
            (define-private (test-mint)
                (ft-mint? ctb u100 tx-sender)
            )
            (test-mint)"#
        );
        let contract = ClarityContractBuilder::default().code_source(src).build();
        let deploy_result = session.deploy_contract(&contract, false, None);
        assert!(deploy_result.is_ok());
        let ExecutionResult { result, .. } = deploy_result.unwrap();

        let contract_identifier_str =
            if let EvaluationResult::Contract(contract_evaluation_result) = result {
                contract_evaluation_result
                    .contract
                    .contract_identifier
                    .clone()
            } else {
                panic!("didn't get EvaluationResult::Contract");
            };

        let contract_identifier = QualifiedContractIdentifier::parse(&contract_identifier_str)
            .expect("failed to parse QualifiedContractIdentifier from {contract_identifier_str}");

        let asset_identifier = format!("{contract_identifier_str}.{token_name}");

        let recipient = contract_identifier.issuer.to_string();

        // more than 4 tokens
        let cmd = format!("::mint_ft {asset_identifier} {recipient} 1000 foo");
        let result = session.handle_command(&cmd);
        println!("{result}");

        // unable to parse asset identifier
        let cmd = format!("::mint_ft ._-{asset_identifier} {recipient} 1000");
        let result = session.handle_command(&cmd);
        println!("{result}");

        // unable to parse recipient address
        let cmd = format!("::mint_ft {asset_identifier} {recipient}. 1000");
        let result = session.handle_command(&cmd);
        println!("{result}");

        // unable to parse recipient address
        let cmd = format!("::mint_ft {asset_identifier} {recipient}XXX 1000");
        let result = session.handle_command(&cmd);
        println!("{result}");

        // unable to parse amount
        let cmd = format!("::mint_ft {asset_identifier} {recipient} -1");
        let result = session.handle_command(&cmd);
        println!("{result}");

        let cmd = format!("::mint_ft {asset_identifier} {recipient} 1000");
        let result = session.handle_command(&cmd);
        println!("{result}");

        // we should be able to skip the deployer part of the contract identifier
        let index = asset_identifier.find('.').unwrap();
        let (_, short_asset_identifier) = asset_identifier.split_at(index);
        let cmd = format!("::mint_ft {short_asset_identifier} {recipient} 10000");
        let result = session.handle_command(&cmd);
        println!("{result}");

        let asset_identifier = Session::parse_asset_identifier("", &asset_identifier).unwrap();

        // we minted 100 when we deployed the contract then 1000 + 10000 in the session cmd
        // if any of the expected failures succeeded, this will fail
        let balance = session
            .interpreter
            .get_balance_for_account(&recipient.to_string(), &asset_identifier.sugared());
        assert_eq!(balance, 11100);
    }
}
