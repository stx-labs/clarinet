use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::vec;

use clarinet_defaults::DEFAULT_CLARITY_VERSION;
use clarinet_deployments::{
    generate_default_deployment, initiate_session_from_manifest,
    update_session_with_deployment_plan,
};
use clarinet_files::{paths, FileAccessor, ProjectManifest, StacksNetwork};
use clarity::types::StacksEpochId;
use clarity::vm::analysis::ContractAnalysis;
use clarity::vm::ast::{build_ast, ContractAST};
use clarity::vm::diagnostic::{Diagnostic, Diagnostic as ClarityDiagnostic, Level as ClarityLevel};
use clarity::vm::types::{QualifiedContractIdentifier, StandardPrincipalData};
use clarity::vm::{ClarityName, ClarityVersion, EvaluationResult, SymbolicExpression};
use clarity_repl::analysis::ast_dependency_detector::DependencySet;
use clarity_repl::analysis::LintDiagnostic;
use clarity_repl::repl::interpreter::BLOCK_LIMIT_MAINNET;
use clarity_repl::repl::session::AnnotatedExecutionResult;
use clarity_repl::repl::ContractDeployer;
use clarity_repl::utils::{get_env_simnet_spans, CHECK_ENVIRONMENTS};
use clarity_static_cost::static_cost::StaticCost;
use clarity_types::execution_cost::ExecutionCost;
use ls_types::{
    CompletionItem, Diagnostic as LspDiagnostic, DiagnosticSeverity, DiagnosticTag, DocumentSymbol,
    Hover, Location, MessageType, Position, Range, SignatureHelp,
};

use super::requests::capabilities::{CostDisplayFormat, InitializationOptions};
use super::requests::completion::{
    build_completion_item_list, get_contract_calls, ContractDefinedData,
};
use super::requests::definitions::{
    get_all_function_definitions, get_definitions, get_public_function_and_trait_definitions,
    get_trait_definitions, DefinitionLocation,
};
use super::requests::document_symbols::ASTSymbols;
use super::requests::helpers::get_atom_or_field_start_at_position;
use super::requests::hover::get_expression_documentation;
use super::requests::signature_help::get_signatures;
use crate::common::requests::completion::check_if_should_wrap;

#[derive(Debug, Clone, PartialEq)]
pub struct ActiveContractData {
    pub clarity_version: ClarityVersion,
    pub epoch: StacksEpochId,
    pub issuer: Option<StandardPrincipalData>,
    pub definitions: Option<HashMap<(u32, u32), DefinitionLocation>>,
    pub expressions: Option<Vec<SymbolicExpression>>,
    pub diagnostic: Option<ClarityDiagnostic>,
    pub source: String,
}

impl ActiveContractData {
    pub fn new(
        clarity_version: ClarityVersion,
        epoch: StacksEpochId,
        issuer: Option<StandardPrincipalData>,
        source: String,
    ) -> Self {
        match build_ast(
            &QualifiedContractIdentifier::transient(),
            &source,
            &mut (),
            clarity_version,
            epoch,
        ) {
            Ok(ast) => ActiveContractData {
                clarity_version,
                epoch,
                issuer: issuer.clone(),
                definitions: Some(get_definitions(clarity_version, &ast.expressions, issuer)),
                expressions: Some(ast.expressions),
                diagnostic: None,
                source,
            },
            Err(err) => ActiveContractData {
                clarity_version,
                epoch,
                issuer,
                definitions: None,
                expressions: None,
                diagnostic: Some(err.diagnostic),
                source,
            },
        }
    }

    pub fn update_expressions(&mut self, with_definitions: bool) {
        self.definitions = None;
        match build_ast(
            &QualifiedContractIdentifier::transient(),
            &self.source,
            &mut (),
            self.clarity_version,
            self.epoch,
        ) {
            Ok(ast) => {
                self.expressions = Some(ast.expressions);
                self.diagnostic = None;
                if with_definitions {
                    self.update_definitions();
                }
            }
            Err(err) => {
                self.expressions = None;
                self.diagnostic = Some(err.diagnostic);
            }
        };
    }

    pub fn update_sources(&mut self, source: &str, with_definitions: bool) {
        source.clone_into(&mut self.source);
        self.update_expressions(with_definitions);
    }

    pub fn update_definitions(&mut self) {
        if let Some(expressions) = &self.expressions {
            self.definitions = Some(get_definitions(
                self.clarity_version,
                expressions,
                self.issuer.clone(),
            ));
        }
    }

    pub fn update_clarity_version(&mut self, clarity_version: ClarityVersion) {
        self.clarity_version = clarity_version;
        self.update_expressions(true);
    }

    pub fn update_issuer(&mut self, issuer: Option<StandardPrincipalData>) {
        self.issuer = issuer;
        self.update_definitions();
    }
}

#[derive(Debug, Clone)]
pub struct ContractState {
    contract_calls: Vec<CompletionItem>,
    errors: Vec<ClarityDiagnostic>,
    warnings: Vec<ClarityDiagnostic>,
    notes: Vec<ClarityDiagnostic>,
    lint_diagnostics: Vec<LintDiagnostic>,
    analysis: Option<ContractAnalysis>,
    definitions: HashMap<ClarityName, Range>,
    clarity_version: ClarityVersion,
    cost_analysis: Option<HashMap<String, (StaticCost, Option<HashMap<String, (u64, u64)>>)>>,
}

impl ContractState {
    pub fn new(
        _ast: ContractAST,
        _deps: DependencySet,
        diags: Vec<ClarityDiagnostic>,
        lint_diagnostics: Vec<LintDiagnostic>,
        analysis: Option<ContractAnalysis>,
        definitions: HashMap<ClarityName, Range>,
        clarity_version: ClarityVersion,
        cost_analysis: Option<HashMap<String, (StaticCost, Option<HashMap<String, (u64, u64)>>)>>,
    ) -> ContractState {
        let mut errors = vec![];
        let mut warnings = vec![];
        let mut notes = vec![];

        for diag in diags.into_iter() {
            match diag.level {
                ClarityLevel::Error => {
                    errors.push(diag);
                }
                ClarityLevel::Warning => {
                    warnings.push(diag);
                }
                ClarityLevel::Note => {
                    notes.push(diag);
                }
            }
        }

        let contract_calls = analysis
            .as_ref()
            .map(get_contract_calls)
            .unwrap_or_default();

        ContractState {
            contract_calls,
            errors,
            warnings,
            notes,
            lint_diagnostics,
            analysis,
            definitions,
            clarity_version,
            cost_analysis,
        }
    }
}

struct CostPercentages {
    runtime: f64,
    read_count: f64,
    read_length: f64,
    write_count: f64,
    write_length: f64,
}

impl CostPercentages {
    fn exceeds_threshold(&self, threshold: f64) -> bool {
        [
            self.runtime,
            self.read_count,
            self.read_length,
            self.write_count,
            self.write_length,
        ]
        .iter()
        .any(|p| *p >= threshold)
    }
}

fn percentage_of_limit(value: u64, limit: u64) -> f64 {
    if limit == 0 {
        return 0.0;
    }
    (value as f64) / (limit as f64) * 100.0
}

fn cost_percentages(cost: &ExecutionCost) -> CostPercentages {
    let limit = &BLOCK_LIMIT_MAINNET;
    CostPercentages {
        runtime: percentage_of_limit(cost.runtime, limit.runtime),
        read_count: percentage_of_limit(cost.read_count, limit.read_count),
        read_length: percentage_of_limit(cost.read_length, limit.read_length),
        write_count: percentage_of_limit(cost.write_count, limit.write_count),
        write_length: percentage_of_limit(cost.write_length, limit.write_length),
    }
}

// Format a number with abbreviation (e.g., 2.6k instead of 2,634)
fn format_abbreviated_number(value: u64) -> String {
    if value >= 1_000_000 {
        format!("{:.1}M", value as f64 / 1_000_000.0)
    } else if value >= 1_000 {
        format!("{:.1}k", value as f64 / 1_000.0)
    } else {
        value.to_string()
    }
}

#[derive(Clone, Debug)]
pub struct ContractMetadata {
    pub base_location: PathBuf,
    pub manifest_location: PathBuf,
    pub relative_path: String,
    pub clarity_version: ClarityVersion,
    pub deployer: ContractDeployer,
}

#[derive(Clone, Default, Debug)]
pub struct EditorState {
    pub protocols: HashMap<PathBuf, ProtocolState>,
    pub contracts_lookup: HashMap<PathBuf, ContractMetadata>,
    pub active_contracts: HashMap<PathBuf, ActiveContractData>,
    pub settings: InitializationOptions,
}

impl EditorState {
    pub fn new() -> EditorState {
        EditorState {
            protocols: HashMap::new(),
            contracts_lookup: HashMap::new(),
            active_contracts: HashMap::new(),
            settings: InitializationOptions::default(),
        }
    }

    pub fn index_protocol(&mut self, manifest_location: PathBuf, protocol: ProtocolState) {
        // Get the project directory (parent of Clarinet.toml)
        let base_location = manifest_location
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf();

        for (contract_location, contract_state) in protocol.contracts.iter() {
            let relative_path = paths::get_relative_path(contract_location, &base_location)
                .expect("could not find relative location");

            let deployer = match &contract_state.analysis {
                Some(analysis) => {
                    ContractDeployer::ContractIdentifier(analysis.contract_identifier.clone())
                }
                None => ContractDeployer::DefaultDeployer,
            };

            if let Some(active_contract) = self.active_contracts.get_mut(contract_location) {
                if active_contract.clarity_version != contract_state.clarity_version {
                    active_contract.update_clarity_version(contract_state.clarity_version)
                }

                let issuer = match &deployer {
                    ContractDeployer::ContractIdentifier(id) => Some(id.issuer.to_owned()),
                    _ => None,
                };
                if active_contract.issuer.is_none() || active_contract.issuer != issuer {
                    active_contract.update_issuer(issuer)
                }
            }

            self.contracts_lookup.insert(
                contract_location.clone(),
                ContractMetadata {
                    base_location: base_location.clone(),
                    manifest_location: manifest_location.clone(),
                    relative_path,
                    clarity_version: contract_state.clarity_version,
                    deployer,
                },
            );
        }
        self.protocols.insert(manifest_location, protocol);
    }

    pub fn clear_protocol(&mut self, manifest_location: &Path) {
        if let Some(protocol) = self.protocols.remove(manifest_location) {
            for (contract_location, _) in protocol.contracts.iter() {
                self.contracts_lookup.remove(contract_location);
            }
        }
    }

    pub fn clear_protocol_associated_with_contract(
        &mut self,
        contract_location: &Path,
    ) -> Option<PathBuf> {
        match self.contracts_lookup.get(contract_location) {
            Some(contract_metadata) => {
                let manifest_location = contract_metadata.manifest_location.clone();
                self.clear_protocol(&manifest_location);
                Some(manifest_location)
            }
            None => None,
        }
    }

    pub fn get_completion_items_for_contract(
        &self,
        contract_location: &Path,
        position: &Position,
    ) -> Vec<ls_types::CompletionItem> {
        let Some(active_contract) = self.active_contracts.get(contract_location) else {
            return vec![];
        };

        let contract_calls = self
            .contracts_lookup
            .get(contract_location)
            .and_then(|d| self.protocols.get(&d.manifest_location))
            .map(|p| p.get_contract_calls_for_contract(contract_location))
            .unwrap_or_default();

        let expressions = active_contract.expressions.as_ref();
        let active_contract_defined_data = ContractDefinedData::new(
            active_contract.clarity_version,
            expressions.unwrap_or(&vec![]),
            position,
        );
        let should_wrap = match self.settings.completion_smart_parenthesis_wrap {
            true => check_if_should_wrap(&active_contract.source, position),
            false => true,
        };

        build_completion_item_list(
            &active_contract.clarity_version,
            expressions.unwrap_or(&vec![]),
            &Position {
                line: position.line + 1,
                character: position.character + 1,
            },
            &active_contract_defined_data,
            contract_calls,
            should_wrap,
            self.settings.completion_include_native_placeholders,
        )
    }

    pub fn get_document_symbols_for_contract(
        &self,
        contract_location: &Path,
    ) -> Vec<DocumentSymbol> {
        let Some(active_contract) = self.active_contracts.get(contract_location) else {
            return vec![];
        };

        let Some(expressions) = &active_contract.expressions else {
            return vec![];
        };

        let ast_symbols = ASTSymbols::new(active_contract.clarity_version);
        ast_symbols.get_symbols(expressions)
    }

    pub fn get_definition_location(
        &self,
        contract_location: &Path,
        position: &Position,
    ) -> Option<ls_types::Location> {
        let contract = self.active_contracts.get(contract_location)?;
        let position = Position {
            line: position.line + 1,
            character: position.character + 1,
        };

        let expressions = contract.expressions.as_ref()?;
        let position_hash = get_atom_or_field_start_at_position(&position, expressions)?;
        // Use holder variable to make sure temporary definitions live long enough
        let mut definitions_holder = None;
        let definitions = contract.definitions.as_ref().unwrap_or_else(|| {
            definitions_holder.insert(get_definitions(
                contract.clarity_version,
                expressions,
                contract.issuer.clone(),
            ))
        });

        match definitions.get(&position_hash)? {
            DefinitionLocation::Internal(range) => Some(Location {
                uri: paths::path_to_url_string(contract_location)
                    .ok()?
                    .parse()
                    .ok()?,
                range: *range,
            }),
            DefinitionLocation::External(contract_identifier, name) => {
                let metadata = self.contracts_lookup.get(contract_location)?;
                let protocol = self.protocols.get(&metadata.manifest_location)?;
                let definition_contract_location =
                    protocol.locations_lookup.get(contract_identifier)?;
                let uri = paths::path_to_url_string(definition_contract_location)
                    .ok()?
                    .parse()
                    .ok()?;

                // if the contract is opened and eventually contains unsaved changes,
                // its public definitions are computed on the fly, which is fairly fast
                if let Some(expressions) = self
                    .active_contracts
                    .get(definition_contract_location)
                    .and_then(|c| c.expressions.as_ref())
                {
                    let mut definitions = HashMap::new();
                    get_public_function_and_trait_definitions(&mut definitions, expressions);
                    return Some(Location {
                        uri,
                        range: *definitions.get(name)?,
                    });
                };

                Some(Location {
                    uri,
                    range: *protocol
                        .contracts
                        .get(definition_contract_location)?
                        .definitions
                        .get(name)?,
                })
            }
        }
    }

    pub fn get_hover_data(
        &self,
        contract_location: &Path,
        position: &ls_types::Position,
    ) -> Option<Hover> {
        let contract = self.active_contracts.get(contract_location)?;
        let position = Position {
            line: position.line + 1,
            character: position.character + 1,
        };
        let documentation =
            get_expression_documentation(&position, contract.expressions.as_ref()?)?;

        Some(Hover {
            contents: ls_types::HoverContents::Markup(ls_types::MarkupContent {
                kind: ls_types::MarkupKind::Markdown,
                value: documentation,
            }),
            range: None,
        })
    }

    pub fn get_code_lenses(&self, contract_location: &Path) -> Vec<ls_types::CodeLens> {
        let mut code_lenses = Vec::new();

        if !self.settings.static_cost_analysis {
            return code_lenses;
        }

        // Get the contract metadata to find the manifest
        let Some(contract_metadata) = self.contracts_lookup.get(contract_location) else {
            return code_lenses;
        };

        // Get the protocol state to find the contract state
        let Some(protocol_state) = self.protocols.get(&contract_metadata.manifest_location) else {
            return code_lenses;
        };

        let Some(contract_state) = protocol_state.contracts.get(contract_location) else {
            return code_lenses;
        };

        // Get the stored cost analysis
        let Some(cost_analysis) = contract_state.cost_analysis.as_ref() else {
            return code_lenses;
        };

        // Compute current function ranges from the live editor content so we can
        // detect which functions have moved (i.e. been edited) since the last save.
        let current_ranges = self
            .active_contracts
            .get(contract_location)
            .and_then(|ac| ac.expressions.as_ref())
            .map(|exprs| {
                let mut ranges = HashMap::new();
                get_all_function_definitions(&mut ranges, exprs);
                get_trait_definitions(&mut ranges, exprs);
                ranges
            });

        // Create a CodeLens for each function definition
        for (function_name, saved_range) in &contract_state.definitions {
            // If the function's range in the live source differs from the saved range,
            // the function (or something above it) was edited — skip the stale lens.
            if let Some(ref current) = current_ranges {
                if current.get(function_name) != Some(saved_range) {
                    continue;
                }
            }

            let function_name_str = function_name.to_string();
            if let Some((cost_node, _)) = cost_analysis.get(&function_name_str) {
                if !cost_percentages(&cost_node.max)
                    .exceeds_threshold(self.settings.static_cost_threshold_percentage)
                {
                    continue;
                }

                let cost_text = Self::format_cost_for_codelens(
                    cost_node,
                    self.settings.static_cost_display_format,
                );

                let code_lens_line = saved_range.start.line;
                let code_lens_range = ls_types::Range {
                    start: ls_types::Position {
                        line: code_lens_line,
                        character: 0,
                    },
                    end: ls_types::Position {
                        line: code_lens_line,
                        character: u32::MAX,
                    },
                };

                code_lenses.push(ls_types::CodeLens {
                    range: code_lens_range,
                    command: Some(ls_types::Command {
                        title: cost_text,
                        command: "clarity.staticCostLens".to_string(),
                        arguments: None,
                    }),
                    data: None,
                });
            }
        }

        code_lenses
    }

    fn format_cost_for_codelens(cost_node: &StaticCost, display: CostDisplayFormat) -> String {
        let cost = &cost_node.max;

        match display {
            CostDisplayFormat::Exact => {
                format!(
                    "runtime: {}, read count: {}, read length: {}, write count: {}, write length: {}",
                    format_abbreviated_number(cost.runtime),
                    format_abbreviated_number(cost.read_count),
                    format_abbreviated_number(cost.read_length),
                    format_abbreviated_number(cost.write_count),
                    format_abbreviated_number(cost.write_length)
                )
            }
            CostDisplayFormat::Percentage => {
                let pct = cost_percentages(cost);
                format!(
                    "runtime: {:.1}%, read count: {:.1}%, read length: {:.1}%, write count: {:.1}%, write length: {:.1}%",
                    pct.runtime, pct.read_count, pct.read_length, pct.write_count, pct.write_length,
                )
            }
        }
    }

    pub fn get_signature_help(
        &self,
        contract_location: &Path,
        position: &ls_types::Position,
        active_signature: Option<u32>,
    ) -> Option<SignatureHelp> {
        let contract = self.active_contracts.get(contract_location)?;
        let position = Position {
            line: position.line + 1,
            character: position.character + 1,
        };

        let signatures = get_signatures(contract, &position)?;

        Some(SignatureHelp {
            signatures,
            active_signature,
            active_parameter: None,
        })
    }

    pub fn get_aggregated_diagnostics(
        &self,
    ) -> (
        Vec<(PathBuf, Vec<LintDiagnostic>)>,
        Option<(MessageType, String)>,
    ) {
        let mut contracts = vec![];
        let mut erroring_files = HashSet::new();
        let mut warning_files = HashSet::new();

        for (_, protocol_state) in self.protocols.iter() {
            for (contract_url, state) in protocol_state.contracts.iter() {
                let mut diags: Vec<LintDiagnostic> = vec![];

                let ContractMetadata { relative_path, .. } = self
                    .contracts_lookup
                    .get(contract_url)
                    .expect("contract not in lookup");

                // Collect non-lint diagnostics
                if !state.errors.is_empty() {
                    erroring_files.insert(relative_path.clone());
                    diags.extend(state.errors.iter().cloned().map(LintDiagnostic::from));
                }
                if !state.warnings.is_empty() {
                    warning_files.insert(relative_path.clone());
                    diags.extend(state.warnings.iter().cloned().map(LintDiagnostic::from));
                }
                diags.extend(state.notes.iter().cloned().map(LintDiagnostic::from));

                // Collect lint diagnostics directly with their lint names
                for ld in state.lint_diagnostics.iter() {
                    match ld.diagnostic.level {
                        ClarityLevel::Error => {
                            erroring_files.insert(relative_path.clone());
                        }
                        ClarityLevel::Warning => {
                            warning_files.insert(relative_path.clone());
                        }
                        ClarityLevel::Note => {}
                    }
                    diags.push(ld.clone());
                }
                contracts.push((contract_url.clone(), diags));
            }
        }

        let tldr = match (erroring_files.len(), warning_files.len()) {
            (0, 0) => None,
            (0, _warnings) => Some((
                MessageType::WARNING,
                format!(
                    "Warning detected in following contracts: {}",
                    warning_files.into_iter().collect::<Vec<_>>().join(", ")
                ),
            )),
            (_errors, 0) => Some((
                MessageType::ERROR,
                format!(
                    "Errors detected in following contracts: {}",
                    erroring_files.into_iter().collect::<Vec<_>>().join(", ")
                ),
            )),
            (_errors, _warnings) => Some((
                MessageType::ERROR,
                format!(
                    "Errors and warnings detected in following contracts: {}",
                    erroring_files.into_iter().collect::<Vec<_>>().join(", ")
                ),
            )),
        };

        (contracts, tldr)
    }

    pub fn get_env_simnet_diagnostics(&self) -> Vec<(PathBuf, Vec<LspDiagnostic>)> {
        let mut result = Vec::new();
        for (path, contract_data) in &self.active_contracts {
            let Ok(spans) = get_env_simnet_spans(&contract_data.source) else {
                continue;
            };
            if spans.is_empty() {
                continue;
            }
            let diags = spans
                .iter()
                .map(|span| LspDiagnostic {
                    range: Range {
                        start: Position {
                            line: span.start_line - 1,
                            character: span.start_column - 1,
                        },
                        end: Position {
                            line: span.end_line - 1,
                            character: span.end_column,
                        },
                    },
                    severity: Some(DiagnosticSeverity::HINT),
                    source: Some("clarity".to_string()),
                    message: "simnet-only code (excluded from on-chain deployments)".to_string(),
                    tags: Some(vec![DiagnosticTag::UNNECESSARY]),
                    ..LspDiagnostic::default()
                })
                .collect();
            result.push((path.clone(), diags));
        }
        result
    }

    pub fn insert_active_contract(
        &mut self,
        contract_location: PathBuf,
        clarity_version: ClarityVersion,
        issuer: Option<StandardPrincipalData>,
        source: String,
    ) {
        let epoch = StacksEpochId::Epoch21;
        let contract = ActiveContractData::new(clarity_version, epoch, issuer, source);
        self.active_contracts.insert(contract_location, contract);
    }

    pub fn update_active_contract(
        &mut self,
        contract_location: &Path,
        source: &str,
        with_definitions: bool,
    ) -> Result<(), String> {
        let contract_state = self
            .active_contracts
            .get_mut(contract_location)
            .ok_or("contract not in active_contracts")?;
        contract_state.update_sources(source, with_definitions);
        Ok(())
    }
}

#[derive(Clone, Default, Debug)]
pub struct ProtocolState {
    contracts: HashMap<PathBuf, ContractState>,
    locations_lookup: HashMap<QualifiedContractIdentifier, PathBuf>,
}

impl ProtocolState {
    pub fn new() -> Self {
        ProtocolState::default()
    }

    pub fn consolidate(
        &mut self,
        locations: &mut HashMap<QualifiedContractIdentifier, PathBuf>,
        asts: &mut BTreeMap<QualifiedContractIdentifier, ContractAST>,
        deps: &mut BTreeMap<QualifiedContractIdentifier, DependencySet>,
        diags: &mut HashMap<QualifiedContractIdentifier, Vec<ClarityDiagnostic>>,
        lint_diags: &mut HashMap<QualifiedContractIdentifier, Vec<LintDiagnostic>>,
        definitions: &mut HashMap<QualifiedContractIdentifier, HashMap<ClarityName, Range>>,
        analyses: &mut HashMap<QualifiedContractIdentifier, Option<ContractAnalysis>>,
        clarity_versions: &mut HashMap<QualifiedContractIdentifier, ClarityVersion>,
        cost_analyses: &mut HashMap<
            QualifiedContractIdentifier,
            HashMap<String, (StaticCost, Option<HashMap<String, (u64, u64)>>)>,
        >,
    ) {
        // Remove old paths
        // TODO(lgalabru)

        // Add / Replace new paths
        for (contract_id, contract_location) in locations.iter() {
            let Some((contract_id, ast)) = asts.remove_entry(contract_id) else {
                continue;
            };
            let deps = deps.remove(&contract_id).unwrap_or_default();
            let diags = diags.remove(&contract_id).unwrap_or_default();
            let lint_diagnostics = lint_diags.remove(&contract_id).unwrap_or_default();
            let analysis = analyses.remove(&contract_id).unwrap_or_default();
            let clarity_version = clarity_versions
                .remove(&contract_id)
                .unwrap_or(DEFAULT_CLARITY_VERSION);

            let definitions = definitions.remove(&contract_id).unwrap_or_default();
            let cost_analysis = cost_analyses.remove(&contract_id);

            let contract_state = ContractState::new(
                ast,
                deps,
                diags,
                lint_diagnostics,
                analysis,
                definitions,
                clarity_version,
                cost_analysis,
            );
            self.contracts
                .insert(contract_location.clone(), contract_state);

            self.locations_lookup
                .insert(contract_id, contract_location.clone());
        }
    }

    pub fn get_contract_calls_for_contract(&self, contract_uri: &Path) -> Vec<CompletionItem> {
        let mut contract_calls = vec![];
        for (url, contract_state) in self.contracts.iter() {
            if !contract_uri.eq(url) {
                contract_calls.append(&mut contract_state.contract_calls.clone());
            }
        }
        contract_calls
    }
}

pub use clarity_repl::utils::Environment;

fn tag_diagnostics(
    environment: Environment,
    found_env_simnet: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if found_env_simnet {
        for ref mut diag in diagnostics {
            let _ = write!(diag.message, " ({environment})");
        }
    }
}

fn tag_lint_diagnostics(
    environment: Environment,
    found_env_simnet: bool,
    lint_diagnostics: &mut Vec<LintDiagnostic>,
) {
    if found_env_simnet {
        for ld in lint_diagnostics {
            let _ = write!(ld.diagnostic.message, " ({environment})");
        }
    }
}

pub async fn build_state(
    manifest_location: &Path,
    protocol_state: &mut ProtocolState,
    file_accessor: Option<&dyn FileAccessor>,
    static_cost_analysis: bool,
) -> Result<(), String> {
    let mut locations = HashMap::new();
    let mut asts = BTreeMap::new();
    let mut deps = BTreeMap::new();
    let mut diagnostics = HashMap::new();
    let mut lint_diagnostics: HashMap<QualifiedContractIdentifier, Vec<LintDiagnostic>> =
        HashMap::new();
    let mut analyses = HashMap::new();
    let mut definitions = HashMap::new();
    let mut clarity_versions = HashMap::new();

    // In the LSP use case, trying to load an existing deployment
    // might not be suitable, in an edition context, we should
    // expect contracts to be created, edited, removed.
    // A on-disk deployment could quickly lead to an outdated
    // view of the repo.
    let manifest = match file_accessor {
        None => ProjectManifest::from_location(manifest_location, false)?,
        Some(file_accessor) => {
            ProjectManifest::from_file_accessor(manifest_location, false, file_accessor).await?
        }
    };

    let mut global_found_env_simnet = false;
    let mut session = initiate_session_from_manifest(&manifest);
    for environment in CHECK_ENVIRONMENTS {
        let (deployment, mut artifacts, found_env_simnet) = generate_default_deployment(
            &manifest,
            &StacksNetwork::Simnet,
            false,
            file_accessor,
            None,
            environment,
        )
        .await?;
        global_found_env_simnet |= found_env_simnet;

        session = initiate_session_from_manifest(&manifest);
        let contracts =
            update_session_with_deployment_plan(&mut session, &deployment, Some(&artifacts.asts));
        for (contract_id, mut result) in contracts.into_iter() {
            let Some((_, contract_location)) = deployment.contracts.get(&contract_id) else {
                continue;
            };
            locations.insert(contract_id.clone(), contract_location.clone());
            if let Some(contract_metadata) = manifest.contracts_settings.get(contract_location) {
                clarity_versions.insert(contract_id.clone(), contract_metadata.clarity_version);
            } else {
                let contract_name = contract_location
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or_default()
                    .to_string();

                if manifest
                    .project
                    .override_boot_contracts_source
                    .contains_key(&contract_name)
                {
                    let (_, version) =
                        clarity_repl::repl::boot::get_boot_contract_epoch_and_clarity_version(
                            &contract_name,
                        );
                    clarity_versions.insert(contract_id.clone(), version);
                }
            }

            match result {
                Ok(annotated) => {
                    let AnnotatedExecutionResult {
                        mut execution_result,
                        lint_diagnostics: mut contract_lint_diags,
                    } = annotated;
                    if let Some(entry) = artifacts.diags.get_mut(&contract_id) {
                        tag_diagnostics(
                            environment,
                            global_found_env_simnet,
                            &mut execution_result.diagnostics,
                        );
                        entry.append(&mut execution_result.diagnostics);
                    }
                    tag_lint_diagnostics(
                        environment,
                        global_found_env_simnet,
                        &mut contract_lint_diags,
                    );
                    lint_diagnostics
                        .entry(contract_id.clone())
                        .or_default()
                        .extend(contract_lint_diags);

                    if let EvaluationResult::Contract(contract_result) = execution_result.result {
                        if let Some(ast) = artifacts.asts.get(&contract_id) {
                            let mut v = HashMap::new();
                            get_all_function_definitions(&mut v, &ast.expressions);
                            get_trait_definitions(&mut v, &ast.expressions);
                            definitions.insert(contract_id.clone(), v);
                        }
                        analyses
                            .insert(contract_id.clone(), Some(contract_result.contract.analysis));
                    };
                }
                Err(ref mut diags) => {
                    if let Some(entry) = artifacts.diags.get_mut(&contract_id) {
                        tag_diagnostics(environment, global_found_env_simnet, &mut *diags);
                        entry.append(diags);
                    }
                    continue;
                }
            };
        }

        // overwrite the asts and deps
        asts = artifacts.asts;
        deps = artifacts.deps;

        // merge the diags
        for (contract_id, diags) in &mut artifacts.diags {
            let entry = diagnostics
                .entry(contract_id.clone())
                .or_insert_with(Vec::new);
            entry.append(diags);
        }

        if !global_found_env_simnet {
            break;
        }
    }

    // Compute cost analysis for each contract
    // Note: Cost analysis must run AFTER contracts are deployed to the session
    // static_cost_tree needs the contract to be available in the global context
    let mut cost_analyses = HashMap::new();
    if static_cost_analysis {
        for contract_id in locations.keys() {
            // Skip cost analysis for empty contracts (no expressions to analyze)
            if asts
                .get(contract_id)
                .is_none_or(|ast| ast.expressions.is_empty())
            {
                continue;
            }

            let clarity_version = clarity_versions
                .get(contract_id)
                .copied()
                .unwrap_or(DEFAULT_CLARITY_VERSION);

            // Run static_cost_tree for this contract
            if let Some(cost_analysis) =
                get_cost_analysis(&mut session, contract_id, clarity_version).await
            {
                clarity_repl::uprint!(
                    "[LSP] Cost analysis completed for {}: {} functions analyzed",
                    contract_id,
                    cost_analysis.len()
                );
                cost_analyses.insert(contract_id.clone(), cost_analysis);
            } else {
                clarity_repl::uprint!("[LSP] Cost analysis failed for contract: {}", contract_id);
            }
        }
    }

    protocol_state.consolidate(
        &mut locations,
        &mut asts,
        &mut deps,
        &mut diagnostics,
        &mut lint_diagnostics,
        &mut definitions,
        &mut analyses,
        &mut clarity_versions,
        &mut cost_analyses,
    );

    Ok(())
}

// Helper function to compute cost analysis for a contract
async fn get_cost_analysis(
    session: &mut clarity_repl::repl::Session,
    contract_id: &QualifiedContractIdentifier,
    clarity_version: ClarityVersion,
) -> Option<HashMap<String, (StaticCost, Option<HashMap<String, (u64, u64)>>)>> {
    use clarity::vm::contexts::{CallStack, ContractContext, ExecutionState, InvocationContext};
    use clarity::vm::errors::{VmExecutionError, VmInternalError};
    use clarity_static_cost::static_cost::static_cost;

    clarity_repl::uprint!(
        "[LSP] get_cost_analysis called for contract: {} (clarity version: {:?})",
        contract_id,
        clarity_version
    );

    let tx_sender: clarity_types::types::PrincipalData = session.interpreter.get_tx_sender().into();
    let epoch = session.interpreter.datastore.get_current_epoch();

    let mut global_context = session
        .interpreter
        .get_global_context(epoch, false)
        .map_err(|e| {
            clarity_repl::uprint!("[LSP] Failed to get global context: {}", e);
            e
        })
        .ok()?;

    global_context.begin();

    let cost_result: Result<
        HashMap<String, (StaticCost, Option<HashMap<String, (u64, u64)>>)>,
        clarity::vm::errors::VmExecutionError,
    > = global_context.execute(|g| {
        let contract_context = ContractContext::new(contract_id.clone(), clarity_version);
        let mut call_stack = CallStack::new();

        let invoke_ctx = InvocationContext {
            contract_context: &contract_context,
            sender: Some(tx_sender.clone()),
            caller: Some(tx_sender),
            sponsor: None,
        };
        let mut env = ExecutionState {
            global_context: g,
            call_stack: &mut call_stack,
        };

        // Use static_cost which returns (StaticCost, Option<TraitCount>)
        // TraitCount is HashMap<String, (u64, u64)>, so we can use it directly
        static_cost(&mut env, &invoke_ctx, contract_id).map_err(|e| {
            clarity_repl::uprint!("[LSP] static_cost failed with error: {}", e);
            let error_msg = format!("Cost analysis failed for contract {}: {}", contract_id, e);
            VmExecutionError::Internal(VmInternalError::Expect(error_msg))
        })
    });

    cost_result
        .map_err(|e| {
            clarity_repl::uprint!("[LSP] Cost analysis failed with error: {:?}", e);
        })
        .ok()
}
