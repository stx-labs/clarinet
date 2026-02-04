use std::borrow::BorrowMut;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::vec;

use clarinet_deployments::{
    generate_default_deployment, initiate_session_from_manifest,
    update_session_with_deployment_plan,
};
use clarinet_files::{FileAccessor, FileLocation, ProjectManifest, StacksNetwork};
use clarity::vm::ast::build_ast;
use clarity::vm::costs::analysis::CostAnalysisNode;
use clarity::vm::costs::ExecutionCost;
use clarity_repl::analysis::ast_dependency_detector::DependencySet;
use clarity_repl::clarity::analysis::ContractAnalysis;
use clarity_repl::clarity::diagnostic::{Diagnostic as ClarityDiagnostic, Level as ClarityLevel};
use clarity_repl::clarity::vm::ast::ContractAST;
use clarity_repl::clarity::vm::types::{QualifiedContractIdentifier, StandardPrincipalData};
use clarity_repl::clarity::vm::EvaluationResult;
use clarity_repl::clarity::{ClarityName, ClarityVersion, StacksEpochId, SymbolicExpression};
use clarity_repl::repl::interpreter::BLOCK_LIMIT_MAINNET;
use clarity_repl::repl::{ContractDeployer, DEFAULT_CLARITY_VERSION};
use ls_types::{
    CompletionItem, DocumentSymbol, Hover, Location, MessageType, Position, Range, SignatureHelp,
};

use super::requests::capabilities::InitializationOptions;
use super::requests::completion::{
    build_completion_item_list, get_contract_calls, ContractDefinedData,
};
use super::requests::definitions::{
    get_definitions, get_public_function_and_trait_definitions, DefinitionLocation,
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
    #[allow(dead_code)]
    contract_id: QualifiedContractIdentifier,
    analysis: Option<ContractAnalysis>,
    definitions: HashMap<ClarityName, Range>,
    #[allow(dead_code)]
    location: FileLocation,
    clarity_version: ClarityVersion,
    cost_analysis: Option<HashMap<String, (CostAnalysisNode, Option<HashMap<String, (u64, u64)>>)>>,
}

impl ContractState {
    pub fn new(
        contract_id: QualifiedContractIdentifier,
        _ast: ContractAST,
        _deps: DependencySet,
        diags: Vec<ClarityDiagnostic>,
        analysis: Option<ContractAnalysis>,
        definitions: HashMap<ClarityName, Range>,
        location: FileLocation,
        clarity_version: ClarityVersion,
        cost_analysis: Option<
            HashMap<String, (CostAnalysisNode, Option<HashMap<String, (u64, u64)>>)>,
        >,
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
            contract_id,
            contract_calls,
            errors,
            warnings,
            notes,
            analysis,
            definitions,
            location,
            clarity_version,
            cost_analysis,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CostPercents {
    pub runtime: f64,
    pub write_length: f64,
    pub write_count: f64,
    pub read_length: f64,
    pub read_count: f64,
}

fn u64_to_f64(value: u64) -> f64 {
    // XXX Not sure if there's a safer conversion
    value as f64
}

// Compute total cost from a CostAnalysisNode tree by summing all children's costs
fn compute_total_cost_from_tree(cost_node: &CostAnalysisNode) -> ExecutionCost {
    let mut total = cost_node.cost.max.clone();

    // Recursively sum costs from all children
    for child in &cost_node.children {
        let child_cost = compute_total_cost_from_tree(child);
        total.runtime += child_cost.runtime;
        total.read_count += child_cost.read_count;
        total.read_length += child_cost.read_length;
        total.write_count += child_cost.write_count;
        total.write_length += child_cost.write_length;
    }

    total
}

// Compute total min cost from a CostAnalysisNode tree by summing all children's costs
fn compute_total_min_cost_from_tree(cost_node: &CostAnalysisNode) -> ExecutionCost {
    let mut total = cost_node.cost.min.clone();

    // Recursively sum costs from all children
    for child in &cost_node.children {
        let child_cost = compute_total_min_cost_from_tree(child);
        total.runtime += child_cost.runtime;
        total.read_count += child_cost.read_count;
        total.read_length += child_cost.read_length;
        total.write_count += child_cost.write_count;
        total.write_length += child_cost.write_length;
    }

    total
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

fn get_cost_percents(cost: &ExecutionCost, block_limits: &ExecutionCost) -> CostPercents {
    let runtime = (u64_to_f64(cost.runtime) / u64_to_f64(block_limits.runtime)) * 100.0;
    let write_length =
        (u64_to_f64(cost.write_length) / u64_to_f64(block_limits.write_length)) * 100.0;
    let write_count = (u64_to_f64(cost.write_count) / u64_to_f64(block_limits.write_count)) * 100.0;
    let read_length = (u64_to_f64(cost.read_length) / u64_to_f64(block_limits.read_length)) * 100.0;
    let read_count = (u64_to_f64(cost.read_count) / u64_to_f64(block_limits.read_count)) * 100.0;

    CostPercents {
        runtime,
        write_length,
        write_count,
        read_length,
        read_count,
    }
}

#[derive(Clone, Debug)]
pub struct ContractMetadata {
    pub base_location: FileLocation,
    pub manifest_location: FileLocation,
    pub relative_path: String,
    pub clarity_version: ClarityVersion,
    pub deployer: ContractDeployer,
}

#[derive(Clone, Default, Debug)]
pub struct EditorState {
    pub protocols: HashMap<FileLocation, ProtocolState>,
    pub contracts_lookup: HashMap<FileLocation, ContractMetadata>,
    pub active_contracts: HashMap<FileLocation, ActiveContractData>,
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

    pub fn index_protocol(&mut self, manifest_location: FileLocation, protocol: ProtocolState) {
        let mut base_location = manifest_location.clone();

        match base_location.borrow_mut() {
            FileLocation::FileSystem { path } => {
                let mut parent = path.clone();
                parent.pop();
                parent.pop();
            }
            FileLocation::Url { url } => {
                let mut segments = url
                    .path_segments_mut()
                    .expect("could not find root location");
                segments.pop();
                segments.pop();
            }
        };

        for (contract_location, contract_state) in protocol.contracts.iter() {
            let relative_path = contract_location
                .get_relative_path_from_base(&base_location)
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

    pub fn clear_protocol(&mut self, manifest_location: &FileLocation) {
        if let Some(protocol) = self.protocols.remove(manifest_location) {
            for (contract_location, _) in protocol.contracts.iter() {
                self.contracts_lookup.remove(contract_location);
            }
        }
    }

    pub fn clear_protocol_associated_with_contract(
        &mut self,
        contract_location: &FileLocation,
    ) -> Option<FileLocation> {
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
        contract_location: &FileLocation,
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
        contract_location: &FileLocation,
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
        contract_location: &FileLocation,
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
                uri: contract_location.to_url_string().ok()?.parse().ok()?,
                range: *range,
            }),
            DefinitionLocation::External(contract_identifier, name) => {
                let metadata = self.contracts_lookup.get(contract_location)?;
                let protocol = self.protocols.get(&metadata.manifest_location)?;
                let definition_contract_location =
                    protocol.locations_lookup.get(contract_identifier)?;
                let uri = definition_contract_location
                    .to_url_string()
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
        contract_location: &FileLocation,
        position: &ls_types::Position,
    ) -> Option<Hover> {
        let contract = self.active_contracts.get(contract_location)?;
        let position = Position {
            line: position.line + 1,
            character: position.character + 1,
        };
        let documentation =
            get_expression_documentation(&position, contract.expressions.as_ref()?)?;

        // TODO: this is where we'd add cost analysis hover data
        // let cost_info = self.get_function_cost_info_for_hover(contract_location, position);

        // let mut hover_content = documentation;
        // if let Some(cost_text) = cost_info {
        //     hover_content.push_str("\n\n---\n\n");
        //     hover_content.push_str(&cost_text);
        // }

        Some(Hover {
            contents: ls_types::HoverContents::Markup(ls_types::MarkupContent {
                kind: ls_types::MarkupKind::Markdown,
                value: documentation,
            }),
            range: None,
        })
    }

    pub fn get_code_lenses(&self, contract_location: &FileLocation) -> Vec<ls_types::CodeLens> {
        let mut code_lenses = Vec::new();

        // Get the contract metadata to find the manifest
        let contract_metadata = match self.contracts_lookup.get(contract_location) {
            Some(md) => md,
            None => return code_lenses,
        };

        // Get the protocol state to find the contract state
        let protocol_state = match self.protocols.get(&contract_metadata.manifest_location) {
            Some(ps) => ps,
            None => return code_lenses,
        };

        let contract_state = match protocol_state.contracts.get(contract_location) {
            Some(cs) => cs,
            None => return code_lenses,
        };

        // Get the stored cost analysis
        let cost_analysis = match contract_state.cost_analysis.as_ref() {
            Some(ca) => ca,
            None => return code_lenses,
        };

        // Create a CodeLens for each function definition
        for (function_name, range) in &contract_state.definitions {
            // Convert ClarityName to String for lookup
            let function_name_str = function_name.to_string();
            if let Some((cost_node, _)) = cost_analysis.get(&function_name_str) {
                let cost_text = Self::format_cost_for_codelens(cost_node);

                let code_lens_line = range.start.line;
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
                        command: "clarity.showCostDetails".to_string(),
                        arguments,
                    }),
                    data: None,
                });
            }
        }

        code_lenses
    }

    fn format_cost_for_codelens(cost_node: &CostAnalysisNode) -> String {
        let total_cost = compute_total_cost_from_tree(cost_node);

        format!(
            "runtime: {}, read count: {}, read length: {}, write count: {}, write length: {}",
            format_abbreviated_number(total_cost.runtime),
            format_abbreviated_number(total_cost.read_count),
            format_abbreviated_number(total_cost.read_length),
            format_abbreviated_number(total_cost.write_count),
            format_abbreviated_number(total_cost.write_length)
        )
    }

    pub fn get_signature_help(
        &self,
        contract_location: &FileLocation,
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
        Vec<(FileLocation, Vec<ClarityDiagnostic>)>,
        Option<(MessageType, String)>,
    ) {
        let mut contracts = vec![];
        let mut erroring_files = HashSet::new();
        let mut warning_files = HashSet::new();

        for (_, protocol_state) in self.protocols.iter() {
            for (contract_url, state) in protocol_state.contracts.iter() {
                let mut diags = vec![];

                let ContractMetadata { relative_path, .. } = self
                    .contracts_lookup
                    .get(contract_url)
                    .expect("contract not in lookup");

                // Convert and collect errors
                if !state.errors.is_empty() {
                    erroring_files.insert(relative_path.clone());
                    for error in state.errors.iter() {
                        diags.push(error.clone());
                    }
                }

                // Convert and collect warnings
                if !state.warnings.is_empty() {
                    warning_files.insert(relative_path.clone());
                    for warning in state.warnings.iter() {
                        diags.push(warning.clone());
                    }
                }

                // Convert and collect notes
                for note in state.notes.iter() {
                    diags.push(note.clone());
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

    pub fn insert_active_contract(
        &mut self,
        contract_location: FileLocation,
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
        contract_location: &FileLocation,
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

    pub fn get_function_cost_analysis(
        &self,
        contract_location: &FileLocation,
        position: &ls_types::Position,
    ) -> Option<String> {
        crate::lsp_log!(
            "[LSP] get_function_cost_analysis called for {} at line {}",
            contract_location,
            position.line,
        );

        let contract_metadata = self.contracts_lookup.get(contract_location)?;
        let protocol_state = self.protocols.get(&contract_metadata.manifest_location)?;
        let contract_state = protocol_state.contracts.get(contract_location)?;

        // Get the stored cost analysis
        let cost_analysis = match contract_state.cost_analysis.as_ref() {
            Some(ca) => ca,
            None => {
                crate::lsp_log!("[LSP] No cost analysis stored for contract");
                return None;
            }
        };

        // Find which function the position is in by checking which function definition contains this position
        let mut function_name = None;
        for (name, range) in &contract_state.definitions {
            if position.line >= range.start.line
                && position.line <= range.end.line
                && position.character >= range.start.character
                && position.character <= range.end.character
            {
                function_name = Some(name.to_string());
                break;
            }
        }

        let function_name = match function_name {
            Some(name) => name,
            None => {
                crate::lsp_log!("[LSP] No function found at position");
                return None;
            }
        };

        let (cost_node, trait_count_opt) = match cost_analysis.get(&function_name) {
            Some((node, trait_count)) => (node, trait_count),
            None => {
                crate::lsp_log!("[LSP] No cost node found for function: {}", function_name);
                crate::lsp_log!(
                    "[LSP] Available functions: {:?}",
                    cost_analysis.keys().collect::<Vec<_>>()
                );
                return None;
            }
        };

        let block_limits = BLOCK_LIMIT_MAINNET;
        let total_cost_max = compute_total_cost_from_tree(cost_node);
        let total_cost_min = compute_total_min_cost_from_tree(cost_node);
        let percents = get_cost_percents(&total_cost_max, &block_limits);

        // Extract trait count from the stored data
        // TraitCount is HashMap<String, (u64, u64)>, so we use the size as the count
        let trait_count = trait_count_opt
            .as_ref()
            .map(|tc| tc.len() as u32)
            .unwrap_or(0u32);

        let result = serde_json::json!({
            "function": function_name,
            "cost": {
                "runtime": {
                    "min": total_cost_min.runtime,
                    "max": total_cost_max.runtime,
                },
                "read_count": {
                    "min": total_cost_min.read_count,
                    "max": total_cost_max.read_count,
                },
                "read_length": {
                    "min": total_cost_min.read_length,
                    "max": total_cost_max.read_length,
                },
                "write_count": {
                    "min": total_cost_min.write_count,
                    "max": total_cost_max.write_count,
                },
                "write_length": {
                    "min": total_cost_min.write_length,
                    "max": total_cost_max.write_length,
                },
            },
            "percentages": {
                "runtime": percents.runtime,
                "read_count": percents.read_count,
                "read_length": percents.read_length,
                "write_count": percents.write_count,
                "write_length": percents.write_length,
            },
            "limits": {
                "runtime": block_limits.runtime,
                "read_count": block_limits.read_count,
                "read_length": block_limits.read_length,
                "write_count": block_limits.write_count,
                "write_length": block_limits.write_length,
            },
            "trait_count": trait_count,
        })
        .to_string();

        Some(result)
    }

    pub fn get_cost_details(
        &self,
        contract_location: &FileLocation,
        function_name: &str,
    ) -> Option<String> {
        crate::lsp_log!(
            "[LSP] get_cost_details called for {} function {}",
            contract_location,
            function_name,
        );

        let contract_metadata = self.contracts_lookup.get(contract_location)?;
        let protocol_state = self.protocols.get(&contract_metadata.manifest_location)?;
        let contract_state = protocol_state.contracts.get(contract_location)?;

        // Get the stored cost analysis
        let cost_analysis = match contract_state.cost_analysis.as_ref() {
            Some(ca) => ca,
            None => {
                crate::lsp_log!("[LSP] No cost analysis stored for contract");
                return None;
            }
        };

        crate::lsp_log!(
            "[LSP] Looking for function '{}' in cost analysis. Available functions: {:?}",
            function_name,
            cost_analysis.keys().collect::<Vec<_>>()
        );

        let (cost_node, trait_count_opt) = match cost_analysis.get(function_name) {
            Some((node, trait_count)) => {
                let total_cost = compute_total_cost_from_tree(node);
                crate::lsp_log!(
                    "[LSP] Found cost node for '{}'. Total cost values: runtime={}, read_count={}, read_length={}, write_count={}, write_length={}",
                    function_name,
                    total_cost.runtime,
                    total_cost.read_count,
                    total_cost.read_length,
                    total_cost.write_count,
                    total_cost.write_length
                );
                (node, trait_count)
            }
            None => {
                crate::lsp_log!("[LSP] No cost node found for function: {}", function_name);
                crate::lsp_log!(
                    "[LSP] Available functions: {:?}",
                    cost_analysis.keys().collect::<Vec<_>>()
                );
                return None;
            }
        };

        let block_limits = BLOCK_LIMIT_MAINNET;
        let total_cost_max = compute_total_cost_from_tree(cost_node);
        let total_cost_min = compute_total_min_cost_from_tree(cost_node);
        let percents = get_cost_percents(&total_cost_max, &block_limits);

        crate::lsp_log!(
            "[LSP] Cost details for '{}': total runtime min={}, max={}, percentages={:?}",
            function_name,
            total_cost_min.runtime,
            total_cost_max.runtime,
            percents
        );

        // Extract trait count from the stored data
        // TraitCount is HashMap<String, (u64, u64)>, so we use the size as the count
        let trait_count = trait_count_opt
            .as_ref()
            .map(|tc| tc.len() as u32)
            .unwrap_or(0u32);

        let result = serde_json::json!({
            "function": function_name,
            "cost": {
                "runtime": {
                    "min": total_cost_min.runtime,
                    "max": total_cost_max.runtime,
                },
                "read_count": {
                    "min": total_cost_min.read_count,
                    "max": total_cost_max.read_count,
                },
                "read_length": {
                    "min": total_cost_min.read_length,
                    "max": total_cost_max.read_length,
                },
                "write_count": {
                    "min": total_cost_min.write_count,
                    "max": total_cost_max.write_count,
                },
                "write_length": {
                    "min": total_cost_min.write_length,
                    "max": total_cost_max.write_length,
                },
            },
            "percentages": {
                "runtime": percents.runtime,
                "read_count": percents.read_count,
                "read_length": percents.read_length,
                "write_count": percents.write_count,
                "write_length": percents.write_length,
            },
            "limits": {
                "runtime": block_limits.runtime,
                "read_count": block_limits.read_count,
                "read_length": block_limits.read_length,
                "write_count": block_limits.write_count,
                "write_length": block_limits.write_length,
            },
            "trait_count": trait_count,
        })
        .to_string();

        Some(result)
    }
}

#[derive(Clone, Default, Debug)]
pub struct ProtocolState {
    contracts: HashMap<FileLocation, ContractState>,
    locations_lookup: HashMap<QualifiedContractIdentifier, FileLocation>,
}

impl ProtocolState {
    pub fn new() -> Self {
        ProtocolState::default()
    }

    pub fn consolidate(
        &mut self,
        locations: &mut HashMap<QualifiedContractIdentifier, FileLocation>,
        asts: &mut BTreeMap<QualifiedContractIdentifier, ContractAST>,
        deps: &mut BTreeMap<QualifiedContractIdentifier, DependencySet>,
        diags: &mut HashMap<QualifiedContractIdentifier, Vec<ClarityDiagnostic>>,
        definitions: &mut HashMap<QualifiedContractIdentifier, HashMap<ClarityName, Range>>,
        analyses: &mut HashMap<QualifiedContractIdentifier, Option<ContractAnalysis>>,
        clarity_versions: &mut HashMap<QualifiedContractIdentifier, ClarityVersion>,
        cost_analyses: &mut HashMap<
            QualifiedContractIdentifier,
            HashMap<String, (CostAnalysisNode, Option<HashMap<String, (u64, u64)>>)>,
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
            let analysis = analyses.remove(&contract_id).unwrap_or_default();
            let clarity_version = clarity_versions
                .remove(&contract_id)
                .unwrap_or(DEFAULT_CLARITY_VERSION);

            let definitions = definitions.remove(&contract_id).unwrap_or_default();
            let cost_analysis = cost_analyses.remove(&contract_id);

            let contract_state = ContractState::new(
                contract_id.clone(),
                ast,
                deps,
                diags,
                analysis,
                definitions,
                contract_location.clone(),
                clarity_version,
                cost_analysis,
            );
            self.contracts
                .insert(contract_location.clone(), contract_state);

            self.locations_lookup
                .insert(contract_id, contract_location.clone());
        }
    }

    pub fn get_contract_calls_for_contract(
        &self,
        contract_uri: &FileLocation,
    ) -> Vec<CompletionItem> {
        let mut contract_calls = vec![];
        for (url, contract_state) in self.contracts.iter() {
            if !contract_uri.eq(url) {
                contract_calls.append(&mut contract_state.contract_calls.clone());
            }
        }
        contract_calls
    }
}

pub async fn build_state(
    manifest_location: &FileLocation,
    protocol_state: &mut ProtocolState,
    file_accessor: Option<&dyn FileAccessor>,
) -> Result<(), String> {
    let mut locations = HashMap::new();
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

    let (deployment, mut artifacts) =
        generate_default_deployment(&manifest, &StacksNetwork::Simnet, false, file_accessor)
            .await?;

    let mut session = initiate_session_from_manifest(&manifest);
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
                .to_path_buf()
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
            Ok(mut execution_result) => {
                if let Some(entry) = artifacts.diags.get_mut(&contract_id) {
                    entry.append(&mut execution_result.diagnostics);
                }

                if let EvaluationResult::Contract(contract_result) = execution_result.result {
                    if let Some(ast) = artifacts.asts.get(&contract_id) {
                        let mut v = HashMap::new();
                        get_public_function_and_trait_definitions(&mut v, &ast.expressions);
                        definitions.insert(contract_id.clone(), v);
                    }
                    analyses.insert(contract_id.clone(), Some(contract_result.contract.analysis));
                };
            }
            Err(ref mut diags) => {
                if let Some(entry) = artifacts.diags.get_mut(&contract_id) {
                    entry.append(diags);
                }
                continue;
            }
        };
    }

    // Compute cost analysis for each contract
    // Note: Cost analysis must run AFTER contracts are deployed to the session
    // static_cost_tree needs the contract to be available in the global context
    let mut cost_analyses = HashMap::new();
    for contract_id in locations.keys() {
        let clarity_version = clarity_versions
            .get(contract_id)
            .copied()
            .unwrap_or(DEFAULT_CLARITY_VERSION);

        crate::lsp_log!(
            "[LSP] Computing cost analysis for contract: {}",
            contract_id
        );

        // Run static_cost_tree for this contract
        if let Some(cost_analysis) =
            get_cost_analysis(&mut session, contract_id, clarity_version).await
        {
            crate::lsp_log!(
                "[LSP] Cost analysis completed for {}: {} functions analyzed",
                contract_id,
                cost_analysis.len()
            );
            cost_analyses.insert(contract_id.clone(), cost_analysis);
        } else {
            crate::lsp_log!("[LSP] Cost analysis failed for contract: {}", contract_id);
        }
    }

    protocol_state.consolidate(
        &mut locations,
        &mut artifacts.asts,
        &mut artifacts.deps,
        &mut artifacts.diags,
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
) -> Option<HashMap<String, (CostAnalysisNode, Option<HashMap<String, (u64, u64)>>)>> {
    use clarity::vm::contexts::{CallStack, ContractContext, Environment};
    use clarity::vm::costs::analysis::static_cost;
    use clarity::vm::errors::{VmExecutionError, VmInternalError};

    crate::lsp_log!(
        "[LSP] get_cost_analysis called for contract: {} (clarity version: {:?})",
        contract_id,
        clarity_version
    );

    let tx_sender: clarity_types::types::PrincipalData = session.interpreter.get_tx_sender().into();

    let mut global_context = session
        .interpreter
        .get_global_context(clarity_repl::clarity::StacksEpochId::Epoch21, false)
        .map_err(|e| {
            crate::lsp_log!("[LSP] Failed to get global context: {}", e);
            e
        })
        .ok()?;

    global_context.begin();

    let cost_result: Result<
        HashMap<String, (CostAnalysisNode, Option<HashMap<String, (u64, u64)>>)>,
        clarity::vm::errors::VmExecutionError,
    > = global_context.execute(|g| {
        let contract_context = ContractContext::new(contract_id.clone(), clarity_version);
        let mut call_stack = CallStack::new();

        let mut env = Environment::new(
            g,
            &contract_context,
            &mut call_stack,
            Some(tx_sender.clone()),
            Some(tx_sender),
            None,
        );

        // Use static_cost which returns (CostAnalysisNode, Option<TraitCount>)
        // TraitCount is HashMap<String, (u64, u64)>, so we can use it directly
        static_cost(&mut env, contract_id).map_err(|e| {
            crate::lsp_log!("[LSP] static_cost failed with error: {}", e);
            let error_msg = format!("Cost analysis failed for contract {}: {}", contract_id, e);
            VmExecutionError::Internal(VmInternalError::Expect(error_msg))
        })
    });

    cost_result
        .map_err(|e| {
            crate::lsp_log!("[LSP] Cost analysis failed with error: {:?}", e);
        })
        .ok()
}
