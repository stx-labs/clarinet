use std::borrow::BorrowMut;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::vec;

use clarinet_deployments::{
    generate_default_deployment, initiate_session_from_manifest,
    update_session_with_deployment_plan,
};
use clarinet_files::{FileAccessor, FileLocation, ProjectManifest, StacksNetwork};
use clarity::vm::ast::build_ast;
use clarity_repl::analysis::ast_dependency_detector::DependencySet;
use clarity_repl::clarity::analysis::ContractAnalysis;
use clarity_repl::clarity::diagnostic::{Diagnostic as ClarityDiagnostic, Level as ClarityLevel};
use clarity_repl::clarity::vm::ast::ContractAST;
use clarity_repl::clarity::vm::types::{QualifiedContractIdentifier, StandardPrincipalData};
use clarity_repl::clarity::vm::EvaluationResult;
use clarity_repl::clarity::{ClarityName, ClarityVersion, StacksEpochId, SymbolicExpression};
use clarity_repl::repl::{ContractDeployer, DEFAULT_CLARITY_VERSION};
use lsp_types::{
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
                definitions: Some(get_definitions(&ast.expressions, issuer)),
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
            self.definitions = Some(get_definitions(expressions, self.issuer.clone()));
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

#[derive(Debug, Clone, PartialEq)]
pub struct ContractState {
    contract_calls: Vec<CompletionItem>,
    errors: Vec<ClarityDiagnostic>,
    warnings: Vec<ClarityDiagnostic>,
    notes: Vec<ClarityDiagnostic>,
    contract_id: QualifiedContractIdentifier,
    analysis: Option<ContractAnalysis>,
    definitions: HashMap<ClarityName, Range>,
    location: FileLocation,
    clarity_version: ClarityVersion,
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
        }
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
    ) -> Vec<lsp_types::CompletionItem> {
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
        let active_contract_defined_data =
            ContractDefinedData::new(expressions.unwrap_or(&vec![]), position);
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

        let ast_symbols = ASTSymbols::new();
        ast_symbols.get_symbols(expressions)
    }

    pub fn get_definition_location(
        &self,
        contract_location: &FileLocation,
        position: &Position,
    ) -> Option<lsp_types::Location> {
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
            definitions_holder.insert(get_definitions(expressions, contract.issuer.clone()))
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
        position: &lsp_types::Position,
    ) -> Option<Hover> {
        let contract = self.active_contracts.get(contract_location)?;
        let position = Position {
            line: position.line + 1,
            character: position.character + 1,
        };
        let documentation =
            get_expression_documentation(&position, contract.expressions.as_ref()?)?;

        Some(Hover {
            contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: documentation,
            }),
            range: None,
        })
    }

    pub fn get_signature_help(
        &self,
        contract_location: &FileLocation,
        position: &lsp_types::Position,
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

            let contract_state = ContractState::new(
                contract_id.clone(),
                ast,
                deps,
                diags,
                analysis,
                definitions,
                contract_location.clone(),
                clarity_version,
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

    let (deployment, mut artifacts) = generate_default_deployment(
        &manifest,
        &StacksNetwork::Simnet,
        false,
        file_accessor,
        Some(StacksEpochId::Epoch21),
    )
    .await?;

    let mut session = initiate_session_from_manifest(&manifest);
    let contracts = update_session_with_deployment_plan(
        &mut session,
        &deployment,
        Some(&artifacts.asts),
        Some(StacksEpochId::Epoch21),
    );
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

    protocol_state.consolidate(
        &mut locations,
        &mut artifacts.asts,
        &mut artifacts.deps,
        &mut artifacts.diags,
        &mut definitions,
        &mut analyses,
        &mut clarity_versions,
    );

    Ok(())
}
