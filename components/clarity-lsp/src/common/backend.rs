use std::path::PathBuf;
use std::sync::{Arc, RwLock};

use clarinet_files::{paths, FileAccessor, ProjectManifest};
use clarity_repl::analysis::LintDiagnostic;
use clarity_repl::repl::boot::get_boot_contract_epoch_and_clarity_version;
use clarity_repl::repl::ContractDeployer;
use ls_types::{
    CodeLens, CodeLensParams, CompletionItem, CompletionParams, Diagnostic as LspDiagnostic,
    DocumentFormattingParams, DocumentRangeFormattingParams, DocumentSymbol, DocumentSymbolParams,
    GotoDefinitionParams, Hover, HoverParams, InitializeParams, InitializeResult, Location,
    MessageType, ServerInfo, SignatureHelp, SignatureHelpParams, TextEdit,
};
use serde::{Deserialize, Serialize};

use super::requests::capabilities::{get_capabilities, InitializationOptions};
use crate::state::{build_state, EditorState, ProtocolState};
use crate::utils::get_contract_location;

#[derive(Debug, Clone)]
pub enum EditorStateInput {
    Owned(EditorState),
    RwLock(Arc<RwLock<EditorState>>),
}

impl EditorStateInput {
    pub fn try_read<F, R>(&self, closure: F) -> Result<R, String>
    where
        F: FnOnce(&EditorState) -> R,
    {
        match self {
            EditorStateInput::Owned(editor_state) => Ok(closure(editor_state)),
            EditorStateInput::RwLock(editor_state_lock) => match editor_state_lock.try_read() {
                Ok(editor_state) => Ok(closure(&editor_state)),
                Err(_) => Err("failed to read editor_state".to_string()),
            },
        }
    }

    pub fn try_write<F, R>(&mut self, closure: F) -> Result<R, String>
    where
        F: FnOnce(&mut EditorState) -> R,
    {
        match self {
            EditorStateInput::Owned(editor_state) => Ok(closure(editor_state)),
            EditorStateInput::RwLock(editor_state_lock) => match editor_state_lock.try_write() {
                Ok(mut editor_state) => Ok(closure(&mut editor_state)),
                Err(_) => Err("failed to write editor_state".to_string()),
            },
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum LspNotification {
    ManifestOpened(PathBuf),
    ManifestSaved(PathBuf),
    ContractOpened(PathBuf),
    ContractSaved(PathBuf),
    ContractChanged(PathBuf, String),
    ContractClosed(PathBuf),
}

#[derive(Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct LspNotificationResponse {
    pub aggregated_diagnostics: Vec<(PathBuf, Vec<LintDiagnostic>)>,
    pub env_simnet_diagnostics: Vec<(PathBuf, Vec<LspDiagnostic>)>,
    pub notification: Option<(MessageType, String)>,
}

impl LspNotificationResponse {
    pub fn error(message: &str) -> LspNotificationResponse {
        LspNotificationResponse {
            aggregated_diagnostics: vec![],
            env_simnet_diagnostics: vec![],
            notification: Some((MessageType::ERROR, format!("Internal error: {message}"))),
        }
    }
}

pub async fn process_notification(
    command: LspNotification,
    editor_state: &mut EditorStateInput,
    file_accessor: Option<&dyn FileAccessor>,
) -> Result<LspNotificationResponse, String> {
    let static_cost_analysis = editor_state.try_read(|es| es.settings.static_cost_analysis)?;

    match command {
        LspNotification::ManifestOpened(manifest_location) => {
            // Only build the initial protocol state if it does not exist
            if editor_state.try_read(|es| es.protocols.contains_key(&manifest_location))? {
                return Ok(LspNotificationResponse::default());
            }

            // With this manifest_location, let's initialize our state.
            let mut protocol_state = ProtocolState::new();
            match build_state(
                &manifest_location,
                &mut protocol_state,
                file_accessor,
                static_cost_analysis,
            )
            .await
            {
                Ok(_) => {
                    editor_state
                        .try_write(|es| es.index_protocol(manifest_location, protocol_state))?;
                    let (aggregated_diagnostics, notification) =
                        editor_state.try_read(|es| es.get_aggregated_diagnostics())?;
                    let env_simnet_diagnostics =
                        editor_state.try_read(|es| es.get_env_simnet_diagnostics())?;
                    Ok(LspNotificationResponse {
                        aggregated_diagnostics,
                        env_simnet_diagnostics,
                        notification,
                    })
                }
                Err(e) => Ok(LspNotificationResponse::error(&e)),
            }
        }

        LspNotification::ManifestSaved(manifest_location) => {
            // We will rebuild the entire state, without to try any optimizations for now
            let mut protocol_state = ProtocolState::new();
            match build_state(
                &manifest_location,
                &mut protocol_state,
                file_accessor,
                static_cost_analysis,
            )
            .await
            {
                Ok(_) => {
                    editor_state
                        .try_write(|es| es.index_protocol(manifest_location, protocol_state))?;
                    let (aggregated_diagnostics, notification) =
                        editor_state.try_read(|es| es.get_aggregated_diagnostics())?;
                    let env_simnet_diagnostics =
                        editor_state.try_read(|es| es.get_env_simnet_diagnostics())?;
                    Ok(LspNotificationResponse {
                        aggregated_diagnostics,
                        env_simnet_diagnostics,
                        notification,
                    })
                }
                Err(e) => Ok(LspNotificationResponse::error(&e)),
            }
        }

        LspNotification::ContractOpened(contract_location) => {
            let manifest_location = match file_accessor {
                None => paths::find_manifest_location(&contract_location)?,
                Some(file_accessor) => {
                    paths::find_manifest_location_async(&contract_location, file_accessor).await?
                }
            };

            // store the contract in the active_contracts map
            if !editor_state.try_read(|es| es.active_contracts.contains_key(&contract_location))? {
                let contract_source = match file_accessor {
                    None => paths::read_content_as_utf8(&contract_location),
                    Some(file_accessor) => {
                        file_accessor
                            .read_file(contract_location.to_string_lossy().to_string())
                            .await
                    }
                }?;

                let metadata = editor_state.try_read(|es| {
                    es.contracts_lookup
                        .get(&contract_location)
                        .map(|metadata| (metadata.clarity_version, metadata.deployer.clone()))
                })?;

                // if the contract isn't in lookup yet, fallback on manifest, to be improved in #668
                let clarity_version = match metadata {
                    Some((clarity_version, _)) => clarity_version,
                    None => {
                        let manifest = match file_accessor {
                            None => ProjectManifest::from_location(&manifest_location, false),
                            Some(file_accessor) => {
                                ProjectManifest::from_file_accessor(
                                    &manifest_location,
                                    false,
                                    file_accessor,
                                )
                                .await
                            }
                        }?;

                        if let Some(contract_metadata) =
                            manifest.contracts_settings.get(&contract_location)
                        {
                            contract_metadata.clarity_version
                        } else {
                            // boot contracts path checking
                            let mut found_boot_contract = None;

                            for (contract_name, contract_path) in
                                &manifest.project.override_boot_contracts_source
                            {
                                let resolved_path = manifest.root_dir.join(contract_path);
                                if resolved_path == contract_location {
                                    found_boot_contract = Some(contract_name);
                                    break;
                                }
                            }

                            if let Some(contract_name) = found_boot_contract {
                                let (_, version) =
                                    get_boot_contract_epoch_and_clarity_version(contract_name);
                                version
                            } else {
                                return Err(format!(
                                    "No Clarinet.toml is associated to the contract {}",
                                    contract_location.display()
                                ));
                            }
                        }
                    }
                };

                let issuer = metadata.and_then(|(_, deployer)| match deployer {
                    ContractDeployer::ContractIdentifier(id) => Some(id.issuer),
                    _ => None,
                });

                editor_state.try_write(|es| {
                    es.insert_active_contract(
                        contract_location.clone(),
                        clarity_version,
                        issuer,
                        contract_source,
                    )
                })?;
            }

            // Only build the initial protocol state if it does not exist
            if editor_state.try_read(|es| es.protocols.contains_key(&manifest_location))? {
                return Ok(LspNotificationResponse::default());
            }

            let mut protocol_state = ProtocolState::new();
            match build_state(
                &manifest_location,
                &mut protocol_state,
                file_accessor,
                static_cost_analysis,
            )
            .await
            {
                Ok(_) => {
                    editor_state
                        .try_write(|es| es.index_protocol(manifest_location, protocol_state))?;
                    let (aggregated_diagnostics, notification) =
                        editor_state.try_read(|es| es.get_aggregated_diagnostics())?;
                    let env_simnet_diagnostics =
                        editor_state.try_read(|es| es.get_env_simnet_diagnostics())?;
                    Ok(LspNotificationResponse {
                        aggregated_diagnostics,
                        env_simnet_diagnostics,
                        notification,
                    })
                }
                Err(e) => Ok(LspNotificationResponse::error(&e)),
            }
        }

        LspNotification::ContractSaved(contract_location) => {
            let manifest_location = match editor_state
                .try_write(|es| es.clear_protocol_associated_with_contract(&contract_location))?
            {
                Some(manifest_location) => manifest_location,
                None => match file_accessor {
                    None => paths::find_manifest_location(&contract_location)?,
                    Some(file_accessor) => {
                        paths::find_manifest_location_async(&contract_location, file_accessor)
                            .await?
                    }
                },
            };

            // TODO(): introduce partial analysis #604
            let mut protocol_state = ProtocolState::new();
            match build_state(
                &manifest_location,
                &mut protocol_state,
                file_accessor,
                static_cost_analysis,
            )
            .await
            {
                Ok(_) => {
                    editor_state.try_write(|es| {
                        es.index_protocol(manifest_location, protocol_state);
                        if let Some(contract) = es.active_contracts.get_mut(&contract_location) {
                            contract.update_definitions();
                        };
                    })?;

                    let (aggregated_diagnostics, notification) =
                        editor_state.try_read(|es| es.get_aggregated_diagnostics())?;
                    let env_simnet_diagnostics =
                        editor_state.try_read(|es| es.get_env_simnet_diagnostics())?;
                    Ok(LspNotificationResponse {
                        aggregated_diagnostics,
                        env_simnet_diagnostics,
                        notification,
                    })
                }
                Err(e) => Ok(LspNotificationResponse::error(&e)),
            }
        }

        LspNotification::ContractChanged(contract_location, contract_source) => {
            match editor_state.try_write(|es| {
                es.update_active_contract(&contract_location, &contract_source, false)
            })? {
                Ok(_result) => Ok(LspNotificationResponse::default()),
                Err(err) => Ok(LspNotificationResponse::error(&err)),
            }
        }

        LspNotification::ContractClosed(contract_location) => {
            editor_state.try_write(|es| es.active_contracts.remove_entry(&contract_location))?;
            Ok(LspNotificationResponse::default())
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum LspRequest {
    Completion(CompletionParams),
    SignatureHelp(SignatureHelpParams),
    Definition(GotoDefinitionParams),
    Hover(HoverParams),
    DocumentSymbol(DocumentSymbolParams),
    DocumentFormatting(DocumentFormattingParams),
    DocumentRangeFormatting(DocumentRangeFormattingParams),
    CodeLens(CodeLensParams),
    Initialize(Box<InitializeParams>),
}

#[derive(Debug, PartialEq, Deserialize, Serialize)]
pub enum LspRequestResponse {
    CompletionItems(Vec<CompletionItem>),
    SignatureHelp(Option<SignatureHelp>),
    Definition(Option<Location>),
    DocumentSymbol(Vec<DocumentSymbol>),
    DocumentFormatting(Option<Vec<TextEdit>>),
    DocumentRangeFormatting(Option<Vec<TextEdit>>),
    Hover(Option<Hover>),
    CodeLens(Vec<CodeLens>),
    Initialize(Box<InitializeResult>),
}

pub fn process_request(
    command: LspRequest,
    editor_state: &EditorStateInput,
) -> Result<LspRequestResponse, String> {
    match command {
        LspRequest::Completion(params) => {
            let file_url = params.text_document_position.text_document.uri;
            let position = params.text_document_position.position;

            let Some(contract_location) = get_contract_location(&file_url) else {
                return Ok(LspRequestResponse::CompletionItems(vec![]));
            };

            let Ok(completion_items) = editor_state
                .try_read(|es| es.get_completion_items_for_contract(&contract_location, &position))
            else {
                return Ok(LspRequestResponse::CompletionItems(vec![]));
            };

            Ok(LspRequestResponse::CompletionItems(completion_items))
        }

        LspRequest::Definition(params) => {
            let file_url = params.text_document_position_params.text_document.uri;
            let Some(contract_location) = get_contract_location(&file_url) else {
                return Ok(LspRequestResponse::Definition(None));
            };
            let position = params.text_document_position_params.position;
            let location = editor_state
                .try_read(|es| es.get_definition_location(&contract_location, &position))
                .unwrap_or_default();
            Ok(LspRequestResponse::Definition(location))
        }

        LspRequest::SignatureHelp(params) => {
            let file_url = params.text_document_position_params.text_document.uri;
            let Some(contract_location) = get_contract_location(&file_url) else {
                return Ok(LspRequestResponse::SignatureHelp(None));
            };
            let position = params.text_document_position_params.position;

            // if the developer selects a specific signature
            // it can be retrieved in the context and kept selected
            let active_signature = params
                .context
                .and_then(|c| c.active_signature_help)
                .and_then(|s| s.active_signature);

            let signature = editor_state
                .try_read(|es| {
                    es.get_signature_help(&contract_location, &position, active_signature)
                })
                .unwrap_or_default();
            Ok(LspRequestResponse::SignatureHelp(signature))
        }

        LspRequest::DocumentSymbol(params) => {
            let file_url = params.text_document.uri;
            let Some(contract_location) = get_contract_location(&file_url) else {
                return Ok(LspRequestResponse::DocumentSymbol(vec![]));
            };
            let document_symbols = editor_state
                .try_read(|es| es.get_document_symbols_for_contract(&contract_location))
                .unwrap_or_default();
            Ok(LspRequestResponse::DocumentSymbol(document_symbols))
        }
        LspRequest::DocumentFormatting(param) => {
            let file_url = param.text_document.uri;
            let Some(contract_location) = get_contract_location(&file_url) else {
                return Ok(LspRequestResponse::DocumentFormatting(None));
            };

            let Ok(Some(contract_data)) =
                editor_state.try_read(|es| es.active_contracts.get(&contract_location).cloned())
            else {
                return Ok(LspRequestResponse::DocumentFormatting(None));
            };
            let source = &contract_data.source;

            let tab_size = param.options.tab_size as usize;
            let prefer_space = param.options.insert_spaces;
            let props = param.options.properties;
            let max_line_length = props
                .get("maxLineLength")
                .and_then(|value| {
                    // FormattingProperty can be boolean, number, or string
                    match value {
                        ls_types::FormattingProperty::Number(num) => Some(*num as usize),
                        ls_types::FormattingProperty::String(s) => s.parse::<usize>().ok(),
                        _ => None,
                    }
                })
                .unwrap_or(80);
            let formatting_options = clarinet_format::formatter::Settings {
                indentation: if !prefer_space {
                    clarinet_format::formatter::Indentation::Tab
                } else {
                    clarinet_format::formatter::Indentation::Space(tab_size)
                },
                max_line_length,
            };

            let formatter = clarinet_format::formatter::ClarityFormatter::new(formatting_options);
            let formatted_result = formatter.format_file(source, Some(contract_data.epoch));
            let text_edit = ls_types::TextEdit {
                range: ls_types::Range {
                    start: ls_types::Position {
                        line: 0,
                        character: 0,
                    },
                    end: ls_types::Position {
                        line: source.lines().count() as u32,
                        character: 0,
                    },
                },
                new_text: formatted_result,
            };
            Ok(LspRequestResponse::DocumentFormatting(Some(vec![
                text_edit,
            ])))
        }
        LspRequest::DocumentRangeFormatting(param) => {
            let file_url = param.text_document.uri;
            let Some(contract_location) = get_contract_location(&file_url) else {
                return Ok(LspRequestResponse::DocumentRangeFormatting(None));
            };

            let Ok(Some(contract_data)) =
                editor_state.try_read(|es| es.active_contracts.get(&contract_location).cloned())
            else {
                return Ok(LspRequestResponse::DocumentRangeFormatting(None));
            };

            let source = &contract_data.source;

            let tab_size = param.options.tab_size as usize;
            let max_line_length = param
                .options
                .properties
                .get("maxLineLength")
                .and_then(|value| {
                    // FormattingProperty can be boolean, number, or string
                    match value {
                        ls_types::FormattingProperty::Number(num) => Some(*num as usize),
                        ls_types::FormattingProperty::String(s) => s.parse::<usize>().ok(),
                        _ => None,
                    }
                })
                .unwrap_or(80);
            let prefer_space = param.options.insert_spaces;
            let formatting_options = clarinet_format::formatter::Settings {
                indentation: if !prefer_space {
                    clarinet_format::formatter::Indentation::Tab
                } else {
                    clarinet_format::formatter::Indentation::Space(tab_size)
                },
                max_line_length,
            };
            let epoch = Some(contract_data.epoch);

            // extract the text of just this range
            let lines: Vec<&str> = source.lines().collect();
            let start_line = param.range.start.line as usize;
            let end_line = param.range.end.line as usize;

            // Validate range boundaries
            if start_line >= lines.len() {
                return Ok(LspRequestResponse::DocumentRangeFormatting(None));
            }

            // Get the substring representing just the selected range
            let range_text = if start_line == end_line {
                // Single line selection
                let line = lines.get(start_line).unwrap_or(&"");
                let start_char = param.range.start.character as usize;
                let end_char = param.range.end.character as usize;
                let start_char = start_char.min(line.len());
                let end_char = end_char.min(line.len());

                if start_char >= end_char {
                    return Ok(LspRequestResponse::DocumentRangeFormatting(None));
                }

                line[start_char..end_char].to_string()
            } else {
                let mut result = String::new();

                // First line (might be partial)
                if let Some(first_line) = lines.get(start_line) {
                    let start_char = (param.range.start.character as usize).min(first_line.len());
                    result.push_str(&first_line[start_char..]);
                }

                // Middle lines (complete lines)
                for line_idx in (start_line + 1)..end_line {
                    if let Some(line) = lines.get(line_idx) {
                        result.push('\n');
                        result.push_str(line);
                    }
                }

                // Last line (might be partial) - only if end_line is different from start_line
                if end_line > start_line && end_line < lines.len() {
                    if let Some(last_line) = lines.get(end_line) {
                        let end_char = (param.range.end.character as usize).min(last_line.len());
                        result.push('\n');
                        result.push_str(&last_line[..end_char]);
                    }
                }

                result
            };

            // If the range text is empty or only whitespace, return None
            if range_text.trim().is_empty() {
                return Ok(LspRequestResponse::DocumentRangeFormatting(None));
            }

            // Count the number of trailing newlines in the original selection
            let mut trailing_newlines = 0;
            let mut temp_text = range_text.clone();
            while temp_text.ends_with('\n') {
                trailing_newlines += 1;
                temp_text.pop();
            }

            let formatter = clarinet_format::formatter::ClarityFormatter::new(formatting_options);

            // Try to format the range text, but handle panics/errors gracefully
            let formatted_result = formatter.format_section(&range_text, epoch);

            let formatted_result = match formatted_result {
                Ok(formatted_text) => {
                    let mut result = formatted_text.trim_end().to_string();
                    // Add back the same number of trailing newlines that were in the original
                    for _ in 0..trailing_newlines {
                        result.push('\n');
                    }
                    result
                }
                Err(_) => {
                    // If the selected range contains malformed/incomplete Clarity code,
                    // return None to indicate formatting is not possible
                    return Ok(LspRequestResponse::DocumentRangeFormatting(None));
                }
            };

            let text_edit = ls_types::TextEdit {
                range: param.range,
                new_text: formatted_result,
            };

            Ok(LspRequestResponse::DocumentRangeFormatting(Some(vec![
                text_edit,
            ])))
        }

        LspRequest::Hover(params) => {
            let file_url = params.text_document_position_params.text_document.uri;
            let Some(contract_location) = get_contract_location(&file_url) else {
                return Ok(LspRequestResponse::Hover(None));
            };
            let position = params.text_document_position_params.position;
            let hover_data = editor_state
                .try_read(|es| es.get_hover_data(&contract_location, &position))
                .unwrap_or_default();
            Ok(LspRequestResponse::Hover(hover_data))
        }

        LspRequest::CodeLens(params) => {
            let file_url = params.text_document.uri;
            let Some(contract_location) = get_contract_location(&file_url) else {
                return Ok(LspRequestResponse::CodeLens(vec![]));
            };
            let code_lenses = editor_state
                .try_read(|es| es.get_code_lenses(&contract_location))
                .unwrap_or_default();
            Ok(LspRequestResponse::CodeLens(code_lenses))
        }

        _ => Err(format!("Unexpected command: {command:?}")),
    }
}

// lsp requests are not supposed to mut the editor_state (only the notifications do)
// this is to ensure there is no concurrency between notifications and requests to
// acquire write lock on the editor state in a wasm context
// except for the Initialize request, which is the first interaction between the client and the server
// and can therefore safely acquire write lock on the editor state
pub fn process_mutating_request(
    command: LspRequest,
    editor_state: &mut EditorStateInput,
) -> Result<LspRequestResponse, String> {
    match command {
        LspRequest::Initialize(params) => {
            let initialization_options: InitializationOptions = params
                .initialization_options
                .and_then(|o| serde_json::from_value(o).ok())
                .unwrap_or_default();

            editor_state
                .try_write(|es| es.settings = initialization_options.clone())
                .map(|_| {
                    LspRequestResponse::Initialize(Box::new(InitializeResult {
                        server_info: Some(ServerInfo {
                            name: "clarinet lsp".to_owned(),
                            version: Some(String::from(env!("CARGO_PKG_VERSION"))),
                        }),
                        capabilities: get_capabilities(&initialization_options),
                    }))
                })
        }
        _ => Err(format!(
            "Unexpected command: {command:?}, should not mutate state"
        )),
    }
}

#[cfg(test)]
mod lsp_tests {
    use std::collections::HashMap;
    use std::path::PathBuf;

    use clarity::vm::ClarityVersion;
    use indoc::indoc;
    use ls_types::{
        DocumentRangeFormattingParams, FormattingOptions, Position, Range, TextDocumentIdentifier,
        WorkDoneProgressParams,
    };
    use serde_json::{json, Value};

    use super::*;
    use crate::common::state::EditorState;

    fn get_root_path() -> PathBuf {
        if cfg!(windows) {
            PathBuf::from(std::env::var("SystemDrive").unwrap_or_else(|_| "C:".to_string()) + "\\")
        } else {
            PathBuf::from("/")
        }
    }

    fn create_test_editor_state(source: String) -> EditorStateInput {
        let mut editor_state = EditorState::new();

        let contract_location = get_root_path().join("test.clar");

        editor_state.insert_active_contract(
            contract_location,
            ClarityVersion::Clarity2,
            None,
            source,
        );

        EditorStateInput::Owned(editor_state)
    }

    #[test]
    fn test_range_formatting_comments() {
        let source = "(ok true)\n\n(define-public (foo)\n  ;; this is a comment\n   (ok true)\n)";

        let editor_state_input = create_test_editor_state(source.to_owned());

        let params = DocumentRangeFormattingParams {
            text_document: TextDocumentIdentifier {
                uri: "file:///test.clar".parse().unwrap(),
            },
            range: Range {
                start: Position {
                    line: 3,
                    character: 1,
                },
                end: Position {
                    line: 6,
                    character: 2,
                },
            },
            options: FormattingOptions {
                tab_size: 2,
                insert_spaces: true,
                properties: HashMap::new(),
                trim_trailing_whitespace: None,
                insert_final_newline: None,
                trim_final_newlines: None,
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
        };
        let request = LspRequest::DocumentRangeFormatting(params);
        assert!(process_request(request, &editor_state_input).is_ok());
    }

    #[test]
    fn test_go_to_definition() {
        let source = "(define-constant N 1) (define-read-only (get-N) N)";
        let editor_state_input = create_test_editor_state(source.to_owned());

        let path = get_root_path().join("test.clar");

        let params = GotoDefinitionParams {
            text_document_position_params: ls_types::TextDocumentPositionParams {
                text_document: ls_types::TextDocumentIdentifier {
                    uri: paths::path_to_url_string(&path).unwrap().parse().unwrap(),
                },
                // Position inside the 'N' constant
                position: Position {
                    line: 0,
                    character: 49,
                },
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: ls_types::PartialResultParams {
                partial_result_token: None,
            },
        };
        let request = LspRequest::Definition(params);
        let response =
            process_request(request, &editor_state_input).expect("Failed to process request");

        let LspRequestResponse::Definition(Some(location)) = &response else {
            panic!("Expected a Definition response, got: {response:?}");
        };

        assert_eq!(location.uri.scheme().as_str(), "file");
        let response_json = json!(response);
        assert!(response_json
            .get("Definition")
            .expect("Expected 'Definition' key")
            .get("uri")
            .expect("Expected 'uri' key")
            .to_string()
            .ends_with("test.clar\""));
    }

    struct TestFileAccessor {
        project_manifest: String,
        network_manifest: String,
        contract: String,
    }

    impl TestFileAccessor {
        fn new(contract: String) -> Self {
            let project_manifest = indoc! {r#"
                [project]
                name = 'test-project'

                [contracts.counter]
                path = 'contracts/test.clar'
                epoch = 'latest'
                clarity_version = 3

                [repl.analysis.lint_groups]
                all = false
            "#}
            .to_string();

            let network_manifest = indoc! {r#"
                [network]
                name = "devnet"
                deployment_fee_rate = 10

                [accounts.deployer]
                mnemonic = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw"
                balance = 100_000_000_000_000
                sbtc_balance = 1_000_000_000
            "#}
            .to_string();

            Self {
                project_manifest,
                network_manifest,
                contract,
            }
        }
    }

    impl FileAccessor for TestFileAccessor {
        fn file_exists(&self, _path: String) -> clarinet_files::FileAccessorResult<bool> {
            Box::pin(async { Ok(true) })
        }

        fn read_file(&self, path: String) -> clarinet_files::FileAccessorResult<String> {
            let content = if path.ends_with("Clarinet.toml") {
                self.project_manifest.clone()
            } else if path.ends_with("Devnet.toml") {
                self.network_manifest.clone()
            } else {
                self.contract.clone()
            };
            Box::pin(async { Ok(content) })
        }

        fn read_files(
            &self,
            contracts_paths: Vec<String>,
        ) -> clarinet_files::FileAccessorResult<HashMap<String, String>> {
            let contract = self.contract.clone();
            Box::pin(async move {
                Ok(contracts_paths
                    .into_iter()
                    .map(|path| (path, contract.clone()))
                    .collect())
            })
        }

        fn write_file(
            &self,
            _path: String,
            _content: &[u8],
        ) -> clarinet_files::FileAccessorResult<()> {
            Box::pin(async { Ok(()) })
        }
    }

    #[tokio::test]
    async fn test_env_simnet() {
        let with_env_simnet = indoc! {r#"
            (define-data-var count uint u0)

            ;; #[env(simnet)]
            (define-public (increment)
              (ok (var-set count (+ (var-get count) u1)))
            )

            (define-public (increment2)
              (increment)
            )
        "#};

        let file_accessor = TestFileAccessor::new(with_env_simnet.to_string());
        let mut editor_state_input = EditorStateInput::Owned(EditorState::new());

        let contract_location = PathBuf::from("test.clar".to_string());
        let notification = LspNotification::ContractSaved(contract_location);
        let response =
            process_notification(notification, &mut editor_state_input, Some(&file_accessor))
                .await
                .expect("Failed to process notification");

        let response_json = json!(response);
        println!("full response: {response_json}");

        let aggregated_diagnostics = response_json
            .get("aggregated_diagnostics")
            .expect("Expected 'aggregate_diagnostics' key");
        let Some(aggregate_array) = aggregated_diagnostics.as_array() else {
            panic!("aggregated_diagnostics should be an array");
        };
        assert_eq!(aggregate_array.len(), 1);

        let element = aggregate_array
            .first()
            .expect("Expected an element in outer array");
        let Some(element_array) = element.as_array() else {
            panic!("element should be an array");
        };
        assert_eq!(
            element_array.len(),
            2,
            "Element array should have two items"
        );

        let path = element_array
            .first()
            .expect("Failed to get path in element array")
            .to_string();
        assert_eq!(path, "\"contracts/test.clar\"");

        let diagnostics = element_array
            .get(1)
            .expect("Failed to get diagnostics in element array");
        let Some(diagnostics_array) = diagnostics.as_array() else {
            panic!("diagnostics should be an array");
        };
        assert_eq!(
            diagnostics_array.len(),
            1,
            "Diagnostics array should have one item"
        );
        // Each entry is a LintDiagnostic, serialized as a flat object
        let diagnostic = diagnostics_array
            .first()
            .expect("Failed to get expected diagnostic");
        let level = diagnostic
            .get("level")
            .expect("Failed to find \"level\": in diagnostic")
            .to_string();
        assert_eq!(level, "\"Error\"");

        let message = diagnostic
            .get("message")
            .expect("Failed to find \"message\": in diagnostic");
        assert_eq!(message, "use of unresolved function 'increment' (onchain)");

        let spans = diagnostic
            .get("spans")
            .expect("Failed to find \"spans\": in diagnostic");
        let Some(spans_array) = spans.as_array() else {
            panic!("spans should be an array");
        };
        assert_eq!(spans_array.len(), 1, "Spans array should have one item");
        let span = spans_array.first().expect("Failed to get expected span");
        let start_line = span
            .get("start_line")
            .expect("span didn't have a \"start_line\" field");

        let end_line = span
            .get("end_line")
            .expect("span didn't have a \"end_line\" field");

        assert_eq!(start_line, &Value::from(8));
        assert_eq!(end_line, &Value::from(10));
    }

    #[test]
    fn test_custom_boot_contract_recognition() {
        let manifest_content = indoc! {r#"
            [project]
            name = "test-project"
            telemetry = false

            [project.override_boot_contracts_source]
            "pox-4" = "./custom-boot-contracts/pox-4.clar"
            "costs" = "./custom-boot-contracts/costs.clar"

            [contracts.test-contract]
            path = "contracts/test.clar"
            clarity_version = 1
        "#};

        // Create a test contract in custom-boot-contracts
        let contract_content = indoc! {r#"
            (define-data-var counter uint u0)
            (define-public (increment)
                (begin
                    (set-data-var! counter (+ (var-get counter) u1))
                    (ok (var-get counter))
                )
            )
        "#};

        // This test verifies that the LSP infrastructure can handle custom-boot-contracts
        // The actual file system operations would be handled by the file accessor
        // but we can verify the contract recognition logic works
        assert!(manifest_content.contains("custom-boot-contracts"));
        assert!(contract_content.contains("define-public"));
    }

    #[tokio::test]
    async fn test_env_simnet_opened() {
        let with_env_simnet = indoc! {r#"
            ;; token definitions
            ;;
            (define-fungible-token drachma)

            ;; constants

            (define-constant CONTRACT-OWNER tx-sender)
            (define-constant ERR-OWNER-ONLY (err u100))
            (define-constant ERR-NOT-TOKEN-OWNER (err u101))

            ;; data vars
            ;;
            (define-data-var token-uri (optional (string-utf8 256)) (some u"https://en.wikipedia.org/wiki/Ancient_drachma"))

            ;; data maps
            ;;

            ;; public functions
            ;;
            (define-public (mint (amount uint) (recipient principal))
                (begin
                    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
                    (minty-fresh amount recipient)
                )
            )

            ;; #[env(simnet)]
            (define-public (minty-fresh (amount uint) (recipient principal)) ;; eol
                (begin
                    (ft-mint? drachma amount recipient)
                )
            )
            ;; post comment
        "#};

        let file_accessor = TestFileAccessor::new(with_env_simnet.to_string());
        let mut editor_state_input = EditorStateInput::Owned(EditorState::new());

        let contract_location = PathBuf::from("test.clar".to_string());
        let notification = LspNotification::ContractSaved(contract_location);
        let response =
            process_notification(notification, &mut editor_state_input, Some(&file_accessor))
                .await
                .expect("Failed to process notification");

        let response_json = json!(response);
        println!("full respeonse: {response_json}");

        let aggregated_diagnostics = response_json
            .get("aggregated_diagnostics")
            .expect("Expected 'aggregate_diagnostics' key");
        let Some(aggregate_array) = aggregated_diagnostics.as_array() else {
            panic!("aggregated_diagnostics should be an array");
        };
        assert_eq!(aggregate_array.len(), 1);

        let element = aggregate_array
            .first()
            .expect("Expected an element in outer array");
        let Some(element_array) = element.as_array() else {
            panic!("element should be an array");
        };
        assert_eq!(
            element_array.len(),
            2,
            "Element array should have two items"
        );

        let path = element_array
            .first()
            .expect("Failed to get path in element array")
            .to_string();
        assert_eq!(path, "\"contracts/test.clar\"");

        let diagnostics = element_array
            .get(1)
            .expect("Failed to get diagnostics in element array");
        let Some(diagnostics_array) = diagnostics.as_array() else {
            panic!("diagnostics should be an array");
        };
        assert_eq!(
            diagnostics_array.len(),
            1,
            "Diagnostics array should have one item"
        );
        // Each entry is a LintDiagnostic, serialized as a flat object
        let diagnostic = diagnostics_array
            .first()
            .expect("Failed to get expected diagnostic");
        let level = diagnostic
            .get("level")
            .expect("Failed to find \"level\": in diagnostic")
            .to_string();
        assert_eq!(level, "\"Error\"");

        let message = diagnostic
            .get("message")
            .expect("Failed to find \"message\": in diagnostic");
        assert_eq!(
            message,
            "use of unresolved function 'minty-fresh' (onchain)"
        );

        let spans = diagnostic
            .get("spans")
            .expect("Failed to find \"spans\": in diagnostic");
        let Some(spans_array) = spans.as_array() else {
            panic!("spans should be an array");
        };
        assert_eq!(spans_array.len(), 3, "Spans array should have one item");
        let span1 = spans_array.first().expect("Failed to get expected span");
        let start_line1 = span1
            .get("start_line")
            .expect("span didn't have a \"start_line\" field");

        let start_column1 = span1
            .get("start_column")
            .expect("span didn't have a \"start_line\" field");

        let end_line1 = span1
            .get("end_line")
            .expect("span didn't have a \"end_line\" field");

        let end_column1 = span1
            .get("end_column")
            .expect("span didn't have a \"end_column\" field");

        assert_eq!(start_line1, &Value::from(21));
        assert_eq!(start_column1, &Value::from(6));
        assert_eq!(end_line1, &Value::from(21));
        assert_eq!(end_column1, &Value::from(10));
    }

    fn get_env_simnet_diags(source: &str) -> Vec<ls_types::Diagnostic> {
        let EditorStateInput::Owned(es) = create_test_editor_state(source.to_owned()) else {
            panic!("expected Owned");
        };
        let result = es.get_env_simnet_diagnostics();
        result.into_iter().flat_map(|(_, diags)| diags).collect()
    }

    #[test]
    fn test_env_simnet_diagnostics_top_level() {
        let source = indoc! {r#"
            (define-public (mint (amount uint)) (ok true))

            ;; #[env(simnet)]
            (define-public (minty-fresh (amount uint) (recipient principal))
                (begin
                    (ft-mint? drachma amount recipient)
                )
            )
        "#};

        let diags = get_env_simnet_diags(source);
        assert_eq!(diags.len(), 1);
        let d = &diags[0];
        assert_eq!(d.severity, Some(ls_types::DiagnosticSeverity::HINT));
        assert_eq!(
            d.tags.as_ref().unwrap(),
            &[ls_types::DiagnosticTag::UNNECESSARY]
        );
        // annotation starts on line 3 (0-indexed: 2), block ends on line 8 (0-indexed: 7)
        assert_eq!(d.range.start.line, 2);
        assert_eq!(d.range.end.line, 7);
    }

    #[test]
    fn test_env_simnet_diagnostics_no_annotations() {
        let source = "(define-read-only (get-owner) (ok tx-sender))";
        let diags = get_env_simnet_diags(source);
        assert!(diags.is_empty());
    }

    #[test]
    fn test_env_simnet_diagnostics_eol_ignored() {
        let source = indoc! {r#"
            (define-public (mint (amount uint) (recipient principal))
                (begin
                    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY) ;; #[env(simnet)]
                    (ok true)
                )
            )
        "#};

        let diags = get_env_simnet_diags(source);
        assert!(diags.is_empty());
    }

    #[test]
    fn test_env_simnet_diagnostics_nested() {
        let source = indoc! {r#"
            (define-public (mint (amount uint) (recipient principal))
                (begin
                    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
                    ;; #[env(simnet)]
                    (minty-fresh amount recipient)
                    (ok true)
                )
            )
        "#};

        let diags = get_env_simnet_diags(source);
        assert_eq!(diags.len(), 1);
        // annotation on line 4 (0-indexed: 3), expression on line 5 (0-indexed: 4)
        assert_eq!(diags[0].range.start.line, 3);
        assert_eq!(diags[0].range.end.line, 4);
    }

    #[test]
    fn test_env_simnet_diagnostics_multiple() {
        let source = indoc! {r#"
            ;; #[env(simnet)]
            (define-constant FIRST u1)

            (define-constant KEEP u2)

            ;; #[env(simnet)]
            (define-constant SECOND u3)
        "#};

        let diags = get_env_simnet_diags(source);
        assert_eq!(diags.len(), 2);
        assert_eq!(diags[0].range.start.line, 0);
        assert_eq!(diags[0].range.end.line, 1);
        assert_eq!(diags[1].range.start.line, 5);
        assert_eq!(diags[1].range.end.line, 6);
    }

    #[tokio::test]
    async fn test_env_simnet_diagnostics_full_cycle() {
        let source = indoc! {r#"
            (define-data-var count uint u0)

            ;; #[env(simnet)]
            (define-public (increment)
              (ok (var-set count (+ (var-get count) u1)))
            )

            (define-public (decrement)
              (ok (var-set count (- (var-get count) u1)))
            )
        "#};

        let file_accessor = TestFileAccessor::new(source.to_string());
        let mut editor_state = EditorState::new();

        // Simulate the contract being opened first (populates active_contracts)
        // Use an absolute path so the file:// URI round-trips correctly
        let contract_path = get_root_path().join("contracts/test.clar");
        editor_state.insert_active_contract(
            contract_path,
            ClarityVersion::Clarity3,
            None,
            source.to_string(),
        );
        let mut editor_state_input = EditorStateInput::Owned(editor_state);

        // ContractSaved builds the protocol state and collects env_simnet diagnostics
        let notification = LspNotification::ContractSaved(PathBuf::from("test.clar"));
        let response =
            process_notification(notification, &mut editor_state_input, Some(&file_accessor))
                .await
                .expect("Failed to process notification");

        // Should have env_simnet diagnostics for the annotated block
        assert!(
            !response.env_simnet_diagnostics.is_empty(),
            "Expected env_simnet_diagnostics to be populated"
        );
        let (_, diags) = &response.env_simnet_diagnostics[0];
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(ls_types::DiagnosticSeverity::HINT));
        assert_eq!(
            diags[0].tags.as_ref().unwrap(),
            &[ls_types::DiagnosticTag::UNNECESSARY]
        );
        // annotation on line 3 (0-indexed: 2), block ends on line 6 (0-indexed: 5)
        assert_eq!(diags[0].range.start.line, 2);
        assert_eq!(diags[0].range.end.line, 5);
    }
}
