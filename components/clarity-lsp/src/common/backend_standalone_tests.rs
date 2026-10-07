use std::collections::HashMap;
use std::path::PathBuf;

use clarinet_files::FileAccessor;
use ls_types::{GotoDefinitionParams, Position, TextDocumentIdentifier, WorkDoneProgressParams};

use super::*;
use crate::common::state::EditorState;

fn root_path() -> PathBuf {
    if cfg!(windows) {
        PathBuf::from(std::env::var("SystemDrive").unwrap_or_else(|_| "C:".to_string()) + "\\")
    } else {
        PathBuf::from("/")
    }
}

struct TestFileAccessor {
    contract: String,
    has_manifest: bool,
}

impl TestFileAccessor {
    fn standalone(contract: &str) -> Self {
        Self {
            contract: contract.to_string(),
            has_manifest: false,
        }
    }

    fn project(contract: &str) -> Self {
        Self {
            contract: contract.to_string(),
            has_manifest: true,
        }
    }
}

impl FileAccessor for TestFileAccessor {
    fn file_exists(&self, path: String) -> clarinet_files::FileAccessorResult<bool> {
        let exists = self.has_manifest || !path.ends_with("Clarinet.toml");
        Box::pin(async move { Ok(exists) })
    }

    fn read_file(&self, path: String) -> clarinet_files::FileAccessorResult<String> {
        let content = if path.ends_with("Clarinet.toml") {
            "[project]\nname = 'test-project'\n\n[contracts.counter]\npath = 'counter.clar'\nclarity_version = 3\nepoch = 'latest'\n".to_string()
        } else {
            self.contract.clone()
        };
        Box::pin(async move { Ok(content) })
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

    fn write_file(&self, _path: String, _content: &[u8]) -> clarinet_files::FileAccessorResult<()> {
        Box::pin(async { Ok(()) })
    }
}

#[tokio::test]
async fn test_unlisted_contract_opened_as_standalone() {
    let source = "(define-constant N u1)\n(define-read-only (get-n) N)";
    let file_accessor = TestFileAccessor::project(source);
    let mut editor_state = EditorStateInput::Owned(EditorState::new());
    let contract_location = root_path().join("standalone.clar");

    let response = process_notification(
        LspNotification::ContractOpened(contract_location.clone()),
        &mut editor_state,
        Some(&file_accessor),
    )
    .await
    .expect("opening a standalone contract should not error");
    assert!(response.notification.is_none());

    editor_state
        .try_read(|state| {
            let contract = state.active_contracts.get(&contract_location).unwrap();
            let symbols = state.get_document_symbols_for_contract(&contract_location);
            assert!(symbols.iter().any(|symbol| symbol.name == "get-n"));
            assert!(state.protocols.is_empty());
            assert_eq!(
                contract.clarity_version,
                clarinet_defaults::DEFAULT_CLARITY_VERSION
            );
            assert_eq!(contract.epoch, clarinet_defaults::DEFAULT_EPOCH);
        })
        .unwrap();
}

#[tokio::test]
async fn test_standalone_go_to_definition_without_manifest() {
    let source = "(define-constant N u1)\n(define-read-only (get-n) N)";
    let file_accessor = TestFileAccessor::standalone(source);
    let mut editor_state = EditorStateInput::Owned(EditorState::new());
    let contract_location = root_path().join("standalone.clar");

    process_notification(
        LspNotification::ContractOpened(contract_location.clone()),
        &mut editor_state,
        Some(&file_accessor),
    )
    .await
    .expect("opening a standalone contract should not error");

    let params = GotoDefinitionParams {
        text_document_position_params: ls_types::TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: paths::path_to_url_string(&contract_location)
                    .unwrap()
                    .parse()
                    .unwrap(),
            },
            position: Position {
                line: 1,
                character: 26,
            },
        },
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: ls_types::PartialResultParams {
            partial_result_token: None,
        },
    };

    let response = process_request(LspRequest::Definition(params), &editor_state)
        .expect("definition request should succeed");
    let LspRequestResponse::Definition(Some(location)) = response else {
        panic!("expected a definition location, got: {response:?}");
    };
    assert_eq!(location.range.start.line, 0);
}

#[tokio::test]
async fn test_standalone_contract_saved_without_manifest() {
    let file_accessor = TestFileAccessor::standalone("(define-data-var count uint u0)");
    let mut editor_state = EditorStateInput::Owned(EditorState::new());
    let contract_location = root_path().join("standalone.clar");

    let response = process_notification(
        LspNotification::ContractSaved(contract_location.clone()),
        &mut editor_state,
        Some(&file_accessor),
    )
    .await
    .expect("saving a standalone contract should not error");
    assert!(response.notification.is_none());
    editor_state
        .try_read(|state| assert!(state.active_contracts.contains_key(&contract_location)))
        .unwrap();
}

#[tokio::test]
async fn test_standalone_parse_error_surfaces_diagnostic() {
    let file_accessor = TestFileAccessor::standalone("(define-data-var count uint u0");
    let mut editor_state = EditorStateInput::Owned(EditorState::new());
    let contract_location = root_path().join("standalone.clar");

    let response = process_notification(
        LspNotification::ContractOpened(contract_location),
        &mut editor_state,
        Some(&file_accessor),
    )
    .await
    .expect("open should succeed even with a parse error");

    assert!(
        response
            .aggregated_diagnostics
            .iter()
            .any(|(_, diagnostics)| !diagnostics.is_empty()),
        "expected a parser diagnostic for the standalone file"
    );
}
