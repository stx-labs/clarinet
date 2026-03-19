mod native_bridge;

use std::sync::mpsc;

use clarity::vm::diagnostic::{Diagnostic as ClarityDiagnostic, Level as ClarityLevel};
use clarity_lsp::utils;
use clarity_repl::analysis::extract_lint_tag;
use crossbeam_channel::unbounded;
use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};
use tower_lsp_server::{LspService, Server};

use self::native_bridge::LspNativeBridge;

pub fn run_lsp() {
    if let Err(_e) = block_on(do_run_lsp()) {
        std::process::exit(1)
    };
}

pub fn block_on<F, R>(future: F) -> R
where
    F: std::future::Future<Output = R>,
{
    let rt = hiro_system_kit::create_basic_runtime();
    rt.block_on(future)
}

async fn do_run_lsp() -> Result<(), String> {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (notification_tx, notification_rx) = unbounded();
    let (request_tx, request_rx) = unbounded();
    let (response_tx, response_rx) = mpsc::channel();
    std::thread::spawn(move || {
        hiro_system_kit::nestable_block_on(native_bridge::start_language_server(
            notification_rx,
            request_rx,
            response_tx,
        ));
    });

    let (service, socket) = LspService::new(|client| {
        LspNativeBridge::new(client, notification_tx, request_tx, response_rx)
    });
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}

pub fn clarity_diagnostics_to_tower_lsp_type(
    diagnostics: &[ClarityDiagnostic],
) -> Vec<tower_lsp_server::ls_types::Diagnostic> {
    diagnostics
        .iter()
        .map(clarity_diagnostic_to_tower_lsp_type)
        .collect()
}

pub fn clarity_diagnostic_to_tower_lsp_type(
    diagnostic: &ClarityDiagnostic,
) -> tower_lsp_server::ls_types::Diagnostic {
    let range = match diagnostic.spans.len() {
        0 => Range::default(),
        _ => Range {
            start: Position {
                line: diagnostic.spans[0].start_line - 1,
                character: diagnostic.spans[0].start_column - 1,
            },
            end: Position {
                line: diagnostic.spans[0].end_line - 1,
                character: diagnostic.spans[0].end_column,
            },
        },
    };
    // Extract lint name from the message tag and populate the LSP `code` field
    let (lint_name, message) = extract_lint_tag(&diagnostic.message);
    let code = lint_name.map(|name| NumberOrString::String(name.to_string()));

    // TODO(lgalabru): add hint for contracts not found errors
    Diagnostic {
        range,
        severity: match diagnostic.level {
            ClarityLevel::Error => Some(DiagnosticSeverity::ERROR),
            ClarityLevel::Warning => Some(DiagnosticSeverity::WARNING),
            ClarityLevel::Note => Some(DiagnosticSeverity::INFORMATION),
        },
        code,
        code_description: None,
        source: Some("clarity".to_string()),
        message: message.to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

#[test]
fn test_opening_counter_contract_should_return_fresh_analysis() {
    use std::sync::mpsc::channel;

    use clarity_lsp::backend::{LspNotification, LspNotificationResponse};
    use crossbeam_channel::unbounded;

    use crate::lsp::native_bridge::LspResponse;

    let (notification_tx, notification_rx) = unbounded();
    let (_request_tx, request_rx) = unbounded();
    let (response_tx, response_rx) = channel();
    std::thread::spawn(move || {
        hiro_system_kit::nestable_block_on(native_bridge::start_language_server(
            notification_rx,
            request_rx,
            response_tx,
        ));
    });

    let contract_location = {
        let mut counter_path = std::env::current_dir().expect("Unable to get current dir");
        counter_path.push("examples");
        counter_path.push("counter");
        counter_path.push("contracts");
        counter_path.push("counter.clar");
        counter_path
    };

    let _ = notification_tx.send(LspNotification::ContractOpened(contract_location.clone()));
    let response = response_rx.recv().expect("Unable to get response");
    let LspResponse::Notification(response) = response else {
        panic!("Unable to get response")
    };

    // the counter project should emit 2 warnings and 2 notes coming from counter.clar
    assert_eq!(response.aggregated_diagnostics.len(), 2);
    let (_url, diags) = &response.aggregated_diagnostics[0];
    assert_eq!(diags.len(), 4);

    // re-opening this contract should not trigger a full analysis
    let _ = notification_tx.send(LspNotification::ContractOpened(contract_location));
    let response = response_rx.recv().expect("Unable to get response");
    let LspResponse::Notification(response) = response else {
        panic!("Unable to get response")
    };

    assert_eq!(response, LspNotificationResponse::default());
}

#[test]
fn test_opening_counter_manifest_should_return_fresh_analysis() {
    use std::sync::mpsc::channel;

    use clarity_lsp::backend::{LspNotification, LspNotificationResponse};
    use crossbeam_channel::unbounded;

    use crate::lsp::native_bridge::LspResponse;

    let (notification_tx, notification_rx) = unbounded();
    let (_request_tx, request_rx) = unbounded();
    let (response_tx, response_rx) = channel();
    std::thread::spawn(move || {
        hiro_system_kit::nestable_block_on(native_bridge::start_language_server(
            notification_rx,
            request_rx,
            response_tx,
        ));
    });

    let manifest_location = {
        let mut manifest_path = std::env::current_dir().expect("Unable to get current dir");
        manifest_path.push("examples");
        manifest_path.push("counter");
        manifest_path.push("Clarinet.toml");
        manifest_path
    };

    let _ = notification_tx.send(LspNotification::ManifestOpened(manifest_location.clone()));
    let response = response_rx.recv().expect("Unable to get response");
    let LspResponse::Notification(response) = response else {
        panic!("Unable to get response")
    };

    // the counter project should emit 2 warnings and 2 notes coming from counter.clar
    assert_eq!(response.aggregated_diagnostics.len(), 2);
    let (_url, diags) = &response.aggregated_diagnostics[0];
    assert_eq!(diags.len(), 4);

    // re-opening this manifest should not trigger a full analysis
    let _ = notification_tx.send(LspNotification::ManifestOpened(manifest_location));
    let response = response_rx.recv().expect("Unable to get response");
    let LspResponse::Notification(response) = response else {
        panic!("Unable to get response")
    };
    assert_eq!(response, LspNotificationResponse::default());
}

#[test]
fn test_opening_simple_nft_manifest_should_return_fresh_analysis() {
    use std::sync::mpsc::channel;

    use clarity_lsp::backend::LspNotification;
    use crossbeam_channel::unbounded;

    use crate::lsp::native_bridge::LspResponse;

    let (notification_tx, notification_rx) = unbounded();
    let (_request_tx, request_rx) = unbounded();
    let (response_tx, response_rx) = channel();
    std::thread::spawn(move || {
        hiro_system_kit::nestable_block_on(native_bridge::start_language_server(
            notification_rx,
            request_rx,
            response_tx,
        ));
    });

    let mut manifest_location = std::env::current_dir().expect("Unable to get current dir");
    manifest_location.push("examples");
    manifest_location.push("simple-nft");
    manifest_location.push("Clarinet.toml");

    let _ = notification_tx.send(LspNotification::ManifestOpened(manifest_location));
    let response = response_rx.recv().expect("Unable to get response");
    let LspResponse::Notification(response) = response else {
        panic!("Unable to get response")
    };

    assert_eq!(response.aggregated_diagnostics.len(), 1);
    let (_, diags_0) = &response.aggregated_diagnostics[0];
    // the counter project should emit 4 warnings and 4 notes coming from counter.clar
    assert_eq!(diags_0.len(), 8);
}

/// Helper: build an `initialize` JSON-RPC request.  When `code_lens_refresh`
/// is true the client capabilities advertise `workspace.codeLens.refreshSupport`.
#[cfg(test)]
fn build_initialize_request(
    id: i64,
    code_lens_refresh: bool,
) -> tower_lsp_server::jsonrpc::Request {
    let capabilities = if code_lens_refresh {
        serde_json::json!({
            "capabilities": {
                "workspace": { "codeLens": { "refreshSupport": true } }
            }
        })
    } else {
        serde_json::json!({ "capabilities": {} })
    };
    tower_lsp_server::jsonrpc::Request::build("initialize")
        .params(capabilities)
        .id(id)
        .finish()
}

/// Helper: build a `textDocument/didOpen` JSON-RPC notification for a .clar file.
#[cfg(test)]
fn build_did_open_notification(uri: &str) -> tower_lsp_server::jsonrpc::Request {
    tower_lsp_server::jsonrpc::Request::build("textDocument/didOpen")
        .params(serde_json::json!({
            "textDocument": {
                "uri": uri,
                "languageId": "clarity",
                "version": 1,
                "text": "(define-read-only (hello) (ok u1))"
            }
        }))
        .finish()
}

/// Drives the LspService: sends initialize → didOpen, then collects any
/// server-to-client requests (like `workspace/codeLens/refresh`) from the
/// socket, responding to each so the server doesn't block.
/// Returns the list of server-to-client request method names observed.
#[cfg(test)]
async fn run_did_open_and_collect_server_requests(code_lens_refresh: bool) -> Vec<String> {
    use std::sync::mpsc::channel;
    use std::time::Duration;

    use crossbeam_channel::unbounded;
    use futures::sink::SinkExt;
    use futures::stream::StreamExt;
    use tower::{Service, ServiceExt};

    let (notification_tx, notification_rx) = unbounded();
    let (request_tx, request_rx) = unbounded();
    let (response_tx, response_rx) = channel();
    std::thread::spawn(move || {
        hiro_system_kit::nestable_block_on(native_bridge::start_language_server(
            notification_rx,
            request_rx,
            response_tx,
        ));
    });

    let (mut service, socket) = LspService::new(|client| {
        native_bridge::LspNativeBridge::new(client, notification_tx, request_tx, response_rx)
    });
    let (mut request_stream, mut response_sink) = socket.split();

    // 1. Initialize
    let _init_resp = service
        .ready()
        .await
        .unwrap()
        .call(build_initialize_request(1, code_lens_refresh))
        .await;

    // 2. Build a file URI for the counter contract
    let contract_path = std::env::current_dir()
        .unwrap()
        .join("examples/counter/contracts/counter.clar");
    let uri =
        clarinet_files::paths::path_to_url_string(&contract_path).expect("failed to build URI");

    // 3. Send didOpen and concurrently handle server-to-client requests
    let did_open = build_did_open_notification(&uri);
    let did_open_fut = async {
        let _ = service.ready().await.unwrap().call(did_open).await;
    };

    let collect_requests_fut = async {
        let mut methods = vec![];
        while let Ok(Some(req)) =
            tokio::time::timeout(Duration::from_secs(10), request_stream.next()).await
        {
            let method = req.method().to_string();
            if let Some(id) = req.id().cloned() {
                let _ = response_sink
                    .send(tower_lsp_server::jsonrpc::Response::from_ok(
                        id,
                        serde_json::json!(null),
                    ))
                    .await;
            }
            methods.push(method);
        }
        methods
    };

    let (_, methods) = tokio::join!(did_open_fut, collect_requests_fut);
    methods
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_code_lens_refresh_sent_when_client_supports_it() {
    let methods = run_did_open_and_collect_server_requests(true).await;
    assert!(
        methods.iter().any(|m| m == "workspace/codeLens/refresh"),
        "expected workspace/codeLens/refresh in server requests, got: {methods:?}"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn test_code_lens_refresh_not_sent_when_client_does_not_support_it() {
    let methods = run_did_open_and_collect_server_requests(false).await;
    assert!(
        !methods.iter().any(|m| m == "workspace/codeLens/refresh"),
        "unexpected workspace/codeLens/refresh in server requests: {methods:?}"
    );
}
