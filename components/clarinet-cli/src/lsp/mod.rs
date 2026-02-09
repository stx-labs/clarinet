mod native_bridge;

use std::sync::mpsc;

use clarity_lsp::utils;
use clarity_repl::clarity::vm::diagnostic::{
    Diagnostic as ClarityDiagnostic, Level as ClarityLevel,
};
use crossbeam_channel::unbounded;
use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, Position, Range};
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
    // TODO(lgalabru): add hint for contracts not found errors
    Diagnostic {
        range,
        severity: match diagnostic.level {
            ClarityLevel::Error => Some(DiagnosticSeverity::ERROR),
            ClarityLevel::Warning => Some(DiagnosticSeverity::WARNING),
            ClarityLevel::Note => Some(DiagnosticSeverity::INFORMATION),
        },
        code: None,
        code_description: None,
        source: Some("clarity".to_string()),
        message: diagnostic.message.clone(),
        related_information: None,
        tags: None,
        data: None,
    }
}

#[test]
fn test_opening_counter_contract_should_return_fresh_analysis() {
    use std::sync::mpsc::channel;

    use clarinet_files::FileLocation;
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
        FileLocation::from_path(counter_path)
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

    use clarinet_files::FileLocation;
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
        FileLocation::from_path(manifest_path)
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

    use clarinet_files::FileLocation;
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

    let _ = notification_tx.send(LspNotification::ManifestOpened(FileLocation::from_path(
        manifest_location,
    )));
    let response = response_rx.recv().expect("Unable to get response");
    let LspResponse::Notification(response) = response else {
        panic!("Unable to get response")
    };

    assert_eq!(response.aggregated_diagnostics.len(), 1);
    let (_, diags_0) = &response.aggregated_diagnostics[0];
    // the counter project should emit 4 warnings and 4 notes coming from counter.clar
    assert_eq!(diags_0.len(), 8);
}
