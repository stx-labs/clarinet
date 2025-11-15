use clarinet_files::FileLocation;
use clarity_repl::clarity::vm::diagnostic::{
    Diagnostic as ClarityDiagnostic, Level as ClarityLevel,
};
use ls_types::{Diagnostic as LspDiagnostic, DiagnosticSeverity, Position, Range, Uri};

#[allow(unused_macros)]
#[cfg(target_arch = "wasm32")]
macro_rules! log {
    ( $( $t:tt )* ) => {
        web_sys::console::log_1(&format!( $( $t )* ).into());
    }
}

#[cfg(target_arch = "wasm32")]
pub(crate) use log;

/// Logs for both browser and native depending on context
#[macro_export]
macro_rules! lsp_log {
    ( $( $t:tt )* ) => {
        #[cfg(target_arch = "wasm32")]
        {
            web_sys::console::log_1(&format!( $( $t )* ).into());
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            eprintln!( $( $t )* );
        }
    };
}

pub fn clarity_diagnostics_to_lsp_type(diagnostics: &Vec<ClarityDiagnostic>) -> Vec<LspDiagnostic> {
    let mut dst = vec![];
    for d in diagnostics {
        dst.push(clarity_diagnostic_to_lsp_type(d));
    }
    dst
}

pub fn clarity_diagnostic_to_lsp_type(diagnostic: &ClarityDiagnostic) -> LspDiagnostic {
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
    LspDiagnostic {
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

pub fn get_manifest_location(text_document_uri: &Uri) -> Option<FileLocation> {
    let file_location = text_document_uri.to_string();
    if !file_location.ends_with("Clarinet.toml") {
        return None;
    }
    FileLocation::try_parse(&file_location, None)
}

pub fn get_contract_location(text_document_uri: &Uri) -> Option<FileLocation> {
    let file_location = text_document_uri.to_string();
    if !file_location.ends_with(".clar") {
        return None;
    }
    FileLocation::try_parse(&file_location, None)
}
