use clarinet_files::FileLocation;
use clarity_repl::clarity::vm::diagnostic::{
    Diagnostic as ClarityDiagnostic, Level as ClarityLevel,
};
use lsp_types::{Diagnostic as LspDiagnostic, DiagnosticSeverity, Position, Range, Uri};

#[allow(unused_macros)]
#[cfg(target_arch = "wasm32")]
macro_rules! log {
    ( $( $t:tt )* ) => {
        web_sys::console::log_1(&format!( $( $t )* ).into());
    }
}

#[cfg(target_arch = "wasm32")]
pub(crate) use log;

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
    
    // Enhance error message with helpful hints for common errors
    let message = enhance_error_message(&diagnostic.message);
    
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
        message,
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Enhance error messages with helpful hints for common issues
fn enhance_error_message(original_message: &str) -> String {
    // Check for "use of unresolved" errors which typically indicate missing contracts or traits
    if original_message.contains("use of unresolved contract") {
        format!(
            "{}\n\nHint: Make sure the contract is:\n  • Listed in your Clarinet.toml\n  • Located in the contracts/ directory\n  • Deployed before this contract (check deployment order)",
            original_message
        )
    } else if original_message.contains("use of unresolved trait") {
        format!(
            "{}\n\nHint: Make sure the trait is:\n  • Defined in an accessible contract\n  • Properly imported with 'use-trait'\n  • The contract defining it is deployed first",
            original_message
        )
    } else if original_message.contains("contract not found") || original_message.contains("NoSuchContract") {
        format!(
            "{}\n\nHint: Verify that:\n  • The contract is registered in Clarinet.toml\n  • The contract file exists in contracts/\n  • The contract name matches exactly (case-sensitive)",
            original_message
        )
    } else if original_message.contains("use of unresolved function") {
        format!(
            "{}\n\nHint: Check if:\n  • The function is defined in this contract\n  • You're using the correct function name (Clarity is case-sensitive)\n  • The function is public (use define-public) if calling from another contract",
            original_message
        )
    } else {
        original_message.to_string()
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
