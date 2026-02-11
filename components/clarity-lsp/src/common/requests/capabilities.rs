use ls_types::{
    CompletionOptions, HoverProviderCapability, ServerCapabilities, SignatureHelpOptions,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase", default)]
pub struct InitializationOptions {
    completion: bool,
    pub completion_smart_parenthesis_wrap: bool,
    pub completion_include_native_placeholders: bool,
    document_symbols: bool,
    formatting: bool,
    go_to_definition: bool,
    hover: bool,
    signature_help: bool,
    pub static_cost_analysis: bool,
}

impl Default for InitializationOptions {
    fn default() -> Self {
        InitializationOptions {
            completion: true,
            completion_smart_parenthesis_wrap: true,
            completion_include_native_placeholders: true,
            document_symbols: false,
            formatting: true,
            go_to_definition: true,
            hover: true,
            signature_help: true,
            static_cost_analysis: true,
        }
    }
}

pub fn get_capabilities(initialization_options: &InitializationOptions) -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                will_save: Some(false),
                will_save_wait_until: Some(false),
                save: Some(TextDocumentSyncSaveOptions::Supported(true)),
            },
        )),
        completion_provider: match initialization_options.completion {
            true => Some(CompletionOptions::default()),
            false => None,
        },
        hover_provider: match initialization_options.hover {
            true => Some(HoverProviderCapability::Simple(true)),
            false => None,
        },
        document_symbol_provider: match initialization_options.document_symbols {
            true => Some(ls_types::OneOf::Left(true)),
            false => None,
        },
        document_formatting_provider: match initialization_options.formatting {
            true => Some(ls_types::OneOf::Left(true)),
            false => None,
        },
        document_range_formatting_provider: match initialization_options.formatting {
            true => Some(ls_types::OneOf::Left(true)),
            false => None,
        },
        definition_provider: match initialization_options.go_to_definition {
            true => Some(ls_types::OneOf::Left(true)),
            false => None,
        },
        signature_help_provider: match initialization_options.signature_help {
            true => Some(SignatureHelpOptions {
                trigger_characters: Some(vec![" ".to_string()]),
                retrigger_characters: None,
                work_done_progress_options: Default::default(),
            }),
            false => None,
        },
        code_lens_provider: match initialization_options.static_cost_analysis {
            true => Some(ls_types::CodeLensOptions {
                resolve_provider: Some(false),
            }),
            false => None,
        },
        ..ServerCapabilities::default()
    }
}
