use std::collections::HashMap;
use std::sync::LazyLock;
use std::vec;

use clarity::vm::types::{BlockInfoProperty, FunctionType, TypeSignatureExt};
use clarity_repl::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use clarity_repl::clarity::analysis::ContractAnalysis;
use clarity_repl::clarity::docs::{
    make_api_reference, make_define_reference, make_keyword_reference,
};
use clarity_repl::clarity::functions::define::DefineFunctions;
use clarity_repl::clarity::functions::NativeFunctions;
use clarity_repl::clarity::variables::NativeVariables;
use clarity_repl::clarity::{ClarityName, ClarityVersion, SymbolicExpression};
use clarity_repl::repl::DEFAULT_EPOCH;
use clarity_types::types::TypeSignature;
use lsp_types::{
    CompletionItem, CompletionItemKind, Documentation, InsertTextFormat, MarkupContent, MarkupKind,
    Position,
};
use regex::Regex;

use super::helpers::{get_function_at_position, is_position_within_span};

static COMPLETION_ITEMS_CLARITY_1: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_default_native_keywords_list(ClarityVersion::Clarity1));
static COMPLETION_ITEMS_CLARITY_2: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_default_native_keywords_list(ClarityVersion::Clarity2));
static COMPLETION_ITEMS_CLARITY_3: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_default_native_keywords_list(ClarityVersion::Clarity3));
static COMPLETION_ITEMS_CLARITY_4: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_default_native_keywords_list(ClarityVersion::Clarity4));
static VAR_FUNCTIONS: LazyLock<Vec<String>> = LazyLock::new(|| {
    vec![
        NativeFunctions::SetVar.to_string(),
        NativeFunctions::FetchVar.to_string(),
    ]
});
static MAP_FUNCTIONS: LazyLock<Vec<String>> = LazyLock::new(|| {
    vec![
        NativeFunctions::InsertEntry.to_string(),
        NativeFunctions::FetchEntry.to_string(),
        NativeFunctions::SetEntry.to_string(),
        NativeFunctions::DeleteEntry.to_string(),
    ]
});
static FT_FUNCTIONS: LazyLock<Vec<String>> = LazyLock::new(|| {
    vec![
        NativeFunctions::GetTokenBalance.to_string(),
        NativeFunctions::GetTokenSupply.to_string(),
        NativeFunctions::BurnToken.to_string(),
        NativeFunctions::MintToken.to_string(),
        NativeFunctions::TransferToken.to_string(),
    ]
});
static NFT_FUNCTIONS: LazyLock<Vec<String>> = LazyLock::new(|| {
    vec![
        NativeFunctions::GetAssetOwner.to_string(),
        NativeFunctions::BurnAsset.to_string(),
        NativeFunctions::MintAsset.to_string(),
        NativeFunctions::TransferAsset.to_string(),
    ]
});
static ITERATOR_FUNCTIONS: LazyLock<Vec<String>> = LazyLock::new(|| {
    vec![
        NativeFunctions::Map.to_string(),
        NativeFunctions::Filter.to_string(),
        NativeFunctions::Fold.to_string(),
    ]
});
static VALID_MAP_FUNCTIONS_CLARITY_1: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_map_valid_cb_completion_items(ClarityVersion::Clarity1));
static VALID_MAP_FUNCTIONS_CLARITY_2: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_map_valid_cb_completion_items(ClarityVersion::Clarity2));
static VALID_MAP_FUNCTIONS_CLARITY_3: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_map_valid_cb_completion_items(ClarityVersion::Clarity3));
static VALID_MAP_FUNCTIONS_CLARITY_4: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_map_valid_cb_completion_items(ClarityVersion::Clarity4));

static VALID_FILTER_FUNCTIONS_CLARITY_1: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_filter_valid_cb_completion_items(ClarityVersion::Clarity1));
static VALID_FILTER_FUNCTIONS_CLARITY_2: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_filter_valid_cb_completion_items(ClarityVersion::Clarity2));
static VALID_FILTER_FUNCTIONS_CLARITY_3: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_filter_valid_cb_completion_items(ClarityVersion::Clarity3));
static VALID_FILTER_FUNCTIONS_CLARITY_4: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_filter_valid_cb_completion_items(ClarityVersion::Clarity4));

static VALID_FOLD_FUNCTIONS_CLARITY_1: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_fold_valid_cb_completion_items(ClarityVersion::Clarity1));
static VALID_FOLD_FUNCTIONS_CLARITY_2: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_fold_valid_cb_completion_items(ClarityVersion::Clarity2));
static VALID_FOLD_FUNCTIONS_CLARITY_3: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_fold_valid_cb_completion_items(ClarityVersion::Clarity3));
static VALID_FOLD_FUNCTIONS_CLARITY_4: LazyLock<Vec<CompletionItem>> =
    LazyLock::new(|| build_fold_valid_cb_completion_items(ClarityVersion::Clarity4));

#[derive(Clone, Debug, Default)]
pub struct ContractDefinedData {
    position: Position,
    consts: Vec<(String, String)>,
    locals: Vec<(String, String)>,
    pub vars: Vec<String>,
    pub maps: Vec<String>,
    pub fts: Vec<String>,
    pub nfts: Vec<String>,
    pub functions_completion_items: Vec<CompletionItem>,
}

impl ContractDefinedData {
    pub fn new(expressions: &[SymbolicExpression], position: &Position) -> Self {
        let mut defined_data = ContractDefinedData {
            position: *position,
            ..Default::default()
        };
        traverse(&mut defined_data, expressions);
        defined_data
    }

    // this methods is in charge of:
    // 1. set the function completion item with its arguments
    // 2. set the local binding names if the position is within this function
    fn set_function_completion_with_bindings(
        &mut self,
        expr: &SymbolicExpression,
        name: &ClarityName,
        parameters: &[TypedVar<'_>],
    ) {
        let mut completion_args: Vec<String> = vec![];
        for (i, typed_var) in parameters.iter().enumerate() {
            if let Ok(signature) =
                TypeSignature::parse_type_repr(DEFAULT_EPOCH, typed_var.type_expr, &mut ())
            {
                completion_args.push(format!("${{{}:{}:{}}}", i + 1, typed_var.name, signature));

                if is_position_within_span(&self.position, &expr.span, 0) {
                    self.locals
                        .push((typed_var.name.to_string(), signature.to_string()));
                }
            };
        }

        let insert_text = match completion_args.len() {
            0 => Some(name.to_string()),
            _ => Some(format!("{} {}", name, completion_args.join(" "))),
        };

        self.functions_completion_items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::MODULE),
            insert_text,
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }

    pub fn populate_snippet_with_options(
        &self,
        version: &ClarityVersion,
        name: &String,
        snippet: &str,
    ) -> Option<String> {
        if VAR_FUNCTIONS.contains(name) && !self.vars.is_empty() {
            let choices = self.vars.join(",");
            return Some(snippet.replace("${1:var}", &format!("${{1|{choices}|}}")));
        }
        if MAP_FUNCTIONS.contains(name) && !self.maps.is_empty() {
            let choices = self.maps.join(",");
            return Some(snippet.replace("${1:map-name}", &format!("${{1|{choices}|}}")));
        }
        if FT_FUNCTIONS.contains(name) && !self.fts.is_empty() {
            let choices = self.fts.join(",");
            return Some(snippet.replace("${1:token-name}", &format!("${{1|{choices}|}}")));
        }
        if NFT_FUNCTIONS.contains(name) && !self.nfts.is_empty() {
            let choices = self.nfts.join(",");
            return Some(snippet.replace("${1:asset-name}", &format!("${{1|{choices}|}}")));
        }
        if ITERATOR_FUNCTIONS.contains(name) && !self.functions_completion_items.is_empty() {
            let mut choices = self
                .functions_completion_items
                .iter()
                .map(|f| f.label.to_string())
                .collect::<Vec<String>>()
                .join(",");
            choices.push(',');
            choices.push_str(
                &get_iterator_cb_completion_item(version, name)
                    .iter()
                    .map(|i| i.insert_text.clone().unwrap())
                    .collect::<Vec<String>>()
                    .join(","),
            );
            return Some(snippet.replace("${1:func}", &format!("${{1|{choices}|}}")));
        }
        None
    }

    pub fn get_contract_completion_items(&self) -> Vec<CompletionItem> {
        [&self.consts[..], &self.locals[..]]
            .concat()
            .iter()
            .map(|(name, definition)| {
                CompletionItem::new_simple(name.to_string(), definition.to_string())
            })
            .collect()
    }
}

impl<'a> ASTVisitor<'a> for ContractDefinedData {
    fn visit_define_constant(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.consts.push((name.to_string(), value.to_string()));
        true
    }

    fn visit_define_data_var(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _data_type: &'a SymbolicExpression,
        _initial: &'a SymbolicExpression,
    ) -> bool {
        self.vars.push(name.to_string());
        true
    }

    fn visit_define_map(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key_type: &'a SymbolicExpression,
        _value_type: &'a SymbolicExpression,
    ) -> bool {
        self.maps.push(name.to_string());
        true
    }

    fn visit_define_ft(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _supply: Option<&'a SymbolicExpression>,
    ) -> bool {
        self.fts.push(name.to_string());
        true
    }

    fn visit_define_nft(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _nft_type: &'a SymbolicExpression,
    ) -> bool {
        self.nfts.push(name.to_string());
        true
    }

    fn visit_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<clarity_repl::analysis::ast_visitor::TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        self.set_function_completion_with_bindings(expr, name, &parameters.unwrap_or_default());
        true
    }

    fn visit_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<clarity_repl::analysis::ast_visitor::TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        self.set_function_completion_with_bindings(expr, name, &parameters.unwrap_or_default());
        true
    }

    fn visit_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<clarity_repl::analysis::ast_visitor::TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        self.set_function_completion_with_bindings(expr, name, &parameters.unwrap_or_default());
        true
    }

    fn visit_let(
        &mut self,
        expr: &'a SymbolicExpression,
        bindings: &HashMap<&'a ClarityName, &'a SymbolicExpression>,
        _body: &'a [SymbolicExpression],
    ) -> bool {
        if is_position_within_span(&self.position, &expr.span, 0) {
            for (name, value) in bindings {
                self.locals.push((name.to_string(), value.to_string()));
            }
        }
        true
    }
}

fn build_contract_calls_args(signature: &FunctionType) -> (Vec<String>, Vec<String>) {
    let mut snippet_args = vec![];
    let mut doc_args = vec![];
    if let FunctionType::Fixed(function) = signature {
        for (i, arg) in function.args.iter().enumerate() {
            snippet_args.push(format!("${{{}:{}:{}}}", i + 1, arg.name, arg.signature));
            doc_args.push(format!("{} `{}`", arg.name, arg.signature));
        }
    }
    (snippet_args, doc_args)
}

pub fn get_contract_calls(analysis: &ContractAnalysis) -> Vec<CompletionItem> {
    let mut inter_contract = vec![];
    for (name, signature) in analysis
        .public_function_types
        .iter()
        .chain(analysis.read_only_function_types.iter())
    {
        let (snippet_args, doc_args) = build_contract_calls_args(signature);
        let label = format!(
            "contract-call? .{} {}",
            analysis.contract_identifier.name, name,
        );
        let documentation = MarkupContent {
            kind: MarkupKind::Markdown,
            value: [vec![format!("**{}**", name.to_string())], doc_args]
                .concat()
                .join("\n\n"),
        };
        let insert_text = format!(
            "contract-call? .{} {} {}",
            analysis.contract_identifier.name,
            name,
            snippet_args.join(" "),
        );

        inter_contract.push(CompletionItem {
            label,
            detail: Some(analysis.contract_identifier.name.to_string()),
            documentation: Some(Documentation::MarkupContent(documentation)),
            kind: Some(CompletionItemKind::EVENT),
            insert_text: Some(insert_text),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }
    inter_contract
}

pub fn build_completion_item_list(
    clarity_version: &ClarityVersion,
    expressions: &[SymbolicExpression],
    position: &Position,
    active_contract_defined_data: &ContractDefinedData,
    contract_calls: Vec<CompletionItem>,
    should_wrap: bool,
    include_native_placeholders: bool,
) -> Vec<CompletionItem> {
    if let Some((function_name, param)) = get_function_at_position(position, expressions) {
        // - for var-*, map-*, ft-* or nft-* methods, return the corresponding data names
        let mut completion_strings: Option<Vec<String>> = None;
        if VAR_FUNCTIONS.contains(&function_name.to_string()) && param == Some(0) {
            completion_strings = Some(active_contract_defined_data.vars.clone());
        } else if MAP_FUNCTIONS.contains(&function_name.to_string()) && param == Some(0) {
            completion_strings = Some(active_contract_defined_data.maps.clone());
        } else if FT_FUNCTIONS.contains(&function_name.to_string()) && param == Some(0) {
            completion_strings = Some(active_contract_defined_data.fts.clone());
        } else if NFT_FUNCTIONS.contains(&function_name.to_string()) && param == Some(0) {
            completion_strings = Some(active_contract_defined_data.nfts.clone());
        }

        if let Some(completion_strings) = completion_strings {
            return completion_strings
                .iter()
                .map(|s| CompletionItem::new_simple(String::from(s), String::from("")))
                .collect();
        }

        // - for iterator methods (filter, fold, map) return the list of available and valid functions
        if ITERATOR_FUNCTIONS.contains(&function_name.to_string()) && param == Some(0) {
            let mut completion_items: Vec<CompletionItem> = vec![];
            completion_items.append(
                &mut active_contract_defined_data
                    .functions_completion_items
                    .iter()
                    .map(|f| CompletionItem::new_simple(f.label.clone(), String::from("")))
                    .collect::<Vec<CompletionItem>>(),
            );
            completion_items.append(&mut get_iterator_cb_completion_item(
                clarity_version,
                &function_name.to_string(),
            ));
            return completion_items;
        }
    }

    let native_keywords = match clarity_version {
        ClarityVersion::Clarity1 => COMPLETION_ITEMS_CLARITY_1.to_vec(),
        ClarityVersion::Clarity2 => COMPLETION_ITEMS_CLARITY_2.to_vec(),
        ClarityVersion::Clarity3 => COMPLETION_ITEMS_CLARITY_3.to_vec(),
        ClarityVersion::Clarity4 => COMPLETION_ITEMS_CLARITY_4.to_vec(),
    };
    let placeholder_pattern = Regex::new(r" \$\{\d+:[\w-]+\}").unwrap();

    let mut completion_items = vec![];
    completion_items.append(&mut active_contract_defined_data.get_contract_completion_items());
    for mut item in [
        native_keywords,
        contract_calls,
        active_contract_defined_data
            .functions_completion_items
            .clone(),
    ]
    .concat()
    .drain(..)
    {
        match item.kind {
            Some(
                CompletionItemKind::EVENT
                | CompletionItemKind::FUNCTION
                | CompletionItemKind::MODULE
                | CompletionItemKind::CLASS,
            ) => {
                let mut snippet = item.insert_text.take().unwrap();
                let mut snippet_has_choices = false;
                if item.kind == Some(CompletionItemKind::FUNCTION) {
                    if let Some(populated_snippet) = active_contract_defined_data
                        .populate_snippet_with_options(clarity_version, &item.label, &snippet)
                    {
                        snippet_has_choices = true;
                        snippet = populated_snippet;
                    }
                }

                if !include_native_placeholders {
                    if snippet_has_choices {
                        // for var-*, map-*, ft-* and nft-* methods
                        // the variable name is kept but the other placeholders are removed
                        let updated_snippet =
                            placeholder_pattern.replace_all(&snippet, "").to_string();
                        if updated_snippet.ne(&snippet) {
                            snippet = updated_snippet;
                            snippet.push_str(" $0");
                        }
                    } else if item.kind == Some(CompletionItemKind::FUNCTION)
                        || item.kind == Some(CompletionItemKind::CLASS)
                    {
                        match item.label.as_str() {
                            "+ (add)"
                            | "- (subtract)"
                            | "/ (divide)"
                            | "* (multiply)"
                            | "< (less than)"
                            | "<= (less than or equal)"
                            | "> (greater than)"
                            | ">= (greater than or equal)" => {
                                snippet = item.label.split_whitespace().next().unwrap().to_string()
                            }
                            _ => snippet.clone_from(&item.label),
                        }
                        snippet.push_str(" $0");
                    }
                }

                item.insert_text = if should_wrap {
                    Some(format!("({snippet})"))
                } else {
                    Some(snippet)
                };
            }
            Some(CompletionItemKind::TYPE_PARAMETER) => {
                if should_wrap {
                    if let "tuple" | "buff" | "string-ascii" | "string-utf8" | "optional" | "list"
                    | "response" = item.label.as_str()
                    {
                        item.insert_text = Some(format!("({} $0)", item.label));
                        item.insert_text_format = Some(InsertTextFormat::SNIPPET);
                    }
                }
            }
            _ => {}
        }

        completion_items.push(item);
    }
    completion_items
}

pub fn check_if_should_wrap(source: &str, position: &Position) -> bool {
    if let Some(line) = source.lines().nth(position.line as usize) {
        if position.character as usize > line.len() {
            return false;
        }

        let mut chars = line[..position.character as usize].chars();
        while let Some(char) = chars.next_back() {
            match char {
                '(' => return false,
                char if char.is_whitespace() => return true,
                _ => {}
            }
        }
    }
    true
}

pub fn build_default_native_keywords_list(version: ClarityVersion) -> Vec<CompletionItem> {
    let clarity2_aliased_functions: Vec<NativeFunctions> = vec![
        NativeFunctions::ElementAt,
        NativeFunctions::IndexOf,
        NativeFunctions::BitwiseXor,
    ];

    let command = lsp_types::Command {
        title: "triggerParameterHints".into(),
        command: "editor.action.triggerParameterHints".into(),
        arguments: None,
    };

    let native_functions: Vec<CompletionItem> = NativeFunctions::ALL
        .iter()
        .filter_map(|func| {
            let mut api = make_api_reference(func);
            if version < api.min_version
                || version > api.max_version.unwrap_or(ClarityVersion::latest())
            {
                return None;
            }
            if clarity2_aliased_functions.contains(func) {
                if version >= ClarityVersion::Clarity2 {
                    return None;
                } else if api.min_version == ClarityVersion::Clarity1 {
                    // only for element-at? and index-of?
                    api.snippet = api.snippet.replace('?', "");
                }
            }

            Some(CompletionItem {
                label: api.name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(api.name),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: api.description,
                })),
                insert_text: Some(api.snippet),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                command: Some(command.clone()),
                ..Default::default()
            })
        })
        .collect();

    let define_functions: Vec<CompletionItem> = DefineFunctions::ALL
        .iter()
        .filter_map(|func| {
            let api = make_define_reference(func);
            if version < api.min_version
                || version > api.max_version.unwrap_or(ClarityVersion::latest())
            {
                return None;
            }
            Some(CompletionItem {
                label: api.name.to_string(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some(api.name),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: api.description,
                })),
                insert_text: Some(api.snippet),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                command: Some(command.clone()),
                ..Default::default()
            })
        })
        .collect();

    let native_variables: Vec<CompletionItem> = NativeVariables::ALL
        .iter()
        .filter_map(|var| {
            if let Some(api) = make_keyword_reference(var) {
                if version < api.min_version
                    || version > api.max_version.unwrap_or(ClarityVersion::latest())
                {
                    return None;
                }
                Some(CompletionItem {
                    label: api.name.to_string(),
                    kind: Some(CompletionItemKind::FIELD),
                    detail: Some(api.name.to_string()),
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: api.description.to_string(),
                    })),
                    insert_text: Some(api.snippet.to_string()),
                    insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                    ..Default::default()
                })
            } else {
                None
            }
        })
        .collect();

    let block_properties: Vec<CompletionItem> = BlockInfoProperty::ALL
        .iter()
        .filter_map(|var| {
            if var.get_min_version() > version {
                return None;
            }
            Some(CompletionItem {
                label: var.to_string(),
                kind: Some(CompletionItemKind::FIELD),
                insert_text: Some(var.to_string()),
                insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                ..Default::default()
            })
        })
        .collect();

    let types = [
        "uint",
        "int",
        "bool",
        "list",
        "tuple",
        "buff",
        "string-ascii",
        "string-utf8",
        "optional",
        "response",
        "principal",
    ]
    .iter()
    .map(|t| CompletionItem {
        label: t.to_string(),
        kind: Some(CompletionItemKind::TYPE_PARAMETER),
        insert_text: Some(t.to_string()),
        insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
        ..Default::default()
    })
    .collect();

    vec![
        native_functions,
        define_functions,
        native_variables,
        block_properties,
        types,
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<CompletionItem>>()
}

pub fn build_map_valid_cb_completion_items(version: ClarityVersion) -> Vec<CompletionItem> {
    [
        NativeFunctions::Add,
        NativeFunctions::Subtract,
        NativeFunctions::Multiply,
        NativeFunctions::Divide,
        NativeFunctions::CmpGeq,
        NativeFunctions::CmpLeq,
        NativeFunctions::CmpLess,
        NativeFunctions::CmpGreater,
        NativeFunctions::ToInt,
        NativeFunctions::ToUInt,
        NativeFunctions::Modulo,
        NativeFunctions::Power,
        NativeFunctions::Sqrti,
        NativeFunctions::Log2,
        NativeFunctions::BitwiseXor,
        NativeFunctions::And,
        NativeFunctions::Or,
        NativeFunctions::Not,
        NativeFunctions::BuffToIntLe,
        NativeFunctions::BuffToUIntLe,
        NativeFunctions::BuffToIntBe,
        NativeFunctions::BuffToUIntBe,
        NativeFunctions::IsStandard,
        NativeFunctions::PrincipalDestruct,
        NativeFunctions::PrincipalConstruct,
        NativeFunctions::StringToInt,
        NativeFunctions::StringToUInt,
        NativeFunctions::IntToAscii,
        NativeFunctions::IntToUtf8,
        NativeFunctions::Hash160,
        NativeFunctions::Sha256,
        NativeFunctions::Sha512,
        NativeFunctions::Sha512Trunc256,
        NativeFunctions::Keccak256,
        NativeFunctions::BitwiseAnd,
        NativeFunctions::BitwiseOr,
        NativeFunctions::BitwiseNot,
        NativeFunctions::BitwiseLShift,
        NativeFunctions::BitwiseRShift,
        NativeFunctions::BitwiseXor2,
    ]
    .iter()
    .filter_map(|func| build_iterator_cb_completion_item(func, version))
    .collect()
}

pub fn build_filter_valid_cb_completion_items(version: ClarityVersion) -> Vec<CompletionItem> {
    [
        NativeFunctions::And,
        NativeFunctions::Or,
        NativeFunctions::Not,
    ]
    .iter()
    .filter_map(|func| build_iterator_cb_completion_item(func, version))
    .collect()
}

pub fn build_fold_valid_cb_completion_items(version: ClarityVersion) -> Vec<CompletionItem> {
    [
        NativeFunctions::Add,
        NativeFunctions::Subtract,
        NativeFunctions::Multiply,
        NativeFunctions::Divide,
        NativeFunctions::CmpGeq,
        NativeFunctions::CmpLeq,
        NativeFunctions::CmpLess,
        NativeFunctions::CmpGreater,
        NativeFunctions::ToInt,
        NativeFunctions::ToUInt,
        NativeFunctions::Modulo,
        NativeFunctions::Power,
        NativeFunctions::Sqrti,
        NativeFunctions::Log2,
        NativeFunctions::BitwiseXor,
        NativeFunctions::And,
        NativeFunctions::Or,
        NativeFunctions::Not,
        NativeFunctions::IsStandard,
        NativeFunctions::BitwiseAnd,
        NativeFunctions::BitwiseOr,
        NativeFunctions::BitwiseNot,
        NativeFunctions::BitwiseLShift,
        NativeFunctions::BitwiseRShift,
        NativeFunctions::BitwiseXor2,
    ]
    .iter()
    .filter_map(|func| build_iterator_cb_completion_item(func, version))
    .collect()
}

fn build_iterator_cb_completion_item(
    func: &NativeFunctions,
    version: ClarityVersion,
) -> Option<CompletionItem> {
    let api = make_api_reference(func);
    if api.min_version > version {
        return None;
    }

    let insert_text = Some(api.snippet.split_whitespace().next().unwrap().to_string());

    Some(CompletionItem {
        label: api.name.clone(),
        kind: Some(CompletionItemKind::FUNCTION),
        detail: Some(api.name.clone()),
        documentation: Some(Documentation::MarkupContent(MarkupContent {
            kind: MarkupKind::Markdown,
            value: api.description,
        })),
        insert_text,
        insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
        ..Default::default()
    })
}

fn get_iterator_cb_completion_item(version: &ClarityVersion, func: &str) -> Vec<CompletionItem> {
    if func.to_string().eq(&NativeFunctions::Map.to_string()) {
        return match version {
            ClarityVersion::Clarity1 => VALID_MAP_FUNCTIONS_CLARITY_1.to_vec(),
            ClarityVersion::Clarity2 => VALID_MAP_FUNCTIONS_CLARITY_2.to_vec(),
            ClarityVersion::Clarity3 => VALID_MAP_FUNCTIONS_CLARITY_3.to_vec(),
            ClarityVersion::Clarity4 => VALID_MAP_FUNCTIONS_CLARITY_4.to_vec(),
        };
    }
    if func.to_string().eq(&NativeFunctions::Filter.to_string()) {
        return match version {
            ClarityVersion::Clarity1 => VALID_FILTER_FUNCTIONS_CLARITY_1.to_vec(),
            ClarityVersion::Clarity2 => VALID_FILTER_FUNCTIONS_CLARITY_2.to_vec(),
            ClarityVersion::Clarity3 => VALID_FILTER_FUNCTIONS_CLARITY_3.to_vec(),
            ClarityVersion::Clarity4 => VALID_FILTER_FUNCTIONS_CLARITY_4.to_vec(),
        };
    }
    match version {
        ClarityVersion::Clarity1 => VALID_FOLD_FUNCTIONS_CLARITY_1.to_vec(),
        ClarityVersion::Clarity2 => VALID_FOLD_FUNCTIONS_CLARITY_2.to_vec(),
        ClarityVersion::Clarity3 => VALID_FOLD_FUNCTIONS_CLARITY_3.to_vec(),
        ClarityVersion::Clarity4 => VALID_FOLD_FUNCTIONS_CLARITY_4.to_vec(),
    }
}

#[cfg(test)]
mod get_contract_global_data_tests {
    use clarity_repl::clarity::ast::build_ast;
    use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
    use clarity_repl::clarity::{ClarityVersion, StacksEpochId};
    use lsp_types::Position;

    use super::ContractDefinedData;

    fn get_defined_data(source: &str) -> ContractDefinedData {
        let contract_ast = build_ast(
            &QualifiedContractIdentifier::transient(),
            source,
            &mut (),
            ClarityVersion::Clarity2,
            StacksEpochId::Epoch21,
        )
        .unwrap();
        ContractDefinedData::new(&contract_ast.expressions, &Position::default())
    }

    #[test]
    fn get_data_vars() {
        let data = get_defined_data(
            "(define-data-var counter uint u1) (define-data-var is-active bool true)",
        );
        assert_eq!(data.vars, ["counter", "is-active"]);
    }

    #[test]
    fn get_map() {
        let data = get_defined_data("(define-map names principal { name: (buff 48) })");
        assert_eq!(data.maps, ["names"]);
    }

    #[test]
    fn get_fts() {
        let data = get_defined_data("(define-fungible-token clarity-coin)");
        assert_eq!(data.fts, ["clarity-coin"]);
    }

    #[test]
    fn get_nfts() {
        let data = get_defined_data("(define-non-fungible-token bitcoin-nft uint)");
        assert_eq!(data.nfts, ["bitcoin-nft"]);
    }
}

#[cfg(test)]
mod get_contract_local_data_tests {
    use clarity_repl::clarity::ast::build_ast;
    use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
    use clarity_repl::clarity::{ClarityVersion, StacksEpochId};
    use lsp_types::Position;

    use super::ContractDefinedData;

    fn get_defined_data(source: &str, position: &Position) -> ContractDefinedData {
        let contract_ast = build_ast(
            &QualifiedContractIdentifier::transient(),
            source,
            &mut (),
            ClarityVersion::Clarity2,
            StacksEpochId::Epoch21,
        )
        .unwrap();
        ContractDefinedData::new(&contract_ast.expressions, position)
    }

    #[test]
    fn get_function_binding() {
        let data = get_defined_data(
            "(define-private (print-arg (arg int)) )",
            &Position {
                line: 1,
                character: 38,
            },
        );
        assert_eq!(data.locals, vec![("arg".to_string(), "int".to_string())]);
        let data = get_defined_data(
            "(define-private (print-arg (arg int)) )",
            &Position {
                line: 1,
                character: 40,
            },
        );
        assert_eq!(data.locals, vec![]);
    }

    #[test]
    fn get_let_binding() {
        let data = get_defined_data(
            "(let ((n u0)) )",
            &Position {
                line: 1,
                character: 15,
            },
        );
        assert_eq!(data.locals, vec![("n".to_string(), "u0".to_string())]);
    }
}

#[cfg(test)]
mod populate_snippet_with_options_tests {
    use clarity_repl::clarity::ast::build_ast;
    use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
    use clarity_repl::clarity::{ClarityVersion, StacksEpochId};
    use lsp_types::Position;

    use super::ContractDefinedData;

    fn get_defined_data(source: &str) -> ContractDefinedData {
        let contract_ast = build_ast(
            &QualifiedContractIdentifier::transient(),
            source,
            &mut (),
            ClarityVersion::Clarity2,
            StacksEpochId::Epoch21,
        )
        .unwrap();
        ContractDefinedData::new(&contract_ast.expressions, &Position::default())
    }

    #[test]
    fn get_data_vars_snippet() {
        let data = get_defined_data(
            "(define-data-var counter uint u1) (define-data-var is-active bool true)",
        );
        let snippet = data.populate_snippet_with_options(
            &ClarityVersion::Clarity2,
            &"var-get".to_string(),
            "var-get ${1:var}",
        );
        assert_eq!(snippet, Some("var-get ${1|counter,is-active|}".to_string()));
    }

    #[test]
    fn get_map_snippet() {
        let data = get_defined_data("(define-map names principal { name: (buff 48) })");
        let snippet = data.populate_snippet_with_options(
            &ClarityVersion::Clarity2,
            &"map-get?".to_string(),
            "map-get? ${1:map-name} ${2:key-tuple}",
        );
        assert_eq!(
            snippet,
            Some("map-get? ${1|names|} ${2:key-tuple}".to_string())
        );
    }

    #[test]
    fn get_fts_snippet() {
        let data = get_defined_data("(define-fungible-token btc u21)");
        let snippet = data.populate_snippet_with_options(
            &ClarityVersion::Clarity2,
            &"ft-mint?".to_string(),
            "ft-mint? ${1:token-name} ${2:amount} ${3:recipient}",
        );
        assert_eq!(
            snippet,
            Some("ft-mint? ${1|btc|} ${2:amount} ${3:recipient}".to_string())
        );
    }

    #[test]
    fn get_nfts_snippet() {
        let data = get_defined_data("(define-non-fungible-token bitcoin-nft uint)");
        let snippet = data.populate_snippet_with_options(
            &ClarityVersion::Clarity2,
            &"nft-mint?".to_string(),
            "nft-mint? ${1:asset-name} ${2:asset-identifier} ${3:recipient}",
        );
        assert_eq!(
            snippet,
            Some("nft-mint? ${1|bitcoin-nft|} ${2:asset-identifier} ${3:recipient}".to_string())
        );
    }
}
