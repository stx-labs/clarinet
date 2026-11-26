use std::collections::BTreeMap;
use std::sync::LazyLock;

use clarity::vm::docs::{
    make_api_reference, make_define_reference, make_keyword_reference, FunctionAPI,
};
use clarity::vm::functions::define::DefineFunctions;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::variables::NativeVariables;

fn normalize_description(description: &str) -> String {
    description.replace('\n', " ")
}

fn format_api_doc(function_api: &FunctionAPI) -> (String, String) {
    let description = normalize_description(&function_api.description);
    let doc = format!(
        "Usage\n{}\n\nDescription\n{}\n\nExamples\n{}",
        function_api.signature, description, function_api.example
    );
    (function_api.name.clone(), doc)
}

fn build_api_reference() -> BTreeMap<String, String> {
    NativeFunctions::ALL
        .iter()
        .map(|func| format_api_doc(&make_api_reference(func)))
        .chain(
            DefineFunctions::ALL
                .iter()
                .map(|func| format_api_doc(&make_define_reference(func))),
        )
        .collect()
}

fn clarity_keywords() -> BTreeMap<String, String> {
    NativeVariables::ALL
        .iter()
        .filter_map(make_keyword_reference)
        .map(|key| {
            let description = normalize_description(key.description);
            let doc = format!("Description\n{}\n\nExamples\n{}", description, key.example);
            (key.name.to_string(), doc)
        })
        .collect()
}

static CLARITY_STD_REF: LazyLock<BTreeMap<String, String>> = LazyLock::new(build_api_reference);

static CLARITY_KEYWORDS_REF: LazyLock<BTreeMap<String, String>> = LazyLock::new(clarity_keywords);

pub fn functions_index() -> impl Iterator<Item = &'static str> {
    CLARITY_STD_REF.keys().map(String::as_str)
}

pub fn keywords_index() -> impl Iterator<Item = &'static str> {
    CLARITY_KEYWORDS_REF.keys().map(String::as_str)
}

/// Operators are keyed with an alias, like `+ (add)`, so they also match on their symbol alone.
pub fn describe(name: &str) -> Option<&'static str> {
    CLARITY_STD_REF
        .get(name)
        .or_else(|| CLARITY_KEYWORDS_REF.get(name))
        .or_else(|| {
            CLARITY_STD_REF
                .iter()
                .find(|(key, _)| {
                    key.split_once(' ')
                        .is_some_and(|(symbol, _)| symbol == name)
                })
                .map(|(_, doc)| doc)
        })
        .map(String::as_str)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn describe_finds_functions_and_keywords() {
        assert!(describe("map").unwrap().starts_with("Usage\n(map "));
        assert!(describe("tx-sender").unwrap().starts_with("Description\n"));
        assert_eq!(describe("not-a-clarity-name"), None);
    }

    #[test]
    fn describe_finds_operators_by_symbol() {
        assert_eq!(describe("+"), describe("+ (add)"));
        assert!(describe("<").unwrap().starts_with("Usage\n(< "));
        assert!(describe("<=").unwrap().starts_with("Usage\n(<= "));
    }

    #[test]
    fn indexes_are_sorted_and_describable() {
        assert!(functions_index().is_sorted());
        assert!(keywords_index().is_sorted());
        assert!(functions_index()
            .chain(keywords_index())
            .all(|name| describe(name).is_some()));
    }
}
