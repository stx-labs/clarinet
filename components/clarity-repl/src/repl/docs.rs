use std::sync::LazyLock;

use clarity::vm::docs::{
    make_api_reference, make_define_reference, make_keyword_reference, FunctionAPI,
};
use clarity::vm::functions::define::DefineFunctions;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::variables::NativeVariables;
use hashbrown::HashMap;

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

fn build_api_reference() -> HashMap<String, String> {
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

fn clarity_keywords() -> HashMap<String, String> {
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

pub static CLARITY_STD_REF: LazyLock<HashMap<String, String>> = LazyLock::new(build_api_reference);

pub static CLARITY_STD_INDEX: LazyLock<Vec<String>> = LazyLock::new(|| {
    let mut keys = CLARITY_STD_REF
        .keys()
        .map(String::from)
        .collect::<Vec<String>>();
    keys.sort();
    keys
});

pub static CLARITY_KEYWORDS_REF: LazyLock<HashMap<String, String>> =
    LazyLock::new(clarity_keywords);

pub static CLARITY_KEYWORDS_INDEX: LazyLock<Vec<String>> = LazyLock::new(|| {
    let mut keys = CLARITY_KEYWORDS_REF
        .keys()
        .map(String::from)
        .collect::<Vec<String>>();
    keys.sort();
    keys
});
