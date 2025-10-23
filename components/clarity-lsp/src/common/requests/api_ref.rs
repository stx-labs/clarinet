use std::collections::HashMap;
use std::sync::LazyLock;

use clarity_repl::clarity::docs::{
    make_api_reference, make_define_reference, make_keyword_reference, FunctionAPI,
};
use clarity_repl::clarity::functions::define::DefineFunctions;
use clarity_repl::clarity::functions::NativeFunctions;
use clarity_repl::clarity::variables::NativeVariables;
use clarity_repl::clarity::ClarityVersion;

fn code(code: &str) -> String {
    ["```clarity", code.trim(), "```"].join("\n")
}

fn format_removed_in(max_version: Option<ClarityVersion>) -> String {
    max_version
        .map(|max_version| {
            format!(
                "Removed in **{}**",
                match max_version {
                    ClarityVersion::Clarity1 => ClarityVersion::Clarity2,
                    ClarityVersion::Clarity2 => ClarityVersion::Clarity3,
                    ClarityVersion::Clarity3 => ClarityVersion::latest(),
                    ClarityVersion::Clarity4 => ClarityVersion::Clarity4,
                }
            )
        })
        .unwrap_or_default()
}

pub static API_REF: LazyLock<HashMap<String, (String, Option<FunctionAPI>)>> =
    LazyLock::new(|| {
        let mut api_references: HashMap<String, (String, Option<FunctionAPI>)> = HashMap::new();
        let separator = "- - -";

        for define_function in DefineFunctions::ALL {
            let reference = make_define_reference(define_function);
            api_references.insert(
                define_function.to_string(),
                (
                    Vec::from([
                        &code(&reference.signature),
                        separator,
                        &reference.description,
                        separator,
                        "**Example**",
                        &code(&reference.example),
                    ])
                    .join("\n"),
                    Some(reference),
                ),
            );
        }

        for native_function in NativeFunctions::ALL {
            let reference = make_api_reference(native_function);
            api_references.insert(
                native_function.to_string(),
                (
                    Vec::from([
                        &code(
                            format!("{} -> {}", &reference.signature, &reference.output_type)
                                .as_str(),
                        ),
                        separator,
                        &reference.description,
                        separator,
                        &format!("Introduced in **{}**  ", &reference.min_version),
                        &format_removed_in(reference.max_version),
                        separator,
                        "**Example**",
                        &code(&reference.example),
                    ])
                    .join("\n"),
                    Some(reference),
                ),
            );
        }

        for native_keyword in NativeVariables::ALL {
            let reference = make_keyword_reference(native_keyword).unwrap();
            api_references.insert(
                native_keyword.to_string(),
                (
                    Vec::from([
                        reference.description,
                        separator,
                        &format!("Introduced in **{}**  ", &reference.min_version),
                        &format_removed_in(reference.max_version),
                        separator,
                        "**Example**",
                        &code(reference.example),
                    ])
                    .join("\n"),
                    None,
                ),
            );
        }

        api_references
    });
