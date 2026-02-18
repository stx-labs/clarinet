use std::collections::HashMap;
use std::fs;
use std::path::Path;

use clarinet_format::formatter::{ClarityFormatter, Indentation, Settings};
use clarity::types::StacksEpochId;
use clarity::vm::ast::stack_depth_checker::StackDepthLimits;

/// This is strictly for reading top metadata from golden tests
fn from_metadata(metadata: &str) -> Settings {
    let mut max_line_length = 80;
    let mut indent = Indentation::Space(2);

    let metadata_map: HashMap<&str, &str> = metadata
        .split(',')
        .map(|pair| pair.trim())
        .filter_map(|kv| kv.split_once(':'))
        .map(|(k, v)| (k.trim(), v.trim()))
        .collect();

    if let Some(length) = metadata_map.get("max_line_length") {
        max_line_length = length.parse().unwrap_or(max_line_length);
    }

    if let Some(&indentation) = metadata_map.get("indentation") {
        indent = match indentation {
            "tab" => Indentation::Tab,
            value => {
                if let Ok(spaces) = value.parse::<usize>() {
                    Indentation::Space(spaces)
                } else {
                    Indentation::Space(2) // Fallback to default
                }
            }
        };
    }

    Settings {
        max_line_length,
        indentation: indent,
    }
}
fn format_file_with_metadata(source: &str) -> String {
    let mut lines = source.lines();
    let metadata_line = lines.next().unwrap_or_default();
    let settings = from_metadata(metadata_line);

    let real_source = lines.collect::<Vec<&str>>().join("\n");
    let formatter = ClarityFormatter::new(settings);
    formatter.format_file(&real_source, None)
}
#[test]
fn test_irl_contracts() {
    let golden_dir = "./tests/golden";
    let intended_dir = "./tests/golden-intended";

    // Iterate over files in the golden directory
    for entry in fs::read_dir(golden_dir).expect("Failed to read golden directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.is_file() {
            let src = fs::read_to_string(&path).expect("Failed to read source file");

            let file_name = path.file_name().expect("Failed to get file name");
            println!("file: {file_name:?}");
            let intended_path = Path::new(intended_dir).join(file_name);

            let intended =
                fs::read_to_string(&intended_path).expect("Failed to read intended file");

            // Apply formatting and compare
            let result = format_file_with_metadata(&src);
            pretty_assertions::assert_eq!(result, intended, "Mismatch in file: {:?}", file_name);
            // parse resulting contract
            let (_statements, diagnostics, success) =
                clarity::vm::ast::parser::v2::parse_collect_diagnostics(
                    &result,
                    StackDepthLimits::for_epoch(StacksEpochId::latest()),
                );

            if !diagnostics.is_empty() {
                println!("Result of re-parsing file: {}", file_name.to_str().unwrap());
                println!("Message: {:?}", diagnostics[0].message);
            }
            assert!(success);
        }
    }
}
