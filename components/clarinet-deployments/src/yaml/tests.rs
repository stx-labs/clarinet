use std::collections::BTreeMap;

use serde::Deserialize;
use serde_json::{json, Value};

use super::from_yaml;

/// YAML 1.1 boolean words remain strings under our default policy, as on main.
#[test]
fn legacy_boolean_words_are_strings() {
    let actual: Value = from_yaml(b"[yes, NO, y, n, on, Off]").unwrap();
    assert_eq!(actual, json!(["yes", "NO", "y", "n", "on", "Off"]));
}

/// Strict booleans also reject legacy words in typed fields, matching main.
#[test]
fn legacy_boolean_words_are_not_typed_booleans() {
    for input in ["yes", "NO", "y", "n", "on", "Off"] {
        assert!(from_yaml::<bool>(input.as_bytes()).is_err(), "{input}");
    }
}

/// Core uses 0o for octal and permits exponents without a decimal point, as on main.
/// These forms differ from YAML 1.1's implicit integer and float resolution.
#[test]
fn core_numeric_extensions() {
    assert_eq!(
        from_yaml::<Value>(b"[0o10, 1e2]").unwrap(),
        json!([8, 100.0])
    );
}

/// Core leaves legacy sexagesimal numbers and timestamps as strings, matching main.
#[test]
fn legacy_numbers_and_dates_are_strings() {
    assert_eq!(
        from_yaml::<Value>(b"[1:20, 2001-12-15]").unwrap(),
        json!(["1:20", "2001-12-15"])
    );
}

/// Rust string fields retain plain scalar text, matching main's application-level conversion policy.
#[test]
fn typed_strings_preserve_text() {
    for input in ["123", "true", "010", "0x10", "3.0"] {
        assert_eq!(from_yaml::<String>(input.as_bytes()).unwrap(), input);
    }
}

/// String-keyed Rust maps preserve numeric-looking key text, matching main's conversion policy.
#[test]
fn typed_string_mapping_keys() {
    let actual: BTreeMap<String, String> = from_yaml(b"010: value").unwrap();
    assert_eq!(actual.get("010").map(String::as_str), Some("value"));
}

/// An alias resolves to its anchor's value without changing scalar types.
#[test]
fn aliases_preserve_values() {
    let actual: Value = from_yaml(b"[&word yes, *word]").unwrap();
    assert_eq!(actual, json!(["yes", "yes"]));
}

/// Merge keys remain ordinary keys, matching main and the YAML 1.2 Core schema.
#[test]
fn merge_keys_are_ordinary() {
    let actual: Value = from_yaml(b"base: &base {value: 1}\ncopy: {<<: *base}").unwrap();
    assert_eq!(actual["copy"], json!({"<<": {"value": 1}}));
}

/// Binary-tagged strings keep their encoded text, matching main's compatibility policy.
/// This is an application policy, not a YAML 1.2 Core requirement for the non-Core binary tag.
#[test]
fn binary_strings_keep_encoded_text() {
    let actual: Value = from_yaml(b"!!binary aGVsbG8=").unwrap();
    assert_eq!(actual, json!("aGVsbG8="));
    assert_eq!(
        from_yaml::<String>(b"!!binary aGVsbG8=").unwrap(),
        "aGVsbG8="
    );
}

/// Internally tagged enums buffer values without turning legacy boolean words into booleans.
#[test]
fn internally_tagged_enum_strings() {
    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(tag = "kind")]
    enum Item {
        Text { value: String },
    }
    let actual: Item = from_yaml(b"kind: Text\nvalue: on").unwrap();
    assert_eq!(actual, Item::Text { value: "on".into() });
}

/// Untagged enums preserve legacy boolean words while trying their candidate variants.
#[test]
fn untagged_enum_strings() {
    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(untagged)]
    enum Item {
        Boolean(bool),
        Text(String),
    }
    assert_eq!(from_yaml::<Item>(b"yes").unwrap(), Item::Text("yes".into()));
}

/// Flattened fields preserve legacy boolean words through Serde's intermediate buffer.
#[test]
fn flattened_strings() {
    #[derive(Debug, Deserialize)]
    struct Item {
        #[serde(flatten)]
        fields: BTreeMap<String, String>,
    }
    let actual: Item = from_yaml(b"value: off").unwrap();
    assert_eq!(actual.fields.get("value").map(String::as_str), Some("off"));
}

/// An explicit 1.2 directive preserves the default handling of legacy boolean words.
#[test]
fn explicit_yaml_12_directive() {
    assert_eq!(
        from_yaml::<Value>(b"%YAML 1.2\n---\nyes").unwrap(),
        json!("yes")
    );
}

/// Directive-like text inside a literal block is content and does not affect other values.
#[test]
fn directive_text_in_scalar_content() {
    let actual: Value = from_yaml(b"text: |\n  %YAML 1.1\nvalue: yes").unwrap();
    assert_eq!(actual, json!({"text": "%YAML 1.1\n", "value": "yes"}));
}
