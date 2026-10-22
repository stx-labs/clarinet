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

/// All six YAML 1.2 Core boolean spellings resolve to booleans.
#[test]
fn core_boolean_spellings() {
    let actual: Value = from_yaml(b"[true, True, TRUE, false, False, FALSE]").unwrap();
    assert_eq!(actual, json!([true, true, true, false, false, false]));
}

/// YAML 1.2 Core null spellings and empty mapping values resolve to null.
#[test]
fn core_null_spellings() {
    let actual: Value = from_yaml(b"[null, Null, NULL, ~]").unwrap();
    assert_eq!(actual, json!([null, null, null, null]));
    assert_eq!(
        from_yaml::<Value>(b"value:").unwrap(),
        json!({"value": null})
    );
}

/// Quotes prevent boolean, null, and numeric inference, preserving the literal text.
#[test]
fn quoted_scalars_stay_strings() {
    let actual: Value = from_yaml(b"['true', 'null', '123', '0x01', 'yes', '3.0']").unwrap();
    assert_eq!(actual, json!(["true", "null", "123", "0x01", "yes", "3.0"]));
}

/// Explicit string tags preserve text even when plain scalar inference would choose another type.
#[test]
fn explicit_string_tags() {
    let actual: Value = from_yaml(b"[!!str true, !!str null, !!str 010]").unwrap();
    assert_eq!(actual, json!(["true", "null", "010"]));
}

/// Decimal integers, lowercase radix prefixes, and decimal floats follow Core resolution.
#[test]
fn core_numeric_forms() {
    let actual: Value = from_yaml(b"[12, -12, 0o10, 0x10, 3.5, 1e2]").unwrap();
    assert_eq!(actual, json!([12, -12, 8, 16, 3.5, 100.0]));
}

/// The largest deployment amount retains every bit when passing through an untyped buffer.
#[test]
fn unsigned_integer_precision() {
    let actual: Value = from_yaml(b"18446744073709551615").unwrap();
    assert_eq!(actual, json!(u64::MAX));
    assert_eq!(from_yaml::<u64>(b"18446744073709551615").unwrap(), u64::MAX);
}

/// Negative and overflowing integers cannot be loaded into an unsigned 64-bit field.
#[test]
fn unsigned_integer_bounds() {
    assert!(from_yaml::<u64>(b"-1").is_err());
    assert!(from_yaml::<u64>(b"18446744073709551616").is_err());
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

/// Unicode and literal-block line breaks survive parsing, including the strip chomping indicator.
#[test]
fn literal_block_text() {
    let actual: String = from_yaml("|-\n  日本語\n  second line\n".as_bytes()).unwrap();
    assert_eq!(actual, "日本語\nsecond line");
}

/// An alias resolves to its anchor's value without changing scalar types.
#[test]
fn aliases_preserve_values() {
    let actual: Value = from_yaml(b"[&word yes, *word]").unwrap();
    assert_eq!(actual, json!(["yes", "yes"]));
}

/// References to missing anchors are invalid YAML.
#[test]
fn undefined_alias_is_rejected() {
    assert!(from_yaml::<Value>(b"*missing").is_err());
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

/// Custom tags can be captured without changing the contained mapping's scalar values.
#[test]
fn custom_mapping_tags() {
    let serde_saphyr::Tagged(value, tag): serde_saphyr::Tagged<Value> =
        from_yaml(b"!item {value: yes}").unwrap();
    assert_eq!(tag.as_deref(), Some("!item"));
    assert_eq!(value, json!({"value": "yes"}));
}

/// A standard mapping tag leaves the mapping's contents intact.
#[test]
fn standard_mapping_tag() {
    assert_eq!(
        from_yaml::<Value>(b"!!map {value: yes}").unwrap(),
        json!({"value": "yes"})
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

/// Quoting or an explicit string tag must override the requested legacy inference.
/// This already works on main and does not depend on version-based schema selection.
#[test]
fn explicit_yaml_11_strings_remain_strings() {
    let actual: Value = from_yaml(b"%YAML 1.1\n---\n['yes', !!str 010]").unwrap();
    assert_eq!(actual, json!(["yes", "010"]));
}
