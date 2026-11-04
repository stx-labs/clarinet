//! Default YAML behavior; version parameters identify the expected schema for a feature.
//! They do not add directives or configure the parser. A feature matching YAML 1.1
//! does not imply that the complete default schema implements YAML 1.1.
//! YAML 1.1 types: https://yaml.org/type/
//! YAML 1.2 Core: https://yaml.org/spec/1.2.2/#1032-tag-resolution
//! Serde coercions and emitter presentation are application policies, tested separately.

use std::collections::BTreeMap;

use indoc::{formatdoc, indoc};
use serde::Deserialize;
use serde_json::{json, Value};
use test_case::test_case;

use super::from_yaml;
use crate::types::{DeploymentSpecificationFile, TransactionSpecificationFile};

#[derive(Debug, Clone, Copy)]
enum YamlVersion {
    V1_1,
    V1_2,
}

use YamlVersion::{V1_1, V1_2};

#[test_case(V1_1, "[true, True, TRUE, false, False, FALSE]", json!([true, true, true, false, false, false]); "yaml11_boolean_spellings")]
#[test_case(V1_2, "[true, True, TRUE, false, False, FALSE]", json!([true, true, true, false, false, false]); "yaml12_boolean_spellings")]
#[test_case(V1_2, "[yes, NO, y, n, on, Off]", json!(["yes", "NO", "y", "n", "on", "Off"]); "yaml12_legacy_boolean_words_are_text")]
#[test_case(V1_1, "[null, Null, NULL, ~]", json!([null, null, null, null]); "yaml11_null_spellings")]
#[test_case(V1_2, "[null, Null, NULL, ~]", json!([null, null, null, null]); "yaml12_null_spellings")]
#[test_case(V1_2, "[0, 42, +42, -42]", json!([0, 42, 42, -42]); "yaml12_decimal_integers")]
#[test_case(V1_2, "0o10", json!(8); "yaml12_octal_prefix")]
#[test_case(V1_2, "0x1f", json!(31); "yaml12_hexadecimal_prefix")]
#[test_case(V1_1, "[-0x10, +0x10]", json!([-16, 16]); "yaml11_signed_hexadecimal")]
#[test_case(V1_1, "[0b101, -0b101, +0b101]", json!([5, -5, 5]); "yaml11_binary_integers")]
#[test_case(V1_1, "1_000", json!(1000); "yaml11_decimal_digit_separators")]
#[test_case(V1_1, "!!int 1_000", json!(1000); "yaml11_explicit_integer_digit_separators")]
#[test_case(V1_2, "1e2", json!(100.0); "yaml12_exponent_without_decimal_point")]
#[test_case(V1_2, "1:20", json!("1:20"); "yaml12_sexagesimal_is_text")]
#[test_case(V1_2, "2001-12-15", json!("2001-12-15"); "yaml12_timestamp_is_text")]
#[test_case(V1_2, "!!str tRuE", json!("tRuE"); "yaml12_explicit_string_tag")]
#[test_case(V1_1, "!!bool true", json!(true); "yaml11_explicit_boolean_tag")]
#[test_case(V1_2, "!!bool true", json!(true); "yaml12_explicit_boolean_tag")]
#[test_case(V1_2, "!!int 123", json!(123); "yaml12_explicit_integer_tag")]
#[test_case(V1_2, "!!float 1.5", json!(1.5); "yaml12_explicit_float_tag")]
#[test_case(V1_2, "!!null ''", json!(null); "yaml12_explicit_empty_null_tag")]
#[test_case(V1_2, "['tRuE', 'nUlL', '0O10', '0X10', '010', '1_000', '.iNf', '+.nan']", json!(["tRuE", "nUlL", "0O10", "0X10", "010", "1_000", ".iNf", "+.nan"]); "yaml12_quotes_preserve_scalar_text")]
fn scalar_resolution(version: YamlVersion, input: &str, expected: Value) {
    assert_eq!(
        from_yaml::<Value>(input.as_bytes()).unwrap(),
        expected,
        "{version:?}: {input}"
    );
}

#[test_case(V1_1; "yaml11_non_finite_floats")]
#[test_case(V1_2; "yaml12_non_finite_floats")]
fn floating_point_special_values(version: YamlVersion) {
    for input in [".inf", ".Inf", ".INF", "+.inf"] {
        assert_eq!(
            from_yaml::<f64>(input.as_bytes()).unwrap(),
            f64::INFINITY,
            "{version:?}: {input}"
        );
    }
    assert_eq!(from_yaml::<f64>(b"-.inf").unwrap(), f64::NEG_INFINITY);
    for input in [".nan", ".NaN", ".NAN"] {
        assert!(from_yaml::<f64>(input.as_bytes()).unwrap().is_nan());
    }
}

#[test_case("yes")]
#[test_case("NO")]
#[test_case("y")]
#[test_case("n")]
#[test_case("on")]
#[test_case("Off")]
fn legacy_words_are_not_typed_booleans(input: &str) {
    assert!(from_yaml::<bool>(input.as_bytes()).is_err());
    assert!(from_yaml::<bool>(b"true").unwrap());
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
    let actual: Value = from_yaml(indoc! {b"
        base: &base {value: 1}
        copy: {<<: *base}
    "})
    .unwrap();
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
    let actual: Item = from_yaml(indoc! {b"
        kind: Text
        value: on
    "})
    .unwrap();
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

/// Directive-like text inside a literal block is content and does not affect other values.
#[test]
fn directive_text_in_scalar_content() {
    let actual: Value = from_yaml(indoc! {b"
        text: |
          %YAML 1.1
        value: yes
    "})
    .unwrap();
    assert_eq!(actual, json!({"text": "%YAML 1.1\n", "value": "yes"}));
}

#[test_case("null"; "lowercase_null")]
#[test_case("Null"; "titlecase_null")]
#[test_case("NULL"; "uppercase_null")]
#[test_case("~")]
#[test_case(""; "empty_scalar")]
fn null_requires_an_optional_string(input: &str) {
    #[derive(Debug, Deserialize)]
    struct Required {
        value: String,
    }
    #[derive(Debug, Deserialize)]
    struct Optional {
        value: Option<String>,
    }
    let source = format!("value: {input}");
    assert!(from_yaml::<Required>(source.as_bytes()).is_err());
    assert_eq!(
        from_yaml::<Optional>(source.as_bytes()).unwrap().value,
        None
    );
    let quoted = serde_json::to_string(input).unwrap();
    let quoted_source = format!("value: {quoted}");
    assert_eq!(
        from_yaml::<Required>(quoted_source.as_bytes())
            .unwrap()
            .value,
        input
    );
}

#[test_case("!!int 123")]
#[test_case("!!bool true")]
#[test_case("!!float 1.5")]
#[test_case("!!int '123'"; "quoted_integer")]
#[test_case("!!bool 'true'"; "quoted_boolean")]
#[test_case("!!float '1.5'"; "quoted_float")]
fn explicit_scalar_tags_are_not_coerced_into_strings(input: &str) {
    assert!(from_yaml::<String>(input.as_bytes()).is_err());
    let quoted = serde_json::to_string(input).unwrap();
    assert!(from_yaml::<String>(format!("!!str {quoted}").as_bytes()).is_ok());
}

#[test_case("!!null ''")]
#[test_case("!!null null")]
fn explicit_null_tags_produce_none(input: &str) {
    assert_eq!(from_yaml::<Option<String>>(input.as_bytes()).unwrap(), None);
    assert_eq!(from_yaml::<Option<String>>(b"''").unwrap(), Some("".into()));
}

#[test_case("'1'", 1; "quoted_decimal")]
#[test_case("0X1F", 31; "uppercase_hexadecimal")]
#[test_case("0O10", 8; "uppercase_octal")]
#[test_case("1_000", 1000; "decimal_digit_separators")]
fn typed_unsigned_integer_conversion(input: &str, expected: u32) {
    // Typed conversion accepts more spellings than either standard's scalar resolver.
    assert_eq!(from_yaml::<u32>(input.as_bytes()).unwrap(), expected);
}

#[test_case(V1_1, indoc! {"
    1_000: first
    1000: second
"}; "yaml11_digit_separators_do_not_distinguish_integer_keys")]
#[test_case(V1_2, indoc! {"
    0xB: first
    11: second
"}; "yaml12_radix_does_not_distinguish_integer_keys")]
fn equivalent_numeric_keys_are_duplicates(version: YamlVersion, input: &str) {
    assert!(
        from_yaml::<BTreeMap<String, String>>(input.as_bytes()).is_err(),
        "{version:?}: {input}"
    );
}

#[test_case("1.1")]
#[test_case("1.2")]
#[test_case("1.3")]
fn version_directives_do_not_select_a_different_schema(declared_version: &str) {
    let input = formatdoc! {"
        %YAML {declared_version}
        ---
        yes
    "};
    assert_eq!(from_yaml::<Value>(input.as_bytes()).unwrap(), json!("yes"));
}

#[test_case(V1_2, '\u{0085}'; "yaml12_next_line_is_content")]
#[test_case(V1_2, '\u{2028}'; "yaml12_line_separator_is_content")]
#[test_case(V1_2, '\u{2029}'; "yaml12_paragraph_separator_is_content")]
fn unicode_non_ascii_line_breaks(version: YamlVersion, character: char) {
    let input = format!("[before{character}after]");
    assert_eq!(
        from_yaml::<Value>(input.as_bytes()).unwrap(),
        json!([format!("before{character}after")]),
        "{version:?}"
    );
    let mapping = format!("name: before{character}after");
    assert_eq!(
        from_yaml::<Value>(mapping.as_bytes()).unwrap(),
        json!({"name": format!("before{character}after")}),
        "{version:?}"
    );
}

#[test_case(V1_1; "yaml11_utf8_bom")]
#[test_case(V1_2; "yaml12_utf8_bom")]
fn utf8_byte_order_mark(version: YamlVersion) {
    assert_eq!(
        from_yaml::<Value>("\u{feff}name: test".as_bytes()).unwrap(),
        json!({"name": "test"}),
        "{version:?}"
    );
}

#[test]
fn unicode_surrogate_pair_escapes_follow_json_compatibility() {
    assert_eq!(
        from_yaml::<Value>(br#"{"name": "\uD83D\uDE00"}"#).unwrap(),
        json!({"name": "😀"})
    );
    assert_eq!(from_yaml::<String>(br#""\U0001F600""#).unwrap(), "😀");
}

#[test_case(br#""\uD83D""#; "unpaired_high_surrogate")]
#[test_case(br#""\uDE00""#; "unpaired_low_surrogate")]
fn unpaired_unicode_surrogate_escapes_are_rejected(input: &[u8]) {
    assert!(from_yaml::<Value>(input).is_err());
}

#[test_case("!!str {}")]
#[test_case("!!seq {}")]
#[test_case("!!map []")]
fn explicit_tags_require_the_matching_node_kind(input: &str) {
    assert!(from_yaml::<Value>(input.as_bytes()).is_err());
    assert_eq!(from_yaml::<Value>(b"!!map {}").unwrap(), json!({}));
}

#[test_case(""; "empty_value")]
#[test_case("~"; "tilde_null")]
#[test_case("null"; "null_word")]
fn null_is_not_a_call_parameter_sequence(parameters: &str) {
    let call = |parameters: &str| {
        formatdoc! {"
            transaction-type: contract-call
            contract-id: ST000000000000000000002AMW42H.counter
            expected-sender: ST000000000000000000002AMW42H
            method: initialize
            cost: 1
            parameters: {parameters}
        "}
    };
    let input = call(parameters);
    assert!(from_yaml::<TransactionSpecificationFile>(input.as_bytes()).is_err());
    let sequence = call("[]");
    assert!(from_yaml::<TransactionSpecificationFile>(sequence.as_bytes()).is_ok());
}

#[test_case("1"; "integer_key")]
#[test_case("true"; "boolean_key")]
fn transaction_buffer_converts_scalar_keys_to_strings(key: &str) {
    let input = formatdoc! {"
        transaction-type: btc-transfer
        expected-sender: sender
        recipient: recipient
        sats-amount: 1
        sats-per-byte: 1
        {key}: ignored
    "};
    assert!(from_yaml::<TransactionSpecificationFile>(input.as_bytes()).is_ok());
}

#[test]
fn transaction_buffer_converts_integer_remap_keys_to_strings() {
    let input = indoc! {b"
        transaction-type: requirement-publish
        contract-id: id
        remap-sender: sender
        remap-principals: {1: recipient}
        cost: 1
        path: contracts/test.clar
    "};
    let TransactionSpecificationFile::RequirementPublish(spec) = from_yaml(input).unwrap() else {
        panic!("expected requirement publish");
    };
    assert_eq!(
        spec.remap_principals.unwrap().get("1").map(String::as_str),
        Some("recipient")
    );
}

#[test]
fn emitter_quotes_numeric_strings_with_double_quotes() {
    let expected = json!({"balance": "100000000000000"});
    let emitted = serde_saphyr::to_string(&expected).unwrap();
    assert_eq!(emitted, "balance: \"100000000000000\"\n");
    assert_eq!(from_yaml::<Value>(emitted.as_bytes()).unwrap(), expected);
}

#[test]
fn emitter_folds_long_strings_without_changing_their_value() {
    let expected = json!({"name": "Simulated deployment, used as a default for `clarinet console`, `clarinet test` and `clarinet check`"});
    let emitted = serde_saphyr::to_string(&expected).unwrap();
    assert!(emitted.contains(">-\n"), "{emitted}");
    assert_eq!(from_yaml::<Value>(emitted.as_bytes()).unwrap(), expected);
}

#[test]
fn duplicate_key_diagnostics_include_source_location() {
    let error = from_yaml::<Value>(indoc! {b"
        name: first
        name: second
    "})
    .unwrap_err()
    .to_string();
    assert!(error.contains("duplicate mapping key"), "{error}");
    assert!(error.contains("DuplicateKeyPolicy"), "{error}");
    assert!(error.contains("line 2"), "{error}");
}

#[test]
fn unsigned_integer_diagnostics_describe_the_expected_type() {
    let error = from_yaml::<u32>(b"not-an-integer").unwrap_err().to_string();
    assert!(error.contains("u32"), "{error}");
    assert!(
        error.lines().next().unwrap().ends_with("invalid u32"),
        "{error}"
    );
}

#[test]
fn deployment_diagnostics_point_to_the_transaction_mapping() {
    let input = indoc! {"
        name: test
        network: devnet
        plan:
          batches:
          - id: 0
            transactions:
            - transaction-type: btc-transfer
              expected-sender: sender
              recipient: recipient
              sats-amount: wrong
              sats-per-byte: 1
    "};
    let error = DeploymentSpecificationFile::from_file_content(input).unwrap_err();
    assert!(
        error.starts_with("unable to read file error: line"),
        "{error}"
    );
    assert!(!error.contains("plan.batches[0].transactions"), "{error}");
}
