//! Behavior of the configured YAML reader and writer (`from_yaml` / `to_yaml`).
//! Each test says whether it expects YAML 1.1, YAML 1.2 Core, both, or an application policy.
//! YAML 1.1 types: https://yaml.org/type/
//! YAML 1.2 Core: https://yaml.org/spec/1.2.2/#1032-tag-resolution

use std::collections::BTreeMap;

use indoc::{formatdoc, indoc};
use serde::Deserialize;
use serde_json::{json, Value};
use test_case::test_case;

use super::{from_yaml, to_yaml};

pub(super) fn assert_parses_to(input: &str, expected: Value) {
    assert_eq!(from_yaml::<Value>(input).unwrap(), expected, "{input}");
}

/// YAML 1.1 and 1.2 share these boolean spellings. Keep one control for the
/// incorrect mixed-case spellings tracked in known_spec_deviations.
#[test]
fn boolean_spellings() {
    assert_parses_to(
        "[true, True, TRUE, false, False, FALSE]",
        json!([true, true, true, false, false, false]),
    );
}

/// Expect YAML 1.2 Core: legacy boolean words remain strings. YAML 1.1 would
/// resolve these words as booleans, so this identifies the default choice.
#[test]
fn legacy_boolean_words_are_text() {
    assert_parses_to(
        "[yes, NO, y, n, on, Off]",
        json!(["yes", "NO", "y", "n", "on", "Off"]),
    );
}

/// YAML 1.1 and 1.2 share these null spellings. This is the valid-form control
/// for the mixed-case null resolution bug in known_spec_deviations.
#[test]
fn null_spellings() {
    assert_parses_to("[null, Null, NULL, ~]", json!([null, null, null, null]));
}

/// Both versions resolve these decimal spellings as integers. Check the Value
/// type as a control for the leading-zero inputs incorrectly resolved as floats.
#[test]
fn decimal_integers() {
    assert_parses_to("[0, 42, +42, -42]", json!([0, 42, 42, -42]));
}

/// Expect YAML 1.2's lowercase 0o octal prefix; YAML 1.1 uses leading-zero octal.
#[test]
fn octal_prefix() {
    assert_parses_to("0o10", json!(8));
}

/// Both versions accept lowercase 0x hexadecimal. This controls the incorrect
/// uppercase-prefix inference tracked in known_spec_deviations.
#[test]
fn hexadecimal_prefix() {
    assert_parses_to("0x1f", json!(31));
}

/// Expect YAML 1.1 signed hexadecimal integers. YAML 1.2 Core leaves these as strings.
#[test]
fn signed_hexadecimal() {
    assert_parses_to("[-0x10, +0x10]", json!([-16, 16]));
}

/// Expect YAML 1.1 binary integers; YAML 1.2 Core does not infer binary notation.
#[test]
fn binary_integers() {
    assert_parses_to("[0b101, -0b101, +0b101]", json!([5, -5, 5]));
}

/// Expect YAML 1.1 digit separators, for both implicit and explicit integer tags.
/// YAML 1.2 Core would preserve the plain scalar as text and reject the !!int form.
#[test_case("1_000"; "plain")]
#[test_case("!!int 1_000"; "explicit_tag")]
fn decimal_digit_separators(input: &str) {
    assert_parses_to(input, json!(1000));
}

/// Expect YAML 1.2: an exponent does not require a decimal point or exponent sign.
/// This spelling does not match YAML 1.1's float pattern.
#[test]
fn exponent_without_decimal_point() {
    assert_parses_to("1e2", json!(100.0));
}

/// Expect YAML 1.2 Core text; YAML 1.1 would resolve 1:20 as a base-60 integer.
#[test]
fn sexagesimal_is_text() {
    assert_parses_to("1:20", json!("1:20"));
}

/// Expect YAML 1.2 Core text; the YAML 1.1 timestamp type is not inferred.
#[test]
fn timestamp_is_text() {
    assert_parses_to("2001-12-15", json!("2001-12-15"));
}

/// Both versions support these explicit standard tags.
#[test_case("!!str tRuE", json!("tRuE"); "string")]
#[test_case("!!bool true", json!(true); "boolean")]
#[test_case("!!int 123", json!(123); "integer")]
#[test_case("!!float 1.5", json!(1.5); "float")]
#[test_case("!!null ''", json!(null); "empty_null")]
fn explicit_standard_scalar_tags(input: &str, expected: Value) {
    assert_parses_to(input, expected);
}

/// Both versions preserve quoted scalar text. This checks that quoting avoids
/// inference even for the spellings with known default-resolution bugs.
#[test]
fn quotes_preserve_scalar_text() {
    assert_parses_to(
        "['tRuE', 'nUlL', '0O10', '0X10', '010', '1_000', '.iNf', '+.nan']",
        json!(["tRuE", "nUlL", "0O10", "0X10", "010", "1_000", ".iNf", "+.nan"]),
    );
}

/// Both versions accept these infinity/NaN spellings. Keep valid f64 controls
/// for the non-standard spellings tracked in known_spec_deviations.
#[test]
fn floating_point_special_values() {
    for input in [".inf", ".Inf", ".INF", "+.inf"] {
        assert_eq!(from_yaml::<f64>(input).unwrap(), f64::INFINITY, "{input}");
    }
    assert_eq!(from_yaml::<f64>(b"-.inf").unwrap(), f64::NEG_INFINITY);
    for input in [".nan", ".NaN", ".NAN"] {
        assert!(from_yaml::<f64>(input).unwrap().is_nan());
    }
}

/// Expect YAML 1.2 boolean resolution even for a Rust bool target: legacy words
/// are rejected. YAML 1.1 would accept them, so this guards the typed-reader path.
#[test_case("yes")]
#[test_case("NO")]
#[test_case("y")]
#[test_case("n")]
#[test_case("on")]
#[test_case("Off")]
fn legacy_words_are_not_typed_booleans(input: &str) {
    assert!(from_yaml::<bool>(input).is_err());
    assert!(from_yaml::<bool>(b"true").unwrap());
}

/// Application policy: direct Rust String conversion retains plain scalar text.
/// This does not test implicit YAML 1.1/1.2 tag resolution; transactions use a buffer.
#[test]
fn typed_strings_preserve_text() {
    for input in ["123", "true", "010", "0x10", "3.0"] {
        assert_eq!(from_yaml::<String>(input).unwrap(), input);
    }
}

/// Application policy: string-keyed Rust maps retain a numeric-looking key
/// verbatim. This guards lexical preservation independently of schema resolution.
#[test]
fn typed_string_mapping_keys() {
    let actual: BTreeMap<String, String> = from_yaml(b"010: value").unwrap();
    assert_eq!(actual.get("010").map(String::as_str), Some("value"));
}

/// Alias identity is shared by YAML 1.1 and 1.2; the word yes follows our default
/// 1.2 string behavior. Guard against aliases changing the resolved value or type.
#[test]
fn aliases_preserve_values() {
    assert_parses_to("[&word yes, *word]", json!(["yes", "yes"]));
}

/// Expect YAML 1.2 Core: << is an ordinary key, not the YAML 1.1 merge extension.
/// This guards the configured merge-key policy when aliases are present.
#[test]
fn merge_keys_are_ordinary() {
    let actual: Value = from_yaml(indoc! {b"
        base: &base {value: 1}
        copy: {<<: *base}
    "})
    .unwrap();
    assert_eq!(actual["copy"], json!({"<<": {"value": 1}}));
}

/// Application compatibility policy: !!binary retains encoded text instead of
/// decoding bytes. This is not a claim about YAML 1.1/1.2 binary-type conformance.
#[test]
fn binary_strings_keep_encoded_text() {
    assert_parses_to("!!binary aGVsbG8=", json!("aGVsbG8="));
    assert_eq!(
        from_yaml::<String>(b"!!binary aGVsbG8=").unwrap(),
        "aGVsbG8="
    );
}

/// Expect YAML 1.2 legacy-word strings to survive internally tagged Serde enum
/// buffering. This guards a different reader path from direct scalar parsing.
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

/// Expect YAML 1.2 legacy-word strings while Serde tries untagged enum variants.
/// The boolean candidate must not capture yes using YAML 1.1 resolution.
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

/// Expect YAML 1.2 legacy-word strings through Serde flattened-field buffering.
/// Keep this control for conversions that occur before the final Rust string field.
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

/// Both versions treat literal-block contents as text. A directive-looking line
/// inside that block must not change the following scalar's default 1.2 resolution.
#[test]
fn directive_text_in_scalar_content() {
    assert_parses_to(
        indoc! {"
            text: |
              %YAML 1.1
            value: yes
        "},
        json!({"text": "%YAML 1.1\n", "value": "yes"}),
    );
}

/// Application policy: null mapping values require Option<String>; a required
/// String rejects them. Both YAML versions distinguish null from a quoted string.
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
    assert!(from_yaml::<Required>(&source).is_err());
    assert_eq!(from_yaml::<Optional>(&source).unwrap().value, None);
    let quoted = serde_json::to_string(input).unwrap();
    let quoted_source = format!("value: {quoted}");
    assert_eq!(from_yaml::<Required>(&quoted_source).unwrap().value, input);
}

/// Application policy: explicit numeric/boolean tags block direct String
/// coercion, even when quoted. These tagged inputs are valid in both YAML versions.
#[test_case("!!int 123")]
#[test_case("!!bool true")]
#[test_case("!!float 1.5")]
#[test_case("!!int '123'"; "quoted_integer")]
#[test_case("!!bool 'true'"; "quoted_boolean")]
#[test_case("!!float '1.5'"; "quoted_float")]
fn explicit_scalar_tags_are_not_coerced_into_strings(input: &str) {
    assert!(from_yaml::<String>(input).is_err());
    let quoted = serde_json::to_string(input).unwrap();
    assert!(from_yaml::<String>(format!("!!str {quoted}")).is_ok());
}

/// Both versions distinguish explicitly tagged null from an empty string.
/// Check the application binding: null becomes None, while an untagged empty string does not.
#[test_case("!!null ''")]
#[test_case("!!null null")]
fn explicit_null_tags_produce_none(input: &str) {
    assert_eq!(from_yaml::<Option<String>>(input).unwrap(), None);
    assert_eq!(from_yaml::<Option<String>>(b"''").unwrap(), Some("".into()));
}

/// Application coercion policy: the Rust integer reader accepts quoted decimals,
/// uppercase radix prefixes and digit separators. This is not standard-tag inference.
#[test_case("'1'", 1; "quoted_decimal")]
#[test_case("0X1F", 31; "uppercase_hexadecimal")]
#[test_case("0O10", 8; "uppercase_octal")]
#[test_case("1_000", 1000; "decimal_digit_separators")]
fn typed_unsigned_integer_conversion(input: &str, expected: u32) {
    assert_eq!(from_yaml::<u32>(input).unwrap(), expected);
}

/// Expect YAML 1.1 digit-separator resolution: these keys both mean integer 1000.
/// Under YAML 1.2 Core the underscored key would be text, making them distinct.
#[test]
fn digit_separators_do_not_distinguish_integer_keys() {
    let input = indoc! {"
        1_000: first
        1000: second
    "};
    assert!(from_yaml::<BTreeMap<String, String>>(input).is_err());
}

/// Both versions compare integer keys by value. Keep a control for duplicate-key
/// validation: a different radix must not allow the integer 11 to appear twice.
#[test]
fn radix_does_not_distinguish_integer_keys() {
    let input = indoc! {"
        0xB: first
        11: second
    "};
    assert!(from_yaml::<BTreeMap<String, String>>(input).is_err());
}

/// Directive handling only: declaring 1.1, 1.2 or a future minor version retains
/// the default YAML 1.2 treatment of yes as text; it does not select a legacy schema.
#[test_case("1.1")]
#[test_case("1.2")]
#[test_case("1.3")]
fn version_directives_do_not_select_a_different_schema(declared_version: &str) {
    let input = formatdoc! {"
        %YAML {declared_version}
        ---
        yes
    "};
    assert_parses_to(&input, json!("yes"));
}

/// Expect YAML 1.2: NEL, line separator and paragraph separator are string content.
/// YAML 1.1 treats them as line breaks; check both flow and block mapping contexts.
#[test_case('\u{0085}'; "next_line_is_content")]
#[test_case('\u{2028}'; "line_separator_is_content")]
#[test_case('\u{2029}'; "paragraph_separator_is_content")]
fn unicode_non_ascii_line_breaks(character: char) {
    let text = format!("before{character}after");
    assert_parses_to(&format!("[{text}]"), json!([text]));
    assert_parses_to(&format!("name: {text}"), json!({"name": text}));
}

/// Both versions permit a leading UTF-8 BOM.
#[test]
fn utf8_byte_order_mark() {
    assert_parses_to("\u{feff}name: test", json!({"name": "test"}));
}

/// Expect YAML 1.2 JSON compatibility: paired surrogate escapes decode to one
/// character. The YAML Unicode escape is a control for the resulting code point.
#[test]
fn unicode_surrogate_pair_escapes_follow_json_compatibility() {
    assert_parses_to(r#"{"name": "\uD83D\uDE00"}"#, json!({"name": "😀"}));
    assert_eq!(from_yaml::<String>(br#""\U0001F600""#).unwrap(), "😀");
}

/// Neither version permits an isolated surrogate as a Unicode scalar value.
/// Guard rejection of individual halves alongside the accepted pair test.
#[test_case(br#""\uD83D""#; "unpaired_high_surrogate")]
#[test_case(br#""\uDE00""#; "unpaired_low_surrogate")]
fn unpaired_unicode_surrogate_escapes_are_rejected(input: &[u8]) {
    assert!(from_yaml::<Value>(input).is_err());
}

/// Both versions require standard tags to match the node kind. Guard against
/// silently ignoring a scalar/sequence/map tag attached to the wrong collection.
#[test_case("!!str {}")]
#[test_case("!!seq {}")]
#[test_case("!!map []")]
fn explicit_tags_require_the_matching_node_kind(input: &str) {
    assert!(from_yaml::<Value>(input).is_err());
    assert_parses_to("!!map {}", json!({}));
}

fn assert_emits(value: &str, expected: &str) {
    let document = json!({"value": value});
    let emitted = to_yaml(&document).unwrap();
    assert_eq!(emitted, expected);
    assert_parses_to(&emitted, document);
}

/// Expect YAML 1.2 quoting: words, dates, and sexagesimal forms that YAML 1.1
/// resolves as other types can be emitted as plain strings. Omit the directive
/// and document marker, and preserve the string when read with our default policy.
#[test_case("yes"; "yes")]
#[test_case("NO"; "no")]
#[test_case("on"; "on")]
#[test_case("Off"; "off")]
#[test_case("y"; "y")]
#[test_case("n"; "n")]
#[test_case("2001-12-15"; "date")]
#[test_case("2001-12-15T02:59:43Z"; "timestamp")]
#[test_case("1:20"; "sexagesimal_integer")]
#[test_case("1:20.5"; "sexagesimal_float")]
fn emitter_uses_yaml12_plain_strings_without_a_directive(value: &str) {
    assert_emits(value, &format!("value: {value}\n"));
}

/// YAML 1.2 booleans, nulls, and numbers must stay quoted when emitted as
/// strings. Also keep quoting the parser's extra integer forms so they round-trip
/// as strings despite the reader's mixed 1.1/1.2 behavior.
#[test_case("true"; "boolean")]
#[test_case("null"; "null")]
#[test_case("~"; "null_tilde")]
#[test_case("42"; "decimal_integer")]
#[test_case("100000000000000"; "wallet_balance")]
#[test_case("010"; "leading_zero_integer")]
#[test_case("1e2"; "float")]
#[test_case("0o10"; "octal")]
#[test_case("0x10"; "hexadecimal")]
#[test_case("0b101"; "binary")]
#[test_case("-0x10"; "signed_hexadecimal")]
#[test_case("1_000"; "digit_separators")]
fn emitter_quotes_scalar_looking_strings(value: &str) {
    assert_emits(value, &format!("value: \"{value}\"\n"));
}

/// Emitter presentation policy: long strings use folded blocks without changing content.
#[test]
fn emitter_folds_long_strings_without_changing_their_value() {
    let expected = json!({"name": "Simulated deployment, used as a default for `clarinet console`, `clarinet test` and `clarinet check`"});
    let emitted = to_yaml(&expected).unwrap();
    assert!(emitted.contains(">-\n"), "{emitted}");
    assert_parses_to(&emitted, expected);
}

/// Error-reporting policy: duplicate keys include a source location and library
/// advice. Duplicate-key rejection itself is required by both YAML versions.
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

/// Error-reporting policy: an invalid typed integer reports u32 in the header.
#[test]
fn unsigned_integer_diagnostics_describe_the_expected_type() {
    let error = from_yaml::<u32>(b"not-an-integer").unwrap_err().to_string();
    assert!(error.contains("u32"), "{error}");
    assert!(
        error.lines().next().unwrap().ends_with("invalid u32"),
        "{error}"
    );
}
