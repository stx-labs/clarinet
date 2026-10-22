//! Spec expectations that crates.io serde-saphyr 2.0.0 does not yet satisfy.
//! Keep these assertions intact rather than treating the current behavior as correct.

use std::collections::BTreeMap;

use serde_json::{json, Value};

use super::from_yaml;

/// Core permits true/True/TRUE, but mixed-case tRuE is a string. Main handles this correctly.
#[test]
#[ignore = "serde-saphyr 2.0.0 infers mixed-case booleans"]
fn mixed_case_boolean_is_a_string() {
    assert_eq!(from_yaml::<Value>(b"tRuE").unwrap(), json!("tRuE"));
}

/// Core numbers cannot contain underscores; main correctly preserves this scalar as a string.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts numeric underscores"]
fn numeric_underscores_are_strings() {
    assert_eq!(from_yaml::<Value>(b"1_000").unwrap(), json!("1_000"));
}

/// Core radix prefixes are lowercase; main correctly preserves uppercase 0X as string content.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts uppercase radix prefixes"]
fn uppercase_radix_prefix_is_a_string() {
    assert_eq!(from_yaml::<Value>(b"0X10").unwrap(), json!("0X10"));
}

/// Core has no binary integer notation. Main also incorrectly resolves this to an integer.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts binary integers in the default schema"]
fn binary_notation_is_a_string() {
    assert_eq!(from_yaml::<Value>(b"0b101").unwrap(), json!("0b101"));
}

/// Core resolves a leading-zero decimal as an integer, not octal or float.
/// Main also misses this rule: it resolves the scalar as a string.
#[test]
#[ignore = "serde-saphyr 2.0.0 infers leading-zero decimals as floats"]
fn leading_zero_decimal_is_an_integer() {
    assert_eq!(from_yaml::<Value>(b"010").unwrap(), json!(10));
}

/// A negative leading-zero decimal is also a Core integer; main instead resolves it as a string.
#[test]
#[ignore = "serde-saphyr 2.0.0 infers leading-zero decimals as floats"]
fn negative_leading_zero_decimal_is_an_integer() {
    assert_eq!(from_yaml::<Value>(b"-010").unwrap(), json!(-10));
}

/// A valid Core decimal can populate an integer field. Main also rejects this form.
#[test]
#[ignore = "serde-saphyr 2.0.0 rejects leading-zero decimals for integer fields"]
fn leading_zero_decimal_in_integer_field() {
    assert_eq!(from_yaml::<u64>(b"010").unwrap(), 10);
}

/// An explicit boolean tag does not make tRuE a valid Core boolean; main rejects it.
#[test]
#[ignore = "serde-saphyr 2.0.0 validates booleans case-insensitively"]
fn invalid_explicit_boolean_is_rejected() {
    assert!(from_yaml::<Value>(b"!!bool tRuE").is_err());
}

/// An explicit integer tag does not permit underscores in Core integers; main rejects them.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts numeric underscores even with an explicit tag"]
fn invalid_explicit_integer_is_rejected() {
    assert!(from_yaml::<Value>(b"!!int 1_000").is_err());
}

/// Aliases must retain the Core string type of their anchor; main handles this correctly.
#[test]
#[ignore = "serde-saphyr 2.0.0 infers the anchored mixed-case word as a boolean"]
fn aliases_preserve_core_strings() {
    assert_eq!(
        from_yaml::<Value>(b"[&word tRuE, *word]").unwrap(),
        json!(["tRuE", "tRuE"])
    );
}

/// Core string key 1_000 and integer key 1000 are distinct, as they are on main.
#[test]
#[ignore = "serde-saphyr 2.0.0 compares the underscored key as an integer"]
fn numeric_looking_string_keys_are_distinct() {
    let actual: BTreeMap<String, String> = from_yaml(b"1_000: first\n1000: second").unwrap();
    assert_eq!(actual.len(), 2);
    assert_eq!(actual.get("1_000").map(String::as_str), Some("first"));
}

/// Explicit 1.2 metadata must not enable non-Core inference; main preserves tRuE as a string.
#[test]
#[ignore = "serde-saphyr 2.0.0 does not enforce Core resolution for an explicit 1.2 directive"]
fn explicit_yaml_12_uses_core_resolution() {
    assert_eq!(
        from_yaml::<Value>(b"%YAML 1.2\n---\ntRuE").unwrap(),
        json!("tRuE")
    );
}

/// Our requested opt-in policy resolves yes as a YAML 1.1 boolean when explicitly declared.
/// Main does not switch schemas either; version-based schema selection is an application policy.
#[test]
#[ignore = "serde-saphyr 2.0.0 does not select a schema from version directives"]
fn explicit_yaml_11_boolean() {
    assert_eq!(
        from_yaml::<Value>(b"%YAML 1.1\n---\nyes").unwrap(),
        json!(true)
    );
}

/// Our explicit-1.1 policy interprets leading-zero octal as eight. Main does not switch schemas.
#[test]
#[ignore = "serde-saphyr 2.0.0 does not select legacy octal from version directives"]
fn explicit_yaml_11_octal() {
    assert_eq!(
        from_yaml::<Value>(b"%YAML 1.1\n---\n010").unwrap(),
        json!(8)
    );
}

/// Our explicit-1.1 policy supports sexagesimal integers. Main also leaves this as a string.
#[test]
#[ignore = "serde-saphyr 2.0.0 does not resolve YAML 1.1 sexagesimal integers"]
fn explicit_yaml_11_sexagesimal() {
    assert_eq!(
        from_yaml::<Value>(b"%YAML 1.1\n---\n1:20").unwrap(),
        json!(80)
    );
}
