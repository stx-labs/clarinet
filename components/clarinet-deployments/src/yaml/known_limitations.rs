//! Incorrect standard-tag resolution under both YAML 1.1 and YAML 1.2 Core.
//! These assertions remain ignored until the parser is fixed; valid counterparts
//! for every feature run in `yaml_behavior`. Plain unrecognized scalars are valid
//! strings, whereas invalid spellings with an explicit standard tag must error.

use serde_json::{json, Value};

use super::from_yaml;

/// Both YAML 1.1 and 1.2 require these mixed-case words to remain strings.
/// Typed bool conversion must reject them, and aliases must retain the string value.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts non-standard boolean casing"]
fn non_standard_boolean_casing_is_text() {
    for input in ["tRuE", "fAlSe"] {
        assert_eq!(from_yaml::<Value>(input.as_bytes()).unwrap(), json!(input));
        assert!(from_yaml::<bool>(input.as_bytes()).is_err());
        let aliases = format!("[&word {input}, *word]");
        assert_eq!(
            from_yaml::<Value>(aliases.as_bytes()).unwrap(),
            json!([input, input])
        );
    }
}

/// Both versions leave nUlL as text; only null, Null and NULL are null words.
/// In particular an optional string must retain this text rather than become None.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts non-standard null casing"]
fn non_standard_null_casing_is_text() {
    assert_eq!(from_yaml::<Value>(b"nUlL").unwrap(), json!("nUlL"));
    assert_eq!(
        from_yaml::<Option<String>>(b"nUlL").unwrap(),
        Some("nUlL".into())
    );
}

/// Neither version infers uppercase radix prefixes or signed 0o octal.
/// Expect strings; valid signed 0x hexadecimal is covered by the YAML 1.1 behavior test.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts uppercase radix prefixes and signed 0o octal"]
fn non_standard_radix_prefixes_are_text() {
    // YAML 1.1 permits signed 0x hexadecimal, but its octal prefix is 0, not 0o.
    // YAML 1.2 permits 0o octal without a sign. Neither uses uppercase 0O or 0X.
    for input in ["0O10", "0X10", "+0o10", "-0o10"] {
        assert_eq!(from_yaml::<Value>(input.as_bytes()).unwrap(), json!(input));
    }
}

/// Neither version recognizes mixed-case infinity or signed NaN spellings.
/// Expect string values rather than non-finite float inference or rejection.
#[test]
#[ignore = "serde-saphyr 2.0.0 recognizes non-standard infinity casing and signed NaN"]
fn non_standard_float_spellings_are_text() {
    for input in [".iNf", "+.nan", "-.nan"] {
        assert_eq!(from_yaml::<Value>(input.as_bytes()).unwrap(), json!(input));
    }
}

/// Expect YAML 1.2 decimal integer resolution and typed integer conversion.
/// The current float inference matches neither 1.2 nor 1.1's leading-zero octal rule.
#[test]
#[ignore = "serde-saphyr 2.0.0 resolves leading-zero integers as floats and rejects typed integers"]
fn leading_zero_integers_have_an_integer_tag() {
    // YAML 1.1 interprets 010 as octal 8; YAML 1.2 interprets it as decimal 10.
    // The current float result is correct under neither schema. Assert 1.2 here.
    for (input, expected) in [("010", 10), ("-010", -10), ("00", 0), ("08", 8)] {
        assert_eq!(
            from_yaml::<Value>(input.as_bytes()).unwrap(),
            json!(expected)
        );
    }
    assert_eq!(from_yaml::<u64>(b"010").unwrap(), 10);
}

/// Neither version permits these spellings for explicit standard scalar tags.
/// They must be rejected; unlike an untagged word, the tag forbids a string fallback.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts invalid spellings with explicit standard tags"]
fn explicit_standard_tags_reject_invalid_scalar_spellings() {
    // !!int 1_000 is valid YAML 1.1 and is tested actively in yaml_behavior.
    for input in [
        "!!bool tRuE",
        "!!bool fAlSe",
        "!!int 0O10",
        "!!int 0X10",
        "!!int +0o10",
    ] {
        assert!(from_yaml::<Value>(input.as_bytes()).is_err(), "{input}");
    }
}
