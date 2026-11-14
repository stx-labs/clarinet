//! Scalar/tag behavior in serde-saphyr 2.0.0 that deviates from YAML 1.1 and 1.2 Core.
//! These ignored assertions describe the standard behavior we want, not parser
//! behavior we intend to preserve. Active valid-form controls live in `yaml_behavior`.
//! Unrecognized plain scalars remain strings; invalid explicit standard tags must error.

use serde_json::{json, Value};

use super::from_yaml;
use super::yaml_behavior::assert_parses_to;

/// Both YAML 1.1 and 1.2 require these mixed-case words to remain strings.
/// Typed bool conversion must reject them, and aliases must retain the string value.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts non-standard boolean casing"]
fn non_standard_boolean_casing_is_text() {
    for input in ["tRuE", "fAlSe"] {
        assert_parses_to(input, json!(input));
        assert!(from_yaml::<bool>(input).is_err());
        assert_parses_to(&format!("[&word {input}, *word]"), json!([input, input]));
    }
}

/// Both versions leave nUlL as text; only null, Null and NULL are null words.
/// In particular an optional string must retain this text rather than become None.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts non-standard null casing"]
fn non_standard_null_casing_is_text() {
    assert_parses_to("nUlL", json!("nUlL"));
    assert_eq!(
        from_yaml::<Option<String>>(b"nUlL").unwrap(),
        Some("nUlL".into())
    );
}

/// YAML 1.1 allows signed 0x but spells octal with a leading 0; YAML 1.2 allows
/// unsigned 0o. Neither uses uppercase 0O/0X or signed 0o, so these stay text.
#[test]
#[ignore = "serde-saphyr 2.0.0 accepts uppercase radix prefixes and signed 0o octal"]
fn non_standard_radix_prefixes_are_text() {
    for input in ["0O10", "0X10", "+0o10", "-0o10"] {
        assert_parses_to(input, json!(input));
    }
}

/// Neither version recognizes mixed-case infinity or signed NaN spellings.
/// Expect string values rather than non-finite float inference or rejection.
#[test]
#[ignore = "serde-saphyr 2.0.0 recognizes non-standard infinity casing and signed NaN"]
fn non_standard_float_spellings_are_text() {
    for input in [".iNf", "+.nan", "-.nan"] {
        assert_parses_to(input, json!(input));
    }
}

/// YAML 1.1 reads 010 as octal 8 and YAML 1.2 as decimal 10; the current float
/// result matches neither. Assert the 1.2 reading, including typed integer conversion.
#[test]
#[ignore = "serde-saphyr 2.0.0 resolves leading-zero integers as floats and rejects typed integers"]
fn leading_zero_integers_have_an_integer_tag() {
    for (input, expected) in [("010", 10), ("-010", -10), ("00", 0), ("08", 8)] {
        assert_parses_to(input, json!(expected));
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
        assert!(from_yaml::<Value>(input).is_err(), "{input}");
    }
}
