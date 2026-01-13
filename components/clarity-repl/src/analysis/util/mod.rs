//! Utility functions used by analysis passes/lints

#[derive(Debug, PartialEq)]
pub enum CaseError {
    /// Empty string
    Empty,
    /// Identifier contains character not allowed in this case
    IllegalCharacter(u8),
    /// Identifier contains more than one consecutive underscore
    ConsecutiveUnderscores,
}

/// Returns error if identifier is not in SCREAMING_SNAME_CASE
///
/// An identifier is considered SCREAMING_SNAKE_CASE if...
///  - It contains only ASCII uppercase letters (A–Z), digits (0–9), and underscores (_)
///  - Does **not** contain consecutive underscores
///
/// NOTE: Leading or trailing underscores **are** allowed
/// NOTE: This function accepts **any** `&str`
pub fn match_screaming_snake_case(s: &str) -> Result<(), CaseError> {
    if s.is_empty() {
        return Err(CaseError::Empty);
    }

    let mut prev_underscore = false;

    for b in s.bytes() {
        match b {
            b'A'..=b'Z' | b'0'..=b'9' => {
                prev_underscore = false;
            }
            b'_' => {
                if prev_underscore {
                    return Err(CaseError::ConsecutiveUnderscores);
                }
                prev_underscore = true;
            }
            _ => {
                return Err(CaseError::IllegalCharacter(b));
            }
        }
    }

    Ok(())
}

/// Returns true if identifier matches our convention for explicity unused code
///
/// An identifier is considered explicilty unused and ignored by `LintGroup::Unused` lints if...
///  - It begins with `_` (illegal in current version of Clarity)
///  - It ends with `_`
pub fn is_explicitly_unused(s: &str) -> bool {
    let prefix = "_";
    let suffix = prefix;

    s.starts_with(prefix) || s.ends_with(suffix)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn match_screaming_snake_case_pass() {
        let test_cases = [
            "SCREAMING_SNAKE_CASE",
            // Underscores
            "HAS_UNDERSCORE",
            "C_9A2_R1NO_YD",
            "MNV_P_T_S_BK4V",
            "X_3W_WB2_IXAI",
            "QZ79_36JD2D",
            "WNM_HWQ_LZGQ",
            "DANK_720464",
            "G_262VQZLDG",
            "QASDGF_55555555QEEFHS1688W34",
            "U_C_6_6_J_G_1_U_0_0_9_7_M_F_R_K_Y_T_K_R",
            // No underscore
            "NOUNDERSCORE",
            "P",
            "TSDB5VUOG2NHW",
            "QQ",
            // Leading number
            "1_LEADING_NUMBER",
            "0YAO9MCJX6",
            "9V_25JWW4EB",
            // Leading underscore
            "_LEADING_UNDERSCORE",
            "_123456",
            "_L",
            // Trailing underscore
            "TRAILING_UNDERSCORE_",
            "54321_",
            "T_",
            // Leading and trailing underscores
            "_LEADING_AND_TRAILING_UNDERSCORES_",
            "_12321_",
            "_LT_",
            "_",
        ];

        for s in test_cases {
            match_screaming_snake_case(s).unwrap_or_else(|e| {
                let msg = format!("Failed on '{s}' with error: {e:?}");
                panic!("{}", msg);
            });
        }
    }

    #[test]
    fn match_screaming_snake_case_illegal_char() {
        let test_cases = [
            ("NOT-SCREAMING-SNAKE-CASE", b'-'),
            ("no_lower_case", b'n'),
            ("@_IS_NOT_ALLOWED", b'@'),
            ("NO_%_EITHER", b'%'),
            ("ALSO_NO_&", b'&'),
        ];

        for (s, c) in test_cases {
            assert_eq!(
                match_screaming_snake_case(s),
                Err(CaseError::IllegalCharacter(c)),
                "string '{s}' did not produce expected error"
            );
        }
    }

    #[test]
    fn match_screaming_snake_case_consecutive_underscores() {
        let test_cases = [
            "NO__CONSECUTIVE__UNDERSCORES",
            "__NO_DOUBLE_LEADING_UNDERSCORES",
            "NO_DOUBLE_TRAILING_UNDERSCORES__",
            "__",
            "__________________",
            "__12321__",
        ];

        for s in test_cases {
            assert_eq!(
                match_screaming_snake_case(s),
                Err(CaseError::ConsecutiveUnderscores),
                "string '{s}' did not produce expected error"
            );
        }
    }

    #[test]
    fn is_explicitly_unused_pass() {
        let test_cases = [
            // Leading underscore
            "_LEADING_UNDERSCORE",
            "_123456",
            "_L",
            // Trailing underscore
            "TRAILING_UNDERSCORE_",
            "54321_",
            "T_",
            // Leading and trailing underscores
            "_LEADING_AND_TRAILING_UNDERSCORES_",
            "_12321_",
            "_LT_",
            "_",
        ];

        for s in test_cases {
            assert!(
                is_explicitly_unused(s),
                "string '{}' was not flagged as 'unused'",
                s
            );
        }
    }

    #[test]
    fn is_explicitly_unused_fail() {
        let test_cases = [
            "SCREAMING_SNAKE_CASE",
            // Underscores
            "HAS_UNDERSCORE",
            "C_9A2_R1NO_YD",
            "MNV_P_T_S_BK4V",
            "X_3W_WB2_IXAI",
            "QZ79_36JD2D",
            "WNM_HWQ_LZGQ",
            "DANK_720464",
            "G_262VQZLDG",
            "QASDGF_55555555QEEFHS1688W34",
            "U_C_6_6_J_G_1_U_0_0_9_7_M_F_R_K_Y_T_K_R",
            // No underscore
            "NOUNDERSCORE",
            "P",
            "TSDB5VUOG2NHW",
            "QQ",
            // Leading number
            "1_LEADING_NUMBER",
            "0YAO9MCJX6",
            "9V_25JWW4EB",
        ];

        for s in test_cases {
            assert!(
                !is_explicitly_unused(s),
                "string '{}' was flagged as 'unused'",
                s
            );
        }
    }
}
