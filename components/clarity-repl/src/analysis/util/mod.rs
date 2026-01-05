use clarity_types::ClarityName;

/// Returns `true` if identifier is
///
/// This function takes a `ClarityName` as input, so we can assume that the following about the input string:
///  - It is not empty
///  - It does not contain characters that are illegal in a Clarity identifier
pub fn is_screaming_snake_case(_ident: &ClarityName) -> bool {
    true
}
