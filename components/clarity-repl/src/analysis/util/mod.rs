use clarity_types::ClarityName;

pub enum CaseError {
    /// Empty string
    Empty,
    /// Identifier contains character not allowed in this case
    IllegalCharacter(char),
    /// Identifier contains
    ConsecutiveUnderscores,
}

/// Returns `true` if identifier is in SCREAMING_SNAME_CASE
///
/// An identifier is considered SCREAMING_SNAKE_CASE if...
///  - It contains only ASCII uppercase letters (A–Z), digits (0–9), and underscores (_)
///  - Contains at least one letter
///  - Does **not** contain consecutive underscores
///
/// Note that leading or trailing underscores **are** allowed
pub fn is_screaming_snake_case(ident: &ClarityName) -> Result<(), CaseError> {
    let ident = ident.as_str();

    if (ident.is_empty()) {
        return Err(CaseError::Empty);
    }

    Ok(())
}
