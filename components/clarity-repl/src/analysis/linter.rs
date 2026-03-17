use std::collections::HashMap;

use clarity_types::diagnostic::Level as ClarityDiagnosticLevel;
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::{EnumMessage, EnumString, VariantArray};

use crate::analysis::annotation::Annotation;

/// Represents a single linter pass
#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    Serialize,
    Deserialize,
    Hash,
    VariantArray,
    EnumMessage,
    EnumString,
    strum::Display,
)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case", try_from = "String")]
#[strum(serialize_all = "snake_case")]
pub enum LintName {
    // Keep sorted alphabetically
    /// Warn about usage of deprecated `at-block`
    AtBlock,
    /// Enforce kebab-case for bindings
    CaseBinding,
    /// Enforce SCREAMING_SNAKE_CASE for constants
    CaseConst,
    /// Enforce kebab-case for data variables
    CaseDataVar,
    /// Enforce kebab-case for maps
    CaseMap,
    /// Check that ERR_ constants are unique and use `(err ...)` values
    ErrorConst,
    /// Find expressions that have no effect
    Noop,
    /// Warn about `unwrap-panic` and `unwrap-err-panic`
    Panic,
    /// Find unnecessary `as-max-len?` calls
    UnnecessaryAsMaxLen,
    /// Find public functions that could be read-only
    UnnecessaryPublic,
    /// Find unused variable bindings
    UnusedBinding,
    /// Find unused constants
    UnusedConst,
    /// Find unused data variables
    UnusedDataVar,
    /// Find unused maps
    UnusedMap,
    /// Find unused private functions
    UnusedPrivateFn,
    /// Find unused tokens
    UnusedToken,
    /// Find unused traits
    UnusedTrait,
}

/// `strum` can automatically derive `TryFrom<&str>`, but we need a wrapper to work with `String`s
impl TryFrom<String> for LintName {
    type Error = strum::ParseError;

    fn try_from(s: String) -> Result<LintName, Self::Error> {
        LintName::try_from(s.as_str())
    }
}

/// Represents a set of linter passes
#[derive(
    Debug,
    PartialEq,
    Eq,
    Copy,
    Clone,
    Serialize,
    Deserialize,
    Hash,
    VariantArray,
    EnumMessage,
    EnumString,
    strum::Display,
)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case", try_from = "String")]
#[strum(serialize_all = "snake_case")]
pub enum LintGroup {
    /// All existing lints
    All,
    /// Find inefficient code
    Perf,
    /// Find code which might not work as user intended
    Safety,
    /// Cosmetic lints like naming conventions
    Style,
    /// Find dead code
    Unused,
}

impl LintGroup {
    /// Find the most specific group a lint belongs to (excluding `All`)
    pub fn of(lint: &LintName) -> Option<&'static Self> {
        LintGroup::VARIANTS
            .iter()
            .find(|g| !matches!(g, LintGroup::All) && g.lints().contains(lint))
    }

    /// Returns lints which belong to group
    pub fn lints(&self) -> &[LintName] {
        use LintGroup::*;

        match self {
            All => LintName::VARIANTS,
            Perf => &[LintName::UnnecessaryAsMaxLen, LintName::UnnecessaryPublic],
            Safety => &[
                LintName::AtBlock,
                LintName::ErrorConst,
                LintName::Noop,
                LintName::Panic,
            ],
            Style => &[
                LintName::CaseBinding,
                LintName::CaseConst,
                LintName::CaseDataVar,
                LintName::CaseMap,
            ],
            Unused => &[
                LintName::UnusedConst,
                LintName::UnusedDataVar,
                LintName::UnusedBinding,
                LintName::UnusedMap,
                LintName::UnusedPrivateFn,
                LintName::UnusedToken,
                LintName::UnusedTrait,
            ],
        }
    }

    /// Insert all members into map at given level
    pub fn insert_into<T: Clone>(&self, map: &mut HashMap<LintName, T>, value: T) {
        for lint in self.lints() {
            map.insert(*lint, value.clone());
        }
    }

    /// Remove all members from map at given level
    pub fn remove_from<T>(&self, map: &mut HashMap<LintName, T>) {
        for lint in self.lints() {
            map.remove(lint);
        }
    }
}

pub type LintMap = HashMap<LintName, ClarityDiagnosticLevel>;

pub struct LintMapBuilder {
    map: LintMap,
}

impl LintMapBuilder {
    pub fn new() -> Self {
        let max_size = LintName::VARIANTS.len();
        let map = HashMap::with_capacity(max_size);

        Self { map }
    }

    pub fn apply_defaults(mut self) -> Self {
        LintGroup::Unused.insert_into(&mut self.map, ClarityDiagnosticLevel::Warning);
        LintGroup::Perf.insert_into(&mut self.map, ClarityDiagnosticLevel::Warning);
        LintGroup::Safety.insert_into(&mut self.map, ClarityDiagnosticLevel::Warning);
        //LintGroup::Style.insert_into(&mut map, ClarityDiagnosticLevel::Notice);

        self
    }

    pub fn build(self) -> LintMap {
        self.map
    }
}

impl Default for LintMapBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// `strum` can automatically derive `TryFrom<&str>`, but we need a wrapper to work with `String`s
impl TryFrom<String> for LintGroup {
    type Error = strum::ParseError;

    fn try_from(s: String) -> Result<LintGroup, Self::Error> {
        LintGroup::try_from(s.as_str())
    }
}

/// Map user intput to `clarity_types::diagnostic::Level` or ignore
#[derive(Debug, Default, PartialEq, Copy, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum LintLevel {
    #[default]
    #[serde(alias = "allow", alias = "off", alias = "none")]
    Ignore,
    #[serde(alias = "note")]
    Notice,
    #[serde(alias = "warn", alias = "on")]
    Warning,
    #[serde(alias = "err")]
    Error,
}

impl From<LintLevel> for Option<ClarityDiagnosticLevel> {
    fn from(level: LintLevel) -> Self {
        match level {
            LintLevel::Ignore => None,
            LintLevel::Notice => Some(ClarityDiagnosticLevel::Note),
            LintLevel::Warning => Some(ClarityDiagnosticLevel::Warning),
            LintLevel::Error => Some(ClarityDiagnosticLevel::Error),
        }
    }
}

pub trait Lint {
    fn get_name() -> LintName;
    fn match_allow_annotation(annotation: &Annotation) -> bool;
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Check that all lints are in at least one `LintGroup`
    #[test]
    fn all_lints_are_part_of_least_one_group() {
        let mut lints = HashMap::new();

        // Add all groups (except `All`) to the map
        for group in LintGroup::VARIANTS {
            if matches!(group, LintGroup::All) {
                continue;
            }
            group.insert_into(&mut lints, ClarityDiagnosticLevel::Warning);
        }

        for lint in LintName::VARIANTS {
            assert!(
                lints.contains_key(lint),
                "{lint} is not part of any `LintGroup` variant"
            )
        }
    }

    #[test]
    fn all_lints_have_documentation() {
        for lint in LintName::VARIANTS {
            assert!(
                lint.get_documentation().is_some(),
                "LintName::{lint} is missing a doc comment"
            );
        }
    }

    #[test]
    fn all_lint_groups_have_documentation() {
        for group in LintGroup::VARIANTS {
            assert!(
                group.get_documentation().is_some(),
                "LintGroup::{group} is missing a doc comment"
            );
        }
    }
}
