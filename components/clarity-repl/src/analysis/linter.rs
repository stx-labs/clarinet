use std::collections::HashMap;

use clarity_types::diagnostic::Level as ClarityDiagnosticLevel;
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::{EnumString, VariantArray};

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
    EnumString,
    strum::Display,
)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case", try_from = "String")]
#[strum(serialize_all = "snake_case")]
pub enum LintName {
    // Keep sorted alphabetically
    AtBlock,
    CaseBinding,
    CaseConst,
    CaseDataVar,
    CaseMap,
    ErrorConst,
    Noop,
    Panic,
    UnnecessaryAsMaxLen,
    UnnecessaryPublic,
    UnusedBinding,
    UnusedConst,
    UnusedDataVar,
    UnusedMap,
    UnusedPrivateFn,
    UnusedToken,
    UnusedTrait,
}

impl LintName {
    /// Short human-readable description of what the lint checks for
    pub fn description(&self) -> &'static str {
        match self {
            Self::AtBlock => "Warn about usage of deprecated `at-block`",
            Self::CaseBinding => "Enforce kebab-case for bindings",
            Self::CaseConst => "Enforce SCREAMING_SNAKE_CASE for constants",
            Self::CaseDataVar => "Enforce kebab-case for data variables",
            Self::CaseMap => "Enforce kebab-case for maps",
            Self::ErrorConst => "Check that ERR_ constants are unique and use `(err ...)` values",
            Self::Noop => "Find expressions that have no effect",
            Self::Panic => "Warn about `unwrap-panic` and `unwrap-err-panic`",
            Self::UnnecessaryAsMaxLen => "Find unnecessary `as-max-len?` calls",
            Self::UnnecessaryPublic => "Find public functions that could be read-only",
            Self::UnusedBinding => "Find unused variable bindings",
            Self::UnusedConst => "Find unused constants",
            Self::UnusedDataVar => "Find unused data variables",
            Self::UnusedMap => "Find unused maps",
            Self::UnusedPrivateFn => "Find unused private functions",
            Self::UnusedToken => "Find unused tokens",
            Self::UnusedTrait => "Find unused traits",
        }
    }
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
    /// Short human-readable description of the group
    pub fn description(&self) -> &'static str {
        match self {
            Self::All => "All existing lints",
            Self::Perf => "Find inefficient code",
            Self::Safety => "Find code which might not work as user intended",
            Self::Style => "Cosmetic lints like naming conventions",
            Self::Unused => "Find dead code",
        }
    }

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
                "{} is not part of any `LintGroup` variant",
                lint
            )
        }
    }
}
