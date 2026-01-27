use clarity_types::diagnostic::Level as ClarityDiagnosticLevel;
use hashbrown::HashMap;
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::Serialize;
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
    Noop,
    UnusedConst,
    UnusedDataVar,
    UnusedBinding,
    UnusedMap,
    UnusedPrivateFn,
    UnusedToken,
    UnusedTrait,
    CaseConst,
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
    Debug, PartialEq, Eq, Copy, Clone, Serialize, Deserialize, Hash, VariantArray, EnumString,
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
    pub fn insert_into<T: Copy>(&self, map: &mut HashMap<LintName, T>, value: T) {
        use LintGroup::*;

        match self {
            All => {
                for lint in LintName::VARIANTS {
                    map.insert(*lint, value);
                }
            }
            Perf => {}
            Safety => {
                map.insert(LintName::Noop, value);
            }
            Style => {
                map.insert(LintName::CaseConst, value);
            }
            Unused => {
                map.insert(LintName::UnusedConst, value);
                map.insert(LintName::UnusedDataVar, value);
                map.insert(LintName::UnusedBinding, value);
                map.insert(LintName::UnusedMap, value);
                map.insert(LintName::UnusedPrivateFn, value);
                map.insert(LintName::UnusedToken, value);
                map.insert(LintName::UnusedTrait, value);
            }
        }
    }
}

pub type LintMap = HashMap<LintName, LintLevel>;

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
        LintGroup::Unused.insert_into(&mut self.map, LintLevel::Warning);
        LintGroup::Perf.insert_into(&mut self.map, LintLevel::Warning);
        LintGroup::Safety.insert_into(&mut self.map, LintLevel::Warning);
        //LintGroup::Style.insert_into(&mut map, LintLevel::Notice);

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
            group.insert_into(&mut lints, LintLevel::default());
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
