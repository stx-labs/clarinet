use std::collections::HashMap;
use std::convert::TryFrom;

use clarity_types::diagnostic::Level as ClarityDiagnosticLevel;
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::Serialize;
use strum::{EnumString, VariantArray};

use crate::analysis::annotation::Annotation;

/// Represents a single linter pass
#[derive(
    Debug, PartialEq, Eq, Copy, Clone, Serialize, Deserialize, Hash, VariantArray, EnumString,
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
    /// Find dead code
    Unused,
    /// Find inefficient code
    Perf,
    /// Cosmetic lints like naming conventions
    Style,
    /// Find code which might not work as user intended
    Safety,
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
            Unused => {
                map.insert(LintName::UnusedConst, value);
                map.insert(LintName::UnusedDataVar, value);
                map.insert(LintName::UnusedBinding, value);
                map.insert(LintName::UnusedMap, value);
                map.insert(LintName::UnusedPrivateFn, value);
                map.insert(LintName::UnusedToken, value);
                map.insert(LintName::UnusedTrait, value);
            }
            Perf => {}
            Style => {
                map.insert(LintName::CaseConst, value);
            }
            Safety => {
                map.insert(LintName::Noop, value);
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
