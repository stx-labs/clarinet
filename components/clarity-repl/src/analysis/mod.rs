pub mod annotation;
pub mod ast_dependency_detector;
pub mod ast_visitor;
pub mod cache;
pub mod coverage;
#[cfg(test)]
mod coverage_tests;
pub mod linter;
pub mod lints;
mod util;

use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level as ClarityDiagnosticLevel};
use glob::Pattern;
use indexmap::IndexMap;
use linter::{LintLevel, LintMapBuilder, LintName};
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use strum::VariantArray;

use crate::analysis::annotation::Annotation;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::LintGroup;

/// A `Diagnostic` paired with the `LintName` that produced it (if any).
///
/// Diagnostics that don't come from a lint (e.g. annotation parse errors)
/// have `lint_name: None`.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LintDiagnostic {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lint_name: Option<LintName>,
    #[serde(flatten)]
    pub diagnostic: Diagnostic,
}

impl From<LintDiagnostic> for Diagnostic {
    fn from(ld: LintDiagnostic) -> Self {
        ld.diagnostic
    }
}

impl From<Diagnostic> for LintDiagnostic {
    fn from(diagnostic: Diagnostic) -> Self {
        Self {
            lint_name: None,
            diagnostic,
        }
    }
}

pub type AnalysisResult = Result<Vec<Diagnostic>, Vec<Diagnostic>>;
pub type AnalysisPassFn =
    fn(&mut AnalysisDatabase, &mut AnalysisCache, level: ClarityDiagnosticLevel) -> AnalysisResult;

pub trait AnalysisPass {
    #[allow(clippy::ptr_arg)]
    fn run_pass(
        analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: ClarityDiagnosticLevel,
    ) -> AnalysisResult;
}

impl From<&LintName> for AnalysisPassFn {
    fn from(lint: &LintName) -> AnalysisPassFn {
        match lint {
            // Keep alphabetically sorted
            LintName::AtBlock => lints::AtBlock::run_pass,
            LintName::CallChecker => lints::CallChecker::run_pass,
            LintName::CaseBinding => lints::CaseBinding::run_pass,
            LintName::CaseConst => lints::CaseConst::run_pass,
            LintName::CaseDataVar => lints::CaseDataVar::run_pass,
            LintName::CaseFn => lints::CaseFn::run_pass,
            LintName::CaseMap => lints::CaseMap::run_pass,
            LintName::CaseToken => lints::CaseToken::run_pass,
            LintName::CaseTrait => lints::CaseTrait::run_pass,
            LintName::ErrorConst => lints::ErrorConst::run_pass,
            LintName::FlattenVariadic => lints::FlattenVariadic::run_pass,
            LintName::Noop => lints::NoopChecker::run_pass,
            LintName::Panic => lints::PanicChecker::run_pass,
            LintName::RenamedBuiltin => lints::RenamedBuiltin::run_pass,
            LintName::UnnecessaryAsMaxLen => lints::UnnecessaryAsMaxLen::run_pass,
            LintName::UnnecessaryPublic => lints::UnnecessaryPublic::run_pass,
            LintName::UnnecessaryTuple => lints::UnnecessaryTuple::run_pass,
            LintName::UnusedConst => lints::UnusedConst::run_pass,
            LintName::UnusedDataVar => lints::UnusedDataVar::run_pass,
            LintName::UnusedBinding => lints::UnusedBinding::run_pass,
            LintName::UnusedMap => lints::UnusedMap::run_pass,
            LintName::UnusedPrivateFn => lints::UnusedPrivateFn::run_pass,
            LintName::UnusedToken => lints::UnusedToken::run_pass,
            LintName::UnusedTrait => lints::UnusedTrait::run_pass,
        }
    }
}

#[derive(Debug, Default, Clone, Deserialize, Serialize, PartialEq)]
pub struct Settings {
    lints: HashMap<LintName, ClarityDiagnosticLevel>,
    /// Compiled glob patterns for files to include in analysis.
    #[serde(skip)]
    include: Vec<Pattern>,
    /// Compiled glob patterns for files to exclude from analysis.
    #[serde(skip)]
    exclude: Vec<Pattern>,
}

impl Settings {
    /// Construct `Settings` with the same default lints used when
    /// deserializing from an empty `SettingsFile`.
    pub fn with_default_lints() -> Self {
        Self {
            lints: LintMapBuilder::new().apply_defaults().build(),
            ..Default::default()
        }
    }

    pub fn enable_lint(
        &mut self,
        lint: LintName,
        level: ClarityDiagnosticLevel,
    ) -> Option<ClarityDiagnosticLevel> {
        self.lints.insert(lint, level)
    }

    pub fn enable_all_lints(&mut self, level: ClarityDiagnosticLevel) {
        LintGroup::All.insert_into(&mut self.lints, level);
    }

    pub fn disable_lints(&mut self, lint: &LintName) {
        self.lints.remove(lint);
    }

    pub fn disable_all_lints(&mut self) {
        self.lints.clear();
    }

    pub fn lints(&self) -> &HashMap<LintName, ClarityDiagnosticLevel> {
        &self.lints
    }

    /// Check if analysis should run for the given file path.
    ///
    /// - If `include` is set, the file must match at least one pattern.
    /// - If `exclude` is set, the file must not match any pattern.
    /// - If neither is set, all files are analyzed.
    pub fn should_analyze(&self, path: &str) -> bool {
        // Normalize path to forward slashes for consistent matching
        let path = path.replace('\\', "/");

        // If include patterns are set, file must match at least one
        if !self.include.is_empty() && !self.include.iter().any(|p| p.matches(&path)) {
            return false;
        }

        // If exclude patterns are set, file must not match any
        if self.exclude.iter().any(|p| p.matches(&path)) {
            return false;
        }

        true
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(untagged)]
pub enum OneOrList<T> {
    /// Allow `T` as shorthand for `[T]` in the TOML
    One(T),
    /// Allow more than one `T` in the TOML
    List(Vec<T>),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(untagged)]
pub enum BoolOr<T> {
    Bool(bool),
    Value(T),
}

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct SettingsFile {
    /// Lint group levels, applied before individual lints
    groups: Option<IndexMap<LintGroup, BoolOr<LintLevel>>>,
    /// Individual lint levels
    lints: Option<IndexMap<LintName, BoolOr<LintLevel>>>,
    /// Glob patterns for files to include in analysis.
    #[serde(default)]
    include: Option<OneOrList<String>>,
    /// Glob patterns for files to exclude from analysis.
    #[serde(default)]
    exclude: Option<OneOrList<String>>,
    // Keys from the removed `[repl.analysis]` section. Reject them so that a
    // moved section does not silently do nothing.
    #[serde(
        rename = "lint_groups",
        default,
        skip_serializing,
        deserialize_with = "reject_lint_groups"
    )]
    #[cfg_attr(feature = "json_schema", schemars(skip))]
    _lint_groups: (),
    #[serde(
        rename = "passes",
        default,
        skip_serializing,
        deserialize_with = "reject_passes"
    )]
    #[cfg_attr(feature = "json_schema", schemars(skip))]
    _passes: (),
    #[serde(
        rename = "check_checker",
        default,
        skip_serializing,
        deserialize_with = "reject_check_checker"
    )]
    #[cfg_attr(feature = "json_schema", schemars(skip))]
    _check_checker: (),
}

fn reject_lint_groups<'de, D: serde::Deserializer<'de>>(_: D) -> Result<(), D::Error> {
    Err(serde::de::Error::custom(
        "`linter.lint_groups` is not supported; use `linter.groups`",
    ))
}

fn reject_passes<'de, D: serde::Deserializer<'de>>(_: D) -> Result<(), D::Error> {
    Err(serde::de::Error::custom(
        "`linter.passes` is not supported; configure lints in `linter.lints` and `linter.groups`",
    ))
}

fn reject_check_checker<'de, D: serde::Deserializer<'de>>(_: D) -> Result<(), D::Error> {
    Err(serde::de::Error::custom(
        "`linter.check_checker` is not supported; the check-checker was removed",
    ))
}

impl TryFrom<SettingsFile> for Settings {
    type Error = String;

    fn try_from(from_file: SettingsFile) -> Result<Self, Self::Error> {
        let mut settings = Self::with_default_lints();
        // Levels to fall back on when a disabled lint is turned back on
        let defaults = settings.lints.clone();

        // Individual lints override group settings.
        for (group, val) in from_file.groups.unwrap_or_default() {
            LintLevel::from(val).apply_to(group.lints(), &defaults, &mut settings.lints);
        }
        for (lint, val) in from_file.lints.unwrap_or_default() {
            LintLevel::from(val).apply_to(&[lint], &defaults, &mut settings.lints);
        }
        // Correctness lints find code that is certainly wrong, so config cannot
        // disable them or lower their level.
        LintGroup::Correctness.insert_into(&mut settings.lints, ClarityDiagnosticLevel::Error);

        // Process include patterns
        if let Some(include) = from_file.include {
            let raw = match include {
                OneOrList::One(s) => vec![s],
                OneOrList::List(v) => v,
            };
            settings.include = compile_patterns(raw, "include")?;
        }

        // Process exclude patterns
        if let Some(exclude) = from_file.exclude {
            let raw = match exclude {
                OneOrList::One(s) => vec![s],
                OneOrList::List(v) => v,
            };
            settings.exclude = compile_patterns(raw, "exclude")?;
        }

        Ok(settings)
    }
}

fn compile_patterns(raw: Vec<String>, field_name: &str) -> Result<Vec<Pattern>, String> {
    raw.into_iter()
        .map(|p| {
            if p.starts_with('/') || std::path::Path::new(&p).is_absolute() {
                return Err(format!(
                    "invalid glob pattern in {field_name}: \"{p}\": absolute paths are not supported, use paths relative to the project root"
                ));
            }
            Pattern::new(&p)
                .map_err(|e| format!("invalid glob pattern in {field_name}: \"{p}\": {e}"))
        })
        .collect()
}

pub fn run_analysis(
    contract_analysis: &mut ContractAnalysis,
    analysis_db: &mut AnalysisDatabase,
    annotations: &[Annotation],
    settings: &Settings,
) -> Result<Vec<LintDiagnostic>, Vec<LintDiagnostic>> {
    let mut diagnostics: Vec<LintDiagnostic> = vec![];
    let mut rejected = false;

    // Create shared cache for all lints
    let mut cache = AnalysisCache::new(contract_analysis, annotations);

    execute(analysis_db, |database| {
        // Run every lint in stable order, even after a rejection.
        for lint_name in LintName::VARIANTS {
            let Some(level) = settings.lints.get(lint_name) else {
                continue;
            };
            let pass = AnalysisPassFn::from(lint_name);
            let lint_diagnostics = match pass(database, &mut cache, level.clone()) {
                Ok(diagnostics) => diagnostics,
                Err(diagnostics) => {
                    rejected = true;
                    diagnostics
                }
            };
            diagnostics.extend(
                lint_diagnostics
                    .into_iter()
                    .map(|diagnostic| LintDiagnostic {
                        lint_name: Some(*lint_name),
                        diagnostic,
                    }),
            );
        }

        if rejected {
            Err(diagnostics)
        } else {
            Ok(diagnostics)
        }
    })
}

pub fn execute<F, T, E>(conn: &mut AnalysisDatabase, f: F) -> std::result::Result<T, E>
where
    F: FnOnce(&mut AnalysisDatabase) -> std::result::Result<T, E>,
{
    conn.begin();
    let result = f(conn).inspect_err(|_| {
        conn.roll_back().expect("Failed to roll back");
    })?;
    conn.commit().expect("Failed to commit");
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Unit tests (using `SessionSettings::default()`) should have all lints disabled.
    #[test]
    fn default_settings_have_no_lints() {
        let settings = Settings::default();
        assert!(settings.lints.is_empty());
    }

    /// `clarinet check <file>` with no Clarinet.toml uses `Settings::with_default_lints()`.
    #[test]
    fn with_default_lints_enables_default_lints() {
        let settings = Settings::with_default_lints();
        assert!(!settings.lints.is_empty());
        assert_eq!(
            settings.lints.get(&LintName::CallChecker),
            Some(&ClarityDiagnosticLevel::Error)
        );
    }

    /// A Clarinet.toml with no `[linter]` section deserializes as
    /// `SettingsFile::default()`, which should produce the same defaults.
    #[test]
    fn settings_from_empty_settings_file_enables_default_lints() {
        let settings = Settings::try_from(SettingsFile::default()).unwrap();
        assert_eq!(settings, Settings::with_default_lints());
    }

    #[test]
    fn groups_apply_before_individual_lints() {
        let file = SettingsFile {
            groups: Some(IndexMap::from([
                (LintGroup::Safety, BoolOr::Bool(false)),
                (LintGroup::Style, BoolOr::Value(LintLevel::Error)),
            ])),
            lints: Some(IndexMap::from([
                (LintName::UnusedConst, BoolOr::Value(LintLevel::Notice)),
                (LintName::CaseConst, BoolOr::Bool(false)),
            ])),
            ..Default::default()
        };
        let settings = Settings::try_from(file).unwrap();

        assert_eq!(
            settings.lints.get(&LintName::UnusedConst),
            Some(&ClarityDiagnosticLevel::Note)
        );
        assert!(!settings.lints.contains_key(&LintName::Panic));
        assert!(!settings.lints.contains_key(&LintName::CaseConst));
        assert_eq!(
            settings.lints.get(&LintName::CaseFn),
            Some(&ClarityDiagnosticLevel::Error)
        );
    }

    /// Config cannot disable correctness lints or lower their level.
    #[test]
    fn config_does_not_change_correctness_lints() {
        let levels = [BoolOr::Bool(false), BoolOr::Value(LintLevel::Warning)];
        let files = levels.into_iter().flat_map(|level| {
            [
                SettingsFile {
                    groups: Some(IndexMap::from([(LintGroup::All, level.clone())])),
                    ..Default::default()
                },
                SettingsFile {
                    groups: Some(IndexMap::from([(LintGroup::Correctness, level.clone())])),
                    ..Default::default()
                },
                SettingsFile {
                    lints: Some(IndexMap::from([(LintName::CallChecker, level)])),
                    ..Default::default()
                },
            ]
        });
        for file in files {
            let settings = Settings::try_from(file.clone()).unwrap();
            assert_eq!(
                settings.lints.get(&LintName::CallChecker),
                Some(&ClarityDiagnosticLevel::Error),
                "{file:?}"
            );
        }
    }

    #[test]
    fn enabling_a_disabled_group_uses_default_levels() {
        let file = SettingsFile {
            groups: Some(IndexMap::from([
                (LintGroup::All, BoolOr::Bool(false)),
                (LintGroup::Safety, BoolOr::Bool(true)),
            ])),
            ..Default::default()
        };
        let settings = Settings::try_from(file).unwrap();

        assert_eq!(
            settings.lints.get(&LintName::Panic),
            Some(&ClarityDiagnosticLevel::Warning)
        );
        assert!(!settings.lints.contains_key(&LintName::UnusedConst));
    }

    /// `true` and `"on"` turn a lint on without changing the level it
    /// already has, and restore the default level of a disabled lint.
    #[test]
    fn enabling_keeps_the_current_level() {
        for on in [BoolOr::Bool(true), BoolOr::Value(LintLevel::On)] {
            let file = SettingsFile {
                groups: Some(IndexMap::from([
                    (LintGroup::Unused, BoolOr::Bool(false)),
                    (LintGroup::Style, BoolOr::Value(LintLevel::Error)),
                ])),
                lints: Some(IndexMap::from([
                    (LintName::CaseConst, on.clone()),
                    (LintName::UnusedConst, on.clone()),
                    (LintName::CallChecker, on),
                ])),
                ..Default::default()
            };
            let settings = Settings::try_from(file).unwrap();

            assert_eq!(
                settings.lints.get(&LintName::CaseConst),
                Some(&ClarityDiagnosticLevel::Error)
            );
            assert_eq!(
                settings.lints.get(&LintName::UnusedConst),
                Some(&ClarityDiagnosticLevel::Warning)
            );
            assert_eq!(
                settings.lints.get(&LintName::CallChecker),
                Some(&ClarityDiagnosticLevel::Error)
            );
        }
    }

    #[test]
    fn should_analyze_with_no_filters() {
        let settings = Settings::with_default_lints();
        assert!(settings.should_analyze("contracts/foo.clar"));
        assert!(settings.should_analyze("tests/bar.clar"));
    }

    #[test]
    fn should_analyze_with_include_filter() {
        let file = SettingsFile {
            include: Some(OneOrList::One("contracts/*.clar".to_string())),
            ..Default::default()
        };
        let settings = Settings::try_from(file).unwrap();

        assert!(settings.should_analyze("contracts/foo.clar"));
        assert!(!settings.should_analyze("tests/foo.clar"));
    }

    #[test]
    fn should_analyze_with_exclude_filter() {
        let file = SettingsFile {
            exclude: Some(OneOrList::One("tests/*.clar".to_string())),
            ..Default::default()
        };
        let settings = Settings::try_from(file).unwrap();

        assert!(settings.should_analyze("contracts/foo.clar"));
        assert!(!settings.should_analyze("tests/foo.clar"));
    }

    #[test]
    fn should_analyze_with_include_and_exclude() {
        let file = SettingsFile {
            include: Some(OneOrList::List(vec![
                "contracts/*.clar".to_string(),
                "lib/*.clar".to_string(),
            ])),
            exclude: Some(OneOrList::One("contracts/legacy*.clar".to_string())),
            ..Default::default()
        };
        let settings = Settings::try_from(file).unwrap();

        assert!(settings.should_analyze("contracts/foo.clar"));
        assert!(!settings.should_analyze("contracts/legacy.clar"));
        assert!(settings.should_analyze("lib/utils.clar"));
        assert!(!settings.should_analyze("tests/foo.clar"));
    }

    #[test]
    fn absolute_path_in_include_returns_error() {
        let file = SettingsFile {
            include: Some(OneOrList::One("/absolute/path/*.clar".to_string())),
            ..Default::default()
        };
        let result = Settings::try_from(file);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("absolute paths are not supported"));
    }

    #[test]
    fn absolute_path_in_exclude_returns_error() {
        let file = SettingsFile {
            exclude: Some(OneOrList::One("/absolute/path/*.clar".to_string())),
            ..Default::default()
        };
        let result = Settings::try_from(file);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("absolute paths are not supported"));
    }

    #[cfg(windows)]
    #[test]
    fn windows_absolute_path_in_include_returns_error() {
        let file = SettingsFile {
            include: Some(OneOrList::One("C:\\absolute\\path\\*.clar".to_string())),
            ..Default::default()
        };
        let result = Settings::try_from(file);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .contains("absolute paths are not supported"));
    }

    #[test]
    fn should_analyze_with_list_include() {
        let file = SettingsFile {
            include: Some(OneOrList::List(vec![
                "contracts/*.clar".to_string(),
                "src/*.clar".to_string(),
            ])),
            ..Default::default()
        };
        let settings = Settings::try_from(file).unwrap();

        assert!(settings.should_analyze("contracts/foo.clar"));
        assert!(settings.should_analyze("src/bar.clar"));
        assert!(!settings.should_analyze("tests/foo.clar"));
    }
}
