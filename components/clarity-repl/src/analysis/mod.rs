pub mod annotation;
pub mod ast_dependency_detector;
pub mod ast_visitor;
pub mod cache;
pub mod call_checker;
pub mod check_checker;
pub mod coverage;
#[cfg(test)]
mod coverage_tests;
pub mod linter;
pub mod lints;
mod util;

use std::collections::{HashMap, HashSet};

use call_checker::CallChecker;
use check_checker::CheckChecker;
use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::Diagnostic;
use clarity_types::diagnostic::Level as ClarityDiagnosticLevel;
use indexmap::IndexMap;
use linter::{LintLevel, LintMapBuilder, LintName};
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::analysis::annotation::Annotation;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::LintGroup;

pub type AnalysisResult = Result<Vec<Diagnostic>, Vec<Diagnostic>>;
pub type AnalysisPassFn = fn(
    &mut AnalysisDatabase,
    &mut AnalysisCache,
    level: ClarityDiagnosticLevel,
    settings: &Settings,
) -> AnalysisResult;

pub trait AnalysisPass {
    #[allow(clippy::ptr_arg)]
    fn run_pass(
        analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: ClarityDiagnosticLevel,
        settings: &Settings,
    ) -> AnalysisResult;
}

impl From<&LintName> for AnalysisPassFn {
    fn from(lint: &LintName) -> AnalysisPassFn {
        match lint {
            // Keep alphabetically sorted
            LintName::UnnecessaryAsMaxLen => lints::UnnecessaryAsMaxLen::run_pass,
            LintName::CaseConst => lints::CaseConst::run_pass,
            LintName::ErrorConst => lints::ErrorConst::run_pass,
            LintName::Noop => lints::NoopChecker::run_pass,
            LintName::Panic => lints::PanicChecker::run_pass,
            LintName::UnnecessaryPublic => lints::UnnecessaryPublic::run_pass,
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

impl TryFrom<&Pass> for AnalysisPassFn {
    type Error = &'static str;

    fn try_from(pass: &Pass) -> Result<AnalysisPassFn, Self::Error> {
        match pass {
            Pass::CheckChecker => Ok(CheckChecker::run_pass),
            Pass::CallChecker => Ok(CallChecker::run_pass),
            Pass::All => Err("Unexpected 'All' in list of passes"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Serialize, Deserialize, Hash)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum Pass {
    All,
    CallChecker,
    CheckChecker,
}

impl Pass {
    fn default_level(&self) -> ClarityDiagnosticLevel {
        match self {
            Self::All => panic!("Cannot call this function on `All`"),
            Self::CallChecker => ClarityDiagnosticLevel::Error,
            Self::CheckChecker => ClarityDiagnosticLevel::Warning,
        }
    }
}

// Each new pass should be included in this list
static ALL_PASSES: [Pass; 2] = [Pass::CheckChecker, Pass::CallChecker];
// Passes that should always be enabled
static DEFAULT_PASSES: [Pass; 1] = [Pass::CallChecker];

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct Settings {
    passes: HashSet<Pass>,
    lints: HashMap<LintName, ClarityDiagnosticLevel>,
    check_checker: check_checker::Settings,
}

impl Settings {
    /// Construct `Settings` with the same default lints and passes used
    /// when deserializing from an empty `SettingsFile`.
    pub fn with_default_lints() -> Self {
        let lints = LintMapBuilder::new().apply_defaults().build();
        let passes = HashSet::from(DEFAULT_PASSES);

        Self {
            passes,
            lints,
            check_checker: check_checker::Settings::default(),
        }
    }

    pub fn enable_all_passes(&mut self) {
        self.passes = HashSet::from(ALL_PASSES)
    }

    pub fn enable_passes(&mut self, passes: &[Pass]) {
        for pass in passes {
            match pass {
                Pass::All => {
                    self.enable_all_passes();
                    return;
                }
                pass => self.passes.insert(*pass),
            };
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

impl From<BoolOr<Self>> for LintLevel {
    fn from(val: BoolOr<Self>) -> Self {
        match val {
            BoolOr::Bool(true) => Self::Warning,
            BoolOr::Bool(false) => Self::Ignore,
            BoolOr::Value(v) => v,
        }
    }
}

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct SettingsFile {
    passes: Option<OneOrList<Pass>>,
    lint_groups: Option<IndexMap<LintGroup, BoolOr<LintLevel>>>,
    lints: Option<IndexMap<LintName, BoolOr<LintLevel>>>,
    check_checker: Option<check_checker::SettingsFile>,
}

impl From<SettingsFile> for Settings {
    fn from(from_file: SettingsFile) -> Self {
        let mut settings = Self::with_default_lints();

        // Process lint groups first
        for (group, val) in from_file.lint_groups.unwrap_or_default() {
            if let Some(level) = LintLevel::from(val).into() {
                group.insert_into(&mut settings.lints, level);
            } else {
                group.remove_from(&mut settings.lints);
            }
        }

        // Individual lints can override group settings
        for (lint, val) in from_file.lints.unwrap_or_default() {
            if let Some(level) = LintLevel::from(val).into() {
                settings.lints.insert(lint, level);
            } else {
                settings.lints.remove(&lint);
            }
        }

        // Add analysis passes listed in config file
        if let Some(file_passes) = from_file.passes {
            let passes = match file_passes {
                OneOrList::One(pass) => HashSet::from([pass]),
                OneOrList::List(passes) => HashSet::from_iter(passes),
            };
            if passes.contains(&Pass::All) {
                settings.passes = HashSet::from(ALL_PASSES);
            } else {
                settings.passes.extend(passes);
            }
        }

        // Each pass that has its own settings should be included here.
        if let Some(check_checker) = from_file.check_checker {
            settings.check_checker = check_checker::Settings::from(check_checker);
        }

        settings
    }
}

pub fn run_analysis(
    contract_analysis: &mut ContractAnalysis,
    analysis_db: &mut AnalysisDatabase,
    annotations: &Vec<Annotation>,
    settings: &Settings,
) -> AnalysisResult {
    let mut errors: Vec<Diagnostic> = vec![];
    let mut passes: Vec<(AnalysisPassFn, ClarityDiagnosticLevel)> = vec![];

    for pass in &settings.passes {
        let f = AnalysisPassFn::try_from(pass).unwrap();
        passes.push((f, pass.default_level()));
    }

    for (name, level) in &settings.lints {
        let lint = AnalysisPassFn::from(name);
        passes.push((lint, level.clone()));
    }

    // Create shared cache for all passes/lints
    let mut cache = AnalysisCache::new(contract_analysis, annotations);

    execute(analysis_db, |database| {
        for (pass, level) in passes {
            // Collect warnings and continue, or if there is an error, return.
            match pass(database, &mut cache, level, settings) {
                Ok(mut w) => errors.append(&mut w),
                Err(mut e) => {
                    errors.append(&mut e);
                    return Err(errors);
                }
            }
        }
        Ok(errors)
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
        assert!(settings.passes.is_empty());
    }

    /// `clarinet check <file>` with no Clarinet.toml uses `Settings::with_default_lints()`.
    #[test]
    fn with_default_lints_enables_default_lints() {
        let settings = Settings::with_default_lints();
        assert!(!settings.lints.is_empty());
        assert_eq!(settings.passes, HashSet::from(DEFAULT_PASSES));
    }

    /// A Clarinet.toml with no `[repl.analysis]` section deserializes as
    /// `SettingsFile::default()`, which should produce the same defaults.
    #[test]
    fn settings_from_empty_settings_file_enables_default_lints() {
        let settings = Settings::from(SettingsFile::default());
        assert!(!settings.lints.is_empty());
        assert_eq!(settings.passes, HashSet::from(DEFAULT_PASSES));
    }

    /// A Clarinet.toml with an empty `[repl.analysis]` section is equivalent
    /// to `SettingsFile` with all `None` fields — same result as above.
    #[test]
    fn settings_from_empty_analysis_section_enables_default_lints() {
        let file = SettingsFile {
            passes: None,
            lint_groups: None,
            lints: None,
            check_checker: None,
        };
        let settings = Settings::from(file);
        assert!(!settings.lints.is_empty());
        assert_eq!(settings.passes, HashSet::from(DEFAULT_PASSES));
    }
}
