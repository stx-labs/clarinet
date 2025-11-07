pub mod annotation;
pub mod ast_dependency_detector;
pub mod ast_visitor;
pub mod call_checker;
pub mod check_checker;
pub mod coverage;
#[cfg(test)]
mod coverage_tests;
pub mod lints;

use std::collections::HashMap;
use std::convert::TryFrom;

use call_checker::CallChecker;
use check_checker::CheckChecker;
use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::Diagnostic;
use clarity_types::diagnostic::Level as ClarityDiagnosticLevel;
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::Serialize;
use strum::{EnumString, VariantArray};

use crate::analysis::annotation::Annotation;

pub type AnalysisResult = Result<Vec<Diagnostic>, Vec<Diagnostic>>;
pub type AnalysisPassFn = fn(
    &mut ContractAnalysis,
    &mut AnalysisDatabase,
    &Vec<Annotation>,
    settings: &Settings,
) -> AnalysisResult;

pub trait AnalysisPass {
    #[allow(clippy::ptr_arg)]
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        settings: &Settings,
    ) -> AnalysisResult;
}

impl From<&Lint> for AnalysisPassFn {
    fn from(lint: &Lint) -> AnalysisPassFn {
        match lint {
            Lint::Noop => lints::NoopChecker::run_pass,
            Lint::UnusedConst => lints::UnusedConst::run_pass,
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

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum Pass {
    All,
    CallChecker,
    CheckChecker,
}

// Each new pass should be included in this list
static ALL_PASSES: [Pass; 2] = [Pass::CheckChecker, Pass::CallChecker];

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, Hash, VariantArray, EnumString)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case", try_from = "String")]
#[strum(serialize_all = "snake_case")]
pub enum Lint {
    Noop,
    UnusedConst,
}

impl Lint {}

/// `strum` can automatically derive `TryFrom<&str>`, but we need a wrapper to work with `String`s
impl TryFrom<String> for Lint {
    type Error = strum::ParseError;

    fn try_from(s: String) -> Result<Lint, Self::Error> {
        Lint::try_from(s.as_str())
    }
}

/// Map user intput to `clarity_types::diagnostic::Level` or ignore
#[derive(Debug, Default, PartialEq, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum LintLevel {
    #[default]
    #[serde(alias = "allow", alias = "false", alias = "off", alias = "none")]
    Ignore,
    #[serde(alias = "note")]
    Notice,
    #[serde(alias = "warn", alias = "true", alias = "on")]
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

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct Settings {
    passes: Vec<Pass>,
    lints: HashMap<Lint, ClarityDiagnosticLevel>,
    check_checker: check_checker::Settings,
}

impl Settings {
    pub fn enable_all_passes(&mut self) {
        self.passes = ALL_PASSES.to_vec();
    }

    pub fn set_passes(&mut self, passes: Vec<Pass>) {
        for pass in passes {
            match pass {
                Pass::All => {
                    self.passes = ALL_PASSES.to_vec();
                    return;
                }
                pass => self.passes.push(pass),
            };
        }
    }

    pub fn enable_lint(
        &mut self,
        lint: Lint,
        level: ClarityDiagnosticLevel,
    ) -> Option<ClarityDiagnosticLevel> {
        self.lints.insert(lint, level)
    }

    pub fn enable_all_lints(&mut self, level: ClarityDiagnosticLevel) {
        for lint in Lint::VARIANTS {
            self.enable_lint(lint.clone(), level.clone());
        }
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

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct SettingsFile {
    passes: Option<OneOrList<Pass>>,
    lints: Option<HashMap<Lint, LintLevel>>,
    check_checker: Option<check_checker::SettingsFile>,
}

impl From<SettingsFile> for Settings {
    fn from(from_file: SettingsFile) -> Self {
        let lints = from_file
            .lints
            .unwrap_or_default()
            .into_iter()
            .filter_map(|(lint, lint_level)| {
                let clarity_level: Option<ClarityDiagnosticLevel> = lint_level.into();
                clarity_level.map(|level| (lint, level))
            })
            .collect();

        let passes = from_file
            .passes
            .map(|file_passes| match file_passes {
                OneOrList::One(pass) => vec![pass],
                OneOrList::List(passes) => passes,
            })
            .map(|passes| {
                if passes.contains(&Pass::All) {
                    ALL_PASSES.to_vec()
                } else {
                    passes
                }
            })
            .unwrap_or_default();

        // Each pass that has its own settings should be included here.
        let check_checker = from_file
            .check_checker
            .map(check_checker::Settings::from)
            .unwrap_or_default();

        Self {
            lints,
            passes,
            check_checker,
        }
    }
}

pub fn run_analysis(
    contract_analysis: &mut ContractAnalysis,
    analysis_db: &mut AnalysisDatabase,
    annotations: &Vec<Annotation>,
    settings: &Settings,
) -> AnalysisResult {
    let mut errors: Vec<Diagnostic> = vec![];
    let mut passes: Vec<AnalysisPassFn> = vec![];

    for pass in &settings.passes {
        let f = AnalysisPassFn::try_from(pass).unwrap();
        passes.push(f);
    }

    for lint in settings.lints.keys() {
        passes.push(AnalysisPassFn::from(lint));
    }

    execute(analysis_db, |database| {
        for pass in passes {
            // Collect warnings and continue, or if there is an error, return.
            match pass(contract_analysis, database, annotations, settings) {
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
