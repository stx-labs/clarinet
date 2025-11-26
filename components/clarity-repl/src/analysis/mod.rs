pub mod annotation;
pub mod ast_dependency_detector;
pub mod ast_visitor;
pub mod call_checker;
pub mod check_checker;
pub mod coverage;
#[cfg(test)]
mod coverage_tests;
pub mod linter;
pub mod lints;

use std::collections::HashMap;
use std::convert::TryFrom;

use call_checker::CallChecker;
use check_checker::CheckChecker;
use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::Diagnostic;
use clarity_types::diagnostic::Level as ClarityDiagnosticLevel;
use linter::{LintLevel, LintName};
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::Serialize;
use strum::VariantArray;

use crate::analysis::annotation::Annotation;
use crate::analysis::linter::LintGroup;

pub type AnalysisResult = Result<Vec<Diagnostic>, Vec<Diagnostic>>;
pub type AnalysisPassFn = fn(
    &mut ContractAnalysis,
    &mut AnalysisDatabase,
    &Vec<Annotation>,
    level: ClarityDiagnosticLevel,
    settings: &Settings,
) -> AnalysisResult;

pub trait AnalysisPass {
    #[allow(clippy::ptr_arg)]
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        level: ClarityDiagnosticLevel,
        settings: &Settings,
    ) -> AnalysisResult;
}

impl From<&LintName> for AnalysisPassFn {
    fn from(lint: &LintName) -> AnalysisPassFn {
        match lint {
            LintName::Noop => lints::NoopChecker::run_pass,
            LintName::UnusedConst => lints::UnusedConst::run_pass,
            LintName::UnusedDataVar => lints::UnusedDataVar::run_pass,
            LintName::UnusedMap => lints::UnusedMap::run_pass,
            LintName::UnusedPrivateFn => lints::UnusedPrivateFn::run_pass,
            LintName::UnusedToken => lints::UnusedToken::run_pass,
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

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct Settings {
    passes: Vec<Pass>,
    lints: HashMap<LintName, ClarityDiagnosticLevel>,
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
        lint: LintName,
        level: ClarityDiagnosticLevel,
    ) -> Option<ClarityDiagnosticLevel> {
        self.lints.insert(lint, level)
    }

    pub fn enable_all_lints(&mut self, level: ClarityDiagnosticLevel) {
        for lint in LintName::VARIANTS {
            self.enable_lint(*lint, level.clone());
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
    lint_groups: Option<HashMap<LintGroup, BoolOr<LintLevel>>>,
    lints: Option<HashMap<LintName, BoolOr<LintLevel>>>,
    check_checker: Option<check_checker::SettingsFile>,
}

impl From<SettingsFile> for Settings {
    fn from(from_file: SettingsFile) -> Self {
        let max_size = LintName::VARIANTS.len();
        let mut lints = HashMap::with_capacity(max_size);

        // Process lint groups first
        for (group, val) in from_file.lint_groups.unwrap_or_default() {
            group.insert_into(&mut lints, LintLevel::from(val));
        }

        // Individual lints can override group settings
        for (lint, val) in from_file.lints.unwrap_or_default() {
            lints.insert(lint, LintLevel::from(val));
        }

        // Filter out explicitly disabled lints
        let lints = lints
            .into_iter()
            .filter_map(|(lint, lint_level)| {
                let diag_level: Option<ClarityDiagnosticLevel> = lint_level.into();
                diag_level.map(|level| (lint, level))
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
    let mut passes: Vec<(AnalysisPassFn, ClarityDiagnosticLevel)> = vec![];

    for pass in &settings.passes {
        let f = AnalysisPassFn::try_from(pass).unwrap();
        passes.push((f, pass.default_level()));
    }

    for (name, level) in &settings.lints {
        let lint = AnalysisPassFn::from(name);
        passes.push((lint, level.clone()));
    }

    execute(analysis_db, |database| {
        for (pass, level) in passes {
            // Collect warnings and continue, or if there is an error, return.
            match pass(contract_analysis, database, annotations, level, settings) {
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
