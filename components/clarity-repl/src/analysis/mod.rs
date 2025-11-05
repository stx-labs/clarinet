pub mod annotation;
pub mod ast_dependency_detector;
pub mod ast_visitor;
pub mod call_checker;
pub mod check_checker;
pub mod coverage;
#[cfg(test)]
mod coverage_tests;
pub mod lints;

use call_checker::CallChecker;
use check_checker::CheckChecker;
use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::Diagnostic;
use clarity_types::diagnostic::Level as ClarityDiagnosticLevel;
use lints::noop::NoopChecker;
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::Serialize;

use crate::analysis::annotation::Annotation;

pub type AnalysisResult = Result<Vec<Diagnostic>, Vec<Diagnostic>>;

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

/// Wrapper around `clarity_types::diagnostic::Level` which adds option to ignore
#[derive(Debug, Default, PartialEq, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(untagged)]
pub enum LintLevel {
    #[default]
    Ignore,
    Enabled(ClarityDiagnosticLevel),
}

impl From<LintLevel> for Option<ClarityDiagnosticLevel> {
    fn from(level: LintLevel) -> Self {
        match level {
            LintLevel::Ignore => None,
            LintLevel::Enabled(l) => Some(l),
        }
    }
}

#[derive(Debug, Default, PartialEq, Clone, Serialize, Deserialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case")]
pub struct LinterSettingsFile {
    noop: Option<LintLevel>,
}

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct LinterSettings {
    noop: Option<ClarityDiagnosticLevel>,
}

impl From<LinterSettingsFile> for LinterSettings {
    fn from(from_file: LinterSettingsFile) -> Self {
        Self {
            noop: from_file.noop.and_then(LintLevel::into),
        }
    }
}

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct Settings {
    passes: Vec<Pass>,
    linter: LinterSettings,
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
    pub fn set_all_lints(&mut self, level: Option<ClarityDiagnosticLevel>) {
        self.linter = LinterSettings { noop: level }
    }
    pub fn enable_all_lints(&mut self, level: ClarityDiagnosticLevel) {
        self.set_all_lints(Some(level));
    }
    pub fn disable_all_lints(&mut self) {
        self.set_all_lints(None);
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
    linter: Option<LinterSettingsFile>,
    check_checker: Option<check_checker::SettingsFile>,
}

impl From<SettingsFile> for Settings {
    fn from(from_file: SettingsFile) -> Self {
        let linter = from_file
            .linter
            .map(LinterSettings::from)
            .unwrap_or_default();

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
            linter,
            passes,
            check_checker,
        }
    }
}

pub trait AnalysisPass {
    #[allow(clippy::ptr_arg)]
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        settings: &Settings,
    ) -> AnalysisResult;
}

pub fn run_analysis(
    contract_analysis: &mut ContractAnalysis,
    analysis_db: &mut AnalysisDatabase,
    annotations: &Vec<Annotation>,
    settings: &Settings,
) -> AnalysisResult {
    let mut errors: Vec<Diagnostic> = Vec::new();
    let mut passes: Vec<
        fn(
            &mut ContractAnalysis,
            &mut AnalysisDatabase,
            &Vec<Annotation>,
            settings: &Settings,
        ) -> AnalysisResult,
    > = vec![];
    for pass in &settings.passes {
        match pass {
            Pass::CheckChecker => passes.push(CheckChecker::run_pass),
            Pass::CallChecker => passes.push(CallChecker::run_pass),
            Pass::All => panic!("unexpected All in list of passes"),
        }
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
