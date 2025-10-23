pub mod annotation;
pub mod ast_dependency_detector;
pub mod ast_visitor;
pub mod call_checker;
pub mod check_checker;
pub mod coverage;
#[cfg(test)]
mod coverage_tests;
pub mod native_func_noop;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::Diagnostic;
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::Serialize;
use strum::VariantArray;

use self::call_checker::CallChecker;
use self::check_checker::CheckChecker;
use self::native_func_noop::NoopChecker;
use crate::analysis::annotation::Annotation;

pub type AnalysisResult = Result<Vec<Diagnostic>, Vec<Diagnostic>>;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize, VariantArray)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(rename_all = "snake_case")]
pub enum Pass {
    CallChecker,
    CheckChecker,
    NoopChecker,
}

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct Settings {
    passes: Vec<Pass>,
    check_checker: check_checker::Settings,
}

impl Settings {
    pub fn enable_all_passes(&mut self) {
        self.passes = Pass::VARIANTS.to_vec();
    }

    pub fn set_passes(&mut self, passes: Vec<Pass>) {
        self.passes = passes;
    }

    pub fn add_passes(&mut self, passes: &[Pass]) {
        self.passes.extend_from_slice(passes);
    }
}

/// Allow different methods to specify elements of a set of type `T`
#[derive(Clone, Debug, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[serde(untagged)]
pub enum SetElementSpecifier<T> {
    /// Allow `T` as shorthand for `[T]` in the TOML
    One(T),
    /// Allow more than one `T` in the TOML
    List(Vec<T>),
    /// All elements in the set
    All(bool),
}

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct SettingsFile {
    passes: Option<SetElementSpecifier<Pass>>,
    check_checker: Option<check_checker::SettingsFile>,
}

impl From<SettingsFile> for Settings {
    fn from(from_file: SettingsFile) -> Self {
        let passes = from_file
            .passes
            .map(|file_passes| match file_passes {
                SetElementSpecifier::All(true) => Pass::VARIANTS.to_vec(),
                SetElementSpecifier::All(false) => vec![],
                SetElementSpecifier::One(pass) => vec![pass],
                SetElementSpecifier::List(passes) => passes,
            })
            .unwrap_or_default();

        // Each pass that has its own settings should be included here.
        let check_checker = from_file
            .check_checker
            .map(check_checker::Settings::from)
            .unwrap_or_default();

        Self {
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
            Pass::NoopChecker => passes.push(NoopChecker::run_pass),
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
