//! Lint to check case of data variable names (declared using `define-data-var`)
//!
//! By default, this enforces kebab-case

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::SymbolicExpression;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::data_vars::DataVarData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::{match_kebab_case, strip_unused_suffix, CaseError};
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct CaseDataVar<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a, 'b> CaseDataVar<'a, 'b> {
    fn new(analysis_cache: &'a mut AnalysisCache<'b>, level: Level) -> CaseDataVar<'a, 'b> {
        Self {
            analysis_cache,
            level,
        }
    }

    fn run(&mut self) -> AnalysisResult {
        let diagnostics = self.generate_diagnostics();
        Ok(diagnostics)
    }

    // Check if the expression is annotated with `allow(<lint_name>)`
    fn allow(data_var_data: &DataVarData, annotations: &[Annotation]) -> bool {
        data_var_data
            .annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    fn make_diagnostic_message(name: &ClarityName, error: &CaseError) -> String {
        format!("data variable `{name}` is not kebab-case: {error}")
    }

    fn make_diagnostic(
        level: Level,
        expr: &'a SymbolicExpression,
        message: String,
        error: &CaseError,
    ) -> Diagnostic {
        Diagnostic {
            level,
            message,
            spans: vec![expr.span.clone()],
            suggestion: Some(error.suggestion()),
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;
        let data_vars = self.analysis_cache.get_data_vars();

        for (name, data_var_data) in data_vars {
            if Self::allow(data_var_data, annotations) {
                continue;
            }
            let Err(error) = match_kebab_case(strip_unused_suffix(name)) else {
                continue;
            };
            let message = Self::make_diagnostic_message(name, &error);
            let diagnostic =
                Self::make_diagnostic(self.level.clone(), data_var_data.expr, message, &error);
            diagnostics.push(diagnostic);
        }

        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for CaseDataVar<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let mut lint = CaseDataVar::new(analysis_cache, level);
        lint.run()
    }
}

impl Lint for CaseDataVar<'_, '_> {
    fn get_name() -> LintName {
        LintName::CaseDataVar
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::CaseDataVar)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use clarity_types::diagnostic::{Diagnostic, Level};
    use indoc::indoc;

    use super::CaseDataVar;
    use crate::analysis::linter::Lint;
    use crate::analysis::util::CaseError;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet_no_panic(
        snippet: String,
    ) -> Result<(Vec<String>, ExecutionResult), (Vec<String>, Vec<Diagnostic>)> {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(CaseDataVar::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings).formatted_interpretation(
            snippet,
            Some("checker".to_string()),
            false,
            None,
        )
    }

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        run_snippet_no_panic(snippet).expect("Invalid code snippet")
    }

    #[test]
    fn valid_names() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)
            (define-data-var total-supply uint u1000000)
            (define-data-var is-active bool true)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn fail_on_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var total_supply uint u1000000)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "total_supply";
        let expected_message = CaseDataVar::make_diagnostic_message(
            &var_name.into(),
            &CaseError::IllegalCharacter(b'_'),
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_on_upper_case() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var COUNTER uint u0)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "COUNTER";
        let expected_message = CaseDataVar::make_diagnostic_message(
            &var_name.into(),
            &CaseError::IllegalCharacter(b'C'),
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_on_camel_case() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var totalSupply uint u1000000)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "totalSupply";
        let expected_message = CaseDataVar::make_diagnostic_message(
            &var_name.into(),
            &CaseError::IllegalCharacter(b'S'),
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_on_consecutive_hyphens() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var total--supply uint u1000000)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "total--supply";
        let expected_message =
            CaseDataVar::make_diagnostic_message(&var_name.into(), &CaseError::ConsecutiveHyphens);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(case_data_var)]
            (define-data-var total_supply uint u1000000)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_trailing_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var used-nonces_ uint u0)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_single_trailing_hyphen() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter- uint u0)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    // We may allow leading hyphens in Clarity some day
    // This test is written so that it does not need to be modified when that occurs
    #[test]
    fn allow_single_leading_hyphen() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var -counter uint u0)
        ").to_string();

        let res = run_snippet_no_panic(snippet);

        match res {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            Err(..) => {
                // Variable name may be illegal in Clarity, so allow interpretation to fail
            }
        }
    }
}
