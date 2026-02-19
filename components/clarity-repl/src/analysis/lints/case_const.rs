//! Lint to check case of constant names (declared using `declare-constant`)
//!
//! By default, this enforces SCREAMING_SNAKE_CASE

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::SymbolicExpression;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::constants::ConstantData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::{match_screaming_snake_case, CaseError};
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct UnusedConstSettings {
    // TODO
}

impl UnusedConstSettings {
    fn new() -> Self {
        Self {}
    }
}

pub struct CaseConst<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    _settings: UnusedConstSettings,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a, 'b> CaseConst<'a, 'b> {
    fn new(
        analysis_cache: &'a mut AnalysisCache<'b>,
        level: Level,
        settings: UnusedConstSettings,
    ) -> CaseConst<'a, 'b> {
        Self {
            analysis_cache,
            _settings: settings,
            level,
        }
    }

    fn run(&mut self) -> AnalysisResult {
        // Process hashmap of unused constants and generate diagnostics
        let diagnostics = self.generate_diagnostics();

        Ok(diagnostics)
    }

    // Check if the expression is annotated with `allow(<lint_name>)`
    fn allow(const_data: &ConstantData, annotations: &[Annotation]) -> bool {
        const_data
            .annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    fn make_diagnostic_message(name: &ClarityName, error: &CaseError) -> String {
        format!("constant `{name}` is not SCREAMING_SNAKE_CASE: {error:?}")
    }

    fn make_diagnostic(
        level: Level,
        expr: &'a SymbolicExpression,
        message: String,
        error: &CaseError,
    ) -> Diagnostic {
        let suggestion = match error {
            CaseError::Empty => "Give the constant a name".to_owned(), // Shouldn't happen
            CaseError::IllegalCharacter(b) => {
                format!("Remove the illegal character '{c}'", c = char::from(*b))
            }
            CaseError::ConsecutiveUnderscores => "Remove the consecutive underscores".to_owned(),
        };
        Diagnostic {
            level,
            message,
            spans: vec![expr.span.clone()],
            suggestion: Some(suggestion),
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;
        let constants = self.analysis_cache.get_constants();

        for (const_name, const_data) in constants {
            if Self::allow(const_data, annotations) {
                continue;
            }
            let Err(error) = match_screaming_snake_case(const_name.as_str()) else {
                continue;
            };
            let message = Self::make_diagnostic_message(const_name, &error);
            let diagnostic =
                Self::make_diagnostic(self.level.clone(), const_data.expr, message, &error);
            diagnostics.push(diagnostic);
        }

        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for CaseConst<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedConstSettings::new();
        let mut lint = CaseConst::new(analysis_cache, level, settings);
        lint.run()
    }
}

impl Lint for CaseConst<'_, '_> {
    fn get_name() -> LintName {
        LintName::CaseConst
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => warning_kinds.contains(&WarningKind::CaseConst),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use clarity_types::diagnostic::{Diagnostic, Level};
    use indoc::indoc;

    use super::CaseConst;
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
            .enable_lint(CaseConst::get_name(), Level::Warning);

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
        let snippet = indoc!(r#"
            (define-constant SECONDS_PER_MINUTE u60)
            (define-constant MINUTES_PER_HOUR u60)
            (define-constant HOURS_PER_DAY u24)

            (define-constant PI u3)
            (define-constant G_R_E_E_T_I_N_G "Hello World!")
        "#).to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_single_trailing_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-constant SECONDS_PER_MINUTE_ u60)
        "#).to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    // We plan to allow leading underscores in Clarity some day
    // This test is written so that it does not need to be modified when that occurs
    #[test]
    fn allow_single_leading_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-constant _SECONDS_PER_MINUTE u60)
        "#).to_string();

        let res = run_snippet_no_panic(snippet);

        match res {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            Err(..) => {
                // Variable name may be illegal in Clarity, so allow interpretaion to fail
            }
        }
    }

    #[test]
    fn fail_on_multiple_trailing_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-constant SECONDS_PER_MINUTE__ u60)
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        let const_name = "SECONDS_PER_MINUTE__";
        let expected_message = CaseConst::make_diagnostic_message(
            &const_name.into(),
            &CaseError::ConsecutiveUnderscores,
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(const_name));
        assert!(output[0].contains(&expected_message));
    }

    // We plan to allow leading underscores in Clarity some day
    // This test is written so that it does not need to be modified when that occurs
    #[test]
    fn fail_on_multiple_leading_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-constant __SECONDS_PER_MINUTE u60)
        "#).to_string();

        let res = run_snippet_no_panic(snippet);

        match res {
            Ok((output, result)) => {
                let const_name = "__SECONDS_PER_MINUTE";
                let expected_message = CaseConst::make_diagnostic_message(
                    &const_name.into(),
                    &CaseError::ConsecutiveUnderscores,
                );

                assert_eq!(result.diagnostics.len(), 1);
                assert!(output[0].contains("warning:"));
                assert!(output[0].contains(const_name));
                assert!(output[0].contains(&expected_message));
            }
            Err(..) => {
                // Variable name may be illegal in Clarity, so allow interpretaion to fail
            }
        }
    }

    #[test]
    fn fail_on_hyphen() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant MINUTES-PER-HOUR u60)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let const_name = "MINUTES-PER-HOUR";
        let expected_message = CaseConst::make_diagnostic_message(
            &const_name.into(),
            &CaseError::IllegalCharacter(b'-'),
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(const_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_on_lower_case() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant minutes_per_hour u60)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let const_name = "minutes_per_hour";
        let expected_message = CaseConst::make_diagnostic_message(
            &const_name.into(),
            &CaseError::IllegalCharacter(b'm'),
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(const_name));
        assert!(output[0].contains(&expected_message));
    }

    // Skip this test because it causes a panic in the interpreter
    #[ignore]
    #[test]
    fn fail_on_consecutive_underscores() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant MINUTES__PER__HOUR u60)
        ").to_string();

        let res = run_snippet_no_panic(snippet);

        match res {
            Ok((output, result)) => {
                let const_name = "MINUTES__PER__HOUR ";
                let expected_message = CaseConst::make_diagnostic_message(
                    &const_name.into(),
                    &CaseError::ConsecutiveUnderscores,
                );

                assert_eq!(result.diagnostics.len(), 1);
                assert!(output[0].contains("warning:"));
                assert!(output[0].contains(const_name));
                assert!(output[0].contains(&expected_message));
            }
            Err(..) => {
                // Variable name may be illegal in Clarity, so allow interpretaion to fail
            }
        }
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(case_const)]
            (define-constant minutes-per-hour u60)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
