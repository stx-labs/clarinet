//! Lint to check case of bindings (`let` bindings and function args)
//!
//! By default, this enforces kebab-case

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::SymbolicExpression;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::bindings::BindingData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::{match_kebab_case, strip_unused_suffix, CaseError};
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct CaseBindingSettings {
    // TODO
}

impl CaseBindingSettings {
    fn new() -> Self {
        Self {}
    }
}

pub struct CaseBinding<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    _settings: CaseBindingSettings,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a, 'b> CaseBinding<'a, 'b> {
    fn new(
        analysis_cache: &'a mut AnalysisCache<'b>,
        level: Level,
        settings: CaseBindingSettings,
    ) -> CaseBinding<'a, 'b> {
        Self {
            analysis_cache,
            _settings: settings,
            level,
        }
    }

    fn run(&mut self) -> AnalysisResult {
        let diagnostics = self.generate_diagnostics();

        Ok(diagnostics)
    }

    fn allow(binding_data: &BindingData, annotations: &[Annotation]) -> bool {
        binding_data
            .annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    fn make_diagnostic_message(name: &ClarityName, error: &CaseError) -> String {
        format!("binding `{name}` is not kebab-case: {error}")
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
        let bindings = self.analysis_cache.get_bindings();

        for (binding, binding_data) in bindings {
            if Self::allow(binding_data, annotations) {
                continue;
            }
            let Err(error) = match_kebab_case(strip_unused_suffix(binding.name)) else {
                continue;
            };
            let message = Self::make_diagnostic_message(binding.name, &error);
            let diagnostic =
                Self::make_diagnostic(self.level.clone(), binding_data.expr, message, &error);
            diagnostics.push(diagnostic);
        }

        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for CaseBinding<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = CaseBindingSettings::new();
        let mut lint = CaseBinding::new(analysis_cache, level, settings);
        lint.run()
    }
}

impl Lint for CaseBinding<'_, '_> {
    fn get_name() -> LintName {
        LintName::CaseBinding
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::CaseBinding)
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

    use super::CaseBinding;
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
            .enable_lint(CaseBinding::get_name(), Level::Warning);

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
    fn valid_let_binding_names() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-read-only (test)
                (let ((my-var u1)
                      (another-binding u2)
                      (x u3))
                    (+ my-var another-binding x)))
        "#).to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn valid_function_arg_names() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-read-only (my-func (amount uint) (sender-address principal))
                (ok u1))
        "#).to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn fail_let_binding_with_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-read-only (test)
                (let ((my_var u1))
                    my_var))
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        let binding_name = "my_var";
        let expected_message = CaseBinding::make_diagnostic_message(
            &binding_name.into(),
            &CaseError::IllegalCharacter(b'_'),
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(binding_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_function_arg_with_uppercase() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-read-only (my-func (Amount uint))
                (ok Amount))
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        let arg_name = "Amount";
        let expected_message = CaseBinding::make_diagnostic_message(
            &arg_name.into(),
            &CaseError::IllegalCharacter(b'A'),
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(arg_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_let_binding_with_consecutive_hyphens() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-read-only (test)
                (let ((my--var u1))
                    my--var))
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        let binding_name = "my--var";
        let expected_message = CaseBinding::make_diagnostic_message(
            &binding_name.into(),
            &CaseError::ConsecutiveHyphens,
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(binding_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test)
                ;; #[allow(case_binding)]
                (let ((my_var u1))
                    my_var))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_trailing_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-read-only (test)
                (let ((my-var_ u1))
                    u1))
        "#).to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn multiple_violations() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-read-only (test)
                (let ((my_var u1)
                      (another_bad u2))
                    (+ my_var another_bad)))
        "#).to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 2);
    }
}
