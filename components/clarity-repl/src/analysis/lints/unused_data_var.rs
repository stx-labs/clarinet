//! Lint to find unused global (`declare-data-var`) variables
//!
//! A diagnostic is generated if:
//!  - The variable is never referenced
//!  - The variable is never modified (suggest using constant instead)

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::SymbolicExpression;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::data_vars::DataVarData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::is_explicitly_unused;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct UnusedDataVarSettings {
    // TODO
}

impl UnusedDataVarSettings {
    fn new() -> Self {
        Self {}
    }
}

pub struct UnusedDataVar<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    _settings: UnusedDataVarSettings,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a, 'b> UnusedDataVar<'a, 'b> {
    fn new(
        analysis_cache: &'a mut AnalysisCache<'b>,
        level: Level,
        settings: UnusedDataVarSettings,
    ) -> UnusedDataVar<'a, 'b> {
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

    // Check if the expression is annotated with `allow(<lint_name>)`
    fn allow(data_var_data: &DataVarData, annotations: &[Annotation]) -> bool {
        data_var_data
            .annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    /// Make diagnostic message and suggestion for variable which is never written to
    fn make_diagnostic_strings_unset(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("data variable `{name}` never modified"),
            Some("Declare using `declare-constant`".to_string()),
        )
    }

    /// Make diagnostic message and suggestion for variable which is never read from
    fn make_diagnostic_strings_unread(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("data variable `{name}` never read"),
            Some("Remove this expression".to_string()),
        )
    }

    /// Make diagnostic message and suggestion for variable which is never used
    fn make_diagnostic_strings_unused(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("data variable `{name}` never used"),
            Some("Remove this expression".to_string()),
        )
    }

    fn make_diagnostic(
        level: Level,
        expr: &'a SymbolicExpression,
        message: String,
        suggestion: Option<String>,
    ) -> Diagnostic {
        Diagnostic {
            level,
            message,
            spans: vec![expr.span.clone()],
            suggestion,
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;
        let data_vars = self.analysis_cache.get_data_vars();

        for (name, data) in data_vars {
            if is_explicitly_unused(name) || Self::allow(data, annotations) {
                continue;
            }
            let (message, suggestion) = match (data.read_from, data.written_to) {
                (true, true) => continue,
                (true, false) => Self::make_diagnostic_strings_unset(name),
                (false, true) => Self::make_diagnostic_strings_unread(name),
                (false, false) => Self::make_diagnostic_strings_unused(name),
            };
            let diagnostic =
                Self::make_diagnostic(self.level.clone(), data.expr, message, suggestion);
            diagnostics.push(diagnostic);
        }

        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for UnusedDataVar<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedDataVarSettings::new();
        let mut lint = UnusedDataVar::new(analysis_cache, level, settings);
        lint.run()
    }
}

impl Lint for UnusedDataVar<'_, '_> {
    fn get_name() -> LintName {
        LintName::UnusedDataVar
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::UnusedDataVar)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use clarity::vm::ClarityName;
    use indoc::indoc;

    use super::UnusedDataVar;
    use crate::analysis::linter::Lint;
    use crate::repl::session::{AnnotatedExecutionResult, Session};
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, AnnotatedExecutionResult) {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedDataVar::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)

            (define-public (increment)
                (ok (var-set counter (+ (var-get counter) u1))))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn not_set() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)

            (define-public (read-counter)
                (ok (var-get counter)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "counter";
        let (expected_message, _) =
            UnusedDataVar::make_diagnostic_strings_unset(&ClarityName::try_from(var_name).unwrap());

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn not_read() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)

            (define-public (set (val uint))
                (ok (var-set counter val)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "counter";
        let (expected_message, _) = UnusedDataVar::make_diagnostic_strings_unread(
            &ClarityName::try_from(var_name).unwrap(),
        );

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn not_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)

            (define-public (read-counter)
                (ok u5))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "counter";
        let (expected_message, _) = UnusedDataVar::make_diagnostic_strings_unused(
            &ClarityName::try_from(var_name).unwrap(),
        );

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_data_var)]
            (define-data-var counter uint u0)

            (define-public (read-counter)
                (ok u5))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn allow_with_naming_convention() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter_ uint u0)

            (define-public (read-counter)
                (ok u5))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 0);
    }
}
