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
use crate::analysis::{self, util, AnalysisPass, AnalysisResult, LintName};

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

    fn make_diagnostic_message(name: &ClarityName) -> String {
        format!("constant `{name}` should be SCREAMING_SNAKE_CASE")
    }

    fn make_diagnostic(level: Level, expr: &'a SymbolicExpression, message: String) -> Diagnostic {
        Diagnostic {
            level,
            message,
            spans: vec![expr.span.clone()],
            suggestion: Some("Rename the constant".to_string()),
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;
        let constants = self.analysis_cache.get_constants();

        for (const_name, const_data) in constants {
            if Self::allow(const_data, annotations) || util::is_screaming_snake_case(const_name) {
                continue;
            }
            let message = Self::make_diagnostic_message(const_name);
            let diagnostic = Self::make_diagnostic(self.level.clone(), const_data.expr, message);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
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
        matches!(
            annotation.kind,
            AnnotationKind::Allow(WarningKind::CaseConst)
        )
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::CaseConst;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .enable_lint(CaseConst::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn valid_names() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-constant SECONDS_PER_MINUTE_ u60)
            (define-constant MINUTES_PER_HOUR u60)
            (define-constant HOURS___PER___DAY u24)

            (define-constant PI u3)
            (define-constant GREETING "Hello World!")
        "#).to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn fail_on_hyphen() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant MINUTES-PER-HOUR u60)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let const_name = "MINUTES-PER-HOUR";
        let expected_message = CaseConst::make_diagnostic_message(&const_name.into());

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
        let expected_message = CaseConst::make_diagnostic_message(&const_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(const_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_const)]
            (define-constant minutes-per-hour u60)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
