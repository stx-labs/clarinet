//! Lint to find unused constants
//!
//! A constant is considered unused if declared but never referenced

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::SymbolicExpression;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::constants::ConstantData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::is_explicitly_unused;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct UnusedConstSettings {
    // TODO
}

impl UnusedConstSettings {
    fn new() -> Self {
        Self {}
    }
}

pub struct UnusedConst<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    _settings: UnusedConstSettings,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a, 'b> UnusedConst<'a, 'b> {
    fn new(
        analysis_cache: &'a mut AnalysisCache<'b>,
        level: Level,
        settings: UnusedConstSettings,
    ) -> UnusedConst<'a, 'b> {
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
        format!("constant `{name}` never used")
    }

    fn make_diagnostic(level: Level, expr: &'a SymbolicExpression, message: String) -> Diagnostic {
        Diagnostic {
            level,
            message,
            spans: vec![expr.span.clone()],
            suggestion: Some("Remove this expression".to_string()),
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;
        let constants = self.analysis_cache.get_constants();

        for (name, const_data) in constants {
            if const_data.used || Self::allow(const_data, annotations) || is_explicitly_unused(name)
            {
                continue;
            }
            let message = Self::make_diagnostic_message(name);
            let diagnostic = Self::make_diagnostic(self.level.clone(), const_data.expr, message);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for UnusedConst<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedConstSettings::new();
        let mut lint = UnusedConst::new(analysis_cache, level, settings);
        lint.run()
    }
}

impl Lint for UnusedConst<'_, '_> {
    fn get_name() -> LintName {
        LintName::UnusedConst
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        matches!(
            annotation.kind,
            AnnotationKind::Allow(WarningKind::UnusedConst)
        )
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::UnusedConst;
    use crate::analysis::linter::{Lint, LintLevel};
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .set_lint_level(UnusedConst::get_name(), LintLevel::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant MINUTES_PER_HOUR u60)

            (define-read-only (hours-to-minutes (hours uint))
                (* hours MINUTES_PER_HOUR))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_before_declaration() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (hours-to-minutes (hours uint))
                (* hours MINUTES_PER_HOUR))

            (define-constant MINUTES_PER_HOUR u60)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn not_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant MINUTES_PER_HOUR u60)

            (define-read-only (hours-to-minutes (hours uint))
                (* hours u60))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let const_name = "MINUTES_PER_HOUR";
        let expected_message = UnusedConst::make_diagnostic_message(&const_name.into());

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
            (define-constant MINUTES_PER_HOUR u60)

            (define-read-only (hours-to-minutes (hours uint))
                (* hours u60))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_with_naming_convention() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant MINUTES_PER_HOUR_ u60)

            (define-read-only (hours-to-minutes (hours uint))
                (* hours u60))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
