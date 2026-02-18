//! Lint to find uses of `unwrap-panic` and `unwrap-err-panic`

use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct PanicChecker<'a> {
    clarity_version: ClarityVersion,
    diagnostics: HashMap<u64, Vec<Diagnostic>>,
    annotations: &'a Vec<Annotation>,
    level: Level,
    active_annotation: Option<usize>,
}

impl<'a> PanicChecker<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
    ) -> Self {
        Self {
            clarity_version,
            level,
            diagnostics: HashMap::new(),
            annotations,
            active_annotation: None,
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        traverse(&mut self, &contract_analysis.expressions);

        let mut diagnostics: Vec<Vec<Diagnostic>> = self.diagnostics.into_values().collect();
        diagnostics.sort_by(|a, b| a[0].spans[0].cmp(&b[0].spans[0]));
        Ok(diagnostics.into_iter().flatten().collect())
    }

    fn set_active_annotation(&mut self, span: &Span) {
        self.active_annotation = get_index_of_span(self.annotations, span);
    }

    fn allow(&self) -> bool {
        self.active_annotation
            .map(|idx| Self::match_allow_annotation(&self.annotations[idx]))
            .unwrap_or(false)
    }

    fn add_diagnostic(
        &mut self,
        expr: &'a SymbolicExpression,
        message: String,
        suggestion: Option<String>,
    ) {
        let diagnostic = Diagnostic {
            level: self.level.clone(),
            message,
            spans: vec![expr.span.clone()],
            suggestion,
        };
        self.diagnostics.insert(expr.id, vec![diagnostic]);
    }

    fn add_unwrap_panic_diagnostic(&mut self, expr: &'a SymbolicExpression) {
        let message =
            "`unwrap-panic` will abort the transaction on failure, making it difficult to debug"
                .to_owned();
        let suggestion =
            Some("Use `try!` or `unwrap!` instead to return the error to the caller".to_owned());
        self.add_diagnostic(expr, message, suggestion);
    }

    fn add_unwrap_err_panic_diagnostic(&mut self, expr: &'a SymbolicExpression) {
        let message = "`unwrap-err-panic` will abort the transaction on failure, making it difficult to debug".to_owned();
        let suggestion = Some("Use `unwrap-err!` instead".to_owned());
        self.add_diagnostic(expr, message, suggestion);
    }
}

impl<'a> ASTVisitor<'a> for PanicChecker<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_unwrap_panic(
        &mut self,
        expr: &'a SymbolicExpression,
        _input: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if !self.allow() {
            self.add_unwrap_panic_diagnostic(expr);
        }
        true
    }

    fn visit_unwrap_err_panic(
        &mut self,
        expr: &'a SymbolicExpression,
        _input: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if !self.allow() {
            self.add_unwrap_err_panic_diagnostic(expr);
        }
        true
    }
}

impl AnalysisPass for PanicChecker<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let checker = PanicChecker::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
        );
        checker.run(analysis_cache.contract_analysis)
    }
}

impl Lint for PanicChecker<'_> {
    fn get_name() -> LintName {
        LintName::Panic
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => warning_kinds.contains(&WarningKind::Panic),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::PanicChecker;
    use clarity_types::diagnostic::Level;

    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .set_lint_level(PanicChecker::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn warn_unwrap_panic() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (ok (unwrap-panic (some u1)))
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("`unwrap-panic`"));
    }

    #[test]
    fn warn_unwrap_err_panic() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (ok (unwrap-err-panic (err u1)))
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("`unwrap-err-panic`"));
    }

    #[test]
    fn no_warn_on_unwrap() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (ok (unwrap! (some u1) (err u1)))
            )
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (ok
                    ;; #[allow(panic)]
                    (unwrap-panic (some u1))
                )
            )
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
