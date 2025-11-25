use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct NoopCheckerSettings {
    // TODO
}

impl NoopCheckerSettings {
    fn new() -> Self {
        Self {}
    }
}

pub struct NoopChecker<'a> {
    clarity_version: ClarityVersion,
    /// Map expression ID to a generated diagnostic
    _settings: NoopCheckerSettings,
    diagnostics: HashMap<u64, Vec<Diagnostic>>,
    annotations: &'a Vec<Annotation>,
    /// Clarity diagnostic level
    level: Level,
    active_annotation: Option<usize>,
}

impl<'a> NoopChecker<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
        settings: NoopCheckerSettings,
    ) -> NoopChecker<'a> {
        Self {
            clarity_version,
            _settings: settings,
            level,
            diagnostics: HashMap::new(),
            annotations,
            active_annotation: None,
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        // Traverse the entire AST
        traverse(&mut self, &contract_analysis.expressions);

        // Collect all of the vecs of diagnostics into a vector
        let mut diagnostics: Vec<Vec<Diagnostic>> = self.diagnostics.into_values().collect();
        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a[0].spans[0].cmp(&b[0].spans[0]));
        // Then flatten into one vector
        Ok(diagnostics.into_iter().flatten().collect())
    }

    // Check for annotations that should be attached to the given span
    fn set_active_annotation(&mut self, span: &Span) {
        self.active_annotation = get_index_of_span(self.annotations, span);
    }

    // Check if the expression is annotated with `allow(noop)`
    fn allow(&self) -> bool {
        self.active_annotation
            .map(|idx| Self::match_allow_annotation(&self.annotations[idx]))
            .unwrap_or(false)
    }

    fn add_noop_diagnostic(&mut self, expr: &'a SymbolicExpression, message: String) {
        let diagnostic = Diagnostic {
            level: self.level.clone(),
            message,
            spans: vec![expr.span.clone()],
            suggestion: Some("Remove this expression".to_string()),
        };
        self.diagnostics.insert(expr.id, vec![diagnostic]);
    }

    fn check_bin_op(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) {
        self.set_active_annotation(&expr.span);

        if self.allow() {
            return;
        }

        if operands.len() < 2 {
            self.add_noop_diagnostic(
                expr,
                format!(
                    "`{}` with fewer than 2 operands has no effect",
                    func.get_name()
                ),
            );
        }
    }
}

impl<'a> ASTVisitor<'a> for NoopChecker<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_comparison(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        self.check_bin_op(expr, func, operands);
        true
    }

    fn visit_arithmetic(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        self.check_bin_op(expr, func, operands);
        true
    }

    fn visit_lazy_logical(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        self.check_bin_op(expr, func, operands);
        true
    }
}

impl AnalysisPass for NoopChecker<'_> {
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        _analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = NoopCheckerSettings::new();
        let checker = NoopChecker::new(
            contract_analysis.clarity_version,
            annotations,
            level,
            settings,
        );
        checker.run(contract_analysis)
    }
}

impl Lint for NoopChecker<'_> {
    fn get_name() -> LintName {
        LintName::Noop
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        matches!(annotation.kind, AnnotationKind::Allow(WarningKind::Noop))
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::NoopChecker;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .enable_lint(NoopChecker::get_name(), Level::Warning);

        Session::new(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn single_operand_equals() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (begin
                    (is-eq true)
                    (ok true)
                )
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("`is-eq` with fewer than 2 operands has no effect"));
    }

    #[test]
    fn single_operand_add() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (begin
                    (+ u1)
                    (ok true)
                )
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("`+` with fewer than 2 operands has no effect"));
    }

    #[test]
    fn single_operand_logical() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (begin
                    (and true)
                    (ok true)
                )
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("`and` with fewer than 2 operands has no effect"));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (begin
                    ;; #[allow(noop)]
                    (is-eq true)
                    (ok true)
                )
            )
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
