//! Lint to suggest flattening nested calls to variadic functions.
//!
//! For example, `(+ a (+ b c))` can be simplified to `(+ a b c)`, and in
//! Clarity 6+, `(concat a (concat b c))` can be simplified to
//! `(concat a b c)`.

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct FlattenNestedVariadic<'a> {
    clarity_version: ClarityVersion,
    diagnostics: Vec<Diagnostic>,
    annotations: &'a [Annotation],
    level: Level,
    active_annotation: Option<usize>,
}

impl<'a> FlattenNestedVariadic<'a> {
    fn new(clarity_version: ClarityVersion, annotations: &'a [Annotation], level: Level) -> Self {
        Self {
            clarity_version,
            level,
            diagnostics: Vec::new(),
            annotations,
            active_annotation: None,
        }
    }

    fn run(mut self, expressions: &'a [SymbolicExpression]) -> AnalysisResult {
        traverse(&mut self, expressions);
        Ok(self.diagnostics)
    }

    fn set_active_annotation(&mut self, span: &Span) {
        self.active_annotation = get_index_of_span(self.annotations, span);
    }

    fn allow(&self) -> bool {
        self.active_annotation
            .map(|idx| Self::match_allow_annotation(&self.annotations[idx]))
            .unwrap_or(false)
    }

    /// Check whether `operand` is a call to `func_name`.
    fn is_nested_call(operand: &SymbolicExpression, func_name: &str) -> bool {
        let Some(list) = operand.match_list() else {
            return false;
        };
        let Some(first) = list.first() else {
            return false;
        };
        let Some(atom) = first.match_atom() else {
            return false;
        };
        atom.as_str() == func_name
    }

    fn check_operands(
        &mut self,
        expr: &'a SymbolicExpression,
        func_name: &str,
        operands: &'a [SymbolicExpression],
    ) {
        self.set_active_annotation(&expr.span);
        if self.allow() {
            return;
        }

        for operand in operands {
            if Self::is_nested_call(operand, func_name) {
                self.diagnostics.push(Diagnostic {
                    level: self.level.clone(),
                    message: format!("nested `{func_name}` can be flattened into a single call"),
                    spans: vec![expr.span.clone()],
                    suggestion: Some(format!(
                        "Merge the inner `{func_name}` arguments into the outer call"
                    )),
                });
                return;
            }
        }
    }
}

impl<'a> ASTVisitor<'a> for FlattenNestedVariadic<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_arithmetic(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        // Only lint associative operators where flattening preserves semantics.
        // e.g. (- a (- b c)) != (- a b c), so we skip Subtract, Divide, etc.
        if matches!(func, NativeFunctions::Add | NativeFunctions::Multiply) {
            let func_name = func.get_name();
            self.check_operands(expr, &func_name, operands);
        }
        true
    }

    fn visit_concat(
        &mut self,
        expr: &'a SymbolicExpression,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        if self.clarity_version >= ClarityVersion::Clarity6 {
            self.check_operands(expr, "concat", operands);
        }
        true
    }
}

impl AnalysisPass for FlattenNestedVariadic<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let checker = FlattenNestedVariadic::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
        );
        checker.run(&analysis_cache.contract_analysis.expressions)
    }
}

impl Lint for FlattenNestedVariadic<'_> {
    fn get_name() -> LintName {
        LintName::FlattenNestedVariadic
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::FlattenNestedVariadic)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::types::StacksEpochId;
    use clarity::vm::diagnostic::Level;
    use indoc::indoc;

    use super::FlattenNestedVariadic;
    use crate::analysis::linter::Lint;
    use crate::repl::session::{AnnotatedExecutionResult, Session};
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, AnnotatedExecutionResult) {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(FlattenNestedVariadic::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    fn run_snippet_epoch40(snippet: String) -> (Vec<String>, AnnotatedExecutionResult) {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(FlattenNestedVariadic::get_name(), Level::Warning);

        let mut session = Session::new_without_boot_contracts(settings);
        session.update_epoch(StacksEpochId::Epoch40);
        session
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn warn_nested_add() {
        let snippet = indoc!(
            "
            (define-private (test)
                (+ u1 (+ u2 u3))
            )
        "
        )
        .to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains("nested `+`"));
    }

    #[test]
    fn warn_nested_multiply() {
        let snippet = indoc!(
            "
            (define-private (test)
                (* u2 (* u3 u4))
            )
        "
        )
        .to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains("nested `*`"));
    }

    #[test]
    fn no_warn_flat_add() {
        let snippet = indoc!(
            "
            (define-private (test)
                (+ u1 u2 u3)
            )
        "
        )
        .to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_nested_subtract() {
        let snippet = indoc!(
            "
            (define-private (test)
                (- u10 (- u5 u2))
            )
        "
        )
        .to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_different_functions() {
        let snippet = indoc!(
            "
            (define-private (test)
                (+ u1 (* u2 u3))
            )
        "
        )
        .to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn warn_nested_concat_clarity6() {
        let snippet = indoc!(
            r#"
            (define-private (test)
                (concat "hello" (concat "world" "!"))
            )
        "#
        )
        .to_string();

        let (_, result) = run_snippet_epoch40(snippet);
        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains("nested `concat`"));
    }

    #[test]
    fn no_warn_nested_concat_pre_clarity6() {
        let snippet = indoc!(
            r#"
            (define-private (test)
                (concat "hello" (concat "world" "!"))
            )
        "#
        )
        .to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_flat_concat_clarity6() {
        let snippet = indoc!(
            r#"
            (define-private (test)
                (concat "hello" "world" "!")
            )
        "#
        )
        .to_string();

        let (_, result) = run_snippet_epoch40(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn allow_with_annotation() {
        let snippet = indoc!(
            "
            (define-private (test)
                ;; #[allow(flatten_nested_variadic)]
                (+ u1 (+ u2 u3))
            )
        "
        )
        .to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }
}
