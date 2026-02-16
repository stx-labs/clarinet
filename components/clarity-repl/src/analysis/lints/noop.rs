//! Lint to find expressions that have no effect (noops)
//! 
//! This lint checks for several logical and arithmetic operations that are considered "noop"s,
//! grouped together in a single lint in order to minimize AST traversals
//! 
//! Here is a full list by category, and what they should be replaced by:
//! 
//! - Operations that always return `true` or `false`
//!   - Comparison ops
//!    - `(is-eq x)` -> `true`
//!    - `(and ... false)` -> `false`
//!    - `(or ... true)` -> `true`
//! - Operations that always return one of the operands
//!   - Comparison ops
//!    - `(is-eq x true)` -> `x`
//!    - `(is-eq x false)` -> `(not x)`
//!   - Logical ops
//!    - `(not (not x))` -> `x`
//!    - `(and x)` -> `x`
//!    - `(or x)` -> `x`
//!   - Arithmetic ops
//!    - `(+ x)`, `(+ x u0)`, `(+ x 1 -1)`, etc. -> `x`
//!    - `(+ y)`, `(- x u0)`, `(- x 1 -1)`, etc. -> `x`
//!    - `(* y)`, `(* x u1)`, `(* x u1 u1`), etc. -> `x`
//!    - `(/ y)`, `(/ x u1)`, `(/ x u1 u1`), etc. -> `x`

use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::{Span, SymbolicExpressionType};
use clarity::vm::{ClarityVersion, SymbolicExpression};

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::cache::AnalysisCache;
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

    /// Some arithmetic functions take a single operand
    /// Do not generate warnings for these!
    fn is_single_op_arithmetic(func: NativeFunctions) -> bool {
        matches!(func, NativeFunctions::Sqrti | NativeFunctions::Log2)
    }

    /// Check if the expression is a boolean literal and return its value.
    /// In Clarity, `true` and `false` are parsed as `Atom("true")` / `Atom("false")`
    /// (reserved variable names), not as `AtomValue(Value::Bool(...))`.
    fn as_bool_literal(expr: &SymbolicExpression) -> Option<bool> {
        match &expr.expr {
            SymbolicExpressionType::Atom(name) if name.as_str() == "true" => Some(true),
            SymbolicExpressionType::Atom(name) if name.as_str() == "false" => Some(false),
            _ => None,
        }
    }

    /// Check if the expression is a `(not ...)` call
    fn is_not_call(expr: &SymbolicExpression) -> bool {
        if let SymbolicExpressionType::List(exprs) = &expr.expr {
            exprs
                .first()
                .and_then(|e| e.match_atom())
                .is_some_and(|name| name.as_str() == "not")
        } else {
            false
        }
    }

    /// Check for redundant boolean comparisons: `(is-eq x true)` or `(is-eq x false)`
    fn check_bool_comparison(
        &mut self,
        expr: &'a SymbolicExpression,
        operands: &'a [SymbolicExpression],
    ) {
        if operands.len() != 2 {
            return;
        }

        let bool_val = if let Some(b) = Self::as_bool_literal(&operands[0]) {
            Some(b)
        } else {
            Self::as_bool_literal(&operands[1])
        };

        let Some(b) = bool_val else { return };

        if b {
            self.add_noop_diagnostic(expr, "comparing with `true` is unnecessary".to_string());
        } else {
            self.add_noop_diagnostic(
                expr,
                "comparing with `false` is unnecessary, use `not` instead".to_string(),
            );
        }
    }

    /// Check for double negation: `(not (not x))`
    fn check_double_negation(
        &mut self,
        expr: &'a SymbolicExpression,
        operands: &'a [SymbolicExpression],
    ) {
        if operands.len() == 1 && Self::is_not_call(&operands[0]) {
            self.add_noop_diagnostic(expr, "double negation is unnecessary".to_string());
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
        if matches!(func, NativeFunctions::Equals) && !self.allow() {
            self.check_bool_comparison(expr, operands);
        }
        true
    }

    fn visit_arithmetic(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        if !Self::is_single_op_arithmetic(func) {
            self.check_bin_op(expr, func, operands);
        }
        true
    }

    fn visit_logical(
        &mut self,
        expr: &'a SymbolicExpression,
        _func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if !self.allow() {
            self.check_double_negation(expr, operands);
        }
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
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = NoopCheckerSettings::new();
        let checker = NoopChecker::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
            settings,
        );
        checker.run(analysis_cache.contract_analysis)
    }
}

impl Lint for NoopChecker<'_> {
    fn get_name() -> LintName {
        LintName::Noop
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => warning_kinds.contains(&WarningKind::Noop),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::NoopChecker;
    use crate::analysis::linter::{Lint, LintLevel};
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .set_lint_level(NoopChecker::get_name(), LintLevel::Warning);

        Session::new_without_boot_contracts(settings)
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

    /// Shoult NOT warn on `log2`, whcih expects single arg
    #[test]
    fn single_operand_log2() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (log4 (x uint))
              (/ (log2 x) u2))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// Shoult NOT warn on `sqrti`, whcih expects single arg
    #[test]
    fn single_operand_sqrti() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (root4 (x uint))
              (sqrti (sqrti x)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// Shoult NOT warn on `not`, whcih expects single arg
    #[test]
    fn single_operand_not() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (invert (b bool))
              (not b))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// Should NOT warn when there are 2+ operands
    #[test]
    fn valid_multi_operand() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (sum (a uint) (b uint))
                (+ a b))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn single_operand_or() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (begin
                    (or true)
                    (ok true)
                )
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("`or` with fewer than 2 operands has no effect"));
    }

    #[test]
    fn single_operand_subtract() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (begin
                    (- u1)
                    (ok true)
                )
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("`-` with fewer than 2 operands has no effect"));
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

    #[test]
    fn is_eq_with_true() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (is-eq x true))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("comparing with `true` is unnecessary"));
    }

    #[test]
    fn is_eq_with_true_reversed() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (is-eq true x))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("comparing with `true` is unnecessary"));
    }

    #[test]
    fn is_eq_with_false() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (is-eq x false))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("comparing with `false` is unnecessary, use `not` instead"));
    }

    #[test]
    fn double_negation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (not (not x)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("double negation is unnecessary"));
    }

    /// Should NOT warn for valid `is-eq` with non-boolean operands
    #[test]
    fn is_eq_non_bool() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (is-eq x u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// Should NOT warn for single `not`
    #[test]
    fn single_not() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (not x))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_bool_comparison_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (begin
                    ;; #[allow(noop)]
                    (is-eq x true)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_double_negation_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (begin
                    ;; #[allow(noop)]
                    (not (not x))))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
