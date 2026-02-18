//! Lint to find expressions that have no effect (noops)
//!
//! This lint checks for several logical and arithmetic operations that are considered "noop"s,
//! grouped together in a single lint in order to minimize AST traversals
//!
//! Here is a full list by category, and what they should be replaced by:
//!
//! - Operations that always return `true`, `false`, or `0`
//!   - Comparison ops
//!     - `(is-eq x)` -> `true`
//!   - Logical ops
//!     - `(and ... false)` -> `false`
//!     - `(or ... true)` -> `true`
//!   - Arithmetic ops
//!     - `(* ... 0)` -> `0`
//!     - `(/ 0 ...)` -> `0`
//!     - `(/ ... 0)` -> Runtime error
//! - Operations that always return one of the operands
//!   - Comparison ops
//!     - `(is-eq x true)` -> `x`
//!     - `(is-eq x false)` -> `(not x)`
//!   - Logical ops
//!     - `(not (not x))` -> `x`
//!     - `(and x)`, `(and x true)`, etc. -> `x`
//!     - `(or x)`, `(or x false)`, etc. -> `x`
//!   - Arithmetic ops
//!     - `(+ x)`, `(+ x u0)`, `(+ x 1 -1)`, etc. -> `x`
//!     - `(- x)`, `(- x u0)`, `(- x 1 -1)`, etc. -> `x`
//!     - `(* x)`, `(* x u1)`, `(* x u1 u1)`, etc. -> `x`
//!     - `(/ x)`, `(/ x u1)`, `(/ x u1 u1)`, etc. -> `x`

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::{Span, SymbolicExpressionType};
use clarity::vm::variables::NativeVariables;
use clarity::vm::{ClarityVersion, SymbolicExpression, Value};

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

/// Result of separating operands into constants and non-constants,
/// accumulating the constants using a checked arithmetic operation.
struct SplitOperands<'a> {
    non_constants: Vec<&'a SymbolicExpression>,
    uint_acc: Option<u128>,
    int_acc: Option<i128>,
}

impl<'a> SplitOperands<'a> {
    fn accumulate(
        operands: &'a [SymbolicExpression],
        uint_init: u128,
        int_init: i128,
        uint_op: fn(u128, u128) -> Option<u128>,
        int_op: fn(i128, i128) -> Option<i128>,
    ) -> Self {
        let mut non_constants = Vec::new();
        let mut uint_acc = Some(uint_init);
        let mut int_acc = Some(int_init);

        for op in operands {
            match op.match_literal_value() {
                Some(Value::UInt(n)) => {
                    uint_acc = uint_acc.and_then(|a| uint_op(a, *n));
                    int_acc = None;
                }
                Some(Value::Int(n)) => {
                    int_acc = int_acc.and_then(|a| int_op(a, *n));
                    uint_acc = None;
                }
                _ => non_constants.push(op),
            }
        }

        Self {
            non_constants,
            uint_acc,
            int_acc,
        }
    }

    fn sum_of(operands: &'a [SymbolicExpression]) -> Self {
        Self::accumulate(operands, 0, 0, u128::checked_add, i128::checked_add)
    }

    fn product_of(operands: &'a [SymbolicExpression]) -> Self {
        Self::accumulate(operands, 1, 1, u128::checked_mul, i128::checked_mul)
    }

    fn has_constants(&self, total: usize) -> bool {
        self.non_constants.len() < total
    }

    fn is_zero(&self) -> bool {
        self.uint_acc == Some(0) || self.int_acc == Some(0)
    }

    fn is_one(&self) -> bool {
        self.uint_acc == Some(1) || self.int_acc == Some(1)
    }

    /// Returns "u0" or "0" depending on the accumulated type.
    fn zero_str(&self) -> &'static str {
        if self.uint_acc.is_some() {
            "u0"
        } else {
            "0"
        }
    }
}

pub struct NoopChecker<'a> {
    clarity_version: ClarityVersion,
    _settings: NoopCheckerSettings,
    diagnostics: Vec<Diagnostic>,
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
            diagnostics: Vec::new(),
            annotations,
            active_annotation: None,
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        // Traverse the entire AST
        traverse(&mut self, &contract_analysis.expressions);

        let mut diagnostics = self.diagnostics;
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        Ok(diagnostics)
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

    fn add_diagnostic(
        &mut self,
        expr: &'a SymbolicExpression,
        message: String,
        suggestion: String,
    ) {
        let diagnostic = Diagnostic {
            level: self.level.clone(),
            message,
            spans: vec![expr.span.clone()],
            suggestion: Some(suggestion),
        };
        self.diagnostics.push(diagnostic);
    }

    /// Check if the expression is allowed by annotation, combining set_active_annotation + allow
    fn check_allowed(&mut self, span: &Span) -> bool {
        self.set_active_annotation(span);
        self.allow()
    }

    /// Format a SymbolicExpression back to Clarity source for use in suggestion text
    fn format_source(expr: &SymbolicExpression) -> String {
        match &expr.expr {
            SymbolicExpressionType::Atom(name) => name.to_string(),
            SymbolicExpressionType::LiteralValue(v) | SymbolicExpressionType::AtomValue(v) => {
                v.to_string()
            }
            SymbolicExpressionType::List(exprs) => {
                let inner: Vec<String> = exprs.iter().map(Self::format_source).collect();
                format!("({})", inner.join(" "))
            }
            _ => "...".to_string(),
        }
    }

    /// Format a replacement suggestion from remaining operands after removing identity elements.
    fn format_replacement_suggestion(
        func: impl std::fmt::Display,
        remaining: &[&SymbolicExpression],
    ) -> String {
        if remaining.len() == 1 {
            format!("Replace with `{}`", Self::format_source(remaining[0]))
        } else {
            let parts: Vec<String> = remaining.iter().map(|op| Self::format_source(op)).collect();
            format!("Replace with `({func} {})`", parts.join(" "))
        }
    }

    /// Extract an `int` literal value
    fn as_int_literal(expr: &SymbolicExpression) -> Option<i128> {
        if let Some(Value::Int(n)) = expr.match_literal_value() {
            Some(*n)
        } else {
            None
        }
    }

    /// Extract a `uint` literal value
    fn as_uint_literal(expr: &SymbolicExpression) -> Option<u128> {
        if let Some(Value::UInt(n)) = expr.match_literal_value() {
            Some(*n)
        } else {
            None
        }
    }

    /// Some arithmetic functions take a single operand
    /// Do not generate warnings for these!
    fn is_single_op_arithmetic(func: NativeFunctions) -> bool {
        matches!(func, NativeFunctions::Sqrti | NativeFunctions::Log2)
    }

    /// Check if the expression is a boolean literal and return its value.
    fn as_bool_literal(expr: &SymbolicExpression) -> Option<bool> {
        match &expr.expr {
            SymbolicExpressionType::Atom(name) => match NativeVariables::lookup_by_name(name) {
                Some(NativeVariables::NativeTrue) => Some(true),
                Some(NativeVariables::NativeFalse) => Some(false),
                _ => None,
            },
            _ => None,
        }
    }

    /// Check if the expression is a `(not ...)` call
    fn is_not_call(expr: &SymbolicExpression) -> bool {
        if let SymbolicExpressionType::List(exprs) = &expr.expr {
            exprs
                .first()
                .and_then(|e| e.match_atom())
                .and_then(|name| NativeFunctions::lookup_by_name(name))
                .is_some_and(|func| matches!(func, NativeFunctions::Not))
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

        let (left_idx, right_idx) = if Self::as_bool_literal(&operands[0]).is_some() {
            (0, 1)
        } else if Self::as_bool_literal(&operands[1]).is_some() {
            (1, 0)
        } else {
            return;
        };

        let left = Self::as_bool_literal(&operands[left_idx]).unwrap();

        // Check if both operands are bool literals
        if let Some(right) = Self::as_bool_literal(&operands[right_idx]) {
            let result = right == left;
            self.add_diagnostic(
                expr,
                format!("comparing two boolean literals always returns `{result}`"),
                format!("Replace with `{result}`"),
            );
            return;
        }

        let right_str = Self::format_source(&operands[right_idx]);

        if left {
            self.add_diagnostic(
                expr,
                "comparing with `true` is unnecessary".to_string(),
                format!("Replace with `{right_str}`"),
            );
        } else {
            self.add_diagnostic(
                expr,
                "comparing with `false` is unnecessary, use `not` instead".to_string(),
                format!("Replace with `(not {right_str})`"),
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
            let suggestion = if let SymbolicExpressionType::List(inner_exprs) = &operands[0].expr {
                inner_exprs
                    .get(1)
                    .map(|inner| format!("Replace with `{}`", Self::format_source(inner)))
                    .unwrap_or_else(|| "Remove this expression".to_string())
            } else {
                "Remove this expression".to_string()
            };
            self.add_diagnostic(
                expr,
                "double negation is unnecessary".to_string(),
                suggestion,
            );
        }
    }

    /// Check for absorbing elements (`(and ... false)` → `false`, `(or ... true)` → `true`)
    /// and identity elements (`(and x true)` → `x`, `(or x false)` → `x`) in a single pass.
    fn check_lazy_logical_operands(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) {
        let (absorber, identity) = match func {
            NativeFunctions::And => (NativeVariables::NativeFalse, NativeVariables::NativeTrue),
            NativeFunctions::Or => (NativeVariables::NativeTrue, NativeVariables::NativeFalse),
            _ => return,
        };
        let absorber_val = matches!(absorber, NativeVariables::NativeTrue);

        let mut has_absorber = false;
        let mut has_identity = false;
        let mut non_identity = Vec::new();

        for op in operands {
            match Self::as_bool_literal(op) {
                Some(b) if b == absorber_val => {
                    has_absorber = true;
                    non_identity.push(op);
                }
                Some(_) => has_identity = true,
                None => non_identity.push(op),
            }
        }

        if has_absorber {
            self.add_diagnostic(
                expr,
                format!("`{func}` with a `{absorber}` operand always evaluates to `{absorber}`"),
                format!("Replace with `{absorber}`"),
            );
        }

        if has_identity && !non_identity.is_empty() {
            let suggestion = Self::format_replacement_suggestion(func, &non_identity);
            self.add_diagnostic(
                expr,
                format!("`{identity}` is the identity element for `{func}` and has no effect"),
                suggestion,
            );
        }
    }

    /// Check for arithmetic identity elements: `(+ x u0)` → `x`, `(* x u1)` → `x`, etc.
    fn check_arithmetic_identity(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) {
        match func {
            NativeFunctions::Add => self.check_additive_identity(expr, operands),
            NativeFunctions::Subtract => self.check_subtractive_identity(expr, operands),
            NativeFunctions::Multiply => self.check_multiply_identity_and_absorb(expr, operands),
            NativeFunctions::Divide => self.check_divide_identity_and_zero(expr, operands),
            _ => {}
        }
    }

    /// Check `(+ x u0)`, `(+ x y 1 -1)` etc. — additive identity
    fn check_additive_identity(
        &mut self,
        expr: &'a SymbolicExpression,
        operands: &'a [SymbolicExpression],
    ) {
        let split = SplitOperands::sum_of(operands);
        if !split.has_constants(operands.len())
            || split.non_constants.is_empty()
            || !split.is_zero()
        {
            return;
        }
        let suggestion = Self::format_replacement_suggestion("+", &split.non_constants);
        self.add_diagnostic(expr, "adding zero has no effect".to_string(), suggestion);
    }

    /// Check `(- x u0)`, `(- x y 0)`, `(- x 1 -1)` etc. — subtractive identity
    /// Only positions 1+ are subtracted; position 0 is the base
    fn check_subtractive_identity(
        &mut self,
        expr: &'a SymbolicExpression,
        operands: &'a [SymbolicExpression],
    ) {
        if operands.len() < 2 {
            return;
        }

        // TODO: What if first arg is a constant?
        let tail = &operands[1..];
        let split = SplitOperands::sum_of(tail);
        if !split.has_constants(tail.len()) || !split.is_zero() {
            return;
        }

        let mut remaining = vec![&operands[0]];
        remaining.extend(&split.non_constants);
        let suggestion = Self::format_replacement_suggestion("-", &remaining);
        self.add_diagnostic(
            expr,
            "subtracting zero has no effect".to_string(),
            suggestion,
        );
    }

    /// Check `(* x u1)`, `(* x y u1)` etc. — multiplicative identity (commutative)
    /// Also checks for `(* ... 0)` — multiply by zero (absorbing element)
    fn check_multiply_identity_and_absorb(
        &mut self,
        expr: &'a SymbolicExpression,
        operands: &'a [SymbolicExpression],
    ) {
        let split = SplitOperands::product_of(operands);
        if !split.has_constants(operands.len()) || split.non_constants.is_empty() {
            return;
        }

        if split.is_zero() {
            let zero_str = split.zero_str();
            self.add_diagnostic(
                expr,
                format!("multiplying by zero always evaluates to `{zero_str}`"),
                format!("Replace with `{zero_str}`"),
            );
            return;
        }

        if split.is_one() {
            let suggestion = Self::format_replacement_suggestion("*", &split.non_constants);
            self.add_diagnostic(
                expr,
                "multiplying by one has no effect".to_string(),
                suggestion,
            );
        }
    }

    /// Check `(/ x u1)`, `(/ x y u1)` etc. — division identity (non-commutative)
    /// Also checks for `(/ ... 0)` (division by zero) and `(/ 0 ...)` (zero dividend)
    /// Position 0 is dividend, positions 1+ are divisors
    fn check_divide_identity_and_zero(
        &mut self,
        expr: &'a SymbolicExpression,
        operands: &'a [SymbolicExpression],
    ) {
        if operands.len() < 2 {
            return;
        }

        // Check for zero dividend: (/ 0 ...) -> 0
        let uint_zero_dividend = Self::as_uint_literal(&operands[0]) == Some(0);
        let int_zero_dividend = Self::as_int_literal(&operands[0]) == Some(0);
        if uint_zero_dividend || int_zero_dividend {
            let zero_str = if uint_zero_dividend { "u0" } else { "0" };
            self.add_diagnostic(
                expr,
                format!("dividing zero always evaluates to `{zero_str}`"),
                format!("Replace with `{zero_str}`"),
            );
            // No return, we still want to check the other conditions
        }

        let tail = &operands[1..];
        let split = SplitOperands::product_of(tail);
        if !split.has_constants(tail.len()) {
            return;
        }

        if split.is_zero() {
            self.add_diagnostic(
                expr,
                "division by zero will cause a runtime error".to_string(),
                "Remove this expression or use a non-zero divisor".to_string(),
            );
            return;
        }

        if split.is_one() {
            let mut remaining = vec![&operands[0]];
            remaining.extend(&split.non_constants);
            let suggestion = Self::format_replacement_suggestion("/", &remaining);
            self.add_diagnostic(
                expr,
                "dividing by one has no effect".to_string(),
                suggestion,
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
        if self.check_allowed(&expr.span) {
            return true;
        }

        if operands.len() < 2 {
            if matches!(func, NativeFunctions::Equals) {
                // (is-eq x) always returns true
                self.add_diagnostic(
                    expr,
                    "`is-eq` with a single operand always returns `true`".to_string(),
                    "Replace with `true`".to_string(),
                );
            } else {
                // Comparison ops like >, >=, <, <= require exactly 2 operands
                // (enforced by the Clarity type checker), so this branch is likely
                // unreachable. Kept as a defensive fallback.
                let suggestion = if operands.len() == 1 {
                    format!("Replace with `{}`", Self::format_source(&operands[0]))
                } else {
                    "Remove this expression".to_string()
                };
                self.add_diagnostic(
                    expr,
                    format!(
                        "`{}` with fewer than 2 operands has no effect",
                        func.get_name()
                    ),
                    suggestion,
                );
            }
        } else if matches!(func, NativeFunctions::Equals) {
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
        if Self::is_single_op_arithmetic(func) || self.check_allowed(&expr.span) {
            return true;
        }

        if operands.len() < 2 {
            // Variable length arithmetic ops with single operand have no effect
            let message = format!("`{func}` with fewer than 2 operands has no effect");
            let suggestion = format!("Replace with `{}`", Self::format_source(&operands[0]));
            self.add_diagnostic(expr, message, suggestion);
            return true;
        }

        // Check for identity elements in arithmetic ops
        self.check_arithmetic_identity(expr, func, operands);
        true
    }

    fn visit_logical(
        &mut self,
        expr: &'a SymbolicExpression,
        _func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        if !self.check_allowed(&expr.span) {
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
        if self.check_allowed(&expr.span) {
            return true;
        }

        if operands.len() < 2 {
            // Variable length logical ops with single operand have no effect
            let message = format!("`{func}` with fewer than 2 operands has no effect");
            let suggestion = format!("Replace with `{}`", Self::format_source(&operands[0]));
            self.add_diagnostic(expr, message, suggestion);
        } else {
            self.check_lazy_logical_operands(expr, func, operands);
        }

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
        assert!(output[0].contains("`is-eq` with a single operand always returns `true`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `true`")
        );
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

    // --- Suggestion text tests ---

    #[test]
    fn is_eq_single_operand_suggestion() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test-func)
                (begin
                    (is-eq u5)
                    (ok true)
                )
            )
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `true`")
        );
    }

    #[test]
    fn single_operand_and_suggestion() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (and x))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn single_operand_or_suggestion() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (or x))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn single_operand_add_suggestion() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (+ x))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn single_operand_sub_suggestion() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (- x))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn double_negation_suggestion() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (not (not x)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn is_eq_true_suggestion() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (is-eq x true))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn is_eq_false_suggestion() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (is-eq x false))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `(not x)`")
        );
    }

    // --- is-eq with two boolean literals ---

    #[test]
    fn is_eq_two_true_literals() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func)
                (is-eq true true))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("comparing two boolean literals always returns `true`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `true`")
        );
    }

    #[test]
    fn is_eq_true_false_literals() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func)
                (is-eq true false))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("comparing two boolean literals always returns `false`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `false`")
        );
    }

    #[test]
    fn is_eq_two_false_literals() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func)
                (is-eq false false))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("comparing two boolean literals always returns `true`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `true`")
        );
    }

    // --- and/or with constant operands ---

    #[test]
    fn and_with_false() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (and x false))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("`and` with a `false` operand always evaluates to `false`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `false`")
        );
    }

    #[test]
    fn and_with_false_first() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (and false x))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("`and` with a `false` operand always evaluates to `false`"));
    }

    #[test]
    fn and_with_false_middle() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool) (y bool))
                (and x false y))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("`and` with a `false` operand always evaluates to `false`"));
    }

    #[test]
    fn or_with_true() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (or x true))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("`or` with a `true` operand always evaluates to `true`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `true`")
        );
    }

    #[test]
    fn or_with_true_first() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (or true x))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("`or` with a `true` operand always evaluates to `true`"));
    }

    #[test]
    fn and_with_true() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (and x true))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].message,
            "`true` is the identity element for `and` and has no effect"
        );
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn or_with_false() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (or x false))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].message,
            "`false` is the identity element for `or` and has no effect"
        );
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn and_multi_operand_with_true() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool) (y bool))
                (and x y true))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `(and x y)`")
        );
    }

    #[test]
    fn or_multi_operand_with_false() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool) (y bool))
                (or x y false))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `(or x y)`")
        );
    }

    /// Should NOT warn for `(and x y)` without false
    #[test]
    fn and_without_false() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool) (y bool))
                (and x y))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// Should NOT warn for `(or x y)` without true
    #[test]
    fn or_without_true() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool) (y bool))
                (or x y))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_and_with_false_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (begin
                    ;; #[allow(noop)]
                    (and x false)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_or_with_true_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x bool))
                (begin
                    ;; #[allow(noop)]
                    (or x true)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    // --- Arithmetic identity element tests ---

    #[test]
    fn add_with_zero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (+ x u0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("adding zero has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn add_with_signed_zero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x int))
                (+ x 0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("adding zero has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn sub_with_zero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (- x u0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("subtracting zero has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn mul_with_one() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (* x u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("multiplying by one has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn div_with_one() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (/ x u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("dividing by one has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn add_constants_cancel() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x int))
                (+ x 1 -1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("adding zero has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn sub_constants_cancel() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x int))
                (- x 1 -1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("subtracting zero has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn mul_multiple_ones() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (* x u1 u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("multiplying by one has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn div_multiple_ones() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (/ x u1 u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("dividing by one has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    // --- Signed int identity tests ---

    #[test]
    fn sub_with_signed_zero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x int))
                (- x 0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("subtracting zero has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn mul_with_signed_one() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x int))
                (* x 1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("multiplying by one has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    #[test]
    fn div_with_signed_one() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x int))
                (/ x 1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("dividing by one has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `x`")
        );
    }

    // --- Arithmetic negative tests ---

    /// Should NOT warn for `(+ x u1)` — not adding zero
    #[test]
    fn add_with_nonzero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (+ x u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// Should NOT warn for `(* x u2)` — not multiplying by one
    #[test]
    fn mul_with_non_one() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (* x u2))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// Should NOT warn for `(- u0 x)` — result is not x
    #[test]
    fn sub_zero_first() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (- u0 x))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// Should NOT warn for `(/ u1 x)` — result is not x
    #[test]
    fn div_one_first() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (/ u1 x))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// `(+ x y u0)` should warn and suggest `(+ x y)`
    #[test]
    fn add_multi_operand_with_zero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint) (y uint))
                (+ x y u0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("adding zero has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `(+ x y)`")
        );
    }

    /// `(+ x y 1 -1)` should warn and suggest `(+ x y)`
    #[test]
    fn add_multi_operand_constants_cancel() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x int) (y int))
                (+ x y 1 -1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("adding zero has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `(+ x y)`")
        );
    }

    /// `(- x y 0)` should warn and suggest `(- x y)`
    #[test]
    fn sub_multi_operand_with_zero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint) (y uint))
                (- x y u0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("subtracting zero has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `(- x y)`")
        );
    }

    /// `(* x y u1)` should warn and suggest `(* x y)`
    #[test]
    fn mul_multi_operand_with_one() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint) (y uint))
                (* x y u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("multiplying by one has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `(* x y)`")
        );
    }

    /// `(/ x y u1)` should warn and suggest `(/ x y)`
    #[test]
    fn div_multi_operand_with_one() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint) (y uint))
                (/ x y u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("dividing by one has no effect"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `(/ x y)`")
        );
    }

    /// Should NOT warn: `(+ x y u1)` — not adding zero
    #[test]
    fn add_multi_operand_non_zero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint) (y uint))
                (+ x y u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// Should NOT warn: `(- x y u1)` — not subtracting zero
    #[test]
    fn sub_multi_operand_non_zero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint) (y uint))
                (- x y u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_add_with_zero_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (begin
                    ;; #[allow(noop)]
                    (+ x u0)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    // --- Multiply by zero tests ---

    #[test]
    fn mul_with_zero_uint() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (* x u0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("multiplying by zero always evaluates to `u0`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `u0`")
        );
    }

    #[test]
    fn mul_with_zero_int() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x int))
                (* x 0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("multiplying by zero always evaluates to `0`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `0`")
        );
    }

    #[test]
    fn mul_with_zero_first() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (* u0 x))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("multiplying by zero always evaluates to `u0`"));
    }

    #[test]
    fn mul_multi_operand_with_zero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint) (y uint))
                (* x y u0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("multiplying by zero always evaluates to `u0`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `u0`")
        );
    }

    // --- Zero dividend tests ---

    #[test]
    fn div_zero_dividend_uint() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (/ u0 x))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("dividing zero always evaluates to `u0`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `u0`")
        );
    }

    #[test]
    fn div_zero_dividend_int() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x int))
                (/ 0 x))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("dividing zero always evaluates to `0`"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Replace with `0`")
        );
    }

    #[test]
    fn div_zero_dividend_multi_divisor() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint) (y uint))
                (/ u0 x y))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("dividing zero always evaluates to `u0`"));
    }

    // --- Division by zero tests ---

    #[test]
    fn div_by_zero_uint() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (/ x u0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("division by zero will cause a runtime error"));
        assert_eq!(
            result.diagnostics[0].suggestion.as_deref(),
            Some("Remove this expression or use a non-zero divisor")
        );
    }

    #[test]
    fn div_by_zero_int() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x int))
                (/ x 0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("division by zero will cause a runtime error"));
    }

    #[test]
    fn div_by_zero_multi_divisor() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint) (y uint))
                (/ x y u0))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("division by zero will cause a runtime error"));
    }

    /// Both zero dividend and division by zero are reported
    #[test]
    fn div_zero_by_zero() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func)
                (/ u0 u0))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 2);
        assert_eq!(
            result.diagnostics[0].message,
            "dividing zero always evaluates to `u0`"
        );
        assert_eq!(
            result.diagnostics[1].message,
            "division by zero will cause a runtime error"
        );
    }

    /// Both zero dividend and identity divisor are reported
    #[test]
    fn div_zero_dividend_with_identity_divisor() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (/ u0 x u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 2);
        assert_eq!(
            result.diagnostics[0].message,
            "dividing zero always evaluates to `u0`"
        );
        assert_eq!(
            result.diagnostics[1].message,
            "dividing by one has no effect"
        );
    }

    /// Should NOT warn: `(/ x u2)` is not a noop
    #[test]
    fn div_with_non_one() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (/ x u2))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    // --- Annotation tests for new checks ---

    #[test]
    fn allow_mul_with_zero_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (begin
                    ;; #[allow(noop)]
                    (* x u0)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_div_by_zero_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (begin
                    ;; #[allow(noop)]
                    (/ x u0)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_div_zero_dividend_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (begin
                    ;; #[allow(noop)]
                    (/ u0 x)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_mul_with_one_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (test-func (x uint))
                (begin
                    ;; #[allow(noop)]
                    (* x u1)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
