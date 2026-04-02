//! Lint to find unnecessary single-field tuples in type and value positions.
//!
//! A tuple with only one field adds encoding overhead without benefit — the
//! inner type or value can be used directly instead.

use std::collections::{HashMap, HashSet};

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::Span;
use clarity::vm::{ClarityName, ClarityVersion, SymbolicExpression};

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct UnnecessaryTuple<'a> {
    clarity_version: ClarityVersion,
    diagnostics: Vec<Diagnostic>,
    annotations: &'a [Annotation],
    level: Level,
    active_annotation: Option<usize>,
    /// Pointers to `SymbolicExpression` nodes that are direct arguments to
    /// `merge` — single-field tuples are required there, so we skip warnings.
    merge_args: HashSet<*const SymbolicExpression>,
}

impl<'a> UnnecessaryTuple<'a> {
    fn new(clarity_version: ClarityVersion, annotations: &'a [Annotation], level: Level) -> Self {
        Self {
            clarity_version,
            level,
            diagnostics: Vec::new(),
            annotations,
            active_annotation: None,
            merge_args: HashSet::new(),
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        traverse(&mut self, &contract_analysis.expressions);
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

    /// Check if a `SymbolicExpression` represents a single-field tuple.
    /// Works for both type expressions like `(tuple (key uint))` and
    /// value expressions like `(tuple (key u1))`.
    fn is_single_field_tuple(&self, expr: &SymbolicExpression) -> bool {
        let Some(list) = expr.match_list() else {
            return false;
        };
        let Some((first, fields)) = list.split_first() else {
            return false;
        };
        let Some(name) = first.match_atom() else {
            return false;
        };
        NativeFunctions::lookup_by_name_at_version(name, &self.clarity_version)
            == Some(NativeFunctions::TupleCons)
            && fields.len() == 1
    }

    fn add_diagnostic(&mut self, span: &Span, message: String, suggestion: String) {
        self.diagnostics.push(Diagnostic {
            level: self.level.clone(),
            message,
            spans: vec![span.clone()],
            suggestion: Some(suggestion),
        });
    }

    fn add_type_diagnostic(&mut self, expr: &SymbolicExpression) {
        self.add_diagnostic(&expr.span, Self::type_message(), Self::type_suggestion());
    }

    fn add_value_diagnostic(&mut self, expr: &SymbolicExpression) {
        self.add_diagnostic(&expr.span, Self::value_message(), Self::value_suggestion());
    }

    fn type_message() -> String {
        "single-field tuple type is unnecessary".to_owned()
    }

    fn type_suggestion() -> String {
        "Use the inner type directly instead of wrapping it in a single-field tuple".to_owned()
    }

    fn value_message() -> String {
        "single-field tuple is unnecessary".to_owned()
    }

    fn value_suggestion() -> String {
        "Use the inner value directly instead of wrapping it in a single-field tuple".to_owned()
    }

    fn check_params(&mut self, parameters: &Option<Vec<TypedVar<'a>>>) {
        let Some(params) = parameters else { return };
        for param in params {
            if self.is_single_field_tuple(param.type_expr) {
                self.add_type_diagnostic(param.type_expr);
            }
        }
    }
}

impl<'a> ASTVisitor<'a> for UnnecessaryTuple<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_data_var(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        data_type: &'a SymbolicExpression,
        _initial: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if self.allow() {
            return true;
        }
        if self.is_single_field_tuple(data_type) {
            self.add_type_diagnostic(data_type);
        }
        true
    }

    fn visit_define_map(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        key_type: &'a SymbolicExpression,
        value_type: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if self.allow() {
            return true;
        }
        if self.is_single_field_tuple(key_type) {
            self.add_type_diagnostic(key_type);
        }
        if self.is_single_field_tuple(value_type) {
            self.add_type_diagnostic(value_type);
        }
        true
    }

    fn visit_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if !self.allow() {
            self.check_params(&parameters);
        }
        true
    }

    fn visit_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if !self.allow() {
            self.check_params(&parameters);
        }
        true
    }

    fn visit_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if !self.allow() {
            self.check_params(&parameters);
        }
        true
    }

    fn visit_define_nft(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        nft_type: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if self.allow() {
            return true;
        }
        if self.is_single_field_tuple(nft_type) {
            self.add_type_diagnostic(nft_type);
        }
        true
    }

    fn visit_define_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        functions: &'a [SymbolicExpression],
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if self.allow() {
            return true;
        }

        // `functions` is a single-element slice containing the wrapper list
        // of all function signatures, e.g. for:
        //   (define-trait t ((fn1 (uint) (response uint uint))))
        // functions[0] = ((fn1 (uint) (response uint uint)))
        for func_wrapper in functions {
            let Some(func_sigs) = func_wrapper.match_list() else {
                continue;
            };
            for func_sig in func_sigs {
                let Some(func_parts) = func_sig.match_list() else {
                    continue;
                };
                // func_parts: [fn-name, (arg-types...), response-type]

                // Check argument types
                if let Some(arg_types) = func_parts.get(1).and_then(|e| e.match_list()) {
                    for arg_type in arg_types {
                        if self.is_single_field_tuple(arg_type) {
                            self.add_type_diagnostic(arg_type);
                        }
                    }
                }

                // Check response type
                if let Some(response_expr) = func_parts.get(2) {
                    if self.is_single_field_tuple(response_expr) {
                        self.add_type_diagnostic(response_expr);
                    } else if let Some(response_parts) = response_expr.match_list() {
                        // Check inside (response ok-type err-type).
                        // "response" is a type-level keyword with no NativeFunctions
                        // variant, so a string comparison is necessary here.
                        if response_parts
                            .first()
                            .and_then(|e| e.match_atom())
                            .is_some_and(|n| n.as_str() == "response")
                        {
                            for part in &response_parts[1..] {
                                if self.is_single_field_tuple(part) {
                                    self.add_type_diagnostic(part);
                                }
                            }
                        }
                    }
                }
            }
        }

        true
    }

    fn traverse_merge(
        &mut self,
        expr: &'a SymbolicExpression,
        tuple1: &'a SymbolicExpression,
        tuple2: &'a SymbolicExpression,
    ) -> bool {
        // Both arguments to `merge` must be tuples, so single-field tuples
        // are legitimate there. Track their identities so visit_tuple skips them.
        let ptr1 = std::ptr::from_ref(tuple1);
        let ptr2 = std::ptr::from_ref(tuple2);
        self.merge_args.insert(ptr1);
        self.merge_args.insert(ptr2);

        let result = self.traverse_expr(tuple1)
            && self.traverse_expr(tuple2)
            && self.visit_merge(expr, tuple1, tuple2);

        self.merge_args.remove(&ptr1);
        self.merge_args.remove(&ptr2);
        result
    }

    fn visit_tuple(
        &mut self,
        expr: &'a SymbolicExpression,
        values: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        if self.merge_args.contains(&std::ptr::from_ref(expr)) {
            return true;
        }
        self.set_active_annotation(&expr.span);
        if self.allow() {
            return true;
        }
        if values.len() == 1 {
            self.add_value_diagnostic(expr);
        }
        true
    }
}

impl AnalysisPass for UnnecessaryTuple<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let checker = UnnecessaryTuple::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
        );
        checker.run(analysis_cache.contract_analysis)
    }
}

impl Lint for UnnecessaryTuple<'_> {
    fn get_name() -> LintName {
        LintName::UnnecessaryTuple
    }

    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::UnnecessaryTuple)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity_types::diagnostic::Level;
    use indoc::indoc;

    use super::UnnecessaryTuple;
    use crate::analysis::linter::Lint;
    use crate::repl::session::{AnnotatedExecutionResult, Session};
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, AnnotatedExecutionResult) {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnnecessaryTuple::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    // ── data var ──────────────────────────────────────────────

    #[test]
    fn warn_data_var_single_field_tuple_type() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-data-var my-var { key: uint } { key: u0 })
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        // value diagnostic fires first (initial traversed before visit_define_data_var),
        // then type diagnostic for the data_type
        assert_eq!(result.lint_diagnostics.len(), 2);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::value_message()));
        assert!(result.lint_diagnostics[1]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
        assert!(output[0].contains("warning["));
    }

    #[test]
    fn no_warn_data_var_multi_field_tuple() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-data-var my-var { a: uint, b: bool } { a: u0, b: false })
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    // ── map ───────────────────────────────────────────────────

    #[test]
    fn warn_map_single_field_key() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-map my-map { key: uint } { a: uint, b: bool })
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
        assert!(output[0].contains("warning["));
    }

    #[test]
    fn warn_map_single_field_value() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-map my-map { a: uint, b: bool } { val: uint })
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
        assert!(output[0].contains("warning["));
    }

    #[test]
    fn warn_map_both_single_field() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-map my-map { key: uint } { val: uint })
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 2);
    }

    // ── constant ──────────────────────────────────────────────

    #[test]
    fn warn_constant_single_field_tuple() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-constant MY_CONST { key: u1 })
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::value_message()));
        assert!(output[0].contains("warning["));
    }

    #[test]
    fn no_warn_constant_multi_field_tuple() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-constant MY_CONST { a: u1, b: u2 })
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    // ── function params ───────────────────────────────────────

    #[test]
    fn warn_private_fn_single_field_param() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-private (my-func (arg { key: uint }))
                (get key arg)
            )
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
        assert!(output[0].contains("warning["));
    }

    #[test]
    fn warn_public_fn_single_field_param() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-public (my-func (arg { key: uint }))
                (ok (get key arg))
            )
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
        assert!(output[0].contains("warning["));
    }

    #[test]
    fn warn_read_only_fn_single_field_param() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-read-only (my-func (arg { key: uint }))
                (get key arg)
            )
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
        assert!(output[0].contains("warning["));
    }

    #[test]
    fn no_warn_fn_multi_field_param() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-private (my-func (arg { a: uint, b: bool }))
                (get a arg)
            )
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    // ── return values (tuple literals) ────────────────────────

    #[test]
    fn warn_return_single_field_tuple() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-private (my-func)
                { key: u1 }
            )
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::value_message()));
        assert!(output[0].contains("warning["));
    }

    // ── nested tuples ─────────────────────────────────────────

    #[test]
    fn warn_single_field_tuple_nested_in_multi_field() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-constant MY_CONST { a: u1, b: { inner: u2 } })
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        // The outer tuple has 2 fields (no warning), but the inner `{ inner: u2 }`
        // is a single-field tuple literal
        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::value_message()));
    }

    // ── merge ─────────────────────────────────────────────────

    #[test]
    fn no_warn_single_field_tuple_in_merge() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-private (my-func (user { name: (string-ascii 50), age: uint }))
                (merge user { age: u30 })
            )
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn warn_nested_single_field_tuple_inside_merge() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-private (my-func (user { name: (string-ascii 50), data: { key: uint } }))
                (merge user { data: { key: u1 } })
            )
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        // The merge arg `{ data: ... }` is exempt, but the nested `{ key: u1 }`
        // value literal is still flagged
        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::value_message()));
    }

    // ── NFT ───────────────────────────────────────────────────

    #[test]
    fn warn_nft_single_field_tuple_type() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-non-fungible-token my-nft { id: uint })
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
        assert!(output[0].contains("warning["));
    }

    #[test]
    fn no_warn_nft_multi_field_tuple() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-non-fungible-token my-nft { id: uint, owner: principal })
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    // ── trait ─────────────────────────────────────────────────

    #[test]
    fn warn_trait_single_field_param() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-trait my-trait (
                (my-fn ({ key: uint }) (response uint uint))
            ))
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
        assert!(output[0].contains("warning["));
    }

    #[test]
    fn warn_trait_single_field_response_ok() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-trait my-trait (
                (my-fn (uint) (response { key: uint } uint))
            ))
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
        assert!(output[0].contains("warning["));
    }

    #[test]
    fn warn_trait_single_field_response_err() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-trait my-trait (
                (my-fn (uint) (response uint { err: uint }))
            ))
        "#).to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
        assert!(output[0].contains("warning["));
    }

    // ── non-tuple types (sanity check) ──────────────────────

    #[test]
    fn no_warn_map_with_bare_types() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-map my-map uint bool)
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    // ── mixed params ──────────────────────────────────────────

    #[test]
    fn warn_only_single_field_params() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-private (my-func (a { key: uint }) (b { x: uint, y: bool }))
                (+ (get key a) (get x b))
            )
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::type_message()));
    }

    // ── explicit (tuple ...) syntax ───────────────────────────

    #[test]
    fn warn_explicit_tuple_syntax() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-constant MY_CONST (tuple (key u1)))
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(result.lint_diagnostics[0]
            .diagnostic
            .message
            .contains(&UnnecessaryTuple::value_message()));
    }

    // ── allow annotation ──────────────────────────────────────

    #[test]
    fn allow_with_annotation_on_define() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            ;; #[allow(unnecessary_tuple)]
            (define-data-var my-var { key: uint } { key: u0 })
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn allow_with_annotation_on_tuple_literal() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (define-private (my-func)
                ;; #[allow(unnecessary_tuple)]
                { key: u1 }
            )
        "#).to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }
}
