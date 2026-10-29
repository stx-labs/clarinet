//! Lint to find calls with the wrong number of arguments

use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::SymbolicExpression;
use clarity::vm::SymbolicExpressionType::List;
use clarity::vm::{ClarityName, ClarityVersion};
use stacks_common::bounded_format;

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{AnalysisPass, AnalysisResult, LintName};

pub struct CallChecker<'a> {
    clarity_version: ClarityVersion,
    diagnostics: Vec<Diagnostic>,
    annotations: &'a [Annotation],
    // For each user-defined function, record the parameter count.
    user_funcs: HashMap<&'a ClarityName, usize>,
    // For each call of a user-defined function which has not been defined yet,
    // record the argument count, to check later.
    user_calls: Vec<(&'a ClarityName, &'a SymbolicExpression, usize)>,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a> CallChecker<'a> {
    fn new(clarity_version: ClarityVersion, annotations: &'a [Annotation], level: Level) -> Self {
        Self {
            clarity_version,
            diagnostics: Vec::new(),
            annotations,
            user_funcs: HashMap::new(),
            user_calls: Vec::new(),
            level,
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        traverse(&mut self, &contract_analysis.expressions);
        self.check_user_calls();

        // Unlike other lints, this one rejects the contract at error level:
        // these calls are almost certainly bugs the type-checker lets through.
        if self.level == Level::Error && !self.diagnostics.is_empty() {
            Err(self.diagnostics)
        } else {
            Ok(self.diagnostics)
        }
    }

    fn check_user_calls(&mut self) {
        for (name, call_expr, num_args) in std::mem::take(&mut self.user_calls) {
            if let Some(&num_params) = self.user_funcs.get(name) {
                if num_args != num_params {
                    self.add_diagnostic(call_expr, name, num_params, num_args);
                }
            }
        }
    }

    fn allow(&self, expr: &SymbolicExpression) -> bool {
        get_index_of_span(self.annotations, &expr.span)
            .is_some_and(|idx| Self::match_allow_annotation(&self.annotations[idx]))
    }

    fn check_builtin_arg_count(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &str,
        param_count: usize,
    ) {
        let List(exprs) = &expr.expr else {
            panic!("expected list expression");
        };
        if exprs.len() != (param_count + 1) {
            self.add_diagnostic(expr, name, param_count, exprs.len() - 1);
        }
    }

    fn add_diagnostic(
        &mut self,
        expr: &SymbolicExpression,
        name: &str,
        expected: usize,
        got: usize,
    ) {
        if self.allow(expr) {
            return;
        }
        self.diagnostics.push(Diagnostic {
            level: self.level.clone(),
            message: bounded_format!(
                "incorrect number of arguments in call to '{name}' (expected {expected} got {got})"
            ),
            spans: vec![expr.span.clone()],
            suggestion: None,
        });
    }
}

impl<'a> ASTVisitor<'a> for CallChecker<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_private(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        let num_params = parameters.map_or(0, |parameters| parameters.len());
        self.user_funcs.insert(name, num_params);
        true
    }

    fn visit_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.visit_define_private(expr, name, parameters, body)
    }

    fn visit_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.visit_define_private(expr, name, parameters, body)
    }

    fn visit_call_user_defined(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        if let Some(&param_count) = self.user_funcs.get(name) {
            if args.len() != param_count {
                self.add_diagnostic(expr, name, param_count, args.len());
            }
        } else {
            self.user_calls.push((name, expr, args.len()));
        }
        true
    }

    // The type-checker does not properly check the argument count for some
    // built-in functions. Those that are not checked by the type-checker are
    // checked below.

    fn visit_map_set(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        _value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        self.check_builtin_arg_count(expr, "map-set", 3);
        true
    }

    fn visit_map_insert(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        _value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        self.check_builtin_arg_count(expr, "map-insert", 3);
        true
    }

    fn visit_map_delete(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        self.check_builtin_arg_count(expr, "map-delete", 2);
        true
    }
}

impl AnalysisPass for CallChecker<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
    ) -> AnalysisResult {
        let checker = CallChecker::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
        );
        checker.run(analysis_cache.contract_analysis)
    }
}

impl Lint for CallChecker<'_> {
    fn get_name() -> LintName {
        LintName::CallChecker
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::CallChecker)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use indoc::{formatdoc, indoc};

    use super::CallChecker;
    use crate::analysis::linter::Lint;
    use crate::analysis::lints::UnusedConst;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn session_with_level(level: Level) -> Session {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(CallChecker::get_name(), level);
        Session::new_without_boot_contracts(settings)
    }

    fn default_session() -> Session {
        session_with_level(Level::Error)
    }

    #[rustfmt::skip]
    const MAP_SET_EXTRA_ARG: &str = indoc!("
        (define-map kv-store { key: int } { value: int })
        (define-private (kv-set (key int) (value int))
            (map-set kv-store { key: key } { value: value } {value: 0}))
    ");

    #[test]
    fn warning_level_does_not_reject_contract() {
        let mut session = session_with_level(Level::Warning);
        let (_, result) = session
            .formatted_interpretation(
                MAP_SET_EXTRA_ARG.to_string(),
                Some("checker".to_string()),
                false,
                None,
            )
            .expect("contract should deploy at warning level");
        assert_eq!(result.lint_diagnostics.len(), 1);
        assert_eq!(result.lint_diagnostics[0].diagnostic.level, Level::Warning);
    }

    #[test]
    fn allow_with_annotation() {
        let mut session = default_session();
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map kv-store { key: int } { value: int })
            (define-private (kv-set (key int) (value int))
                ;; #[allow(call_checker)]
                (map-set kv-store { key: key } { value: value } {value: 0}))
        ").to_string();
        let (_, result) = session
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("allowed call should not reject the contract");
        assert!(result.lint_diagnostics.is_empty());
    }

    // CallChecker runs before UnusedConst; rejection must not hide later lints.
    #[test]
    fn rejecting_lint_does_not_hide_the_others() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant MY_UNUSED u42)
            (define-private (foo (a uint)) (ok a))
            (define-public (main) (ok (foo u1 u2)))
        ").to_string();

        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(CallChecker::get_name(), Level::Error);
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedConst::get_name(), Level::Warning);

        let (output, diagnostics) = Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect_err("the wrong-arity call must reject the contract");
        assert_eq!(
            diagnostics.iter().map(|d| d.lint_name).collect::<Vec<_>>(),
            [Some(CallChecker::get_name()), Some(UnusedConst::get_name())]
        );
        let output = output.join("\n");

        assert!(output.contains("error[call_checker]"), "{output}");
        assert!(output.contains("warning[unused_const]"), "{output}");
    }

    #[test]
    fn user_defined_function_arity() {
        for definition in ["define-private", "define-read-only", "define-public"] {
            for args in ["", "u1 u2"] {
                let snippet = formatdoc!(
                    "
                    ({definition} (foo (amount uint)) (ok amount))
                    (define-public (main) (ok (foo {args})))
                    "
                );
                let diagnostics = default_session()
                    .eval(snippet, false)
                    .expect_err("wrong-arity calls must reject the contract");
                assert_eq!(diagnostics[0].lint_name, Some(CallChecker::get_name()));
                assert_eq!(diagnostics[0].diagnostic.level, Level::Error);
            }
        }
    }

    #[test]
    fn correct_call() {
        let mut session = default_session();
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (foo (amount uint))
                (ok amount)
            )

            (define-public (main)
                (ok (foo u1))
            )
        ").to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.lint_diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn builtin_function_arg_count() {
        for call in [
            "(map-set kv-store { key: 1 } { value: 1 } { value: 0 })",
            "(map-insert kv-store { key: 1 } { value: 1 } { value: 0 })",
            "(map-delete kv-store { key: 1 } { value: 0 })",
        ] {
            let snippet = formatdoc!(
                "
                (define-map kv-store {{ key: int }} {{ value: int }})
                (define-private (run) {call})
                "
            );
            let diagnostics = default_session()
                .eval(snippet, false)
                .expect_err("extra map arguments must reject the contract");
            assert_eq!(diagnostics[0].lint_name, Some(CallChecker::get_name()));
            assert_eq!(diagnostics[0].diagnostic.level, Level::Error);
        }
    }
}
