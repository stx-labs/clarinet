use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
pub use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::SymbolicExpression;
use clarity::vm::SymbolicExpressionType::List;
use clarity::vm::{ClarityName, ClarityVersion};

use crate::analysis::annotation::Annotation;
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use crate::analysis::{AnalysisPass, AnalysisResult, Settings};

pub struct CallChecker<'a> {
    clarity_version: ClarityVersion,
    diagnostics: Vec<Diagnostic>,
    // For each user-defined function, record the parameter count.
    user_funcs: HashMap<&'a ClarityName, usize>,
    // For each call of a user-defined function which has not been defined yet,
    // record the argument count, to check later.
    user_calls: Vec<(&'a ClarityName, &'a SymbolicExpression, usize)>,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a> CallChecker<'a> {
    fn new(clarity_version: ClarityVersion, level: Level) -> CallChecker<'a> {
        Self {
            clarity_version,
            diagnostics: Vec::new(),
            user_funcs: HashMap::new(),
            user_calls: Vec::new(),
            level,
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        traverse(&mut self, &contract_analysis.expressions);
        self.check_user_calls();

        if !self.diagnostics.is_empty() {
            Err(self.diagnostics)
        } else {
            Ok(vec![])
        }
    }

    fn check_user_calls(&mut self) {
        for i in 0..self.user_calls.len() {
            let (name, call_expr, num_args) = self.user_calls[i];
            if let Some(&num_params) = self.user_funcs.get(name) {
                if num_args != num_params {
                    let diagnostic =
                        self.generate_diagnostic(call_expr, name, num_params, num_args);
                    self.diagnostics.push(diagnostic);
                }
            }
        }
    }

    fn check_builtin_arg_count(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &str,
        param_count: usize,
    ) {
        let exprs: &[SymbolicExpression] = if let List(exprs) = &expr.expr {
            exprs
        } else {
            panic!("expected list expression");
        };
        if exprs.len() != (param_count + 1) {
            let diagnostic = self.generate_diagnostic(expr, name, param_count, exprs.len() - 1);
            self.diagnostics.push(diagnostic);
        }
    }

    fn generate_diagnostic(
        &mut self,
        expr: &SymbolicExpression,
        name: &str,
        expected: usize,
        got: usize,
    ) -> Diagnostic {
        Diagnostic {
            level: self.level.clone(),
            message: format!(
                "incorrect number of arguments in call to '{name}' (expected {expected} got {got})"
            ),
            spans: vec![expr.span.clone()],
            suggestion: None,
        }
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
        let num_params = match parameters {
            Some(parameters) => parameters.len(),
            None => 0,
        };
        self.user_funcs.insert(name, num_params);
        true
    }

    fn visit_define_public(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        let num_params = match parameters {
            Some(parameters) => parameters.len(),
            None => 0,
        };
        self.user_funcs.insert(name, num_params);
        true
    }

    fn visit_define_read_only(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        let num_params = match parameters {
            Some(parameters) => parameters.len(),
            None => 0,
        };
        self.user_funcs.insert(name, num_params);
        true
    }

    fn visit_call_user_defined(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        if let Some(param_count) = self.user_funcs.get(name) {
            let param_count = *param_count;
            if args.len() != param_count {
                let diagnostic = self.generate_diagnostic(expr, name, param_count, args.len());
                self.diagnostics.push(diagnostic);
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
        contract_analysis: &mut ContractAnalysis,
        _analysis_db: &mut AnalysisDatabase,
        _annotations: &Vec<Annotation>,
        level: Level,
        _settings: &Settings,
    ) -> AnalysisResult {
        let tc = CallChecker::new(contract_analysis.clarity_version, level);
        tc.run(contract_analysis)
    }
}

#[cfg(test)]
mod tests {
    use crate::analysis::Pass;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    #[test]
    fn define_private() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CallChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-private (foo (amount uint))
    (ok amount)
)

(define-public (main)
    (ok (foo u1 u2))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Err((output, _)) => {
                assert_eq!(output.len(), 3);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:7:9: {}",
                        format_err!(
                            "incorrect number of arguments in call to 'foo' (expected 1 got 2)"
                        )
                    )
                );
                assert_eq!(output[1], "    (ok (foo u1 u2))");
                assert_eq!(output[2], "        ^~~~~~~~~~~");
            }
            _ => panic!("Expected error"),
        };
    }

    #[test]
    fn define_read_only() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CallChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-read-only (foo (amount uint))
    (ok amount)
)

(define-public (main)
    (ok (foo))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Err((output, _)) => {
                assert_eq!(output.len(), 3);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:7:9: {}",
                        format_err!(
                            "incorrect number of arguments in call to 'foo' (expected 1 got 0)"
                        )
                    )
                );
                assert_eq!(output[1], "    (ok (foo))");
                assert_eq!(output[2], "        ^~~~~");
            }
            _ => panic!("Expected error"),
        };
    }

    #[test]
    fn define_public() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CallChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-public (foo (amount uint))
    (ok amount)
)

(define-public (main)
    (ok (foo u1 u2))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Err((output, _)) => {
                assert_eq!(output.len(), 3);
                assert_eq!(
                    output[0],
                    format!(
                        "checker:7:9: {}",
                        format_err!(
                            "incorrect number of arguments in call to 'foo' (expected 1 got 2)"
                        )
                    )
                );
                assert_eq!(output[1], "    (ok (foo u1 u2))");
                assert_eq!(output[2], "        ^~~~~~~~~~~");
            }
            _ => panic!("Expected error"),
        };
    }

    #[test]
    fn correct_call() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CallChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-private (foo (amount uint))
    (ok amount)
)

(define-public (main)
    (ok (foo u1))
)
"
        .to_string();
        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected successful interpretation"),
        };
    }

    #[test]
    fn builtin_function_arg_count() {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.passes = vec![Pass::CallChecker];
        let mut session = Session::new(settings);
        let snippet = "
(define-map kv-store { key: int } { value: int })
(define-private (incompatible-tuple) (tuple (k 1)))
(define-private (kv-set (key int) (value int))
    (map-set kv-store { key: key } { value: value } {value: 0}))"
            .to_string();
        if let Err((err_output, _)) =
            session.formatted_interpretation(snippet, Some("checker".to_string()), false, None)
        {
            assert_eq!(
                err_output[0],
                format!(
                    "checker:5:5: {}",
                    format_err!(
                        "incorrect number of arguments in call to 'map-set' (expected 3 got 4)"
                    )
                )
            );
        } else {
            panic!("expected error")
        }

        let snippet = "
(define-map kv-store { key: int } { value: int })
(define-private (incompatible-tuple) (tuple (k 1)))
(define-private (kv-add (key int) (value int))
    (map-insert kv-store { key: key } { value: value } { value: 0}))"
            .to_string();
        if let Err((err_output, _)) =
            session.formatted_interpretation(snippet, Some("checker".to_string()), false, None)
        {
            assert_eq!(
                err_output[0],
                format!(
                    "checker:5:5: {}",
                    format_err!(
                        "incorrect number of arguments in call to 'map-insert' (expected 3 got 4)"
                    )
                )
            );
        } else {
            panic!("expected error")
        }

        let snippet = "
(define-map kv-store { key: int } { value: int })
(define-private (incompatible-tuple) (tuple (k 1)))
(define-private (kv-del (key int))
    (map-delete kv-store { key: 1 } {value: 0}))"
            .to_string();
        if let Err((err_output, _)) =
            session.formatted_interpretation(snippet, Some("checker".to_string()), false, None)
        {
            assert_eq!(
                err_output[0],
                format!(
                    "checker:5:5: {}",
                    format_err!(
                        "incorrect number of arguments in call to 'map-delete' (expected 2 got 3)"
                    )
                )
            );
        } else {
            panic!("expected error")
        }
    }
}
