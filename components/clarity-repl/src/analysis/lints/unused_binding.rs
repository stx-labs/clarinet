//! Lint to find unused bindings from `let` statements or function args
//!
//! A diagnostic is generated if:
//!  - A function argument is not referenced
//!  - A `let` binding is not referenced

use std::collections::HashMap;
use std::hash::Hash;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct UnusedBindingSettings {
    // TODO
}

impl UnusedBindingSettings {
    fn new() -> Self {
        Self {}
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum BindingType {
    FunctionArg,
    LetBinding,
}

/// Unique identifier for each binding, including span to prevent ambiguity
#[derive(PartialEq, Eq, Hash)]
struct Binding<'a> {
    kind: BindingType,
    name: &'a ClarityName,
    span: Span,
}

/// Data associated with a local variable
struct BindingData<'a> {
    expr: &'a SymbolicExpression,
    /// Has this variable been referenced?
    pub used: bool,
}

impl<'a> BindingData<'a> {
    fn new(expr: &'a SymbolicExpression) -> Self {
        Self { expr, used: false }
    }
}

pub struct UnusedBinding<'a> {
    clarity_version: ClarityVersion,
    _settings: UnusedBindingSettings,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
    level: Level,
    /// Names of all `let` bindings and function args currently in scope
    active_bindings: HashMap<&'a ClarityName, BindingData<'a>>,
    /// Variables which went out of scope without being referenced
    /// TODO: Move into `AnalysisCache`
    bindings: HashMap<Binding<'a>, BindingData<'a>>,
}

impl<'a> UnusedBinding<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
        settings: UnusedBindingSettings,
    ) -> UnusedBinding<'a> {
        Self {
            clarity_version,
            _settings: settings,
            level,
            annotations,
            active_annotation: None,
            active_bindings: HashMap::new(),
            bindings: HashMap::new(),
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        // Traverse the entire AST
        traverse(&mut self, &contract_analysis.expressions);

        // Process hashmap of unused constants and generate diagnostics
        let diagnostics = self.generate_diagnostics();

        Ok(diagnostics)
    }

    // Check for annotations that should be attached to the given span
    fn set_active_annotation(&mut self, span: &Span) {
        self.active_annotation = get_index_of_span(self.annotations, span);
    }

    // Check if the expression is annotated with `allow(<lint_name>)`
    fn allow(&self) -> bool {
        self.active_annotation
            .map(|idx| Self::match_allow_annotation(&self.annotations[idx]))
            .unwrap_or(false)
    }

    /// Make diagnostic message and suggestion for unused function argument
    fn make_diagnostic_strings(
        var_type: BindingType,
        name: &ClarityName,
    ) -> (String, Option<String>) {
        let msg = match var_type {
            BindingType::FunctionArg => format!("function parameter `{name}` is never used"),
            BindingType::LetBinding => format!("`let` binding `{name}` is never used"),
        };
        (msg, Some("Remove this expression".to_string()))
    }

    fn make_diagnostic(
        &self,
        span: Span,
        message: String,
        suggestion: Option<String>,
    ) -> Diagnostic {
        Diagnostic {
            level: self.level.clone(),
            message,
            spans: vec![span],
            suggestion,
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        for (binding, data) in &self.bindings {
            if data.used {
                continue;
            }
            let (message, suggestion) = Self::make_diagnostic_strings(binding.kind, binding.name);
            let diagnostic = self.make_diagnostic(binding.span.clone(), message, suggestion);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }

    /// Add function parameters to current scope (`active_bindings`)
    fn add_to_scope(&mut self, params: &[TypedVar<'a>]) {
        for param in params {
            self.set_active_annotation(&param.type_expr.span);
            if self.allow() {
                continue;
            }
            let data = BindingData::new(param.type_expr);
            self.active_bindings.insert(param.name, data);
        }
    }

    /// Remove function parameters to current scope (`active_bindings`)
    fn remove_from_scope(&mut self, params: &[TypedVar<'a>]) {
        for param in params {
            if let Some(data) = self.active_bindings.remove(param.name) {
                let binding = Binding {
                    kind: BindingType::FunctionArg,
                    name: param.name,
                    span: param.decl_span.clone(),
                };
                self.bindings.insert(binding, data);
            }
        }
    }

    fn traverse_define_fn(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        if let Some(params) = parameters {
            self.add_to_scope(&params);
            let res = self.traverse_expr(body);
            self.remove_from_scope(&params);
            res
        } else {
            self.traverse_expr(body)
        }
    }
}

impl<'a> ASTVisitor<'a> for UnusedBinding<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn traverse_let(
        &mut self,
        expr: &'a SymbolicExpression,
        bindings: &HashMap<&'a ClarityName, &'a SymbolicExpression>,
        body: &'a [SymbolicExpression],
    ) -> bool {
        // Traverse bindings
        for (name, expr) in bindings {
            if !self.traverse_expr(expr) {
                return false;
            }
            self.set_active_annotation(&expr.span);
            if self.allow() {
                continue;
            }
            // Binding becomes active AFTER traversing it's `expr` and BEFORE traversing the next binding's `expr`
            self.active_bindings.insert(name, BindingData::new(expr));
        }

        // Traverse `let` body
        for expr in body {
            if !self.traverse_expr(expr) {
                return false;
            }
        }

        let res = self.visit_let(expr, bindings, body);

        // Leaving scope, remove current `let` bindings and save those that were not used
        for name in bindings.keys() {
            if let Some(data) = self.active_bindings.remove(name) {
                let binding = Binding {
                    kind: BindingType::LetBinding,
                    name,
                    span: data.expr.span.clone(),
                };
                self.bindings.insert(binding, data);
            }
        }
        res
    }

    fn traverse_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<analysis::ast_visitor::TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_define_fn(expr, name, parameters, body)
    }

    fn traverse_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<analysis::ast_visitor::TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_define_fn(expr, name, parameters, body)
    }

    fn traverse_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<analysis::ast_visitor::TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_define_fn(expr, name, parameters, body)
    }

    fn visit_atom(&mut self, _expr: &'a SymbolicExpression, atom: &'a ClarityName) -> bool {
        if let Some(var) = self.active_bindings.get_mut(atom) {
            var.used = true;
        };
        true
    }
}

impl AnalysisPass for UnusedBinding<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedBindingSettings::new();
        let lint = UnusedBinding::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
            settings,
        );
        lint.run(analysis_cache.contract_analysis)
    }
}

impl Lint for UnusedBinding<'_> {
    fn get_name() -> LintName {
        LintName::UnusedBinding
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        matches!(
            annotation.kind,
            AnnotationKind::Allow(WarningKind::UnusedBinding)
        )
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::UnusedBinding;
    use crate::analysis::linter::Lint;
    use crate::analysis::lints::unused_binding::BindingType;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedBinding::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    // Simple cases

    #[test]
    fn used_function_arg() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (increment (x uint))
                (+ x u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn unused_function_arg() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (increment (x uint))
                (+ u1 u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "x";
        let (expected_message, _) =
            UnusedBinding::make_diagnostic_strings(BindingType::FunctionArg, &var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_unused_function_arg_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_binding)]
            (define-read-only (increment (x uint))
                (+ u1 u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_let_binding() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (double (x uint))
                (let ((doubled (* x u2)))
                    doubled))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn unused_let_binding() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (double (x uint))
                (let ((doubled (* x u2)))
                    (* x u2)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "doubled";
        let (expected_message, _) =
            UnusedBinding::make_diagnostic_strings(BindingType::LetBinding, &var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_unused_let_binding_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (double (x uint))
                ;; #[allow(unused_binding)]
                (let ((doubled (* x u2)))
                    (* x u2)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    // TODO: Nested and complex cases
}
