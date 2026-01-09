use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct UnusedLocalSettings {
    // TODO
}

impl UnusedLocalSettings {
    fn new() -> Self {
        Self {}
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum LocalVarType {
    FunctionArg,
    LetBinding,
}

/// Unique identifier for each local variable in a contract
#[derive(PartialEq, Eq)]
struct LocalVarDefinition<'a> {
    var_type: LocalVarType,
    name: &'a ClarityName,
    expr: &'a SymbolicExpression,
}

impl Hash for LocalVarDefinition<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.expr.span.hash(state);
    }
}

/// Data associated with a local variable
struct LocalVarData<'a> {
    expr: &'a SymbolicExpression,
    /// Has this variable been referenced?
    pub used: bool,
}

impl<'a> LocalVarData<'a> {
    fn new(expr: &'a SymbolicExpression) -> Self {
        Self { expr, used: false }
    }
}

pub struct UnusedLocal<'a> {
    clarity_version: ClarityVersion,
    _settings: UnusedLocalSettings,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
    level: Level,
    /// Names of all `let` bindings and function args currently in scope
    active_bindings: HashMap<&'a ClarityName, LocalVarData<'a>>,
    /// Variables which went out of scope without being referenced
    unused: HashSet<LocalVarDefinition<'a>>,
}

impl<'a> UnusedLocal<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
        settings: UnusedLocalSettings,
    ) -> UnusedLocal<'a> {
        Self {
            clarity_version,
            _settings: settings,
            level,
            annotations,
            active_annotation: None,
            active_bindings: HashMap::new(),
            unused: HashSet::new(),
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
        var_type: LocalVarType,
        name: &ClarityName,
    ) -> (String, Option<String>) {
        let msg = match var_type {
            LocalVarType::FunctionArg => format!("function parameter `{name}` is never used"),
            LocalVarType::LetBinding => format!("`let` binding `{name}` is never used"),
        };
        (msg, Some("Remove this expression".to_string()))
    }

    fn make_diagnostic(
        &self,
        expr: &'a SymbolicExpression,
        message: String,
        suggestion: Option<String>,
    ) -> Diagnostic {
        Diagnostic {
            level: self.level.clone(),
            message,
            spans: vec![expr.span.clone()],
            suggestion,
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::with_capacity(self.unused.len());

        for var in &self.unused {
            let (message, suggestion) = Self::make_diagnostic_strings(var.var_type, var.name);
            let diagnostic = self.make_diagnostic(var.expr, message, suggestion);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a> ASTVisitor<'a> for UnusedLocal<'a> {
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
            self.active_bindings.insert(name, LocalVarData::new(expr));
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
            if let Some(var) = self.active_bindings.remove(name) {
                if !var.used {
                    let unused_var_definition = LocalVarDefinition {
                        var_type: LocalVarType::LetBinding,
                        name,
                        expr: var.expr,
                    };
                    self.unused.insert(unused_var_definition);
                }
            }
        }
        res
    }

    fn visit_atom(&mut self, _expr: &'a SymbolicExpression, atom: &'a ClarityName) -> bool {
        if let Some(var) = self.active_bindings.get_mut(atom) {
            var.used = true;
        };
        true
    }
}

impl AnalysisPass for UnusedLocal<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedLocalSettings::new();
        let lint = UnusedLocal::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
            settings,
        );
        lint.run(analysis_cache.contract_analysis)
    }
}

impl Lint for UnusedLocal<'_> {
    fn get_name() -> LintName {
        LintName::UnusedLocal
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        matches!(
            annotation.kind,
            AnnotationKind::Allow(WarningKind::UnusedLocal)
        )
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::UnusedLocal;
    use crate::analysis::linter::Lint;
    use crate::analysis::lints::unused_local::LocalVarType;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedLocal::get_name(), Level::Warning);

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
            UnusedLocal::make_diagnostic_strings(LocalVarType::FunctionArg, &var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_unused_function_arg_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_local)]
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
            UnusedLocal::make_diagnostic_strings(LocalVarType::LetBinding, &var_name.into());

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
                ;; #[allow(unused_local)]
                (let ((doubled (* x u2)))
                    (* x u2)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    // TODO: Nested and complex cases
}
