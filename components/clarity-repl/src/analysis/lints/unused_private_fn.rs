use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct UnusedPrivateFnSettings {
    // TODO
}

impl UnusedPrivateFnSettings {
    fn new() -> Self {
        Self {}
    }
}

/// Data associated with a `define-data-var` variable
struct PrivateFnData<'a> {
    expr: &'a SymbolicExpression,
    /// Has this function been called?
    pub called: bool,
}

impl<'a> PrivateFnData<'a> {
    fn new(expr: &'a SymbolicExpression) -> Self {
        Self { expr, called: true }
    }
}

pub struct UnusedPrivateFn<'a> {
    clarity_version: ClarityVersion,
    _settings: UnusedPrivateFnSettings,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
    level: Level,
    private_fns: HashMap<&'a ClarityName, PrivateFnData<'a>>,
}

impl<'a> UnusedPrivateFn<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
        settings: UnusedPrivateFnSettings,
    ) -> UnusedPrivateFn<'a> {
        Self {
            clarity_version,
            _settings: settings,
            level,
            annotations,
            active_annotation: None,
            private_fns: HashMap::new(),
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        // Traverse the entire AST
        traverse(&mut self, &contract_analysis.expressions);

        /*
        let msg = self.private_fns.iter()
            .map(|(k, v)| format!("  {k}({v})", v=v.called))
            .collect::<Vec<_>>()
            .join("\n");
        panic!("Function map:\n{}", msg);
        */

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

    /// Make diagnostic message and suggestion for unused private fn
    fn make_diagnostic_strings(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("private function `{name}` is never used"),
            Some("Remove this expression".to_string()),
        )
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
        let mut diagnostics = vec![];

        for (name, data) in &self.private_fns {
            let (message, suggestion) = match data.called {
                true => continue,
                false => Self::make_diagnostic_strings(name),
            };
            let diagnostic = self.make_diagnostic(data.expr, message, suggestion);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a> ASTVisitor<'a> for UnusedPrivateFn<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _parameters: Option<Vec<analysis::ast_visitor::TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);

        if !self.allow() {
            self.private_fns.insert(name, PrivateFnData::new(expr));
        }

        true
    }

    fn visit_call_user_defined(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _args: &'a [SymbolicExpression],
    ) -> bool {
        _ = self
            .private_fns
            .get_mut(name)
            .map(|data| data.called = true);
        true
    }
}

impl AnalysisPass for UnusedPrivateFn<'_> {
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        _analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedPrivateFnSettings::new();
        let lint = UnusedPrivateFn::new(
            contract_analysis.clarity_version,
            annotations,
            level,
            settings,
        );
        lint.run(contract_analysis)
    }
}

impl Lint for UnusedPrivateFn<'_> {
    fn get_name() -> LintName {
        LintName::UnusedPrivateFn
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        matches!(
            annotation.kind,
            AnnotationKind::Allow(WarningKind::UnusedPrivateFn)
        )
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::UnusedPrivateFn;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedPrivateFn::get_name(), Level::Warning);

        Session::new(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn private_fn_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (square (x uint))
                (* x x))

            (define-read-only (cube (x uint))
                (* x (square x)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn private_fn_not_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (square (x uint))
                (* x x))

            (define-read-only (cube (x uint))
                (* x (* x x)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let fn_name = "square";
        let (expected_message, _) = UnusedPrivateFn::make_diagnostic_strings(&fn_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(fn_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_comment() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_private_fn)]
            (define-private (square (x uint))
                (* x x))

            (define-read-only (cube (x uint))
                (* x (* x x)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
