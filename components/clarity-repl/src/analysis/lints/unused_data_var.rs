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

struct UnusedDataVarSettings {
    // TODO
}

impl UnusedDataVarSettings {
    fn new() -> Self {
        Self {}
    }
}

/// Data associated with a `define-data-var` variable
struct DataVarData<'a> {
    expr: &'a SymbolicExpression,
    /// Has this variable been written to?
    pub written_to: bool,
    /// Has this variable been read from?
    pub read_from: bool,
}

impl<'a> DataVarData<'a> {
    fn new(expr: &'a SymbolicExpression) -> Self {
        Self {
            expr,
            written_to: false,
            read_from: false,
        }
    }
}

pub struct UnusedDataVar<'a> {
    clarity_version: ClarityVersion,
    _settings: UnusedDataVarSettings,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
    level: Level,
    data_vars: HashMap<&'a ClarityName, DataVarData<'a>>,
}

impl<'a> UnusedDataVar<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
        settings: UnusedDataVarSettings,
    ) -> UnusedDataVar<'a> {
        Self {
            clarity_version,
            _settings: settings,
            level,
            annotations,
            active_annotation: None,
            data_vars: HashMap::new(),
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

    /// Make diagnostic message and suggestion for variable which is never written to
    fn make_diagnostic_strings_unset(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("data variable `{name}` never modified"),
            Some("Declare using `declare-constant`".to_string()),
        )
    }

    /// Make diagnostic message and suggestion for variable which is never read from
    fn make_diagnostic_strings_unread(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("data variable `{name}` never read"),
            Some("Remove this expression".to_string()),
        )
    }

    /// Make diagnostic message and suggestion for variable which is never used
    fn make_diagnostic_strings_unused(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("data variable `{name}` never used"),
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

        for (name, data) in &self.data_vars {
            let (message, suggestion) = match (data.read_from, data.written_to) {
                (true, true) => continue,
                (true, false) => Self::make_diagnostic_strings_unset(name),
                (false, true) => Self::make_diagnostic_strings_unread(name),
                (false, false) => Self::make_diagnostic_strings_unused(name),
            };
            let diagnostic = self.make_diagnostic(data.expr, message, suggestion);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a> ASTVisitor<'a> for UnusedDataVar<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_data_var(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _data_type: &'a SymbolicExpression,
        _initial: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);

        if !self.allow() {
            self.data_vars.insert(name, DataVarData::new(expr));
        }

        true
    }

    fn visit_var_set(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _value: &'a SymbolicExpression,
    ) -> bool {
        _ = self
            .data_vars
            .get_mut(name)
            .map(|data| data.written_to = true);
        true
    }

    fn visit_var_get(&mut self, _expr: &'a SymbolicExpression, name: &'a ClarityName) -> bool {
        _ = self
            .data_vars
            .get_mut(name)
            .map(|data| data.read_from = true);
        true
    }
}

impl AnalysisPass for UnusedDataVar<'_> {
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        _analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedDataVarSettings::new();
        let lint = UnusedDataVar::new(
            contract_analysis.clarity_version,
            annotations,
            level,
            settings,
        );
        lint.run(contract_analysis)
    }
}

impl Lint for UnusedDataVar<'_> {
    fn get_name() -> LintName {
        LintName::UnusedDataVar
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        matches!(
            annotation.kind,
            AnnotationKind::Allow(WarningKind::UnusedDataVar)
        )
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::UnusedDataVar;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedDataVar::get_name(), Level::Warning);

        Session::new(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)

            (define-public (increment)
                (ok (var-set counter (+ (var-get counter) u1))))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn not_set() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)

            (define-public (read-counter)
                (ok (var-get counter)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "counter";
        let (expected_message, _) = UnusedDataVar::make_diagnostic_strings_unset(&var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn not_read() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)

            (define-public (set (val uint))
                (ok (var-set counter val)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "counter";
        let (expected_message, _) = UnusedDataVar::make_diagnostic_strings_unread(&var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn not_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)

            (define-public (read-counter)
                (ok u5))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "counter";
        let (expected_message, _) = UnusedDataVar::make_diagnostic_strings_unused(&var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_data_var)]
            (define-data-var counter uint u0)

            (define-public (read-counter)
                (ok u5))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
