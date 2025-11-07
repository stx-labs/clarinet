use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::{self, AnalysisPass, AnalysisResult, Lint};

struct NoopCheckerSettings {
    level: Level,
}

impl NoopCheckerSettings {
    fn new(level: Level) -> Self {
        Self { level }
    }
}

pub struct NoopChecker<'a> {
    clarity_version: ClarityVersion,
    // Map expression ID to a generated diagnostic
    settings: NoopCheckerSettings,
    diagnostics: HashMap<u64, Vec<Diagnostic>>,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
}

impl<'a> NoopChecker<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        settings: NoopCheckerSettings,
    ) -> NoopChecker<'a> {
        Self {
            clarity_version,
            settings,
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
    fn process_annotations(&mut self, span: &Span) {
        self.active_annotation = None;

        for (i, annotation) in self.annotations.iter().enumerate() {
            if annotation.span.start_line == (span.start_line - 1) {
                self.active_annotation = Some(i);
                return;
            } else if annotation.span.start_line >= span.start_line {
                // The annotations are ordered by span, so if we have passed
                // the target line, return.
                return;
            }
        }
    }

    // Check if the expression is annotated with `allow(noop)`
    fn allow_noop(&self) -> bool {
        if let Some(idx) = self.active_annotation {
            let annotation = &self.annotations[idx];
            return matches!(annotation.kind, AnnotationKind::Allow(WarningKind::Noop));
        }
        false
    }

    fn add_noop_diagnostic(&mut self, expr: &'a SymbolicExpression, message: &str) {
        let diagnostic = Diagnostic {
            level: self.settings.level.clone(),
            message: message.to_string(),
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
        self.process_annotations(&expr.span);

        if self.allow_noop() {
            return;
        }

        if operands.len() < 2 {
            self.add_noop_diagnostic(
                expr,
                &format!(
                    "`{}` with fewer than 2 operands has no effect",
                    func.get_name()
                ),
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
        self.check_bin_op(expr, func, operands);
        true
    }

    fn visit_arithmetic(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        self.check_bin_op(expr, func, operands);
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
        contract_analysis: &mut ContractAnalysis,
        _analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        settings: &analysis::Settings,
    ) -> AnalysisResult {
        let level = settings
            .lints
            .get(&Lint::Noop)
            .cloned()
            .unwrap_or(Level::Warning);
        let settings = NoopCheckerSettings::new(level);
        let checker = NoopChecker::new(contract_analysis.clarity_version, annotations, settings);
        checker.run(contract_analysis)
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use indoc::indoc;

    use crate::analysis::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    #[test]
    fn single_operand_equals() {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(Lint::Noop, Level::Warning);
        let mut session = Session::new(settings);
        let snippet = indoc!(
            "
            (define-public (test-func)
                (begin
                    (is-eq true)
                    (ok true)
                )
            )
        "
        )
        .to_string();

        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, result)) => {
                assert_eq!(result.diagnostics.len(), 1);
                assert!(output[0].contains("warning:"));
                assert!(output[0].contains("`is-eq` with fewer than 2 operands has no effect"));
            }
            _ => panic!("Expected warning"),
        };
    }

    #[test]
    fn single_operand_add() {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(Lint::Noop, Level::Warning);
        let mut session = Session::new(settings);
        let snippet = indoc!(
            "
            (define-public (test-func)
                (begin
                    (+ u1)
                    (ok true)
                )
            )
        "
        )
        .to_string();

        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, result)) => {
                assert_eq!(result.diagnostics.len(), 1);
                assert!(output[0].contains("warning:"));
                assert!(output[0].contains("`+` with fewer than 2 operands has no effect"));
            }
            _ => panic!("Expected warning"),
        };
    }

    #[test]
    fn single_operand_logical() {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(Lint::Noop, Level::Warning);
        let mut session = Session::new(settings);
        let snippet = indoc!(
            "
            (define-public (test-func)
                (begin
                    (and true)
                    (ok true)
                )
            )
        "
        )
        .to_string();

        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((output, result)) => {
                assert_eq!(result.diagnostics.len(), 1);
                assert!(output[0].contains("warning:"));
                assert!(output[0].contains("`and` with fewer than 2 operands has no effect"));
            }
            _ => panic!("Expected warning"),
        };
    }

    #[test]
    fn allow_noop_annotation() {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(Lint::Noop, Level::Warning);
        let mut session = Session::new(settings);
        let snippet = indoc!(
            "
            (define-public (test-func)
                (begin
                    ;; #[allow(noop)]
                    (is-eq true)
                    (ok true)
                )
            )
        "
        )
        .to_string();

        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            _ => panic!("Expected warning"),
        };
    }
}
