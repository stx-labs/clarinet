use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct UnusedConstSettings {
    level: Level,
}

impl UnusedConstSettings {
    fn new(level: Level) -> Self {
        Self { level }
    }
}

pub struct UnusedConst<'a> {
    clarity_version: ClarityVersion,
    settings: UnusedConstSettings,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
    /// Map of constants not yet used
    unused_constants: HashMap<&'a ClarityName, &'a SymbolicExpression>,
}

impl<'a> UnusedConst<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        settings: UnusedConstSettings,
    ) -> UnusedConst<'a> {
        Self {
            clarity_version,
            settings,
            annotations,
            active_annotation: None,
            unused_constants: HashMap::new(),
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

    // Check if the expression is annotated with `allow(<lint_name>)`
    fn allow(&self) -> bool {
        self.active_annotation
            .map(|idx| {
                matches!(
                    self.annotations[idx].kind,
                    AnnotationKind::Allow(WarningKind::UnusedConst)
                )
            })
            .unwrap_or(false)
    }

    fn make_diagnostic_message(name: &ClarityName) -> String {
        format!("constant `{name}` never used")
    }

    fn make_diagnostic(&self, expr: &'a SymbolicExpression, message: String) -> Diagnostic {
        Diagnostic {
            level: self.settings.level.clone(),
            message,
            spans: vec![expr.span.clone()],
            suggestion: Some("Remove this expression".to_string()),
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        for (name, expr) in &self.unused_constants {
            let message = Self::make_diagnostic_message(name);
            let diagnostic = self.make_diagnostic(expr, message);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a> ASTVisitor<'a> for UnusedConst<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_constant(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _value: &'a SymbolicExpression,
    ) -> bool {
        self.process_annotations(&expr.span);

        if !self.allow() {
            self.unused_constants.insert(name, expr);
        }

        true
    }

    fn visit_atom(&mut self, _expr: &'a SymbolicExpression, atom: &'a ClarityName) -> bool {
        self.unused_constants.remove(atom);
        true
    }
}

impl AnalysisPass for UnusedConst<'_> {
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        _analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        settings: &analysis::Settings,
    ) -> AnalysisResult {
        let level = settings
            .lints
            .get(&LintName::UnusedConst)
            .cloned()
            .unwrap_or(Level::Warning);
        let settings = UnusedConstSettings::new(level);
        let lint = UnusedConst::new(contract_analysis.clarity_version, annotations, settings);
        lint.run(contract_analysis)
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use indoc::indoc;

    use crate::analysis::lints::UnusedConst;
    use crate::analysis::LintName;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn get_session() -> Session {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .enable_lint(LintName::UnusedConst, Level::Warning);

        Session::new(settings)
    }

    #[test]
    fn const_used() {
        let mut session = get_session();

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant MINUTES_PER_HOUR u60)

            (define-read-only (hours-to-minutes (hours uint))
                (* hours MINUTES_PER_HOUR))
        ").to_string();

        let (_, result) = session
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet");

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn const_used_before_declaration() {
        let mut session = get_session();

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (hours-to-minutes (hours uint))
                (* hours MINUTES_PER_HOUR))

            (define-constant MINUTES_PER_HOUR u60)
        ").to_string();

        let (_, result) = session
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet");

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn const_not_used() {
        let mut session = get_session();

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant MINUTES_PER_HOUR u60)

            (define-read-only (hours-to-minutes (hours uint))
                (* hours u60))
        ").to_string();

        let (output, result) = session
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet");

        let const_name = "MINUTES_PER_HOUR";
        let expected_message = UnusedConst::make_diagnostic_message(&const_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(const_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_comment() {
        let mut session = get_session();

        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_const)]
            (define-constant MINUTES_PER_HOUR u60)

            (define-read-only (hours-to-minutes (hours uint))
                (* hours u60))
        ").to_string();

        let (_, result) = session
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet");

        assert_eq!(result.diagnostics.len(), 0);
    }
}
