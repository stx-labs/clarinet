//! Find unused traits imported with `use-trait`
//! A trait is considered unused if there is no public or read-only function parameter with the trait type

use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression, SymbolicExpressionType};
use clarity_types::types::TraitIdentifier;
use clarity_types::ClarityName;

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct UnusedTraitSettings {
    // TODO
}

impl UnusedTraitSettings {
    fn new() -> Self {
        Self {}
    }
}

pub struct UnusedTrait<'a> {
    clarity_version: ClarityVersion,
    _settings: UnusedTraitSettings,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
    level: Level,
    unused_traits: HashMap<&'a ClarityName, &'a SymbolicExpression>,
}

impl<'a> UnusedTrait<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
        settings: UnusedTraitSettings,
    ) -> UnusedTrait<'a> {
        Self {
            clarity_version,
            _settings: settings,
            level,
            annotations,
            active_annotation: None,
            unused_traits: HashMap::new(),
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

    /// Make diagnostic message and suggestion for unused private fn
    fn make_diagnostic_strings(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("imported trait `{name}` is never used"),
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

        for (name, expr) in &self.unused_traits {
            let (message, suggestion) = Self::make_diagnostic_strings(name);
            let diagnostic = self.make_diagnostic(expr, message, suggestion);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }

    fn process_param_type_expr(&mut self, param_type: &SymbolicExpressionType) {
        use SymbolicExpressionType::*;

        match param_type {
            TraitReference(name, _) => {
                self.unused_traits.remove(name);
            }
            List(exprs) => {
                for expr in exprs {
                    self.process_param_type_expr(&expr.expr);
                }
            }
            Field(_) | AtomValue(_) | Atom(_) | LiteralValue(_) => {}
        }
    }

    fn process_params(&mut self, params: &[TypedVar]) {
        for param in params {
            self.process_param_type_expr(&param.type_expr.expr);
        }
    }
}

impl<'a> ASTVisitor<'a> for UnusedTrait<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_use_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _trait_identifier: &clarity_types::types::TraitIdentifier,
    ) -> bool {
        self.set_active_annotation(&expr.span);

        if !self.allow() {
            self.unused_traits.insert(name, expr);
        }

        true
    }

    /// Override so that we only traverse the params, and not the entire function
    fn traverse_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.visit_define_read_only(expr, name, parameters, body)
    }

    fn visit_define_read_only(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        if let Some(params) = parameters {
            self.process_params(&params);
        }
        true
    }

    /// Override so that we only traverse the params, and not the entire function
    fn traverse_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.visit_define_public(expr, name, parameters, body)
    }

    fn visit_define_public(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        if let Some(params) = parameters {
            self.process_params(&params);
        }
        true
    }

    /// Skip, not relevant to this lint
    fn traverse_define_constant(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    /// Skip, not relevant to this lint
    fn traverse_define_private(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    /// Skip, not relevant to this lint
    fn traverse_define_nft(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _nft_type: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    /// Skip, not relevant to this lint
    fn traverse_define_ft(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _supply: Option<&'a SymbolicExpression>,
    ) -> bool {
        true
    }

    /// Skip, not relevant to this lint
    fn traverse_define_map(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _key_type: &'a SymbolicExpression,
        _value_type: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    /// Skip, not relevant to this lint
    fn traverse_define_data_var(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _data_type: &'a SymbolicExpression,
        _initial: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    /// Skip, not relevant to this lint
    fn traverse_impl_trait(
        &mut self,
        _expr: &'a SymbolicExpression,
        _trait_identifier: &TraitIdentifier,
    ) -> bool {
        true
    }

    fn visit_define_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        functions: &'a [SymbolicExpression],
    ) -> bool {
        panic!("visit trait {}", name);
    }
}

impl AnalysisPass for UnusedTrait<'_> {
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        _analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedTraitSettings::new();
        let lint = UnusedTrait::new(
            contract_analysis.clarity_version,
            annotations,
            level,
            settings,
        );
        lint.run(contract_analysis)
    }
}

impl Lint for UnusedTrait<'_> {
    fn get_name() -> LintName {
        LintName::UnusedTrait
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        matches!(
            annotation.kind,
            AnnotationKind::Allow(WarningKind::UnusedTrait)
        )
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::UnusedTrait;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedTrait::get_name(), Level::Warning);

        Session::new(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn used() {
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
    fn used_in_filter() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (is-even (x int))
                (is-eq (mod x 2) 0))

            (define-read-only (even-ints-to-ten)
                (filter is-even (list 0 1 2 3 4 5 6 7 8 9)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_in_fold() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (sum (x int) (y int))
                (+ x y))

            (define-read-only (sum-to-ten)
                (fold sum (list 0 1 2 3 4 5 6 7 8 9) 0))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_in_map() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (square (x int))
                (* x x))

            (define-read-only (squares-to-ten)
                (map square (list 0 1 2 3 4 5 6 7 8 9)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn not_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (square (x uint))
                (* x x))

            (define-read-only (cube (x uint))
                (* x (* x x)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let fn_name = "square";
        let (expected_message, _) = UnusedTrait::make_diagnostic_strings(&fn_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(fn_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_annotation() {
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
