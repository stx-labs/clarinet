//! Find unused traits imported with `use-trait`
//!
//! A trait is considered unused if there is no public or read-only function parameter with the trait type

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression, SymbolicExpressionType};
use clarity_types::types::TraitIdentifier;
use clarity_types::ClarityName;
use hashbrown::HashMap;

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::is_explicitly_unused;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct UnusedTraitSettings {
    // TODO
}

impl UnusedTraitSettings {
    fn new() -> Self {
        Self {}
    }
}

/// Data on trait usage
struct TraitUsage<'a> {
    /// Keep track of where trait was imported with `use-trait`
    expr: &'a SymbolicExpression,
    /// Has this trait appeared a function signature inside `declare-trait`?
    pub declare_trait: bool,
    /// Has this trait appeared in the arg list of a public function?
    pub public_fn: bool,
    /// Has this trait appeared in the arg list of a read-only function?
    pub read_only_fn: bool,
    /// Has this trait appeared in the arg list of a private function?
    pub private_fn: bool,
}

pub struct UnusedTrait<'a> {
    clarity_version: ClarityVersion,
    _settings: UnusedTraitSettings,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
    level: Level,
    traits: HashMap<&'a ClarityName, TraitUsage<'a>>,
}

impl<'a> TraitUsage<'a> {
    fn new(expr: &'a SymbolicExpression) -> Self {
        Self {
            expr,
            declare_trait: false,
            public_fn: false,
            read_only_fn: false,
            private_fn: false,
        }
    }
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
            traits: HashMap::new(),
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

    /// Make diagnostic message and suggestion for unused trait
    fn make_diagnostic_strings_unused(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("imported trait `{name}` is never used"),
            Some("Remove this expression".to_string()),
        )
    }

    /// Make diagnostic message and suggestion if trait is only used in private function
    fn make_diagnostic_strings_private_fn_only(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("imported trait `{name}` only appears in private functions, so cannot be used"),
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

        for (name, usage) in &self.traits {
            if is_explicitly_unused(name) {
                continue;
            }
            let used_in = (
                usage.declare_trait,
                usage.public_fn,
                usage.read_only_fn,
                usage.private_fn,
            );
            let (message, suggestion) = match used_in {
                (true, _, _, _) | (_, true, _, _) | (_, _, true, _) => continue,
                (false, false, false, true) => Self::make_diagnostic_strings_private_fn_only(name),
                (false, false, false, false) => Self::make_diagnostic_strings_unused(name),
            };
            let diagnostic = self.make_diagnostic(usage.expr, message, suggestion);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }

    /// Search a symbolic expression for a trait object, and fun `func` on associated `TraitUsage` struct`
    fn process_symbolic_expr(
        &mut self,
        expr: &SymbolicExpression,
        func: fn(&mut TraitUsage) -> (),
    ) {
        use SymbolicExpressionType::*;

        match &expr.expr {
            TraitReference(name, _) => {
                self.traits.get_mut(name).map(func);
            }
            List(exprs) => {
                for expr in exprs {
                    self.process_symbolic_expr(expr, func);
                }
            }
            Field(_) | AtomValue(_) | Atom(_) | LiteralValue(_) => {}
        }
    }

    /// Search parameter list for a trait object, and fun `func` on associated `TraitUsage` struct`
    fn process_param_list(&mut self, params: &[TypedVar], func: fn(&mut TraitUsage) -> ()) {
        for param in params {
            self.process_symbolic_expr(param.type_expr, func);
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
            self.traits.insert(name, TraitUsage::new(expr));
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
            self.process_param_list(&params, |usage| usage.read_only_fn = true);
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
            self.process_param_list(&params, |usage| usage.public_fn = true);
        }
        true
    }

    fn traverse_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.visit_define_private(expr, name, parameters, body)
    }

    fn visit_define_private(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        if let Some(params) = parameters {
            self.process_param_list(&params, |usage| usage.private_fn = true);
        }
        true
    }

    fn visit_define_trait(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        functions: &'a [SymbolicExpression],
    ) -> bool {
        for expr in functions {
            self.process_symbolic_expr(expr, |usage| usage.declare_trait = true);
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
}

impl AnalysisPass for UnusedTrait<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedTraitSettings::new();
        let lint = UnusedTrait::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
            settings,
        );
        lint.run(analysis_cache.contract_analysis)
    }
}

impl Lint for UnusedTrait<'_> {
    fn get_name() -> LintName {
        LintName::UnusedTrait
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::UnusedTrait)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::types::StacksEpochId;
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::UnusedTrait;
    use crate::analysis::linter::{Lint, LintLevel};
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;
    use crate::test_fixtures::clarity_contract::ClarityContractBuilder;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .set_lint_level(UnusedTrait::get_name(), LintLevel::Warning);

        // We need a trait in a separate contract for these tests
        #[rustfmt::skip]
        let trait_source = indoc!("
            (define-trait token-trait (
                (get-supply () (response uint uint))
                (get-balance (principal) (response uint uint))
            ))
        ");

        let mut session = Session::new_without_boot_contracts(settings);
        session.update_epoch(StacksEpochId::Epoch33);

        let trait_contract = ClarityContractBuilder::new()
            .name("token-trait")
            .code_source(trait_source.to_owned())
            .build();

        session
            .interpreter
            .run(&trait_contract, None, false, None)
            .expect("Invalid trait contract");

        session
            .formatted_interpretation(snippet, Some("unit-test".to_owned()), false, None)
            .expect("Invalid code snippet")
    }

    /// Currently a trait cannot define a read-only function, so trait args cannot be used by other read-only functions
    /// There are proposals to change this though, so enable this test if and when that changes
    #[ignore]
    #[test]
    fn used_in_read_only_fn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token-trait .token-trait.token-trait)

            (define-read-only (get-token-balance (token <token-trait>) (p principal))
                (contract-call? token get-balance p))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_in_public_fn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token-trait .token-trait.token-trait)

            (define-public (get-token-balance (token <token-trait>) (p principal))
                (contract-call? token get-balance p))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_in_public_fn_nested_optional() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token-trait .token-trait.token-trait)

            (define-public (get-token-balance (token-opt (optional <token-trait>)) (p principal))
                (let ((token (unwrap-panic token-opt)))
                    (contract-call? token get-balance p)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_in_public_fn_nested_tuple() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (use-trait token-trait .token-trait.token-trait)

            (define-public (get-token-balance (args { t: <token-trait>, p: principal }))
                (let ((token (get t args)))
                    (contract-call? token get-balance (get p args))))
        "#).to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// If trait is used *only* in an unused private function, it's unreachable, so consider it unused
    #[test]
    fn used_in_unused_private_fn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token-trait .token-trait.token-trait)

            (define-private (get-token-balance (token <token-trait>) (p principal))
                (contract-call? token get-balance p))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let trait_name = "token-trait";
        let (expected_message, _) =
            UnusedTrait::make_diagnostic_strings_private_fn_only(&trait_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(trait_name));
        assert!(output[0].contains(&expected_message));
    }

    /// If trait is used *only* in private function, even if the private function is called by a public function, it's still unusable
    #[test]
    fn used_in_used_private_fn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token-trait .token-trait.token-trait)

            (define-public (get-token-balance (p principal))
                (get-token-balance-private none p))

            (define-private (get-token-balance-private (token-opt (optional <token-trait>)) (p principal))
                (let ((token (unwrap! token-opt (err u1))))
                    (contract-call? token get-balance p)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let trait_name = "token-trait";
        let (expected_message, _) =
            UnusedTrait::make_diagnostic_strings_private_fn_only(&trait_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(trait_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn used_in_declare_trait() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token-trait .token-trait.token-trait)

            (define-trait token-proxy-trait (
                (get-supply (<token-trait>) (response uint uint))
                (get-balance (<token-trait> principal) (response uint uint))
            ))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_in_declare_trait_nested_optional() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token-trait .token-trait.token-trait)

            (define-trait token-proxy-trait (
                (get-supply ((optional <token-trait>)) (response uint uint))
                (get-balance ((optional <token-trait>) principal) (response uint uint))
            ))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_in_declare_trait_nested_tuple() {
        #[rustfmt::skip]
        let snippet = indoc!(r#"
            (use-trait token-trait .token-trait.token-trait)

            (define-trait token-proxy-trait (
                (get-supply ({ t: <token-trait> }) (response uint uint))
                (get-balance ({ t: <token-trait>, p: principal }) (response uint uint))
            ))
        "#).to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn not_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token-trait .token-trait.token-trait)

            (define-read-only (get-token-amount (p principal))
                u100)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let trait_name = "token-trait";
        let (expected_message, _) = UnusedTrait::make_diagnostic_strings_unused(&trait_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(trait_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_trait)]
            (use-trait token-trait .token-trait.token-trait)

            (define-read-only (get-token-amount (p principal))
                u100)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_with_naming_convention() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token-trait_ .token-trait.token-trait)

            (define-read-only (get-token-amount (p principal))
                u100)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
