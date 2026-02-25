//! Lint to find public functions that could be declared read-only
//!
//! A public function can be read-only if it does not modify any blockchain state:
//! - No `var-set`
//! - No `map-set`, `map-insert`, or `map-delete`
//! - No `stx-transfer?`, `stx-burn?`
//! - No `ft-mint?`, `ft-burn?`, `ft-transfer?`
//! - No `nft-mint?`, `nft-burn?`, `nft-transfer?`
//! - No `contract-call?` to a public (non-read-only) function
//! - No dynamic `contract-call?` (trait-based dispatch, conservatively assumed to have side effects)

use std::collections::{HashMap, HashSet};

use clarity::types::StacksEpochId;
use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity::vm::{ClarityName, ClarityVersion, SymbolicExpression};
use clarity_types::types::QualifiedContractIdentifier;

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct UnnecessaryPublic<'a, 'b, 'c> {
    clarity_version: ClarityVersion,
    diagnostics: Vec<Diagnostic>,
    annotations: &'a Vec<Annotation>,
    analysis_db: &'b mut AnalysisDatabase<'c>,
    epoch: StacksEpochId,
    level: Level,
    active_annotation: Option<usize>,
    /// Set to `true` when traversing a function body that contains a state-modifying op
    found_side_effect: bool,
    /// Names of functions (public or private) whose bodies contain state-modifying operations,
    /// either directly or transitively via calls to other functions with side effects.
    ///
    /// --- IMPORTANT ---
    /// This assumes `ASTVisitor` traverses called functions before the functions that call them.
    /// This should always be the case as the Clarity interpreter *should* topologically sort functions by dependency before traversal
    /// -----------------
    fns_with_side_effects: HashSet<String>,
}

impl<'a, 'b, 'c> UnnecessaryPublic<'a, 'b, 'c> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        analysis_db: &'b mut AnalysisDatabase<'c>,
        epoch: StacksEpochId,
        level: Level,
    ) -> UnnecessaryPublic<'a, 'b, 'c> {
        Self {
            clarity_version,
            level,
            diagnostics: Vec::new(),
            annotations,
            analysis_db,
            epoch,
            active_annotation: None,
            found_side_effect: false,
            fns_with_side_effects: HashSet::new(),
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        traverse(&mut self, &contract_analysis.expressions);
        Ok(self.diagnostics)
    }

    fn set_active_annotation(&mut self, span: &Span) {
        self.active_annotation = get_index_of_span(self.annotations, span);
    }

    fn allow(&self) -> bool {
        self.active_annotation
            .map(|idx| Self::match_allow_annotation(&self.annotations[idx]))
            .unwrap_or(false)
    }
}

impl<'a, 'b, 'c> ASTVisitor<'a> for UnnecessaryPublic<'a, 'b, 'c> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    // Traverse private function bodies to record side effects (but don't emit diagnostics)
    fn traverse_define_private(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.found_side_effect = false;
        self.traverse_expr(body);

        if self.found_side_effect {
            self.fns_with_side_effects.insert(name.to_string());
        }

        true
    }

    // Skip read-only function bodies, they're already read-only
    fn traverse_define_read_only(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_constant(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_nft(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _nft_type: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_ft(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _supply: Option<&'a SymbolicExpression>,
    ) -> bool {
        true
    }

    fn traverse_define_map(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _key_type: &'a SymbolicExpression,
        _value_type: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_data_var(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _data_type: &'a SymbolicExpression,
        _initial: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_trait(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _functions: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.found_side_effect = false;

        // Traverse the body to detect state-modifying operations.
        // If a side effect is found, traversal short-circuits via `false` returns.
        self.traverse_expr(body);

        if self.found_side_effect {
            self.fns_with_side_effects.insert(name.to_string());
        } else {
            self.set_active_annotation(&expr.span);
            if !self.allow() {
                self.diagnostics.push(Diagnostic {
                    level: self.level.clone(),
                    message: format!(
                        "function `{}` could be declared as `define-read-only`",
                        name,
                    ),
                    spans: vec![expr.span.clone()],
                    suggestion: Some("Replace `define-public` with `define-read-only`".to_string()),
                });
            }
        }

        true
    }

    // --- State-modifying operations ---
    // Each sets the flag and returns `false` to short-circuit further body traversal.

    fn visit_var_set(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _value: &'a SymbolicExpression,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_map_set(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        _value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_map_insert(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        _value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_map_delete(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_stx_burn(
        &mut self,
        _expr: &'a SymbolicExpression,
        _amount: &'a SymbolicExpression,
        _sender: &'a SymbolicExpression,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_stx_transfer(
        &mut self,
        _expr: &'a SymbolicExpression,
        _amount: &'a SymbolicExpression,
        _sender: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
        _memo: Option<&'a SymbolicExpression>,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_ft_burn(
        &mut self,
        _expr: &'a SymbolicExpression,
        _token: &'a ClarityName,
        _amount: &'a SymbolicExpression,
        _sender: &'a SymbolicExpression,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_ft_transfer(
        &mut self,
        _expr: &'a SymbolicExpression,
        _token: &'a ClarityName,
        _amount: &'a SymbolicExpression,
        _sender: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_ft_mint(
        &mut self,
        _expr: &'a SymbolicExpression,
        _token: &'a ClarityName,
        _amount: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_nft_burn(
        &mut self,
        _expr: &'a SymbolicExpression,
        _token: &'a ClarityName,
        _identifier: &'a SymbolicExpression,
        _sender: &'a SymbolicExpression,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_nft_transfer(
        &mut self,
        _expr: &'a SymbolicExpression,
        _token: &'a ClarityName,
        _identifier: &'a SymbolicExpression,
        _sender: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_nft_mint(
        &mut self,
        _expr: &'a SymbolicExpression,
        _token: &'a ClarityName,
        _identifier: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        self.found_side_effect = true;
        false
    }

    fn visit_call_user_defined(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _args: &'a [SymbolicExpression],
    ) -> bool {
        if self.fns_with_side_effects.contains(name.as_str()) {
            self.found_side_effect = true;
            return false;
        }
        true
    }

    fn visit_static_contract_call(
        &mut self,
        _expr: &'a SymbolicExpression,
        contract_identifier: &'a QualifiedContractIdentifier,
        function_name: &'a ClarityName,
        _args: &'a [SymbolicExpression],
    ) -> bool {
        // If the called function is read-only, it has no side effects
        let is_read_only = self
            .analysis_db
            .get_read_only_function_type(contract_identifier, function_name, &self.epoch)
            .ok()
            .flatten()
            .is_some();

        if !is_read_only {
            self.found_side_effect = true;
            return false;
        }
        true
    }

    fn visit_dynamic_contract_call(
        &mut self,
        _expr: &'a SymbolicExpression,
        _trait_ref: &'a SymbolicExpression,
        _function_name: &'a ClarityName,
        _args: &'a [SymbolicExpression],
    ) -> bool {
        self.found_side_effect = true;
        false
    }
}

impl AnalysisPass for UnnecessaryPublic<'_, '_, '_> {
    fn run_pass(
        analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let checker = UnnecessaryPublic::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            analysis_db,
            analysis_cache.contract_analysis.epoch,
            level,
        );
        checker.run(analysis_cache.contract_analysis)
    }
}

impl Lint for UnnecessaryPublic<'_, '_, '_> {
    fn get_name() -> LintName {
        LintName::UnnecessaryPublic
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::UnnecessaryPublic)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use clarity_types::diagnostic::Level;
    use indoc::indoc;

    use super::UnnecessaryPublic;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;
    use crate::test_fixtures::clarity_contract::ClarityContractBuilder;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnnecessaryPublic::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    fn run_snippet_with_other_contract(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnnecessaryPublic::get_name(), Level::Warning);

        let mut session = Session::new_without_boot_contracts(settings);

        #[rustfmt::skip]
        let source = indoc!("
            (define-public (do-something)
                (ok true))

            (define-read-only (get-value)
                u1)
        ")
        .to_string();

        let other_contract = ClarityContractBuilder::new()
            .name("other-contract")
            .code_source(source)
            .build();

        session
            .interpreter
            .run(&other_contract, None, false, None)
            .expect("Invalid helper contract");

        session
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn warn_on_public_with_no_side_effects() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (get-value)
                (ok u1)
            )
        ")
        .to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains("could be declared as `define-read-only`"));
    }

    #[test]
    fn no_warn_on_public_with_var_set() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)
            (define-public (increment)
                (begin
                    (var-set counter (+ (var-get counter) u1))
                    (ok true)
                )
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_map_set() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map balances principal uint)
            (define-public (set-balance (who principal) (amount uint))
                (begin
                    (map-set balances who amount)
                    (ok true)
                )
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_map_insert() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map balances principal uint)
            (define-public (init-balance (who principal))
                (begin
                    (map-insert balances who u0)
                    (ok true)
                )
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_map_delete() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map balances principal uint)
            (define-public (remove-balance (who principal))
                (begin
                    (map-delete balances who)
                    (ok true)
                )
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_stx_transfer() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (send-stx (amount uint) (to principal))
                (stx-transfer? amount tx-sender to)
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_stx_burn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (burn-stx (amount uint))
                (stx-burn? amount tx-sender)
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_ft_mint() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-fungible-token my-token)
            (define-public (mint (amount uint) (to principal))
                (ft-mint? my-token amount to)
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_ft_burn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-fungible-token my-token)
            (define-public (burn (amount uint))
                (ft-burn? my-token amount tx-sender)
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_ft_transfer() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-fungible-token my-token)
            (define-public (transfer (amount uint) (to principal))
                (ft-transfer? my-token amount tx-sender to)
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_nft_mint() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-non-fungible-token my-nft uint)
            (define-public (mint (id uint) (to principal))
                (nft-mint? my-nft id to)
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_nft_burn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-non-fungible-token my-nft uint)
            (define-public (burn (id uint))
                (nft-burn? my-nft id tx-sender)
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_with_nft_transfer() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-non-fungible-token my-nft uint)
            (define-public (transfer (id uint) (to principal))
                (nft-transfer? my-nft id tx-sender to)
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_public_calling_public_fn_with_side_effects() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var count uint u0)
            (define-public (increment2)
                (increment)
            )
            (define-public (increment)
                (ok (var-set count (+ (var-get count) u1)))
            )
        ")
        .to_string();

        let (_, result) = run_snippet_with_other_contract(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn warn_on_public_calling_public_fn_with_no_side_effects() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (increment2)
                (increment)
            )
            (define-public (increment)
                (ok true)
            )
        ")
        .to_string();

        let (_, result) = run_snippet_with_other_contract(snippet);

        // Should be a warning for BOTH functions
        assert_eq!(result.diagnostics.len(), 2);
    }

    #[test]
    fn no_warn_on_public_calling_private_fn_with_side_effects() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var count uint u0)
            (define-public (increment2)
                (increment)
            )
            (define-private (increment)
                (ok (var-set count (+ (var-get count) u1)))
            )
        ")
        .to_string();

        let (_, result) = run_snippet_with_other_contract(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn warn_on_public_calling_private_fn_with_no_side_effects() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (increment2)
                (increment)
            )
            (define-private (increment)
                (ok true)
            )
        ")
        .to_string();

        let (output, result) = run_snippet_with_other_contract(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("could be declared as `define-read-only`"));
    }

    #[test]
    fn no_warn_on_public_contract_call_public_fn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (call-other)
                (contract-call? .other-contract do-something)
            )
        ")
        .to_string();

        let (_, result) = run_snippet_with_other_contract(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn warn_on_public_contract_call_read_only_fn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (call-other)
                (ok (contract-call? .other-contract get-value))
            )
        ")
        .to_string();

        let (output, result) = run_snippet_with_other_contract(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("could be declared as `define-read-only`"));
    }

    #[test]
    fn no_warn_on_dynamic_contract_call() {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnnecessaryPublic::get_name(), Level::Warning);

        let mut session = Session::new_without_boot_contracts(settings);

        #[rustfmt::skip]
        let trait_source = indoc!("
            (define-trait action-trait (
                (do-action () (response bool uint))
            ))
        ")
        .to_string();

        let trait_contract = ClarityContractBuilder::new()
            .name("action-trait")
            .code_source(trait_source)
            .build();

        session
            .interpreter
            .run(&trait_contract, None, false, None)
            .expect("Invalid trait contract");

        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait action-trait .action-trait.action-trait)

            (define-public (run-action (action <action-trait>))
                (contract-call? action do-action))
        ")
        .to_string();

        let (_, result) = session
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet");

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_read_only() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (get-value)
                u1
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn no_warn_on_private() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (helper)
                u1
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn warn_on_multiple_public_fns_without_side_effects() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (get-a)
                (ok u1)
            )
            (define-public (get-b)
                (ok u2)
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 2);
    }

    #[test]
    fn mixed_public_fns() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)
            (define-public (get-counter)
                (ok (var-get counter))
            )
            (define-public (increment)
                (begin
                    (var-set counter (+ (var-get counter) u1))
                    (ok true)
                )
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unnecessary_public)]
            (define-public (get-value)
                (ok u1)
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn warn_on_public_with_only_reads() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)
            (define-map balances principal uint)
            (define-public (get-info (who principal))
                (ok {
                    counter: (var-get counter),
                    balance: (default-to u0 (map-get? balances who))
                })
            )
        ")
        .to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("could be declared as `define-read-only`"));
    }

    #[test]
    fn no_warn_on_side_effect_in_branch() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var counter uint u0)
            (define-public (maybe-increment (do-it bool))
                (begin
                    (if do-it
                        (var-set counter (+ (var-get counter) u1))
                        false
                    )
                    (ok true)
                )
            )
        ")
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
