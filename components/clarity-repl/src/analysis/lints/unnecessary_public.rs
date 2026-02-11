//! Lint to find public functions that could be declared read-only
//!
//! A public function can be read-only if it does not modify any blockchain state:
//! - No `var-set`
//! - No `map-set`, `map-insert`, or `map-delete`
//! - No `stx-transfer?`, `stx-burn?`
//! - No `ft-mint?`, `ft-burn?`, `ft-transfer?`
//! - No `nft-mint?`, `nft-burn?`, `nft-transfer?`
//! - No `contract-call?` (which could call state-modifying functions)

use std::collections::HashMap;

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

pub struct UnnecessaryPublic<'a> {
    clarity_version: ClarityVersion,
    diagnostics: Vec<Diagnostic>,
    annotations: &'a Vec<Annotation>,
    level: Level,
    active_annotation: Option<usize>,
    /// Set to `true` when traversing a public function body that contains a state-modifying op
    found_side_effect: bool,
}

impl<'a> UnnecessaryPublic<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
    ) -> UnnecessaryPublic<'a> {
        Self {
            clarity_version,
            level,
            diagnostics: Vec::new(),
            annotations,
            active_annotation: None,
            found_side_effect: false,
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        traverse(&mut self, &contract_analysis.expressions);

        // TODO: Do we need to sort here? The functions should always be traversed in the order they are declared, right?
        //       And errors should be generated in the same order, right?
        self.diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
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

impl<'a> ASTVisitor<'a> for UnnecessaryPublic<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    // Skip private function bodies, we only care about public functions
    fn traverse_define_private(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
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

    // TODO: We skip private and read-only functions, but there are more top-level declarations that we can skip

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

        if !self.found_side_effect {
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

    fn visit_static_contract_call(
        &mut self,
        _expr: &'a SymbolicExpression,
        _contract_identifier: &'a QualifiedContractIdentifier,
        _function_name: &'a ClarityName,
        _args: &'a [SymbolicExpression],
    ) -> bool {
        // TODO: Currently any `contract-call?` is considered to have side effects,
        //       but in theory we could check a static call and see if the called function is read-only
        self.found_side_effect = true;
        false
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

impl AnalysisPass for UnnecessaryPublic<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let checker = UnnecessaryPublic::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
        );
        checker.run(analysis_cache.contract_analysis)
    }
}

impl Lint for UnnecessaryPublic<'_> {
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
    use indoc::indoc;

    use super::UnnecessaryPublic;
    use crate::analysis::linter::{Lint, LintLevel};
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .set_lint_level(UnnecessaryPublic::get_name(), LintLevel::Warning);

        Session::new_without_boot_contracts(settings)
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

    // Note: `contract-call?` cannot be tested with a single snippet because the
    // target contract must exist in the session. The `visit_static_contract_call`
    // and `visit_dynamic_contract_call` handlers follow the same trivial pattern
    // as the other state-modifying operations tested here.

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

    // TODO: Add test for `contract-call?`
    //       See `unused_trait.rs` for example of how to deploy multiple contracts in a unit tests
}
