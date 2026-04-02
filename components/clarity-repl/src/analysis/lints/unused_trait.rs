//! Find unused traits imported with `use-trait`
//!
//! A trait is considered unused if there is no public or read-only function parameter with the trait type

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::traits::ImportedTraitData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::is_explicitly_unused;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct UnusedTrait<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    level: Level,
}

impl<'a, 'b> UnusedTrait<'a, 'b> {
    fn new(analysis_cache: &'a mut AnalysisCache<'b>, level: Level) -> UnusedTrait<'a, 'b> {
        Self {
            analysis_cache,
            level,
        }
    }

    fn run(&mut self) -> AnalysisResult {
        let diagnostics = self.generate_diagnostics();
        Ok(diagnostics)
    }

    fn allow(trait_data: &ImportedTraitData, annotations: &[Annotation]) -> bool {
        trait_data
            .annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    /// Make diagnostic message and suggestion for unused trait
    pub(crate) fn make_diagnostic_strings_unused(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("imported trait `{name}` is never used"),
            Some("Remove this expression".to_string()),
        )
    }

    /// Make diagnostic message and suggestion if trait is only used in private function
    pub(crate) fn make_diagnostic_strings_private_fn_only(
        name: &ClarityName,
    ) -> (String, Option<String>) {
        (
            format!("imported trait `{name}` only appears in private functions, so cannot be used"),
            Some("Remove this expression".to_string()),
        )
    }

    fn make_diagnostic(
        level: &Level,
        trait_data: &ImportedTraitData,
        message: String,
        suggestion: Option<String>,
    ) -> Diagnostic {
        Diagnostic {
            level: level.clone(),
            message,
            spans: vec![trait_data.expr.span.clone()],
            suggestion,
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;
        // We only check imported traits here, declared traits are never used in the same file
        let traits = self.analysis_cache.get_imported_traits();

        for (name, trait_data) in traits {
            if Self::allow(trait_data, annotations) || is_explicitly_unused(name) {
                continue;
            }

            let (message, suggestion) = if trait_data.is_used() {
                continue;
            } else if trait_data.is_private_fn_only() {
                Self::make_diagnostic_strings_private_fn_only(name)
            } else {
                Self::make_diagnostic_strings_unused(name)
            };

            diagnostics.push(Self::make_diagnostic(
                &self.level,
                trait_data,
                message,
                suggestion,
            ));
        }

        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for UnusedTrait<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let mut lint = UnusedTrait::new(analysis_cache, level);
        lint.run()
    }
}

impl Lint for UnusedTrait<'_, '_> {
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
    use clarity::vm::diagnostic::Level;
    use indoc::indoc;

    use super::UnusedTrait;
    use crate::analysis::linter::Lint;
    use crate::repl::session::{AnnotatedExecutionResult, Session};
    use crate::repl::SessionSettings;
    use crate::test_fixtures::clarity_contract::ClarityContractBuilder;

    fn run_snippet(snippet: String) -> (Vec<String>, AnnotatedExecutionResult) {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedTrait::get_name(), Level::Warning);

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

        assert_eq!(result.lint_diagnostics.len(), 0);
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

        assert_eq!(result.lint_diagnostics.len(), 0);
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

        assert_eq!(result.lint_diagnostics.len(), 0);
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

        assert_eq!(result.lint_diagnostics.len(), 0);
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

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
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

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
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

        assert_eq!(result.lint_diagnostics.len(), 0);
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

        assert_eq!(result.lint_diagnostics.len(), 0);
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

        assert_eq!(result.lint_diagnostics.len(), 0);
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

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
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

        assert_eq!(result.lint_diagnostics.len(), 0);
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

        assert_eq!(result.lint_diagnostics.len(), 0);
    }
}
