//! Lint to check case of trait names (declared with `define-trait` or imported with `use-trait`)
//!
//! By default, this enforces kebab-case

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::SymbolicExpression;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::{match_kebab_case, strip_unused_suffix, CaseError};
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct CaseTrait<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    level: Level,
}

impl<'a, 'b> CaseTrait<'a, 'b> {
    fn new(analysis_cache: &'a mut AnalysisCache<'b>, level: Level) -> CaseTrait<'a, 'b> {
        Self {
            analysis_cache,
            level,
        }
    }

    fn run(&mut self) -> AnalysisResult {
        let diagnostics = self.generate_diagnostics();
        Ok(diagnostics)
    }

    fn allow(annotation: Option<usize>, annotations: &[Annotation]) -> bool {
        annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    fn make_diagnostic_message(name: &ClarityName, error: &CaseError) -> String {
        format!("trait `{name}` is not kebab-case: {error}")
    }

    fn check_name(
        level: &Level,
        name: &ClarityName,
        expr: &SymbolicExpression,
        annotation: Option<usize>,
        annotations: &[Annotation],
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        if Self::allow(annotation, annotations) {
            return;
        }
        let Err(error) = match_kebab_case(strip_unused_suffix(name.as_str())) else {
            return;
        };
        let message = Self::make_diagnostic_message(name, &error);
        diagnostics.push(Diagnostic {
            level: level.clone(),
            message,
            spans: vec![expr.span.clone()],
            suggestion: Some(error.suggestion()),
        });
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        let annotations = self.analysis_cache.annotations;

        let declared = self.analysis_cache.get_declared_traits();
        for (name, data) in declared {
            Self::check_name(
                &self.level,
                name,
                data.expr,
                data.annotation,
                annotations,
                &mut diagnostics,
            );
        }

        let imported = self.analysis_cache.get_imported_traits();
        for (name, data) in imported {
            Self::check_name(
                &self.level,
                name,
                data.expr,
                data.annotation,
                annotations,
                &mut diagnostics,
            );
        }

        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for CaseTrait<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let mut lint = CaseTrait::new(analysis_cache, level);
        lint.run()
    }
}

impl Lint for CaseTrait<'_, '_> {
    fn get_name() -> LintName {
        LintName::CaseTrait
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => warning_kinds.contains(&WarningKind::CaseTrait),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::types::StacksEpochId;
    use clarity_types::diagnostic::{Diagnostic, Level};
    use indoc::indoc;

    use super::CaseTrait;
    use crate::analysis::linter::Lint;
    use crate::analysis::util::CaseError;
    use crate::repl::session::{AnnotatedExecutionResult, Session};
    use crate::repl::SessionSettings;
    use crate::test_fixtures::clarity_contract::ClarityContractBuilder;

    fn run_snippet_no_panic(
        snippet: String,
    ) -> Result<(Vec<String>, AnnotatedExecutionResult), (Vec<String>, Vec<Diagnostic>)> {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(CaseTrait::get_name(), Level::Warning);

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

        session.formatted_interpretation(snippet, Some("unit-test".to_owned()), false, None)
    }

    fn run_snippet(snippet: String) -> (Vec<String>, AnnotatedExecutionResult) {
        run_snippet_no_panic(snippet).expect("Invalid code snippet")
    }

    #[test]
    fn valid_names() {
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
    fn fail_on_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token_trait .token-trait.token-trait)

            (define-public (get-token-balance (token <token_trait>) (p principal))
                (contract-call? token get-balance p))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let trait_name = "token_trait";
        let expected_message = CaseTrait::make_diagnostic_message(
            &trait_name.into(),
            &CaseError::IllegalCharacter(b'_'),
        );

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(trait_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_on_upper_case() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait TokenTrait .token-trait.token-trait)

            (define-public (get-token-balance (token <TokenTrait>) (p principal))
                (contract-call? token get-balance p))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let trait_name = "TokenTrait";
        let expected_message = CaseTrait::make_diagnostic_message(
            &trait_name.into(),
            &CaseError::IllegalCharacter(b'T'),
        );

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(trait_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(case_trait)]
            (use-trait token_trait .token-trait.token-trait)

            (define-public (get-token-balance (token <token_trait>) (p principal))
                (contract-call? token get-balance p))
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn fail_define_trait_on_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-trait my_trait (
                (get-amount () (response uint uint))
            ))
        ").to_string();

        let (output, result) = run_snippet_no_panic(snippet).expect("Invalid code snippet");

        let trait_name = "my_trait";
        let expected_message = CaseTrait::make_diagnostic_message(
            &trait_name.into(),
            &CaseError::IllegalCharacter(b'_'),
        );

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(trait_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_trailing_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (use-trait token-trait_ .token-trait.token-trait)

            (define-public (get-token-balance (token <token-trait_>) (p principal))
                (contract-call? token get-balance p))
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.lint_diagnostics.len(), 0);
    }
}
