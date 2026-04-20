//! Lint to find unused FTs and NFTs
//!
//! Tokens are considered unused if declared but never minted

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::tokens::TokenData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::is_explicitly_unused;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct UnusedToken<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    level: Level,
}

impl<'a, 'b> UnusedToken<'a, 'b> {
    fn new(analysis_cache: &'a mut AnalysisCache<'b>, level: Level) -> UnusedToken<'a, 'b> {
        Self {
            analysis_cache,
            level,
        }
    }

    fn run(&mut self) -> AnalysisResult {
        let diagnostics = self.generate_diagnostics();
        Ok(diagnostics)
    }

    fn allow(token_data: &TokenData, annotations: &[Annotation]) -> bool {
        token_data
            .annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    pub(crate) fn make_diagnostic_message_ft(name: &ClarityName) -> String {
        format!("fungible token `{name}` never used")
    }

    pub(crate) fn make_diagnostic_message_nft(name: &ClarityName) -> String {
        format!("non-fungible token `{name}` never used")
    }

    fn make_diagnostic(level: &Level, token_data: &TokenData, message: String) -> Diagnostic {
        Diagnostic {
            level: level.clone(),
            message,
            spans: vec![token_data.expr.span.clone()],
            suggestion: Some("Remove this expression".to_string()),
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;

        let fts = self.analysis_cache.get_fts();
        for (name, token_data) in fts {
            if token_data.minted
                || Self::allow(token_data, annotations)
                || is_explicitly_unused(name)
            {
                continue;
            }
            let message = Self::make_diagnostic_message_ft(name);
            diagnostics.push(Self::make_diagnostic(&self.level, token_data, message));
        }

        let nfts = self.analysis_cache.get_nfts();
        for (name, token_data) in nfts {
            if token_data.minted
                || Self::allow(token_data, annotations)
                || is_explicitly_unused(name)
            {
                continue;
            }
            let message = Self::make_diagnostic_message_nft(name);
            diagnostics.push(Self::make_diagnostic(&self.level, token_data, message));
        }

        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for UnusedToken<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let mut lint = UnusedToken::new(analysis_cache, level);
        lint.run()
    }
}

impl Lint for UnusedToken<'_, '_> {
    fn get_name() -> LintName {
        LintName::UnusedToken
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::UnusedToken)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use indoc::{formatdoc, indoc};

    use super::UnusedToken;
    use crate::analysis::linter::Lint;
    use crate::repl::session::{AnnotatedExecutionResult, Session};
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, AnnotatedExecutionResult) {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedToken::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn ft_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-fungible-token sbtc)

            (define-public (mint (amount uint) (recipient principal))
                (ft-mint? sbtc amount recipient))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn ft_not_used() {
        let ft_name = "sbtc";

        #[rustfmt::skip]
        let snippet = formatdoc!("
            (define-fungible-token {ft_name})

            (define-public (mint (amount uint) (recipient principal))
                (err u1))
        ");

        let (output, result) = run_snippet(snippet);

        let expected_message = UnusedToken::make_diagnostic_message_ft(&ft_name.into());

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(ft_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_ft_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_token)]
            (define-fungible-token sbtc)

            (define-public (mint (amount uint) (recipient principal))
                (err u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn allow_ft_with_naming_convention() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-fungible-token sbtc_)

            (define-public (mint (amount uint) (recipient principal))
                (err u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn both_ft_and_nft_unused() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-fungible-token my-ft)
            (define-non-fungible-token my-nft uint)

            (define-public (noop)
                (ok true))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 2);
    }

    #[test]
    fn nft_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-non-fungible-token hashes (buff 32))

            (define-public (mint (hash (buff 32)) (recipient principal))
                (nft-mint? hashes hash recipient))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn nft_not_used() {
        let nft_name = "hashes";

        #[rustfmt::skip]
        let snippet = formatdoc!("
            (define-non-fungible-token {nft_name} (buff 32))

            (define-public (mint (hash (buff 32)) (recipient principal))
                (err u1))
        ");

        let (output, result) = run_snippet(snippet);

        let expected_message = UnusedToken::make_diagnostic_message_nft(&nft_name.into());

        assert_eq!(result.lint_diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(nft_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_nft_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_token)]
            (define-non-fungible-token hashes (buff 32))

            (define-public (mint (hash (buff 32)) (recipient principal))
                (err u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 0);
    }

    #[test]
    fn allow_nft_with_naming_convention() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-non-fungible-token hashes_ (buff 32))

            (define-public (mint (hash (buff 32)) (recipient principal))
                (err u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.lint_diagnostics.len(), 0);
    }
}
