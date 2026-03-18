//! Lint to check case of FT and NFT names
//!
//! By default, this enforces kebab-case

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::SymbolicExpression;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::tokens::TokenData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::{match_kebab_case, strip_unused_suffix, CaseError};
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct CaseToken<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    level: Level,
}

impl<'a, 'b> CaseToken<'a, 'b> {
    fn new(analysis_cache: &'a mut AnalysisCache<'b>, level: Level) -> CaseToken<'a, 'b> {
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

    fn make_diagnostic_message(name: &ClarityName, error: &CaseError) -> String {
        format!("token `{name}` is not kebab-case: {error}")
    }

    fn make_diagnostic(
        level: Level,
        expr: &'a SymbolicExpression,
        message: String,
        error: &CaseError,
    ) -> Diagnostic {
        Diagnostic {
            level,
            message,
            spans: vec![expr.span.clone()],
            suggestion: Some(error.suggestion()),
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;

        let fts = self.analysis_cache.get_fts();
        for (name, token_data) in fts {
            if Self::allow(token_data, annotations) {
                continue;
            }
            let Err(error) = match_kebab_case(strip_unused_suffix(name)) else {
                continue;
            };
            let message = Self::make_diagnostic_message(name, &error);
            let diagnostic =
                Self::make_diagnostic(self.level.clone(), token_data.expr, message, &error);
            diagnostics.push(diagnostic);
        }

        let nfts = self.analysis_cache.get_nfts();
        for (name, token_data) in nfts {
            if Self::allow(token_data, annotations) {
                continue;
            }
            let Err(error) = match_kebab_case(strip_unused_suffix(name)) else {
                continue;
            };
            let message = Self::make_diagnostic_message(name, &error);
            let diagnostic =
                Self::make_diagnostic(self.level.clone(), token_data.expr, message, &error);
            diagnostics.push(diagnostic);
        }

        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for CaseToken<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let mut lint = CaseToken::new(analysis_cache, level);
        lint.run()
    }
}

impl Lint for CaseToken<'_, '_> {
    fn get_name() -> LintName {
        LintName::CaseToken
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => warning_kinds.contains(&WarningKind::CaseToken),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use clarity_types::diagnostic::{Diagnostic, Level};
    use indoc::indoc;

    use super::CaseToken;
    use crate::analysis::linter::Lint;
    use crate::analysis::util::CaseError;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet_no_panic(
        snippet: String,
    ) -> Result<(Vec<String>, ExecutionResult), (Vec<String>, Vec<Diagnostic>)> {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(CaseToken::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings).formatted_interpretation(
            snippet,
            Some("checker".to_string()),
            false,
            None,
        )
    }

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        run_snippet_no_panic(snippet).expect("Invalid code snippet")
    }

    #[test]
    fn valid_ft_names() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-fungible-token sbtc)
            (define-fungible-token my-token)
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn valid_nft_names() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-non-fungible-token hashes (buff 32))
            (define-non-fungible-token my-nft uint)
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn fail_ft_on_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-fungible-token my_token)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let token_name = "my_token";
        let expected_message = CaseToken::make_diagnostic_message(
            &token_name.into(),
            &CaseError::IllegalCharacter(b'_'),
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(token_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_nft_on_upper_case() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-non-fungible-token MyNFT uint)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let token_name = "MyNFT";
        let expected_message = CaseToken::make_diagnostic_message(
            &token_name.into(),
            &CaseError::IllegalCharacter(b'M'),
        );

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(token_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(case_token)]
            (define-fungible-token my_token)
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_trailing_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-fungible-token my-token_)
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.diagnostics.len(), 0);
    }
}
