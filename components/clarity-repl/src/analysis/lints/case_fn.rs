//! Lint to check case of function names
//!
//! By default, this enforces kebab-case on public, private, and read-only functions

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::SymbolicExpression;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::{match_kebab_case, strip_unused_suffix, CaseError};
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct CaseFn<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    level: Level,
}

impl<'a, 'b> CaseFn<'a, 'b> {
    fn new(analysis_cache: &'a mut AnalysisCache<'b>, level: Level) -> CaseFn<'a, 'b> {
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
        format!("function `{name}` is not kebab-case: {error}")
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

        let public_fns = self.analysis_cache.get_public_fns();
        for (name, data) in public_fns {
            Self::check_name(
                &self.level,
                name,
                data.expr,
                data.annotation,
                annotations,
                &mut diagnostics,
            );
        }

        let read_only_fns = self.analysis_cache.get_read_only_fns();
        for (name, data) in read_only_fns {
            Self::check_name(
                &self.level,
                name,
                data.expr,
                data.annotation,
                annotations,
                &mut diagnostics,
            );
        }

        let private_fns = self.analysis_cache.get_private_fns();
        for (name, data) in private_fns {
            Self::check_name(
                &self.level,
                name,
                data.expr,
                data.annotation,
                annotations,
                &mut diagnostics,
            );
        }

        // Functions are topologically sorted, not in source order
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for CaseFn<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let mut lint = CaseFn::new(analysis_cache, level);
        lint.run()
    }
}

impl Lint for CaseFn<'_, '_> {
    fn get_name() -> LintName {
        LintName::CaseFn
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => warning_kinds.contains(&WarningKind::CaseFn),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use clarity_types::diagnostic::{Diagnostic, Level};
    use indoc::indoc;

    use super::CaseFn;
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
            .enable_lint(CaseFn::get_name(), Level::Warning);

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
    fn valid_public_fn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (transfer-tokens (amount uint))
                (ok amount))
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn valid_read_only_fn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (get-balance (who principal))
                u0)
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn valid_private_fn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (do-transfer (amount uint))
                (ok amount))
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn fail_public_fn_on_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (transfer_tokens (amount uint))
                (ok amount))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let fn_name = "transfer_tokens";
        let expected_message =
            CaseFn::make_diagnostic_message(&fn_name.into(), &CaseError::IllegalCharacter(b'_'));

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(fn_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_read_only_fn_on_upper_case() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (getBalance (who principal))
                u0)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let fn_name = "getBalance";
        let expected_message =
            CaseFn::make_diagnostic_message(&fn_name.into(), &CaseError::IllegalCharacter(b'B'));

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(fn_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_private_fn_on_screaming_snake_case() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (DO_TRANSFER (amount uint))
                (ok amount))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let fn_name = "DO_TRANSFER";
        let expected_message =
            CaseFn::make_diagnostic_message(&fn_name.into(), &CaseError::IllegalCharacter(b'D'));

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(fn_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn multiple_bad_functions() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (transfer_tokens (amount uint))
                (ok amount))
            (define-read-only (getBalance (who principal))
                u0)
            (define-private (do_transfer (amount uint))
                (ok amount))
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.diagnostics.len(), 3);
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(case_fn)]
            (define-public (transfer_tokens (amount uint))
                (ok amount))
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_trailing_underscore() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (transfer-tokens_ (amount uint))
                (ok amount))
        ").to_string();

        let (_, result) = run_snippet(snippet);
        assert_eq!(result.diagnostics.len(), 0);
    }
}
