//! Lint to find unused private functions
//!
//! A private function is considered unused if it is never referenced
//!
//! **NOTE:** A private function is considered used if it is referenced by another unused private function.
//! In this case, repeated applications of the lint will find all unused code
//!
//! **NOTE:** It is common to intentionally have unused private functions for unit testing.
//! In this case, you should annotate the function with `;; #[allow(unused_private_fn)]`

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::functions::PrivateFnData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::is_explicitly_unused;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct UnusedPrivateFn<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    level: Level,
}

impl<'a, 'b> UnusedPrivateFn<'a, 'b> {
    fn new(analysis_cache: &'a mut AnalysisCache<'b>, level: Level) -> UnusedPrivateFn<'a, 'b> {
        Self {
            analysis_cache,
            level,
        }
    }

    fn run(&mut self) -> AnalysisResult {
        let diagnostics = self.generate_diagnostics();
        Ok(diagnostics)
    }

    fn allow(fn_data: &PrivateFnData, annotations: &[Annotation]) -> bool {
        fn_data
            .annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    /// Make diagnostic message and suggestion for unused private fn
    pub(crate) fn make_diagnostic_strings(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("private function `{name}` is never used"),
            Some(
                "Remove this expression or suffix function name with '_' if this is intentional"
                    .to_string(),
            ),
        )
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;
        let private_fns = self.analysis_cache.get_private_fns();

        for (name, fn_data) in private_fns {
            if fn_data.called || Self::allow(fn_data, annotations) || is_explicitly_unused(name) {
                continue;
            }
            let (message, suggestion) = Self::make_diagnostic_strings(name);
            diagnostics.push(Diagnostic {
                level: self.level.clone(),
                message,
                spans: vec![fn_data.expr.span.clone()],
                suggestion,
            });
        }

        // Functions are topologically sorted, not in source order
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for UnusedPrivateFn<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let mut lint = UnusedPrivateFn::new(analysis_cache, level);
        lint.run()
    }
}

impl Lint for UnusedPrivateFn<'_, '_> {
    fn get_name() -> LintName {
        LintName::UnusedPrivateFn
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::UnusedPrivateFn)
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

    use super::UnusedPrivateFn;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedPrivateFn::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings)
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

    /// A private fn that calls another private fn, but neither is called from public/read-only.
    /// The called fn is considered "used" (by the caller), but the caller is unused.
    #[test]
    fn unused_fn_calling_other_private_fn() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (square (x uint))
                (* x x))

            (define-private (square-plus-one (x uint))
                (+ (square x) u1))

            (define-read-only (identity (x uint))
                x)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let fn_name = "square-plus-one";
        let (expected_message, _) = UnusedPrivateFn::make_diagnostic_strings(&fn_name.into());

        // Only square-plus-one should warn; square is "used" by square-plus-one
        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
        assert!(output[0].contains(fn_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn multiple_unused() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (square (x uint))
                (* x x))

            (define-private (double (x uint))
                (* x u2))

            (define-read-only (identity (x uint))
                x)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 2);
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
        let (expected_message, _) = UnusedPrivateFn::make_diagnostic_strings(&fn_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning["));
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

    #[test]
    fn allow_with_naming_convention() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (square_ (x uint))
                (* x x))

            (define-read-only (cube (x uint))
                (* x (* x x)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_inside_as_contract_safe() {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedPrivateFn::get_name(), Level::Warning);

        let mut session = Session::new_without_boot_contracts(settings);
        session.update_epoch(clarity::types::StacksEpochId::Epoch33);

        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (helper (x uint))
                (* x x))

            (define-public (my-func (x uint))
                (as-contract? () (helper x)))
        ").to_string();

        let (_, result) = session
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet");

        assert_eq!(result.diagnostics.len(), 0);
    }
}
