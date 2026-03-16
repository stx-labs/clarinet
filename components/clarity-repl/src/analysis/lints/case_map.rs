//! Lint to check case of map names (declared using `define-map`)
//!
//! By default, this enforces kebab-case

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::SymbolicExpression;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::maps::MapData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::{match_kebab_case, CaseError};
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct CaseMap<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a, 'b> CaseMap<'a, 'b> {
    fn new(analysis_cache: &'a mut AnalysisCache<'b>, level: Level) -> CaseMap<'a, 'b> {
        Self {
            analysis_cache,
            level,
        }
    }

    fn run(&mut self) -> AnalysisResult {
        let diagnostics = self.generate_diagnostics();
        Ok(diagnostics)
    }

    // Check if the expression is annotated with `allow(<lint_name>)`
    fn allow(map_data: &MapData, annotations: &[Annotation]) -> bool {
        map_data
            .annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    fn make_diagnostic_message(name: &ClarityName, error: &CaseError) -> String {
        format!("map `{name}` is not kebab-case: {error}")
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
        let maps = self.analysis_cache.get_maps();

        for (name, map_data) in maps {
            if Self::allow(map_data, annotations) {
                continue;
            }
            let Err(error) = match_kebab_case(name.as_str()) else {
                continue;
            };
            let message = Self::make_diagnostic_message(name, &error);
            let diagnostic =
                Self::make_diagnostic(self.level.clone(), map_data.expr, message, &error);
            diagnostics.push(diagnostic);
        }

        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for CaseMap<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let mut lint = CaseMap::new(analysis_cache, level);
        lint.run()
    }
}

impl Lint for CaseMap<'_, '_> {
    fn get_name() -> LintName {
        LintName::CaseMap
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => warning_kinds.contains(&WarningKind::CaseMap),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use clarity_types::diagnostic::{Diagnostic, Level};
    use indoc::indoc;

    use super::CaseMap;
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
            .enable_lint(CaseMap::get_name(), Level::Warning);

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
    fn valid_names() {
        let snippet = indoc!(
            "
            (define-map balances principal uint)
            (define-map user-roles principal (string-ascii 20))
            (define-map consumed-messages (buff 32) bool)
        "
        )
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn fail_on_underscore() {
        let snippet = indoc!(
            "
            (define-map user_roles principal (string-ascii 20))
        "
        )
        .to_string();

        let (output, result) = run_snippet(snippet);

        let map_name = "user_roles";
        let expected_message =
            CaseMap::make_diagnostic_message(&map_name.into(), &CaseError::IllegalCharacter(b'_'));

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(map_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_on_upper_case() {
        let snippet = indoc!(
            "
            (define-map Balances principal uint)
        "
        )
        .to_string();

        let (output, result) = run_snippet(snippet);

        let map_name = "Balances";
        let expected_message =
            CaseMap::make_diagnostic_message(&map_name.into(), &CaseError::IllegalCharacter(b'B'));

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(map_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_on_screaming_snake_case() {
        let snippet = indoc!(
            "
            (define-map USER_ROLES principal (string-ascii 20))
        "
        )
        .to_string();

        let (output, result) = run_snippet(snippet);

        let map_name = "USER_ROLES";
        let expected_message =
            CaseMap::make_diagnostic_message(&map_name.into(), &CaseError::IllegalCharacter(b'U'));

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(map_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn fail_on_consecutive_hyphens() {
        let snippet = indoc!(
            "
            (define-map user--roles principal (string-ascii 20))
        "
        )
        .to_string();

        let res = run_snippet_no_panic(snippet);

        match res {
            Ok((output, result)) => {
                let map_name = "user--roles";
                let expected_message = CaseMap::make_diagnostic_message(
                    &map_name.into(),
                    &CaseError::ConsecutiveHyphens,
                );

                assert_eq!(result.diagnostics.len(), 1);
                assert!(output[0].contains("warning:"));
                assert!(output[0].contains(map_name));
                assert!(output[0].contains(&expected_message));
            }
            Err(..) => {
                // Map name may be illegal in Clarity, so allow interpretation to fail
            }
        }
    }

    #[test]
    fn allow_with_annotation() {
        let snippet = indoc!(
            "
            ;; #[allow(case_map)]
            (define-map user_roles principal (string-ascii 20))
        "
        )
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_single_trailing_hyphen() {
        let snippet = indoc!(
            "
            (define-map balances- principal uint)
        "
        )
        .to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    // We plan to allow leading hyphens in Clarity some day
    // This test is written so that it does not need to be modified when that occurs
    #[test]
    fn allow_single_leading_hyphen() {
        let snippet = indoc!(
            "
            (define-map -balances principal uint)
        "
        )
        .to_string();

        let res = run_snippet_no_panic(snippet);

        match res {
            Ok((_, result)) => {
                assert_eq!(result.diagnostics.len(), 0);
            }
            Err(..) => {
                // Map name may be illegal in Clarity, so allow interpretation to fail
            }
        }
    }
}
