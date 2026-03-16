//! Lint to find unused map (`declare-map`) variables
//!
//! A diagnostic is generated if:
//!  - The map is never referenced
//!  - The map is used, but in a way that cannot affect contract execution

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::SymbolicExpression;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::maps::MapData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::is_explicitly_unused;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

enum Usage {
    Unused,
    Unset,
    Unread,
    Used,
}

impl From<&MapData<'_>> for Usage {
    fn from(map_data: &MapData) -> Self {
        if map_data.is_used() {
            Self::Used
        } else if map_data.map_get && !map_data.map_set {
            Self::Unset
        } else if !map_data.map_get && map_data.map_set {
            Self::Unread
        } else if !map_data.map_get && !map_data.map_set {
            Self::Unused
        } else {
            unreachable!(
                "Unhandled usage pattern: insert={}, delete={}, set={}, get={}",
                map_data.map_insert, map_data.map_delete, map_data.map_set, map_data.map_get
            )
        }
    }
}

struct UnusedMapSettings {
    // TODO
}

impl UnusedMapSettings {
    fn new() -> Self {
        Self {}
    }
}

pub struct UnusedMap<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    _settings: UnusedMapSettings,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a, 'b> UnusedMap<'a, 'b> {
    fn new(
        analysis_cache: &'a mut AnalysisCache<'b>,
        level: Level,
        settings: UnusedMapSettings,
    ) -> UnusedMap<'a, 'b> {
        Self {
            analysis_cache,
            _settings: settings,
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

    /// Make diagnostic message and suggestion for variable which is never written to
    fn make_diagnostic_strings_unset(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("map `{name}` is read from but never written to"),
            Some("Remove map or write to it using `map-set`/`map-insert`".to_string()),
        )
    }

    /// Make diagnostic message and suggestion for variable which is never read from
    fn make_diagnostic_strings_unread(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("map `{name}` never read from"),
            Some("Remove map or read from it using `map-get?`".to_string()),
        )
    }

    /// Make diagnostic message and suggestion for variable which is never used
    fn make_diagnostic_strings_unused(name: &ClarityName) -> (String, Option<String>) {
        (
            format!("map `{name}` never used"),
            Some("Remove this expression".to_string()),
        )
    }

    fn make_diagnostic(
        level: Level,
        expr: &'a SymbolicExpression,
        message: String,
        suggestion: Option<String>,
    ) -> Diagnostic {
        Diagnostic {
            level,
            message,
            spans: vec![expr.span.clone()],
            suggestion,
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;
        let maps = self.analysis_cache.get_maps();

        for (name, data) in maps {
            if is_explicitly_unused(name) || Self::allow(data, annotations) {
                continue;
            }
            let (message, suggestion) = match Usage::from(data) {
                Usage::Used => continue,
                Usage::Unset => Self::make_diagnostic_strings_unset(name),
                Usage::Unread => Self::make_diagnostic_strings_unread(name),
                Usage::Unused => Self::make_diagnostic_strings_unused(name),
            };
            let diagnostic =
                Self::make_diagnostic(self.level.clone(), data.expr, message, suggestion);
            diagnostics.push(diagnostic);
        }

        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for UnusedMap<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedMapSettings::new();
        let mut lint = UnusedMap::new(analysis_cache, level, settings);
        lint.run()
    }
}

impl Lint for UnusedMap<'_, '_> {
    fn get_name() -> LintName {
        LintName::UnusedMap
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => warning_kinds.contains(&WarningKind::UnusedMap),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use clarity_types::diagnostic::Level;
    use indoc::indoc;

    use super::UnusedMap;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedMap::get_name(), Level::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn used_by_insert() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map consumed-messages (buff 32) bool)

            (define-public (consume-message (hash (buff 32)))
                (if (map-insert consumed-messages hash true)
                    (ok true)
                    (err u1)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_by_set_and_get() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map admins principal bool)

            (define-public (set-admin (p principal))
                (ok (map-set admins p true)))

            (define-read-only (is-admin (p principal))
                (default-to false (map-get? admins p)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_by_set_and_delete() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map sessions principal uint)

            (define-public (set-session (p principal) (id uint))
                (ok (map-set sessions p id)))

            (define-public (end-session (p principal))
                (ok (map-delete sessions p)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn delete_only() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map sessions principal uint)

            (define-public (end-session (p principal))
                (ok (map-delete sessions p)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "sessions";
        let (expected_message, _) = UnusedMap::make_diagnostic_strings_unused(&var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn not_set() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map admins principal bool)

            (define-read-only (is-admin (p principal))
                (default-to false (map-get? admins p)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "admins";
        let (expected_message, _) = UnusedMap::make_diagnostic_strings_unset(&var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn not_read() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map admins principal bool)

            (define-public (set-admin (p principal))
                (ok (map-set admins p true)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "admins";
        let (expected_message, _) = UnusedMap::make_diagnostic_strings_unread(&var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn not_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map admins principal bool)

            (define-read-only (is-admin (p principal))
                true)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "admins";
        let (expected_message, _) = UnusedMap::make_diagnostic_strings_unused(&var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_map)]
            (define-map admins principal bool)

            (define-read-only (is-admin (p principal))
                true)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_with_naming_convention() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-map admins_ principal bool)

            (define-read-only (is-admin (p principal))
                true)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
