use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

enum Usage {
    Unused,
    Unset,
    Unread,
    Used,
}

struct UnusedMapSettings {
    // TODO
}

impl UnusedMapSettings {
    fn new() -> Self {
        Self {}
    }
}

/// Data associated with a `define-map` variable
#[derive(Debug)]
struct MapData<'a> {
    expr: &'a SymbolicExpression,
    /// Has `map-insert` been called on map?
    pub map_insert: bool,
    /// Has `map-delete` been called on map?
    pub map_delete: bool,
    /// Has `map-set` been called on map?
    pub map_set: bool,
    /// Has `map-get?` been called on map?
    pub map_get: bool,
}

impl<'a> MapData<'a> {
    fn new(expr: &'a SymbolicExpression) -> Self {
        Self {
            expr,
            map_insert: false,
            map_delete: false,
            map_set: false,
            map_get: false,
        }
    }
}

pub struct UnusedMap<'a> {
    clarity_version: ClarityVersion,
    _settings: UnusedMapSettings,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
    level: Level,
    /// Clarity maps declared with `define-map`
    maps: HashMap<&'a ClarityName, MapData<'a>>,
}

impl<'a> UnusedMap<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
        settings: UnusedMapSettings,
    ) -> UnusedMap<'a> {
        Self {
            clarity_version,
            _settings: settings,
            level,
            annotations,
            active_annotation: None,
            maps: HashMap::new(),
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        // Traverse the entire AST
        traverse(&mut self, &contract_analysis.expressions);

        // Process hashmap of unused constants and generate diagnostics
        let diagnostics = self.generate_diagnostics();

        Ok(diagnostics)
    }

    // Check for annotations that should be attached to the given span
    fn set_active_annotation(&mut self, span: &Span) {
        self.active_annotation = get_index_of_span(self.annotations, span);
    }

    // Check if the expression is annotated with `allow(<lint_name>)`
    fn allow(&self) -> bool {
        self.active_annotation
            .map(|idx| Self::match_allow_annotation(&self.annotations[idx]))
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
        &self,
        expr: &'a SymbolicExpression,
        message: String,
        suggestion: Option<String>,
    ) -> Diagnostic {
        Diagnostic {
            level: self.level.clone(),
            message,
            spans: vec![expr.span.clone()],
            suggestion,
        }
    }

    /// Decide if a map is "unused"
    /// This means whether or not it can affect contract execution, which is still possible even if `map-get?` is never called.
    /// For example, it might track unique hashes using `map-insert` and fail if the same hash is used twice
    /// So the conditions we will use to determine if a map is used:
    ///  - `map-insert` is called
    ///  - `map-set` and either `map-delete` or `map-get?` are called
    ///
    /// TODO: If the `bool` return value from `map-insert` or `map-delete` is not bound to a variable or returned from a function,
    ///       the map may still be unused
    fn is_map_used(map_data: &MapData) -> bool {
        map_data.map_insert || (map_data.map_set && (map_data.map_get || map_data.map_delete))
    }

    fn compute_map_usage(map_data: &MapData) -> Usage {
        if Self::is_map_used(map_data) {
            Usage::Used
        } else if map_data.map_get && !map_data.map_set {
            Usage::Unset
        } else if !map_data.map_get && map_data.map_set {
            Usage::Unread
        } else if !map_data.map_get && !map_data.map_set {
            Usage::Unused
        } else {
            unreachable!("Unhandled usage pattern: {:?}", map_data)
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        for (name, data) in &self.maps {
            let (message, suggestion) = match Self::compute_map_usage(data) {
                Usage::Used => continue,
                Usage::Unset => Self::make_diagnostic_strings_unset(name),
                Usage::Unread => Self::make_diagnostic_strings_unread(name),
                Usage::Unused => Self::make_diagnostic_strings_unused(name),
            };
            let diagnostic = self.make_diagnostic(data.expr, message, suggestion);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a> ASTVisitor<'a> for UnusedMap<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_map(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key_type: &'a SymbolicExpression,
        _value_type: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);

        if !self.allow() {
            self.maps.insert(name, MapData::new(expr));
        }

        true
    }

    fn visit_map_insert(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        _value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        _ = self.maps.get_mut(name).map(|data| data.map_insert = true);
        true
    }

    fn visit_map_delete(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        _ = self.maps.get_mut(name).map(|data| data.map_delete = true);
        true
    }

    fn visit_map_set(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        _value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        _ = self.maps.get_mut(name).map(|data| data.map_set = true);
        true
    }

    fn visit_map_get(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        _ = self.maps.get_mut(name).map(|data| data.map_get = true);
        true
    }
}

impl AnalysisPass for UnusedMap<'_> {
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        _analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedMapSettings::new();
        let lint = UnusedMap::new(
            contract_analysis.clarity_version,
            annotations,
            level,
            settings,
        );
        lint.run(contract_analysis)
    }
}

impl Lint for UnusedMap<'_> {
    fn get_name() -> LintName {
        LintName::UnusedMap
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        matches!(
            annotation.kind,
            AnnotationKind::Allow(WarningKind::UnusedMap)
        )
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::UnusedMap;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedMap::get_name(), Level::Warning);

        Session::new(settings)
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
}
