//! Lint to check error constants
//!
//! Checks two things:
//!   1. All constants prefixed with `ERR_` should have an error type value, `(err ...)`
//!   2. No two `ERR_` constants should have the same error value

use std::collections::HashMap;

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::functions::NativeFunctions;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::constants::ConstantData;
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct ErrorConstSettings {
    // TODO
}

impl ErrorConstSettings {
    fn new() -> Self {
        Self {}
    }
}

pub struct ErrorConst<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    _settings: ErrorConstSettings,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a, 'b> ErrorConst<'a, 'b> {
    fn new(
        analysis_cache: &'a mut AnalysisCache<'b>,
        level: Level,
        settings: ErrorConstSettings,
    ) -> ErrorConst<'a, 'b> {
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

    fn allow(const_data: &ConstantData, annotations: &[Annotation]) -> bool {
        const_data
            .annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    /// Extract the value expression from a define-constant expression.
    /// For `(define-constant NAME VALUE)`, returns `VALUE`.
    fn get_value_expr(const_data: &ConstantData<'a>) -> Option<&'a SymbolicExpression> {
        const_data.expr.match_list().and_then(|list| list.get(2))
    }

    /// Check if a value expression is `(err ...)`
    fn is_err_value(value: &SymbolicExpression, clarity_version: &ClarityVersion) -> bool {
        value
            .match_list()
            .and_then(|list| list.first())
            .and_then(|first| first.match_atom())
            .and_then(|atom| NativeFunctions::lookup_by_name_at_version(atom, clarity_version))
            .is_some_and(|func| func == NativeFunctions::ConsError)
    }

    /// Get the inner value of an `(err X)` expression (returns `X`)
    fn get_err_inner_value(value: &SymbolicExpression) -> Option<&SymbolicExpression> {
        value.match_list().and_then(|list| list.get(1))
    }

    fn make_not_err_message(name: &ClarityName) -> String {
        format!("constant `{name}` has the `ERR_` prefix but its value is not an error type")
    }

    fn make_duplicate_message(name: &ClarityName, other_name: &ClarityName) -> String {
        format!("constant `{name}` has the same error value as `{other_name}`")
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;
        let clarity_version = self.analysis_cache.contract_analysis.clarity_version;
        let constants = self.analysis_cache.get_constants();

        // Collect ERR_ constants sorted by span position for deterministic duplicate detection
        let mut err_constants: Vec<(&ClarityName, &ConstantData)> = constants
            .iter()
            // TODO: Can we declare the prefix as a constant at the top of the file, rather than hardcoding it here (and elsewhere in the file)?
            .filter(|(name, _)| name.as_str().starts_with("ERR_"))
            // TODO: Also ignore if the constant name begins or ends with `_` (use `util::is_explicitly_unused()`)
            .map(|(name, data)| (*name, data))
            .collect();
        // TODO: How about using an `IndexMap` in `AnalysisCache` instead, so we know the map order is the key order is the same as the declaration order
        //       Then we can just use an iterator here rather than `collect()` and sort
        err_constants.sort_by(|a, b| a.1.expr.span.cmp(&b.1.expr.span));

        // Track error values for duplicate detection: value_string -> name
        let mut seen_values: HashMap<String, &ClarityName> = HashMap::new();

        for (name, const_data) in err_constants {
            if Self::allow(const_data, annotations) {
                continue;
            }

            let Some(value) = Self::get_value_expr(const_data) else {
                continue;
            };

            // Check 1: ERR_ constant must have (err ...) value
            if !Self::is_err_value(value, &clarity_version) {
                diagnostics.push(Diagnostic {
                    level: self.level.clone(),
                    message: Self::make_not_err_message(name),
                    spans: vec![const_data.expr.span.clone()],
                    suggestion: Some(
                        "Use `(err ...)` as the value or remove the `ERR_` prefix".to_string(),
                    ),
                });
                continue;
            }

            // Check 2: No duplicate error values
            //
            // Stringifying values here seems to be the only practical way to compare them
            // There is no risk of collision because:
            //   - UInt(100) → u100
            //   - Int(100) → 100
            //   - ASCII string "100" → "100" (with literal quote characters)
            //   - UTF8 string u"100" → u"100" (with prefix and quotes)
            // And using a map key that implements `Hash` or `PartialEq` is not practical because:
            //   - `Hash`: `SymbolicExpression` is defined in stacks-core (a git dependency), so we can't add derives. And even `Value` doesn't implement `Hash`
            //   - `PartialEq` with `BTreeMap`: `SymbolicExpression` does implement `Eq`/`PartialEq`, but the derived `PartialEq` compares all fields including `id` (a unique per-expression counter). Also, `Ord` isn't implemented, so `BTreeMap` won't work
            if let Some(inner_value) = Self::get_err_inner_value(value) {
                let value_key = format!("{inner_value}");
                // TODO: Can we use `Entry` instead of separate `get()` and `insert()`?
                if let Some(other_name) = seen_values.get(&value_key) {
                    diagnostics.push(Diagnostic {
                        level: self.level.clone(),
                        message: Self::make_duplicate_message(name, other_name),
                        spans: vec![const_data.expr.span.clone()],
                        suggestion: Some("Use a unique error value".to_string()),
                    });
                } else {
                    seen_values.insert(value_key, name);
                }
            }
        }

        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for ErrorConst<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = ErrorConstSettings::new();
        let mut lint = ErrorConst::new(analysis_cache, level, settings);
        lint.run()
    }
}

impl Lint for ErrorConst<'_, '_> {
    fn get_name() -> LintName {
        LintName::ErrorConst
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::ErrorConst)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::ErrorConst;
    use crate::analysis::linter::{Lint, LintLevel};
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .set_lint_level(ErrorConst::get_name(), LintLevel::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn valid_error_constants() {
        // TODO: Can we use `indoc` with curly brackets instead of parenthesis everywhere in this file so we can remove the `#[rustfmt::skip]`?
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant ERR_NOT_AUTHORIZED (err u1001))
            (define-constant ERR_PANIC (err u1002))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn valid_error_constants_different_types() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant ERR_A (err u1))
            (define-constant ERR_B (err 1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn non_err_prefix_ignored() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant MY_CONST u100)
            (define-constant SOME_VALUE (ok u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn err_prefix_not_error_type() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant ERR_NOT_AUTHORIZED u1001)
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let const_name = "ERR_NOT_AUTHORIZED";
        let expected_message = ErrorConst::make_not_err_message(&const_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(const_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn err_prefix_with_ok_type() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant ERR_FOO (ok u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let const_name = "ERR_FOO";
        let expected_message = ErrorConst::make_not_err_message(&const_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(const_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn duplicate_error_values() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant ERR_A (err u1))
            (define-constant ERR_B (err u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let expected_message = ErrorConst::make_duplicate_message(&"ERR_B".into(), &"ERR_A".into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn three_constants_two_duplicates() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant ERR_A (err u1))
            (define-constant ERR_B (err u2))
            (define-constant ERR_C (err u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let expected_message = ErrorConst::make_duplicate_message(&"ERR_C".into(), &"ERR_A".into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_not_err_type_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(error_const)]
            (define-constant ERR_NOT_AUTHORIZED u1001)
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_duplicate_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant ERR_A (err u1))
            ;; #[allow(error_const)]
            (define-constant ERR_B (err u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn both_checks_combined() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-constant ERR_A (err u1))
            (define-constant ERR_B u100)
            (define-constant ERR_C (err u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        // ERR_B: not an error type
        // ERR_C: duplicate of ERR_A
        assert_eq!(result.diagnostics.len(), 2);
    }
}
