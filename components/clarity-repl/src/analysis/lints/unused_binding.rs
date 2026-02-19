//! Lint to find unused bindings from `let` statements or function args
//!
//! A diagnostic is generated if:
//!  - A function argument is not referenced
//!  - A `let` binding is not referenced

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity_types::ClarityName;

use crate::analysis::annotation::{Annotation, AnnotationKind, WarningKind};
use crate::analysis::cache::bindings::{BindingData, BindingType};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::util::is_explicitly_unused;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

struct UnusedBindingSettings {
    // TODO
}

impl UnusedBindingSettings {
    fn new() -> Self {
        Self {}
    }
}

pub struct UnusedBinding<'a, 'b>
where
    'b: 'a,
{
    analysis_cache: &'a mut AnalysisCache<'b>,
    _settings: UnusedBindingSettings,
    level: Level,
}

impl<'a, 'b> UnusedBinding<'a, 'b> {
    fn new(
        analysis_cache: &'a mut AnalysisCache<'b>,
        level: Level,
        settings: UnusedBindingSettings,
    ) -> UnusedBinding<'a, 'b> {
        Self {
            analysis_cache,
            _settings: settings,
            level,
        }
    }

    fn run(&mut self) -> AnalysisResult {
        // Process hashmap of unused constants and generate diagnostics
        let diagnostics = self.generate_diagnostics();

        Ok(diagnostics)
    }

    // Check if the expression is annotated with `allow(<lint_name>)`
    fn allow(binding_data: &BindingData, annotations: &[Annotation]) -> bool {
        binding_data
            .annotation
            .map(|idx| Self::match_allow_annotation(&annotations[idx]))
            .unwrap_or(false)
    }

    /// Make diagnostic message and suggestion for unused function argument
    fn make_diagnostic_strings(kind: BindingType, name: &ClarityName) -> (String, Option<String>) {
        let msg = match kind {
            BindingType::FunctionArg => format!("function parameter `{name}` is never used"),
            BindingType::LetBinding => format!("`let` binding `{name}` is never used"),
        };
        (
            msg,
            Some(
                "Remove this expression or suffix binding with '_' if this is intentional"
                    .to_string(),
            ),
        )
    }

    fn make_diagnostic(
        level: Level,
        span: Span,
        message: String,
        suggestion: Option<String>,
    ) -> Diagnostic {
        Diagnostic {
            level,
            message,
            spans: vec![span],
            suggestion,
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        let annotations = self.analysis_cache.annotations;
        let bindings = self.analysis_cache.get_bindings();

        for (binding, data) in bindings {
            if data.used || Self::allow(data, annotations) || is_explicitly_unused(binding.name) {
                continue;
            }
            let (message, suggestion) = Self::make_diagnostic_strings(binding.kind, binding.name);
            let diagnostic = Self::make_diagnostic(
                self.level.clone(),
                binding.span.clone(),
                message,
                suggestion,
            );
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a, 'b> AnalysisPass for UnusedBinding<'a, 'b> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedBindingSettings::new();
        let mut lint = UnusedBinding::new(analysis_cache, level, settings);
        lint.run()
    }
}

impl Lint for UnusedBinding<'_, '_> {
    fn get_name() -> LintName {
        LintName::UnusedBinding
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::UnusedBinding)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use clarity_types::diagnostic::Diagnostic;
    use indoc::indoc;

    use super::UnusedBinding;
    use crate::analysis::linter::{Lint, LintLevel};
    use crate::analysis::lints::unused_binding::BindingType;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet_no_panic(
        snippet: String,
    ) -> Result<(Vec<String>, ExecutionResult), (Vec<String>, Vec<Diagnostic>)> {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .set_lint_level(UnusedBinding::get_name(), LintLevel::Warning);

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
    fn used_function_arg() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (increment (x uint))
                (+ x u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_function_arg_in_let() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (increment (x uint))
                (let ((val (+ x u1)))
                    val))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn unused_function_arg() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (increment (x uint))
                (+ u1 u1))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "x";
        let (expected_message, _) =
            UnusedBinding::make_diagnostic_strings(BindingType::FunctionArg, &var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn multiple_unused_function_args() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (add (a uint) (b uint))
                (+ u1 u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 2);
    }

    #[test]
    fn allow_unused_function_arg_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_binding)]
            (define-read-only (increment (x uint))
                (+ u1 u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_unused_function_arg_with_naming_convention() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (increment (x_ uint))
                (+ u1 u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_let_binding() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (double (x uint))
                (let ((doubled (* x u2)))
                    doubled))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_let_binding_in_next_binding() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (triple (x uint))
                (let ((doubled (* x u2))
                      (tripled (* doubled x)))
                    tripled))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn used_let_binding_in_next_binding_in_nested_let() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (triple (x uint))
                (let ((doubled (* x u2))
                      (tripled
                          (let ((t (* doubled x)))
                              t)))
                    tripled))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn unused_let_binding() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (double (x uint))
                (let ((doubled (* x u2)))
                    (* x u2)))
        ").to_string();

        let (output, result) = run_snippet(snippet);

        let var_name = "doubled";
        let (expected_message, _) =
            UnusedBinding::make_diagnostic_strings(BindingType::LetBinding, &var_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(var_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_unused_let_binding_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (double (x uint))
                ;; #[allow(unused_binding)]
                (let ((doubled (* x u2)))
                    (* x u2)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn allow_unused_let_binding_with_naming_convention() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (double (x uint))
                (let ((doubled_ (* x u2)))
                    (* x u2)))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    /// Linter assumes we cannot redeclare or "shadow" bindings
    /// It would not work right in its current form if this were possible
    /// See comment in `BindingMapBuilder::traverse_let()` for partial context
    /// It is currently not possible in Clarity, but f this ever changes, this test will notify us
    #[test]
    fn cannot_redeclare_let_bindings() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (double (x uint))
                (let ((x (* x u2)))
                    x))
        ").to_string();

        let result = run_snippet_no_panic(snippet);

        let Err((_, diagnostics)) = result else {
            panic!("Expected snippet to fail")
        };

        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0]
            .message
            .contains("conflicts with previous value"));
    }
}
