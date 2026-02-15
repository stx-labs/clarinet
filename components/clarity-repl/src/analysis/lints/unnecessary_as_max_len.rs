//! Lint to find unnecessary `as-max-len?` calls where the sequence's maximum
//! possible size is already less than or equal to the specified length.

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::type_checker::contexts::TypeMap;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::types::{SequenceSubtype, StringSubtype, TypeSignature};

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct UnnecessaryAsMaxLen<'a> {
    clarity_version: ClarityVersion,
    diagnostics: Vec<Diagnostic>,
    annotations: &'a Vec<Annotation>,
    type_map: Option<&'a TypeMap>,
    level: Level,
    active_annotation: Option<usize>,
}

impl<'a> UnnecessaryAsMaxLen<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        type_map: Option<&'a TypeMap>,
        level: Level,
    ) -> Self {
        Self {
            clarity_version,
            level,
            diagnostics: Vec::new(),
            annotations,
            type_map,
            active_annotation: None,
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        traverse(&mut self, &contract_analysis.expressions);
        Ok(self.diagnostics)
    }

    fn set_active_annotation(&mut self, span: &Span) {
        self.active_annotation = get_index_of_span(self.annotations, span);
    }

    fn allow(&self) -> bool {
        self.active_annotation
            .map(|idx| Self::match_allow_annotation(&self.annotations[idx]))
            .unwrap_or(false)
    }

    /// Get the maximum possible length of a sequence type from the type map.
    fn get_sequence_max_len(&self, sequence: &SymbolicExpression) -> Option<u32> {
        let type_sig = self.type_map?.get_type_expected(sequence)?;

        match type_sig {
            TypeSignature::SequenceType(seq_type) => match seq_type {
                SequenceSubtype::ListType(list_data) => Some(list_data.get_max_len()),
                SequenceSubtype::BufferType(len) => Some(u32::from(len)),
                SequenceSubtype::StringType(StringSubtype::ASCII(len)) => Some(u32::from(len)),
                SequenceSubtype::StringType(StringSubtype::UTF8(len)) => Some(u32::from(len)),
            },
            _ => None,
        }
    }

    fn add_diagnostic(&mut self, expr: &SymbolicExpression, max_len: u32, specified_len: u128) {
        self.diagnostics.push(Diagnostic {
            level: self.level.clone(),
            message: Self::make_diagnostic_message(max_len, specified_len),
            spans: vec![expr.span.clone()],
            suggestion: Some(Self::suggestion()),
        });
    }

    fn make_diagnostic_message(max_len: u32, specified_len: u128) -> String {
        format!(
            "unnecessary `as-max-len?`: sequence already has a maximum length of {max_len}, \
             which is within the specified limit of {specified_len}"
        )
    }

    fn suggestion() -> String {
        "Remove the `as-max-len?` call and use the sequence directly".to_owned()
    }
}

impl<'a> ASTVisitor<'a> for UnnecessaryAsMaxLen<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_as_max_len(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
        length: u128,
    ) -> bool {
        self.set_active_annotation(&expr.span);
        if self.allow() {
            return true;
        }

        if let Some(max_len) = self.get_sequence_max_len(sequence) {
            if u128::from(max_len) <= length {
                self.add_diagnostic(expr, max_len, length);
            }
        }

        true
    }
}

impl AnalysisPass for UnnecessaryAsMaxLen<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let checker = UnnecessaryAsMaxLen::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            analysis_cache.contract_analysis.type_map.as_ref(),
            level,
        );
        checker.run(analysis_cache.contract_analysis)
    }
}

impl Lint for UnnecessaryAsMaxLen<'_> {
    fn get_name() -> LintName {
        LintName::UnnecessaryAsMaxLen
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::UnnecessaryAsMaxLen)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::ExecutionResult;
    use indoc::indoc;

    use super::UnnecessaryAsMaxLen;
    use crate::analysis::linter::{Lint, LintLevel};
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .set_lint_level(UnnecessaryAsMaxLen::get_name(), LintLevel::Warning);

        Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn warn_unnecessary_as_max_len_list() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (test-func (items (list 5 uint)))
                (as-max-len? items u10)
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].message,
            UnnecessaryAsMaxLen::make_diagnostic_message(5, 10)
        );
        assert!(output[0].contains("warning:"));
    }

    #[test]
    fn warn_unnecessary_as_max_len_list_equal() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (test-func (items (list 5 uint)))
                (as-max-len? items u5)
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].message,
            UnnecessaryAsMaxLen::make_diagnostic_message(5, 5)
        );
        assert!(output[0].contains("warning:"));
    }

    #[test]
    fn no_warn_when_length_is_smaller() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (test-func (items (list 10 uint)))
                (as-max-len? items u5)
            )
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn warn_unnecessary_as_max_len_buff() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (test-func (data (buff 8)))
                (as-max-len? data u20)
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].message,
            UnnecessaryAsMaxLen::make_diagnostic_message(8, 20)
        );
        assert!(output[0].contains("warning:"));
    }

    #[test]
    fn warn_unnecessary_as_max_len_string_ascii() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (test-func (name (string-ascii 10)))
                (as-max-len? name u50)
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].message,
            UnnecessaryAsMaxLen::make_diagnostic_message(10, 50)
        );
        assert!(output[0].contains("warning:"));
    }

    #[test]
    fn warn_unnecessary_as_max_len_string_utf8() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (test-func (name (string-utf8 10)))
                (as-max-len? name u50)
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].message,
            UnnecessaryAsMaxLen::make_diagnostic_message(10, 50)
        );
        assert!(output[0].contains("warning:"));
    }

    #[test]
    fn no_warn_buff_when_length_is_smaller() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (test-func (data (buff 20)))
                (as-max-len? data u10)
            )
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn warn_data_var_sequence() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var my-list (list 5 uint) (list))
            (define-private (test-func)
                (as-max-len? (var-get my-list) u10)
            )
        ").to_string();

        let (output, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 1);
        assert_eq!(
            result.diagnostics[0].message,
            UnnecessaryAsMaxLen::make_diagnostic_message(5, 10)
        );
        assert!(output[0].contains("warning:"));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (test-func (items (list 5 uint)))
                ;; #[allow(unnecessary_as_max_len)]
                (as-max-len? items u10)
            )
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
