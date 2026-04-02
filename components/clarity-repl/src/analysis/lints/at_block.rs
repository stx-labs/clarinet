//! Lint to warn about usage of `at-block`, which is proposed for removal
//! starting in Epoch 3.5.

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::{ClarityVersion, SymbolicExpression};

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

pub struct AtBlock<'a> {
    clarity_version: ClarityVersion,
    diagnostics: Vec<Diagnostic>,
    annotations: &'a Vec<Annotation>,
    level: Level,
    active_annotation: Option<usize>,
}

impl<'a> AtBlock<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
    ) -> Self {
        Self {
            clarity_version,
            level,
            diagnostics: Vec::new(),
            annotations,
            active_annotation: None,
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        // In Clarity 5+, `at-block` is no longer part of the language, so allow it to be used as a user-defined function
        if contract_analysis.clarity_version >= ClarityVersion::Clarity5 {
            return Ok(self.diagnostics);
        }
        traverse(&mut self, &contract_analysis.expressions);
        Ok(self.diagnostics)
    }

    fn allow(&self) -> bool {
        self.active_annotation
            .map(|idx| Self::match_allow_annotation(&self.annotations[idx]))
            .unwrap_or(false)
    }
}

impl<'a> ASTVisitor<'a> for AtBlock<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_at_block(
        &mut self,
        expr: &'a SymbolicExpression,
        _block: &'a SymbolicExpression,
        _inner: &'a SymbolicExpression,
    ) -> bool {
        self.active_annotation = get_index_of_span(self.annotations, &expr.span);
        if self.allow() {
            return true;
        }

        self.diagnostics.push(Diagnostic {
            level: self.level.clone(),
            message: "A SIP proposes removing the `at-block` function starting in Epoch 3.4. \
                If approved, new contracts using `at-block` will be rejected at deployment. \
                Learn more and vote [here](https://ballot.gg/67a34537-0375-4046-a4b7-432e8dfd4eb3/1MP9LjMBZRKXWq3tRio6nGwFyUoboozEQx)."
                .to_string(),
            spans: vec![expr.span.clone()],
            suggestion: None,
        });

        true
    }
}

impl AnalysisPass for AtBlock<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let checker = AtBlock::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
        );
        checker.run(analysis_cache.contract_analysis)
    }
}

impl Lint for AtBlock<'_> {
    fn get_name() -> LintName {
        LintName::AtBlock
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => warning_kinds.contains(&WarningKind::AtBlock),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::types::StacksEpochId;
    use clarity::vm::diagnostic::Level;
    use indoc::indoc;

    use super::AtBlock;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> Vec<String> {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(AtBlock::get_name(), Level::Warning);

        let mut session = Session::new_without_boot_contracts(settings);
        session.update_epoch(StacksEpochId::Epoch33);

        let (output, _result) = session
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet");
        output
    }

    #[test]
    fn warn_at_block_usage() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (get-info)
                (at-block 0x0000000000000000000000000000000000000000000000000000000000000000
                    (var-get my-var)
                )
            )
            (define-data-var my-var uint u0)
        ").to_string();

        let output = run_snippet(snippet);
        assert!(output.iter().any(|line| line.contains("warning[")));
        assert!(output.iter().any(|line| line.contains("at-block")));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-read-only (get-info)
                ;; #[allow(at_block)]
                (at-block 0x0000000000000000000000000000000000000000000000000000000000000000
                    (var-get my-var)
                )
            )
            (define-data-var my-var uint u0)
        ").to_string();

        let output = run_snippet(snippet);
        assert!(!output.iter().any(|line| line.contains("warning[")));
    }
}
