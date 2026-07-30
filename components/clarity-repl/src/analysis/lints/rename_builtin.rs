//! Lint to detect calls to builtin functions that were renamed in a later
//! Clarity version.  All renames are checked in a single AST pass, so adding
//! a new entry here does not require an additional traversal.

use clarity::vm::analysis::analysis_db::AnalysisDatabase;
use clarity::vm::analysis::types::ContractAnalysis;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::{ClarityName, ClarityVersion, SymbolicExpression};

use crate::analysis::annotation::{get_index_of_span, Annotation, AnnotationKind, WarningKind};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};
use crate::analysis::cache::AnalysisCache;
use crate::analysis::linter::Lint;
use crate::analysis::{self, AnalysisPass, AnalysisResult, LintName};

/// A single rename entry: the old name, the new name, and the minimum Clarity
/// version in which the rename takes effect.
struct RenameEntry {
    old_name: &'static str,
    new_name: &'static str,
    since: ClarityVersion,
}

/// All known builtin renames, checked in a single AST pass.
const RENAMES: &[RenameEntry] = &[RenameEntry {
    old_name: "with-stacking",
    new_name: "with-staking",
    since: ClarityVersion::Clarity6,
}];

pub fn check_rename_builtin(
    expressions: &[SymbolicExpression],
    clarity_version: ClarityVersion,
    annotations: &[Annotation],
    level: Level,
) -> Vec<Diagnostic> {
    let checker = RenameBuiltin::new(clarity_version, annotations, level);
    checker.run_expressions(expressions)
}

pub struct RenameBuiltin<'a> {
    clarity_version: ClarityVersion,
    diagnostics: Vec<Diagnostic>,
    annotations: &'a [Annotation],
    level: Level,
    active_annotation: Option<usize>,
}

impl<'a> RenameBuiltin<'a> {
    fn new(clarity_version: ClarityVersion, annotations: &'a [Annotation], level: Level) -> Self {
        Self {
            clarity_version,
            diagnostics: Vec::new(),
            annotations,
            level,
            active_annotation: None,
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        traverse(&mut self, &contract_analysis.expressions);
        Ok(self.diagnostics)
    }

    fn run_expressions(mut self, expressions: &'a [SymbolicExpression]) -> Vec<Diagnostic> {
        traverse(&mut self, expressions);
        self.diagnostics
    }

    fn allow(&self) -> bool {
        self.active_annotation
            .map(|idx| Self::match_allow_annotation(&self.annotations[idx]))
            .unwrap_or(false)
    }
}

impl<'a> ASTVisitor<'a> for RenameBuiltin<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_call_user_defined(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _args: &'a [SymbolicExpression],
    ) -> bool {
        let entry = RENAMES
            .iter()
            .find(|e| e.old_name == name.as_str() && self.clarity_version >= e.since);

        let Some(entry) = entry else {
            return true;
        };

        self.active_annotation = get_index_of_span(self.annotations, &expr.span);
        if self.allow() {
            return true;
        }

        self.diagnostics.push(Diagnostic {
            level: self.level.clone(),
            message: format!(
                "`{}` was renamed to `{}` in Clarity {}. Replace this call with `{}`.",
                entry.old_name, entry.new_name, entry.since, entry.new_name,
            ),
            spans: vec![expr.span.clone()],
            suggestion: Some(format!(
                "Replace `{}` with `{}`.",
                entry.old_name, entry.new_name
            )),
        });

        true
    }
}

impl AnalysisPass for RenameBuiltin<'_> {
    fn run_pass(
        _analysis_db: &mut AnalysisDatabase,
        analysis_cache: &mut AnalysisCache,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let checker = RenameBuiltin::new(
            analysis_cache.contract_analysis.clarity_version,
            analysis_cache.annotations,
            level,
        );
        checker.run(analysis_cache.contract_analysis)
    }
}

impl Lint for RenameBuiltin<'_> {
    fn get_name() -> LintName {
        LintName::RenameBuiltin
    }

    fn match_allow_annotation(annotation: &Annotation) -> bool {
        match &annotation.kind {
            AnnotationKind::Allow(warning_kinds) => {
                warning_kinds.contains(&WarningKind::RenameBuiltin)
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use clarity::types::StacksEpochId;
    use clarity::vm::diagnostic::Level;
    use indoc::indoc;

    use super::{check_rename_builtin, RenameBuiltin};
    use crate::analysis::annotation::Annotation;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;
    use crate::test_fixtures::clarity_contract::ClarityContractBuilder;

    fn run_snippet(
        snippet: String,
    ) -> Result<
        crate::repl::session::AnnotatedExecutionResult,
        Vec<clarity::vm::diagnostic::Diagnostic>,
    > {
        let mut settings = SessionSettings::default();
        settings
            .repl_settings
            .analysis
            .enable_lint(RenameBuiltin::get_name(), Level::Warning);

        let mut session = Session::new_without_boot_contracts(settings);
        session.update_epoch(StacksEpochId::Epoch40);

        match session.formatted_interpretation(snippet, Some("checker".to_string()), false, None) {
            Ok((_, result)) => Ok(result),
            Err((_, diagnostics)) => Err(diagnostics),
        }
    }

    #[test]
    fn warn_with_stacking_usage_in_clarity6() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test)
                (as-contract? ((with-stacking u1)) true))
        ").to_string();

        let result = run_snippet(snippet);
        match result {
            Ok(_) => panic!("expected analysis error for unresolved `with-stacking`"),
            Err(diagnostics) => {
                assert!(diagnostics
                    .iter()
                    .any(|diagnostic| { diagnostic.message.contains("with-staking") }));
            }
        }
    }

    #[test]
    fn check_pre_analysis_directly() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test)
                (as-contract? ((with-stacking u1)) true))
        ").to_string();

        let mut session = Session::new_without_boot_contracts(SessionSettings::default());
        session.update_epoch(StacksEpochId::Epoch40);

        let contract = ClarityContractBuilder::new()
            .code_source(snippet)
            .name("checker")
            .epoch(clarity::types::StacksEpochId::Epoch40)
            .clarity_version(clarity::vm::ClarityVersion::Clarity6)
            .build();
        let (ast, _, _) = session.interpreter.build_ast(&contract);

        let diagnostics = check_rename_builtin(
            &ast.expressions,
            clarity::vm::ClarityVersion::Clarity6,
            &Vec::<Annotation>::new(),
            Level::Warning,
        );

        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("with-staking"));
    }

    #[test]
    fn allow_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test)
                ;; #[allow(rename_builtin)]
                (as-contract? ((with-stacking u1)) true))
        ").to_string();

        let result = run_snippet(snippet);
        match result {
            Ok(_) => panic!("expected analysis error for unresolved `with-stacking`"),
            Err(diagnostics) => {
                assert!(!diagnostics.iter().any(|diagnostic| {
                    diagnostic.message.contains("was renamed to `with-staking`")
                }));
            }
        }
    }
}
