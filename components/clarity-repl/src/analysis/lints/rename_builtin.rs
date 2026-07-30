//! Lint to detect calls to builtin functions that were renamed in a later
//! Clarity version.  All renames are checked in a single AST pass, so adding
//! a new entry here does not require an additional traversal.

use std::collections::HashSet;

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

/// Pre-scan `expressions` to collect all user-defined function names
/// (`define-private`, `define-public`, `define-read-only`).
///
/// We do this in a separate pass so that forward references are handled correctly.
fn collect_user_defined_functions(expressions: &[SymbolicExpression]) -> HashSet<&ClarityName> {
    let mut names = HashSet::new();
    for expr in expressions {
        let Some(list) = expr.match_list() else {
            continue;
        };
        let Some(first) = list.first().and_then(|e| e.match_atom()) else {
            continue;
        };
        if !matches!(
            first.as_str(),
            "define-private" | "define-public" | "define-read-only"
        ) {
            continue;
        }
        if let Some(name) = list
            .get(1)
            .and_then(|e| e.match_list())
            .and_then(|sig| sig.first().and_then(|n| n.match_atom()))
        {
            names.insert(name);
        }
    }
    names
}

pub fn check_rename_builtin(
    expressions: &[SymbolicExpression],
    clarity_version: ClarityVersion,
    annotations: &[Annotation],
    level: Level,
) -> Vec<Diagnostic> {
    let mut checker = RenameBuiltin::new(clarity_version, annotations, level, expressions);
    traverse(&mut checker, expressions);
    checker.diagnostics
}

pub struct RenameBuiltin<'a> {
    clarity_version: ClarityVersion,
    diagnostics: Vec<Diagnostic>,
    annotations: &'a [Annotation],
    level: Level,
    active_annotation: Option<usize>,
    /// Names of functions defined in this contract.  Calls to these names must
    /// not be flagged as renamed builtins.
    user_defined_functions: HashSet<&'a ClarityName>,
}

impl<'a> RenameBuiltin<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a [Annotation],
        level: Level,
        expressions: &'a [SymbolicExpression],
    ) -> Self {
        Self {
            clarity_version,
            diagnostics: Vec::new(),
            annotations,
            level,
            active_annotation: None,
            user_defined_functions: collect_user_defined_functions(expressions),
        }
    }

    fn run(mut self, contract_analysis: &'a ContractAnalysis) -> AnalysisResult {
        traverse(&mut self, &contract_analysis.expressions);
        Ok(self.diagnostics)
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
        let entry = RENAMES.iter().find(|e| {
            e.old_name == name.as_str()
                && self.clarity_version >= e.since
                && !self.user_defined_functions.contains(name)
        });

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
            &analysis_cache.contract_analysis.expressions,
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
    use clarity::vm::diagnostic::{Diagnostic, Level};
    use clarity::vm::ClarityVersion;
    use indoc::indoc;

    use super::{check_rename_builtin, RenameBuiltin};
    use crate::analysis::annotation::Annotation;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;
    use crate::test_fixtures::clarity_contract::ClarityContractBuilder;

    /// Parse `snippet` at the given epoch/version and run the lint over the raw AST.
    ///
    /// `build_ast` only parses, so this exercises the visitor in isolation: the
    /// result cannot be influenced by the type checker accepting or rejecting the
    /// contract.
    fn lint_ast_at(
        snippet: String,
        epoch: StacksEpochId,
        clarity_version: ClarityVersion,
    ) -> Vec<Diagnostic> {
        let mut session = Session::new_without_boot_contracts(SessionSettings::default());
        session.update_epoch(epoch);

        let contract = ClarityContractBuilder::new()
            .code_source(snippet)
            .name("checker")
            .epoch(epoch)
            .clarity_version(clarity_version)
            .build();
        let (ast, _, _) = session.interpreter.build_ast(&contract);

        check_rename_builtin(
            &ast.expressions,
            clarity_version,
            &Vec::<Annotation>::new(),
            Level::Warning,
        )
    }

    /// Run the lint over `snippet` as a Clarity 6 contract.
    fn lint_ast(snippet: String) -> Vec<Diagnostic> {
        lint_ast_at(snippet, StacksEpochId::Epoch40, ClarityVersion::Clarity6)
    }

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

    /// `with-stacking` is still a builtin in Clarity 5, so using it must not warn.
    #[test]
    fn with_stacking_no_warning_in_clarity5() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test)
                (as-contract? ((with-stacking u1)) true))
        ").to_string();

        let diagnostics = lint_ast_at(snippet, StacksEpochId::Epoch34, ClarityVersion::Clarity5);

        assert!(
            diagnostics.is_empty(),
            "`with-stacking` is a valid builtin in Clarity 5 and must not be \
             reported as renamed; got {diagnostics:?}"
        );
    }

    /// `with-stacking` was introduced in Clarity 4, so at Clarity 3 it is neither a
    /// builtin nor the post-rename spelling
    #[test]
    fn no_warning_before_with_stacking_was_introduced() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test)
                (ok (with-stacking u1)))
        ").to_string();

        let diagnostics = lint_ast_at(snippet, StacksEpochId::Epoch30, ClarityVersion::Clarity3);

        assert!(
            diagnostics.is_empty(),
            "`with-stacking` did not exist before Clarity 4, so a Clarity 3 contract \
             must not be told it was renamed; got {diagnostics:?}"
        );
    }

    /// The lint must fire when `with-stacking` appears in a `restrict-assets?`
    /// allowance list.
    #[test]
    fn warn_with_stacking_in_restrict_assets() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-public (test)
                (restrict-assets? tx-sender ((with-stacking u1)) (ok true)))
        ").to_string();

        let diagnostics = lint_ast(snippet);

        assert_eq!(
            diagnostics.len(),
            1,
            "expected `with-stacking` in a `restrict-assets?` allowance list to be \
             flagged, but the allowance list is never traversed; got {diagnostics:?}"
        );
    }

    /// `with-stacking` is a legal user-defined function name in Clarity 6 (it is no
    /// longer a builtin), so calling a user-defined `with-stacking` must not be
    /// reported as the renamed builtin.
    #[test]
    fn no_warning_for_user_defined_with_stacking() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (with-stacking (amount uint))
                amount)
            (define-public (test)
                (ok (with-stacking u1)))
        ").to_string();

        let diagnostics = lint_ast(snippet);

        assert!(
            diagnostics.is_empty(),
            "`with-stacking` is a legal user-defined function name in Clarity 6, so \
             calling it must not be reported as the renamed builtin; got {diagnostics:?}"
        );
    }

    #[test]
    fn no_warning_for_user_defined_with_stacking_end_to_end() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-private (with-stacking (amount uint))
                amount)
            (define-public (test)
                (ok (with-stacking u1)))
        ").to_string();

        let result = run_snippet(snippet)
            .expect("contract defining its own `with-stacking` should type-check");

        let renamed: Vec<&str> = result
            .lint_diagnostics
            .iter()
            .map(|ld| ld.diagnostic.message.as_str())
            .filter(|message| message.contains("was renamed to `with-staking`"))
            .collect();

        assert!(
            renamed.is_empty(),
            "calling a user-defined `with-stacking` must not be reported as the \
             renamed builtin; got {renamed:?}"
        );
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
