//! Lint to find unused FTs and NFTs
//! Tokens are considered unused if declared but never minted

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

struct UnusedTokenSettings {
    // TODO
}

impl UnusedTokenSettings {
    fn new() -> Self {
        Self {}
    }
}

pub struct UnusedToken<'a> {
    clarity_version: ClarityVersion,
    _settings: UnusedTokenSettings,
    annotations: &'a Vec<Annotation>,
    active_annotation: Option<usize>,
    /// Maps of tokens not yet used
    unused_fts: HashMap<&'a ClarityName, &'a SymbolicExpression>,
    unused_nfts: HashMap<&'a ClarityName, &'a SymbolicExpression>,
    /// Clarity diagnostic level
    level: Level,
}

impl<'a> UnusedToken<'a> {
    fn new(
        clarity_version: ClarityVersion,
        annotations: &'a Vec<Annotation>,
        level: Level,
        settings: UnusedTokenSettings,
    ) -> UnusedToken<'a> {
        Self {
            clarity_version,
            _settings: settings,
            level,
            annotations,
            active_annotation: None,
            unused_fts: HashMap::new(),
            unused_nfts: HashMap::new(),
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

    fn make_diagnostic_message_ft(name: &ClarityName) -> String {
        format!("fungible token `{name}` never used")
    }

    fn make_diagnostic_message_nft(name: &ClarityName) -> String {
        format!("non-fungible token `{name}` never used")
    }

    fn make_diagnostic(&self, expr: &'a SymbolicExpression, message: String) -> Diagnostic {
        Diagnostic {
            level: self.level.clone(),
            message,
            spans: vec![expr.span.clone()],
            suggestion: Some("Remove this expression".to_string()),
        }
    }

    fn generate_diagnostics(&mut self) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];

        for (name, expr) in &self.unused_fts {
            let message = Self::make_diagnostic_message_ft(name);
            let diagnostic = self.make_diagnostic(expr, message);
            diagnostics.push(diagnostic);
        }

        for (name, expr) in &self.unused_nfts {
            let message = Self::make_diagnostic_message_nft(name);
            let diagnostic = self.make_diagnostic(expr, message);
            diagnostics.push(diagnostic);
        }

        // Order the sets by the span of the error (the first diagnostic)
        diagnostics.sort_by(|a, b| a.spans[0].cmp(&b.spans[0]));
        diagnostics
    }
}

impl<'a> ASTVisitor<'a> for UnusedToken<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_ft(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _supply: Option<&'a SymbolicExpression>,
    ) -> bool {
        self.set_active_annotation(&expr.span);

        if !self.allow() {
            self.unused_fts.insert(name, expr);
        }

        true
    }

    fn visit_define_nft(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _nft_type: &'a SymbolicExpression,
    ) -> bool {
        self.set_active_annotation(&expr.span);

        if !self.allow() {
            self.unused_nfts.insert(name, expr);
        }

        true
    }

    fn visit_ft_mint(
        &mut self,
        _expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _amount: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        self.unused_fts.remove(token);
        true
    }

    fn visit_nft_mint(
        &mut self,
        _expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _identifier: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        self.unused_nfts.remove(token);
        true
    }
}

impl AnalysisPass for UnusedToken<'_> {
    fn run_pass(
        contract_analysis: &mut ContractAnalysis,
        _analysis_db: &mut AnalysisDatabase,
        annotations: &Vec<Annotation>,
        level: Level,
        _settings: &analysis::Settings,
    ) -> AnalysisResult {
        let settings = UnusedTokenSettings::new();
        let lint = UnusedToken::new(
            contract_analysis.clarity_version,
            annotations,
            level,
            settings,
        );
        lint.run(contract_analysis)
    }
}

impl Lint for UnusedToken<'_> {
    fn get_name() -> LintName {
        LintName::UnusedToken
    }
    fn match_allow_annotation(annotation: &Annotation) -> bool {
        matches!(
            annotation.kind,
            AnnotationKind::Allow(WarningKind::UnusedToken)
        )
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::diagnostic::Level;
    use clarity::vm::ExecutionResult;
    use indoc::{formatdoc, indoc};

    use super::UnusedToken;
    use crate::analysis::linter::Lint;
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    fn run_snippet(snippet: String) -> (Vec<String>, ExecutionResult) {
        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .enable_lint(UnusedToken::get_name(), Level::Warning);

        Session::new(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet")
    }

    #[test]
    fn ft_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-fungible-token sbtc)

            (define-public (mint (amount uint) (recipient principal))
                (ft-mint? sbtc amount recipient))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn ft_not_used() {
        let ft_name = "sbtc";

        #[rustfmt::skip]
        let snippet = formatdoc!("
            (define-fungible-token {ft_name})

            (define-public (mint (amount uint) (recipient principal))
                (err u1))
        ");

        let (output, result) = run_snippet(snippet);

        let expected_message = UnusedToken::make_diagnostic_message_ft(&ft_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(ft_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_ft_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_token)]
            (define-fungible-token sbtc)

            (define-public (mint (amount uint) (recipient principal))
                (err u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn nft_used() {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-non-fungible-token hashes (buff 32))

            (define-public (mint (hash (buff 32)) (recipient principal))
                (nft-mint? hashes hash recipient))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }

    #[test]
    fn nft_not_used() {
        let nft_name = "hashes";

        #[rustfmt::skip]
        let snippet = formatdoc!("
            (define-non-fungible-token {nft_name} (buff 32))

            (define-public (mint (hash (buff 32)) (recipient principal))
                (err u1))
        ");

        let (output, result) = run_snippet(snippet);

        let expected_message = UnusedToken::make_diagnostic_message_nft(&nft_name.into());

        assert_eq!(result.diagnostics.len(), 1);
        assert!(output[0].contains("warning:"));
        assert!(output[0].contains(nft_name));
        assert!(output[0].contains(&expected_message));
    }

    #[test]
    fn allow_nft_with_annotation() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_token)]
            (define-non-fungible-token hashes (buff 32))

            (define-public (mint (hash (buff 32)) (recipient principal))
                (err u1))
        ").to_string();

        let (_, result) = run_snippet(snippet);

        assert_eq!(result.diagnostics.len(), 0);
    }
}
