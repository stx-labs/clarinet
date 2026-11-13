use std::collections::HashMap;

use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::types::QualifiedContractIdentifier;
use clarity_repl::analysis::LintDiagnostic;
use clarity_repl::repl::diagnostic::output_diagnostic;

use crate::types::DeploymentSpecification;

/// Whether a diagnostic at `level` is shown when only `min` and above are.
/// `Level` has no ordering of its own.
pub fn meets_min_level(level: &Level, min: &Level) -> bool {
    let severity = |level: &Level| match level {
        Level::Note => 0,
        Level::Warning => 1,
        Level::Error => 2,
    };
    severity(level) >= severity(min)
}

pub struct DiagnosticsDigest {
    pub message: String,
    pub errors: usize,
    pub warnings: usize,
    pub contracts_checked: usize,
}

impl DiagnosticsDigest {
    pub fn new(
        contracts_diags: &HashMap<QualifiedContractIdentifier, Vec<Diagnostic>>,
        contracts_lint_diags: &HashMap<QualifiedContractIdentifier, Vec<LintDiagnostic>>,
        deployment: &DeploymentSpecification,
        min_level: &Level,
    ) -> DiagnosticsDigest {
        let mut warnings = 0;
        let mut errors = 0;
        let mut contracts_checked = 0;
        let mut outputs = vec![];

        for (contract_id, diags) in contracts_diags.iter() {
            let (source, contract_location) = match deployment.contracts.get(contract_id) {
                Some(entry) => {
                    contracts_checked += 1;
                    entry
                }
                None => {
                    // `deployment.contracts` only includes contracts from the project, requirements should be ignored
                    continue;
                }
            };

            let lint_diags = contracts_lint_diags.get(contract_id);
            let has_lint_diags = lint_diags.is_some_and(|lds| !lds.is_empty());

            if diags.is_empty() && !has_lint_diags {
                continue;
            }

            let formatted_lines: Vec<String> = source.lines().map(|l| l.to_string()).collect();
            let contract_path = contract_location.to_string_lossy().into_owned();

            for ld in diags
                .iter()
                .cloned()
                .map(LintDiagnostic::from)
                .chain(lint_diags.into_iter().flatten().cloned())
                .filter(|ld| meets_min_level(&ld.diagnostic.level, min_level))
            {
                match ld.diagnostic.level {
                    Level::Error => errors += 1,
                    Level::Warning => warnings += 1,
                    Level::Note => {}
                }
                outputs.append(&mut output_diagnostic(
                    &ld,
                    &contract_path,
                    &formatted_lines,
                ));
            }
        }

        DiagnosticsDigest {
            errors,
            warnings,
            contracts_checked,
            message: outputs.join("\n"),
        }
    }

    pub fn has_feedbacks(&self) -> bool {
        !self.message.is_empty()
    }
}
