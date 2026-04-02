use std::collections::HashMap;

use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::types::QualifiedContractIdentifier;
use clarity_repl::analysis::LintDiagnostic;
use clarity_repl::repl::diagnostic::output_diagnostic;

use crate::types::DeploymentSpecification;

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
            let contract_path = contract_location.to_string_lossy().to_string();

            for ld in diags
                .iter()
                .cloned()
                .map(LintDiagnostic::from)
                .chain(lint_diags.into_iter().flatten().cloned())
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
        self.errors > 0 || self.warnings > 0
    }
}
