use std::collections::HashMap;

use clarity_repl::clarity::diagnostic::{Diagnostic, Level};
use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
use clarity_repl::repl::diagnostic::output_code;
use colored::Colorize;

use crate::types::DeploymentSpecification;

#[allow(dead_code)]
pub struct DiagnosticsDigest {
    pub message: String,
    pub errors: usize,
    pub warnings: usize,
    pub contracts_checked: usize,
    full_success: usize,
    total: usize,
}

impl DiagnosticsDigest {
    pub fn new(
        contracts_diags: &HashMap<QualifiedContractIdentifier, Vec<Diagnostic>>,
        deployment: &DeploymentSpecification,
    ) -> DiagnosticsDigest {
        let mut full_success = 0;
        let mut warnings = 0;
        let mut errors = 0;
        let mut contracts_checked = 0;
        let mut outputs = vec![];
        let total = deployment.contracts.len();

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
            if diags.is_empty() {
                full_success += 1;
                continue;
            }

            let lines = source.lines();
            let formatted_lines: Vec<String> = lines.map(|l| l.to_string()).collect();

            for diagnostic in diags {
                match diagnostic.level {
                    Level::Error => {
                        errors += 1;
                        outputs.push(format!("{} {}", "error:".red().bold(), diagnostic.message));
                    }
                    Level::Warning => {
                        warnings += 1;
                        outputs.push(format!(
                            "{} {}",
                            "warning:".yellow().bold(),
                            diagnostic.message
                        ));
                    }
                    Level::Note => {
                        outputs.push(format!("{}: {}", "note:".blue().bold(), diagnostic.message));
                        outputs.append(&mut output_code(diagnostic, &formatted_lines));
                        continue;
                    }
                }
                let contract_path = contract_location.to_string_lossy().to_string();

                if let Some(span) = diagnostic.spans.first() {
                    outputs.push(format!(
                        "{} {}:{}:{}",
                        "-->".blue().bold(),
                        contract_path,
                        span.start_line,
                        span.start_column
                    ));
                }
                outputs.append(&mut output_code(diagnostic, &formatted_lines));

                if let Some(ref suggestion) = diagnostic.suggestion {
                    outputs.push(suggestion.to_string());
                }
            }
        }

        DiagnosticsDigest {
            full_success,
            errors,
            warnings,
            total,
            contracts_checked,
            message: outputs.join("\n"),
        }
    }

    pub fn has_feedbacks(&self) -> bool {
        self.errors > 0 || self.warnings > 0
    }
}
