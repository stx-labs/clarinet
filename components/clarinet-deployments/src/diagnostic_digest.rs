use std::collections::HashMap;

use clarity_repl::clarity::diagnostic::{Diagnostic, Level};
use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
use clarity_repl::repl::diagnostic::output_code;
use colored::Colorize;

use crate::types::{DeploymentSpecification, TransactionSpecification};

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
            let (source, contract_location, is_requirement) =
                match deployment.contracts.get(contract_id) {
                    Some((source, location)) => {
                        contracts_checked += 1;
                        (
                            source.as_str(),
                            location.to_string_lossy().to_string(),
                            false,
                        )
                    }
                    None => {
                        // Look up requirement source from the deployment plan
                        let Some((source, location)) =
                            find_requirement_in_plan(deployment, contract_id)
                        else {
                            continue;
                        };
                        (source, location, true)
                    }
                };

            // For requirements, only show errors (not lints/warnings)
            let relevant_diags: Vec<&Diagnostic> = if is_requirement {
                diags.iter().filter(|d| d.level == Level::Error).collect()
            } else {
                diags.iter().collect()
            };

            if relevant_diags.is_empty() {
                if !is_requirement {
                    full_success += 1;
                }
                continue;
            }

            let formatted_lines: Vec<String> = source.lines().map(|l| l.to_string()).collect();

            for diagnostic in relevant_diags {
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

                if let Some(span) = diagnostic.spans.first() {
                    outputs.push(format!(
                        "{} {}:{}:{}",
                        "-->".blue().bold(),
                        contract_location,
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

fn find_requirement_in_plan<'a>(
    deployment: &'a DeploymentSpecification,
    contract_id: &QualifiedContractIdentifier,
) -> Option<(&'a str, String)> {
    for batch in &deployment.plan.batches {
        for tx in &batch.transactions {
            match tx {
                TransactionSpecification::EmulatedContractPublish(data) => {
                    let tx_id = QualifiedContractIdentifier::new(
                        data.emulated_sender.clone(),
                        data.contract_name.clone(),
                    );
                    if &tx_id == contract_id {
                        return Some((&data.source, data.location.to_string_lossy().to_string()));
                    }
                }
                TransactionSpecification::RequirementPublish(data) => {
                    if &data.contract_id == contract_id {
                        return Some((&data.source, data.location.to_string_lossy().to_string()));
                    }
                }
                _ => {}
            }
        }
    }
    None
}
