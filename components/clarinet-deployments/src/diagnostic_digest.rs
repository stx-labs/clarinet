use std::collections::HashMap;

use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::types::QualifiedContractIdentifier;
use clarity_repl::analysis::linter::LintName;
use clarity_repl::analysis::LintDiagnostic;
use clarity_repl::repl::diagnostic::output_code;
use colored::Colorize;

use crate::types::DeploymentSpecification;

/// Format the diagnostic level with an optional lint name.
///
/// With a lint name: `warning[unused_const]:`
/// Without: `warning:`
fn level_string(level: &Level, lint_name: Option<&LintName>) -> String {
    let level_str = match level {
        Level::Error => "error".red().bold(),
        Level::Warning => "warning".yellow().bold(),
        Level::Note => "note".blue().bold(),
    };
    match lint_name {
        Some(name) => format!("{level_str}[{}]:", name.to_string().purple().bold()),
        None => format!("{level_str}:"),
    }
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

            // Chain parse/annotation diagnostics (no lint name) with lint diagnostics (with lint name)
            let all_diags = diags.iter().map(|d| (d, None)).chain(
                lint_diags
                    .into_iter()
                    .flatten()
                    .map(|ld| (&ld.diagnostic, ld.lint_name.as_ref())),
            );

            for (diagnostic, lint_name) in all_diags {
                match diagnostic.level {
                    Level::Error => {
                        errors += 1;
                        outputs.push(format!(
                            "{} {}",
                            level_string(&diagnostic.level, lint_name),
                            diagnostic.message,
                        ));
                    }
                    Level::Warning => {
                        warnings += 1;
                        outputs.push(format!(
                            "{} {}",
                            level_string(&diagnostic.level, lint_name),
                            diagnostic.message,
                        ));
                    }
                    Level::Note => {
                        outputs.push(format!(
                            "{} {}",
                            level_string(&diagnostic.level, lint_name),
                            diagnostic.message,
                        ));
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
