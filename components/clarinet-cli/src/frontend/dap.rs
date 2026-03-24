use std::path::PathBuf;

use clarinet_deployments::setup_session_with_deployment;
use clarinet_files::transaction_log::{RecordedTx, TransactionLog};
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_repl::repl::clarity_values::uint8_to_value;
use clarity_repl::repl::debug::dap::{DAPDebugger, LaunchMode};
use clarity_repl::repl::{ClarityCodeSource, ClarityContract, ContractDeployer, Epoch};
use clarity_repl::utils::Environment;

#[cfg(feature = "telemetry")]
use super::telemetry::{telemetry_report_event, DeveloperUsageDigest, DeveloperUsageEvent};
use crate::deployments::generate_default_deployment;

pub fn run_dap() -> Result<(), String> {
    let mut dap = DAPDebugger::new();
    let launch_mode = dap.init().map_err(|e| format!("dap_init: {e}"))?;
    run_dap_session(dap, launch_mode)
}

fn setup_dap_session(
    dap: &mut DAPDebugger,
    manifest_location_str: &str,
) -> Result<clarity_repl::repl::Session, String> {
    let manifest_location = PathBuf::from(manifest_location_str);
    let project_manifest = ProjectManifest::from_location(&manifest_location, false)?;
    let (mut deployment, artifacts, _) = generate_default_deployment(
        &project_manifest,
        &StacksNetwork::Simnet,
        false,
        Environment::Simnet,
    )?;
    let session = setup_session_with_deployment(
        &project_manifest,
        &mut deployment,
        Some(&artifacts.asts),
        false,
    )
    .session;

    if project_manifest.project.telemetry {
        #[cfg(feature = "telemetry")]
        telemetry_report_event(DeveloperUsageEvent::DAPDebugStarted(
            DeveloperUsageDigest::new(
                &project_manifest.project.name,
                &project_manifest.project.authors,
            ),
        ));
    }

    for (contract_id, (_, location)) in deployment.contracts {
        dap.path_to_contract_id
            .insert(location.clone(), contract_id.clone());
        dap.contract_id_to_path.insert(contract_id, location);
    }

    Ok(session)
}

fn run_dap_session(mut dap: DAPDebugger, launch_mode: LaunchMode) -> Result<(), String> {
    match launch_mode {
        LaunchMode::Expression {
            manifest,
            expression,
        } => {
            let mut session = setup_dap_session(&mut dap, &manifest)?;

            match session.eval_with_hooks(expression, Some(vec![&mut dap]), false) {
                Ok(_result) => Ok(()),
                Err(_diagnostics) => Err("unable to interpret expression".to_string()),
            }
        }
        LaunchMode::TransactionLog { manifest, path } => {
            let mut session = setup_dap_session(&mut dap, &manifest)?;
            session.set_debug_hook(Box::new(dap));

            let tx_log_content = std::fs::read_to_string(&path)
                .map_err(|e| format!("failed to read transaction log '{}': {}", path, e))?;
            let tx_log: TransactionLog = serde_json::from_str(&tx_log_content)
                .map_err(|e| format!("failed to parse transaction log: {}", e))?;

            for tx in tx_log.transactions {
                replay_transaction(&mut session, tx)?;
            }
            Ok(())
        }
    }
}

fn replay_transaction(
    session: &mut clarity_repl::repl::Session,
    tx: RecordedTx,
) -> Result<(), String> {
    match tx {
        RecordedTx::CallPublicFn {
            contract,
            method,
            args,
            sender,
        } => {
            let parsed_args = deserialize_args(&args);
            session
                .call_contract_fn(&contract, &method, &parsed_args, &sender, false, false)
                .map_err(|diags| format_diagnostics(&diags))?;
            Ok(())
        }
        RecordedTx::CallPrivateFn {
            contract,
            method,
            args,
            sender,
        } => {
            let parsed_args = deserialize_args(&args);
            session
                .call_contract_fn(&contract, &method, &parsed_args, &sender, true, false)
                .map_err(|diags| format_diagnostics(&diags))?;
            Ok(())
        }
        RecordedTx::CallReadOnlyFn {
            contract,
            method,
            args,
            sender,
        } => {
            let parsed_args = deserialize_args(&args);
            session
                .call_contract_fn(&contract, &method, &parsed_args, &sender, false, false)
                .map_err(|diags| format_diagnostics(&diags))?;
            Ok(())
        }
        RecordedTx::DeployContract {
            name,
            content,
            sender,
            clarity_version,
        } => {
            let current_epoch = session.interpreter.datastore.get_current_epoch();
            let version = clarity_version
                .and_then(|v| clarity_repl::repl::clarity_version_from_u8(v as u8))
                .unwrap_or(ClarityVersion::default_for_epoch(current_epoch));

            let contract = ClarityContract {
                code_source: ClarityCodeSource::ContractInMemory(content),
                name,
                deployer: ContractDeployer::Address(sender),
                clarity_version: version,
                epoch: Epoch::Specific(current_epoch),
                skip_analysis: false,
            };
            session
                .deploy_contract(&contract, false, None)
                .map_err(|diags| format_diagnostics(&diags))?;
            Ok(())
        }
        RecordedTx::TransferSTX {
            amount,
            recipient,
            sender,
        } => {
            let initial_tx_sender = session.get_tx_sender();
            session.set_tx_sender(&sender);
            session
                .stx_transfer(amount, &recipient)
                .map_err(|diags| format_diagnostics(&diags))?;
            session.set_tx_sender(&initial_tx_sender);
            Ok(())
        }
        RecordedTx::AdvanceChainTip { count } => {
            session.advance_chain_tip(count);
            Ok(())
        }
    }
}

fn deserialize_args(args: &[Vec<u8>]) -> Vec<SymbolicExpression> {
    args.iter()
        .map(|a| SymbolicExpression::atom_value(uint8_to_value(a)))
        .collect()
}

fn format_diagnostics(diags: &[clarity::vm::diagnostic::Diagnostic]) -> String {
    diags
        .iter()
        .map(|d| d.message.as_str())
        .collect::<Vec<_>>()
        .join("; ")
}
