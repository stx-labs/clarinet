use std::path::PathBuf;

use clarinet_deployments::setup_session_with_deployment;
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity_repl::repl::debug::dap::{DAPDebugger, LaunchMode};
use clarity_repl::utils::Environment;

#[cfg(feature = "telemetry")]
use super::telemetry::{telemetry_report_event, DeveloperUsageDigest, DeveloperUsageEvent};
use crate::deployments::generate_default_deployment;

pub fn run_dap() -> Result<(), String> {
    let mut dap = DAPDebugger::new();
    match dap.init() {
        Ok(launch_mode) => run_dap_session(&mut dap, launch_mode),
        Err(e) => Err(format!("dap_init: {e}")),
    }
}

fn setup_dap_session(
    dap: &mut DAPDebugger,
    manifest_location_str: &str,
) -> Result<(clarity_repl::repl::Session, clarinet_files::ProjectManifest), String> {
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

    Ok((session, project_manifest))
}

fn run_dap_session(dap: &mut DAPDebugger, launch_mode: LaunchMode) -> Result<(), String> {
    match launch_mode {
        LaunchMode::Expression {
            manifest,
            expression,
        } => {
            let (mut session, _) = setup_dap_session(dap, &manifest)?;

            match session.eval_with_hooks(expression, Some(vec![dap]), false) {
                Ok(_result) => Ok(()),
                Err(_diagnostics) => Err("unable to interpret expression".to_string()),
            }
        }
    }
}
