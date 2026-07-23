use std::io::{BufRead, BufWriter, Write};
use std::path::PathBuf;

use clarinet_deployments::setup_session_with_deployment;
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity::vm::types::QualifiedContractIdentifier;
use clarity::vm::EvaluationResult;
use clarity_repl::repl::clarity_values::value_to_string;
use clarity_repl::repl::debug::dap::DAPDebugger;
use clarity_repl::utils::Environment;

#[cfg(feature = "telemetry")]
use super::telemetry::{telemetry_report_event, DeveloperUsageDigest, DeveloperUsageEvent};
use crate::deployments::generate_default_deployment;

pub fn run_dap() -> Result<(), String> {
    let mut dap = DAPDebugger::new();
    match dap.init() {
        Ok((manifest_location_str, expression)) => {
            let manifest_location = PathBuf::from(&manifest_location_str);
            let project_manifest = ProjectManifest::from_location(&manifest_location, false)?;
            let (mut deployment, artifacts, _) = generate_default_deployment(
                &project_manifest,
                &StacksNetwork::Simnet,
                false,
                Environment::Simnet,
            )?;
            let mut session = setup_session_with_deployment(
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

            // Begin execution of the expression in debug mode
            match session.eval_with_hooks(expression, Some(vec![&mut dap]), false) {
                Ok(_result) => Ok(()),
                Err(_diagnostics) => Err("unable to interpret expression".to_string()),
            }
        }
        Err(e) => Err(format!("dap_init: {e}")),
    }
}

/// Run a DAP debug server that accepts two TCP connections:
///
/// 1. A DAP client (e.g. VSCode) connects on `dap_port` using the attach protocol.
/// 2. A test runner (e.g. Vitest) connects on `sdk_port` and sends newline-delimited
///    JSON requests to evaluate Clarity snippets under debugger control.
///
/// This allows test authors to place breakpoints in `.clar` source files and have
/// the debugger pause when those lines are reached during a test run.
pub fn run_dap_server(dap_port: u16, sdk_port: u16, manifest_path: PathBuf) -> Result<(), String> {
    // Set up the simnet session from the project manifest.
    let project_manifest = ProjectManifest::from_location(&manifest_path, false)?;
    let (mut deployment, artifacts, _) = generate_default_deployment(
        &project_manifest,
        &StacksNetwork::Simnet,
        false,
        Environment::Simnet,
    )?;
    let mut session = setup_session_with_deployment(
        &project_manifest,
        &mut deployment,
        Some(&artifacts.asts),
        false,
    )
    .session;

    // Wait for a VSCode DAP connection on the DAP port.
    eprintln!("clarinet dap: listening for DAP client on 127.0.0.1:{dap_port}");
    let dap_listener = std::net::TcpListener::bind(("127.0.0.1", dap_port))
        .map_err(|e| format!("failed to bind DAP port {dap_port}: {e}"))?;
    let (dap_stream, _) = dap_listener
        .accept()
        .map_err(|e| format!("DAP accept error: {e}"))?;

    // Build the DAP debugger over the TCP stream and populate the contract path maps
    // before the attach handshake so that setBreakpoints requests can resolve paths.
    let mut dap = DAPDebugger::from_std_tcp_stream(dap_stream);
    for (contract_id, (_, location)) in deployment.contracts {
        // Canonicalize to absolute path so the map matches what VSCode sends.
        let abs = std::fs::canonicalize(&location).unwrap_or(location);
        dap.path_to_contract_id
            .insert(abs.clone(), contract_id.clone());
        dap.contract_id_to_path.insert(contract_id, abs);
    }

    // Complete the DAP attach handshake: initialize → attach → setBreakpoints → configurationDone.
    eprintln!("clarinet dap: completing attach handshake...");
    dap.init_attach()
        .map_err(|e| format!("DAP init_attach error: {e:?}"))?;
    eprintln!("clarinet dap: DAP client attached");

    // Wait for the test-runner SDK client on the SDK port.
    eprintln!("clarinet dap: listening for SDK client on 127.0.0.1:{sdk_port}");
    let sdk_listener = std::net::TcpListener::bind(("127.0.0.1", sdk_port))
        .map_err(|e| format!("failed to bind SDK port {sdk_port}: {e}"))?;
    let (sdk_stream, _) = sdk_listener
        .accept()
        .map_err(|e| format!("SDK accept error: {e}"))?;
    eprintln!("clarinet dap: SDK client connected, ready to evaluate");

    // Clone the stream so we can have independent reader/writer halves.
    let sdk_read_stream = sdk_stream
        .try_clone()
        .map_err(|e| format!("SDK stream clone error: {e}"))?;
    let mut reader = std::io::BufReader::new(sdk_read_stream);
    let mut writer = BufWriter::new(sdk_stream);
    let mut line = String::new();

    loop {
        line.clear();
        match reader.read_line(&mut line) {
            Ok(0) => break, // EOF — client disconnected
            Ok(_) => {}
            Err(e) => return Err(format!("SDK read error: {e}")),
        }

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let request: serde_json::Value = match serde_json::from_str(trimmed) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("clarinet dap: invalid SDK request ({e}): {trimmed}");
                continue;
            }
        };

        let id = request["id"].clone();
        let method = request["method"].as_str().unwrap_or("");

        match method {
            "disconnect" => {
                let resp = serde_json::json!({"id": id, "result": null});
                let _ = writeln!(writer, "{}", serde_json::to_string(&resp).unwrap());
                let _ = writer.flush();
                break;
            }
            // `eval` runs an arbitrary Clarity snippet in the simnet under the debugger.
            "eval" => {
                let snippet = request["snippet"].as_str().unwrap_or("").to_string();
                let contract_id = QualifiedContractIdentifier::transient();
                dap.prepare_for_call(&contract_id, &snippet);

                let response = eval_snippet(&mut session, &mut dap, snippet, id);
                write_response(&mut writer, &response)?;
            }
            // `call` evaluates a contract call by name, resolving the contract to its full
            // principal and optionally setting the tx-sender.
            "call" => {
                let contract = request["contract"].as_str().unwrap_or("").to_string();
                let function = request["function"].as_str().unwrap_or("").to_string();
                let sender = request["sender"].as_str().map(|s| s.to_string());
                let args: Vec<String> = request["args"]
                    .as_array()
                    .map(|a| {
                        a.iter()
                            .filter_map(|v| v.as_str().map(|s| s.to_string()))
                            .collect()
                    })
                    .unwrap_or_default();

                // Resolve the short contract name to a full principal by matching against
                // the contracts registered in the DAP debugger's path map.
                let full_contract_principal =
                    if contract.contains('.') && !contract.starts_with('.') {
                        // Already a full principal like "ST1PQHQ....counter"
                        format!("'{contract}")
                    } else {
                        // Short name like "counter" — find in deployed contracts
                        let short_name = contract.trim_start_matches('.');
                        dap.contract_id_to_path
                            .keys()
                            .find(|id| id.name.as_str() == short_name)
                            .map(|id| format!("'{id}"))
                            .unwrap_or_else(|| format!(".{short_name}"))
                    };

                let args_str = args.join(" ");
                let snippet = if args_str.is_empty() {
                    format!("(contract-call? {full_contract_principal} {function})")
                } else {
                    format!("(contract-call? {full_contract_principal} {function} {args_str})")
                };

                // Temporarily set the tx-sender if the client provided one.
                let original_sender = sender.as_ref().map(|_| session.get_tx_sender());
                if let Some(ref s) = sender {
                    session.set_tx_sender(s);
                }

                let contract_id = dap
                    .contract_id_to_path
                    .keys()
                    .find(|id| id.name.as_str() == contract.trim_start_matches('.'))
                    .cloned()
                    .unwrap_or_else(QualifiedContractIdentifier::transient);

                dap.prepare_for_call(&contract_id, &snippet);
                let response = eval_snippet(&mut session, &mut dap, snippet, id);

                if let Some(ref prev) = original_sender {
                    session.set_tx_sender(prev);
                }

                write_response(&mut writer, &response)?;
            }
            _ => {
                let response =
                    serde_json::json!({"id": id, "error": format!("unknown method: {method}")});
                write_response(&mut writer, &response)?;
            }
        }
    }

    Ok(())
}

fn eval_snippet(
    session: &mut clarity_repl::repl::Session,
    dap: &mut DAPDebugger,
    snippet: String,
    id: serde_json::Value,
) -> serde_json::Value {
    match session.eval_with_hooks(snippet, Some(vec![dap]), false) {
        Ok(result) => {
            let value_str = match &result.result {
                EvaluationResult::Contract(contract_result) => contract_result
                    .result
                    .as_ref()
                    .map(value_to_string)
                    .unwrap_or_default(),
                EvaluationResult::Snippet(snippet_result) => {
                    value_to_string(&snippet_result.result)
                }
            };
            serde_json::json!({"id": id, "result": {"value": value_str}})
        }
        Err(diagnostics) => {
            let errors: Vec<&str> = diagnostics.iter().map(|d| d.message.as_str()).collect();
            serde_json::json!({"id": id, "error": errors.join("; ")})
        }
    }
}

fn write_response(writer: &mut impl Write, response: &serde_json::Value) -> Result<(), String> {
    let response_str =
        serde_json::to_string(response).map_err(|e| format!("serialize error: {e}"))?;
    writeln!(writer, "{response_str}").map_err(|e| format!("write error: {e}"))?;
    writer.flush().map_err(|e| format!("flush error: {e}"))?;
    Ok(())
}
