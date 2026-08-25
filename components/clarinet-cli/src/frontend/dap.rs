use std::collections::HashMap;
use std::io::{BufRead, BufWriter, Write};
use std::path::{Path, PathBuf};

use clarinet_deployments::setup_session_with_deployment;
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity::vm::types::{PrincipalData, QualifiedContractIdentifier};
use clarity::vm::EvaluationResult;
use clarity_repl::repl::clarity_values::to_raw_value;
use clarity_repl::repl::debug::dap::DAPDebugger;
use clarity_repl::repl::Session;
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

// ---------------------------------------------------------------------------
// Session setup helper — builds a fresh simnet session from a manifest.
// Returns (session, accounts map, deployer address, contract→path pairs).
// ---------------------------------------------------------------------------
fn make_session(
    manifest_path: &Path,
) -> Result<
    (
        Session,
        HashMap<String, String>,
        String,
        Vec<(QualifiedContractIdentifier, PathBuf)>,
    ),
    String,
> {
    let project_manifest = ProjectManifest::from_location(manifest_path, false)?;
    let (mut deployment, artifacts, _) = generate_default_deployment(
        &project_manifest,
        &StacksNetwork::Simnet,
        false,
        Environment::Simnet,
    )?;

    // Extract accounts from genesis before deployment is consumed.
    let mut accounts: HashMap<String, String> = HashMap::new();
    let mut deployer = String::new();
    if let Some(ref genesis) = deployment.genesis {
        for wallet in &genesis.wallets {
            let addr = wallet.address.to_string();
            if wallet.name == "deployer" {
                deployer = addr.clone();
            }
            accounts.insert(wallet.name.clone(), addr);
        }
    }

    let contract_maps: Vec<(QualifiedContractIdentifier, PathBuf)> = deployment
        .contracts
        .iter()
        .map(|(contract_id, (_, location))| {
            let abs = std::fs::canonicalize(location).unwrap_or(location.clone());
            (contract_id.clone(), abs)
        })
        .collect();

    let session = setup_session_with_deployment(
        &project_manifest,
        &mut deployment,
        Some(&artifacts.asts),
        false,
    )
    .session;

    Ok((session, accounts, deployer, contract_maps))
}

/// Run a DAP debug server that accepts two TCP connections:
///
/// 1. (Optional) A DAP client (e.g. VSCode) connects on `dap_port` using the attach
///    protocol. When omitted the server runs in SDK-only mode: no breakpoints fire
///    but the test runner can still drive contract evaluation via the SDK port.
/// 2. A test runner (e.g. Vitest) connects on `sdk_port` and sends newline-delimited
///    JSON requests to evaluate Clarity snippets under debugger control.
///
/// Both listeners are bound before either connection is accepted, so the server
/// prints `CLARINET_DAP_SDK_READY:<sdk_port>` to stderr as soon as it is ready.
/// The SDK client and the DAP client then connect in any order; the eval loop
/// starts only after both (or just the SDK client in SDK-only mode) are ready.
pub fn run_dap_server(
    dap_port: Option<u16>,
    sdk_port: u16,
    manifest_path: PathBuf,
) -> Result<(), String> {
    let (mut session, mut accounts, _deployer, contract_maps) = make_session(&manifest_path)?;

    let sdk_listener = std::net::TcpListener::bind(("127.0.0.1", sdk_port))
        .map_err(|e| format!("failed to bind SDK port {sdk_port}: {e}"))?;
    let sdk_port = sdk_listener
        .local_addr()
        .map_err(|e| format!("failed to read SDK listener address: {e}"))?
        .port();

    // When a DAP port is given, bind that listener and spawn a background thread
    // that accepts the DAP client and drives the full attach handshake
    // (`init_attach`) to completion.  Running the handshake in a thread lets
    // `startDebugging` in the VSCode extension complete (it waits for
    // `configurationDone`) while the main thread concurrently waits for the
    // SDK client.  Without this separation the two sides deadlock: the extension
    // only opens the test terminal after `startDebugging` returns, so the SDK
    // client can only connect after the handshake is already done.
    let dap_thread = if let Some(dap_port) = dap_port {
        let dap_listener = std::net::TcpListener::bind(("127.0.0.1", dap_port))
            .map_err(|e| format!("failed to bind DAP port {dap_port}: {e}"))?;
        eprintln!("clarinet dap: listening for DAP client on 127.0.0.1:{dap_port}");
        let maps = contract_maps.clone();
        Some(std::thread::spawn(
            move || -> Result<DAPDebugger, String> {
                let (stream, _) = dap_listener
                    .accept()
                    .map_err(|e| format!("DAP accept error: {e}"))?;
                let mut d = DAPDebugger::from_std_tcp_stream(stream);
                for (contract_id, path) in &maps {
                    d.path_to_contract_id
                        .insert(path.clone(), contract_id.clone());
                    d.contract_id_to_path
                        .insert(contract_id.clone(), path.clone());
                }
                eprintln!("clarinet dap: completing attach handshake...");
                d.init_attach()
                    .map_err(|e| format!("DAP init_attach error: {e:?}"))?;
                eprintln!("clarinet dap: DAP client attached");
                Ok(d)
            },
        ))
    } else {
        None
    };

    // Signal readiness - both ports are now bound and accepting.
    eprintln!("CLARINET_DAP_SDK_READY:{sdk_port}");

    // Join the DAP thread (waits for the handshake to finish if it hasn't yet)
    // or build a headless debugger for SDK-only mode.
    let mut dap = if let Some(thread) = dap_thread {
        thread
            .join()
            .map_err(|_| "DAP handshake thread panicked".to_string())??
    } else {
        eprintln!("clarinet dap: running in SDK-only mode (no DAP client)");
        let mut d = DAPDebugger::no_op();
        for (contract_id, path) in &contract_maps {
            d.path_to_contract_id
                .insert(path.clone(), contract_id.clone());
            d.contract_id_to_path
                .insert(contract_id.clone(), path.clone());
        }
        d
    };

    // Accept SDK clients in a loop so successive Vitest workers (or a reconnect
    // after disconnect) can reuse the same server without restarting it.
    eprintln!("clarinet dap: listening for SDK client on 127.0.0.1:{sdk_port}");
    loop {
        let (sdk_stream, _) = sdk_listener
            .accept()
            .map_err(|e| format!("SDK accept error: {e}"))?;
        eprintln!("clarinet dap: SDK client connected");

        // Clone the stream so we can have independent reader/writer halves.
        let sdk_read_stream = sdk_stream
            .try_clone()
            .map_err(|e| format!("SDK stream clone error: {e}"))?;
        let mut reader = std::io::BufReader::new(sdk_read_stream);
        let mut writer = BufWriter::new(sdk_stream);
        let mut line = String::new();

        'request: loop {
            line.clear();
            match reader.read_line(&mut line) {
                Ok(0) => break 'request, // EOF - client disconnected
                Ok(_) => {}
                Err(e) => {
                    eprintln!("clarinet dap: SDK read error: {e}");
                    break 'request;
                }
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
                    break 'request;
                }

                // Re-initialise the simnet session from the manifest on disk.
                "initSession" => {
                    // Reject requests for a different project so callers get a clear
                    // error rather than silently using the wrong manifest.
                    if let Some(req) = request["manifestPath"].as_str() {
                        let req_path = PathBuf::from(req);
                        let req_canon = std::fs::canonicalize(&req_path).unwrap_or(req_path);
                        let srv_canon = std::fs::canonicalize(&manifest_path)
                            .unwrap_or_else(|_| manifest_path.clone());
                        if req_canon != srv_canon {
                            let resp = serde_json::json!({
                                "id": id,
                                "error": format!(
                                    "debug server uses '{}'; pass the same path to initSimnet()",
                                    manifest_path.display()
                                )
                            });
                            write_response(&mut writer, &resp)?;
                            continue;
                        }
                    }
                    match make_session(&manifest_path) {
                        Ok((new_session, new_accounts, _, new_maps)) => {
                            session = new_session;
                            accounts = new_accounts;
                            // Rebuild the contract maps in the debugger.
                            dap.path_to_contract_id.clear();
                            dap.contract_id_to_path.clear();
                            for (contract_id, path) in &new_maps {
                                dap.path_to_contract_id
                                    .insert(path.clone(), contract_id.clone());
                                dap.contract_id_to_path
                                    .insert(contract_id.clone(), path.clone());
                            }
                            let resp = serde_json::json!({"id": id, "result": {}});
                            write_response(&mut writer, &resp)?;
                        }
                        Err(e) => {
                            let resp = serde_json::json!({"id": id, "error": e});
                            write_response(&mut writer, &resp)?;
                        }
                    }
                }

                // Return the accounts map (name → address).
                "getAccounts" => {
                    let resp = serde_json::json!({"id": id, "result": {"accounts": accounts}});
                    write_response(&mut writer, &resp)?;
                }

                // Return STX and token balances as string-encoded amounts.
                "getAssetsMap" => {
                    let assets = session.get_assets_maps();
                    // Convert u128 balances to strings so they survive JSON without precision loss.
                    let as_strings: HashMap<&str, HashMap<String, String>> = assets
                        .iter()
                        .map(|(asset, holders)| {
                            let inner: HashMap<String, String> = holders
                                .iter()
                                .map(|(addr, bal)| (addr.clone(), bal.to_string()))
                                .collect();
                            (asset.as_str(), inner)
                        })
                        .collect();
                    let resp = serde_json::json!({"id": id, "result": {"assetsMap": as_strings}});
                    write_response(&mut writer, &resp)?;
                }

                // Return the current Stacks block height.
                "getBlockHeight" => {
                    let height = session.interpreter.get_block_height();
                    let resp = serde_json::json!({"id": id, "result": {"blockHeight": height}});
                    write_response(&mut writer, &resp)?;
                }

                // Return the current burn (Bitcoin) block height.
                "getBurnBlockHeight" => {
                    let height = session.interpreter.get_burn_block_height();
                    let resp = serde_json::json!({"id": id, "result": {"burnBlockHeight": height}});
                    write_response(&mut writer, &resp)?;
                }

                // Advance the chain tip by `count` blocks (default 1).
                "mineEmptyBlock" => {
                    let count = request["count"].as_u64().unwrap_or(1) as u32;
                    let new_height = session.advance_chain_tip(count);
                    let resp = serde_json::json!({"id": id, "result": {"blockHeight": new_height}});
                    write_response(&mut writer, &resp)?;
                }

                // Execute an arbitrary Clarity snippet under the debugger.
                "eval" | "execute" => {
                    let snippet = request["snippet"].as_str().unwrap_or("").to_string();
                    let contract_id = QualifiedContractIdentifier::transient();
                    dap.prepare_for_call(&contract_id, &snippet);
                    let inner = eval_snippet_as_tx(&mut session, &mut dap, snippet);
                    let response = wrap_response(id, inner);
                    write_response(&mut writer, &response)?;
                }

                // Execute a single public or read-only contract call.
                "call" | "callPublicFn" | "callReadOnlyFn" => {
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

                    let inner =
                        call_contract(&mut session, &mut dap, &contract, &function, &args, sender);
                    let response = wrap_response(id, inner);
                    write_response(&mut writer, &response)?;
                }

                // Execute a block of transactions in order, returning one result per tx.
                "mineBlock" => {
                    let txs = request["txs"].as_array().cloned().unwrap_or_default();

                    let mut results: Vec<serde_json::Value> = Vec::with_capacity(txs.len());
                    for tx_val in &txs {
                        let tx_type = tx_val["type"].as_str().unwrap_or("");
                        let result = match tx_type {
                            "callPublicFn" | "callPrivateFn" => {
                                let contract =
                                    tx_val["contract"].as_str().unwrap_or("").to_string();
                                let function =
                                    tx_val["function"].as_str().unwrap_or("").to_string();
                                let sender = tx_val["sender"].as_str().map(|s| s.to_string());
                                let args: Vec<String> = tx_val["args"]
                                    .as_array()
                                    .map(|a| {
                                        a.iter()
                                            .filter_map(|v| v.as_str().map(|s| s.to_string()))
                                            .collect()
                                    })
                                    .unwrap_or_default();
                                call_contract(
                                    &mut session,
                                    &mut dap,
                                    &contract,
                                    &function,
                                    &args,
                                    sender,
                                )
                            }
                            "transferSTX" => {
                                let amount = tx_val["amount"].as_u64().unwrap_or(0);
                                let recipient =
                                    tx_val["recipient"].as_str().unwrap_or("").to_string();
                                let sender_str = tx_val["sender"].as_str().unwrap_or("");
                                if sender_str.is_empty() {
                                    serde_json::json!({"error": "transferSTX: missing sender"})
                                } else if PrincipalData::parse_standard_principal(sender_str)
                                    .is_err()
                                {
                                    serde_json::json!({"error": format!("transferSTX: invalid sender: {sender_str}")})
                                } else {
                                    let snippet =
                                        format!("(stx-transfer? u{amount} tx-sender '{recipient})");
                                    let orig = session.get_tx_sender();
                                    session.set_tx_sender(sender_str);
                                    let cid = QualifiedContractIdentifier::transient();
                                    dap.prepare_for_call(&cid, &snippet);
                                    let r = eval_snippet_as_tx(&mut session, &mut dap, snippet);
                                    session.set_tx_sender(&orig);
                                    r
                                }
                            }
                            _ => {
                                serde_json::json!({"error": format!("unsupported transaction type: {tx_type}")})
                            }
                        };
                        results.push(result);
                    }

                    // Advance the chain tip by one block, mirroring simnet.mineBlock semantics.
                    session.advance_chain_tip(1);

                    let response = serde_json::json!({"id": id, "result": {"results": results}});
                    write_response(&mut writer, &response)?;
                }

                _ => {
                    let response =
                        serde_json::json!({"id": id, "error": format!("unknown method: {method}")});
                    write_response(&mut writer, &response)?;
                }
            }
        } // end 'request loop

        eprintln!("clarinet dap: SDK client disconnected");
    } // end accept loop
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Resolve a short contract name or full principal to the snippet form `'principal`.
fn resolve_contract_principal(dap: &DAPDebugger, contract: &str) -> String {
    if contract.contains('.') && !contract.starts_with('.') {
        format!("'{contract}")
    } else {
        let short = contract.trim_start_matches('.');
        dap.contract_id_to_path
            .keys()
            .find(|id| id.name.as_str() == short)
            .map(|id| format!("'{id}"))
            .unwrap_or_else(|| format!(".{short}"))
    }
}

/// Look up the `QualifiedContractIdentifier` for a contract name or full principal.
/// Handles both short names (`counter`) and fully-qualified principals (`ST1….counter`).
fn find_contract_id(dap: &DAPDebugger, contract: &str) -> QualifiedContractIdentifier {
    if contract.contains('.') && !contract.starts_with('.') {
        // Try to find by parsing as a fully-qualified principal.
        if let Ok(qid) = QualifiedContractIdentifier::parse(contract) {
            if dap.contract_id_to_path.contains_key(&qid) {
                return qid;
            }
        }
    }
    let short = contract.trim_start_matches('.');
    dap.contract_id_to_path
        .keys()
        .find(|id| id.name.as_str() == short)
        .cloned()
        .unwrap_or_else(QualifiedContractIdentifier::transient)
}

/// Execute a contract-call? snippet and return an inner tx-result JSON object
/// (`{"result": "0x...", "events": "[]", "costs": "..."}` on success, or
/// `{"error": "..."}` on failure). The caller is responsible for wrapping with
/// `{"id": ..., "result": ...}` or `{"id": ..., "error": ...}`.
fn call_contract(
    session: &mut Session,
    dap: &mut DAPDebugger,
    contract: &str,
    function: &str,
    args: &[String],
    sender: Option<String>,
) -> serde_json::Value {
    let principal = resolve_contract_principal(dap, contract);
    let args_str = args.join(" ");
    let snippet = if args_str.is_empty() {
        format!("(contract-call? {principal} {function})")
    } else {
        format!("(contract-call? {principal} {function} {args_str})")
    };

    // Validate and set the sender before calling into the interpreter so a bad
    // address returns a JSON error rather than panicking via `expect`.
    let orig_sender = if let Some(ref s) = sender {
        if PrincipalData::parse_standard_principal(s).is_err() {
            return serde_json::json!({"error": format!("invalid sender address: {s}")});
        }
        let prev = session.get_tx_sender();
        session.set_tx_sender(s);
        Some(prev)
    } else {
        None
    };

    let contract_id = find_contract_id(dap, contract);
    dap.prepare_for_call(&contract_id, &snippet);
    let result = eval_snippet_as_tx(session, dap, snippet);

    if let Some(ref prev) = orig_sender {
        session.set_tx_sender(prev);
    }

    result
}

/// Run a Clarity snippet via `eval_with_hooks` and return an inner tx-result JSON object:
/// `{"result": "0x...", "events": "[]", "costs": "..."}` on success, or
/// `{"error": "..."}` on failure.
///
/// Callers must wrap the returned value with `wrap_response(id, inner)` before
/// sending to the SDK client.
fn eval_snippet_as_tx(
    session: &mut Session,
    dap: &mut DAPDebugger,
    snippet: String,
) -> serde_json::Value {
    match session.eval_with_hooks(snippet, Some(vec![dap]), false) {
        Ok(result) => {
            let hex = match &result.result {
                EvaluationResult::Contract(c) => c
                    .result
                    .as_ref()
                    .map(to_raw_value)
                    .unwrap_or_else(|| "0x03".to_string()),
                EvaluationResult::Snippet(s) => to_raw_value(&s.result),
            };
            let costs_json = serde_json::to_string(&result.cost).unwrap_or_else(|_| "null".into());
            serde_json::json!({"result": hex, "events": "[]", "costs": costs_json})
        }
        Err(diagnostics) => {
            let errors: Vec<&str> = diagnostics.iter().map(|d| d.message.as_str()).collect();
            let msg = errors.join("; ");
            serde_json::json!({"error": msg})
        }
    }
}

/// Wrap an inner result with `{"id": ..., "result": ...}` or `{"id": ..., "error": ...}`.
fn wrap_response(id: serde_json::Value, inner: serde_json::Value) -> serde_json::Value {
    if let Some(msg) = inner.get("error") {
        serde_json::json!({"id": id, "error": msg})
    } else {
        serde_json::json!({"id": id, "result": inner})
    }
}

fn write_response(writer: &mut impl Write, response: &serde_json::Value) -> Result<(), String> {
    let response_str =
        serde_json::to_string(response).map_err(|e| format!("serialize error: {e}"))?;
    writeln!(writer, "{response_str}").map_err(|e| format!("write error: {e}"))?;
    writer.flush().map_err(|e| format!("flush error: {e}"))?;
    Ok(())
}
