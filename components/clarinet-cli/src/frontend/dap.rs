use std::collections::HashMap;
use std::io::{BufRead, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use clarinet_deployments::setup_session_with_deployment;
use clarinet_files::{paths, ProjectManifest, StacksNetwork};
use clarity::util::hash::hex_bytes;
use clarity::vm::types::{PrincipalData, QualifiedContractIdentifier};
use clarity::vm::EvaluationResult;
use clarity_repl::repl::clarity_values::{to_raw_value, value_from_bytes_exact, value_to_string};
use clarity_repl::repl::debug::dap::DAPDebugger;
use clarity_repl::repl::Session;
use clarity_repl::utils::sdk_events::serialize_events;
use clarity_repl::utils::{serialize_event, Environment};
use serde::Deserialize;

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
                Environment::Simnet,
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

            // Begin execution of the expression in debug mode. Debugging is a
            // simnet session, so the expression gets the same boot rewrite the
            // console and the SDK apply — otherwise a mainnet-addressed PoX
            // call reaches the dead twin only while under the debugger.
            let expression = session.remap_user_snippet(expression);
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
            let abs = std::fs::canonicalize(location).unwrap_or_else(|_| location.clone());
            (contract_id.clone(), abs)
        })
        .collect();

    let session = setup_session_with_deployment(
        &project_manifest,
        &mut deployment,
        Some(&artifacts.asts),
        false,
        Environment::Simnet,
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
    // Only contract_maps is needed here to configure the outer dap; each SDK
    // client thread creates its own session and accounts via make_session().
    let (_, _, _, contract_maps) = make_session(&manifest_path)?;

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
    let dap = if let Some(thread) = dap_thread {
        match thread
            .join()
            .map_err(|_| "DAP handshake thread panicked".to_string())
        {
            Ok(result) => match result {
                Ok(d) => d,
                Err(e) if e.contains("Eof") => {
                    eprintln!("clarinet dap: DAP client disconnected before attach handshake; running in SDK-only mode");
                    let mut d = DAPDebugger::no_op();
                    for (contract_id, path) in &contract_maps {
                        d.path_to_contract_id
                            .insert(path.clone(), contract_id.clone());
                        d.contract_id_to_path
                            .insert(contract_id.clone(), path.clone());
                    }
                    d
                }
                Err(e) => {
                    return Err(e);
                }
            },
            Err(e) => {
                return Err(e);
            }
        }
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

    // Wrap in Arc<Mutex> so all SDK client threads share the same DAP state
    // (breakpoints, contract maps).
    let dap = Arc::new(Mutex::new(dap));

    // Accept SDK clients in a loop so successive Vitest workers (or a reconnect
    // after disconnect) can reuse the same server without restarting it.
    eprintln!("clarinet dap: listening for SDK client on 127.0.0.1:{sdk_port}");
    loop {
        let (sdk_stream, _) = sdk_listener
            .accept()
            .map_err(|e| format!("SDK accept error: {e}"))?;
        eprintln!("clarinet dap: SDK client connected");

        // Each SDK client gets its own session but all share the same DAP
        // debugger (breakpoints, contract maps) via Arc<Mutex<>>.
        let manifest_path_clone = manifest_path.clone();
        let dap_clone = Arc::clone(&dap);
        std::thread::spawn(move || match make_session(&manifest_path_clone) {
            Ok((mut session, mut accounts, _, _)) => {
                run_sdk_request_loop(
                    sdk_stream,
                    &manifest_path_clone,
                    &mut session,
                    &mut accounts,
                    &dap_clone,
                );
            }
            Err(e) => {
                eprintln!("clarinet dap: failed to create session: {e}");
            }
        });
    } // end accept loop
}

fn run_sdk_request_loop(
    sdk_stream: std::net::TcpStream,
    manifest_path: &PathBuf,
    session: &mut Session,
    accounts: &mut HashMap<String, String>,
    dap: &Arc<Mutex<DAPDebugger>>,
) {
    // Clone the stream so we can have independent reader/writer halves.
    let sdk_read_stream = match sdk_stream.try_clone() {
        Ok(s) => s,
        Err(e) => {
            eprintln!("clarinet dap: SDK stream clone error: {e}");
            return;
        }
    };
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
                // Resolve manifestPath against the client's cwd (if provided)
                // rather than the server's cwd.
                let cwd = request["cwd"].as_str().map(PathBuf::from);
                let resolved_path: PathBuf = if let Some(req) = request["manifestPath"].as_str() {
                    let req_path = PathBuf::from(req);
                    paths::try_parse_path(req, cwd.as_deref()).unwrap_or(req_path)
                } else {
                    manifest_path.clone()
                };
                // Reject requests for a different project so callers get a clear
                // error rather than silently using the wrong manifest.
                {
                    let req_canon = std::fs::canonicalize(&resolved_path)
                        .unwrap_or_else(|_| resolved_path.clone());
                    let srv_canon = std::fs::canonicalize(manifest_path)
                        .unwrap_or_else(|_| manifest_path.clone());
                    if req_canon != srv_canon {
                        let resp = serde_json::json!({
                            "id": id,
                            "error": format!(
                                "debug server uses '{}'; pass the same path to initSimnet()",
                                manifest_path.display()
                            )
                        });
                        let _ = writeln!(writer, "{}", serde_json::to_string(&resp).unwrap());
                        let _ = writer.flush();
                        continue;
                    }
                }
                match make_session(&resolved_path) {
                    Ok((new_session, new_accounts, _, new_maps)) => {
                        *session = new_session;
                        *accounts = new_accounts;
                        // Rebuild the contract maps in the shared debugger.
                        let mut dap_guard = dap.lock().expect("DAP mutex poisoned");
                        dap_guard.path_to_contract_id.clear();
                        dap_guard.contract_id_to_path.clear();
                        for (contract_id, path) in &new_maps {
                            dap_guard
                                .path_to_contract_id
                                .insert(path.clone(), contract_id.clone());
                            dap_guard
                                .contract_id_to_path
                                .insert(contract_id.clone(), path.clone());
                        }
                        drop(dap_guard);
                        let resp = serde_json::json!({"id": id, "result": {}});
                        let _ = writeln!(writer, "{}", serde_json::to_string(&resp).unwrap());
                        let _ = writer.flush();
                    }
                    Err(e) => {
                        let resp = serde_json::json!({"id": id, "error": e});
                        let _ = writeln!(writer, "{}", serde_json::to_string(&resp).unwrap());
                        let _ = writer.flush();
                    }
                }
            }

            // Return the accounts map (name → address).
            "getAccounts" => {
                let resp = serde_json::json!({"id": id, "result": {"accounts": accounts}});
                let _ = writeln!(writer, "{}", serde_json::to_string(&resp).unwrap());
                let _ = writer.flush();
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
                let _ = writeln!(writer, "{}", serde_json::to_string(&resp).unwrap());
                let _ = writer.flush();
            }

            // Return the current Stacks block height.
            "getBlockHeight" => {
                let height = session.interpreter.get_block_height();
                let resp = serde_json::json!({"id": id, "result": {"blockHeight": height}});
                let _ = writeln!(writer, "{}", serde_json::to_string(&resp).unwrap());
                let _ = writer.flush();
            }

            // Return the current burn (Bitcoin) block height.
            "getBurnBlockHeight" => {
                let height = session.interpreter.get_burn_block_height();
                let resp = serde_json::json!({"id": id, "result": {"burnBlockHeight": height}});
                let _ = writeln!(writer, "{}", serde_json::to_string(&resp).unwrap());
                let _ = writer.flush();
            }

            // Advance the chain tip by `count` blocks (default 1).
            "mineEmptyBlock" => {
                let count = request["count"].as_u64().unwrap_or(1) as u32;
                let new_height = session.advance_chain_tip(count);
                let resp = serde_json::json!({"id": id, "result": {"blockHeight": new_height}});
                let _ = writeln!(writer, "{}", serde_json::to_string(&resp).unwrap());
                let _ = writer.flush();
            }

            // Execute an arbitrary Clarity snippet under the debugger.
            "eval" | "execute" => {
                let snippet = request["snippet"].as_str().unwrap_or("").to_string();
                let contract_id = QualifiedContractIdentifier::transient();
                let mut dap_guard = dap.lock().expect("DAP mutex poisoned");
                dap_guard.prepare_for_call(&contract_id, &snippet);
                let inner = eval_snippet_as_tx(session, &mut dap_guard, snippet);
                drop(dap_guard);
                let response = wrap_response(id, inner);
                let _ = writeln!(writer, "{}", serde_json::to_string(&response).unwrap());
                let _ = writer.flush();
            }

            // Execute a single public contract call.
            "call" | "callPublicFn" => {
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

                let mut dap_guard = dap.lock().expect("DAP mutex poisoned");
                let inner =
                    call_contract(session, &mut dap_guard, &contract, &function, &args, sender);
                drop(dap_guard);
                let response = wrap_response(id, inner);
                let _ = writeln!(writer, "{}", serde_json::to_string(&response).unwrap());
                let _ = writer.flush();
            }

            // Execute a read-only contract call (no state mutation).
            "callReadOnlyFn" => {
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

                let mut dap_guard = dap.lock().expect("DAP mutex poisoned");
                let principal = resolve_contract_principal(&dap_guard, &contract);
                let args_str = args.join(" ");
                let snippet = if args_str.is_empty() {
                    format!("(contract-call? {principal} {function})")
                } else {
                    format!("(contract-call? {principal} {function} {args_str})")
                };

                // Validate and set the sender before calling into the interpreter.
                let orig_sender = if let Some(s) = &sender {
                    if PrincipalData::parse_standard_principal(s).is_err() {
                        let inner =
                            serde_json::json!({"error": format!("invalid sender address: {s}")});
                        let response = wrap_response(id, inner);
                        drop(dap_guard);
                        let _ = writeln!(writer, "{}", serde_json::to_string(&response).unwrap());
                        let _ = writer.flush();
                        continue 'request;
                    }
                    let prev = session.get_tx_sender();
                    session.set_tx_sender(s);
                    Some(prev)
                } else {
                    None
                };

                let contract_id = find_contract_id(&dap_guard, &contract);
                dap_guard.prepare_for_call(&contract_id, &snippet);
                let inner = eval_snippet_as_tx(session, &mut dap_guard, snippet);
                drop(dap_guard);

                if let Some(prev) = &orig_sender {
                    session.set_tx_sender(prev);
                }

                let response = wrap_response(id, inner);
                let _ = writeln!(writer, "{}", serde_json::to_string(&response).unwrap());
                let _ = writer.flush();
            }

            "broadcastTransaction" => {
                let raw = request["rawHex"].as_str().unwrap_or_default();
                let result = session.submit_transaction(raw);
                let response = serde_json::json!({"id": id, "result": result});
                let _ = writeln!(writer, "{}", serde_json::to_string(&response).unwrap());
                let _ = writer.flush();
            }

            // Execute a block of transactions in order, returning one result per tx.
            "mineBlock" => {
                let txs = request["txs"]
                    .as_array()
                    .map(Vec::as_slice)
                    .unwrap_or_default();

                let mut dap_guard = dap.lock().expect("DAP mutex poisoned");
                let mut results: Vec<serde_json::Value> = Vec::with_capacity(txs.len());
                session.advance_chain_tip(1);
                while let Some(tx) = session.next_mempool_transaction() {
                    if let stacks_codec::transaction::TransactionPayload::ContractCall(call) =
                        &tx.payload
                    {
                        dap_guard
                            .prepare_for_call(&call.to_clarity_contract_id(), &call.function_name);
                    }
                    results.push(raw_block_result(
                        session.execute_transaction_with_hooks(&tx, vec![&mut *dap_guard]),
                    ));
                }
                for tx in txs {
                    results.push(execute_block_entry(session, &mut dap_guard, tx));
                }

                drop(dap_guard);

                let response = serde_json::json!({"id": id, "result": {"results": results}});
                let _ = writeln!(writer, "{}", serde_json::to_string(&response).unwrap());
                let _ = writer.flush();
            }

            _ => {
                let response =
                    serde_json::json!({"id": id, "error": format!("unknown method: {method}")});
                let _ = writeln!(writer, "{}", serde_json::to_string(&response).unwrap());
                let _ = writer.flush();
            }
        }
    } // end 'request loop

    eprintln!("clarinet dap: SDK client disconnected");
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn raw_block_result(
    outcome: clarity_repl::repl::raw_transaction::RawTransactionOutcome,
) -> serde_json::Value {
    use clarity_repl::repl::raw_transaction::{
        RawTransactionMetadata, RawTransactionOutcome, RawTransactionResult,
    };
    use serde_json::json;
    let metadata = |m: &RawTransactionMetadata| json!({"txid": m.txid, "fee": m.fee.to_string(), "nonce": m.nonce.to_string()});
    match outcome {
        RawTransactionOutcome::Rejected {
            metadata: tx,
            error,
        } => {
            json!({"status": "rejected", "error": error, "transaction": tx.as_ref().map(metadata)})
        }
        RawTransactionOutcome::Included {
            metadata: tx,
            result,
        } => match result {
            RawTransactionResult::Success(execution) => {
                let value = match &execution.result {
                    EvaluationResult::Snippet(result) => &result.result,
                    EvaluationResult::Contract(result) => {
                        result.result.as_ref().expect("raw deployment result")
                    }
                };
                json!({"status": "success", "transaction": metadata(&tx), "execution": {
                    "result": to_raw_value(value), "events": serialize_events(&execution.events),
                    "costs": serde_json::to_string(&execution.cost).unwrap(), "performance": null,
                }})
            }
            RawTransactionResult::Failure {
                kind,
                error,
                output,
                events,
                cost,
            } => {
                let kind: &'static str = kind.into();
                json!({"status": "included_failure", "kind": kind, "error": error, "execution": null,
                    "transaction": metadata(&tx), "result": output.as_deref().map(to_raw_value),
                    "events": serialize_events(&events), "costs": serde_json::to_string(&cost).unwrap(), "performance": null})
            }
        },
    }
}

/// The SDK's `ContractOptions`, so both batch paths reject the same malformed options.
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct DeployOptions {
    clarity_version: Option<u32>,
}

/// Batch entries use transaction methods so VM inclusion owns nonce settlement.
fn execute_block_entry(
    session: &mut Session,
    dap: &mut DAPDebugger,
    tx: &serde_json::Value,
) -> serde_json::Value {
    if tx["type"].as_str() == Some("broadcastTx") {
        return raw_block_result(session.execute_raw_transaction_with_hooks(
            tx["rawHex"].as_str().unwrap_or_default(),
            vec![dap],
        ));
    }
    use clarity::vm::analysis::contract_interface_builder::ContractInterfaceFunctionAccess;
    use clarity::vm::types::Value;
    use clarity::vm::{ClarityVersion, ExecutionResult, SymbolicExpression};
    use clarity_repl::repl::interpreter::BlockInclusion;
    use clarity_repl::repl::post_conditions::{
        parse_post_condition_mode, PostConditionCheck, DEFAULT_POST_CONDITION_MODE,
    };
    use clarity_repl::repl::session::CallKind;
    use clarity_repl::repl::{ClarityCodeSource, ClarityContract, ContractDeployer, Epoch};

    let mut execute = || -> Result<ExecutionResult, (BlockInclusion, String)> {
        let reject = |message: String| (BlockInclusion::Rejected, message);
        let string_field = |field: &str| {
            tx[field]
                .as_str()
                .ok_or_else(|| reject(format!("Missing or invalid {field}")))
        };
        let nonempty_field = |field: &str| {
            let value = string_field(field)?;
            if value.is_empty() {
                return Err(reject(format!("Empty {field}")));
            }
            Ok(value)
        };
        let sender = nonempty_field("sender")?;
        let principal = PrincipalData::parse_standard_principal(sender)
            .map_err(|e| reject(format!("Invalid sender: {e}")))?;
        let conditions = if tx["postConditions"].is_null() && tx["postConditionMode"].is_null() {
            PostConditionCheck::Unchecked
        } else {
            let mode = tx["postConditionMode"]
                .as_str()
                .map(parse_post_condition_mode)
                .transpose()
                .map_err(reject)?
                .unwrap_or(DEFAULT_POST_CONDITION_MODE);
            let hex: Vec<String> = if tx["postConditions"].is_null() {
                vec![]
            } else {
                Vec::deserialize(&tx["postConditions"]).map_err(|e| reject(e.to_string()))?
            };
            PostConditionCheck::from_hex(&hex, mode, principal.clone().into()).map_err(reject)?
        };
        let epoch = session.interpreter.datastore.get_current_epoch();
        conditions.validate_for_epoch(epoch).map_err(reject)?;
        let failure = |error: clarity_repl::repl::interpreter::ExecutionError| {
            (
                error.inclusion,
                error
                    .diagnostics
                    .iter()
                    .map(|d| d.message.to_string())
                    .collect::<Vec<_>>()
                    .join("; "),
            )
        };
        match nonempty_field("type")? {
            operation @ ("callPublicFn" | "callPrivateFn") => {
                let contract = nonempty_field("contract")?;
                let method = nonempty_field("function")?;
                let contract_id = session
                    .resolve_contract_id(&session.get_tx_sender(), contract)
                    .map_err(reject)?;
                let args = tx["args"]
                    .as_array()
                    .ok_or_else(|| reject("Missing or invalid arguments".into()))?
                    .iter()
                    .map(|arg| {
                        let hex = arg.as_str().ok_or_else(|| {
                            reject("Invalid argument: expected hex string".into())
                        })?;
                        let bytes = hex_bytes(hex.strip_prefix("0x").unwrap_or(hex))
                            .map_err(|e| reject(format!("Invalid argument: {e}")))?;
                        value_from_bytes_exact(&bytes)
                            .map(SymbolicExpression::atom_value)
                            .map_err(|e| reject(format!("Invalid Clarity argument: {e}")))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let required_access = if operation == "callPrivateFn" {
                    ContractInterfaceFunctionAccess::private
                } else {
                    ContractInterfaceFunctionAccess::public
                };
                let wrong_access = session
                    .contracts
                    .get(&contract_id)
                    .and_then(|contract| contract.analysis.contract_interface.as_ref())
                    .and_then(|interface| interface.functions.iter().find(|f| f.name == method))
                    .is_some_and(|function| {
                        function.access != required_access
                            && !(required_access == ContractInterfaceFunctionAccess::public
                                && function.access == ContractInterfaceFunctionAccess::read_only)
                    });
                if wrong_access {
                    let inclusion =
                        BlockInclusion::from_uncallable_function(contract, method, epoch);
                    if inclusion.is_included() {
                        session
                            .charge_nonce_without_execution(principal.into())
                            .map_err(reject)?;
                    }
                    return Err((
                        inclusion,
                        format!("{method} is not a {required_access:?} function"),
                    ));
                }
                dap.prepare_for_call(&contract_id, method);
                session
                    .call_contract_fn_with_hooks(
                        contract,
                        method,
                        &args,
                        sender,
                        operation == "callPrivateFn",
                        false,
                        CallKind::Transaction,
                        conditions,
                        Some(vec![dap]),
                    )
                    .map_err(failure)
            }
            "transferSTX" => {
                let amount = tx["amount"]
                    .as_u64()
                    .ok_or_else(|| reject("Invalid transfer amount".into()))?;
                let recipient = nonempty_field("recipient")?;
                let previous = session.get_tx_sender();
                session.set_tx_sender(sender);
                let result = session.stx_transfer(amount, recipient, conditions);
                session.set_tx_sender(&previous);
                result.map(|r| r.execution_result).map_err(|diagnostics| {
                    reject(
                        diagnostics
                            .iter()
                            .map(|d| d.message.to_string())
                            .collect::<Vec<_>>()
                            .join("; "),
                    )
                })
            }
            "deployContract" => {
                let name = nonempty_field("name")?;
                let content = string_field("content")?;
                let contract_id =
                    QualifiedContractIdentifier::parse(&format!("{sender}.{name}"))
                        .map_err(|error| reject(format!("Invalid contract identifier: {error}")))?;
                let options = Option::<DeployOptions>::deserialize(&tx["options"])
                    .map_err(|e| reject(format!("Invalid deployment options: {e}")))?;
                let clarity_version = match options.and_then(|o| o.clarity_version) {
                    Some(v) => clarity_repl::repl::clarity_version_from_u8(v as u8)
                        .unwrap_or(clarinet_defaults::DEFAULT_CLARITY_VERSION),
                    None => ClarityVersion::default_for_epoch(epoch),
                };
                dap.prepare_for_call(&contract_id, content);
                let contract = ClarityContract {
                    name: name.into(),
                    code_source: ClarityCodeSource::ContractInMemory(
                        session.remap_user_snippet(content.into()),
                    ),
                    deployer: ContractDeployer::Address(sender.into()),
                    clarity_version,
                    epoch: Epoch::Specific(epoch),
                    skip_analysis: false,
                };
                let conditions = if session.interpreter.is_mainnet() {
                    conditions
                } else {
                    conditions.remap_mainnet_boot_principals()
                };
                session
                    .deploy_contract_with_inclusion(
                        &contract,
                        false,
                        None,
                        conditions,
                        Some(vec![dap]),
                    )
                    .map(|r| r.execution_result)
                    .map_err(failure)
            }
            operation => Err(reject(format!("Unsupported transaction type: {operation}"))),
        }
    };
    match execute() {
        Ok(execution) => {
            let value = match &execution.result {
                EvaluationResult::Snippet(snippet) => Some(&snippet.result),
                EvaluationResult::Contract(contract) => contract.result.as_ref(),
            };
            let failed = matches!(value, Some(Value::Response(response)) if !response.committed);
            let result = serde_json::json!({
                "result": value.map(to_raw_value).unwrap_or_else(|| "0x03".into()),
                "events": serialize_events(&execution.events),
                "costs": serde_json::to_string(&execution.cost).unwrap(),
            });
            if failed {
                serde_json::json!({"status": "included_failure", "error": value_to_string(value.unwrap()), "execution": result})
            } else {
                serde_json::json!({"status": "success", "execution": result})
            }
        }
        Err((BlockInclusion::Included, error)) => {
            serde_json::json!({"status": "included_failure", "error": error, "execution": null})
        }
        Err((BlockInclusion::Rejected, error)) => {
            serde_json::json!({"status": "rejected", "error": error})
        }
    }
}

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
/// `{"result": "0x...", "events": "[...]", "costs": "..."}` on success, or
/// `{"error": "..."}` on failure.
///
/// Callers must wrap the returned value with `wrap_response(id, inner)` before
/// sending to the SDK client.
fn eval_snippet_as_tx(
    session: &mut Session,
    dap: &mut DAPDebugger,
    snippet: String,
) -> serde_json::Value {
    // The single funnel for every DAP evaluate/call request, so the rewrite
    // belongs here rather than at each caller.
    let snippet = session.remap_user_snippet(snippet);
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
            let events_json = serde_json::to_string(
                &result
                    .events
                    .iter()
                    .map(serialize_event)
                    .collect::<Vec<_>>(),
            )
            .unwrap_or_else(|_| "[]".into());
            serde_json::json!({"result": hex, "events": events_json, "costs": costs_json})
        }
        Err(diagnostics) => {
            let errors: Vec<&str> = diagnostics.iter().map(|d| &*d.message).collect();
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

#[cfg(test)]
mod batch_tests {
    use clarity::types::StacksEpochId;
    use clarity::vm::types::Value;
    use clarity_repl::repl::SessionSettings;
    use serde_json::json;

    use super::*;

    #[test]
    fn debug_batch_preserves_signed_transaction_metadata() {
        let fixtures: serde_json::Value = serde_json::from_str(include_str!(
            "../../../clarinet-sdk/node/tests/fixtures/raw-transactions/transactions.json"
        ))
        .unwrap();
        let mut session = Session::new(SessionSettings {
            epoch_id: Some(StacksEpochId::Epoch34),
            ..SessionSettings::default()
        });
        let sender = PrincipalData::parse(fixtures["sender"].as_str().unwrap()).unwrap();
        session
            .interpreter
            .mint_stx_balance(sender.clone(), 1_000_000)
            .unwrap();
        let mut dap = DAPDebugger::no_op();
        let entry =
            json!({ "type": "broadcastTx", "rawHex": fixtures["transactions"]["transfer"]["hex"] });
        let result = execute_block_entry(&mut session, &mut dap, &entry);
        assert_eq!(result["status"], "success");
        assert_eq!(
            result["transaction"],
            json!({
                "txid": fixtures["transactions"]["transfer"]["txid"], "fee": "10", "nonce": "0",
            })
        );
        assert_eq!(session.get_nonce(&sender).unwrap(), 1);
        assert_eq!(
            execute_block_entry(&mut session, &mut dap, &entry)["status"],
            "rejected"
        );
    }

    #[test]
    fn debug_batch_executes_read_only_targets_as_transactions() {
        let mut session = Session::new(SessionSettings::default());
        session.update_epoch(clarinet_defaults::DEFAULT_EPOCH);
        let sender = session.get_tx_sender();
        let principal = PrincipalData::parse(&sender).unwrap();
        let mut dap = DAPDebugger::no_op();
        let deployment = json!({
            "type": "deployContract", "name": "reads", "sender": sender,
            "content": indoc::indoc! {"
                (define-data-var count uint u0)
                (define-public (increment) (ok (var-set count (+ (var-get count) u1))))
                (define-read-only (peek) (var-get count))
                (define-read-only (fail) (err u7))
                (define-read-only (panic) (/ u1 u0))
                (define-private (hidden) u99)
            "},
        });
        assert_eq!(
            execute_block_entry(&mut session, &mut dap, &deployment)["status"],
            "success"
        );
        let nonce = session.get_nonce(&principal).unwrap();
        session.advance_chain_tip(1);
        let height = session.interpreter.get_block_height();
        let results: Vec<_> = ["peek", "increment", "peek", "fail", "panic", "hidden", "peek"]
            .into_iter()
            .map(|method| {
                execute_block_entry(
                    &mut session,
                    &mut dap,
                    &json!({"type": "callPublicFn", "contract": "reads", "function": method, "args": [], "sender": sender}),
                )
            })
            .collect();
        for (result, expected) in results.iter().zip([
            "success",
            "success",
            "success",
            "included_failure",
            "included_failure",
            "included_failure",
            "success",
        ]) {
            assert_eq!(result["status"], expected, "{result}");
        }
        for (index, value) in [
            (0, Value::UInt(0)),
            (2, Value::UInt(1)),
            (3, Value::err_uint(7)),
            (6, Value::UInt(1)),
        ] {
            assert_eq!(results[index]["execution"]["result"], to_raw_value(&value));
            assert_eq!(results[index]["execution"]["events"], "[]");
        }
        assert_eq!(results[4]["execution"], serde_json::Value::Null);
        assert!(results[4]["error"]
            .as_str()
            .unwrap()
            .contains("DivisionByZero"));
        assert_eq!(results[5]["execution"], serde_json::Value::Null);
        assert_eq!(results[5]["error"], "hidden is not a public function");
        assert_eq!(session.get_nonce(&principal).unwrap(), nonce + 7);
        assert_eq!(session.interpreter.get_block_height(), height);
    }

    #[test]
    fn debug_batch_rejects_malformed_entries_without_charging_nonces() {
        let mut session = Session::new(SessionSettings::default());
        session.update_epoch(clarinet_defaults::DEFAULT_EPOCH);
        let sender = session.get_tx_sender();
        let principal = PrincipalData::parse(&sender).unwrap();
        let recipient = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        session
            .interpreter
            .mint_stx_balance(principal.clone(), 100)
            .unwrap();
        let mut dap = DAPDebugger::no_op();
        let deployment = json!({
            "type": "deployContract", "name": "batch", "sender": sender,
            "content": "(define-data-var count uint u0) (define-public (increment) (ok (var-set count (+ (var-get count) u1)))) (define-private (hidden) u1)",
        });
        assert_eq!(
            execute_block_entry(&mut session, &mut dap, &deployment)["status"],
            "success"
        );
        let call = json!({
            "type": "callPublicFn", "contract": "batch", "function": "increment",
            "args": [], "sender": sender,
        });
        let nonce = session.get_nonce(&principal).unwrap();
        let contracts = session.contracts.len();
        let fixtures = [
            (
                call.clone(),
                vec!["type", "sender", "contract", "function", "args"],
            ),
            (
                json!({"type": "callPrivateFn", "contract": "batch", "function": "hidden", "args": [], "sender": sender}),
                vec!["contract", "function", "args"],
            ),
            (
                json!({"type": "deployContract", "name": "new", "content": "true", "sender": sender}),
                vec!["name", "content"],
            ),
            (
                json!({"type": "transferSTX", "amount": 1, "recipient": recipient, "sender": sender}),
                vec!["amount", "recipient"],
            ),
        ];
        let mut malformed = vec![];
        for (entry, fields) in fixtures {
            for field in fields {
                let mut missing = entry.clone();
                missing.as_object_mut().unwrap().remove(field);
                malformed.push(missing);
                for value in [serde_json::Value::Null, json!(false), json!("")] {
                    // An explicitly supplied empty source is a valid contract.
                    if field == "content" && value == json!("") {
                        continue;
                    }
                    let mut invalid = entry.clone();
                    invalid[field] = value;
                    malformed.push(invalid);
                }
            }
        }
        for contract in ["é", "é.bad"] {
            let mut invalid = call.clone();
            invalid["contract"] = json!(contract);
            malformed.push(invalid);
        }
        // Argument validation must precede the access check that charges a nonce.
        let leftover = format!("{}ff", to_raw_value(&Value::UInt(1)));
        for args in [
            serde_json::Value::Null,
            json!([false]),
            json!(["zz"]),
            json!([leftover]),
        ] {
            let mut invalid = call.clone();
            invalid["function"] = json!("hidden");
            invalid["args"] = args;
            malformed.push(invalid);
        }
        let deploy =
            json!({"type": "deployContract", "name": "new", "content": "true", "sender": sender});
        for options in [
            json!(false),
            json!({"clarityVersion": "3"}),
            json!({"clarityVersion": -1}),
        ] {
            let mut invalid = deploy.clone();
            invalid["options"] = options;
            malformed.push(invalid);
        }

        session.advance_chain_tip(1);
        for entry in malformed {
            let outcome = execute_block_entry(&mut session, &mut dap, &entry);
            assert_eq!(outcome["status"], "rejected", "{entry}: {outcome}");
            assert_eq!(session.get_nonce(&principal).unwrap(), nonce);
            assert_eq!(session.contracts.len(), contracts);
            assert_eq!(
                session.interpreter.get_balance_for_account(&sender, "STX"),
                100
            );
        }
        assert_eq!(
            execute_block_entry(&mut session, &mut dap, &call)["status"],
            "success"
        );
        let id = session.resolve_contract_id(&sender, "batch").unwrap();
        assert_eq!(
            session.interpreter.get_data_var(&id, "count"),
            Some(to_raw_value(&Value::UInt(1)))
        );
        assert_eq!(session.get_nonce(&principal).unwrap(), nonce + 1);
        let empty =
            json!({"type": "deployContract", "name": "empty", "content": "", "sender": sender});
        assert_eq!(
            execute_block_entry(&mut session, &mut dap, &empty)["status"],
            "success"
        );
        for (name, options) in [
            ("null-options", serde_json::Value::Null),
            ("empty-options", json!({})),
            ("versioned", json!({"clarityVersion": 2})),
        ] {
            let mut valid = deploy.clone();
            valid["name"] = json!(name);
            valid["options"] = options;
            assert_eq!(
                execute_block_entry(&mut session, &mut dap, &valid)["status"],
                "success",
                "{valid}"
            );
        }
        let id = session.resolve_contract_id(&sender, "versioned").unwrap();
        assert_eq!(
            session.contracts[&id].analysis.clarity_version,
            clarity::vm::ClarityVersion::Clarity2
        );
    }

    #[test]
    fn debug_batch_entries_settle_nonces_roll_back_failures_and_preserve_caches() {
        let mut session = Session::new(SessionSettings::default());
        session.update_epoch(clarinet_defaults::DEFAULT_EPOCH);
        let sender = session.get_tx_sender();
        let principal = PrincipalData::parse(&sender).unwrap();
        let recipient = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        session
            .interpreter
            .mint_stx_balance(principal.clone(), 100)
            .unwrap();
        let mut dap = DAPDebugger::no_op();
        let source = "(define-data-var count uint u0) (define-public (increment) (ok (var-set count (+ (var-get count) u1)))) (define-public (fail) (begin (var-set count u99) (err u7))) (define-public (panic) (begin (var-set count u99) (ok (/ u1 u0)))) (define-private (private-value) u42)";
        let txs = [
            json!({"type": "deployContract", "name": "batch", "content": source, "sender": sender}),
            json!({"type": "callPublicFn", "contract": "batch", "function": "increment", "args": [], "sender": sender}),
            json!({"type": "transferSTX", "amount": 0, "recipient": recipient, "sender": sender}),
            json!({"type": "callPublicFn", "contract": "batch", "function": "fail", "args": [], "sender": sender}),
            json!({"type": "callPublicFn", "contract": "batch", "function": "panic", "args": [], "sender": sender}),
            json!({"type": "deployContract", "name": "batch", "content": "(+ u1 true)", "sender": sender}),
            json!({"type": "callPrivateFn", "contract": "batch", "function": "private-value", "args": [], "sender": sender}),
            json!({"type": "callPublicFn", "contract": "batch", "function": "private-value", "args": [], "sender": sender}),
            json!({"type": "transferSTX", "amount": 1, "recipient": recipient, "sender": sender}),
        ];
        session.advance_chain_tip(1);
        let results: Vec<_> = txs
            .iter()
            .map(|tx| execute_block_entry(&mut session, &mut dap, tx))
            .collect();
        let statuses: Vec<_> = results
            .iter()
            .map(|r| r["status"].as_str().unwrap())
            .collect();
        assert_eq!(
            statuses,
            [
                "success",
                "success",
                "rejected",
                "included_failure",
                "included_failure",
                "rejected",
                "success",
                "included_failure",
                "success"
            ]
        );
        assert_eq!(
            results[3]["execution"]["result"],
            to_raw_value(&Value::err_uint(7))
        );
        assert_eq!(results[4]["execution"], serde_json::Value::Null);
        assert_eq!(
            results[6]["execution"]["result"],
            to_raw_value(&Value::UInt(42))
        );
        assert_eq!(session.get_nonce(&principal).unwrap(), 7);
        assert_eq!(
            session.interpreter.get_balance_for_account(&sender, "STX"),
            99
        );
        let id = session.resolve_contract_id(&sender, "batch").unwrap();
        assert_eq!(
            session.interpreter.get_data_var(&id, "count").unwrap(),
            to_raw_value(&Value::UInt(1))
        );
        assert!(session.contracts.contains_key(&id));
    }

    #[test]
    fn debug_batch_events_use_sdk_encoding_and_preserve_clarity_values() {
        let mut session = Session::new(SessionSettings::default());
        session.update_epoch(clarinet_defaults::DEFAULT_EPOCH);
        let sender = session.get_tx_sender();
        let recipient = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
        session
            .interpreter
            .mint_stx_balance(PrincipalData::parse(&sender).unwrap(), 100)
            .unwrap();
        let mut dap = DAPDebugger::no_op();
        let transfer = execute_block_entry(
            &mut session,
            &mut dap,
            &json!({
                "type": "transferSTX", "amount": 1, "recipient": recipient, "sender": sender,
            }),
        );
        let deployment = execute_block_entry(
            &mut session,
            &mut dap,
            &json!({
                "type": "deployContract", "name": "events", "sender": sender,
                "content": "(define-non-fungible-token badge uint) (print u42) (nft-mint? badge u7 tx-sender)",
            }),
        );
        let decode_events = |outcome: &serde_json::Value| {
            assert_eq!(outcome["status"], "success");
            let events: Vec<String> =
                serde_json::from_str(outcome["execution"]["events"].as_str().unwrap()).unwrap();
            events
                .iter()
                .map(|event| serde_json::from_str::<serde_json::Value>(event).unwrap())
                .collect::<Vec<_>>()
        };
        let events = decode_events(&transfer);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0]["event"], "stx_transfer_event");
        assert_eq!(events[0]["data"]["sender"], sender);
        assert_eq!(events[0]["data"]["recipient"], recipient);
        assert_eq!(events[0]["data"]["amount"], "1");
        let events = decode_events(&deployment);
        assert_eq!(events.len(), 2);
        assert_eq!(events[0]["event"], "print_event");
        assert_eq!(
            events[0]["data"]["raw_value"],
            to_raw_value(&Value::UInt(42))
        );
        assert_eq!(events[1]["event"], "nft_mint_event");
        assert_eq!(
            events[1]["data"]["raw_value"],
            to_raw_value(&Value::UInt(7))
        );
        assert_eq!(events[1]["data"]["recipient"], sender);
    }

    #[test]
    fn debug_batch_missing_calls_obey_epoch_inclusion() {
        for (epoch, status, nonce) in [
            (StacksEpochId::Epoch2_05, "rejected", 0),
            (StacksEpochId::Epoch21, "included_failure", 1),
        ] {
            let mut session = Session::new(SessionSettings::default());
            session.update_epoch(epoch);
            let sender = session.get_tx_sender();
            let mut dap = DAPDebugger::no_op();
            let result = execute_block_entry(
                &mut session,
                &mut dap,
                &json!({
                    "type": "callPublicFn", "contract": "missing", "function": "missing", "args": [], "sender": sender,
                }),
            );
            assert_eq!(result["status"], status);
            assert_eq!(
                session
                    .get_nonce(&PrincipalData::parse(&sender).unwrap())
                    .unwrap(),
                nonce
            );
        }
    }
}
