use std::io::Write;
use std::process::{Command, Stdio};

fn mock_remote_node(network_id: u32, height: u32) -> mockito::ServerGuard {
    let mut server = mockito::Server::new();

    server
        .mock("GET", mockito::Matcher::Any)
        .with_status(404)
        .expect_at_least(0)
        .create();
    server
        .mock("GET", "/v2/info")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(format!(
            r#"{{"network_id": {network_id}, "stacks_tip_height": {height}}}"#
        ))
        .create();
    server
        .mock("GET", format!("/extended/v2/blocks/{height}").as_str())
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(
            serde_json::json!({
                "height": height,
                "burn_block_height": 100,
                "tenure_height": 1,
                "block_time": 1,
                "burn_block_time": 1,
                "hash": format!("0x{}", "11".repeat(32)),
                "index_block_hash": format!("0x{}", "22".repeat(32)),
                "burn_block_hash": format!("0x{}", "33".repeat(32)),
            })
            .to_string(),
        )
        .create();

    server
}

fn run_console_command(args: &[&str], commands: &[&str]) -> Vec<String> {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["console"])
        .args(args)
        .current_dir(&temp_dir)
        // Rustyline prints prompts when TERM identifies an unsupported terminal.
        .env("TERM", "xterm-256color")
        .env("CLICOLOR_FORCE", "1")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to start console");

    let stdin = child.stdin.as_mut().expect("Failed to open stdin");
    for command in commands {
        stdin
            .write_all(command.as_bytes())
            .expect("Failed to write to stdin");
        stdin.write_all(b"\n").expect("Failed to write newline");
    }

    let output = child.wait_with_output().expect("Failed to read stdout");
    assert!(
        output.status.success(),
        "Console command failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout_str = String::from_utf8_lossy(&output.stdout);
    // Skip the three console instruction lines.
    stdout_str.lines().skip(3).map(|s| s.to_string()).collect()
}

#[test]
fn can_set_epoch_in_empty_session() {
    let output = run_console_command(&[], &["::get_epoch", "::set_epoch 3.1", "::get_epoch"]);
    assert_eq!(output[0], "Current epoch: 2.05");
    assert_eq!(output[1], "Epoch updated to: 3.1");
    assert_eq!(output[2], "Current epoch: 3.1");
}

#[test]
fn can_init_console_with_mxs() {
    let testnet = mock_remote_node(0x8000_0000, 50_000);
    let testnet_url = testnet.url();
    // Height 50,000 is in epoch 4.0 on the Krypton testnet.
    let output = run_console_command(
        &[
            "--enable-remote-data",
            "--remote-data-api-url",
            &testnet_url,
            "--remote-data-initial-height",
            "50000",
        ],
        &[
            "::get_epoch",
            "(is-standard 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)",
            "(is-standard 'SP1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRCBGD7R)",
        ],
    );
    assert_eq!(output[0], "Current epoch: 4.0");
    assert_eq!(output[1], "true");
    assert_eq!(output[2], "false");

    // Mainnet.
    let mainnet = mock_remote_node(1, 907_820);
    let mainnet_url = mainnet.url();
    let output = run_console_command(
        &[
            "--enable-remote-data",
            "--remote-data-api-url",
            &mainnet_url,
            "--remote-data-initial-height",
            "907820",
        ],
        &[
            "::get_epoch",
            "(is-standard 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)",
            "(is-standard 'SP1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRCBGD7R)",
        ],
    );
    assert_eq!(output[0], "Current epoch: 3.1");
    assert_eq!(output[1], "false");
    assert_eq!(output[2], "true");
}

#[test]
fn eval_is_quiet_and_stops_on_the_first_error() {
    let temp_dir = tempfile::tempdir().unwrap();
    for (args, stdout, success, error) in [
        (vec!["-e", "(+ 1 1)"], "2\n", true, ""),
        (
            vec!["--eval", "(+ 1 1)", "-e", "(+ 1 u1)", "-e", "(+ 2 2)"],
            "2\n",
            false,
            "error:",
        ),
        (
            vec!["-e", "(contract-call? .counter get-count)", "-e", "(+ 2 2)"],
            "",
            false,
            "error:",
        ),
    ] {
        let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
            .arg("console")
            .args(args)
            .env("CLICOLOR_FORCE", "1")
            .current_dir(&temp_dir)
            .output()
            .unwrap();
        assert_eq!(String::from_utf8_lossy(&output.stdout), stdout);
        assert_eq!(output.status.success(), success);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains(error), "{stderr}");
        assert!(!stderr.contains('\u{1b}'));
    }
}

#[test]
fn eval_uses_one_project_session_and_routes_startup_diagnostics() {
    let temp_dir = tempfile::tempdir().unwrap();
    let clarinet = env!("CARGO_BIN_EXE_clarinet");
    let output = Command::new(clarinet)
        .args(["new", "counter-project", "--disable-telemetry"])
        .current_dir(&temp_dir)
        .output()
        .unwrap();
    assert!(output.status.success());
    let project = temp_dir.path().join("counter-project");
    assert!(Command::new(clarinet)
        .args(["contract", "new", "counter"])
        .current_dir(&project)
        .output()
        .unwrap()
        .status
        .success());
    let manifest = project.join("Clarinet.toml");
    let mut manifest_value: toml::Value =
        toml::from_str(&std::fs::read_to_string(&manifest).unwrap()).unwrap();
    manifest_value["contracts"]["counter"]
        .as_table_mut()
        .unwrap()
        .insert("deployer".into(), "wallet_1".into());
    std::fs::write(&manifest, toml::to_string(&manifest_value).unwrap()).unwrap();
    let contract = project.join("contracts/counter.clar");
    std::fs::write(
        &contract,
        ";; #[unknown]\n(define-data-var count uint u0)\n\
         (define-public (increment) (begin (var-set count (+ (var-get count) u1)) (ok true)))\n\
         (define-read-only (get-count) (var-get count))",
    )
    .unwrap();
    let output = Command::new(clarinet)
        .args([
            "console",
            "-e",
            "::set_tx_sender ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5",
            "-e",
            "(contract-call? .counter increment)",
            "-e",
            "(contract-call? .counter get-count)",
            "-e",
            "(print (+ 1 1))",
            "-e",
            "::advance_chain_tip 1",
            "-e",
            "::get_contracts",
            "-e",
            "::get_assets_maps",
        ])
        .current_dir(&project)
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(stdout.starts_with("tx-sender switched to ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5\n(ok true)\nu1\n2\nnew burn height:"), "{stdout}");
    assert!(stdout.contains("Contract identifier"));
    assert!(stdout.contains("Address"));
    assert!(!stdout.contains("Events emitted"));
    assert!(!stdout.contains("CTRL-D"));
    assert!(!stdout.contains('\u{1b}'));

    assert!(String::from_utf8_lossy(&output.stderr).contains("warning:"));

    assert!(Command::new(clarinet)
        .args(["deployments", "generate", "--simnet"])
        .current_dir(&project)
        .output()
        .unwrap()
        .status
        .success());
    assert!(Command::new(clarinet)
        .args(["contract", "new", "other"])
        .current_dir(&project)
        .output()
        .unwrap()
        .status
        .success());
    let output = Command::new(clarinet)
        .args(["console", "-e", "(+ 1 1)"])
        .current_dir(&project)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(output.stdout, b"2\n");

    // Startup errors must prevent evaluation.
    std::fs::write(&contract, "(define-read-only (get-count) missing)").unwrap();
    let output = Command::new(clarinet)
        .args(["console", "-e", "(+ 1 1)"])
        .current_dir(&project)
        .output()
        .unwrap();
    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("error:"));
}

#[test]
fn eval_command_errors_stop_evaluation() {
    let temp_dir = tempfile::tempdir().unwrap();
    for command in [
        "::set_tx_sender invalid",
        "::advance_chain_tip invalid",
        "::set_epoch invalid",
        "::unknown",
        "::read missing.clar",
        "::encode (+ 1 u1)",
        "::decode invalid",
        "::trace (+ 1 u1)",
        "::perf runtime (+ 1 u1)",
        "::get_costs (+ 1 u1)",
        "::debug (+ 1 1)",
    ] {
        let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
            .args(["console", "-e", command, "-e", "(+ 2 2)"])
            .current_dir(&temp_dir)
            .output()
            .unwrap();
        assert!(!output.status.success(), "{command}");
        assert!(output.stdout.is_empty(), "{command}");
        assert!(!output.stderr.is_empty(), "{command}");
    }
}
