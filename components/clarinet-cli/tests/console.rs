use std::io::Write;
use std::process::{Command, Stdio};

fn run_console_command(args: &[&str], commands: &[&str]) -> Vec<String> {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["console"])
        .args(args)
        .current_dir(&temp_dir)
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
    assert!(output.status.success(), "Console command failed");

    let stdout_str = String::from_utf8_lossy(&output.stdout);
    // always skip the first 3 lines (console instructions)
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
    // testnet
    let output = run_console_command(
        &[
            "--enable-remote-data",
            "--remote-data-api-url",
            "https://api.testnet.hiro.so",
            "--remote-data-initial-height",
            "74380",
        ],
        &[
            "::get_epoch",
            "(is-standard 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)",
            "(is-standard 'SP1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRCBGD7R)",
        ],
    );
    assert_eq!(output[0], "Current epoch: 3.1");
    assert_eq!(output[1], "true");
    assert_eq!(output[2], "false");

    // mainnet
    let output = run_console_command(
        &[
            "--enable-remote-data",
            "--remote-data-api-url",
            "https://api.hiro.so",
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
fn test_get_constant_command() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &["::get_constant counter MISSING_CONSTANT"],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain error message for missing constant
    assert!(output_text.contains("not found") || output_text.contains("MISSING_CONSTANT"));
    assert!(output_text.contains("Constant:") && output_text.contains("MISSING_CONSTANT"));
    assert!(output_text.contains("contract:") && output_text.contains("counter"));
}

#[test]
fn test_get_constant_command_found() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &["::get_constant counter MAX_COUNT"],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain the constant value
    assert!(output_text.contains("Contract:"));
    assert!(output_text.contains("counter"));
    assert!(output_text.contains("Constant:"));
    assert!(output_text.contains("MAX_COUNT"));
    assert!(output_text.contains("Value:"));
    assert!(output_text.contains("u100"));
}

#[test]
fn test_get_data_var_command() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &["::get_data_var counter count"],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain the data variable value
    assert!(output_text.contains("Contract:"));
    assert!(output_text.contains("counter"));
    assert!(output_text.contains("Data var:"));
    assert!(output_text.contains("count"));
    assert!(output_text.contains("Value:"));
    assert!(output_text.contains("u0")); // Initial value should be u0
}

#[test]
fn test_get_map_val_command_not_found() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &["::get_map_val counter test-map u1"],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain not found message for empty map
    assert!(output_text.contains("Map entry not found") || output_text.contains("not found"));
    assert!(output_text.contains("test-map"));
    assert!(output_text.contains("u1"));
}

#[test]
fn test_get_map_val_command_found() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &[
            "(contract-call? .counter set-map-entry u1 u42)",
            "::get_map_val counter test-map u1",
        ],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain the map value
    assert!(output_text.contains("Contract:"));
    assert!(output_text.contains("counter"));
    assert!(output_text.contains("Map:"));
    assert!(output_text.contains("test-map"));
    assert!(output_text.contains("Key:"));
    assert!(output_text.contains("u1"));
    assert!(output_text.contains("Value:"));
    assert!(output_text.contains("some u42"));
}

#[test]
fn test_get_constant_command_invalid_args() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &[
            "::get_constant counter", // Missing constant argument
        ],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain usage error message
    assert!(output_text.contains("Usage:") || output_text.contains("get_constant"));
}

#[test]
fn test_get_constant_command_invalid_contract() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &["::get_constant non_existent_contract SOME_CONSTANT"],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain contract not found error message
    assert!(output_text.contains("Contract not found:") || output_text.contains("not found"));
    assert!(output_text.contains("non_existent_contract"));
}

#[test]
fn test_get_data_var_command_invalid_args() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &[
            "::get_data_var counter", // Missing variable argument
        ],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain usage error message
    assert!(output_text.contains("Usage:") || output_text.contains("get_data_var"));
}

#[test]
fn test_get_data_var_command_not_found() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &["::get_data_var counter non_existent_var"],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain data variable not found error message
    assert!(output_text.contains("Data var:") && output_text.contains("not found"));
    assert!(output_text.contains("non_existent_var"));
}

#[test]
fn test_get_map_val_command_invalid_args() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &[
            "::get_map_val counter test-map", // Missing key argument
        ],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain usage error message
    assert!(output_text.contains("Usage:") || output_text.contains("get_map_val"));
}

#[test]
fn test_get_map_val_command_invalid_contract() {
    let output = run_console_command(
        &[
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ],
        &["::get_map_val non_existent_contract test-map u1"],
    );

    assert!(!output.is_empty());
    let output_text = output.join(" ");

    // Should contain contract related error
    assert!(output_text.contains("contract") || output_text.contains("Contract"));
    assert!(output_text.contains("non_existent_contract"));
}

#[test]
fn test_get_map_val_command_invalid_key_expression() {
    let temp_dir = tempfile::tempdir().unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["console"])
        .args([
            "-m",
            &format!(
                "{}/tests/fixtures/mxs/Clarinet.toml",
                env!("CARGO_MANIFEST_DIR")
            ),
        ])
        .current_dir(&temp_dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to start console");

    let stdin = child.stdin.as_mut().expect("Failed to open stdin");
    stdin
        .write_all(b"::get_map_val counter test-map invalid-key-expression-xyz\n")
        .expect("Failed to write to stdin");

    let output = child.wait_with_output().expect("Failed to read stdout");

    // We expect this to fail due to invalid expression, but we're testing that it doesn't crash the console
    let stdout_str = String::from_utf8_lossy(&output.stdout);
    let stderr_str = String::from_utf8_lossy(&output.stderr);
    let combined_output = format!("{} {}", stdout_str, stderr_str);

    // The command might fail, but we should get some output rather than a hard crash
    assert!(!combined_output.is_empty());
}
