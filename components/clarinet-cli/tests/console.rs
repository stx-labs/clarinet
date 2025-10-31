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
            "https://api.testnet.stg.hiro.so",
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
            "https://api.stg.hiro.so",
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
        &["-m", "tests/fixtures/mxs/Clarinet.toml"],
        &[
            "::get_constant counter MISSING_CONSTANT",
        ],
    );

    assert!(!output.is_empty());
    let first_line = &output[0];
    assert!(!first_line.is_empty());
}

#[test]
fn test_get_data_var_command() {
    let output = run_console_command(
        &["-m", "tests/fixtures/mxs/Clarinet.toml"],
        &[
            "::get_data_var counter count",
        ],
    );

    assert!(!output.is_empty());

    let first_line = &output[0];
    assert!(!first_line.is_empty());
}

#[test]
fn test_get_map_val_command() {
    let output = run_console_command(
        &["-m", "tests/fixtures/mxs/Clarinet.toml"],
        &[
            "::get_map_val counter MISSING_MAP { key: 1 }",
        ],
    );

    assert!(!output.is_empty());
    let first_line = &output[0];
    assert!(!first_line.is_empty());
}
