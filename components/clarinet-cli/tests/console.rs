use std::io::Write;
use std::process::{Command, Stdio};

use mockito::{Matcher, Server, ServerGuard};

fn mock_remote_node(network_id: u32, height: u32) -> ServerGuard {
    let mut server = Server::new();

    server
        .mock("GET", Matcher::Any)
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
                "burn_block_height": 882262,
                "tenure_height": 184037,
                "block_time": 1735934294,
                "burn_block_time": 1735451504,
                "hash": "0xaff3b535a135348ed00023ec1bdc3da9005253a9ce80a4906ade03ea6685d342",
                "index_block_hash": "0x201cf66636e693d95998b40ddd0cbe038432806046eed11866052f15a9fa8fc5",
                "burn_block_hash": "0x57f3e2bd4519e4263353bf6b7614a9cee7f2d36fe61409852d42e41afe5e6cad",
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
    // Skip console instructions and remove interactive prompts from command output.
    stdout_str
        .lines()
        .skip(3)
        .map(|line| line.strip_prefix(">> ").unwrap_or(line).to_string())
        .collect()
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
    // Testnet — height 50000 is in Epoch 4.0 on the krypton testnet.
    let testnet_server = mock_remote_node(0x8000_0000, 50000);
    let testnet_url = testnet_server.url();
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
    let mainnet_server = mock_remote_node(1, 907820);
    let mainnet_url = mainnet_server.url();
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
