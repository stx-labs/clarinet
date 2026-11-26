use std::path::Path;
use std::process::{Command, Output};

fn run_docs_in(dir: &Path, names: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .arg("docs")
        .args(names)
        .current_dir(dir)
        .output()
        .expect("Failed to run clarinet docs")
}

fn run_docs(names: &[&str]) -> Output {
    let temp_dir = tempfile::tempdir().unwrap();
    run_docs_in(temp_dir.path(), names)
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

#[test]
fn lists_functions_then_keywords() {
    let output = run_docs(&[]);
    assert!(output.status.success());
    let lines: Vec<String> = stdout(&output).lines().map(String::from).collect();

    assert_eq!(lines[0], "Functions");
    let keywords_heading = lines.iter().position(|l| l == "Keywords").unwrap();
    let map = lines.iter().position(|l| l == "map").unwrap();
    let tx_sender = lines.iter().position(|l| l == "tx-sender").unwrap();
    assert!(map < keywords_heading);
    assert!(keywords_heading < tx_sender);
}

#[test]
fn describes_a_function() {
    let output = run_docs(&["map"]);
    assert!(output.status.success());
    assert!(stdout(&output).starts_with("# map\nUsage\n(map func sequence_A"));
}

#[test]
fn describes_several_names_in_order() {
    let output = run_docs(&["tx-sender", "as-contract?", "with-stx"]);
    assert!(output.status.success());
    let out = stdout(&output);

    let positions: Vec<usize> = ["# tx-sender\n", "# as-contract?\n", "# with-stx\n"]
        .iter()
        .map(|heading| out.find(heading).unwrap())
        .collect();
    assert!(positions.is_sorted());
    assert!(out.contains("Returns the original sender of the current transaction"));
}

#[test]
fn unknown_name_fails_but_prints_found_names() {
    let output = run_docs(&["map", "not-a-clarity-name"]);
    assert_eq!(output.status.code(), Some(1));
    assert!(stdout(&output).starts_with("# map\n"));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("no documentation found for 'not-a-clarity-name'"));
}

#[test]
fn ignores_an_invalid_project() {
    let temp_dir = tempfile::tempdir().unwrap();
    std::fs::write(temp_dir.path().join("Clarinet.toml"), "not valid toml [[[").unwrap();

    let output = run_docs_in(temp_dir.path(), &["map"]);
    assert!(output.status.success());
    assert!(stdout(&output).starts_with("# map\n"));
}
