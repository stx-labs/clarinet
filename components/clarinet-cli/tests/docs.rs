use std::path::Path;
use std::process::{Command, Output};

use clarinet_defaults::DEFAULT_CLARITY_VERSION;

fn run_docs_in(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .arg("docs")
        .args(args)
        .current_dir(dir)
        .output()
        .expect("Failed to run clarinet docs")
}

fn run_docs(args: &[&str]) -> Output {
    let temp_dir = tempfile::tempdir().unwrap();
    run_docs_in(temp_dir.path(), args)
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn lines(output: &Output) -> Vec<String> {
    stdout(output).lines().map(String::from).collect()
}

#[test]
fn lists_functions_then_keywords_of_the_default_version() {
    let output = run_docs(&[]);
    assert!(output.status.success());
    let lines = lines(&output);
    let position = |line| lines.iter().position(|l| l == line);

    assert!(lines[0].starts_with(&format!(
        "Functions and keywords in {DEFAULT_CLARITY_VERSION} ("
    )));
    let functions_heading = position("## Functions").unwrap();
    let keywords_heading = position("## Keywords").unwrap();
    let map = position("map").unwrap();
    let tx_sender = position("tx-sender").unwrap();
    assert!(functions_heading < map && map < keywords_heading && keywords_heading < tx_sender);
    assert_eq!(position("block-height"), None);
}

#[test]
fn lists_another_version_or_all_of_them() {
    let clarity1 = lines(&run_docs(&["-c", "1"]));
    assert!(clarity1[0].starts_with("Functions and keywords in Clarity 1 ("));
    assert!(clarity1.iter().any(|l| l == "block-height"));
    assert!(!clarity1.iter().any(|l| l == "stacks-block-height"));

    let all = lines(&run_docs(&["--clarity-version", "all"]));
    assert!(all[0].starts_with("Functions and keywords in all Clarity versions ("));
    assert!(all.iter().any(|l| l == "block-height"));
    assert!(all.iter().any(|l| l == "stacks-block-height"));
}

#[test]
fn rejects_an_unknown_version() {
    let output = run_docs(&["--clarity-version", "0"]);
    assert_eq!(output.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("invalid Clarity version '0'"), "{stderr}");
}

#[test]
fn describes_names_from_any_version() {
    let output = run_docs(&["block-height"]);
    assert!(output.status.success());
    assert!(stdout(&output).starts_with(
        "# block-height\n## Clarity version\nAvailable since Clarity 1 through Clarity 2\n"
    ));
}

#[test]
fn describes_a_function() {
    let output = run_docs(&["map"]);
    assert!(output.status.success());
    assert!(stdout(&output).starts_with("# map\n## Usage\n```clarity\n(map func sequence_A"));
}

#[test]
fn describes_an_operator_by_its_symbol() {
    let output = run_docs(&["<="]);
    assert!(output.status.success());
    assert!(stdout(&output).starts_with("# <=\n## Usage\n```clarity\n(<= "));
}

#[test]
fn describes_an_operator_as_listed() {
    let output = run_docs(&["--", "- (subtract)"]);
    assert!(output.status.success());
    assert!(stdout(&output).starts_with("# - (subtract)\n## Usage\n"));
}

#[test]
fn keeps_flags_after_names() {
    let output = run_docs(&["map", "--help"]);
    assert!(output.status.success());
    assert!(stdout(&output).starts_with("Display Clarity documentation"));
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
