use std::fs;
use std::path::Path;
use std::process::Command;

use clarinet_files::{FileLocation, ProjectManifest, ProjectManifestFile};
use indoc::formatdoc;

#[track_caller]
fn parse_manifest(project_dir: &Path) -> ProjectManifest {
    let manifest_path = project_dir.join("Clarinet.toml");
    let manifest_str = fs::read_to_string(&manifest_path).expect("Failed to read Clarinet.toml");
    let manifest_file: ProjectManifestFile = toml::from_str(&manifest_str).unwrap();
    let location = FileLocation::from_path(manifest_path);
    ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap()
}

#[track_caller]
fn create_new_project(project_name: &str) -> tempfile::TempDir {
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let status = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["new", project_name, "--disable-telemetry"])
        .current_dir(&temp_dir)
        .status();
    assert!(status.unwrap().success());
    temp_dir
}

#[test]
fn test_new_project() {
    let project_name = "test_project";
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
    let cmd = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["new", project_name, "--disable-telemetry"])
        .current_dir(&temp_dir)
        .output()
        .expect("Failed to execute clarinet new");
    assert!(cmd.status.success());

    let expected_lines = [
        "Telemetry disabled. Clarinet will not collect any data on this project.",
        "Created directory test_project",
        "Created directory contracts",
        "Created directory settings",
        "Created directory tests",
        "Created file Clarinet.toml",
        "Created file settings/Mainnet.toml",
        "Created file settings/Testnet.toml",
        "Created file settings/Devnet.toml",
        "Created directory .vscode",
        "Created file .vscode/settings.json",
        "Created file .vscode/tasks.json",
        "Created file .gitignore",
        "Created file .gitattributes",
        "Created file package.json",
        "Created file tsconfig.json",
        "Created file vitest.config.ts",
    ];
    let stdout = String::from_utf8_lossy(&cmd.stdout);
    let stdout_lines: Vec<_> = stdout.lines().map(str::trim).collect();
    let actual = &stdout_lines[..expected_lines.len()];
    assert_eq!(actual, expected_lines);

    let project_path = temp_dir.path().join(project_name);
    assert!(project_path.is_dir(), "Project directory missing");
    let clarinet_toml = project_path.join("Clarinet.toml");
    assert!(clarinet_toml.is_file(), "Clarinet.toml missing");

    let manifest_str = fs::read_to_string(&clarinet_toml).expect("Failed to read Clarinet.toml");
    let expected = formatdoc! {"
        [project]
        name = \"{}\"", project_name};
    let actual = manifest_str.lines().take(2).collect::<Vec<_>>().join("\n");
    assert_eq!(actual, expected, "Clarinet.toml header mismatch");

    let expected_files = [
        "Clarinet.toml",
        "settings/Mainnet.toml",
        "settings/Testnet.toml",
        "settings/Devnet.toml",
        ".vscode/settings.json",
        ".vscode/tasks.json",
        ".gitignore",
        ".gitattributes",
        "package.json",
        "tsconfig.json",
        "vitest.config.ts",
    ];
    for file in expected_files.iter() {
        let file_path = project_path.join(file);
        assert!(file_path.is_file(), "'{file}' is missing");
        let metadata = fs::metadata(&file_path).expect("Failed to get file metadata");
        assert!(metadata.len() > 0, "'{file}' is empty");
    }
}

#[test]
fn test_contract_new() {
    let project_name = "test_contract_new";
    let temp_dir = create_new_project(project_name);
    let project_path = temp_dir.path().join(project_name);
    let contract_name = "test_contract";
    let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["contract", "new", contract_name])
        .current_dir(&project_path)
        .output()
        .unwrap();
    assert!(output.status.success(), "clarinet contract new failed");

    let expected_lines = [
        format!("Created file contracts/{contract_name}.clar"),
        format!("Created file tests/{contract_name}.test.ts"),
        format!("Updated Clarinet.toml with contract {contract_name}"),
    ];
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stdout_lines: Vec<_> = stdout.lines().map(str::trim).collect();
    let actual = &stdout_lines[..expected_lines.len()];
    assert_eq!(actual, expected_lines);

    let contract_path = project_path
        .join("contracts")
        .join(format!("{contract_name}.clar"));
    assert!(contract_path.is_file(), "Contract file missing");

    let contract_str = fs::read_to_string(&contract_path).expect("Failed to read contract file");
    let expected = format!(";; title: {contract_name}");
    assert_eq!(contract_str.lines().next().unwrap_or(""), expected);

    let expected_files = [
        "contracts/test_contract.clar",
        "tests/test_contract.test.ts",
    ];
    for file in expected_files.iter() {
        let file_path = project_path.join(file);
        assert!(file_path.is_file(), "'{file}' is missing");
        let metadata = fs::metadata(&file_path).expect("Failed to get file metadata");
        assert!(metadata.len() > 0, "'{file}' is empty");
    }
}

#[test]
fn test_requirement_add() {
    let project_name = "test_requirement_add";
    let temp_dir = create_new_project(project_name);
    let project_path = temp_dir.path().join(project_name);
    let requirement_name = "SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard";
    let status = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["requirement", "add", requirement_name])
        .current_dir(&project_path)
        .status();
    assert!(status.unwrap().success());

    let manifest = parse_manifest(&project_path);
    let found = manifest
        .project
        .requirements
        .iter()
        .flatten()
        .any(|c| c.contract_id == requirement_name);
    assert!(found, "Requirement not found in manifest");
}

#[test]
fn test_formatter_check() {
    let project_name = "test_formatter_check";
    let temp_dir = create_new_project(project_name);
    let project_path = temp_dir.path().join(project_name);

    // Create a contract with unformatted code
    let contract_name = "test_contract";
    let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["contract", "new", contract_name])
        .current_dir(&project_path)
        .output()
        .unwrap();
    assert!(output.status.success(), "clarinet contract new failed");

    let contract_path = project_path
        .join("contracts")
        .join(format!("{contract_name}.clar"));

    // Write unformatted code to the contract file
    let unformatted_code = indoc::indoc! {"
        ;; title: test_contract
        ;; description: A test contract for formatter check

        (define-public (test-function (amount uint))
        (ok amount))

        (define-private (another-function)
        (begin
        (ok true)
        (ok false)))
    "};
    fs::write(&contract_path, unformatted_code).expect("Failed to write unformatted code");

    // Test that --check flag detects unformatted files
    let check_output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["format", "--check"])
        .current_dir(&project_path)
        .output()
        .unwrap();

    // Should fail because files need formatting
    assert!(
        !check_output.status.success(),
        "Check should fail for unformatted files"
    );

    let stderr = String::from_utf8_lossy(&check_output.stderr);
    assert!(stderr.contains("✗"), "Should show error symbol");
    assert!(
        stderr.contains("file needs formatting"),
        "Should indicate file needs formatting"
    );
    assert!(
        stderr.contains("test_contract.clar"),
        "Should list the unformatted file"
    );

    // Format the file in-place
    let format_output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["format", "--in-place"])
        .current_dir(&project_path)
        .output()
        .unwrap();
    assert!(
        format_output.status.success(),
        "Format in-place should succeed"
    );

    // Test that --check flag now passes
    let check_output_after = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["format", "--check"])
        .current_dir(&project_path)
        .output()
        .unwrap();

    // Should succeed because files are now formatted
    assert!(
        check_output_after.status.success(),
        "Check should pass for formatted files"
    );

    let stdout = String::from_utf8_lossy(&check_output_after.stdout);
    assert!(stdout.contains("✔"), "Should show success symbol");
    assert!(
        stdout.contains("All files are properly formatted"),
        "Should indicate all files are formatted"
    );

    // Test --check with a specific file
    let check_specific_output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args([
            "format",
            "--check",
            "--file",
            "contracts/test_contract.clar",
        ])
        .current_dir(&project_path)
        .output()
        .unwrap();

    assert!(
        check_specific_output.status.success(),
        "Check specific file should pass"
    );

    let stdout_specific = String::from_utf8_lossy(&check_specific_output.stdout);
    assert!(
        stdout_specific.contains("✔"),
        "Should show success symbol for specific file"
    );
    assert!(
        stdout_specific.contains("All files are properly formatted"),
        "Should indicate file is formatted"
    );

    // Test --check with non-existent file
    let check_nonexistent_output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["format", "--check", "--file", "contracts/nonexistent.clar"])
        .current_dir(&project_path)
        .output()
        .unwrap();

    // Should fail because file doesn't exist
    assert!(
        !check_nonexistent_output.status.success(),
        "Check should fail for non-existent file"
    );
}
