use std::fs;
use std::path::Path;
use std::process::Command;

use clarinet_deployments::load_deployment;
use clarinet_deployments::types::TransactionSpecification;
use clarinet_files::{ProjectManifest, ProjectManifestFile, StacksNetwork};
use clarinet_lib::deployments::generate_devnet_deployment;
use clarinet_lib::frontend::cli::load_manifest_or_exit;
use indoc::{formatdoc, indoc};

const ENV_SIMNET_CONTRACT_SOURCE: &str = indoc! {r#"
    (define-public (double (n uint))
      (ok (* n u2))
    )

    ;; #[env(simnet)]
    (define-private (test-double)
      (ok (asserts! (is-eq (ok u8) (double u4)) (err "fail")))
    )
"#};

/// Create a new project with a contract containing `#[env(simnet)]` code.
/// Returns `(temp_dir, project_path)`.
#[track_caller]
fn create_project_with_env_simnet(project_name: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let temp_dir = create_new_project(project_name);
    let project_path = temp_dir.path().join(project_name);

    let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["contract", "new", "my-contract"])
        .current_dir(&project_path)
        .output()
        .unwrap();
    assert!(output.status.success(), "clarinet contract new failed");

    let contract_path = project_path.join("contracts").join("my-contract.clar");
    fs::write(&contract_path, ENV_SIMNET_CONTRACT_SOURCE).unwrap();

    (temp_dir, project_path)
}

#[track_caller]
fn parse_manifest(project_dir: &Path) -> ProjectManifest {
    let manifest_path = project_dir.join("Clarinet.toml");
    let manifest_str = fs::read_to_string(&manifest_path).expect("Failed to read Clarinet.toml");
    let manifest_file: ProjectManifestFile = toml::from_str(&manifest_str).unwrap();
    ProjectManifest::from_project_manifest_file(manifest_file, &manifest_path, false).unwrap()
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

/// Run `clarinet check` with JSON output in the given directory and return the parsed JSON.
#[track_caller]
fn run_clarinet_check_json(args: &[&str], working_dir: &Path) -> serde_json::Value {
    let mut full_args = vec!["check"];
    full_args.extend_from_slice(args);
    full_args.extend_from_slice(&["--output", "json"]);

    let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(&full_args)
        .current_dir(working_dir)
        .output()
        .expect("Failed to execute clarinet check");

    let stdout = String::from_utf8_lossy(&output.stdout);
    serde_json::from_str(&stdout).expect("clarinet check should produce valid JSON")
}

/// Extract all diagnostic messages from `clarinet check --output json` output.
#[track_caller]
fn collect_check_diagnostic_messages(json: &serde_json::Value) -> Vec<String> {
    json["diagnostics"]
        .as_object()
        .expect("diagnostics should be an object")
        .values()
        .filter_map(|v| v.as_array())
        .flatten()
        .filter_map(|d| d["message"].as_str())
        .map(String::from)
        .collect()
}

/// Strip a TOML section (and its contents up to the next section header) from a file.
fn strip_toml_section(path: &Path, section_header: &str) {
    let content = fs::read_to_string(path).unwrap();
    let mut in_section = false;
    let stripped: String = content
        .lines()
        .filter(|line| {
            if line.starts_with(section_header) {
                in_section = true;
                return false;
            }
            if in_section && line.starts_with('[') {
                in_section = false;
            }
            !in_section
        })
        .collect::<Vec<_>>()
        .join("\n");
    fs::write(path, stripped).expect("Failed to write file");
}

/// `clarinet check <file>` with no Clarinet.toml should emit lint warnings by default.
#[test]
fn test_check_single_file_default_lints() {
    let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");

    let contract_path = temp_dir.path().join("unused.clar");
    fs::write(&contract_path, "(define-constant MY_UNUSED_CONST u42)\n")
        .expect("Failed to write contract");

    let json = run_clarinet_check_json(&[contract_path.to_str().unwrap()], temp_dir.path());
    let messages = collect_check_diagnostic_messages(&json);

    assert!(
        messages.iter().any(|m| m.contains("never used")),
        "expected an unused constant lint warning from default lints, got: {messages:?}"
    );
}

/// `clarinet check` in a project with no `[repl.analysis]` section should emit lint warnings.
#[test]
fn test_check_project_default_lints() {
    let project_name = "test_default_lints";
    let temp_dir = create_new_project(project_name);
    let project_path = temp_dir.path().join(project_name);

    let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["contract", "new", "my-contract"])
        .current_dir(&project_path)
        .output()
        .unwrap();
    assert!(output.status.success(), "clarinet contract new failed");

    let contract_path = project_path.join("contracts").join("my-contract.clar");
    fs::write(&contract_path, "(define-constant MY_UNUSED_CONST u42)\n")
        .expect("Failed to write contract");

    strip_toml_section(&project_path.join("Clarinet.toml"), "[repl.analysis]");

    let json = run_clarinet_check_json(&[], &project_path);
    let messages = collect_check_diagnostic_messages(&json);

    assert!(
        messages.iter().any(|m| m.contains("never used")),
        "expected an unused constant lint warning from default lints, got: {messages:?}"
    );
}

/// `clarinet check` should emit lint warnings for project contracts but not for requirements,
/// even when a requirement has the same contract name as a project contract.
#[test]
fn test_check_skips_requirement_lint_warnings() {
    let project_name = "test_skip_req_warnings";
    let temp_dir = create_new_project(project_name);
    let project_path = temp_dir.path().join(project_name);

    // Create a project contract
    let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["contract", "new", "my-contract"])
        .current_dir(&project_path)
        .output()
        .unwrap();
    assert!(output.status.success(), "clarinet contract new failed");

    // Write lint-triggering code (unused constant) to the project contract
    let project_contract = project_path.join("contracts").join("my-contract.clar");
    fs::write(
        &project_contract,
        "(define-constant PROJECT_UNUSED_CONST u42)\n",
    )
    .unwrap();

    // Strip [repl.analysis] so default lints are used
    strip_toml_section(&project_path.join("Clarinet.toml"), "[repl.analysis]");

    // Create requirement contract files in the cache directory.
    // Requirement A: a uniquely-named requirement.
    // Requirement B: a requirement with the SAME contract name as the project contract
    //   but deployed from a different address. This tests that we don't confuse
    //   two contracts that share a name but differ in sender and path.
    let cache_req_dir = project_path.join(".cache").join("requirements");
    fs::create_dir_all(&cache_req_dir).unwrap();

    fs::write(
        cache_req_dir.join("other-req.clar"),
        "(define-constant REQ_A_UNUSED u99)\n",
    )
    .unwrap();

    fs::write(
        cache_req_dir.join("my-contract.clar"),
        "(define-constant REQ_B_UNUSED u100)\n",
    )
    .unwrap();

    // Write a deployment plan that includes all three contracts.
    // Requirements are listed as emulated-contract-publish entries with a different
    // emulated-sender and paths under .cache/requirements/.
    // Using --use-on-disk-deployment-plan forces clarinet to use this file directly,
    // which (unlike computed deployments) includes ALL contracts in the diagnostics output.
    let deployment_yaml = indoc! {r#"
        ---
        id: 0
        name: "Test deployment with requirements"
        network: simnet
        genesis:
          wallets:
            - name: deployer
              address: ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
              balance: "100000000000000"
              sbtc-balance: "0"
          contracts:
            - genesis
            - lockup
            - bns
            - cost-voting
            - costs
            - pox
            - costs-2
            - pox-2
            - costs-3
            - pox-3
            - pox-4
            - signers
            - signers-voting
            - costs-4
        plan:
          batches:
            - id: 0
              transactions:
                - transaction-type: emulated-contract-publish
                  contract-name: other-req
                  emulated-sender: SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE
                  path: .cache/requirements/other-req.clar
                  clarity-version: 1
                - transaction-type: emulated-contract-publish
                  contract-name: my-contract
                  emulated-sender: SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE
                  path: .cache/requirements/my-contract.clar
                  clarity-version: 1
                - transaction-type: emulated-contract-publish
                  contract-name: my-contract
                  emulated-sender: ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
                  path: contracts/my-contract.clar
                  clarity-version: 1
              epoch: "2.0"
    "#};
    let deployment_dir = project_path.join("deployments");
    fs::create_dir_all(&deployment_dir).unwrap();
    fs::write(
        deployment_dir.join("default.simnet-plan.yaml"),
        deployment_yaml,
    )
    .unwrap();

    // Run clarinet check with JSON output and the on-disk deployment plan
    let json = run_clarinet_check_json(&["--use-on-disk-deployment-plan"], &project_path);

    let diagnostics = json["diagnostics"]
        .as_object()
        .expect("diagnostics should be an object");

    // Project contract should have lint warnings
    let has_project_warning = diagnostics
        .iter()
        .filter(|(path, _)| path.ends_with("contracts/my-contract.clar"))
        .flat_map(|(_, diags)| diags.as_array().unwrap())
        .any(|d| {
            d["message"]
                .as_str()
                .is_some_and(|m| m.contains("never used"))
        });
    assert!(
        has_project_warning,
        "expected an unused constant lint warning for the project contract, got diagnostics: {diagnostics:?}"
    );

    // Requirement contracts should NOT have any diagnostics.
    // Since requirements have skip_analysis=true, REPL analysis (lints) is skipped.
    // They only get standard Clarity analysis (which produces no warnings for unused
    // constants), so they should not appear in the JSON output at all (entries with
    // empty diagnostics are filtered out).
    for (path, diags) in diagnostics {
        if path.contains("requirements") {
            let diag_list = diags.as_array().unwrap();
            assert!(
                diag_list.is_empty(),
                "expected no diagnostics for requirement at {path}, got: {diag_list:?}"
            );
        }
    }
}

/// `clarinet check` (project-wide, standard output) should display lint warnings
/// from `DiagnosticsDigest`, not just in JSON mode.
#[test]
fn test_check_project_standard_output_shows_lint_warnings() {
    let project_name = "test_std_lint_output";
    let temp_dir = create_new_project(project_name);
    let project_path = temp_dir.path().join(project_name);

    let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["contract", "new", "my-contract"])
        .current_dir(&project_path)
        .output()
        .unwrap();
    assert!(output.status.success(), "clarinet contract new failed");

    let contract_path = project_path.join("contracts").join("my-contract.clar");
    fs::write(&contract_path, "(define-constant MY_UNUSED_CONST u42)\n")
        .expect("Failed to write contract");

    strip_toml_section(&project_path.join("Clarinet.toml"), "[repl.analysis]");

    // Run clarinet check with standard output (no --output json)
    let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["check"])
        .env("NO_COLOR", "1")
        .current_dir(&project_path)
        .output()
        .expect("Failed to execute clarinet check");

    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(
        stdout.contains("warning") && stdout.contains("never used"),
        "expected lint warning in standard output, got:\n{stdout}"
    );
    assert!(
        stdout.contains("warning detected"),
        "expected warning count in standard output, got:\n{stdout}"
    );
}

/// Devnet deployments should strip `#[env(simnet)]` code from contract sources.
/// This tests the fix for https://github.com/hirosystems/clarinet/issues/2338.
#[test]
fn test_devnet_deployment_strips_env_simnet() {
    let (_temp_dir, project_path) = create_project_with_env_simnet("test_env_simnet_devnet");

    let manifest_path = project_path
        .join("Clarinet.toml")
        .to_string_lossy()
        .to_string();
    let manifest = load_manifest_or_exit(Some(manifest_path), false);

    let (deployment, _, found_env_simnet) =
        generate_devnet_deployment(&manifest).expect("Failed to generate devnet deployment");

    assert!(
        found_env_simnet,
        "should detect #[env(simnet)] annotations in the source"
    );

    // Devnet uses ContractPublish (real transactions), not EmulatedContractPublish.
    // Verify the simnet-only code was stripped from every contract source.
    for batch in &deployment.plan.batches {
        for tx in &batch.transactions {
            if let TransactionSpecification::ContractPublish(spec) = tx {
                assert!(
                    spec.source.contains("double"),
                    "non-simnet code should be preserved, got:\n{}",
                    spec.source
                );
                assert!(
                    !spec.source.contains("test-double"),
                    "#[env(simnet)] code should be stripped from devnet ContractPublish source, got:\n{}",
                    spec.source
                );
            }
        }
    }

    // Also check the contracts map used for analysis.
    for (source, _) in deployment.contracts.values() {
        assert!(
            !source.contains("test-double"),
            "#[env(simnet)] code should be stripped from devnet contracts map, got:\n{source}"
        );
    }
}

/// `clarinet deployments generate --devnet` should produce a deployment plan
/// whose cost reflects the stripped (shorter) source, not the full source.
#[test]
fn test_deployments_generate_devnet_strips_env_simnet() {
    let (_temp_dir, project_path) =
        create_project_with_env_simnet("test_deploy_gen_devnet_env_simnet");

    let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["deployments", "generate", "--devnet"])
        .current_dir(&project_path)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "clarinet deployments generate --devnet failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Load the generated plan. `load_deployment` re-reads the .clar source from
    // disk (unstripped), but preserves the `cost` field from the YAML — which was
    // computed from the stripped source during generation.
    let yaml_path = project_path
        .join("deployments")
        .join("default.devnet-plan.yaml");
    let deployment = load_deployment(&project_path, &yaml_path)
        .expect("failed to load generated devnet deployment");

    let full_source =
        fs::read_to_string(project_path.join("contracts").join("my-contract.clar")).unwrap();
    let default_fee_rate: u64 = 10;
    let full_cost = default_fee_rate * full_source.len() as u64;

    for batch in &deployment.plan.batches {
        for tx in &batch.transactions {
            if let TransactionSpecification::ContractPublish(spec) = tx {
                assert!(
                    spec.cost < full_cost,
                    "cost should reflect stripped source (expected < {full_cost}, got {})",
                    spec.cost
                );
            }
        }
    }
}

/// `deployment_environment()` should return `OnChain` for all non-simnet networks.
/// This ensures `#[env(simnet)]` code is stripped for devnet, testnet, and mainnet.
#[test]
fn test_deployment_environment_mapping() {
    use clarity_repl::utils::Environment;

    assert_eq!(
        StacksNetwork::Simnet.deployment_environment(),
        Environment::Simnet,
        "simnet should keep #[env(simnet)] code"
    );
    assert_eq!(
        StacksNetwork::Devnet.deployment_environment(),
        Environment::OnChain,
        "devnet should strip #[env(simnet)] code"
    );
    assert_eq!(
        StacksNetwork::Testnet.deployment_environment(),
        Environment::OnChain,
        "testnet should strip #[env(simnet)] code"
    );
    assert_eq!(
        StacksNetwork::Mainnet.deployment_environment(),
        Environment::OnChain,
        "mainnet should strip #[env(simnet)] code"
    );
}

#[test]
fn test_schema_validates_all_manifests() {
    // Generate schema from `clarinet manifest schema`
    let output = Command::new(env!("CARGO_BIN_EXE_clarinet"))
        .args(["manifest", "schema"])
        .output()
        .expect("Failed to execute clarinet manifest schema");
    assert!(output.status.success(), "clarinet manifest schema failed");

    let schema_json: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("Schema output is not valid JSON");
    let validator =
        jsonschema::validator_for(&schema_json).expect("Schema itself is not a valid JSON Schema");

    // Find all Clarinet.toml files in the repo
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();

    let manifest_paths = [
        "components/clarinet-cli/examples/billboard/Clarinet.toml",
        "components/clarinet-cli/examples/counter/Clarinet.toml",
        "components/clarinet-cli/examples/custom-boot-contracts/Clarinet.toml",
        "components/clarinet-cli/examples/simple-nft/Clarinet.toml",
        "components/clarinet-cli/examples/swappool/Clarinet.toml",
        "components/clarinet-cli/tests/fixtures/mxs/Clarinet.toml",
        "components/clarinet-sdk/node/tests/fixtures/Clarinet.toml",
        "components/clarity-vscode/test-data/simple/Clarinet.toml",
        "components/clarity-vscode/test-data/test-cases/Clarinet.toml",
    ];

    let mut failures = Vec::new();
    for relative_path in &manifest_paths {
        let path = workspace_root.join(relative_path);
        let toml_str = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("Failed to read {relative_path}: {e}"));
        let toml_value: serde_json::Value = toml::from_str(&toml_str)
            .unwrap_or_else(|e| panic!("Failed to parse {relative_path}: {e}"));

        if let Err(error) = validator.validate(&toml_value) {
            failures.push(format!("{relative_path}:\n  - {error}"));
        }
    }

    assert!(
        failures.is_empty(),
        "Schema validation failed for:\n{}",
        failures.join("\n\n")
    );
}
