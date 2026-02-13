use std::fs;

use clarinet_deployments::initiate_session_from_manifest;
use clarinet_files::ProjectManifest;
use tempfile::TempDir;

#[test]
fn test_override_boot_contracts() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let project_path = temp_dir.path();

    let pox4_path = project_path.join("pox-4.clar");
    let clarinet_toml = format!(
        r#"
[project]
name = "test-project"
authors = []
description = "Test project for boot contract override"
telemetry = false

[project.override_boot_contracts_source]
pox-4 = "{}"
"#,
        pox4_path.to_string_lossy().replace('\\', "/")
    );
    fs::write(project_path.join("Clarinet.toml"), clarinet_toml)
        .expect("Failed to write Clarinet.toml");

    let custom_pox4 = r#"
(define-public (print-something)
    (ok (print "Hello, world!"))
)
"#;
    fs::write(project_path.join("pox-4.clar"), custom_pox4).expect("Failed to write pox-4.clar");

    let manifest_path = project_path.join("Clarinet.toml");
    let manifest =
        ProjectManifest::from_location(&manifest_path, false).expect("Failed to load manifest");
    let mut session = initiate_session_from_manifest(&manifest);
    session.update_epoch(clarity::types::StacksEpochId::Epoch25);

    let expr = "(contract-call? 'SP000000000000000000002Q6VF78.pox-4 print-something)";
    let result = session.eval(expr.to_string(), false);
    if let Err(diags) = &result {
        println!("Diagnostics: {diags:?}");
    }
    assert!(result.is_ok());
    let execution_result = result.unwrap();
    assert!(execution_result.diagnostics.is_empty());
    temp_dir.close().expect("Failed to clean up temp dir");
}
