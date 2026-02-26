pub mod types;
mod ui;

use std::fs::{self};
use std::path::{Path, PathBuf};

use clarinet_deployments::types::{DeploymentGenerationArtifacts, DeploymentSpecification};
use clarinet_files::{paths, ProjectManifest, StacksNetwork};
pub use ui::start_ui;

pub fn generate_default_deployment(
    manifest: &ProjectManifest,
    network: &StacksNetwork,
    _no_batch: bool,
) -> Result<(DeploymentSpecification, DeploymentGenerationArtifacts), String> {
    let future = clarinet_deployments::generate_default_deployment(manifest, network, false, None);
    hiro_system_kit::nestable_block_on(future)
}

pub fn check_deployments(project_root: &Path) -> Result<(), String> {
    let deployments_path = project_root.join("deployments");
    let entries = fs::read_dir(&deployments_path).map_err(|e| e.to_string())?;

    let deployment_files_paths: Vec<PathBuf> = entries
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|p| {
            matches!(
                p.extension()
                    .and_then(|e| e.to_str())
                    .map(str::to_ascii_lowercase)
                    .as_deref(),
                Some("yml") | Some("yaml")
            )
        })
        .collect();

    for path in deployment_files_paths {
        let rel_path = path.strip_prefix(project_root).unwrap().to_string_lossy();
        if let Err(err_msg) = DeploymentSpecification::from_config_file(&path, project_root) {
            eprintln!("{} {rel_path} syntax incorrect\n{err_msg}", red!("x"));
            continue;
        }
        println!("{} {rel_path} successfully checked", green!("✔"));
    }
    Ok(())
}

pub fn write_deployment(
    deployment: &DeploymentSpecification,
    target_location: &Path,
    project_root: &Path,
    prompt_override: bool,
) -> Result<(), String> {
    if target_location.exists() && prompt_override {
        println!(
            "Deployment {} already exists.\n{}?",
            target_location.display(),
            yellow!("Overwrite [Y/n]")
        );
        let mut buffer = String::new();
        std::io::stdin().read_line(&mut buffer).unwrap();
        if buffer.starts_with('n') {
            return Err("deployment update aborted".to_string());
        }
    }

    paths::write_content(target_location, &deployment.to_file_content(project_root)?)?;
    Ok(())
}
