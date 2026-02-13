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

pub fn check_deployments(manifest: &ProjectManifest) -> Result<(), String> {
    let project_root_location = paths::project_root_from_manifest_location(&manifest.location)?;
    let files = get_deployments_files(&project_root_location)?;
    for (path, relative_path) in files.into_iter() {
        let _spec = match DeploymentSpecification::from_config_file(&path, &project_root_location) {
            Ok(spec) => spec,
            Err(msg) => {
                println!("{} {} syntax incorrect\n{}", red!("x"), relative_path, msg);
                continue;
            }
        };
        println!("{} {} successfully checked", green!("✔"), relative_path);
    }
    Ok(())
}

fn get_deployments_files(project_root_location: &Path) -> Result<Vec<(PathBuf, String)>, String> {
    let project_dir = project_root_location.to_path_buf();
    let prefix_len = project_dir.to_string_lossy().len() + 1;
    let deployments_dir = project_dir.join("deployments");
    let Ok(paths) = fs::read_dir(&deployments_dir) else {
        return Ok(vec![]);
    };
    let mut plans_paths = vec![];
    for path in paths {
        let file = path.unwrap().path();
        let is_extension_valid = file
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|ext| ext == "yml" || ext == "yaml");

        if let Some(true) = is_extension_valid {
            let relative_path = file.clone();
            let (_, relative_path) = relative_path.to_str().unwrap().split_at(prefix_len);
            plans_paths.push((file, relative_path.to_string()));
        }
    }

    Ok(plans_paths)
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
