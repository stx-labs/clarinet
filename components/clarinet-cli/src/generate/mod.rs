pub mod changes;
mod contract;
mod project;

use std::path::PathBuf;

pub use changes::Changes;
use contract::GetChangesForNewContract;
use project::GetChangesForNewProject;

use self::contract::GetChangesForRmContract;

pub fn get_changes_for_new_project(
    project_path: String,
    project_name: String,
    use_current_dir: bool,
    telemetry_enabled: bool,
) -> Result<Vec<Changes>, String> {
    let mut command = GetChangesForNewProject::new(
        project_path,
        project_name,
        use_current_dir,
        telemetry_enabled,
    );
    command.run()
}

pub fn get_changes_for_new_contract(
    manifest_location: PathBuf,
    contract_name: String,
    source: Option<String>,
    include_test: bool,
) -> Result<Vec<Changes>, String> {
    let mut command = GetChangesForNewContract::new(manifest_location, contract_name, source);
    command.run(include_test)
}

pub fn get_changes_for_rm_contract(
    manifest_location: PathBuf,
    contract_name: String,
) -> Result<Vec<Changes>, String> {
    let mut command = GetChangesForRmContract::new(manifest_location, contract_name);
    command.run()
}
