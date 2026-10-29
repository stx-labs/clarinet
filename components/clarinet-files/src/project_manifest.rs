use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use clarity::types::StacksEpochId;
use clarity::vm::diagnostic::{Diagnostic, Level};
use clarity::vm::ClarityVersion;
use clarity_repl::repl::boot::BOOT_CONTRACTS_NAMES;
use clarity_repl::repl::{ClarityCodeSource, ClarityContract, ContractDeployer};
use clarity_repl::{analysis, repl};
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use toml::Value as TomlValue;

#[cfg(feature = "json_schema")]
use crate::schema;
use crate::{paths, FileAccessor};

pub const INVALID_CLARITY_VERSION: &str =
    "clarity_version field invalid (value supported: 1, 2, 3, 4, 5, 6, 7)";

#[derive(Debug, Clone)]
pub struct ClarityContractMetadata {
    pub name: String,
    pub deployer: ContractDeployer,
    pub clarity_version: ClarityVersion,
    pub epoch: StacksEpochId,
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
#[cfg_attr(feature = "json_schema", schemars(rename_all = "snake_case"))]
pub struct ProjectConfigFile {
    /// Name of the project
    name: String,
    /// List of project authors
    #[serde(default)]
    authors: Option<Vec<String>>,
    /// Project description
    #[serde(default)]
    description: Option<String>,
    /// Enable or disable telemetry
    #[serde(default)]
    telemetry: Option<bool>,
    /// External contract dependencies
    #[serde(default)]
    #[cfg_attr(
        feature = "json_schema",
        schemars(schema_with = "schema::requirements_schema")
    )]
    requirements: Option<TomlValue>,
    /// List of boot contracts to include
    #[serde(default)]
    boot_contracts: Option<Vec<String>>,
    /// Override default boot contract implementations
    #[serde(default)]
    override_boot_contracts_source: Option<BTreeMap<String, String>>,
    /// Directory for caching build artifacts
    #[serde(default = "default_cache_dir")]
    cache_dir: String,
    /// Removed list of analysis passes, only captured to warn about it
    #[serde(default, skip_serializing)]
    #[cfg_attr(feature = "json_schema", schemars(skip))]
    analysis: Option<serde::de::IgnoredAny>,
}

fn default_cache_dir() -> String {
    ".cache".to_string()
}

#[derive(Serialize, Deserialize, Debug)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct ProjectManifestFile {
    /// Project-level configuration
    project: ProjectConfigFile,
    /// Contract definitions
    #[serde(default)]
    #[cfg_attr(
        feature = "json_schema",
        schemars(schema_with = "schema::contracts_schema")
    )]
    contracts: Option<TomlValue>,
    /// REPL settings
    #[serde(default)]
    #[cfg_attr(feature = "json_schema", schemars(schema_with = "schema::repl_schema"))]
    repl: Option<repl::SettingsFile>,
    /// Linter settings
    #[serde(default)]
    #[cfg_attr(
        feature = "json_schema",
        schemars(schema_with = "schema::linter_schema")
    )]
    linter: Option<analysis::SettingsFile>,
}

#[derive(Debug, Clone)]
pub struct ProjectManifest {
    pub project: ProjectConfig,
    pub contracts: BTreeMap<String, ClarityContract>,
    pub repl_settings: repl::Settings,
    pub root_dir: PathBuf,
    pub location: PathBuf,
    pub contracts_settings: HashMap<PathBuf, ClarityContractMetadata>,
    #[serde(skip)]
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub struct OverrideBootContract {
    /// Path as written in `Clarinet.toml`, relative to the manifest root.
    pub path: String,
    /// Pre-loaded Clarity source. Populated by `from_location` / `from_file_accessor`
    /// so consumers don't need filesystem access.
    pub source: String,
}

impl OverrideBootContract {
    pub fn resolve_path(&self, root: &Path) -> PathBuf {
        root.join(&self.path)
    }
}

#[derive(Debug, Clone)]
pub struct ProjectConfig {
    pub name: String,
    pub authors: Vec<String>,
    pub description: String,
    pub telemetry: bool,
    pub requirements: Option<Vec<RequirementConfig>>,
    pub cache_location: PathBuf,
    pub boot_contracts: Vec<String>,
    // Populated by `from_location` / `from_file_accessor`.
    pub override_boot_contracts_source: BTreeMap<String, OverrideBootContract>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Default)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct RequirementConfig {
    /// Contract identifier (e.g., SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait)
    pub contract_id: String,
}

impl ProjectManifest {
    pub async fn from_file_accessor(
        location: &Path,
        allow_remote_data_fetching: bool,
        file_accessor: &dyn FileAccessor,
    ) -> Result<ProjectManifest, String> {
        let content = file_accessor
            .read_file(location.to_string_lossy().into_owned())
            .await?;

        let project_manifest_file: ProjectManifestFile = toml::from_slice(content.as_bytes())
            .map_err(|e| format!("Clarinet.toml file malformatted {e:?}"))?;

        let mut manifest = ProjectManifest::from_project_manifest_file(
            project_manifest_file,
            location,
            allow_remote_data_fetching,
        )?;

        let resolved_paths: BTreeMap<String, String> = manifest
            .project
            .override_boot_contracts_source
            .iter()
            .map(|(name, entry)| {
                (
                    name.clone(),
                    entry
                        .resolve_path(&manifest.root_dir)
                        .to_string_lossy()
                        .into_owned(),
                )
            })
            .collect();
        let sources = file_accessor
            .read_files(resolved_paths.values().cloned().collect())
            .await?;
        for (name, entry) in manifest.project.override_boot_contracts_source.iter_mut() {
            let path = &resolved_paths[name];
            entry.source = sources.get(path).cloned().ok_or_else(|| {
                format!("Unable to read override boot contract '{name}' at {path}")
            })?;
        }

        Ok(manifest)
    }

    pub fn from_location(
        location: &Path,
        allow_remote_data_fetching: bool,
    ) -> Result<ProjectManifest, String> {
        let project_manifest_file_content = paths::read_content(location)?;
        let project_manifest_file: ProjectManifestFile =
            toml::from_slice(&project_manifest_file_content[..])
                .map_err(|e| format!("Clarinet.toml file malformatted {e:?}"))?;

        let mut manifest = ProjectManifest::from_project_manifest_file(
            project_manifest_file,
            location,
            allow_remote_data_fetching,
        )?;

        for entry in manifest.project.override_boot_contracts_source.values_mut() {
            entry.source = paths::read_content_as_utf8(&entry.resolve_path(&manifest.root_dir))?;
        }

        Ok(manifest)
    }

    // process a contract and add it to the configuration
    fn register_contract(
        contract_name: &str,
        contract_path: &str,
        deployer: ContractDeployer,
        epoch: Option<&str>,
        clarity_version: Option<&str>,
        project_root_location: &Path,
        config_contracts: &mut BTreeMap<String, ClarityContract>,
        contracts_settings: &mut HashMap<PathBuf, ClarityContractMetadata>,
    ) -> Result<(), String> {
        let code_source = match PathBuf::from_str(contract_path) {
            Ok(path) => ClarityCodeSource::ContractOnDisk(path),
            Err(e) => return Err(format!("unable to parse path {contract_path} ({e})")),
        };

        let (epoch, parsed_clarity_version) =
            get_epoch_and_clarity_version(epoch, clarity_version)?;

        config_contracts.insert(
            contract_name.to_string(),
            ClarityContract {
                name: contract_name.to_string(),
                deployer: deployer.clone(),
                code_source,
                clarity_version: parsed_clarity_version,
                epoch: epoch.clone(),
                skip_analysis: false,
            },
        );

        let contract_location = project_root_location.join(contract_path);
        contracts_settings.insert(
            contract_location,
            ClarityContractMetadata {
                name: contract_name.to_string(),
                deployer,
                clarity_version: parsed_clarity_version,
                epoch: epoch.resolve(),
            },
        );

        Ok(())
    }

    pub fn from_project_manifest_file(
        project_manifest_file: ProjectManifestFile,
        manifest_location: &Path,
        allow_remote_data_fetching: bool,
    ) -> Result<ProjectManifest, String> {
        let diagnostics = [
            project_manifest_file.project.analysis.is_some().then_some(
                "`analysis` in [project] is no longer supported and will be ignored. Configure lints in [linter.lints]",
            ),
            project_manifest_file
                .repl
                .as_ref()
                .is_some_and(repl::SettingsFile::has_removed_analysis_section)
                .then_some(
                    "[repl.analysis] is no longer supported and will be ignored. Move `lints` to [linter.lints], `lint_groups` to [linter.groups], and `include`/`exclude` to [linter]. `true` and \"on\" now use the default level of each lint, not `warning`",
                ),
        ]
        .into_iter()
        .flatten()
        .map(|message| {
            clarity_repl::ueprint!("Warning: {message}");
            Diagnostic {
                level: Level::Warning,
                message: message.into(),
                spans: vec![],
                suggestion: None,
            }
        })
        .collect();
        let mut repl_settings = project_manifest_file
            .repl
            .map(repl::Settings::from)
            .unwrap_or_default();
        repl_settings.analysis = project_manifest_file
            .linter
            .map(analysis::Settings::try_from)
            .transpose()?
            .unwrap_or_else(analysis::Settings::with_default_lints);

        // when performing static analysis, we never want to enable remote data fetching
        if !allow_remote_data_fetching {
            repl_settings.remote_data.enabled = false;
        }

        let project_name = project_manifest_file.project.name;
        let root_path = manifest_location
            .parent()
            .ok_or_else(|| "unable to get parent of manifest location".to_string())?;

        let cache_dir = project_manifest_file.project.cache_dir;
        let cache_location = paths::try_parse_path(&cache_dir, Some(root_path))
            .ok_or_else(|| format!("unable to parse path {cache_dir}"))?;

        let boot_contracts = BOOT_CONTRACTS_NAMES
            .iter()
            .map(|n| n.to_string())
            .collect::<Vec<_>>();

        // if an override doesn't correspond with one of the boot contracts, we warn and discard that override
        let mut override_boot_contracts_source: BTreeMap<String, OverrideBootContract> =
            BTreeMap::new();
        if let Some(overrides) = project_manifest_file.project.override_boot_contracts_source {
            for (contract_name, contract_path) in overrides {
                if !boot_contracts.contains(&contract_name) {
                    clarity_repl::ueprint!("Warning: {contract_name} custom boot contract was not included because it's not part of the set of default boot contracts");
                    clarity_repl::ueprint!("Available boot contracts: {boot_contracts:?}");
                    continue;
                }
                override_boot_contracts_source.insert(
                    contract_name,
                    OverrideBootContract {
                        path: contract_path,
                        source: String::new(),
                    },
                );
            }
        }

        let project = ProjectConfig {
            name: project_name,
            requirements: None,
            description: project_manifest_file
                .project
                .description
                .unwrap_or_default(),
            authors: project_manifest_file.project.authors.unwrap_or_default(),
            telemetry: project_manifest_file.project.telemetry.unwrap_or(false),
            cache_location,
            boot_contracts,
            override_boot_contracts_source,
        };

        let mut config = ProjectManifest {
            project,
            contracts: BTreeMap::new(),
            repl_settings,
            root_dir: root_path.to_path_buf(),
            location: manifest_location.to_path_buf(),
            contracts_settings: HashMap::new(),
            diagnostics,
        };
        let mut config_contracts = BTreeMap::new();
        let mut contracts_settings = HashMap::new();
        let mut config_requirements: Vec<RequirementConfig> = Vec::new();

        if let Some(TomlValue::Array(requirements)) = project_manifest_file.project.requirements {
            for link_settings in requirements.iter() {
                if let TomlValue::Table(link_settings) = link_settings {
                    let contract_id = match link_settings.get("contract_id") {
                        Some(TomlValue::String(contract_id)) => contract_id.to_string(),
                        _ => continue,
                    };
                    config_requirements.push(RequirementConfig { contract_id });
                }
            }
        };

        if let Some(TomlValue::Table(contracts)) = project_manifest_file.contracts {
            for (contract_name, contract_settings) in contracts.iter() {
                if let TomlValue::Table(contract_settings) = contract_settings {
                    let Some(TomlValue::String(contract_path)) = contract_settings.get("path")
                    else {
                        continue;
                    };

                    let deployer = match contract_settings.get("deployer") {
                        Some(TomlValue::String(path)) => {
                            ContractDeployer::LabeledDeployer(path.clone())
                        }
                        _ => ContractDeployer::DefaultDeployer,
                    };

                    let parsed_epoch = match contract_settings.get("epoch") {
                        None => None,
                        Some(TomlValue::String(epoch)) => Some(epoch.clone()),
                        Some(TomlValue::Float(epoch)) => Some(epoch.to_string()),
                        Some(value) => {
                            return Err(format!(
                                "epoch must be a string in toml config (got {value})"
                            ));
                        }
                    };

                    let parsed_clarity_version = match contract_settings.get("clarity_version") {
                        Some(TomlValue::Integer(clarity_version)) => {
                            Some(clarity_version.to_string())
                        }
                        None => None,
                        _ => return Err(INVALID_CLARITY_VERSION.into()),
                    };

                    ProjectManifest::register_contract(
                        contract_name,
                        contract_path,
                        deployer,
                        parsed_epoch.as_deref(),
                        parsed_clarity_version.as_deref(),
                        root_path,
                        &mut config_contracts,
                        &mut contracts_settings,
                    )?;
                }
            }
        };

        config.contracts = config_contracts;
        config.contracts_settings = contracts_settings;
        config.project.requirements = Some(config_requirements);
        Ok(config)
    }

    pub fn use_mainnet_wallets(&self) -> bool {
        self.repl_settings.remote_data.enabled && self.repl_settings.remote_data.use_mainnet_wallets
    }
}

fn get_epoch_and_clarity_version(
    settings_epoch: Option<&str>,
    settings_clarity_version: Option<&str>,
) -> Result<(clarity_repl::repl::Epoch, ClarityVersion), String> {
    // if neither epoch or version are specified in clarinet.toml use: epoch 2.05 and clarity 1
    // if epoch is specified but not version: use the default version for that epoch

    let epoch = match settings_epoch {
        None => clarity_repl::repl::Epoch::Specific(StacksEpochId::Epoch2_05), // Keep the existing default unchanged
        // The "latest" epoch means the current epoch that is running on mainnet
        // Which can differ from `StacksEpochId::latest()` if it hasn't been activated yet
        Some("latest") => clarity_repl::repl::Epoch::Latest,
        Some(epoch) => {
            let stacks_epoch = clarity_repl::repl::epoch_from_str(epoch).ok_or(
                    "epoch field invalid (value supported: 2.0, 2.05, 2.1, 2.2, 2.3, 2.4, 2.5, 3.0, 3.1, 3.2, 3.3, 3.4, 4.0, latest)",
                )?;
            clarity_repl::repl::Epoch::Specific(stacks_epoch)
        }
    };

    let clarity_version = match settings_clarity_version {
        None => match settings_epoch {
            None => ClarityVersion::Clarity1,
            Some(_) => ClarityVersion::default_for_epoch(epoch.resolve()),
        },
        Some(version) => ClarityVersion::from_str(&format!("clarity{version}"))
            .map_err(|_| INVALID_CLARITY_VERSION)?,
    };

    if clarity_version > ClarityVersion::default_for_epoch(epoch.resolve()) {
        return Err(format!("{clarity_version} can not be used with {epoch}"));
    }

    Ok((epoch, clarity_version))
}

#[cfg(test)]
mod tests {
    use toml::toml;

    use super::*;

    #[test]
    fn test_allow_remote_data_fetching() {
        let manifest_toml = toml! {
            [project]
            name = "pyth-client"
            telemetry = false
            [repl.remote_data]
            enabled = true
        };
        let manifest_file: ProjectManifestFile = manifest_toml.clone().try_into().unwrap();
        let location = PathBuf::from("/tmp/clarinet.toml");

        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, true).unwrap();
        assert!(manifest.repl_settings.remote_data.enabled);

        let manifest_file: ProjectManifestFile = manifest_toml.try_into().unwrap();
        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap();
        assert!(!manifest.repl_settings.remote_data.enabled);
    }

    #[test]
    fn test_linter_settings() {
        use clarity::vm::diagnostic::Level;
        use clarity_repl::analysis::linter::LintName;

        let manifest_toml = toml! {
            [project]
            name = "linter"
            [linter.groups]
            correctness = "off"
            [linter.lints]
            unused_const = "error"
        };
        let manifest_file: ProjectManifestFile = manifest_toml.try_into().unwrap();
        let location = PathBuf::from("/tmp/clarinet.toml");
        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap();

        let lints = manifest.repl_settings.analysis.lints();
        assert_eq!(lints.get(&LintName::UnusedConst), Some(&Level::Error));
        // Correctness lints ignore config.
        assert_eq!(lints.get(&LintName::CallChecker), Some(&Level::Error));
    }

    #[test]
    fn test_removed_repl_analysis_is_ignored() {
        use clarity_repl::analysis::linter::LintName;

        let manifest_toml = toml! {
            [project]
            name = "legacy"
            [repl.analysis]
            passes = ["check_checker"]
            [repl.analysis.lint_groups]
            all = false
        };
        let manifest_file: ProjectManifestFile = manifest_toml.try_into().unwrap();
        let location = PathBuf::from("/tmp/clarinet.toml");
        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap();

        assert!(manifest
            .repl_settings
            .analysis
            .lints()
            .contains_key(&LintName::CallChecker));
    }

    #[test]
    fn test_removed_analysis_keys_in_linter_are_rejected() {
        for (key, value, expected) in [
            ("lint_groups", "{ all = false }", "use `linter.groups`"),
            ("passes", r#"["check_checker"]"#, "`linter.passes`"),
            (
                "check_checker",
                "{ strict = false }",
                "check-checker was removed",
            ),
        ] {
            let manifest = format!("[project]\nname = \"legacy\"\n[linter]\n{key} = {value}\n");
            let err = toml::from_str::<ProjectManifestFile>(&manifest)
                .expect_err(&format!("`linter.{key}` should be rejected"))
                .to_string();
            assert!(err.contains(expected), "unexpected error for {key}: {err}");
        }
    }

    #[test]
    fn test_get_epoch_and_clarity_version() {
        use clarity::types::StacksEpochId::*;
        use clarity::vm::ClarityVersion::*;
        use clarity_repl::repl::Epoch::*;

        // no epoch, no version
        let result = get_epoch_and_clarity_version(None, None);
        assert_eq!(result, Ok((Specific(Epoch2_05), Clarity1)));

        // epoch 2.0, no version
        let result = get_epoch_and_clarity_version(Some("2.0"), None);
        assert_eq!(result, Ok((Specific(Epoch20), Clarity1)));

        // epoch 2.05, no version
        let result = get_epoch_and_clarity_version(Some("2.05"), None);
        assert_eq!(result, Ok((Specific(Epoch2_05), Clarity1)));

        // epoch 2.1, no version
        let result = get_epoch_and_clarity_version(Some("2.1"), None);
        assert_eq!(result, Ok((Specific(Epoch21), Clarity2)));

        // epoch 3.0, no version
        let result = get_epoch_and_clarity_version(Some("3.0"), None);
        assert_eq!(result, Ok((Specific(Epoch30), Clarity3)));

        // epoch 3.1, no version
        let result = get_epoch_and_clarity_version(Some("3.1"), None);
        assert_eq!(result, Ok((Specific(Epoch31), Clarity3)));

        // no epoch, version 1
        let result = get_epoch_and_clarity_version(None, Some("1"));
        assert_eq!(result, Ok((Specific(Epoch2_05), Clarity1)));

        // no epoch, version 2 -> error, must specify epoch
        let result = get_epoch_and_clarity_version(None, Some("2"));
        assert_eq!(result, Err("Clarity 2 can not be used with 2.05".into()));

        // epoch and clarity version
        // no epoch 2.05, version 1
        let result = get_epoch_and_clarity_version(Some("2.05"), Some("1"));
        assert_eq!(result, Ok((Specific(Epoch2_05), Clarity1)));

        // no epoch 2.05, version 2 -> error
        let result = get_epoch_and_clarity_version(Some("2.05"), Some("2"));
        assert_eq!(result, Err("Clarity 2 can not be used with 2.05".into()));

        // no epoch 2.05, version 1
        let result = get_epoch_and_clarity_version(Some("2.1"), Some("1"));
        assert_eq!(result, Ok((Specific(Epoch21), Clarity1)));

        // no epoch 2.05, version 2 -> error
        let result = get_epoch_and_clarity_version(Some("2.1"), Some("2"));
        assert_eq!(result, Ok((Specific(Epoch21), Clarity2)));
    }

    #[test]
    fn test_latest_epoch_and_clarity_version() {
        let expected_current_clarity_version = ClarityVersion::Clarity3;
        let result = get_epoch_and_clarity_version(
            Some("latest"),
            // Replace "Clarity N" to "N" for clarity version
            Some(
                &expected_current_clarity_version
                    .to_string()
                    .replace("Clarity ", ""),
            ),
        );
        assert_eq!(
            result,
            Ok((
                clarity_repl::repl::Epoch::Latest,
                expected_current_clarity_version
            ))
        );
    }

    #[test]
    fn test_epoch_latest_preservation() {
        let manifest_toml = toml! {
            [project]
            name = "test-project"
            telemetry = false
            [contracts.test-contract]
            path = "contracts/test.clar"
            clarity_version = 2
            epoch = "latest"
        };

        let manifest_file: ProjectManifestFile = manifest_toml.try_into().unwrap();
        let location = PathBuf::from("/tmp/clarinet.toml");
        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, true).unwrap();
        let contract = manifest.contracts.get("test-contract").unwrap();
        assert!(matches!(contract.epoch, clarity_repl::repl::Epoch::Latest));
        let toml_value = toml::Value::try_from(contract).unwrap();
        let pretty_toml = toml::ser::to_string_pretty(&toml_value).unwrap();
        assert!(pretty_toml.contains("epoch = 'latest'"));
    }

    #[test]
    fn test_override_boot_contracts_source_parsing() {
        let manifest_toml = toml! {
            [project]
            name = "test-project"
            telemetry = false
            [project.override_boot_contracts_source]
            "pox-4" = "./custom-boot-contracts/pox-4.clar"
            "costs" = "./custom-boot-contracts/costs.clar"
        };
        let manifest_file: ProjectManifestFile = manifest_toml.try_into().unwrap();
        let location = PathBuf::from("/tmp/clarinet.toml");

        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap();

        assert_eq!(manifest.project.override_boot_contracts_source.len(), 2);
        assert_eq!(
            manifest
                .project
                .override_boot_contracts_source
                .get("pox-4")
                .map(|e| e.path.as_str()),
            Some("./custom-boot-contracts/pox-4.clar")
        );
        assert_eq!(
            manifest
                .project
                .override_boot_contracts_source
                .get("costs")
                .map(|e| e.path.as_str()),
            Some("./custom-boot-contracts/costs.clar")
        );
    }

    #[test]
    fn test_override_boot_contracts_source_with_invalid_contract() {
        let manifest_toml = toml! {
            [project]
            name = "test-project"
            telemetry = false
            [project.override_boot_contracts_source]
            "pox-4" = "./custom-boot-contracts/pox-4.clar"
            "pox-x" = "./custom-boot-contracts/pox-x.clar"
            "costs" = "./custom-boot-contracts/costs.clar"
        };
        let manifest_file: ProjectManifestFile = manifest_toml.try_into().unwrap();
        let location = PathBuf::from("/tmp/clarinet.toml");

        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap();

        // Only valid contracts should be included
        assert_eq!(manifest.project.override_boot_contracts_source.len(), 2);
        assert_eq!(
            manifest
                .project
                .override_boot_contracts_source
                .get("pox-4")
                .map(|e| e.path.as_str()),
            Some("./custom-boot-contracts/pox-4.clar")
        );
        assert_eq!(
            manifest
                .project
                .override_boot_contracts_source
                .get("costs")
                .map(|e| e.path.as_str()),
            Some("./custom-boot-contracts/costs.clar")
        );
        // Invalid contract should not be included
        assert!(!manifest
            .project
            .override_boot_contracts_source
            .contains_key("pox-x"));
    }

    #[test]
    fn test_warning_message_for_invalid_boot_contract() {
        let manifest_toml = toml! {
            [project]
            name = "test-project"
            telemetry = false
            [project.override_boot_contracts_source]
            "pox-x" = "./custom-boot-contracts/pox-x.clar"
        };
        let manifest_file: ProjectManifestFile = manifest_toml.try_into().unwrap();
        let location = PathBuf::from("/tmp/clarinet.toml");

        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap();

        // Verify that the invalid contract was filtered out
        assert_eq!(manifest.project.override_boot_contracts_source.len(), 0);
        assert!(!manifest
            .project
            .override_boot_contracts_source
            .contains_key("pox-x"));
    }
}
