use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::str::FromStr;

use clarity::types::StacksEpochId;
use clarity::vm::ClarityVersion;
use clarity_repl::repl;
use clarity_repl::repl::boot::BOOT_CONTRACTS_NAMES;
use clarity_repl::repl::{ClarityCodeSource, ClarityContract, ContractDeployer};
#[cfg(feature = "json_schema")]
use schemars::JsonSchema;
use serde::ser::SerializeMap;
use serde::{Deserializer, Serialize, Serializer};
use serde_json::Value as JsonValue;
use toml::Value as TomlValue;

use super::FileLocation;
#[cfg(feature = "json_schema")]
use crate::schema;
use crate::FileAccessor;

pub const INVALID_CLARITY_VERSION: &str =
    "clarity_version field invalid (value supported: 1, 2, 3)";

#[derive(Deserialize, Debug, Clone)]
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

    // The fields below have been moved into repl above, but are kept here for
    // backwards compatibility.
    #[cfg_attr(feature = "json_schema", schemars(skip))]
    analysis: Option<Vec<clarity_repl::analysis::Pass>>,
    /// Directory for caching build artifacts
    #[serde(default)]
    cache_dir: Option<String>,
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
    /// REPL and analysis settings
    #[serde(default)]
    #[cfg_attr(feature = "json_schema", schemars(schema_with = "schema::repl_schema"))]
    repl: Option<repl::SettingsFile>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct ProjectManifest {
    pub project: ProjectConfig,
    #[serde(serialize_with = "toml::ser::tables_last")]
    #[serde(deserialize_with = "contracts_deserializer")]
    pub contracts: BTreeMap<String, ClarityContract>,
    #[serde(rename = "repl")]
    pub repl_settings: repl::Settings,
    #[serde(skip_serializing)]
    #[serde(default = "default_location")]
    pub location: FileLocation,
    #[serde(skip_serializing, skip_deserializing)]
    pub contracts_settings: HashMap<FileLocation, ClarityContractMetadata>,
}

fn default_location() -> FileLocation {
    let path = std::env::temp_dir();
    FileLocation::from_path(path)
}

fn contracts_deserializer<'de, D>(des: D) -> Result<BTreeMap<String, ClarityContract>, D::Error>
where
    D: Deserializer<'de>,
{
    let mut map: BTreeMap<String, ClarityContract> = BTreeMap::new();

    let container: HashMap<String, HashMap<String, JsonValue>> =
        serde::Deserialize::deserialize(des)?;

    for (contract_name, contract_settings) in container {
        let Some(JsonValue::String(contract_path)) = contract_settings.get("path") else {
            continue;
        };

        let code_source = match PathBuf::from_str(contract_path) {
            Ok(path) => ClarityCodeSource::ContractOnDisk(path),
            Err(e) => {
                return Err(serde::de::Error::custom(format!(
                    "unable to parse path {contract_path} ({e})"
                )))
            }
        };

        let deployer = match contract_settings.get("deployer") {
            Some(JsonValue::String(path)) => ContractDeployer::LabeledDeployer(path.clone()),
            _ => ContractDeployer::DefaultDeployer,
        };

        let parsed_settings_epoch = match contract_settings.get("epoch") {
            None => None,
            Some(JsonValue::String(epoch)) => Some(epoch.as_str()),
            Some(JsonValue::Number(epoch)) => Some(epoch.as_str()),
            Some(value) => {
                return Err(serde::de::Error::invalid_type(
                    serde::de::Unexpected::Other(&format!("{value}")),
                    // we try to parse the epoch as a string or number, but it's recommended to use a string
                    &"epoch must be a string in json config",
                ));
            }
        };

        let parsed_clarity_version = match contract_settings.get("clarity_version") {
            None => None,
            Some(JsonValue::Number(version)) => Some(version.as_str()),
            _ => return Err(serde::de::Error::custom(INVALID_CLARITY_VERSION)),
        };

        let (epoch, clarity_version) =
            get_epoch_and_clarity_version(parsed_settings_epoch, parsed_clarity_version)
                .map_err(serde::de::Error::custom)?;

        let cc = ClarityContract {
            code_source,
            name: contract_name.clone(),
            deployer,
            clarity_version,
            epoch,
        };

        map.insert(contract_name, cc);
    }
    Ok(map)
}

#[derive(Deserialize, Debug, Clone)]
pub struct ProjectConfig {
    pub name: String,
    pub authors: Vec<String>,
    pub description: String,
    pub telemetry: bool,
    pub requirements: Option<Vec<RequirementConfig>>,
    #[serde(rename = "cache_dir")]
    #[serde(deserialize_with = "cache_location_deserializer")]
    pub cache_location: FileLocation,
    #[serde(skip_deserializing)]
    pub boot_contracts: Vec<String>,
    pub override_boot_contracts_source: BTreeMap<String, String>,
}

fn cache_location_deserializer<'de, D>(des: D) -> Result<FileLocation, D::Error>
where
    D: Deserializer<'de>,
{
    let container: String = serde::Deserialize::deserialize(des)?;
    FileLocation::from_path_string(&container).map_err(serde::de::Error::custom)
}

impl Serialize for ProjectConfig {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("name", &self.name)?;
        map.serialize_entry("description", &self.description)?;
        map.serialize_entry("authors", &self.authors)?;
        map.serialize_entry("telemetry", &self.telemetry)?;
        map.serialize_entry(
            "cache_dir",
            &self
                .cache_location
                .get_relative_location()
                .unwrap_or(self.cache_location.to_string()),
        )?;
        if self.requirements.is_some() {
            map.serialize_entry("requirements", &self.requirements)?;
        }
        map.end()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Default)]
#[cfg_attr(feature = "json_schema", derive(JsonSchema))]
pub struct RequirementConfig {
    /// Contract identifier (e.g., SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait)
    pub contract_id: String,
}

impl ProjectManifest {
    pub async fn from_file_accessor(
        location: &FileLocation,
        allow_remote_data_fetching: bool,
        file_accessor: &dyn FileAccessor,
    ) -> Result<ProjectManifest, String> {
        let content = file_accessor.read_file(location.to_string()).await?;

        let project_manifest_file: ProjectManifestFile = match toml::from_slice(content.as_bytes())
        {
            Ok(s) => s,
            Err(e) => {
                return Err(format!("Clarinet.toml file malformatted {e:?}"));
            }
        };
        ProjectManifest::from_project_manifest_file(
            project_manifest_file,
            location,
            allow_remote_data_fetching,
        )
    }

    pub fn from_location(
        location: &FileLocation,
        allow_remote_data_fetching: bool,
    ) -> Result<ProjectManifest, String> {
        let project_manifest_file_content = location.read_content()?;
        let project_manifest_file: ProjectManifestFile =
            match toml::from_slice(&project_manifest_file_content[..]) {
                Ok(s) => s,
                Err(e) => {
                    return Err(format!("Clarinet.toml file malformatted {e:?}"));
                }
            };

        ProjectManifest::from_project_manifest_file(
            project_manifest_file,
            location,
            allow_remote_data_fetching,
        )
    }

    // process a contract and add it to the configuration
    fn register_contract(
        contract_name: &str,
        contract_path: &str,
        deployer: ContractDeployer,
        epoch: Option<&str>,
        clarity_version: Option<&str>,
        project_root_location: &FileLocation,
        config_contracts: &mut BTreeMap<String, ClarityContract>,
        contracts_settings: &mut HashMap<FileLocation, ClarityContractMetadata>,
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
            },
        );

        let mut contract_location = project_root_location.clone();
        contract_location.append_path(contract_path)?;
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
        manifest_location: &FileLocation,
        allow_remote_data_fetching: bool,
    ) -> Result<ProjectManifest, String> {
        let mut repl_settings = if let Some(repl_settings) = project_manifest_file.repl {
            repl::Settings::from(repl_settings)
        } else {
            repl::Settings::default()
        };

        // when performing static analysis, we never want to enable remote data fetching
        if !allow_remote_data_fetching {
            repl_settings.remote_data.enabled = false;
        }

        // Check for deprecated settings
        if let Some(passes) = project_manifest_file.project.analysis {
            repl_settings.analysis.set_passes(passes);
        }

        let project_name = project_manifest_file.project.name;
        let project_root_location = manifest_location.get_parent_location()?;
        let cache_location = match project_manifest_file.project.cache_dir {
            Some(ref path) => FileLocation::try_parse(path, Some(&project_root_location))
                .ok_or_else(|| format!("unable to parse path {path}"))?,
            None => {
                let mut cache_location = project_root_location.clone();
                cache_location.append_path(".cache")?;
                cache_location
            }
        };

        let mut override_boot_contracts_source = BTreeMap::new();
        if let Some(overrides) = project_manifest_file.project.override_boot_contracts_source {
            for (contract_name, contract_path) in overrides.iter() {
                override_boot_contracts_source.insert(contract_name.clone(), contract_path.clone());
            }
        }

        let boot_contracts = BOOT_CONTRACTS_NAMES
            .iter()
            .map(|n| n.to_string())
            .collect::<Vec<_>>();

        // if an override doesn't correspond with one of the boot contracts, we warn and discard that override
        let mut valid_override_boot_contracts_source = BTreeMap::new();
        for (contract_name, contract_path) in override_boot_contracts_source.iter() {
            if !boot_contracts.contains(contract_name) {
                eprintln!("Warning: {contract_name} custom boot contract was not included because it's not part of the set of default boot contracts");
                eprintln!("Available boot contracts: {boot_contracts:?}");
            } else {
                valid_override_boot_contracts_source
                    .insert(contract_name.clone(), contract_path.clone());
            }
        }

        let project = ProjectConfig {
            name: project_name,
            requirements: None,
            description: project_manifest_file
                .project
                .description
                .unwrap_or("".into()),
            authors: project_manifest_file.project.authors.unwrap_or_default(),
            telemetry: project_manifest_file.project.telemetry.unwrap_or(false),
            cache_location,
            boot_contracts,
            override_boot_contracts_source: valid_override_boot_contracts_source,
        };

        let mut config = ProjectManifest {
            project,
            contracts: BTreeMap::new(),
            repl_settings,
            location: manifest_location.clone(),
            contracts_settings: HashMap::new(),
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
                        &project_root_location,
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
            let stacks_epoch = match epoch {
                "2" | "2.0" => StacksEpochId::Epoch20,
                "2.05" => StacksEpochId::Epoch2_05,
                "2.1" => StacksEpochId::Epoch21,
                "2.2" => StacksEpochId::Epoch22,
                "2.3" => StacksEpochId::Epoch23,
                "2.4" => StacksEpochId::Epoch24,
                "2.5" => StacksEpochId::Epoch25,
                "3" | "3.0" => StacksEpochId::Epoch30,
                "3.1" => StacksEpochId::Epoch31,
                "3.2" => StacksEpochId::Epoch32,
                "3.3" => StacksEpochId::Epoch33,
                _ => return Err(
                    "epoch field invalid (value supported: 2.0, 2.05, 2.1, 2.2, 2.3, 2.4, 3.0, 3.1, 3.2, 3.3 latest)"
                        .into(),
                ),
            };
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
        let location = FileLocation::from_path("/tmp/clarinet.toml".into());

        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, true).unwrap();
        assert!(manifest.repl_settings.remote_data.enabled);

        let manifest_file: ProjectManifestFile = manifest_toml.try_into().unwrap();
        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap();
        assert!(!manifest.repl_settings.remote_data.enabled);
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

        // no epoch
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
        let location = FileLocation::from_path("/tmp/clarinet.toml".into());
        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, true).unwrap();
        let contract = manifest.contracts.get("test-contract").unwrap();
        assert!(matches!(contract.epoch, clarity_repl::repl::Epoch::Latest));
        let toml_value = toml::Value::try_from(&manifest).unwrap();
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
        let manifest_file: ProjectManifestFile = manifest_toml.clone().try_into().unwrap();
        let location = FileLocation::from_path("/tmp/clarinet.toml".into());

        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap();

        assert_eq!(manifest.project.override_boot_contracts_source.len(), 2);
        assert_eq!(
            manifest.project.override_boot_contracts_source.get("pox-4"),
            Some(&"./custom-boot-contracts/pox-4.clar".to_string())
        );
        assert_eq!(
            manifest.project.override_boot_contracts_source.get("costs"),
            Some(&"./custom-boot-contracts/costs.clar".to_string())
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
        let manifest_file: ProjectManifestFile = manifest_toml.clone().try_into().unwrap();
        let location = FileLocation::from_path("/tmp/clarinet.toml".into());

        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap();

        // Only valid contracts should be included
        assert_eq!(manifest.project.override_boot_contracts_source.len(), 2);
        assert_eq!(
            manifest.project.override_boot_contracts_source.get("pox-4"),
            Some(&"./custom-boot-contracts/pox-4.clar".to_string())
        );
        assert_eq!(
            manifest.project.override_boot_contracts_source.get("costs"),
            Some(&"./custom-boot-contracts/costs.clar".to_string())
        );
        // Invalid contract should not be included
        assert_eq!(
            manifest.project.override_boot_contracts_source.get("pox-x"),
            None
        );
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
        let manifest_file: ProjectManifestFile = manifest_toml.clone().try_into().unwrap();
        let location = FileLocation::from_path("/tmp/clarinet.toml".into());

        let manifest =
            ProjectManifest::from_project_manifest_file(manifest_file, &location, false).unwrap();

        // Verify that the invalid contract was filtered out
        assert_eq!(manifest.project.override_boot_contracts_source.len(), 0);
        assert_eq!(
            manifest.project.override_boot_contracts_source.get("pox-x"),
            None
        );
    }
}
