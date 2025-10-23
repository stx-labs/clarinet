pub mod boot;
pub mod clarity_values;
pub mod datastore;
pub mod diagnostic;
#[cfg(not(target_arch = "wasm32"))]
mod docs;
pub mod hooks;
pub mod interpreter;
pub mod remote_data;
pub mod session;
pub mod settings;

#[cfg(any(not(target_arch = "wasm32"), feature = "dap"))]
pub mod debug;

use std::convert::TryInto;
use std::fmt::Display;
use std::path::PathBuf;

use clarity::types::StacksEpochId;
use clarity::vm::ClarityVersion;
use clarity_types::types::{PrincipalData, QualifiedContractIdentifier, StandardPrincipalData};
pub use interpreter::ClarityInterpreter;
use serde::ser::{Serialize, SerializeMap, Serializer};
pub use session::Session;
pub use settings::{SessionSettings, Settings, SettingsFile};

pub const DEFAULT_CLARITY_VERSION: ClarityVersion = ClarityVersion::Clarity3;
pub const DEFAULT_EPOCH: StacksEpochId = StacksEpochId::Epoch32;

#[derive(Debug, Clone, PartialEq)]
pub enum Epoch {
    Specific(StacksEpochId),
    Latest,
}

impl PartialEq<StacksEpochId> for Epoch {
    fn eq(&self, other: &StacksEpochId) -> bool {
        match self {
            Epoch::Specific(epoch) => epoch == other,
            Epoch::Latest => &DEFAULT_EPOCH == other,
        }
    }
}

impl Serialize for Epoch {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Epoch::Specific(epoch) => serializer.serialize_str(&format!("{epoch}")),
            Epoch::Latest => serializer.serialize_str("latest"),
        }
    }
}

impl<'de> serde::Deserialize<'de> for Epoch {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct EpochVisitor;

        impl<'de> serde::de::Visitor<'de> for EpochVisitor {
            type Value = Epoch;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a string or StacksEpochId")
            }

            fn visit_str<E>(self, value: &str) -> Result<Epoch, E>
            where
                E: serde::de::Error,
            {
                if value == "latest" {
                    Ok(Epoch::Latest)
                } else {
                    Err(serde::de::Error::custom(format!(
                        "unknown epoch value: {value}"
                    )))
                }
            }

            fn visit_f64<E>(self, value: f64) -> Result<Epoch, E>
            where
                E: serde::de::Error,
            {
                // Handle numeric epoch values by converting to StacksEpochId
                let epoch = match value {
                    1.0 => StacksEpochId::Epoch10,
                    2.0 => StacksEpochId::Epoch20,
                    2.05 => StacksEpochId::Epoch2_05,
                    2.1 => StacksEpochId::Epoch21,
                    2.2 => StacksEpochId::Epoch22,
                    2.3 => StacksEpochId::Epoch23,
                    2.4 => StacksEpochId::Epoch24,
                    2.5 => StacksEpochId::Epoch25,
                    3.0 => StacksEpochId::Epoch30,
                    3.1 => StacksEpochId::Epoch31,
                    _ => {
                        return Err(serde::de::Error::custom(format!(
                            "unknown epoch value: {value}"
                        )))
                    }
                };
                Ok(Epoch::Specific(epoch))
            }
        }

        deserializer.deserialize_any(EpochVisitor)
    }
}

impl Epoch {
    pub fn resolve(&self) -> StacksEpochId {
        match self {
            Epoch::Specific(epoch) => *epoch,
            Epoch::Latest => DEFAULT_EPOCH,
        }
    }
}

impl Display for Epoch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Epoch::Specific(epoch) => write!(f, "{epoch}"),
            Epoch::Latest => write!(f, "latest"),
        }
    }
}

#[derive(Deserialize, Debug, Clone)]
pub struct ClarityContract {
    pub code_source: ClarityCodeSource,
    pub name: String,
    pub deployer: ContractDeployer,
    pub clarity_version: ClarityVersion,
    pub epoch: Epoch,
}

impl Serialize for ClarityContract {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        match self.code_source {
            ClarityCodeSource::ContractOnDisk(ref path) => {
                map.serialize_entry("path", &format!("{}", path.display()))?;
            }
            _ => unreachable!(),
        }
        match self.deployer {
            ContractDeployer::LabeledDeployer(ref label) => {
                map.serialize_entry("deployer", &label)?;
            }
            ContractDeployer::DefaultDeployer => {}
            _ => unreachable!(),
        }
        match self.clarity_version {
            ClarityVersion::Clarity1 => {
                map.serialize_entry("clarity_version", &1)?;
            }
            ClarityVersion::Clarity2 => {
                map.serialize_entry("clarity_version", &2)?;
            }
            ClarityVersion::Clarity3 => {
                map.serialize_entry("clarity_version", &3)?;
            }
            ClarityVersion::Clarity4 => {
                map.serialize_entry("clarity_version", &4)?;
            }
        }
        map.serialize_entry("epoch", &self.epoch)?;
        map.end()
    }
}

impl Display for ClarityContract {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<Contract contract_id={}, clarity_version={}, epoch={}>",
            self.expect_resolved_contract_identifier(None),
            self.clarity_version,
            self.epoch
        )
    }
}

impl ClarityContract {
    pub fn expect_in_memory_code_source(&self) -> &str {
        match self.code_source {
            ClarityCodeSource::ContractInMemory(ref code_source) => code_source.as_str(),
            _ => panic!("source code expected to be in memory"),
        }
    }

    pub fn expect_contract_path_as_str(&self) -> &str {
        match self.code_source {
            ClarityCodeSource::ContractOnDisk(ref path) => path.to_str().unwrap(),
            _ => panic!("source code expected to be in memory"),
        }
    }

    pub fn expect_resolved_contract_identifier(
        &self,
        default_deployer: Option<&StandardPrincipalData>,
    ) -> QualifiedContractIdentifier {
        let deployer = match &self.deployer {
            ContractDeployer::ContractIdentifier(contract_identifier) => {
                return contract_identifier.clone()
            }
            ContractDeployer::Transient => StandardPrincipalData::transient(),
            ContractDeployer::Address(address) => {
                PrincipalData::parse_standard_principal(address).expect("unable to parse address")
            }
            ContractDeployer::DefaultDeployer => default_deployer
                .expect("default provider should have been provided")
                .clone(),
            _ => panic!("deployer expected to be resolved"),
        };
        let contract_name = self
            .name
            .clone()
            .try_into()
            .expect("unable to parse contract name");
        QualifiedContractIdentifier::new(deployer, contract_name)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum ContractDeployer {
    Transient,
    DefaultDeployer,
    LabeledDeployer(String),
    Address(String),
    ContractIdentifier(QualifiedContractIdentifier),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum ClarityCodeSource {
    ContractInMemory(String),
    ContractOnDisk(PathBuf),
    Empty,
}
