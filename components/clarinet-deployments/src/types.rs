use std::collections::{BTreeMap, HashMap};

use clarinet_files::{DevnetConfig, FileAccessor, FileLocation, StacksNetwork};
use clarity_repl::analysis::ast_dependency_detector::DependencySet;
use clarity_repl::clarity::util::hash::{hex_bytes, to_hex};
use clarity_repl::clarity::vm::analysis::ContractAnalysis;
use clarity_repl::clarity::vm::ast::ContractAST;
use clarity_repl::clarity::vm::diagnostic::Diagnostic;
use clarity_repl::clarity::vm::types::{
    PrincipalData, QualifiedContractIdentifier, StandardPrincipalData,
};
use clarity_repl::clarity::{ClarityName, ClarityVersion, ContractName, StacksEpochId, Value};
use clarity_repl::repl::{Session, DEFAULT_CLARITY_VERSION};
use serde::{Deserialize, Serialize};
use serde_yaml;
use strum::{EnumIter, IntoEnumIterator};

#[derive(Serialize, Deserialize, PartialEq, Debug, Clone, Copy, Eq, PartialOrd, Ord, EnumIter)]
pub enum EpochSpec {
    #[serde(rename = "2.0")]
    Epoch2_0,
    #[serde(rename = "2.05")]
    Epoch2_05,
    #[serde(rename = "2.1")]
    Epoch2_1,
    #[serde(rename = "2.2")]
    Epoch2_2,
    #[serde(rename = "2.3")]
    Epoch2_3,
    #[serde(rename = "2.4")]
    Epoch2_4,
    #[serde(rename = "2.5")]
    Epoch2_5,
    #[serde(rename = "3.0")]
    Epoch3_0,
    #[serde(rename = "3.1")]
    Epoch3_1,
    #[serde(rename = "3.2")]
    Epoch3_2,
    #[serde(rename = "3.3")]
    Epoch3_3,
}

impl From<StacksEpochId> for EpochSpec {
    fn from(epoch: StacksEpochId) -> Self {
        match epoch {
            StacksEpochId::Epoch20 => EpochSpec::Epoch2_0,
            StacksEpochId::Epoch2_05 => EpochSpec::Epoch2_05,
            StacksEpochId::Epoch21 => EpochSpec::Epoch2_1,
            StacksEpochId::Epoch22 => EpochSpec::Epoch2_2,
            StacksEpochId::Epoch23 => EpochSpec::Epoch2_3,
            StacksEpochId::Epoch24 => EpochSpec::Epoch2_4,
            StacksEpochId::Epoch25 => EpochSpec::Epoch2_5,
            StacksEpochId::Epoch30 => EpochSpec::Epoch3_0,
            StacksEpochId::Epoch31 => EpochSpec::Epoch3_1,
            StacksEpochId::Epoch32 => EpochSpec::Epoch3_2,
            StacksEpochId::Epoch33 => EpochSpec::Epoch3_3,
            StacksEpochId::Epoch10 => unreachable!("epoch 1.0 is not supported"),
        }
    }
}

impl From<EpochSpec> for StacksEpochId {
    fn from(val: EpochSpec) -> Self {
        match val {
            EpochSpec::Epoch2_0 => StacksEpochId::Epoch20,
            EpochSpec::Epoch2_05 => StacksEpochId::Epoch2_05,
            EpochSpec::Epoch2_1 => StacksEpochId::Epoch21,
            EpochSpec::Epoch2_2 => StacksEpochId::Epoch22,
            EpochSpec::Epoch2_3 => StacksEpochId::Epoch23,
            EpochSpec::Epoch2_4 => StacksEpochId::Epoch24,
            EpochSpec::Epoch2_5 => StacksEpochId::Epoch25,
            EpochSpec::Epoch3_0 => StacksEpochId::Epoch30,
            EpochSpec::Epoch3_1 => StacksEpochId::Epoch31,
            EpochSpec::Epoch3_2 => StacksEpochId::Epoch32,
            EpochSpec::Epoch3_3 => StacksEpochId::Epoch33,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct EpochConfig {
    epoch_name: EpochSpec,
    start_height: u64,
}

#[derive(Serialize, Deserialize, Debug)]
struct EpochsConfig {
    epochs: Vec<EpochConfig>,
}

#[derive(Serialize, Deserialize, Debug)]
pub struct BurnchainEpochConfig {
    burnchain: EpochsConfig,
}

impl From<&DevnetConfig> for BurnchainEpochConfig {
    fn from(config: &DevnetConfig) -> Self {
        let epochs: Vec<EpochConfig> = EpochSpec::iter()
            .filter_map(|epoch| {
                let start_height = match epoch {
                    EpochSpec::Epoch2_0 => Some(config.epoch_2_0),
                    EpochSpec::Epoch2_05 => Some(config.epoch_2_05),
                    EpochSpec::Epoch2_1 => Some(config.epoch_2_1),
                    EpochSpec::Epoch2_2 => Some(config.epoch_2_2),
                    EpochSpec::Epoch2_3 => Some(config.epoch_2_3),
                    EpochSpec::Epoch2_4 => Some(config.epoch_2_4),
                    EpochSpec::Epoch2_5 => Some(config.epoch_2_5),
                    EpochSpec::Epoch3_0 => Some(config.epoch_3_0),
                    EpochSpec::Epoch3_1 => Some(config.epoch_3_1),
                    EpochSpec::Epoch3_2 => Some(config.epoch_3_2),
                    // epoch 3.3 is optional while it's not enabled on the default image
                    EpochSpec::Epoch3_3 => config.epoch_3_3,
                };
                start_height.map(|start_height| EpochConfig {
                    epoch_name: epoch,
                    start_height,
                })
            })
            .collect();

        Self {
            burnchain: EpochsConfig { epochs },
        }
    }
}

fn try_clarity_version_from_option(value: Option<u8>) -> Result<ClarityVersion, String> {
    match value {
        Some(1) => Ok(ClarityVersion::Clarity1),
        Some(2) => Ok(ClarityVersion::Clarity2),
        Some(3) => Ok(ClarityVersion::Clarity3),
        Some(4) => Ok(ClarityVersion::Clarity4),
        Some(_) => {
            Err("unable to parse clarity_version (can either be '1', '2', '3', or '4'".to_string())
        }
        None => Ok(DEFAULT_CLARITY_VERSION),
    }
}

#[derive(Clone)]
pub struct DeploymentGenerationArtifacts {
    pub asts: BTreeMap<QualifiedContractIdentifier, ContractAST>,
    pub deps: BTreeMap<QualifiedContractIdentifier, DependencySet>,
    pub diags: HashMap<QualifiedContractIdentifier, Vec<Diagnostic>>,
    pub analysis: HashMap<QualifiedContractIdentifier, ContractAnalysis>,
    pub results_values: HashMap<QualifiedContractIdentifier, Option<Value>>,
    pub session: Session,
    pub success: bool,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct TransactionPlanSpecification {
    pub batches: Vec<TransactionsBatchSpecification>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct TransactionPlanSpecificationFile {
    pub batches: Vec<TransactionsBatchSpecificationFile>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct TransactionsBatchSpecificationFile {
    pub id: usize,
    pub transactions: Vec<TransactionSpecificationFile>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub epoch: Option<EpochSpec>,
}

impl TransactionsBatchSpecificationFile {
    pub fn remove_publish_transactions(&mut self) {
        self.transactions.retain(|transaction| {
            !matches!(
                transaction,
                TransactionSpecificationFile::RequirementPublish(_)
                    | TransactionSpecificationFile::ContractPublish(_)
                    | TransactionSpecificationFile::EmulatedContractPublish(_)
            )
        });
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum TransactionSpecificationFile {
    ContractCall(ContractCallSpecificationFile),
    ContractPublish(ContractPublishSpecificationFile),
    EmulatedContractCall(EmulatedContractCallSpecificationFile),
    EmulatedContractPublish(EmulatedContractPublishSpecificationFile),
    RequirementPublish(RequirementPublishSpecificationFile),
    BtcTransfer(BtcTransferSpecificationFile),
    StxTransfer(StxTransferSpecificationFile),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct StxTransferSpecificationFile {
    pub expected_sender: String,
    pub recipient: String,
    pub mstx_amount: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memo: Option<String>,
    pub cost: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anchor_block_only: Option<bool>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct BtcTransferSpecificationFile {
    pub expected_sender: String,
    pub recipient: String,
    pub sats_amount: u64,
    pub sats_per_byte: u64,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ContractCallSpecificationFile {
    pub contract_id: String,
    pub expected_sender: String,
    pub method: String,
    pub parameters: Vec<String>,
    pub cost: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anchor_block_only: Option<bool>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct RequirementPublishSpecificationFile {
    pub contract_id: String,
    pub remap_sender: String,
    pub remap_principals: Option<BTreeMap<String, String>>,
    pub cost: u64,
    #[serde(flatten)]
    pub location: Option<FileLocation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub clarity_version: Option<u8>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ContractPublishSpecificationFile {
    pub contract_name: String,
    pub expected_sender: String,
    pub cost: u64,
    #[serde(flatten)]
    pub location: Option<FileLocation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anchor_block_only: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub clarity_version: Option<u8>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct EmulatedContractCallSpecificationFile {
    pub contract_id: String,
    pub emulated_sender: String,
    pub method: String,
    pub parameters: Vec<String>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct EmulatedContractPublishSpecificationFile {
    pub contract_name: String,
    pub emulated_sender: String,
    #[serde(flatten)]
    pub location: Option<FileLocation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub clarity_version: Option<u8>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct TransactionsBatchSpecification {
    pub id: usize,
    pub transactions: Vec<TransactionSpecification>,
    pub epoch: Option<EpochSpec>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
#[serde(tag = "transaction_type")]
pub enum TransactionSpecification {
    ContractCall(ContractCallSpecification),
    ContractPublish(ContractPublishSpecification),
    RequirementPublish(RequirementPublishSpecification),
    EmulatedContractCall(EmulatedContractCallSpecification),
    EmulatedContractPublish(EmulatedContractPublishSpecification),
    BtcTransfer(BtcTransferSpecification),
    StxTransfer(StxTransferSpecification),
}

type Memo = [u8; 34];

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct StxTransferSpecification {
    #[serde(with = "standard_principal_data_serde")]
    pub expected_sender: StandardPrincipalData,
    #[serde(with = "principal_data_serde")]
    pub recipient: PrincipalData,
    pub mstx_amount: u64,
    #[serde(with = "memo_serde")]
    pub memo: Memo,
    pub cost: u64,
    pub anchor_block_only: bool,
}

pub mod memo_serde {
    use std::fmt::Write;

    use clarity_repl::clarity::util::hash::hex_bytes;
    use serde::{Deserialize, Deserializer, Serializer};

    use super::Memo;
    pub fn serialize<S>(bytes: &Memo, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut str = String::with_capacity((bytes.len() * 2) + 1);
        write!(&mut str, "0x").unwrap();
        for &b in bytes {
            write!(&mut str, "{b:02x}").unwrap();
        }
        s.serialize_str(&str)
    }
    pub fn deserialize<'de, D>(deserializer: D) -> Result<Memo, D::Error>
    where
        D: Deserializer<'de>,
    {
        let hex_memo = String::deserialize(deserializer).map_err(serde::de::Error::custom)?;

        let mut memo = [0u8; 34];

        if !hex_memo.is_empty() && !hex_memo.starts_with("0x") {
            return Err(serde::de::Error::custom(
                "unable to parse memo (up to 34 bytes, starting with '0x')",
            ));
        }
        match hex_bytes(&hex_memo[2..]) {
            Ok(ref mut bytes) => {
                bytes.resize(34, 0);
                memo.copy_from_slice(bytes);
            }
            Err(_) => {
                return Err(serde::de::Error::custom(
                    "unable to parse memo (up to 34 bytes)",
                ))
            }
        }
        Ok(memo)
    }
}
pub mod principal_data_serde {
    use clarity_repl::clarity::vm::types::PrincipalData;
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S>(x: &PrincipalData, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.serialize_str(&x.to_string())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<PrincipalData, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer).map_err(serde::de::Error::custom)?;
        PrincipalData::parse(&s).map_err(serde::de::Error::custom)
    }
}

impl StxTransferSpecification {
    pub fn from_specifications(
        specs: &StxTransferSpecificationFile,
    ) -> Result<StxTransferSpecification, String> {
        let Ok(expected_sender) = PrincipalData::parse_standard_principal(&specs.expected_sender)
        else {
            return Err(format!(
                "unable to parse expected sender '{}' as a valid Stacks address",
                specs.expected_sender
            ));
        };

        let Ok(recipient) = PrincipalData::parse(&specs.recipient) else {
            return Err(format!(
                "unable to parse recipient '{}' as a valid Stacks address",
                specs.expected_sender
            ));
        };

        let mut memo = [0u8; 34];
        if let Some(ref hex_memo) = specs.memo {
            if !hex_memo.is_empty() && !hex_memo.starts_with("0x") {
                return Err("unable to parse memo (up to 34 bytes, starting with '0x')".to_string());
            }
            match hex_bytes(&hex_memo[2..]) {
                Ok(ref mut bytes) => {
                    bytes.resize(34, 0);
                    memo.copy_from_slice(bytes);
                }
                Err(_) => return Err("unable to parse memo (up to 34 bytes)".to_string()),
            }
        }

        Ok(StxTransferSpecification {
            expected_sender,
            recipient,
            memo,
            mstx_amount: specs.mstx_amount,
            cost: specs.cost,
            anchor_block_only: specs.anchor_block_only.unwrap_or(true),
        })
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct BtcTransferSpecification {
    pub expected_sender: String,
    pub recipient: String,
    pub sats_amount: u64,
    pub sats_per_byte: u64,
}

impl BtcTransferSpecification {
    pub fn from_specifications(
        specs: &BtcTransferSpecificationFile,
    ) -> Result<BtcTransferSpecification, String> {
        // TODO(lgalabru): Data validation
        Ok(BtcTransferSpecification {
            expected_sender: specs.expected_sender.clone(),
            recipient: specs.recipient.clone(),
            sats_amount: specs.sats_amount,
            sats_per_byte: specs.sats_per_byte,
        })
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct ContractCallSpecification {
    #[serde(with = "qualified_contract_identifier_serde")]
    pub contract_id: QualifiedContractIdentifier,
    #[serde(with = "standard_principal_data_serde")]
    pub expected_sender: StandardPrincipalData,
    pub method: ClarityName,
    pub parameters: Vec<String>,
    pub cost: u64,
    pub anchor_block_only: bool,
}

impl ContractCallSpecification {
    pub fn from_specifications(
        specs: &ContractCallSpecificationFile,
    ) -> Result<ContractCallSpecification, String> {
        let Ok(contract_id) = QualifiedContractIdentifier::parse(&specs.contract_id) else {
            return Err(format!(
                "unable to parse '{}' as a valid contract identifier",
                specs.contract_id
            ));
        };

        let Ok(expected_sender) = PrincipalData::parse_standard_principal(&specs.expected_sender)
        else {
            return Err(format!(
                "unable to parse emulated sender '{}' as a valid Stacks address",
                specs.expected_sender
            ));
        };

        let Ok(method) = ClarityName::try_from(specs.method.to_string()) else {
            return Err(format!(
                "unable to parse '{}' as a valid contract name",
                specs.method
            ));
        };

        Ok(ContractCallSpecification {
            contract_id,
            expected_sender,
            method,
            parameters: specs.parameters.clone(),
            cost: specs.cost,
            anchor_block_only: specs.anchor_block_only.unwrap_or(true),
        })
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ContractPublishSpecification {
    pub contract_name: ContractName,
    #[serde(with = "standard_principal_data_serde")]
    pub expected_sender: StandardPrincipalData,
    pub location: FileLocation,
    #[serde(with = "source_serde")]
    pub source: String,
    #[serde(with = "clarity_version_serde")]
    pub clarity_version: ClarityVersion,
    pub cost: u64,
    pub anchor_block_only: bool,
}

impl ContractPublishSpecification {
    pub fn from_specifications(
        specs: &ContractPublishSpecificationFile,
        project_root_location: &FileLocation,
    ) -> Result<ContractPublishSpecification, String> {
        let Ok(contract_name) = ContractName::try_from(specs.contract_name.to_string()) else {
            return Err(format!(
                "unable to parse '{}' as a valid contract name",
                specs.contract_name
            ));
        };

        let Ok(expected_sender) = PrincipalData::parse_standard_principal(&specs.expected_sender)
        else {
            return Err(format!(
                "unable to parse expected sender '{}' as a valid Stacks address",
                specs.expected_sender
            ));
        };

        let location = match (&specs.path, &specs.url) {
            (Some(location_string), None) | (None, Some(location_string)) => {
                FileLocation::try_parse(location_string, Some(project_root_location))
            }
            _ => None,
        }
        .ok_or("unable to parse file location (can either be 'path' or 'url'".to_string())?;

        let source = location.read_content_as_utf8()?;
        let clarity_version = try_clarity_version_from_option(specs.clarity_version)?;

        Ok(ContractPublishSpecification {
            contract_name,
            expected_sender,
            source,
            location,
            cost: specs.cost,
            anchor_block_only: specs.anchor_block_only.unwrap_or(true),
            clarity_version,
        })
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct RequirementPublishSpecification {
    #[serde(with = "qualified_contract_identifier_serde")]
    pub contract_id: QualifiedContractIdentifier,
    #[serde(with = "standard_principal_data_serde")]
    pub remap_sender: StandardPrincipalData,
    #[serde(with = "remap_principals_serde")]
    pub remap_principals: BTreeMap<StandardPrincipalData, StandardPrincipalData>,
    #[serde(with = "source_serde")]
    pub source: String,
    #[serde(with = "clarity_version_serde")]
    pub clarity_version: ClarityVersion,
    pub cost: u64,
    pub location: FileLocation,
}

pub mod source_serde {
    use std::str::from_utf8;

    use base64::engine::general_purpose::STANDARD as b64;
    use base64::Engine as _;
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S>(x: &str, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let enc = b64.encode(x);
        s.serialize_str(&enc)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<String, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        base64_decode(&s).map_err(serde::de::Error::custom)
    }

    pub fn base64_decode(encoded: &str) -> Result<String, String> {
        let bytes = b64
            .decode(encoded)
            .map_err(|e| format!("unable to decode contract source: {e}"))?;
        let decoded = from_utf8(&bytes)
            .map_err(|e| format!("invalid UTF-8 sequence when decoding contract source: {e}"))?;
        Ok(decoded.to_owned())
    }
}

pub mod standard_principal_data_serde {
    use clarity_repl::clarity::vm::types::{PrincipalData, StandardPrincipalData};
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S>(x: &StandardPrincipalData, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.serialize_str(&x.to_address())
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<StandardPrincipalData, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        PrincipalData::parse_standard_principal(&s).map_err(serde::de::Error::custom)
    }
}

pub mod qualified_contract_identifier_serde {
    use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
    use serde::{Deserializer, Serializer};

    pub fn serialize<S>(x: &QualifiedContractIdentifier, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serde::Serialize::serialize(&x.to_string(), s)
    }

    pub fn deserialize<'de, D>(des: D) -> Result<QualifiedContractIdentifier, D::Error>
    where
        D: Deserializer<'de>,
    {
        let literal: String = serde::Deserialize::deserialize(des)?;

        QualifiedContractIdentifier::parse(&literal).map_err(serde::de::Error::custom)
    }
}

pub mod remap_principals_serde {
    use std::collections::{BTreeMap, HashMap};

    use clarity_repl::clarity::vm::types::{PrincipalData, StandardPrincipalData};
    use serde::ser::SerializeMap;
    use serde::{Deserializer, Serializer};

    pub fn serialize<S>(
        target: &BTreeMap<StandardPrincipalData, StandardPrincipalData>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(target.len()))?;
        for (k, v) in target {
            map.serialize_entry(&k.to_address(), &v.to_address())?;
        }
        map.end()
    }

    pub fn deserialize<'de, D>(
        des: D,
    ) -> Result<BTreeMap<StandardPrincipalData, StandardPrincipalData>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let container: HashMap<String, String> = serde::Deserialize::deserialize(des)?;
        let mut m = BTreeMap::new();
        for (k, v) in container {
            m.insert(
                PrincipalData::parse_standard_principal(&k).map_err(serde::de::Error::custom)?,
                PrincipalData::parse_standard_principal(&v).map_err(serde::de::Error::custom)?,
            );
        }
        Ok(m)
    }
}

pub mod clarity_version_serde {
    use clarinet_files::INVALID_CLARITY_VERSION;
    use clarity_repl::clarity::ClarityVersion;
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S>(clarity_version: &ClarityVersion, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match clarity_version {
            ClarityVersion::Clarity1 => s.serialize_i64(1),
            ClarityVersion::Clarity2 => s.serialize_i64(2),
            ClarityVersion::Clarity3 => s.serialize_i64(3),
            ClarityVersion::Clarity4 => s.serialize_i64(4),
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<ClarityVersion, D::Error>
    where
        D: Deserializer<'de>,
    {
        let cv = i64::deserialize(deserializer)?;
        match cv {
            1 => Ok(ClarityVersion::Clarity1),
            2 => Ok(ClarityVersion::Clarity2),
            3 => Ok(ClarityVersion::Clarity3),
            4 => Ok(ClarityVersion::Clarity4),
            _ => Err(serde::de::Error::custom(INVALID_CLARITY_VERSION)),
        }
    }
}

impl RequirementPublishSpecification {
    pub fn from_specifications(
        specs: &RequirementPublishSpecificationFile,
        project_root_location: &FileLocation,
    ) -> Result<RequirementPublishSpecification, String> {
        let Ok(contract_id) = QualifiedContractIdentifier::parse(&specs.contract_id) else {
            return Err(format!(
                "unable to parse '{}' as a valid contract identifier",
                specs.contract_id
            ));
        };

        let Ok(remap_sender) = PrincipalData::parse_standard_principal(&specs.remap_sender) else {
            return Err(format!(
                "unable to parse remap sender '{}' as a valid Stacks address",
                specs.remap_sender
            ));
        };

        let mut remap_principals = BTreeMap::new();
        if let Some(ref remap_principals_spec) = specs.remap_principals {
            for (src_spec, dst_spec) in remap_principals_spec {
                let Ok(src) = PrincipalData::parse_standard_principal(src_spec) else {
                    return Err(format!(
                        "unable to parse remap source '{}' as a valid Stacks address",
                        specs.remap_sender
                    ));
                };
                let Ok(dst) = PrincipalData::parse_standard_principal(dst_spec) else {
                    return Err(format!(
                        "unable to parse remap destination '{}' as a valid Stacks address",
                        specs.remap_sender
                    ));
                };
                remap_principals.insert(src, dst);
            }
        }

        let location = match (&specs.path, &specs.url) {
            (Some(location_string), None) | (None, Some(location_string)) => {
                FileLocation::try_parse(location_string, Some(project_root_location))
            }
            _ => None,
        }
        .ok_or("unable to parse file location (can either be 'path' or 'url'".to_string())?;

        let source = location.read_content_as_utf8()?;
        let clarity_version = try_clarity_version_from_option(specs.clarity_version)?;

        Ok(RequirementPublishSpecification {
            contract_id,
            remap_sender,
            remap_principals,
            source,
            clarity_version,
            location,
            cost: specs.cost,
        })
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct EmulatedContractCallSpecification {
    pub contract_id: QualifiedContractIdentifier,
    #[serde(with = "standard_principal_data_serde")]
    pub emulated_sender: StandardPrincipalData,
    pub method: ClarityName,
    pub parameters: Vec<String>,
}

impl EmulatedContractCallSpecification {
    pub fn from_specifications(
        specs: &EmulatedContractCallSpecificationFile,
    ) -> Result<EmulatedContractCallSpecification, String> {
        let Ok(contract_id) = QualifiedContractIdentifier::parse(&specs.contract_id) else {
            return Err(format!(
                "unable to parse '{}' as a valid contract_id",
                specs.contract_id
            ));
        };

        let Ok(emulated_sender) = PrincipalData::parse_standard_principal(&specs.emulated_sender)
        else {
            return Err(format!(
                "unable to parse emulated sender '{}' as a valid Stacks address",
                specs.emulated_sender
            ));
        };

        let Ok(method) = ClarityName::try_from(specs.method.to_string()) else {
            return Err(format!(
                "unable to parse '{}' as a valid contract name",
                specs.method
            ));
        };

        Ok(EmulatedContractCallSpecification {
            contract_id,
            emulated_sender,
            method,
            parameters: specs.parameters.clone(),
        })
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct EmulatedContractPublishSpecification {
    pub contract_name: ContractName,
    #[serde(with = "standard_principal_data_serde")]
    pub emulated_sender: StandardPrincipalData,
    pub source: String,
    #[serde(with = "clarity_version_serde")]
    pub clarity_version: ClarityVersion,
    pub location: FileLocation,
}

impl EmulatedContractPublishSpecification {
    pub fn from_specifications(
        specs: &EmulatedContractPublishSpecificationFile,
        project_root_location: &FileLocation,
        source: Option<String>,
    ) -> Result<EmulatedContractPublishSpecification, String> {
        let Ok(contract_name) = ContractName::try_from(specs.contract_name.to_string()) else {
            return Err(format!(
                "unable to parse '{}' as a valid contract name",
                specs.contract_name
            ));
        };

        let Ok(emulated_sender) = PrincipalData::parse_standard_principal(&specs.emulated_sender)
        else {
            return Err(format!(
                "unable to parse emulated sender '{}' as a valid Stacks address",
                specs.emulated_sender
            ));
        };

        let location = match (&specs.path, &specs.url) {
            (Some(location_string), None) | (None, Some(location_string)) => {
                FileLocation::try_parse(location_string, Some(project_root_location))
            }
            _ => None,
        }
        .ok_or("unable to parse file location (can either be 'path' or 'url'".to_string())?;

        let source = match source {
            Some(source) => source,
            None => location.read_content_as_utf8()?,
        };
        let clarity_version = try_clarity_version_from_option(specs.clarity_version)?;

        Ok(EmulatedContractPublishSpecification {
            contract_name,
            emulated_sender,
            source,
            location,
            clarity_version,
        })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct DeploymentSpecification {
    pub id: u32,
    pub name: String,
    pub network: StacksNetwork,
    pub stacks_node: Option<String>,
    pub bitcoin_node: Option<String>,
    pub genesis: Option<GenesisSpecification>,
    #[serde(flatten)]
    pub plan: TransactionPlanSpecification,
    // Keep a cache of contract's (source, relative_path)
    #[serde(with = "contracts_serde")]
    pub contracts: BTreeMap<QualifiedContractIdentifier, (String, FileLocation)>,
}

pub mod contracts_serde {
    use std::collections::{BTreeMap, HashMap};

    use base64::engine::general_purpose::STANDARD as b64;
    use base64::Engine as _;
    use clarinet_files::FileLocation;
    use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
    use serde::ser::SerializeSeq;
    use serde::{Deserializer, Serializer};

    use super::source_serde;

    pub fn serialize<S>(
        target: &BTreeMap<QualifiedContractIdentifier, (String, FileLocation)>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut out = serializer.serialize_seq(Some(target.len()))?;
        for (contract_id, (source, file_location)) in target {
            let encoded = b64.encode(source);
            let mut map = BTreeMap::new();
            map.insert("contract_id", contract_id.to_string());
            map.insert("source", encoded);
            match file_location {
                FileLocation::FileSystem { path } => {
                    map.insert("path", path.to_str().unwrap().to_string())
                }
                FileLocation::Url { url } => map.insert("url", url.to_string()),
            };
            out.serialize_element(&map)?;
        }
        out.end()
    }

    pub fn deserialize<'de, D>(
        des: D,
    ) -> Result<BTreeMap<QualifiedContractIdentifier, (String, FileLocation)>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let mut res: BTreeMap<QualifiedContractIdentifier, (String, FileLocation)> =
            BTreeMap::new();
        let container: Vec<HashMap<String, String>> = serde::Deserialize::deserialize(des)?;

        for entry in container {
            let contract_id = match entry.get("contract_id") {
                Some(contract_id) => QualifiedContractIdentifier::parse(contract_id).map_err(|e| {
                    serde::de::Error::custom(format!("failed to parse contract id: {e}"))
                }),
                None => Err(serde::de::Error::custom(
                    "Contract entry must have `contract_id` field",
                )),
            }?;

            let file_location = if let Some(url) = entry.get("url") {
                FileLocation::from_url_string(url)
            } else if let Some(path) = entry.get("path") {
                FileLocation::from_path_string(path)
            } else {
                Err("Invalid file location field. Must have key \"url\" or \"path\"".into())
            }
            .map_err(serde::de::Error::custom)?;

            let source = match entry.get("source") {
                Some(source) => {
                    source_serde::base64_decode(source).map_err(serde::de::Error::custom)
                }
                None => Err(serde::de::Error::custom(
                    "Contract entry must have `source` field",
                )),
            }?;

            res.insert(contract_id, (source, file_location));
        }

        Ok(res)
    }
}

impl DeploymentSpecification {
    pub fn from_config_file(
        deployment_location: &FileLocation,
        project_root_location: &FileLocation,
    ) -> Result<DeploymentSpecification, String> {
        let spec_file_content = deployment_location.read_content()?;

        let specification_file: DeploymentSpecificationFile =
            match serde_yaml::from_slice(&spec_file_content[..]) {
                Ok(res) => res,
                Err(msg) => return Err(format!("unable to read file {msg}")),
            };

        let network = match specification_file.network.to_lowercase().as_str() {
            "simnet" => StacksNetwork::Simnet,
            "devnet" => StacksNetwork::Devnet,
            "testnet" => StacksNetwork::Testnet,
            "mainnet" => StacksNetwork::Mainnet,
            _ => {
                return Err(format!(
                    "network '{}' not supported (simnet, devnet, testnet, mainnet)",
                    specification_file.network
                ));
            }
        };

        let deployment_spec = DeploymentSpecification::from_specifications(
            &specification_file,
            &network,
            project_root_location,
            None,
        )?;

        Ok(deployment_spec)
    }

    pub fn from_specifications(
        specs: &DeploymentSpecificationFile,
        network: &StacksNetwork,
        project_root_location: &FileLocation,
        contracts_sources: Option<&HashMap<String, String>>,
    ) -> Result<DeploymentSpecification, String> {
        let mut contracts = BTreeMap::new();
        let (plan, genesis) = match network {
            StacksNetwork::Simnet => {
                let mut batches = vec![];
                let mut genesis = None;
                if let Some(ref plan) = specs.plan {
                    for batch in plan.batches.iter() {
                        let mut transactions = vec![];
                        for tx in batch.transactions.iter() {
                            let transaction = match tx {
                                TransactionSpecificationFile::EmulatedContractCall(spec) => {
                                    TransactionSpecification::EmulatedContractCall(EmulatedContractCallSpecification::from_specifications(spec)?)
                                }
                                TransactionSpecificationFile::EmulatedContractPublish(spec) => {
                                    let source = contracts_sources.as_ref().map(|contracts_sources| {
                                        let contract_path = FileLocation::try_parse(spec.path.as_ref().expect("missing path"), Some(project_root_location))
                                            .expect("failed to get contract path").to_string();
                                        contracts_sources
                                            .get(&contract_path)
                                            .cloned()
                                            .unwrap_or_else(|| panic!("missing contract source for {}", spec.path.clone().unwrap_or_default()))
                                    });

                                    let spec = EmulatedContractPublishSpecification::from_specifications(spec, project_root_location, source)?;
                                    let contract_id = QualifiedContractIdentifier::new(spec.emulated_sender.clone(), spec.contract_name.clone());
                                    contracts.insert(contract_id, (spec.source.clone(), spec.location.clone()));
                                    TransactionSpecification::EmulatedContractPublish(spec)
                                }
                                TransactionSpecificationFile::StxTransfer(spec) => {
                                    let spec = StxTransferSpecification::from_specifications(spec)?;
                                    TransactionSpecification::StxTransfer(spec)
                                }
                                TransactionSpecificationFile::BtcTransfer(_) | TransactionSpecificationFile::ContractCall(_) | TransactionSpecificationFile::ContractPublish(_) | TransactionSpecificationFile::RequirementPublish(_) => {
                                    return Err(format!("{} only supports transactions of type 'emulated-contract-call' and 'emulated-contract-publish", specs.network.to_lowercase()))
                                }
                            };
                            transactions.push(transaction);
                        }
                        if !transactions.is_empty() {
                            batches.push(TransactionsBatchSpecification {
                                id: batch.id,
                                transactions,
                                epoch: batch.epoch,
                            });
                        }
                    }
                }
                if let Some(ref genesis_specs) = specs.genesis {
                    let genesis_specs = GenesisSpecification::from_specifications(genesis_specs)?;
                    genesis = Some(genesis_specs);
                }
                (TransactionPlanSpecification { batches }, genesis)
            }
            StacksNetwork::Devnet | StacksNetwork::Testnet | StacksNetwork::Mainnet => {
                let mut batches = vec![];
                if let Some(ref plan) = specs.plan {
                    for batch in plan.batches.iter() {
                        let mut transactions = vec![];
                        for tx in batch.transactions.iter() {
                            let transaction = match tx {
                                TransactionSpecificationFile::ContractCall(spec) => {
                                    TransactionSpecification::ContractCall(ContractCallSpecification::from_specifications(spec)?)
                                }
                                TransactionSpecificationFile::RequirementPublish(spec) => {
                                    if matches!(network, StacksNetwork::Mainnet) {
                                        return Err(format!("{} only supports transactions of type 'contract-call' and 'contract-publish", specs.network.to_lowercase()))
                                    }
                                    let spec = RequirementPublishSpecification::from_specifications(spec, project_root_location)?;
                                    TransactionSpecification::RequirementPublish(spec)
                                }
                                TransactionSpecificationFile::ContractPublish(spec) => {
                                    let spec = ContractPublishSpecification::from_specifications(spec, project_root_location)?;

                                    let contract_id = QualifiedContractIdentifier::new(spec.expected_sender.clone(), spec.contract_name.clone());
                                    contracts.insert(contract_id, (spec.source.clone(), spec.location.clone()));
                                    TransactionSpecification::ContractPublish(spec)
                                }
                                TransactionSpecificationFile::BtcTransfer(spec) => {
                                    let spec = BtcTransferSpecification::from_specifications(spec)?;
                                    TransactionSpecification::BtcTransfer(spec)
                                }
                                TransactionSpecificationFile::StxTransfer(spec) => {
                                    let spec = StxTransferSpecification::from_specifications(spec)?;
                                    TransactionSpecification::StxTransfer(spec)
                                }
                                TransactionSpecificationFile::EmulatedContractCall(_) | TransactionSpecificationFile::EmulatedContractPublish(_) => {
                                    return Err(format!("{} only supports transactions of type 'contract-call' and 'contract-publish'", specs.network.to_lowercase()))
                                }
                            };
                            transactions.push(transaction);
                        }
                        if !transactions.is_empty() {
                            batches.push(TransactionsBatchSpecification {
                                id: batch.id,
                                transactions,
                                epoch: batch.epoch,
                            });
                        }
                    }
                }
                (TransactionPlanSpecification { batches }, None)
            }
        };
        let stacks_node = match (&specs.stacks_node, &specs.node) {
            (Some(node), _) | (None, Some(node)) => Some(node.clone()),
            _ => None,
        };
        let bitcoin_node = match (&specs.bitcoin_node, &specs.node) {
            (Some(node), _) | (None, Some(node)) => Some(node.clone()),
            _ => None,
        };

        Ok(DeploymentSpecification {
            id: specs.id.unwrap_or(0),
            stacks_node,
            bitcoin_node,
            name: specs.name.to_string(),
            network: network.clone(),
            genesis,
            plan,
            contracts,
        })
    }

    pub fn to_specification_file(&self) -> DeploymentSpecificationFile {
        DeploymentSpecificationFile {
            id: Some(self.id),
            name: self.name.clone(),
            network: match self.network {
                StacksNetwork::Simnet => "simnet".to_string(),
                StacksNetwork::Devnet => "devnet".to_string(),
                StacksNetwork::Testnet => "testnet".to_string(),
                StacksNetwork::Mainnet => "mainnet".to_string(),
            },
            stacks_node: self.stacks_node.clone(),
            bitcoin_node: self.bitcoin_node.clone(),
            node: None,
            genesis: self.genesis.as_ref().map(|g| g.to_specification_file()),
            plan: Some(self.plan.to_specification_file()),
        }
    }

    pub fn to_file_content(&self) -> Result<Vec<u8>, String> {
        serde_yaml::to_vec(&self.to_specification_file())
            .map_err(|err| format!("failed to serialize deployment\n{err}"))
    }

    pub fn sort_batches_by_epoch(&mut self) {
        self.plan.batches.sort_by(|a, b| a.epoch.cmp(&b.epoch));
        for (i, batch) in self.plan.batches.iter_mut().enumerate() {
            batch.id = i;
        }
    }

    pub fn extract_no_contract_publish_txs(&self) -> (Self, Vec<TransactionsBatchSpecification>) {
        let mut deployment_only_contract_publish_txs = self.clone();
        let mut custom_txs_batches = vec![];

        for batch in deployment_only_contract_publish_txs.plan.batches.iter_mut() {
            let (ref contract_publish_txs, custom_txs): (
                Vec<TransactionSpecification>,
                Vec<TransactionSpecification>,
            ) = batch.transactions.clone().into_iter().partition(|tx| {
                matches!(tx, TransactionSpecification::ContractPublish(_))
                    || matches!(tx, TransactionSpecification::EmulatedContractPublish(_))
            });

            batch.transactions.clone_from(contract_publish_txs);
            if !custom_txs.is_empty() {
                custom_txs_batches.push(TransactionsBatchSpecification {
                    id: batch.id,
                    transactions: custom_txs,
                    epoch: batch.epoch,
                });
            }
        }

        deployment_only_contract_publish_txs
            .plan
            .batches
            .retain(|b| !b.transactions.is_empty());

        (deployment_only_contract_publish_txs, custom_txs_batches)
    }

    pub fn merge_batches(&mut self, custom_batches: Vec<TransactionsBatchSpecification>) {
        for custom_batch in custom_batches {
            if let Some(batch) = self
                .plan
                .batches
                .iter_mut()
                .find(|b| b.id == custom_batch.id && b.epoch == custom_batch.epoch)
            {
                batch.transactions.extend(custom_batch.transactions);
            } else if !custom_batch.transactions.is_empty() {
                self.plan.batches.push(custom_batch);
            }
        }
        self.sort_batches_by_epoch();
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct DeploymentSpecificationFile {
    pub id: Option<u32>,
    pub name: String,
    pub network: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stacks_node: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bitcoin_node: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub node: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub genesis: Option<GenesisSpecificationFile>,
    pub plan: Option<TransactionPlanSpecificationFile>,
}

impl DeploymentSpecificationFile {
    pub async fn from_file_accessor(
        path: &FileLocation,
        file_accesor: &dyn FileAccessor,
    ) -> Result<DeploymentSpecificationFile, String> {
        let spec_file_content = file_accesor.read_file(path.to_string()).await?;

        serde_yaml::from_str(&spec_file_content).map_err(|msg| format!("unable to read file {msg}"))
    }
    pub fn from_file_content(
        spec_file_content: &str,
    ) -> Result<DeploymentSpecificationFile, String> {
        serde_yaml::from_str(spec_file_content).map_err(|msg| format!("unable to read file {msg}"))
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct GenesisSpecificationFile {
    pub wallets: Vec<WalletSpecificationFile>,
    pub contracts: Vec<String>,
}

#[derive(Debug, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct WalletSpecificationFile {
    pub name: String,
    pub address: String,
    pub balance: String,
    pub sbtc_balance: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct GenesisSpecification {
    pub wallets: Vec<WalletSpecification>,
    pub contracts: Vec<String>,
}

impl GenesisSpecification {
    pub fn from_specifications(
        specs: &GenesisSpecificationFile,
    ) -> Result<GenesisSpecification, String> {
        let mut wallets = vec![];
        for wallet in specs.wallets.iter() {
            wallets.push(WalletSpecification::from_specifications(wallet)?);
        }

        Ok(GenesisSpecification {
            wallets,
            contracts: specs.contracts.clone(),
        })
    }

    pub fn to_specification_file(&self) -> GenesisSpecificationFile {
        let mut wallets = vec![];
        for wallet in self.wallets.iter() {
            wallets.push(WalletSpecificationFile {
                name: wallet.name.to_string(),
                address: wallet.address.to_string(),
                balance: format!("{}", wallet.balance),
                sbtc_balance: Some(format!("{}", wallet.sbtc_balance)),
            })
        }

        GenesisSpecificationFile {
            wallets,
            contracts: self.contracts.clone(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct WalletSpecification {
    pub name: String,
    #[serde(with = "standard_principal_data_serde")]
    pub address: StandardPrincipalData,
    pub balance: u128,
    pub sbtc_balance: u128,
}

impl WalletSpecification {
    pub fn from_specifications(
        specs: &WalletSpecificationFile,
    ) -> Result<WalletSpecification, String> {
        let address = PrincipalData::parse_standard_principal(&specs.address).map_err(|_| {
            format!(
                "unable to parse {}'s principal as a valid Stacks address",
                specs.name
            )
        })?;

        let balance = specs
            .balance
            .parse::<u128>()
            .map_err(|_| format!("unable to parse {}'s balance as a u128", specs.name))?;

        let sbtc_balance = specs
            .sbtc_balance
            .clone()
            .unwrap_or("1000000000".to_string())
            .parse::<u128>()
            .map_err(|_| format!("unable to parse {}'s sbtc_balance as a u128", specs.name))?;

        Ok(WalletSpecification {
            name: specs.name.to_string(),
            address,
            balance,
            sbtc_balance,
        })
    }
}

impl TransactionPlanSpecification {
    pub fn to_specification_file(&self) -> TransactionPlanSpecificationFile {
        let mut batches = vec![];
        for batch in self.batches.iter() {
            let mut transactions = vec![];
            for tx in batch.transactions.iter() {
                let tx = match tx {
                    TransactionSpecification::ContractCall(tx) => {
                        TransactionSpecificationFile::ContractCall(ContractCallSpecificationFile {
                            contract_id: tx.contract_id.to_string(),
                            expected_sender: tx.expected_sender.to_address(),
                            method: tx.method.to_string(),
                            parameters: tx.parameters.clone(),
                            cost: tx.cost,
                            anchor_block_only: Some(tx.anchor_block_only),
                        })
                    }
                    TransactionSpecification::ContractPublish(tx) => {
                        TransactionSpecificationFile::ContractPublish(
                            ContractPublishSpecificationFile {
                                contract_name: tx.contract_name.to_string(),
                                expected_sender: tx.expected_sender.to_address(),
                                location: Some(tx.location.clone()),
                                path: None,
                                url: None,
                                cost: tx.cost,
                                anchor_block_only: Some(tx.anchor_block_only),
                                clarity_version: match tx.clarity_version {
                                    ClarityVersion::Clarity1 => Some(1),
                                    ClarityVersion::Clarity2 => Some(2),
                                    ClarityVersion::Clarity3 => Some(3),
                                    ClarityVersion::Clarity4 => Some(4),
                                },
                            },
                        )
                    }
                    TransactionSpecification::EmulatedContractCall(tx) => {
                        TransactionSpecificationFile::EmulatedContractCall(
                            EmulatedContractCallSpecificationFile {
                                contract_id: tx.contract_id.to_string(),
                                emulated_sender: tx.emulated_sender.to_address(),
                                method: tx.method.to_string(),
                                parameters: tx.parameters.clone(),
                            },
                        )
                    }
                    TransactionSpecification::EmulatedContractPublish(tx) => {
                        TransactionSpecificationFile::EmulatedContractPublish(
                            EmulatedContractPublishSpecificationFile {
                                contract_name: tx.contract_name.to_string(),
                                emulated_sender: tx.emulated_sender.to_address(),
                                location: Some(tx.location.clone()),
                                path: None,
                                url: None,
                                clarity_version: match tx.clarity_version {
                                    ClarityVersion::Clarity1 => Some(1),
                                    ClarityVersion::Clarity2 => Some(2),
                                    ClarityVersion::Clarity3 => Some(3),
                                    ClarityVersion::Clarity4 => Some(4),
                                },
                            },
                        )
                    }
                    TransactionSpecification::RequirementPublish(tx) => {
                        let mut remap_principals = BTreeMap::new();
                        for (src, dst) in tx.remap_principals.iter() {
                            remap_principals.insert(src.to_address(), dst.to_address());
                        }
                        TransactionSpecificationFile::RequirementPublish(
                            RequirementPublishSpecificationFile {
                                contract_id: tx.contract_id.to_string(),
                                remap_sender: tx.remap_sender.to_address(),
                                remap_principals: Some(remap_principals),
                                location: Some(tx.location.clone()),
                                path: None,
                                url: None,
                                cost: tx.cost,
                                clarity_version: match tx.clarity_version {
                                    ClarityVersion::Clarity1 => Some(1),
                                    ClarityVersion::Clarity2 => Some(2),
                                    ClarityVersion::Clarity3 => Some(3),
                                    ClarityVersion::Clarity4 => Some(4),
                                },
                            },
                        )
                    }
                    TransactionSpecification::BtcTransfer(tx) => {
                        TransactionSpecificationFile::BtcTransfer(BtcTransferSpecificationFile {
                            expected_sender: tx.expected_sender.to_string(),
                            recipient: tx.recipient.clone(),
                            sats_amount: tx.sats_amount,
                            sats_per_byte: tx.sats_per_byte,
                        })
                    }
                    TransactionSpecification::StxTransfer(tx) => {
                        TransactionSpecificationFile::StxTransfer(StxTransferSpecificationFile {
                            expected_sender: tx.expected_sender.to_address(),
                            recipient: tx.recipient.to_string(),
                            mstx_amount: tx.mstx_amount,
                            memo: if tx.memo == [0; 34] {
                                None
                            } else {
                                Some(format!("0x{}", to_hex(&tx.memo)))
                            },
                            cost: tx.cost,
                            anchor_block_only: Some(tx.anchor_block_only),
                        })
                    }
                };
                transactions.push(tx);
            }

            batches.push(TransactionsBatchSpecificationFile {
                id: batch.id,
                transactions,
                epoch: batch.epoch,
            });
        }

        TransactionPlanSpecificationFile { batches }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn test_epoch_config() {
        let devnet_config = DevnetConfig {
            epoch_2_0: 1,
            epoch_2_05: 2,
            epoch_2_1: 3,
            epoch_2_2: 4,
            epoch_2_3: 5,
            epoch_2_4: 6,
            epoch_2_5: 7,
            epoch_3_0: 8,
            epoch_3_1: 9,
            epoch_3_2: 10,
            epoch_3_3: None,
            ..Default::default()
        };

        let epoch_config = BurnchainEpochConfig::from(&devnet_config);
        let epoch_config_toml = toml::to_string(&epoch_config).unwrap();

        assert_eq!(
            epoch_config_toml,
            indoc! { r#"
                [[burnchain.epochs]]
                epoch_name = "2.0"
                start_height = 1

                [[burnchain.epochs]]
                epoch_name = "2.05"
                start_height = 2

                [[burnchain.epochs]]
                epoch_name = "2.1"
                start_height = 3

                [[burnchain.epochs]]
                epoch_name = "2.2"
                start_height = 4

                [[burnchain.epochs]]
                epoch_name = "2.3"
                start_height = 5

                [[burnchain.epochs]]
                epoch_name = "2.4"
                start_height = 6

                [[burnchain.epochs]]
                epoch_name = "2.5"
                start_height = 7

                [[burnchain.epochs]]
                epoch_name = "3.0"
                start_height = 8

                [[burnchain.epochs]]
                epoch_name = "3.1"
                start_height = 9

                [[burnchain.epochs]]
                epoch_name = "3.2"
                start_height = 10
                "#
            }
        );
    }

    #[test]
    fn test_epoch_config_with_optional_3_3() {
        let devnet_config = DevnetConfig {
            epoch_2_0: 1,
            epoch_2_05: 2,
            epoch_2_1: 3,
            epoch_2_2: 4,
            epoch_2_3: 5,
            epoch_2_4: 6,
            epoch_2_5: 7,
            epoch_3_0: 8,
            epoch_3_1: 9,
            epoch_3_2: 10,
            epoch_3_3: Some(11),
            ..Default::default()
        };

        let epoch_config = BurnchainEpochConfig::from(&devnet_config);
        let epoch_config_toml = toml::to_string(&epoch_config).unwrap();

        assert_eq!(
            epoch_config_toml,
            indoc! { r#"
                [[burnchain.epochs]]
                epoch_name = "2.0"
                start_height = 1

                [[burnchain.epochs]]
                epoch_name = "2.05"
                start_height = 2

                [[burnchain.epochs]]
                epoch_name = "2.1"
                start_height = 3

                [[burnchain.epochs]]
                epoch_name = "2.2"
                start_height = 4

                [[burnchain.epochs]]
                epoch_name = "2.3"
                start_height = 5

                [[burnchain.epochs]]
                epoch_name = "2.4"
                start_height = 6

                [[burnchain.epochs]]
                epoch_name = "2.5"
                start_height = 7

                [[burnchain.epochs]]
                epoch_name = "3.0"
                start_height = 8

                [[burnchain.epochs]]
                epoch_name = "3.1"
                start_height = 9

                [[burnchain.epochs]]
                epoch_name = "3.2"
                start_height = 10

                [[burnchain.epochs]]
                epoch_name = "3.3"
                start_height = 11
                "#
            }
        );
    }
}
