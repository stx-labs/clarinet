use std::str::FromStr;

use serde::ser::{SerializeSeq, Serializer};
use serde::{Deserialize, Serialize};

use crate::chainhook::hooks::bitcoin::{
    BitcoinChainhookInstance, BitcoinChainhookSpecificationNetworkMap,
};
use crate::chainhook::hooks::stacks::{
    StacksChainhookInstance, StacksChainhookSpecificationNetworkMap,
};
use crate::chainhook::types::{BitcoinNetwork, StacksNetwork};

#[derive(Deserialize, Debug, Clone)]
pub struct ChainhookStore {
    pub stacks_chainhooks: Vec<StacksChainhookInstance>,
    pub bitcoin_chainhooks: Vec<BitcoinChainhookInstance>,
}

impl Default for ChainhookStore {
    fn default() -> Self {
        Self::new()
    }
}

impl ChainhookStore {
    pub fn new() -> ChainhookStore {
        ChainhookStore {
            stacks_chainhooks: vec![],
            bitcoin_chainhooks: vec![],
        }
    }

    pub fn register_instance_from_network_map(
        &mut self,
        networks: (&BitcoinNetwork, &StacksNetwork),
        hook: ChainhookSpecificationNetworkMap,
    ) -> Result<ChainhookInstance, String> {
        let spec = match hook {
            ChainhookSpecificationNetworkMap::Stacks(hook) => {
                let spec = hook.into_specification_for_network(networks.1)?;
                self.stacks_chainhooks.push(spec.clone());
                ChainhookInstance::Stacks(spec)
            }
            ChainhookSpecificationNetworkMap::Bitcoin(hook) => {
                let spec = hook.into_specification_for_network(networks.0)?;
                self.bitcoin_chainhooks.push(spec.clone());
                ChainhookInstance::Bitcoin(spec)
            }
        };
        Ok(spec)
    }

    pub fn enable_instance(&mut self, predicate_spec: &mut ChainhookInstance) {
        match predicate_spec {
            ChainhookInstance::Stacks(spec_to_enable) => {
                for spec in self.stacks_chainhooks.iter_mut() {
                    if spec.uuid.eq(&spec_to_enable.uuid) {
                        spec.enabled = true;
                        spec_to_enable.enabled = true;
                        break;
                    }
                }
            }
            ChainhookInstance::Bitcoin(spec_to_enable) => {
                for spec in self.bitcoin_chainhooks.iter_mut() {
                    if spec.uuid.eq(&spec_to_enable.uuid) {
                        spec.enabled = true;
                        spec_to_enable.enabled = true;
                        break;
                    }
                }
            }
        };
    }

    pub fn register_instance(&mut self, spec: ChainhookInstance) -> Result<(), String> {
        match spec {
            ChainhookInstance::Stacks(spec) => {
                let spec = spec.clone();
                self.stacks_chainhooks.push(spec);
            }
            ChainhookInstance::Bitcoin(spec) => {
                let spec = spec.clone();
                self.bitcoin_chainhooks.push(spec);
            }
        };
        Ok(())
    }

    pub fn deregister_stacks_hook(&mut self, hook_uuid: String) -> Option<StacksChainhookInstance> {
        let mut i = 0;
        while i < self.stacks_chainhooks.len() {
            if self.stacks_chainhooks[i].uuid == hook_uuid {
                let hook = self.stacks_chainhooks.remove(i);
                return Some(hook);
            } else {
                i += 1;
            }
        }
        None
    }

    pub fn deregister_bitcoin_hook(
        &mut self,
        hook_uuid: String,
    ) -> Option<BitcoinChainhookInstance> {
        let mut i = 0;
        while i < self.bitcoin_chainhooks.len() {
            if self.bitcoin_chainhooks[i].uuid == hook_uuid {
                let hook = self.bitcoin_chainhooks.remove(i);
                return Some(hook);
            } else {
                i += 1;
            }
        }
        None
    }

    pub fn expire_stacks_hook(&mut self, hook_uuid: String, block_height: u64) {
        let mut i = 0;
        while i < self.stacks_chainhooks.len() {
            if ChainhookInstance::stacks_key(&self.stacks_chainhooks[i].uuid) == hook_uuid {
                self.stacks_chainhooks[i].expired_at = Some(block_height);
                break;
            } else {
                i += 1;
            }
        }
    }

    pub fn expire_bitcoin_hook(&mut self, hook_uuid: String, block_height: u64) {
        let mut i = 0;
        while i < self.bitcoin_chainhooks.len() {
            if ChainhookInstance::bitcoin_key(&self.bitcoin_chainhooks[i].uuid) == hook_uuid {
                self.bitcoin_chainhooks[i].expired_at = Some(block_height);
                break;
            } else {
                i += 1;
            }
        }
    }
}

impl Serialize for ChainhookStore {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(
            self.bitcoin_chainhooks.len() + self.stacks_chainhooks.len(),
        ))?;
        for chainhook in self.bitcoin_chainhooks.iter() {
            seq.serialize_element(chainhook)?;
        }
        for chainhook in self.stacks_chainhooks.iter() {
            seq.serialize_element(chainhook)?;
        }
        seq.end()
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ChainhookInstance {
    Bitcoin(BitcoinChainhookInstance),
    Stacks(StacksChainhookInstance),
}

impl ChainhookInstance {
    pub fn either_stx_or_btc_key(uuid: &str) -> String {
        format!("predicate:{}", uuid)
    }

    pub fn stacks_key(uuid: &str) -> String {
        format!("predicate:{}", uuid)
    }

    pub fn bitcoin_key(uuid: &str) -> String {
        format!("predicate:{}", uuid)
    }

    pub fn key(&self) -> String {
        match &self {
            Self::Bitcoin(data) => Self::bitcoin_key(&data.uuid),
            Self::Stacks(data) => Self::stacks_key(&data.uuid),
        }
    }

    pub fn deserialize_specification(spec: &str) -> Result<ChainhookInstance, String> {
        let spec: ChainhookInstance = serde_json::from_str(spec)
            .map_err(|e| format!("unable to deserialize predicate {}", e))?;
        Ok(spec)
    }

    pub fn uuid(&self) -> &str {
        match &self {
            Self::Bitcoin(data) => &data.uuid,
            Self::Stacks(data) => &data.uuid,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case", tag = "chain")]
pub enum ChainhookSpecificationNetworkMap {
    Bitcoin(BitcoinChainhookSpecificationNetworkMap),
    Stacks(StacksChainhookSpecificationNetworkMap),
}

impl ChainhookSpecificationNetworkMap {
    pub fn validate(&self) -> Result<(), String> {
        match &self {
            Self::Bitcoin(data) => {
                let mut errors = vec![];
                for (network, spec) in data.networks.iter() {
                    if let Err(e) = spec.validate() {
                        errors.append(&mut append_error_context(
                            &format!(
                                "invalid Bitcoin predicate '{}' for network {}",
                                data.name, network
                            ),
                            e,
                        ));
                    }
                }
                if !errors.is_empty() {
                    return Err(errors.join("\n"));
                }
            }
            Self::Stacks(data) => {
                let mut errors = vec![];
                for (network, spec) in data.networks.iter() {
                    if let Err(e) = spec.validate() {
                        errors.append(&mut append_error_context(
                            &format!(
                                "invalid Stacks predicate '{}' for network {}",
                                data.name, network
                            ),
                            e,
                        ));
                    }
                }
                if !errors.is_empty() {
                    return Err(errors.join("\n"));
                }
            }
        }
        Ok(())
    }

    pub fn get_uuid(&self) -> &str {
        match &self {
            Self::Bitcoin(data) => &data.uuid,
            Self::Stacks(data) => &data.uuid,
        }
    }

    pub fn deserialize_specification(
        spec: &str,
        _key: &str,
    ) -> Result<ChainhookSpecificationNetworkMap, String> {
        let spec: ChainhookSpecificationNetworkMap = serde_json::from_str(spec)
            .map_err(|e| format!("unable to deserialize predicate {}", e))?;
        Ok(spec)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum HookAction {
    HttpPost(HttpHook),
    FileAppend(FileHook),
    Noop,
}

impl HookAction {
    pub fn validate(&self) -> Result<(), Vec<String>> {
        match &self {
            HookAction::HttpPost(spec) => {
                if let Err(e) = spec.validate() {
                    return Err(append_error_context("invalid 'http_post' data", e));
                }
            }
            HookAction::FileAppend(_) => {}
            HookAction::Noop => {}
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct HttpHook {
    pub url: String,
    pub authorization_header: String,
}

impl HttpHook {
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = vec![];
        if let Err(e) = reqwest::Url::from_str(&self.url) {
            errors.push(format!("url string must be a valid Url: {}", e));
        }
        if let Err(e) = reqwest::header::HeaderValue::from_str(&self.authorization_header) {
            errors.push(format!("auth header must be a valid header value: {}", e));
        };

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct FileHook {
    pub path: String,
}
// todo: can we remove this struct?
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct ScriptTemplate {
    pub instructions: Vec<ScriptInstruction>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ScriptInstruction {
    Opcode(u8),
    RawBytes(Vec<u8>),
    Placeholder(String, u8),
}

impl ScriptTemplate {
    pub fn parse(template: &str) -> Result<ScriptTemplate, String> {
        let raw_instructions = template
            .split_ascii_whitespace()
            .map(|c| c.to_string())
            .collect::<Vec<_>>();
        let mut instructions = vec![];
        for raw_instruction in raw_instructions.into_iter() {
            if raw_instruction.starts_with("{") {
                let placeholder = &raw_instruction[1..raw_instruction.len() - 1];
                let (name, size) = match placeholder.split_once(":") {
                    Some(res) => res,
                    None => return Err(format!("malformed placeholder {}: should be {{placeholder-name:number-of-bytes}} (ex: {{id:4}}", raw_instruction))
                };
                let size = match size.parse::<u8>() {
                    Ok(res) => res,
                    Err(_) => return Err(format!("malformed placeholder {}: should be {{placeholder-name:number-of-bytes}} (ex: {{id:4}}", raw_instruction))
                };
                instructions.push(ScriptInstruction::Placeholder(name.to_string(), size));
            } else if let Some(opcode) = opcode_to_hex(&raw_instruction) {
                instructions.push(ScriptInstruction::Opcode(opcode));
            } else if let Ok(bytes) = hex::decode(&raw_instruction) {
                instructions.push(ScriptInstruction::RawBytes(bytes));
            } else {
                return Err(format!("unable to handle instruction {}", raw_instruction));
            }
        }
        Ok(ScriptTemplate { instructions })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct PoxConfig {
    pub first_burnchain_block_height: u64,
    pub prepare_phase_len: u64,
    pub reward_phase_len: u64,
    // TODO currently unused
    pub rewarded_addresses_per_block: usize,
}

impl PoxConfig {
    pub fn mainnet_default() -> PoxConfig {
        PoxConfig {
            first_burnchain_block_height: 666050,
            prepare_phase_len: 100,
            reward_phase_len: 2000,
            rewarded_addresses_per_block: 2,
        }
    }

    pub fn testnet_default() -> PoxConfig {
        PoxConfig {
            first_burnchain_block_height: 2000000,
            prepare_phase_len: 50,
            reward_phase_len: 1000,
            rewarded_addresses_per_block: 2,
        }
    }

    pub fn devnet_default() -> PoxConfig {
        Self::default()
    }
    pub fn get_pox_cycle_len(&self) -> u64 {
        self.prepare_phase_len + self.reward_phase_len
    }

    pub fn get_pox_cycle_id(&self, block_height: u64) -> u64 {
        (block_height.saturating_sub(self.first_burnchain_block_height)) / self.get_pox_cycle_len()
    }

    pub fn get_pos_in_pox_cycle(&self, block_height: u64) -> u64 {
        (block_height.saturating_sub(self.first_burnchain_block_height)) % self.get_pox_cycle_len()
    }

    pub fn get_burn_address(&self) -> &str {
        match self.first_burnchain_block_height {
            666050 => "1111111111111111111114oLvT2",
            2000000 => "burn-address-regtest",
            _ => "burn-address",
        }
    }
}

impl Default for PoxConfig {
    fn default() -> PoxConfig {
        PoxConfig {
            first_burnchain_block_height: 100,
            prepare_phase_len: 5,
            reward_phase_len: 15,
            rewarded_addresses_per_block: 2,
        }
    }
}

pub fn get_canonical_pox_config(network: &BitcoinNetwork) -> PoxConfig {
    match network {
        BitcoinNetwork::Mainnet => PoxConfig::mainnet_default(),
        BitcoinNetwork::Testnet => PoxConfig::testnet_default(),
        BitcoinNetwork::Regtest => PoxConfig::default(),
        BitcoinNetwork::Signet => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum StacksOpcodes {
    BlockCommit = b'[',
    KeyRegister = b'^',
    StackStx = b'x',
    PreStx = b'p',
    TransferStx = b'$',
}

impl TryFrom<u8> for StacksOpcodes {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == StacksOpcodes::BlockCommit as u8 => Ok(StacksOpcodes::BlockCommit),
            x if x == StacksOpcodes::KeyRegister as u8 => Ok(StacksOpcodes::KeyRegister),
            x if x == StacksOpcodes::StackStx as u8 => Ok(StacksOpcodes::StackStx),
            x if x == StacksOpcodes::PreStx as u8 => Ok(StacksOpcodes::PreStx),
            x if x == StacksOpcodes::TransferStx as u8 => Ok(StacksOpcodes::TransferStx),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct TxinPredicate {
    pub txid: String,
    pub vout: u32,
}

impl TxinPredicate {
    pub fn validate(&self) -> Result<(), Vec<String>> {
        if let Err(e) = validate_txid(&self.txid) {
            return Err(vec![e]);
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum BlockIdentifierIndexRule {
    Equals(u64),
    HigherThan(u64),
    LowerThan(u64),
    Between(u64, u64),
}

impl BlockIdentifierIndexRule {
    pub fn validate(&self) -> Result<(), String> {
        match self {
            BlockIdentifierIndexRule::Equals(_) => {}
            BlockIdentifierIndexRule::HigherThan(_) => {}
            BlockIdentifierIndexRule::LowerThan(val) => {
                if val.eq(&0) {
                    return Err("'lower_than' filter must be greater than 0".into());
                }
            }
            BlockIdentifierIndexRule::Between(lhs, rhs) => {
                if lhs >= rhs {
                    return Err("'between' filter must have left-hand-side valud greater than right-hand-side value".into());
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum Scope {
    Inputs,
    Outputs,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum MatchingRule {
    Equals(String),
    StartsWith(String),
    EndsWith(String),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ExactMatchingRule {
    Equals(String),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum BlockIdentifierHashRule {
    Equals(String),
    BuildsOff(String),
}

pub fn opcode_to_hex(asm: &str) -> Option<u8> {
    match asm {
        "OP_PUSHBYTES_0" => Some(0x00),
        // Push the next byte as an array onto the stack
        "OP_PUSHBYTES_1" => Some(0x01),
        // Push the next 2 bytes as an array onto the stack
        "OP_PUSHBYTES_2" => Some(0x02),
        // Push the next 2 bytes as an array onto the stack
        "OP_PUSHBYTES_3" => Some(0x03),
        // Push the next 4 bytes as an array onto the stack
        "OP_PUSHBYTES_4" => Some(0x04),
        // Push the next 5 bytes as an array onto the stack
        "OP_PUSHBYTES_5" => Some(0x05),
        // Push the next 6 bytes as an array onto the stack
        "OP_PUSHBYTES_6" => Some(0x06),
        // Push the next 7 bytes as an array onto the stack
        "OP_PUSHBYTES_7" => Some(0x07),
        // Push the next 8 bytes as an array onto the stack
        "OP_PUSHBYTES_8" => Some(0x08),
        // Push the next 9 bytes as an array onto the stack
        "OP_PUSHBYTES_9" => Some(0x09),
        // Push the next 10 bytes as an array onto the stack
        "OP_PUSHBYTES_10" => Some(0x0a),
        // Push the next 11 bytes as an array onto the stack
        "OP_PUSHBYTES_11" => Some(0x0b),
        // Push the next 12 bytes as an array onto the stack
        "OP_PUSHBYTES_12" => Some(0x0c),
        // Push the next 13 bytes as an array onto the stack
        "OP_PUSHBYTES_13" => Some(0x0d),
        // Push the next 14 bytes as an array onto the stack
        "OP_PUSHBYTES_14" => Some(0x0e),
        // Push the next 15 bytes as an array onto the stack
        "OP_PUSHBYTES_15" => Some(0x0f),
        // Push the next 16 bytes as an array onto the stack
        "OP_PUSHBYTES_16" => Some(0x10),
        // Push the next 17 bytes as an array onto the stack
        "OP_PUSHBYTES_17" => Some(0x11),
        // Push the next 18 bytes as an array onto the stack
        "OP_PUSHBYTES_18" => Some(0x12),
        // Push the next 19 bytes as an array onto the stack
        "OP_PUSHBYTES_19" => Some(0x13),
        // Push the next 20 bytes as an array onto the stack
        "OP_PUSHBYTES_20" => Some(0x14),
        // Push the next 21 bytes as an array onto the stack
        "OP_PUSHBYTES_21" => Some(0x15),
        // Push the next 22 bytes as an array onto the stack
        "OP_PUSHBYTES_22" => Some(0x16),
        // Push the next 23 bytes as an array onto the stack
        "OP_PUSHBYTES_23" => Some(0x17),
        // Push the next 24 bytes as an array onto the stack
        "OP_PUSHBYTES_24" => Some(0x18),
        // Push the next 25 bytes as an array onto the stack
        "OP_PUSHBYTES_25" => Some(0x19),
        // Push the next 26 bytes as an array onto the stack
        "OP_PUSHBYTES_26" => Some(0x1a),
        // Push the next 27 bytes as an array onto the stack
        "OP_PUSHBYTES_27" => Some(0x1b),
        // Push the next 28 bytes as an array onto the stack
        "OP_PUSHBYTES_28" => Some(0x1c),
        // Push the next 29 bytes as an array onto the stack
        "OP_PUSHBYTES_29" => Some(0x1d),
        // Push the next 30 bytes as an array onto the stack
        "OP_PUSHBYTES_30" => Some(0x1e),
        // Push the next 31 bytes as an array onto the stack
        "OP_PUSHBYTES_31" => Some(0x1f),
        // Push the next 32 bytes as an array onto the stack
        "OP_PUSHBYTES_32" => Some(0x20),
        // Push the next 33 bytes as an array onto the stack
        "OP_PUSHBYTES_33" => Some(0x21),
        // Push the next 34 bytes as an array onto the stack
        "OP_PUSHBYTES_34" => Some(0x22),
        // Push the next 35 bytes as an array onto the stack
        "OP_PUSHBYTES_35" => Some(0x23),
        // Push the next 36 bytes as an array onto the stack
        "OP_PUSHBYTES_36" => Some(0x24),
        // Push the next 37 bytes as an array onto the stack
        "OP_PUSHBYTES_37" => Some(0x25),
        // Push the next 38 bytes as an array onto the stack
        "OP_PUSHBYTES_38" => Some(0x26),
        // Push the next 39 bytes as an array onto the stack
        "OP_PUSHBYTES_39" => Some(0x27),
        // Push the next 40 bytes as an array onto the stack
        "OP_PUSHBYTES_40" => Some(0x28),
        // Push the next 41 bytes as an array onto the stack
        "OP_PUSHBYTES_41" => Some(0x29),
        // Push the next 42 bytes as an array onto the stack
        "OP_PUSHBYTES_42" => Some(0x2a),
        // Push the next 43 bytes as an array onto the stack
        "OP_PUSHBYTES_43" => Some(0x2b),
        // Push the next 44 bytes as an array onto the stack
        "OP_PUSHBYTES_44" => Some(0x2c),
        // Push the next 45 bytes as an array onto the stack
        "OP_PUSHBYTES_45" => Some(0x2d),
        // Push the next 46 bytes as an array onto the stack
        "OP_PUSHBYTES_46" => Some(0x2e),
        // Push the next 47 bytes as an array onto the stack
        "OP_PUSHBYTES_47" => Some(0x2f),
        // Push the next 48 bytes as an array onto the stack
        "OP_PUSHBYTES_48" => Some(0x30),
        // Push the next 49 bytes as an array onto the stack
        "OP_PUSHBYTES_49" => Some(0x31),
        // Push the next 50 bytes as an array onto the stack
        "OP_PUSHBYTES_50" => Some(0x32),
        // Push the next 51 bytes as an array onto the stack
        "OP_PUSHBYTES_51" => Some(0x33),
        // Push the next 52 bytes as an array onto the stack
        "OP_PUSHBYTES_52" => Some(0x34),
        // Push the next 53 bytes as an array onto the stack
        "OP_PUSHBYTES_53" => Some(0x35),
        // Push the next 54 bytes as an array onto the stack
        "OP_PUSHBYTES_54" => Some(0x36),
        // Push the next 55 bytes as an array onto the stack
        "OP_PUSHBYTES_55" => Some(0x37),
        // Push the next 56 bytes as an array onto the stack
        "OP_PUSHBYTES_56" => Some(0x38),
        // Push the next 57 bytes as an array onto the stack
        "OP_PUSHBYTES_57" => Some(0x39),
        // Push the next 58 bytes as an array onto the stack
        "OP_PUSHBYTES_58" => Some(0x3a),
        // Push the next 59 bytes as an array onto the stack
        "OP_PUSHBYTES_59" => Some(0x3b),
        // Push the next 60 bytes as an array onto the stack
        "OP_PUSHBYTES_60" => Some(0x3c),
        // Push the next 61 bytes as an array onto the stack
        "OP_PUSHBYTES_61" => Some(0x3d),
        // Push the next 62 bytes as an array onto the stack
        "OP_PUSHBYTES_62" => Some(0x3e),
        // Push the next 63 bytes as an array onto the stack
        "OP_PUSHBYTES_63" => Some(0x3f),
        // Push the next 64 bytes as an array onto the stack
        "OP_PUSHBYTES_64" => Some(0x40),
        // Push the next 65 bytes as an array onto the stack
        "OP_PUSHBYTES_65" => Some(0x41),
        // Push the next 66 bytes as an array onto the stack
        "OP_PUSHBYTES_66" => Some(0x42),
        // Push the next 67 bytes as an array onto the stack
        "OP_PUSHBYTES_67" => Some(0x43),
        // Push the next 68 bytes as an array onto the stack
        "OP_PUSHBYTES_68" => Some(0x44),
        // Push the next 69 bytes as an array onto the stack
        "OP_PUSHBYTES_69" => Some(0x45),
        // Push the next 70 bytes as an array onto the stack
        "OP_PUSHBYTES_70" => Some(0x46),
        // Push the next 71 bytes as an array onto the stack
        "OP_PUSHBYTES_71" => Some(0x47),
        // Push the next 72 bytes as an array onto the stack
        "OP_PUSHBYTES_72" => Some(0x48),
        // Push the next 73 bytes as an array onto the stack
        "OP_PUSHBYTES_73" => Some(0x49),
        // Push the next 74 bytes as an array onto the stack
        "OP_PUSHBYTES_74" => Some(0x4a),
        // Push the next 75 bytes as an array onto the stack
        "OP_PUSHBYTES_75" => Some(0x4b),
        // Read the next byte as N; push the next N bytes as an array onto the stack
        "OP_PUSHDATA1" => Some(0x4c),
        // Read the next 2 bytes as N; push the next N bytes as an array onto the stack
        "OP_PUSHDATA2" => Some(0x4d),
        // Read the next 4 bytes as N; push the next N bytes as an array onto the stack
        "OP_PUSHDATA4" => Some(0x4e),
        // Push the array `0x81` onto the stack
        "OP_PUSHNUM_NEG1" => Some(0x4f),
        // Synonym for OP_RETURN
        "OP_RESERVED" => Some(0x50),
        // Push the number `0x01` onto the stack
        "OP_PUSHNUM_1" => Some(0x51),
        // Push the number `0x02` onto the stack
        "OP_PUSHNUM_2" => Some(0x52),
        // Push the number `0x03` onto the stack
        "OP_PUSHNUM_3" => Some(0x53),
        // Push the number `0x04` onto the stack
        "OP_PUSHNUM_4" => Some(0x54),
        // Push the number `0x05` onto the stack
        "OP_PUSHNUM_5" => Some(0x55),
        // Push the number `0x06` onto the stack
        "OP_PUSHNUM_6" => Some(0x56),
        // Push the number `0x07` onto the stack
        "OP_PUSHNUM_7" => Some(0x57),
        // Push the number `0x08` onto the stack
        "OP_PUSHNUM_8" => Some(0x58),
        // Push the number `0x09` onto the stack
        "OP_PUSHNUM_9" => Some(0x59),
        // Push the number `0x0a` onto the stack
        "OP_PUSHNUM_10" => Some(0x5a),
        // Push the number `0x0b` onto the stack
        "OP_PUSHNUM_11" => Some(0x5b),
        // Push the number `0x0c` onto the stack
        "OP_PUSHNUM_12" => Some(0x5c),
        // Push the number `0x0d` onto the stack
        "OP_PUSHNUM_13" => Some(0x5d),
        // Push the number `0x0e` onto the stack
        "OP_PUSHNUM_14" => Some(0x5e),
        // Push the number `0x0f` onto the stack
        "OP_PUSHNUM_15" => Some(0x5f),
        // Push the number `0x10` onto the stack
        "OP_PUSHNUM_16" => Some(0x60),
        // Does nothing
        "OP_NOP" => Some(0x61),
        // Synonym for OP_RETURN
        "OP_VER" => Some(0x62),
        // Pop and execute the next statements if a nonzero element was popped
        "OP_IF" => Some(0x63),
        // Pop and execute the next statements if a zero element was popped
        "OP_NOTIF" => Some(0x64),
        // Fail the script unconditionally, does not even need to be executed
        "OP_VERIF" => Some(0x65),
        // Fail the script unconditionally, does not even need to be executed
        "OP_VERNOTIF" => Some(0x66),
        // Execute statements if those after the previous OP_IF were not, and vice-versa.
        // If there is no previous OP_IF, this acts as a RETURN.
        "OP_ELSE" => Some(0x67),
        // Pop and execute the next statements if a zero element was popped
        "OP_ENDIF" => Some(0x68),
        // If the top value is zero or the stack is empty, fail; otherwise, pop the stack
        "OP_VERIFY" => Some(0x69),
        // Fail the script immediately. (Must be executed.)
        "OP_RETURN" => Some(0x6a),
        // Pop one element from the main stack onto the alt stack
        "OP_TOALTSTACK" => Some(0x6b),
        // Pop one element from the alt stack onto the main stack
        "OP_FROMALTSTACK" => Some(0x6c),
        // Drops the top two stack items
        "OP_2DROP" => Some(0x6d),
        // Duplicates the top two stack items as AB -> ABAB
        "OP_2DUP" => Some(0x6e),
        // Duplicates the two three stack items as ABC -> ABCABC
        "OP_3DUP" => Some(0x6f),
        // Copies the two stack items of items two spaces back to
        // the front, as xxAB -> ABxxAB
        "OP_2OVER" => Some(0x70),
        // Moves the two stack items four spaces back to the front,
        // as xxxxAB -> ABxxxx
        "OP_2ROT" => Some(0x71),
        // Swaps the top two pairs, as ABCD -> CDAB
        "OP_2SWAP" => Some(0x72),
        // Duplicate the top stack element unless it is zero
        "OP_IFDUP" => Some(0x73),
        // Push the current number of stack items onto the stack
        "OP_DEPTH" => Some(0x74),
        // Drops the top stack item
        "OP_DROP" => Some(0x75),
        // Duplicates the top stack item
        "OP_DUP" => Some(0x76),
        // Drops the second-to-top stack item
        "OP_NIP" => Some(0x77),
        // Copies the second-to-top stack item, as xA -> AxA
        "OP_OVER" => Some(0x78),
        // Pop the top stack element as N. Copy the Nth stack element to the top
        "OP_PICK" => Some(0x79),
        // Pop the top stack element as N. Move the Nth stack element to the top
        "OP_ROLL" => Some(0x7a),
        // Rotate the top three stack items, as [top next1 next2] -> [next2 top next1]
        "OP_ROT" => Some(0x7b),
        // Swap the top two stack items
        "OP_SWAP" => Some(0x7c),
        // Copy the top stack item to before the second item, as [top next] -> [top next top]
        "OP_TUCK" => Some(0x7d),
        // Fail the script unconditionally, does not even need to be executed
        "OP_CAT" => Some(0x7e),
        // Fail the script unconditionally, does not even need to be executed
        "OP_SUBSTR" => Some(0x7f),
        // Fail the script unconditionally, does not even need to be executed
        "OP_LEFT" => Some(0x80),
        // Fail the script unconditionally, does not even need to be executed
        "OP_RIGHT" => Some(0x81),
        // Pushes the length of the top stack item onto the stack
        "OP_SIZE" => Some(0x82),
        // Fail the script unconditionally, does not even need to be executed
        "OP_INVERT" => Some(0x83),
        // Fail the script unconditionally, does not even need to be executed
        "OP_AND" => Some(0x84),
        // Fail the script unconditionally, does not even need to be executed
        "OP_OR" => Some(0x85),
        // Fail the script unconditionally, does not even need to be executed
        "OP_XOR" => Some(0x86),
        // Pushes 1 if the inputs are exactly equal, 0 otherwise
        "OP_EQUAL" => Some(0x87),
        // Returns success if the inputs are exactly equal, failure otherwise
        "OP_EQUALVERIFY" => Some(0x88),
        // Synonym for OP_RETURN
        "OP_RESERVED1" => Some(0x89),
        // Synonym for OP_RETURN
        "OP_RESERVED2" => Some(0x8a),
        // Increment the top stack element in place
        "OP_1ADD" => Some(0x8b),
        // Decrement the top stack element in place
        "OP_1SUB" => Some(0x8c),
        // Fail the script unconditionally, does not even need to be executed
        "OP_2MUL" => Some(0x8d),
        // Fail the script unconditionally, does not even need to be executed
        "OP_2DIV" => Some(0x8e),
        // Multiply the top stack item by -1 in place
        "OP_NEGATE" => Some(0x8f),
        // Absolute value the top stack item in place
        "OP_ABS" => Some(0x90),
        // Map 0 to 1 and everything else to 0, in place
        "OP_NOT" => Some(0x91),
        // Map 0 to 0 and everything else to 1, in place
        "OP_0NOTEQUAL" => Some(0x92),
        // Pop two stack items and push their sum
        "OP_ADD" => Some(0x93),
        // Pop two stack items and push the second minus the top
        "OP_SUB" => Some(0x94),
        // Fail the script unconditionally, does not even need to be executed
        "OP_MUL" => Some(0x95),
        // Fail the script unconditionally, does not even need to be executed
        "OP_DIV" => Some(0x96),
        // Fail the script unconditionally, does not even need to be executed
        "OP_MOD" => Some(0x97),
        // Fail the script unconditionally, does not even need to be executed
        "OP_LSHIFT" => Some(0x98),
        // Fail the script unconditionally, does not even need to be executed
        "OP_RSHIFT" => Some(0x99),
        // Pop the top two stack items and push 1 if both are nonzero, else push 0
        "OP_BOOLAND" => Some(0x9a),
        // Pop the top two stack items and push 1 if either is nonzero, else push 0
        "OP_BOOLOR" => Some(0x9b),
        // Pop the top two stack items and push 1 if both are numerically equal, else push 0
        "OP_NUMEQUAL" => Some(0x9c),
        // Pop the top two stack items and return success if both are numerically equal, else return failure
        "OP_NUMEQUALVERIFY" => Some(0x9d),
        // Pop the top two stack items and push 0 if both are numerically equal, else push 1
        "OP_NUMNOTEQUAL" => Some(0x9e),
        // Pop the top two items; push 1 if the second is less than the top, 0 otherwise
        "OP_LESSTHAN" => Some(0x9f),
        // Pop the top two items; push 1 if the second is greater than the top, 0 otherwise
        "OP_GREATERTHAN" => Some(0xa0),
        // Pop the top two items; push 1 if the second is <= the top, 0 otherwise
        "OP_LESSTHANOREQUAL" => Some(0xa1),
        // Pop the top two items; push 1 if the second is >= the top, 0 otherwise
        "OP_GREATERTHANOREQUAL" => Some(0xa2),
        // Pop the top two items; push the smaller
        "OP_MIN" => Some(0xa3),
        // Pop the top two items; push the larger
        "OP_MAX" => Some(0xa4),
        // Pop the top three items; if the top is >= the second and < the third, push 1, otherwise push 0
        "OP_WITHIN" => Some(0xa5),
        // Pop the top stack item and push its RIPEMD160 hash
        "OP_RIPEMD160" => Some(0xa6),
        // Pop the top stack item and push its SHA1 hash
        "OP_SHA1" => Some(0xa7),
        // Pop the top stack item and push its SHA256 hash
        "OP_SHA256" => Some(0xa8),
        // Pop the top stack item and push its RIPEMD(SHA256) hash
        "OP_HASH160" => Some(0xa9),
        // Pop the top stack item and push its SHA256(SHA256) hash
        "OP_HASH256" => Some(0xaa),
        // Ignore this and everything preceding when deciding what to sign when signature-checking
        "OP_CODESEPARATOR" => Some(0xab),
        // <https://en.bitcoin.it/wiki/OP_CHECKSIG> pushing 1/0 for success/failure
        "OP_CHECKSIG" => Some(0xac),
        // <https://en.bitcoin.it/wiki/OP_CHECKSIG> returning success/failure
        "OP_CHECKSIGVERIFY" => Some(0xad),
        // Pop N, N pubkeys, M, M signatures, a dummy (due to bug in reference code), and verify that all M signatures are valid.
        // Push 1 for "all valid", 0 otherwise
        "OP_CHECKMULTISIG" => Some(0xae),
        // Like the above but return success/failure
        "OP_CHECKMULTISIGVERIFY" => Some(0xaf),
        // Does nothing
        "OP_NOP1" => Some(0xb0),
        // <https://github.com/bitcoin/bips/blob/master/bip-0065.mediawiki>
        "OP_CLTV" => Some(0xb1),
        // <https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki>
        "OP_CSV" => Some(0xb2),
        // Does nothing
        "OP_NOP4" => Some(0xb3),
        // Does nothing
        "OP_NOP5" => Some(0xb4),
        // Does nothing
        "OP_NOP6" => Some(0xb5),
        // Does nothing
        "OP_NOP7" => Some(0xb6),
        // Does nothing
        "OP_NOP8" => Some(0xb7),
        // Does nothing
        "OP_NOP9" => Some(0xb8),
        // Does nothing
        "OP_NOP10" => Some(0xb9),
        // Every other opcode acts as OP_RETURN
        // Synonym for OP_RETURN
        "OP_RETURN_186" => Some(0xba),
        // Synonym for OP_RETURN
        "OP_RETURN_187" => Some(0xbb),
        // Synonym for OP_RETURN
        "OP_RETURN_188" => Some(0xbc),
        // Synonym for OP_RETURN
        "OP_RETURN_189" => Some(0xbd),
        // Synonym for OP_RETURN
        "OP_RETURN_190" => Some(0xbe),
        // Synonym for OP_RETURN
        "OP_RETURN_191" => Some(0xbf),
        // Synonym for OP_RETURN
        "OP_RETURN_192" => Some(0xc0),
        // Synonym for OP_RETURN
        "OP_RETURN_193" => Some(0xc1),
        // Synonym for OP_RETURN
        "OP_RETURN_194" => Some(0xc2),
        // Synonym for OP_RETURN
        "OP_RETURN_195" => Some(0xc3),
        // Synonym for OP_RETURN
        "OP_RETURN_196" => Some(0xc4),
        // Synonym for OP_RETURN
        "OP_RETURN_197" => Some(0xc5),
        // Synonym for OP_RETURN
        "OP_RETURN_198" => Some(0xc6),
        // Synonym for OP_RETURN
        "OP_RETURN_199" => Some(0xc7),
        // Synonym for OP_RETURN
        "OP_RETURN_200" => Some(0xc8),
        // Synonym for OP_RETURN
        "OP_RETURN_201" => Some(0xc9),
        // Synonym for OP_RETURN
        "OP_RETURN_202" => Some(0xca),
        // Synonym for OP_RETURN
        "OP_RETURN_203" => Some(0xcb),
        // Synonym for OP_RETURN
        "OP_RETURN_204" => Some(0xcc),
        // Synonym for OP_RETURN
        "OP_RETURN_205" => Some(0xcd),
        // Synonym for OP_RETURN
        "OP_RETURN_206" => Some(0xce),
        // Synonym for OP_RETURN
        "OP_RETURN_207" => Some(0xcf),
        // Synonym for OP_RETURN
        "OP_RETURN_208" => Some(0xd0),
        // Synonym for OP_RETURN
        "OP_RETURN_209" => Some(0xd1),
        // Synonym for OP_RETURN
        "OP_RETURN_210" => Some(0xd2),
        // Synonym for OP_RETURN
        "OP_RETURN_211" => Some(0xd3),
        // Synonym for OP_RETURN
        "OP_RETURN_212" => Some(0xd4),
        // Synonym for OP_RETURN
        "OP_RETURN_213" => Some(0xd5),
        // Synonym for OP_RETURN
        "OP_RETURN_214" => Some(0xd6),
        // Synonym for OP_RETURN
        "OP_RETURN_215" => Some(0xd7),
        // Synonym for OP_RETURN
        "OP_RETURN_216" => Some(0xd8),
        // Synonym for OP_RETURN
        "OP_RETURN_217" => Some(0xd9),
        // Synonym for OP_RETURN
        "OP_RETURN_218" => Some(0xda),
        // Synonym for OP_RETURN
        "OP_RETURN_219" => Some(0xdb),
        // Synonym for OP_RETURN
        "OP_RETURN_220" => Some(0xdc),
        // Synonym for OP_RETURN
        "OP_RETURN_221" => Some(0xdd),
        // Synonym for OP_RETURN
        "OP_RETURN_222" => Some(0xde),
        // Synonym for OP_RETURN
        "OP_RETURN_223" => Some(0xdf),
        // Synonym for OP_RETURN
        "OP_RETURN_224" => Some(0xe0),
        // Synonym for OP_RETURN
        "OP_RETURN_225" => Some(0xe1),
        // Synonym for OP_RETURN
        "OP_RETURN_226" => Some(0xe2),
        // Synonym for OP_RETURN
        "OP_RETURN_227" => Some(0xe3),
        // Synonym for OP_RETURN
        "OP_RETURN_228" => Some(0xe4),
        // Synonym for OP_RETURN
        "OP_RETURN_229" => Some(0xe5),
        // Synonym for OP_RETURN
        "OP_RETURN_230" => Some(0xe6),
        // Synonym for OP_RETURN
        "OP_RETURN_231" => Some(0xe7),
        // Synonym for OP_RETURN
        "OP_RETURN_232" => Some(0xe8),
        // Synonym for OP_RETURN
        "OP_RETURN_233" => Some(0xe9),
        // Synonym for OP_RETURN
        "OP_RETURN_234" => Some(0xea),
        // Synonym for OP_RETURN
        "OP_RETURN_235" => Some(0xeb),
        // Synonym for OP_RETURN
        "OP_RETURN_236" => Some(0xec),
        // Synonym for OP_RETURN
        "OP_RETURN_237" => Some(0xed),
        // Synonym for OP_RETURN
        "OP_RETURN_238" => Some(0xee),
        // Synonym for OP_RETURN
        "OP_RETURN_239" => Some(0xef),
        // Synonym for OP_RETURN
        "OP_RETURN_240" => Some(0xf0),
        // Synonym for OP_RETURN
        "OP_RETURN_241" => Some(0xf1),
        // Synonym for OP_RETURN
        "OP_RETURN_242" => Some(0xf2),
        // Synonym for OP_RETURN
        "OP_RETURN_243" => Some(0xf3),
        // Synonym for OP_RETURN
        "OP_RETURN_244" => Some(0xf4),
        // Synonym for OP_RETURN
        "OP_RETURN_245" => Some(0xf5),
        // Synonym for OP_RETURN
        "OP_RETURN_246" => Some(0xf6),
        // Synonym for OP_RETURN
        "OP_RETURN_247" => Some(0xf7),
        // Synonym for OP_RETURN
        "OP_RETURN_248" => Some(0xf8),
        // Synonym for OP_RETURN
        "OP_RETURN_249" => Some(0xf9),
        // Synonym for OP_RETURN
        "OP_RETURN_250" => Some(0xfa),
        // Synonym for OP_RETURN
        "OP_RETURN_251" => Some(0xfb),
        // Synonym for OP_RETURN
        "OP_RETURN_252" => Some(0xfc),
        // Synonym for OP_RETURN
        "OP_RETURN_253" => Some(0xfd),
        // Synonym for OP_RETURN
        "OP_RETURN_254" => Some(0xfe),
        // Synonym for OP_RETURN
        "OP_RETURN_255" => Some(0xff),
        _ => None,
    }
}

pub fn append_error_context(context: &str, errors: Vec<String>) -> Vec<String> {
    errors
        .iter()
        .map(|e| format!("{}: {}", context, e))
        .collect()
}

pub fn is_hex(s: &str) -> bool {
    s.chars().all(|c| c.is_ascii_hexdigit())
}

pub fn validate_txid(txid: &str) -> Result<(), String> {
    if !txid.starts_with("0x") || txid.len() != 66 || !is_hex(&txid[2..66]) {
        return Err(
            "txid must be a 32 byte (64 character) hexadecimal string prefixed with '0x'".into(),
        );
    }
    Ok(())
}
