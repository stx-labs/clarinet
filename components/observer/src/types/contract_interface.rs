// NOTE: This module is a very slightly simplified version of the
// `clarity-vm` repository's [ContractInterface](https://github.com/stacks-network/stacks-blockchain/blob/eca1cfe81f0c0989ebd3e53c32e3e5d70ed83757/clarity/src/vm/analysis/contract_interface_builder/mod.rs#L368) type.
// We've copied it here rather than using `clarity-vm` as a dependency to avoid circular dependencies.

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContractInterface {
    pub functions: Vec<ContractInterfaceFunction>,
    pub variables: Vec<ContractInterfaceVariable>,
    pub maps: Vec<ContractInterfaceMap>,
    pub fungible_tokens: Vec<ContractInterfaceFungibleTokens>,
    pub non_fungible_tokens: Vec<ContractInterfaceNonFungibleTokens>,
    pub epoch: String,
    pub clarity_version: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContractInterfaceFunction {
    pub name: String,
    pub access: ContractInterfaceFunctionAccess,
    pub args: Vec<ContractInterfaceFunctionArg>,
    pub outputs: ContractInterfaceFunctionOutput,
}
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ContractInterfaceFunctionAccess {
    private,
    public,
    read_only,
}
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ContractInterfaceVariableAccess {
    constant,
    variable,
}
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContractInterfaceFunctionArg {
    pub name: String,
    #[serde(rename = "type")]
    pub type_f: ContractInterfaceAtomType,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContractInterfaceFunctionOutput {
    #[serde(rename = "type")]
    pub type_f: ContractInterfaceAtomType,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContractInterfaceVariable {
    pub name: String,
    #[serde(rename = "type")]
    pub type_f: ContractInterfaceAtomType,
    pub access: ContractInterfaceVariableAccess,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContractInterfaceMap {
    pub name: String,
    pub key: ContractInterfaceAtomType,
    pub value: ContractInterfaceAtomType,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ContractInterfaceAtomType {
    none,
    int128,
    uint128,
    bool,
    principal,
    buffer {
        length: u32,
    },
    #[serde(rename = "string-utf8")]
    string_utf8 {
        length: u32,
    },
    #[serde(rename = "string-ascii")]
    string_ascii {
        length: u32,
    },
    tuple(Vec<ContractInterfaceTupleEntryType>),
    optional(Box<ContractInterfaceAtomType>),
    response {
        ok: Box<ContractInterfaceAtomType>,
        error: Box<ContractInterfaceAtomType>,
    },
    list {
        #[serde(rename = "type")]
        type_f: Box<ContractInterfaceAtomType>,
        length: u32,
    },
    trait_reference,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContractInterfaceTupleEntryType {
    pub name: String,
    #[serde(rename = "type")]
    pub type_f: ContractInterfaceAtomType,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContractInterfaceFungibleTokens {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContractInterfaceNonFungibleTokens {
    pub name: String,
    #[serde(rename = "type")]
    pub type_f: ContractInterfaceAtomType,
}
