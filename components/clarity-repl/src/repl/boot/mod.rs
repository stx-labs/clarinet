// Copyright (C) 2013-2020 Blockstack PBC, a public benefit corporation
// Copyright (C) 2020 Stacks Open Internet Foundation
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// This code is inspired from stacks-blockchain/src/chainstate/atacks/boot/mod.rs

const BOOT_CODE_GENESIS: &str = std::include_str!("genesis.clar");
const BOOT_CODE_BNS: &str = std::include_str!("bns.clar");
const BOOT_CODE_LOCKUP: &str = std::include_str!("lockup.clar");

const BOOT_CODE_COSTS: &str = std::include_str!("costs.clar");
const BOOT_CODE_COSTS_2: &str = std::include_str!("costs-2.clar");
const BOOT_CODE_COSTS_2_TESTNET: &str = std::include_str!("costs-2-testnet.clar");
const BOOT_CODE_COSTS_3: &str = std::include_str!("costs-3.clar");
const BOOT_CODE_COSTS_4: &str = std::include_str!("costs-4.clar");
const BOOT_CODE_COST_VOTING_MAINNET: &str = std::include_str!("cost-voting.clar");

const POX_TESTNET: &str = std::include_str!("pox-testnet.clar");
const POX_MAINNET: &str = std::include_str!("pox-mainnet.clar");
const POX_BODY: &str = std::include_str!("pox.clar");
const POX_2_BODY: &str = std::include_str!("pox-2.clar");
const POX_3_BODY: &str = std::include_str!("pox-3.clar");
const POX_4_BODY: &str = std::include_str!("pox-4.clar");

const BOOT_CODE_SIGNERS: &str = std::include_str!("signers.clar");
const BOOT_CODE_SIGNERS_VOTING: &str = std::include_str!("signers-voting.clar");

// sBTC contracts are not boot contracts
// but we want to handle a similar behavior for contract addresses mapping
pub const SBTC_CONTRACTS_NAMES: &[&str] = &["sbtc-registry", "sbtc-token", "sbtc-deposit"];

pub const SBTC_TESTNET_ADDRESS: &str = "ST1F7QA2MDF17S807EPA36TSS8AMEFY4KA9TVGWXT";
pub const SBTC_MAINNET_ADDRESS: &str = "SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4";

pub static SBTC_TESTNET_ADDRESS_PRINCIPAL: LazyLock<StandardPrincipalData> =
    LazyLock::new(|| PrincipalData::parse_standard_principal(SBTC_TESTNET_ADDRESS).unwrap());

pub static SBTC_DEPOSIT_MAINNET_ADDRESS: LazyLock<QualifiedContractIdentifier> =
    LazyLock::new(|| {
        QualifiedContractIdentifier::parse(&format!("{SBTC_MAINNET_ADDRESS}.sbtc-deposit")).unwrap()
    });

pub static SBTC_TOKEN_MAINNET_ADDRESS: LazyLock<QualifiedContractIdentifier> =
    LazyLock::new(|| {
        QualifiedContractIdentifier::parse(&format!("{SBTC_MAINNET_ADDRESS}.sbtc-token")).unwrap()
    });

use std::collections::BTreeMap;
use std::fs;
use std::sync::LazyLock;

use clarity::types::StacksEpochId;
use clarity::vm::ast::ContractAST;
use clarity::vm::ClarityVersion;
use clarity_types::types::{PrincipalData, QualifiedContractIdentifier, StandardPrincipalData};

use crate::repl::{
    ClarityCodeSource, ClarityContract, ClarityInterpreter, ContractDeployer, Epoch, Settings,
};

fn make_testnet_cost_voting() -> String {
    BOOT_CODE_COST_VOTING_MAINNET
        .replacen(
            "(define-constant VETO_LENGTH u1008)",
            "(define-constant VETO_LENGTH u50)",
            1,
        )
        .replacen(
            "(define-constant REQUIRED_VETOES u500)",
            "(define-constant REQUIRED_VETOES u25)",
            1,
        )
}

static BOOT_CODE_POX_MAINNET: LazyLock<String> =
    LazyLock::new(|| format!("{POX_MAINNET}\n{POX_BODY}"));
static BOOT_CODE_POX_TESTNET: LazyLock<String> =
    LazyLock::new(|| format!("{POX_TESTNET}\n{POX_BODY}"));
static BOOT_CODE_POX_2_MAINNET: LazyLock<String> =
    LazyLock::new(|| format!("{POX_MAINNET}\n{POX_2_BODY}"));
static BOOT_CODE_POX_2_TESTNET: LazyLock<String> =
    LazyLock::new(|| format!("{POX_TESTNET}\n{POX_2_BODY}"));
static BOOT_CODE_POX_3_MAINNET: LazyLock<String> =
    LazyLock::new(|| format!("{POX_MAINNET}\n{POX_3_BODY}"));
static BOOT_CODE_POX_3_TESTNET: LazyLock<String> =
    LazyLock::new(|| format!("{POX_TESTNET}\n{POX_3_BODY}"));
static BOOT_CODE_COST_VOTING_TESTNET: LazyLock<String> = LazyLock::new(make_testnet_cost_voting);

pub static BOOT_CODE_MAINNET: LazyLock<[(&'static str, &'static str); 14]> = LazyLock::new(|| {
    [
        ("pox", &BOOT_CODE_POX_MAINNET),
        ("lockup", BOOT_CODE_LOCKUP),
        ("costs", BOOT_CODE_COSTS),
        ("cost-voting", BOOT_CODE_COST_VOTING_MAINNET),
        ("bns", BOOT_CODE_BNS),
        ("genesis", BOOT_CODE_GENESIS),
        ("costs-2", BOOT_CODE_COSTS_2),
        ("pox-2", &BOOT_CODE_POX_2_MAINNET),
        ("costs-3", BOOT_CODE_COSTS_3),
        ("pox-3", &BOOT_CODE_POX_3_MAINNET),
        ("pox-4", POX_4_BODY),
        ("signers", BOOT_CODE_SIGNERS),
        ("signers-voting", BOOT_CODE_SIGNERS_VOTING),
        ("costs-4", BOOT_CODE_COSTS_4),
    ]
});

pub static BOOT_CODE_TESTNET: LazyLock<[(&'static str, &'static str); 14]> = LazyLock::new(|| {
    [
        ("pox", &BOOT_CODE_POX_TESTNET),
        ("lockup", BOOT_CODE_LOCKUP),
        ("costs", BOOT_CODE_COSTS),
        ("cost-voting", &BOOT_CODE_COST_VOTING_TESTNET),
        ("bns", BOOT_CODE_BNS),
        ("genesis", BOOT_CODE_GENESIS),
        ("costs-2", BOOT_CODE_COSTS_2_TESTNET),
        ("pox-2", &BOOT_CODE_POX_2_TESTNET),
        ("costs-3", BOOT_CODE_COSTS_3),
        ("pox-3", &BOOT_CODE_POX_3_TESTNET),
        ("pox-4", POX_4_BODY),
        ("signers", BOOT_CODE_SIGNERS),
        ("signers-voting", BOOT_CODE_SIGNERS_VOTING),
        ("costs-4", BOOT_CODE_COSTS_4),
    ]
});

pub const BOOT_TESTNET_ADDRESS: &str = "ST000000000000000000002AMW42H";
pub const BOOT_MAINNET_ADDRESS: &str = "SP000000000000000000002Q6VF78";

pub const BOOT_CONTRACTS_NAMES: &[&str] = &[
    "genesis",
    "lockup",
    "bns",
    "cost-voting",
    "costs",
    "pox",
    "costs-2",
    "pox-2",
    "costs-3",
    "pox-3",
    "pox-4",
    "signers",
    "signers-voting",
    "costs-4",
];

pub static BOOT_TESTNET_PRINCIPAL: LazyLock<StandardPrincipalData> =
    LazyLock::new(|| PrincipalData::parse_standard_principal(BOOT_TESTNET_ADDRESS).unwrap());
pub static BOOT_MAINNET_PRINCIPAL: LazyLock<StandardPrincipalData> =
    LazyLock::new(|| PrincipalData::parse_standard_principal(BOOT_MAINNET_ADDRESS).unwrap());
pub static BOOT_CONTRACTS_DATA: LazyLock<
    BTreeMap<QualifiedContractIdentifier, (ClarityContract, ContractAST)>,
> = LazyLock::new(|| {
    let mut result = BTreeMap::new();
    let deploy: [(&StandardPrincipalData, [(&str, &str); 14]); 2] = [
        (&*BOOT_TESTNET_PRINCIPAL, *BOOT_CODE_TESTNET),
        (&*BOOT_MAINNET_PRINCIPAL, *BOOT_CODE_MAINNET),
    ];

    let interpreter = ClarityInterpreter::new(
        StandardPrincipalData::transient(),
        Settings::default(),
        None,
    );
    for (deployer, boot_code) in deploy.iter() {
        for (name, code) in boot_code.iter() {
            let (epoch, clarity_version) = get_boot_contract_epoch_and_clarity_version(name);
            let boot_contract = ClarityContract {
                code_source: ClarityCodeSource::ContractInMemory(code.to_string()),
                deployer: ContractDeployer::Address(deployer.to_address()),
                name: name.to_string(),
                epoch: Epoch::Specific(epoch),
                clarity_version,
            };
            let (ast, _, _) = interpreter.build_ast(&boot_contract);
            result.insert(
                boot_contract.expect_resolved_contract_identifier(None),
                (boot_contract, ast),
            );
        }
    }
    result
});

pub fn load_custom_boot_contract(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("Failed to read boot contract file {path}: {e}"))
}

/// Get boot contracts data with optional overrides (only existing boot contracts can be overridden)
pub fn get_boot_contracts_data_with_overrides(
    overrides: &BTreeMap<String, String>,
) -> BTreeMap<QualifiedContractIdentifier, (ClarityContract, ContractAST)> {
    let mut result = BOOT_CONTRACTS_DATA.clone();

    let interpreter = ClarityInterpreter::new(
        StandardPrincipalData::transient(),
        Settings::default(),
        None,
    );

    for (contract_name, file_path) in overrides {
        if !BOOT_CONTRACTS_NAMES.contains(&contract_name.as_str()) {
            eprintln!("Warning: Skipping custom boot contract '{contract_name}' - only existing boot contracts can be overridden. Valid boot contracts are: {BOOT_CONTRACTS_NAMES:?}");
            continue;
        }

        let custom_source = match load_custom_boot_contract(file_path) {
            Ok(source) => source,
            Err(e) => {
                eprintln!("Warning: Failed to load custom boot contract {contract_name}: {e}");
                continue;
            }
        };

        // Use standard epoch/version mapping for known boot contracts
        let (epoch, clarity_version) =
            get_boot_contract_epoch_and_clarity_version(contract_name.as_str());

        for deployer in [&*BOOT_TESTNET_PRINCIPAL, &*BOOT_MAINNET_PRINCIPAL] {
            let boot_contract = ClarityContract {
                code_source: ClarityCodeSource::ContractInMemory(custom_source.clone()),
                deployer: ContractDeployer::Address(deployer.to_address()),
                name: contract_name.clone(),
                epoch: Epoch::Specific(epoch),
                clarity_version,
            };

            let (ast, _, _) = interpreter.build_ast(&boot_contract);
            let contract_id = boot_contract.expect_resolved_contract_identifier(None);

            // Insert the contract (this will replace the existing boot contract)
            result.insert(contract_id, (boot_contract, ast));
        }
    }
    result
}

pub fn get_boot_contract_epoch_and_clarity_version(
    contract_name: &str,
) -> (StacksEpochId, ClarityVersion) {
    let (epoch, clarity_version) = match contract_name {
        "costs-4" => (StacksEpochId::Epoch33, ClarityVersion::Clarity4),
        "pox-4" | "signers" | "signers-voting" => {
            (StacksEpochId::Epoch25, ClarityVersion::Clarity2)
        }
        "pox-3" => (StacksEpochId::Epoch24, ClarityVersion::Clarity2),
        "pox-2" | "costs-3" => (StacksEpochId::Epoch21, ClarityVersion::Clarity2),
        "costs-2" => (StacksEpochId::Epoch2_05, ClarityVersion::Clarity1),
        "genesis" | "lockup" | "bns" | "cost-voting" | "costs" | "pox" => {
            (StacksEpochId::Epoch20, ClarityVersion::Clarity1)
        }
        _ => {
            panic!(
                "Unknown boot contract '{}' - cannot validate",
                contract_name
            );
        }
    };
    (epoch, clarity_version)
}
