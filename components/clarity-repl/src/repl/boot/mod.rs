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
const POX_5_BODY: &str = std::include_str!("pox-5.clar");

const BOOT_CODE_SIGNERS: &str = std::include_str!("signers.clar");
const BOOT_CODE_SIGNERS_VOTING: &str = std::include_str!("signers-voting.clar");

/// mainnet sBTC contract sources
/// (from SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4).
/// Deployed as boot dependencies so pox-5 can resolve its `contract-call?`
/// and `with-ft` references to sbtc-token, and so genesis `sbtc_balance`
/// can be minted through sbtc-deposit.
const SBTC_REGISTRY_SOURCE: &str = std::include_str!("sbtc-registry.clar");
const SBTC_TOKEN_SOURCE: &str = std::include_str!("sbtc-token.clar");
const SBTC_DEPOSIT_SOURCE: &str = std::include_str!("sbtc-deposit.clar");

/// The sBTC contracts deployed as boot contracts, in deployment order.
pub const SBTC_CONTRACTS_NAMES: &[&str] = &["sbtc-registry", "sbtc-token", "sbtc-deposit"];

pub const SBTC_TESTNET_ADDRESS: &str = "SN3VMHXEN64ZZF71JQ5VESXDWTR301XTTXGF4J8F1";
pub const SBTC_MAINNET_ADDRESS: &str = "SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4";

pub static SBTC_TESTNET_ADDRESS_PRINCIPAL: LazyLock<StandardPrincipalData> =
    LazyLock::new(|| PrincipalData::parse_standard_principal(SBTC_TESTNET_ADDRESS).unwrap());

pub static SBTC_MAINNET_ADDRESS_PRINCIPAL: LazyLock<StandardPrincipalData> =
    LazyLock::new(|| PrincipalData::parse_standard_principal(SBTC_MAINNET_ADDRESS).unwrap());

pub static SBTC_DEPOSIT_MAINNET_ADDRESS: LazyLock<QualifiedContractIdentifier> =
    LazyLock::new(|| {
        QualifiedContractIdentifier::parse(&format!("{SBTC_MAINNET_ADDRESS}.sbtc-deposit")).unwrap()
    });

pub static SBTC_TOKEN_MAINNET_ADDRESS: LazyLock<QualifiedContractIdentifier> =
    LazyLock::new(|| {
        QualifiedContractIdentifier::parse(&format!("{SBTC_MAINNET_ADDRESS}.sbtc-token")).unwrap()
    });

use std::collections::BTreeMap;
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

/// mainnet bond-admin principal baked into pox-5 source.
const POX_5_BOND_ADMIN_MAINNET: &str = "SP000000000000000000002Q6VF78";

/// Build the testnet pox-5 body by rewriting the bond-admin to the
/// simnet/testnet equivalent.
fn make_pox_5_testnet() -> String {
    // Only rewrite the bond-admin principal. The sBTC contract reference
    // keeps the mainnet address (SM3VDXK3...) because on simnet sbtc-token
    // is only deployed at that address
    POX_5_BODY.replace(POX_5_BOND_ADMIN_MAINNET, BOOT_TESTNET_ADDRESS)
}

static BOOT_CODE_POX_5_TESTNET: LazyLock<String> = LazyLock::new(make_pox_5_testnet);

pub static BOOT_CODE_MAINNET: LazyLock<[(&'static str, &'static str); 15]> = LazyLock::new(|| {
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
        ("pox-5", POX_5_BODY),
    ]
});

pub static BOOT_CODE_TESTNET: LazyLock<[(&'static str, &'static str); 15]> = LazyLock::new(|| {
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
        ("pox-5", &BOOT_CODE_POX_5_TESTNET),
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
    "pox-5",
];

pub static BOOT_TESTNET_PRINCIPAL: LazyLock<StandardPrincipalData> =
    LazyLock::new(|| PrincipalData::parse_standard_principal(BOOT_TESTNET_ADDRESS).unwrap());
pub static BOOT_MAINNET_PRINCIPAL: LazyLock<StandardPrincipalData> =
    LazyLock::new(|| PrincipalData::parse_standard_principal(BOOT_MAINNET_ADDRESS).unwrap());

/// The testnet twin of a mainnet boot contract, or `None` if `contract_id`
/// isn't one.
///
/// Simnet deploys every boot contract under both addresses, but its chain
/// state is testnet-flavored: stacks-core's PoX handler keys off
/// `GlobalContext::mainnet`, so only the `ST000...` contracts move consensus
/// state. Redirecting to the twin makes a mainnet-addressed call behave.
///
/// Scoped to [`BOOT_CONTRACTS_NAMES`], the same rule
/// [`remap_mainnet_boot_principals`] applies to source: sBTC lives at
/// `SM3VDXK3...` and has no testnet twin in simnet, so it never matches.
pub fn remap_mainnet_boot_contract_id(
    contract_id: &QualifiedContractIdentifier,
) -> Option<QualifiedContractIdentifier> {
    let is_mainnet_boot = contract_id.issuer == *BOOT_MAINNET_PRINCIPAL
        && BOOT_CONTRACTS_NAMES.contains(&contract_id.name.as_str());

    is_mainnet_boot.then(|| {
        QualifiedContractIdentifier::new(BOOT_TESTNET_PRINCIPAL.clone(), contract_id.name.clone())
    })
}

/// Characters Clarity allows in a contract name.
fn is_contract_name_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '-' || c == '_'
}

/// The boot contract name in a `SP000000000000000000002Q6VF78.<name>` prefix
/// of `s`, or `None` if `s` doesn't start with one.
///
/// The name is read greedily, so `.pox-5` can never be truncated to the
/// shorter boot name `.pox`, and a project's own `.pox-helper` is rejected
/// outright rather than matched as a prefix.
fn mainnet_boot_contract_name_at(s: &str) -> Option<&str> {
    let rest = s.strip_prefix(BOOT_MAINNET_ADDRESS)?.strip_prefix('.')?;
    let name = &rest[..rest.len() - rest.trim_start_matches(is_contract_name_char).len()];
    BOOT_CONTRACTS_NAMES.contains(&name).then_some(name)
}

/// Rewrite every mainnet boot-contract principal literal in `source` to its
/// testnet twin, returning `None` when there is nothing to rewrite.
///
/// Deliberately not a `str::replace`. Only full principal literals
/// (`'SP000000000000000000002Q6VF78.<boot-name>`) match, so:
///
/// - the bare mainnet burn address `'SP000000000000000000002Q6VF78`, a real
///   transfer target, is left alone — a contract name is required;
/// - the name must be a [`BOOT_CONTRACTS_NAMES`] entry, which keeps sBTC and
///   every requirement contract out of scope;
/// - occurrences inside `;;` comments and string literals are skipped;
/// - the leading `'` is required, which is what separates a principal literal
///   from an address that merely appears in text.
///
/// Both addresses are 29 characters, so the rewrite preserves every span in
/// the source — diagnostics and coverage line/column data stay valid.
///
/// Callers apply this to *user* sources only — manifest contracts (via
/// `clarinet-deployments`) and `simnet.deployContract`. It deliberately does
/// not live inside `Session::deploy_contract`, which also deploys the boot
/// contracts themselves: rewriting those would point the mainnet boot set at
/// the testnet one.
///
/// Idempotent: the output holds no mainnet boot literals, so a second pass
/// returns `None`.
pub fn remap_mainnet_boot_principals(source: &str) -> Option<String> {
    // Cheap reject for the overwhelmingly common case.
    if !source.contains(BOOT_MAINNET_ADDRESS) {
        return None;
    }

    let bytes = source.as_bytes();
    let mut out = String::with_capacity(source.len());
    let mut remapped = false;
    let mut i = 0;

    // `i` stays on a char boundary throughout: every arm advances either by a
    // whole char or to an offset just past an ASCII delimiter.
    while i < bytes.len() {
        match bytes[i] {
            // `;;` runs to the end of the line.
            b';' if bytes.get(i + 1) == Some(&b';') => {
                let end = source[i..]
                    .find('\n')
                    .map_or(bytes.len(), |eol| i + eol + 1);
                out.push_str(&source[i..end]);
                i = end;
            }
            // A string literal, `"..."` or the tail of `u"..."`.
            b'"' => {
                out.push('"');
                i += 1;
                let mut escaped = false;
                while let Some(c) = source[i..].chars().next() {
                    out.push(c);
                    i += c.len_utf8();
                    match c {
                        _ if escaped => escaped = false,
                        '\\' => escaped = true,
                        '"' => break,
                        _ => {}
                    }
                }
            }
            // A principal literal is the only place a boot address is code.
            b'\'' => {
                out.push('\'');
                i += 1;
                if let Some(name) = mainnet_boot_contract_name_at(&source[i..]) {
                    out.push_str(BOOT_TESTNET_ADDRESS);
                    out.push('.');
                    out.push_str(name);
                    i += BOOT_MAINNET_ADDRESS.len() + 1 + name.len();
                    remapped = true;
                }
            }
            _ => {
                let c = source[i..]
                    .chars()
                    .next()
                    .expect("i is within bytes and on a char boundary");
                out.push(c);
                i += c.len_utf8();
            }
        }
    }

    remapped.then_some(out)
}
pub static BOOT_CONTRACTS_DATA: LazyLock<
    BTreeMap<QualifiedContractIdentifier, (ClarityContract, ContractAST)>,
> = LazyLock::new(|| {
    let mut result = BTreeMap::new();
    let deploy: [(&StandardPrincipalData, [(&str, &str); 15]); 2] = [
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
                skip_analysis: true,
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

/// `overrides` maps boot contract name → Clarity source code. File I/O is the
/// caller's responsibility so this stays runtime-agnostic — `wasm32-unknown-unknown`
/// has no working `std::fs`.
pub fn get_boot_contracts_data_with_overrides(
    overrides: &BTreeMap<String, String>,
) -> BTreeMap<QualifiedContractIdentifier, (ClarityContract, ContractAST)> {
    let mut result = BOOT_CONTRACTS_DATA.clone();

    let interpreter = ClarityInterpreter::new(
        StandardPrincipalData::transient(),
        Settings::default(),
        None,
    );

    for (contract_name, custom_source) in overrides {
        if !BOOT_CONTRACTS_NAMES.contains(&contract_name.as_str()) {
            ueprint!("Warning: Skipping custom boot contract '{contract_name}' - only existing boot contracts can be overridden. Valid boot contracts are: {BOOT_CONTRACTS_NAMES:?}");
            continue;
        }

        let (epoch, clarity_version) =
            get_boot_contract_epoch_and_clarity_version(contract_name.as_str());

        for deployer in [&*BOOT_TESTNET_PRINCIPAL, &*BOOT_MAINNET_PRINCIPAL] {
            let boot_contract = ClarityContract {
                code_source: ClarityCodeSource::ContractInMemory(custom_source.clone()),
                deployer: ContractDeployer::Address(deployer.to_address()),
                name: contract_name.clone(),
                epoch: Epoch::Specific(epoch),
                clarity_version,
                skip_analysis: true,
            };

            let (ast, _, _) = interpreter.build_ast(&boot_contract);
            let contract_id = boot_contract.expect_resolved_contract_identifier(None);
            result.insert(contract_id, (boot_contract, ast));
        }
    }
    result
}

/// Pre-parsed sBTC contracts deployed at the mainnet sBTC address (SM3VDXK3...).
/// - sbtc-registry deployed first (required by sbtc-token and sbtc-deposit)
/// - sbtc-token next before the regular boot contracts (required by pox-5)
/// - sbtc-deposit last (calls into both, and mints the genesis sBTC balances)
///
/// The Vec preserves that deployment order.
pub static SBTC_BOOT_CONTRACTS: LazyLock<
    Vec<(QualifiedContractIdentifier, (ClarityContract, ContractAST))>,
> = LazyLock::new(|| {
    let mut result = Vec::new();
    let interpreter = ClarityInterpreter::new(
        StandardPrincipalData::transient(),
        Settings::default(),
        None,
    );

    // Deployed at the sBTC address with Clarity3 to match the on-chain deployment.
    let epoch = StacksEpochId::Epoch30;
    let clarity_version = ClarityVersion::Clarity3;

    let contracts: [(&str, &str); 3] = [
        ("sbtc-registry", SBTC_REGISTRY_SOURCE),
        ("sbtc-token", SBTC_TOKEN_SOURCE),
        ("sbtc-deposit", SBTC_DEPOSIT_SOURCE),
    ];

    for (name, source) in &contracts {
        let contract = ClarityContract {
            code_source: ClarityCodeSource::ContractInMemory(source.to_string()),
            deployer: ContractDeployer::Address(SBTC_MAINNET_ADDRESS.to_string()),
            name: name.to_string(),
            epoch: Epoch::Specific(epoch),
            clarity_version,
            skip_analysis: true,
        };
        let (ast, _, _) = interpreter.build_ast(&contract);
        let contract_id = contract.expect_resolved_contract_identifier(None);
        result.push((contract_id, (contract, ast)));
    }

    result
});

pub fn get_boot_contract_epoch_and_clarity_version(
    contract_name: &str,
) -> (StacksEpochId, ClarityVersion) {
    let (epoch, clarity_version) = match contract_name {
        "pox-5" => (StacksEpochId::Epoch40, ClarityVersion::Clarity6),
        "costs-4" => (StacksEpochId::Epoch33, ClarityVersion::Clarity4),
        "pox-4" | "signers" | "signers-voting" => {
            (StacksEpochId::Epoch25, ClarityVersion::Clarity2)
        }
        "pox-3" => (StacksEpochId::Epoch24, ClarityVersion::Clarity2),
        "pox-2" | "costs-3" => (StacksEpochId::Epoch21, ClarityVersion::Clarity2),
        "costs-2" => (StacksEpochId::Epoch2_05, ClarityVersion::Clarity1),
        "sbtc-registry" | "sbtc-token" | "sbtc-deposit" => {
            (StacksEpochId::Epoch30, ClarityVersion::Clarity3)
        }
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

#[cfg(test)]
mod remap_tests {
    use super::*;

    #[track_caller]
    fn remapped(source: &str) -> String {
        remap_mainnet_boot_principals(source)
            .unwrap_or_else(|| panic!("expected a remap in:\n{source}"))
    }

    #[track_caller]
    fn assert_untouched(source: &str) {
        assert_eq!(
            remap_mainnet_boot_principals(source),
            None,
            "expected no remap in:\n{source}"
        );
    }

    #[test]
    fn remaps_a_contract_call_target() {
        assert_eq!(
            remapped("(contract-call? 'SP000000000000000000002Q6VF78.pox-5 get-x)"),
            "(contract-call? 'ST000000000000000000002AMW42H.pox-5 get-x)"
        );
    }

    #[test]
    fn remaps_every_boot_contract_name() {
        for name in BOOT_CONTRACTS_NAMES {
            assert_eq!(
                remapped(&format!("'{BOOT_MAINNET_ADDRESS}.{name}")),
                format!("'{BOOT_TESTNET_ADDRESS}.{name}"),
                "boot contract {name} should be remapped"
            );
        }
    }

    #[test]
    fn remaps_trait_references() {
        assert_eq!(
            remapped("(use-trait m 'SP000000000000000000002Q6VF78.pox-5.signer-manager-trait)"),
            "(use-trait m 'ST000000000000000000002AMW42H.pox-5.signer-manager-trait)"
        );
    }

    #[test]
    fn remaps_every_occurrence() {
        let source = "(define-constant A 'SP000000000000000000002Q6VF78.pox-4)\n\
                      (define-constant B 'SP000000000000000000002Q6VF78.pox-5)";
        assert_eq!(
            remapped(source),
            "(define-constant A 'ST000000000000000000002AMW42H.pox-4)\n\
             (define-constant B 'ST000000000000000000002AMW42H.pox-5)"
        );
    }

    /// The whole point of matching the name greedily: `pox` is also a boot
    /// contract, so a prefix match would rewrite the address but leave `-5`
    /// dangling off a `pox` lookup.
    #[test]
    fn does_not_confuse_pox_with_pox_5() {
        assert_eq!(
            remapped("'SP000000000000000000002Q6VF78.pox-5"),
            "'ST000000000000000000002AMW42H.pox-5"
        );
        assert_eq!(
            remapped("'SP000000000000000000002Q6VF78.pox"),
            "'ST000000000000000000002AMW42H.pox"
        );
    }

    /// `SP000000000000000000002Q6VF78` on its own is the mainnet burn address,
    /// which contracts legitimately send funds to.
    #[test]
    fn leaves_the_bare_burn_address_alone() {
        assert_untouched("(stx-transfer? u1 tx-sender 'SP000000000000000000002Q6VF78)");
    }

    #[test]
    fn leaves_non_boot_contract_names_alone() {
        assert_untouched("(contract-call? 'SP000000000000000000002Q6VF78.pox-helper go)");
        assert_untouched("(contract-call? 'SP000000000000000000002Q6VF78.my-pox go)");
    }

    #[test]
    fn leaves_string_literals_alone() {
        assert_untouched(r#"(define-constant S "'SP000000000000000000002Q6VF78.pox-5")"#);
        assert_untouched(r#"(define-constant S u"'SP000000000000000000002Q6VF78.pox-5")"#);
    }

    #[test]
    fn leaves_comments_alone() {
        assert_untouched(";; calls 'SP000000000000000000002Q6VF78.pox-5 on mainnet");
        assert_untouched(";; trailing comment with no newline 'SP000000000000000000002Q6VF78.pox");
    }

    /// A string or comment must not swallow the code that follows it.
    #[test]
    fn resumes_after_comments_and_strings() {
        let source = ";; 'SP000000000000000000002Q6VF78.pox-5\n\
                      (contract-call? 'SP000000000000000000002Q6VF78.pox-5 go)";
        assert_eq!(
            remapped(source),
            ";; 'SP000000000000000000002Q6VF78.pox-5\n\
             (contract-call? 'ST000000000000000000002AMW42H.pox-5 go)"
        );

        let source =
            r#"(f "'SP000000000000000000002Q6VF78.pox" 'SP000000000000000000002Q6VF78.pox)"#;
        assert_eq!(
            remapped(source),
            r#"(f "'SP000000000000000000002Q6VF78.pox" 'ST000000000000000000002AMW42H.pox)"#
        );
    }

    /// An escaped quote must not be read as closing the literal.
    #[test]
    fn handles_escaped_quotes_in_strings() {
        let source =
            r#"(f "a\" 'SP000000000000000000002Q6VF78.pox b" 'SP000000000000000000002Q6VF78.pox)"#;
        assert_eq!(
            remapped(source),
            r#"(f "a\" 'SP000000000000000000002Q6VF78.pox b" 'ST000000000000000000002AMW42H.pox)"#
        );
    }

    #[test]
    fn leaves_the_testnet_address_alone() {
        assert_untouched("(contract-call? 'ST000000000000000000002AMW42H.pox-5 go)");
    }

    #[test]
    fn leaves_sbtc_and_requirement_principals_alone() {
        assert_untouched(&format!(
            "(contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-token go)"
        ));
        assert_untouched(
            "(contract-call? 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait go)",
        );
    }

    #[test]
    fn is_idempotent() {
        let once = remapped("(contract-call? 'SP000000000000000000002Q6VF78.pox-5 go)");
        assert_untouched(&once);
    }

    /// Same-length addresses keep every diagnostic span and coverage
    /// line/column offset valid after the rewrite.
    #[test]
    fn preserves_source_length() {
        assert_eq!(BOOT_MAINNET_ADDRESS.len(), BOOT_TESTNET_ADDRESS.len());

        let source = "(contract-call? 'SP000000000000000000002Q6VF78.pox-5 go)";
        assert_eq!(remapped(source).len(), source.len());
    }

    /// Clarity rejects non-ASCII outside `u"..."` literals, but the scanner
    /// walks the whole file and must not split a multi-byte character.
    #[test]
    fn handles_multibyte_characters() {
        let source = "u\"héllo\"\n'SP000000000000000000002Q6VF78.pox";
        assert_eq!(
            remapped(source),
            "u\"héllo\"\n'ST000000000000000000002AMW42H.pox"
        );
    }

    #[test]
    fn remaps_contract_ids() {
        let mainnet_pox =
            QualifiedContractIdentifier::parse(&format!("{BOOT_MAINNET_ADDRESS}.pox-5")).unwrap();
        let testnet_pox =
            QualifiedContractIdentifier::parse(&format!("{BOOT_TESTNET_ADDRESS}.pox-5")).unwrap();

        assert_eq!(
            remap_mainnet_boot_contract_id(&mainnet_pox),
            Some(testnet_pox.clone())
        );
        assert_eq!(remap_mainnet_boot_contract_id(&testnet_pox), None);

        let not_boot =
            QualifiedContractIdentifier::parse(&format!("{BOOT_MAINNET_ADDRESS}.pox-helper"))
                .unwrap();
        assert_eq!(remap_mainnet_boot_contract_id(&not_boot), None);

        let sbtc =
            QualifiedContractIdentifier::parse(&SBTC_TOKEN_MAINNET_ADDRESS.to_string()).unwrap();
        assert_eq!(remap_mainnet_boot_contract_id(&sbtc), None);
    }
}
