use clarity::types::StacksEpochId;
use clarity::vm::ClarityVersion;
use clarity_repl::repl::boot::{
    get_boot_contract_epoch_and_clarity_version, BOOT_CONTRACTS_DATA, BOOT_TESTNET_ADDRESS,
    SBTC_BOOT_CONTRACTS, SBTC_CONTRACTS_NAMES,
};
use clarity_repl::repl::{ClarityInterpreter, Epoch, Settings};
use clarity_types::types::StandardPrincipalData;

#[test]
fn can_run_boot_contracts() {
    // Turn off lints, which currently result in diagnostics
    let mut repl_settings = Settings::default();
    repl_settings.analysis.disable_all_lints();

    let mut interpreter =
        ClarityInterpreter::new(StandardPrincipalData::transient(), repl_settings, None);

    // Deploy sbtc boot contracts first (pox-5 depends on sbtc-token)
    for (_, (contract, ast)) in SBTC_BOOT_CONTRACTS.clone() {
        interpreter
            .run(&contract, Some(&ast), false, None)
            .expect("sbtc boot contract should deploy");
    }

    let boot_contracts_data = BOOT_CONTRACTS_DATA.clone();

    for (_, (boot_contract, ast)) in boot_contracts_data {
        match interpreter.run(&boot_contract, Some(&ast), false, None) {
            Ok(res) => {
                assert!(res.diagnostics.is_empty(), "{:?}", res.diagnostics);
            }
            Err(err) => {
                dbg!(&err);
                panic!("failed to interpret {} boot contract", boot_contract.name);
            }
        }
    }
}

/// The sBTC set is spelled out in three places: `SBTC_CONTRACTS_NAMES`, the
/// source list inside `SBTC_BOOT_CONTRACTS`, and the match in
/// `get_boot_contract_epoch_and_clarity_version`. Nothing derives one from
/// another, and a name missing from the match only panics at runtime, so pin
/// them together here.
#[test]
fn sbtc_contract_lists_agree() {
    let deployed: Vec<&str> = SBTC_BOOT_CONTRACTS
        .iter()
        .map(|(contract_id, _)| contract_id.name.as_str())
        .collect();

    assert_eq!(
        deployed, SBTC_CONTRACTS_NAMES,
        "SBTC_CONTRACTS_NAMES must list the deployed sBTC contracts, in deployment order"
    );

    for name in SBTC_CONTRACTS_NAMES {
        // Panics if `name` has no entry in the match.
        let (epoch, clarity_version) = get_boot_contract_epoch_and_clarity_version(name);
        assert_eq!(epoch, StacksEpochId::Epoch30, "{name}");
        assert_eq!(clarity_version, ClarityVersion::Clarity3, "{name}");
    }
}

#[test]
fn pox_5_configured_for_epoch_40() {
    let (epoch, clarity_version) = get_boot_contract_epoch_and_clarity_version("pox-5");
    assert_eq!(epoch, StacksEpochId::Epoch40);
    assert_eq!(clarity_version, ClarityVersion::Clarity6);
}

#[test]
fn pox_5_in_boot_contracts_data() {
    let pox_5_testnet = format!("{BOOT_TESTNET_ADDRESS}.pox-5");
    let entry = BOOT_CONTRACTS_DATA
        .iter()
        .find(|(id, _)| id.to_string() == pox_5_testnet);
    assert!(entry.is_some(), "pox-5 should be in boot contracts data");

    let (_, (contract, _)) = entry.unwrap();
    assert_eq!(contract.epoch, Epoch::Specific(StacksEpochId::Epoch40));
    assert_eq!(contract.clarity_version, ClarityVersion::Clarity6);
}

#[test]
fn pox_5_testnet_keeps_mainnet_sbtc_address() {
    let pox_5_testnet = format!("{BOOT_TESTNET_ADDRESS}.pox-5");
    let entry = BOOT_CONTRACTS_DATA
        .iter()
        .find(|(id, _)| id.to_string() == pox_5_testnet)
        .expect("pox-5 should be in boot contracts data");

    let (_, (contract, _)) = entry;
    let code = contract.expect_in_memory_code_source();

    assert!(code.contains("SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4"));
}
