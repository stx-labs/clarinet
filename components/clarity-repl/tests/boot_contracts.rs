use clarity_repl::repl::boot::BOOT_CONTRACTS_DATA;
use clarity_repl::repl::{ClarityInterpreter, Settings};
use clarity_types::types::StandardPrincipalData;

#[test]
fn can_run_boot_contracts() {
    // Turn off lints, which currently result in diagnostics
    let mut repl_settings = Settings::default();
    repl_settings.analysis.disable_all_lints();

    let mut interpreter =
        ClarityInterpreter::new(StandardPrincipalData::transient(), repl_settings, None);
    let boot_contracts_data = BOOT_CONTRACTS_DATA.clone();

    for (_, (boot_contract, ast)) in boot_contracts_data {
        let res = interpreter
            .run(&boot_contract, Some(&ast), false, None)
            .unwrap_or_else(|err| {
                dbg!(&err);
                panic!("failed to interpret {} boot contract", &boot_contract.name)
            });
        assert!(res.diagnostics.is_empty());
    }
}
