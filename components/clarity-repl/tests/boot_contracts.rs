use clarity_repl::repl::boot::BOOT_CONTRACTS_DATA;
use clarity_repl::repl::{ClarityInterpreter, Settings};
use clarity_types::types::StandardPrincipalData;

#[test]
fn can_run_boot_contracts() {
    let mut interpreter = ClarityInterpreter::new(
        StandardPrincipalData::transient(),
        Settings::default(),
        None,
    );
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
