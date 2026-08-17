use clarinet_defaults::DEFAULT_EPOCH;
use clarity::vm::Value as ClarityValue;
use clarity_repl::repl::settings::{ApiUrl, RemoteDataSettings};
use gloo_utils::format::JsValueSerdeExt;
use js_sys::Function as JsFunction;
use wasm_bindgen::JsValue;
use wasm_bindgen_test::*;

use super::core::DeployContractArgs;
use crate::core::{CallFnArgs, ContractOptions, EpochString, TransactionRes, SDK};

async fn init_sdk() -> SDK {
    let js_noop = JsFunction::new_no_args("return");
    let mut sdk = SDK::new(js_noop, None);
    let _ = sdk.init_empty_session(JsValue::undefined()).await;
    // `DEFAULT_EPOCH`, not `StacksEpochId::latest()`: clarinet trails upstream
    // until an epoch is adopted here, and `set_epoch` falls back to the default
    // for one it does not know — which would make these tests assert against an
    // epoch they did not actually select.
    sdk.set_epoch(EpochString::new(&DEFAULT_EPOCH.to_string()));
    sdk
}

#[track_caller]
fn assert_tx_result(tx: &TransactionRes, expected: ClarityValue) {
    assert_eq!(
        tx.result,
        format!("0x{}", expected.serialize_to_hex().unwrap())
    );
}

#[track_caller]
fn deploy_basic_contract(sdk: &mut SDK) -> TransactionRes {
    let contract = DeployContractArgs::new(
        "basic-contract".into(),
        "(define-private (two) (+ u1 u1))".into(),
        ContractOptions::new(None),
        "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM".into(),
    );
    sdk.deploy_contract(&contract).unwrap()
}

#[wasm_bindgen_test]
async fn it_can_execute_clarity_code() {
    let mut sdk = init_sdk().await;
    let tx = sdk.execute("(+ u41 u1)".into()).unwrap();
    let expected = format!("0x{}", ClarityValue::UInt(42).serialize_to_hex().unwrap());
    assert_eq!(tx.result, expected);
}

#[wasm_bindgen_test]
async fn it_can_set_epoch() {
    let mut sdk = init_sdk().await;
    // set_epoch("4.0") transitions from Epoch2_05, which advances the burn chain tip by 1.
    assert_eq!(sdk.block_height(), 1);
    assert_eq!(sdk.current_epoch(), DEFAULT_EPOCH.to_string());
}

#[wasm_bindgen_test]
async fn it_can_deploy_contract() {
    let mut sdk = init_sdk().await;
    let tx = deploy_basic_contract(&mut sdk);
    let expected = format!("0x{}", ClarityValue::Bool(true).serialize_to_hex().unwrap());
    assert_eq!(tx.result, expected);
}

#[wasm_bindgen_test]
async fn it_can_call_a_private_function() {
    let mut sdk = init_sdk().await;
    let _ = deploy_basic_contract(&mut sdk);
    let tx = sdk
        .call_private_fn(&CallFnArgs::new(
            "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.basic-contract".into(),
            "two".into(),
            vec![],
            "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM".into(),
        ))
        .unwrap();
    let expected = format!("0x{}", ClarityValue::UInt(2).serialize_to_hex().unwrap());
    assert_eq!(tx.result, expected);
}

/// Which contract calls consume a nonce is decided here in `core.rs`, not in
/// `Session::call_contract_fn` — that primitive also backs `callReadOnlyFn`,
/// which sends nothing. So this rule can only be tested at this layer; a
/// `cargo tst` run will not tell you whether a call consumes a nonce.
#[wasm_bindgen_test]
async fn it_bumps_the_sender_nonce_only_for_contract_call_transactions() {
    const SENDER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
    let mut sdk = init_sdk().await;

    let call = |method: &str| {
        CallFnArgs::new(
            format!("{SENDER}.nonce-contract"),
            method.into(),
            vec![],
            SENDER.into(),
        )
    };

    assert_eq!(sdk.get_account_nonce(SENDER).unwrap(), 0);

    sdk.deploy_contract(&DeployContractArgs::new(
        "nonce-contract".into(),
        "(define-read-only (peek) u1)
         (define-public (poke) (ok u1))
         (define-private (hidden) u1)
         (define-public (boom) (ok (/ u1 u0)))"
            .into(),
        ContractOptions::new(None),
        SENDER.into(),
    ))
    .unwrap();

    // The deploy is itself a transaction. Landing on exactly 1 also proves the
    // boot contracts deployed by `init_sdk` consumed nothing.
    assert_eq!(sdk.get_account_nonce(SENDER).unwrap(), 1);

    // Each call asserts its return value as well as the nonce, so that a
    // regression turning one of these into a no-op cannot satisfy the nonce
    // check vacuously.
    assert_tx_result(
        &sdk.call_read_only_fn(&call("peek")).unwrap(),
        ClarityValue::UInt(1),
    );
    assert_eq!(
        sdk.get_account_nonce(SENDER).unwrap(),
        1,
        "a read-only call sends nothing and must not consume a nonce"
    );

    assert_tx_result(
        &sdk.call_public_fn(&call("poke")).unwrap(),
        ClarityValue::okay(ClarityValue::UInt(1)).unwrap(),
    );
    assert_eq!(sdk.get_account_nonce(SENDER).unwrap(), 2);

    // simnet models a private call as a transaction, even though mainnet has no
    // way to reach a private function from one.
    assert_tx_result(
        &sdk.call_private_fn(&call("hidden")).unwrap(),
        ClarityValue::UInt(1),
    );
    assert_eq!(sdk.get_account_nonce(SENDER).unwrap(), 3);

    // A call that fails at runtime is still mined on mainnet, so it still
    // consumes a nonce. This is the divergence the old `is_ok()` rule had, and
    // the contract-call half of it can only be observed from here.
    let err = sdk
        .call_public_fn(&call("boom"))
        .expect_err("dividing by zero must surface as an error");
    assert!(err.contains("DivisionByZero"), "got: {err}");
    assert_eq!(
        sdk.get_account_nonce(SENDER).unwrap(),
        4,
        "a failed-but-included contract call still consumes a nonce"
    );
}

/// The access preflight short-circuits a call the VM would otherwise run, so
/// it has to reach the VM's answer about the nonce as well as about the error.
///
/// Mainnet mines a `contract-call?` naming a non-public function:
/// `NoSuchPublicFunction` is a non-rejectable `RuntimeCheck`. The pairs below
/// are that same failure arriving by the two different routes — one with a
/// cached interface, one without — and they must not disagree.
#[wasm_bindgen_test]
async fn it_charges_a_nonce_for_a_call_the_access_preflight_rejects() {
    const SENDER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
    let mut sdk = init_sdk().await;

    let call = |method: &str| {
        CallFnArgs::new(
            format!("{SENDER}.access-contract"),
            method.into(),
            vec![],
            SENDER.into(),
        )
    };

    sdk.deploy_contract(&DeployContractArgs::new(
        "access-contract".into(),
        "(define-read-only (peek) u1)
         (define-public (poke) (ok u1))
         (define-private (hidden) u1)"
            .into(),
        ContractOptions::new(None),
        SENDER.into(),
    ))
    .unwrap();
    assert_eq!(sdk.get_account_nonce(SENDER).unwrap(), 1);

    // Rejected by the preflight, because the interface says the access is wrong.
    for (method, expected) in [
        ("peek", "peek is not a public function"),
        ("hidden", "hidden is not a public function"),
    ] {
        let before = sdk.get_account_nonce(SENDER).unwrap();
        let err = sdk.call_public_fn(&call(method)).unwrap_err();
        assert_eq!(err, expected);
        assert_eq!(
            sdk.get_account_nonce(SENDER).unwrap(),
            before + 1,
            "mainnet mines a call to a non-public function, so `{method}` owes a nonce"
        );
    }

    // The same failure reached through the VM: no interface entry exists for a
    // name the contract does not define, so the preflight has nothing to say.
    let before = sdk.get_account_nonce(SENDER).unwrap();
    sdk.call_public_fn(&call("no-such-fn"))
        .expect_err("a call to an undefined function must fail");
    assert_eq!(
        sdk.get_account_nonce(SENDER).unwrap(),
        before + 1,
        "the VM route must charge the same nonce the preflight route does"
    );

    // simnet models a private call as a transaction, so its preflight follows
    // the same rule.
    let before = sdk.get_account_nonce(SENDER).unwrap();
    let err = sdk.call_private_fn(&call("poke")).unwrap_err();
    assert_eq!(err, "poke is not a private function");
    assert_eq!(sdk.get_account_nonce(SENDER).unwrap(), before + 1);

    // A read-only call sends nothing, so neither route charges anything.
    let before = sdk.get_account_nonce(SENDER).unwrap();
    let err = sdk.call_read_only_fn(&call("poke")).unwrap_err();
    assert_eq!(err, "poke is not a read-only function");
    sdk.call_read_only_fn(&call("no-such-fn"))
        .expect_err("a read-only call to an undefined function must fail");
    assert_eq!(
        sdk.get_account_nonce(SENDER).unwrap(),
        before,
        "a read-only call is not a transaction by either route"
    );
}

#[wasm_bindgen_test]
async fn it_rejects_a_nonce_lookup_for_an_invalid_address() {
    let mut sdk = init_sdk().await;

    let err = sdk.get_account_nonce("not-an-address").unwrap_err();
    assert!(err.contains("Invalid address"), "got: {err}");
}

#[wasm_bindgen_test]
async fn it_can_call_remote_data() {
    let js_noop = JsFunction::new_no_args("return");
    let mut sdk = SDK::new(js_noop, None);
    let options = RemoteDataSettings {
        enabled: true,
        api_url: ApiUrl("https://api.testnet.hiro.so".to_string()),
        initial_height: Some(50000),
        use_mainnet_wallets: false,
    };
    let _ = sdk
        .init_empty_session(JsValue::from_serde(&options).unwrap())
        .await;

    // height 50000 is in Epoch 4.0 on the current krypton testnet
    assert_eq!(sdk.current_epoch(), "4.0");

    // testnet addresses (ST prefix) are standard on testnet
    let tx = sdk.execute("(is-standard 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)".into());
    let expected = format!("0x{}", ClarityValue::Bool(true).serialize_to_hex().unwrap());
    assert_eq!(tx.unwrap().result, expected);
}

#[wasm_bindgen_test]
async fn it_handles_invalid_sender_address() {
    let mut sdk = init_sdk().await;
    let _ = deploy_basic_contract(&mut sdk);

    // Test with invalid sender address (full contract address instead of just sender)
    let result = sdk.call_public_fn(&CallFnArgs::new(
        "basic-contract".into(),
        "two".into(),
        vec![],
        "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.basic-contract".into(), // Invalid: full contract address
    ));

    assert!(result.is_err());
    let error_msg = result.unwrap_err();
    assert!(error_msg.contains("Invalid sender address"));
}

#[wasm_bindgen_test]
async fn it_handles_contract_recipient_address() {
    let mut sdk = init_sdk().await;

    let result = sdk.transfer_stx(&crate::core::TransferSTXArgs::new(
        1000,
        "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.basic-contract".into(), // valid: contract address
        "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM".into(),
    ));

    // contract addresses are valid recipients
    assert!(result.is_ok());
}

#[wasm_bindgen_test]
async fn it_handles_invalid_deployer_address() {
    let mut sdk = init_sdk().await;

    // Test with invalid deployer address
    let contract = DeployContractArgs::new(
        "basic-contract".into(),
        "(define-private (two) (+ u1 u1))".into(),
        ContractOptions::new(None),
        "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.invalid-contract".into(), // Invalid: contract address
    );

    let result = sdk.deploy_contract(&contract);
    assert!(result.is_err());
    let error_msg = result.unwrap_err();
    assert!(error_msg.contains("Invalid sender address"));
}

#[wasm_bindgen_test]
async fn it_handles_contract_address_as_sender() {
    let mut sdk = init_sdk().await;
    let _ = deploy_basic_contract(&mut sdk);

    let contract_address = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.test";
    let result = sdk.call_public_fn(&CallFnArgs::new(
        "basic-contract".into(),
        "two".into(),
        vec![],
        contract_address.into(), // Invalid: contract address instead of sender address
    ));

    assert!(result.is_err());
    let error_msg = result.unwrap_err();
    assert!(error_msg.contains("Invalid sender address"));
    assert!(error_msg.contains("ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.test"));
}

#[wasm_bindgen_test]
async fn it_handles_contract_address_as_recipient() {
    let mut sdk = init_sdk().await;

    let contract_address = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.test";
    let result = sdk.transfer_stx(&crate::core::TransferSTXArgs::new(
        1000,
        contract_address.into(), // Valid: contract address instead of recipient address
        "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM".into(),
    ));

    // we support contract addresses as recipients
    assert!(result.is_ok());
}
