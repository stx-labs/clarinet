use clarity::types::StacksEpochId;
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
    sdk.set_epoch(EpochString::new(&StacksEpochId::latest().to_string()));
    sdk
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
    assert_eq!(sdk.block_height(), 1);
    assert_eq!(sdk.current_epoch(), StacksEpochId::latest().to_string());
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

#[wasm_bindgen_test]
async fn it_can_call_remote_data() {
    let js_noop = JsFunction::new_no_args("return");
    let mut sdk = SDK::new(js_noop, None);
    let options = RemoteDataSettings {
        enabled: true,
        api_url: ApiUrl("https://api.testnet.stg.hiro.so".to_string()),
        initial_height: Some(42000),
        use_mainnet_wallets: false,
    };
    let _ = sdk
        .init_empty_session(JsValue::from_serde(&options).unwrap())
        .await;

    assert_eq!(sdk.current_epoch(), "3.1");

    let tx = sdk.call_public_fn(&CallFnArgs::new(
        "STJCAB2T9TR2EJM7YS4DM2CGBBVTF7BV237Y8KNV.counter".into(),
        "get-count".into(),
        vec![],
        "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM".into(),
    ));

    let expected = format!("0x{}", ClarityValue::UInt(0).serialize_to_hex().unwrap());
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
