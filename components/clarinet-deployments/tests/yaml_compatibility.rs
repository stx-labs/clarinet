use std::path::Path;

use clarinet_deployments::types::{
    DeploymentSpecification, DeploymentSpecificationFile, EpochSpec, TransactionPlanSpecification,
    TransactionSpecificationFile,
};
use clarinet_deployments::yaml::{from_yaml, to_yaml};
use clarinet_files::StacksNetwork;
use indoc::{formatdoc, indoc};
use serde_json::{json, Value};
use test_case::test_case;

fn transactions() -> Vec<(&'static str, Value)> {
    vec![
        (
            "contract-call",
            json!({"contract-id": "ST000000000000000000002AMW42H.test", "expected-sender": "ST000000000000000000002AMW42H", "method": "run", "parameters": ["u1", "true", "0x00", "(some u1)"], "cost": 123, "anchor-block-only": false}),
        ),
        (
            "contract-publish",
            json!({"contract-name": "test", "expected-sender": "ST000000000000000000002AMW42H", "cost": 123, "path": "contracts/test.clar", "clarity-version": 2}),
        ),
        (
            "emulated-contract-call",
            json!({"contract-id": "ST000000000000000000002AMW42H.test", "emulated-sender": "ST000000000000000000002AMW42H", "method": "run", "parameters": ["'ST000000000000000000002AMW42H", "\"hello\""]}),
        ),
        (
            "emulated-contract-publish",
            json!({"contract-name": "test", "emulated-sender": "ST000000000000000000002AMW42H", "path": "contracts/test.clar", "clarity-version": 2, "remap-principals": {"SP000000000000000000002Q6VF78": "ST000000000000000000002AMW42H"}}),
        ),
        (
            "requirement-publish",
            json!({"contract-id": "SP000000000000000000002Q6VF78.test", "remap-sender": "ST000000000000000000002AMW42H", "remap-principals": {"SP000000000000000000002Q6VF78": "ST000000000000000000002AMW42H"}, "cost": 123, "path": "contracts/test.clar"}),
        ),
        (
            "btc-transfer",
            json!({"expected-sender": "sender", "recipient": "recipient", "sats-amount": u64::MAX, "sats-per-byte": 42}),
        ),
        (
            "stx-transfer",
            json!({"expected-sender": "sender", "recipient": "recipient", "mstx-amount": u64::MAX, "cost": 42, "memo": "0x0001"}),
        ),
    ]
}

/// The legacy single-key mapping and the YAML-tagged spelling of one transaction body.
fn legacy_and_tagged(variant: &str, body: &str) -> [String; 2] {
    let indented: String = body.lines().map(|line| format!("  {line}\n")).collect();
    [
        format!("{variant}:\n{indented}"),
        format!("!{variant}\n{body}\n"),
    ]
}

#[test]
fn all_transaction_variants_accept_all_three_formats() {
    for (variant, fields) in transactions() {
        let mut current = fields.clone();
        current["transaction-type"] = json!(variant);
        let expected: TransactionSpecificationFile =
            serde_json::from_value(current.clone()).unwrap();
        let body = serde_json::to_string_pretty(&fields).unwrap();
        let [legacy, tagged] = legacy_and_tagged(variant, &body);
        let current = serde_json::to_string(&current).unwrap();

        for input in [current, legacy, tagged] {
            let actual: TransactionSpecificationFile =
                from_yaml(&input).unwrap_or_else(|error| panic!("{input}\n{error}"));
            assert_eq!(actual, expected, "{input}");
            let emitted = to_yaml(&actual).unwrap();
            assert!(
                emitted.contains(&format!("transaction-type: {variant}")),
                "{emitted}"
            );
            assert_eq!(
                from_yaml::<TransactionSpecificationFile>(&emitted).unwrap(),
                expected
            );
        }
    }
}

#[test]
fn malformed_transactions_are_rejected() {
    for input in [
        "transaction-type: nonexistent\n",
        "!nonexistent {}",
        indoc! {"
            transaction-type: stx-transfer
            cost: 1
        "},
        "[]",
        "null",
        indoc! {"
            transaction-type: btc-transfer
            expected-sender: sender
            recipient: recipient
            sats-amount: -1
            sats-per-byte: 1
        "},
        indoc! {"
            transaction-type: btc-transfer
            expected-sender: sender
            recipient: recipient
            sats-amount: 18446744073709551616
            sats-per-byte: 1
        "},
    ] {
        assert!(
            from_yaml::<TransactionSpecificationFile>(input).is_err(),
            "{input}"
        );
    }
}

#[test]
fn standard_yaml_map_tag_is_not_a_transaction_variant() {
    let input = "!!map {transaction-type: btc-transfer, expected-sender: sender, recipient: recipient, sats-amount: 1, sats-per-byte: 1}";
    assert!(from_yaml::<TransactionSpecificationFile>(input).is_ok());
}

#[test]
fn parse_counter_simnet_deployment_file() {
    let input =
        include_str!("../../clarinet-cli/examples/counter/deployments/default.simnet-plan.yaml");
    let deployment = DeploymentSpecificationFile::from_file_content(input).unwrap();
    assert_eq!(deployment.network, "simnet");
    let genesis = deployment.genesis.as_ref().unwrap();
    assert_eq!(genesis.wallets[0].name, "deployer");
    assert_eq!(genesis.wallets[0].balance, "100000000000000");
    assert_eq!(
        genesis.wallets[0].sbtc_balance.as_deref(),
        Some("1000000000")
    );
    let batches = &deployment.plan.as_ref().unwrap().batches;
    assert_eq!(batches.len(), 2);
    assert_eq!(batches[0].epoch, Some(EpochSpec::Epoch2_0));
    assert_eq!(batches[1].epoch, Some(EpochSpec::Epoch2_4));
    let TransactionSpecificationFile::EmulatedContractPublish(publish) =
        &batches[0].transactions[0]
    else {
        panic!("expected emulated contract publish");
    };
    assert_eq!(publish.contract_name, "counter");
    let emitted = to_yaml(&deployment).unwrap();
    assert_eq!(
        DeploymentSpecificationFile::from_file_content(&emitted).unwrap(),
        deployment
    );
}

#[test]
fn parse_counter_devnet_deployment_file() {
    let input =
        include_str!("../../clarinet-cli/examples/counter/deployments/default.devnet-plan.yaml");
    let deployment = DeploymentSpecificationFile::from_file_content(input).unwrap();
    assert_eq!(deployment.network, "devnet");
    assert_eq!(
        deployment.stacks_node.as_deref(),
        Some("http://localhost:20443")
    );
    let batches = &deployment.plan.as_ref().unwrap().batches;
    assert_eq!(batches.len(), 2);
    assert_eq!(batches[0].epoch, Some(EpochSpec::Epoch2_0));
    let TransactionSpecificationFile::ContractPublish(publish) = &batches[0].transactions[0] else {
        panic!("expected contract publish");
    };
    assert_eq!(publish.contract_name, "counter");
    assert_eq!(publish.cost, 5960);
    assert_eq!(publish.anchor_block_only, Some(true));
    let emitted = to_yaml(&deployment).unwrap();
    assert_eq!(
        DeploymentSpecificationFile::from_file_content(&emitted).unwrap(),
        deployment
    );
}

/// The emitted legacy boolean word remains a string on re-reading.
#[test]
fn generated_deployment_uses_yaml12_quoting_without_a_directive() {
    let deployment = DeploymentSpecification {
        id: 0,
        name: "yes".into(),
        network: StacksNetwork::Simnet,
        stacks_node: None,
        bitcoin_node: None,
        genesis: None,
        plan: TransactionPlanSpecification { batches: vec![] },
        contracts: Default::default(),
        contract_remaps: None,
    };
    let project_root = Path::new(".");
    let emitted = String::from_utf8(deployment.to_file_content(project_root).unwrap()).unwrap();
    assert!(
        emitted.starts_with(indoc! {"
        id: 0
        name: yes
    "}),
        "{emitted}"
    );
    assert_eq!(
        DeploymentSpecificationFile::from_file_content(&emitted).unwrap(),
        deployment.to_specification_file(project_root)
    );
}

/// The example plan was written by yaml_serde: single quotes, no folded blocks.
/// It describes the same deployment as today's output, so it is not reported as changed.
#[test]
fn plan_in_another_yaml_style_matches_the_deployment() {
    let project_root =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("../clarinet-cli/examples/counter");
    let plan_path = project_root.join("deployments/default.simnet-plan.yaml");
    let on_disk = std::fs::read_to_string(&plan_path).unwrap();
    let deployment = DeploymentSpecification::from_config_file(&plan_path, &project_root).unwrap();

    let emitted = deployment.to_file_content(&project_root).unwrap();
    assert_ne!(emitted, on_disk.as_bytes());
    assert!(deployment.matches_file_content(&emitted, &project_root));
    assert!(deployment.matches_file_content(&on_disk, &project_root));

    let edited = on_disk.replace("wallet_1", "wallet_x");
    assert!(!deployment.matches_file_content(edited, &project_root));
    assert!(!deployment.matches_file_content("not: a plan", &project_root));
}

/// Unquoted legacy boolean words stay strings through the custom transaction deserializer.
#[test]
fn unquoted_call_strings_in_all_transaction_formats() {
    let fields = indoc! {"
        contract-id: ST000000000000000000002AMW42H.test
        expected-sender: ST000000000000000000002AMW42H
        method: on
        parameters: [yes, NO]
        cost: 1
    "};
    let current = format!("transaction-type: contract-call\n{fields}");
    let [legacy, tagged] = legacy_and_tagged("contract-call", fields);
    for input in [current, legacy, tagged] {
        let TransactionSpecificationFile::ContractCall(call) = from_yaml(&input).unwrap() else {
            panic!("expected contract call");
        };
        assert_eq!(call.method, "on");
        assert_eq!(call.parameters, ["yes", "NO"]);
    }
}

/// A `parameters:` key with no value reads as the empty list, as it did with
/// yaml_serde's value buffer. The key itself stays required.
#[test_case(""; "empty_value")]
#[test_case("~"; "tilde_null")]
#[test_case("null"; "null_word")]
fn call_parameters_without_a_value_are_an_empty_list(parameters: &str) {
    for (variant, sender) in [
        ("contract-call", "expected-sender"),
        ("emulated-contract-call", "emulated-sender"),
    ] {
        let required = formatdoc! {"
            transaction-type: {variant}
            contract-id: ST000000000000000000002AMW42H.counter
            {sender}: ST000000000000000000002AMW42H
            method: initialize
            cost: 1
        "};
        let call = |parameters: &str| {
            from_yaml::<TransactionSpecificationFile>(format!(
                "{required}parameters: {parameters}\n"
            ))
        };
        assert_eq!(call(parameters).unwrap(), call("[]").unwrap());
        assert!(from_yaml::<TransactionSpecificationFile>(&required).is_err());
    }
}

/// Application policy: JSON buffering converts integer/boolean keys to strings,
/// after which unknown fields are ignored. YAML 1.1 and 1.2 both allow scalar keys.
#[test_case("1"; "integer_key")]
#[test_case("true"; "boolean_key")]
fn transaction_buffer_converts_scalar_keys_to_strings(key: &str) {
    let input = formatdoc! {"
        transaction-type: btc-transfer
        expected-sender: sender
        recipient: recipient
        sats-amount: 1
        sats-per-byte: 1
        {key}: ignored
    "};
    assert!(from_yaml::<TransactionSpecificationFile>(input).is_ok());
}

/// Application policy: JSON buffering stringifies a numeric remap key before
/// field deserialization. This tests loading, not later principal validation.
#[test]
fn transaction_buffer_converts_integer_remap_keys_to_strings() {
    let input = indoc! {b"
        transaction-type: requirement-publish
        contract-id: id
        remap-sender: sender
        remap-principals: {1: recipient}
        cost: 1
        path: contracts/test.clar
    "};
    let TransactionSpecificationFile::RequirementPublish(spec) = from_yaml(input).unwrap() else {
        panic!("expected requirement publish");
    };
    assert_eq!(
        spec.remap_principals.unwrap().get("1").map(String::as_str),
        Some("recipient")
    );
}

/// Error-reporting policy: a buffered transaction error points at its mapping
/// and omits the nested field path.
#[test]
fn deployment_diagnostics_point_to_the_transaction_mapping() {
    let input = indoc! {"
        name: test
        network: devnet
        plan:
          batches:
          - id: 0
            transactions:
            - transaction-type: btc-transfer
              expected-sender: sender
              recipient: recipient
              sats-amount: wrong
              sats-per-byte: 1
    "};
    let error = DeploymentSpecificationFile::from_file_content(input).unwrap_err();
    assert!(
        error.starts_with("unable to read file error: line"),
        "{error}"
    );
    assert!(!error.contains("plan.batches[0].transactions"), "{error}");
}
