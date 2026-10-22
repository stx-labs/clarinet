use clarinet_deployments::types::{
    from_yaml, DeploymentSpecificationFile, TransactionSpecification, TransactionSpecificationFile,
};
use indoc::indoc;
use serde_json::{json, Value};

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

#[test]
fn all_transaction_variants_accept_all_three_formats() {
    for (variant, fields) in transactions() {
        let mut current = fields.clone();
        current["transaction-type"] = json!(variant);
        let expected: TransactionSpecificationFile =
            serde_json::from_value(current.clone()).unwrap();
        let body = serde_saphyr::to_string(&fields).unwrap();
        let legacy = format!(
            "{variant}:\n{}",
            body.lines()
                .map(|line| format!("  {line}\n"))
                .collect::<String>()
        );
        let tagged = format!("!{variant}\n{body}");
        let current = serde_saphyr::to_string(&current).unwrap();

        for input in [current, legacy, tagged] {
            let actual: TransactionSpecificationFile =
                serde_saphyr::from_str(&input).unwrap_or_else(|error| panic!("{input}\n{error}"));
            assert_eq!(actual, expected, "{input}");
            let emitted = serde_saphyr::to_string(&actual).unwrap();
            assert!(
                emitted.contains(&format!("transaction-type: {variant}")),
                "{emitted}"
            );
            assert_eq!(
                serde_saphyr::from_str::<TransactionSpecificationFile>(&emitted).unwrap(),
                expected
            );
        }
    }
}

#[test]
fn deployment_preserves_epochs_aliases_and_literal_parameters() {
    let input = r#"
id: 0
name: "Deployment: 日本語"
network: simnet
plan:
  batches:
    - id: 0
      epoch: "3.0"
      transactions:
        - transaction-type: emulated-contract-call
          contract-id: ST000000000000000000002AMW42H.test
          emulated-sender: &sender ST000000000000000000002AMW42H
          method: run
          parameters: ["true", "null", "123", "0x01", "yes", "NO", "3.0"]
        - !emulated-contract-call
          contract-id: ST000000000000000000002AMW42H.test
          emulated-sender: *sender
          method: run
          parameters:
            - |-
              (some
                u1)
"#;
    let plan = DeploymentSpecificationFile::from_file_content(input).unwrap();
    let bytes_plan: DeploymentSpecificationFile =
        serde_saphyr::from_slice(input.as_bytes()).unwrap();
    assert_eq!(plan, bytes_plan);
    let emitted = serde_saphyr::to_string(&plan).unwrap();
    assert_eq!(
        DeploymentSpecificationFile::from_file_content(&emitted).unwrap(),
        plan
    );
    let batch = &plan.plan.unwrap().batches[0];
    if let TransactionSpecificationFile::EmulatedContractCall(call) = &batch.transactions[0] {
        assert_eq!(
            call.parameters,
            ["true", "null", "123", "0x01", "yes", "NO", "3.0"]
        );
    } else {
        panic!("expected emulated contract call");
    }
}

#[test]
fn unquoted_yes_no_on_off_stay_strings() {
    let input = indoc! {"
        id: 0
        name: deployment
        network: simnet
        plan:
          batches:
            - id: 0
              transactions:
                - transaction-type: contract-call
                  contract-id: ST000000000000000000002AMW42H.test
                  expected-sender: ST000000000000000000002AMW42H
                  method: on
                  parameters: [yes, NO, y, Off]
                  cost: 1
                - contract-call:
                    contract-id: ST000000000000000000002AMW42H.test
                    expected-sender: ST000000000000000000002AMW42H
                    method: on
                    parameters: [yes, NO, y, Off]
                    cost: 1
                - !contract-call
                  contract-id: ST000000000000000000002AMW42H.test
                  expected-sender: ST000000000000000000002AMW42H
                  method: on
                  parameters: [yes, NO, y, Off]
                  cost: 1
                - transaction-type: contract-publish
                  contract-name: n
                  expected-sender: ST000000000000000000002AMW42H
                  cost: 1
                  path: contracts/n.clar
                - transaction-type: stx-transfer
                  expected-sender: ST000000000000000000002AMW42H
                  recipient: ST000000000000000000002AMW42H
                  mstx-amount: 1
                  memo: off
                  cost: 1
    "};
    let plan = DeploymentSpecificationFile::from_file_content(input).unwrap();
    let transactions = &plan.plan.unwrap().batches[0].transactions;

    for transaction in &transactions[..3] {
        let TransactionSpecificationFile::ContractCall(call) = transaction else {
            panic!("expected contract call, got {transaction:?}");
        };
        assert_eq!(call.method, "on");
        assert_eq!(call.parameters, ["yes", "NO", "y", "Off"]);
    }
    let TransactionSpecificationFile::ContractPublish(publish) = &transactions[3] else {
        panic!("expected contract publish, got {:?}", transactions[3]);
    };
    assert_eq!(publish.contract_name, "n");
    let TransactionSpecificationFile::StxTransfer(transfer) = &transactions[4] else {
        panic!("expected stx transfer, got {:?}", transactions[4]);
    };
    assert_eq!(transfer.memo.as_deref(), Some("off"));
}

// The devnet coordinator reads the serde-derived `DeploymentSpecification`, buffered by serde itself.
#[test]
fn unquoted_yes_no_on_off_stay_strings_in_specification() {
    let input = indoc! {"
        transaction_type: ContractCall
        contract_id: ST000000000000000000002AMW42H.test
        expected_sender: ST000000000000000000002AMW42H
        method: on
        parameters: [yes, n]
        cost: 1
        anchor_block_only: false
    "};
    let TransactionSpecification::ContractCall(call) = from_yaml(input.as_bytes()).unwrap() else {
        panic!("expected contract call");
    };
    assert_eq!(call.method.as_str(), "on");
    assert_eq!(call.parameters, ["yes", "n"]);
}

#[test]
fn malformed_transactions_are_rejected() {
    for input in [
        "transaction-type: nonexistent\n",
        "!nonexistent {}",
        "transaction-type: stx-transfer\ncost: 1\n",
        "[]",
        "null",
        "transaction-type: btc-transfer\nexpected-sender: sender\nrecipient: recipient\nsats-amount: -1\nsats-per-byte: 1\n",
        "transaction-type: btc-transfer\nexpected-sender: sender\nrecipient: recipient\nsats-amount: 18446744073709551616\nsats-per-byte: 1\n",
    ] {
        assert!(serde_saphyr::from_str::<TransactionSpecificationFile>(input).is_err(), "{input}");
    }
}

#[test]
fn standard_yaml_map_tag_is_not_a_transaction_variant() {
    let input = "!!map {transaction-type: btc-transfer, expected-sender: sender, recipient: recipient, sats-amount: 1, sats-per-byte: 1}";
    assert!(serde_saphyr::from_str::<TransactionSpecificationFile>(input).is_ok());
}
