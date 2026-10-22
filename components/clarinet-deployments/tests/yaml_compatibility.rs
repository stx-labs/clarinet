use clarinet_deployments::types::{
    DeploymentSpecificationFile, EpochSpec, TransactionSpecificationFile,
};
use clarinet_deployments::yaml::from_yaml;
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

/// All seven transaction variants accept current mappings, legacy mappings, and YAML tags.
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
                from_yaml(input.as_bytes()).unwrap_or_else(|error| panic!("{input}\n{error}"));
            assert_eq!(actual, expected, "{input}");
            let emitted = serde_saphyr::to_string(&actual).unwrap();
            assert!(
                emitted.contains(&format!("transaction-type: {variant}")),
                "{emitted}"
            );
            assert_eq!(
                from_yaml::<TransactionSpecificationFile>(emitted.as_bytes()).unwrap(),
                expected
            );
        }
    }
}

/// Invalid variants, missing fields, wrong shapes, and out-of-range amounts are rejected.
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
        assert!(
            from_yaml::<TransactionSpecificationFile>(input.as_bytes()).is_err(),
            "{input}"
        );
    }
}

/// A standard mapping tag is not mistaken for a custom transaction-variant tag.
#[test]
fn standard_yaml_map_tag_is_not_a_transaction_variant() {
    let input = "!!map {transaction-type: btc-transfer, expected-sender: sender, recipient: recipient, sats-amount: 1, sats-per-byte: 1}";
    assert!(from_yaml::<TransactionSpecificationFile>(input.as_bytes()).is_ok());
}

/// Parse the checked-in simnet plan, including genesis balances, epochs, and publish transactions.
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
    let emitted = serde_saphyr::to_string(&deployment).unwrap();
    assert_eq!(
        DeploymentSpecificationFile::from_file_content(&emitted).unwrap(),
        deployment
    );
}

/// Parse the checked-in devnet plan, including node URLs, fees, and publish transactions.
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
    let emitted = serde_saphyr::to_string(&deployment).unwrap();
    assert_eq!(
        DeploymentSpecificationFile::from_file_content(&emitted).unwrap(),
        deployment
    );
}

/// Unquoted legacy boolean words stay strings through the custom transaction deserializer.
#[test]
fn unquoted_call_strings_in_all_transaction_formats() {
    let fields = indoc::indoc! {"
        contract-id: ST000000000000000000002AMW42H.test
        expected-sender: ST000000000000000000002AMW42H
        method: on
        parameters: [yes, NO]
        cost: 1
    "};
    let indented: String = fields.lines().map(|line| format!("  {line}\n")).collect();
    for input in [
        format!("transaction-type: contract-call\n{fields}"),
        format!("contract-call:\n{indented}"),
        format!("!contract-call\n{fields}"),
    ] {
        let TransactionSpecificationFile::ContractCall(call) = from_yaml(input.as_bytes()).unwrap()
        else {
            panic!("expected contract call");
        };
        assert_eq!(call.method, "on");
        assert_eq!(call.parameters, ["yes", "NO"]);
    }
}
