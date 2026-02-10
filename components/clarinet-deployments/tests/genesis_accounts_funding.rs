use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::LazyLock;

use clarinet_deployments::types::*;
use clarinet_deployments::update_session_with_deployment_plan;
use clarinet_files::StacksNetwork;
use clarity::types::chainstate::StacksAddress;
use clarity::types::Address;
use clarity::vm::types::StandardPrincipalData;
use clarity_repl::clarity::{ClarityVersion, ContractName};
use clarity_repl::repl::{Session, SessionSettings};

static SBTC_DEPLOYER: LazyLock<StandardPrincipalData> = LazyLock::new(|| {
    StandardPrincipalData::from(
        StacksAddress::from_string("SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4").unwrap(),
    )
});
static WALLET_1: LazyLock<StandardPrincipalData> = LazyLock::new(|| {
    StandardPrincipalData::from(
        StacksAddress::from_string("ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5").unwrap(),
    )
});

fn build_test_deployement_plan(
    batches: Vec<TransactionsBatchSpecification>,
    genesis: Option<GenesisSpecification>,
) -> DeploymentSpecification {
    DeploymentSpecification {
        id: 1,
        name: "test".to_string(),
        network: StacksNetwork::Simnet,
        stacks_node: None,
        bitcoin_node: None,
        genesis,
        contracts: BTreeMap::new(),
        plan: TransactionPlanSpecification { batches },
    }
}

#[test]
fn fund_geneis_account_with_stx() {
    let mut session = Session::new(SessionSettings::default());
    let genesis = GenesisSpecification {
        contracts: vec![],
        wallets: vec![WalletSpecification {
            address: WALLET_1.clone(),
            balance: 100_000_000,
            name: "wallet_1".to_string(),
            sbtc_balance: 0,
        }],
    };
    let deployment = build_test_deployement_plan(vec![], Some(genesis));
    update_session_with_deployment_plan(&mut session, &deployment, None);

    let assets_maps = session.get_assets_maps();
    assert!(assets_maps.len() == 1);
    assert!(assets_maps.contains_key("STX"));
    let stxs = assets_maps.get("STX").unwrap();
    assert_eq!(stxs.get(&WALLET_1.to_string()), Some(&100_000_000));
}

#[test]
fn does_not_fund_sbtc_without_sbtc_contract() {
    let mut session = Session::new(SessionSettings::default());
    let genesis = GenesisSpecification {
        contracts: vec![],
        wallets: vec![WalletSpecification {
            address: WALLET_1.clone(),
            balance: 100_000_000,
            name: "wallet_1".to_string(),
            sbtc_balance: 10_000_000_000,
        }],
    };
    let deployment = build_test_deployement_plan(vec![], Some(genesis));
    update_session_with_deployment_plan(&mut session, &deployment, None);

    let assets_maps = session.get_assets_maps();
    assert!(assets_maps.len() == 1);
    assert!(assets_maps.contains_key("STX"));
}

#[test]
fn can_fund_initial_sbtc_balance() {
    let mut session = Session::new(SessionSettings::default());

    let sbtc_contracts = [
        (
            "sbtc-registry",
            include_str!("./fixtures/sbtc-registry.clar"),
        ),
        ("sbtc-token", include_str!("./fixtures/sbtc-token.clar")),
        ("sbtc-deposit", include_str!("./fixtures/sbtc-deposit.clar")),
    ];

    let contract_requirements_txs = sbtc_contracts
        .iter()
        .map(|(contract_name, source)| {
            TransactionSpecification::EmulatedContractPublish(
                EmulatedContractPublishSpecification {
                    contract_name: ContractName::try_from(contract_name.to_string()).unwrap(),
                    source: source.to_string(),
                    clarity_version: ClarityVersion::Clarity3,
                    location: PathBuf::from("./fixtures/sbtc-registry.clar"),
                    emulated_sender: SBTC_DEPLOYER.clone(),
                },
            )
        })
        .collect::<Vec<_>>();

    let batch = TransactionsBatchSpecification {
        id: 0,
        epoch: Some(EpochSpec::Epoch3_0),
        transactions: contract_requirements_txs,
    };

    let genesis = GenesisSpecification {
        contracts: vec![],
        wallets: vec![WalletSpecification {
            address: WALLET_1.clone(),
            balance: 100_000_000,
            name: "wallet_1".to_string(),
            sbtc_balance: 10_000_000_000,
        }],
    };
    let deployment = build_test_deployement_plan(vec![batch], Some(genesis));
    update_session_with_deployment_plan(&mut session, &deployment, None);

    let assets_maps = session.get_assets_maps();
    assert!(assets_maps.len() == 2);
    assert!(assets_maps.contains_key("STX"));
    assert!(assets_maps.contains_key(".sbtc-token.sbtc-token"));
    let stxs = assets_maps.get("STX").unwrap();
    assert_eq!(stxs.get(&WALLET_1.to_string()), Some(&100_000_000));
    let sbtcs = assets_maps.get(".sbtc-token.sbtc-token").unwrap();
    assert_eq!(sbtcs.get(&WALLET_1.to_string()), Some(&10_000_000_000));
}
