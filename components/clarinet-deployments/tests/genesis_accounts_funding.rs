use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::LazyLock;

use clarinet_deployments::types::*;
use clarinet_deployments::update_session_with_deployment_plan;
use clarinet_files::StacksNetwork;
use clarity::types::chainstate::StacksAddress;
use clarity::types::Address;
use clarity::vm::types::StandardPrincipalData;
use clarity::vm::{ClarityVersion, ContractName};
use clarity_repl::repl::boot::SBTC_CONTRACTS_NAMES;
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

/// An empty batch that only moves the session to epoch 3.0, where the sBTC
/// boot contracts are deployed. A stock project reaches at least this epoch.
fn epoch_3_0_batch() -> TransactionsBatchSpecification {
    TransactionsBatchSpecification {
        id: 0,
        epoch: Some(EpochSpec::Epoch3_0),
        transactions: vec![],
    }
}

#[test]
fn fund_genesis_account_with_stx() {
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

/// The sBTC contracts are boot contracts from epoch 3.0 on, so a plan that
/// never reaches epoch 3.0 has no way to mint. `sbtc_balance` is then ignored
/// rather than failing the session.
#[test]
fn does_not_fund_sbtc_before_epoch_3_0() {
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
    let batch = TransactionsBatchSpecification {
        id: 0,
        epoch: Some(EpochSpec::Epoch2_5),
        transactions: vec![],
    };
    let deployment = build_test_deployement_plan(vec![batch], Some(genesis));
    update_session_with_deployment_plan(&mut session, &deployment, None);

    let assets_maps = session.get_assets_maps();
    assert!(assets_maps.len() == 1);
    assert!(assets_maps.contains_key("STX"));
}

/// A wallet with `sbtc_balance = 0` must not show up in the sBTC asset map.
#[test]
fn does_not_fund_sbtc_when_the_balance_is_zero() {
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
    let deployment = build_test_deployement_plan(vec![epoch_3_0_batch()], Some(genesis));
    update_session_with_deployment_plan(&mut session, &deployment, None);

    let assets_maps = session.get_assets_maps();
    assert!(assets_maps.len() == 1);
    assert!(assets_maps.contains_key("STX"));
}

/// The property that matters: a plan carrying *no* sBTC transaction — what a
/// stock `clarinet new` project generates — still funds the genesis wallets,
/// because `sbtc-deposit` is a boot contract.
#[test]
fn can_fund_initial_sbtc_balance_without_any_sbtc_transaction() {
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
    let deployment = build_test_deployement_plan(vec![epoch_3_0_batch()], Some(genesis));
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

/// An explicit sBTC requirement remains harmless: the contracts are already
/// deployed as boot contracts, so the publish is skipped and the funding is
/// unaffected.
#[test]
fn can_fund_initial_sbtc_balance_with_explicit_sbtc_requirements() {
    let mut session = Session::new(SessionSettings::default());

    let transactions = SBTC_CONTRACTS_NAMES
        .iter()
        .map(|contract_name| {
            TransactionSpecification::EmulatedContractPublish(
                EmulatedContractPublishSpecification {
                    contract_name: ContractName::try_from(contract_name.to_string()).unwrap(),
                    source: "(define-read-only (unused) u1)".to_string(),
                    clarity_version: ClarityVersion::Clarity3,
                    location: PathBuf::from(format!("./requirements/{contract_name}.clar")),
                    emulated_sender: SBTC_DEPLOYER.clone(),
                    skip_analysis: true,
                },
            )
        })
        .collect::<Vec<_>>();

    let batch = TransactionsBatchSpecification {
        id: 0,
        epoch: Some(EpochSpec::Epoch3_0),
        transactions,
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
    let sbtcs = assets_maps
        .get(".sbtc-token.sbtc-token")
        .expect("sBTC should be minted");
    assert_eq!(sbtcs.get(&WALLET_1.to_string()), Some(&10_000_000_000));
}
