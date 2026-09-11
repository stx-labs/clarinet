use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::LazyLock;

use clarinet_deployments::types::*;
use clarinet_deployments::update_session_with_deployment_plan;
use clarinet_files::StacksNetwork;
use clarity::types::chainstate::StacksAddress;
use clarity::types::{Address, StacksEpochId};
use clarity::vm::types::StandardPrincipalData;
use clarity::vm::{ClarityVersion, ContractName, Value};
use clarity_repl::repl::boot::{
    SBTC_CONTRACTS_NAMES, SBTC_MAINNET_ADDRESS_PRINCIPAL, SBTC_TOKEN_MAINNET_ADDRESS,
};
use clarity_repl::repl::{Session, SessionSettings};

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

/// A genesis spec with a single wallet, whose `sbtc_balance` is what each test
/// varies. The STX balance is fixed so it can double as a control: it must be
/// credited whether or not the sBTC mint happens.
fn genesis_with_sbtc_balance(sbtc_balance: u128) -> GenesisSpecification {
    GenesisSpecification {
        contracts: vec![],
        wallets: vec![WalletSpecification {
            address: WALLET_1.clone(),
            balance: 100_000_000,
            name: "wallet_1".to_string(),
            sbtc_balance,
        }],
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

/// A session at epoch 3.0, funded from a one-wallet genesis spec.
fn funded_session(sbtc_balance: u128) -> Session {
    let mut session = Session::new(SessionSettings::default());
    let genesis = genesis_with_sbtc_balance(sbtc_balance);
    let deployment = build_test_deployement_plan(vec![epoch_3_0_batch()], Some(genesis));
    update_session_with_deployment_plan(&mut session, &deployment, None);
    session
}

#[test]
fn fund_genesis_account_with_stx() {
    let mut session = Session::new(SessionSettings::default());
    let genesis = genesis_with_sbtc_balance(0);
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
    let genesis = genesis_with_sbtc_balance(10_000_000_000);
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
    let session = funded_session(0);

    let assets_maps = session.get_assets_maps();
    assert!(assets_maps.len() == 1);
    assert!(assets_maps.contains_key("STX"));
}

/// The property that matters: a plan carrying *no* sBTC transaction — what a
/// stock `clarinet new` project generates — still funds the genesis wallets,
/// because `sbtc-token` is a boot contract.
#[test]
fn can_fund_initial_sbtc_balance_without_any_sbtc_transaction() {
    let session = funded_session(10_000_000_000);

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
                    emulated_sender: SBTC_MAINNET_ADDRESS_PRINCIPAL.clone(),
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

    let genesis = genesis_with_sbtc_balance(10_000_000_000);
    let deployment = build_test_deployement_plan(vec![batch], Some(genesis));
    update_session_with_deployment_plan(&mut session, &deployment, None);

    let assets_maps = session.get_assets_maps();
    let sbtcs = assets_maps
        .get(".sbtc-token.sbtc-token")
        .expect("sBTC should be minted");
    assert_eq!(sbtcs.get(&WALLET_1.to_string()), Some(&10_000_000_000));
}

/// The balance is written to the datastore, so it has to be visible to Clarity
/// itself and not just to the session's asset map.
#[test]
fn can_read_the_funded_sbtc_balance_and_supply_from_clarity() {
    let mut session = funded_session(10_000_000_000);

    let token = format!("'{}", *SBTC_TOKEN_MAINNET_ADDRESS);
    let balance = session.eval_clarity_string(&format!(
        "(contract-call? {token} get-balance '{})",
        *WALLET_1
    ));
    let supply = session.eval_clarity_string(&format!("(contract-call? {token} get-total-supply)"));

    let expected = Value::okay(Value::UInt(10_000_000_000)).unwrap();
    assert_eq!(balance.match_atom_value(), Some(&expected));
    assert_eq!(supply.match_atom_value(), Some(&expected));
}

/// sBTC funding is the last thing the plan does, so the epoch it ran at is
/// still recorded when it returns. It has to be the session's own epoch.
#[test]
fn funding_sbtc_runs_at_the_session_epoch() {
    use clarity::vm::database::ClarityBackingStore;

    let mut session = funded_session(10_000_000_000);
    let epoch = session.interpreter.datastore.get_current_epoch();
    let clarity_db_epoch = session
        .interpreter
        .clarity_datastore
        .get_data("vm-epoch::epoch-version");

    assert_eq!(epoch, StacksEpochId::Epoch30);
    assert_eq!(clarity_db_epoch, Ok(Some(format!("{:08x}", epoch as u32))));
}
