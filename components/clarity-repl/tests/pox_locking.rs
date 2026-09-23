//! Stacking must lock STX whichever of the two boot addresses the PoX
//! contract was called through — see
//! <https://github.com/stx-labs/clarinet/issues/2491>.
//!
//! Simnet deploys the boot contracts under both addresses, but its chain state
//! is testnet-flavored: stacks-core's PoX handler keys off
//! `GlobalContext::mainnet`, so only `ST000...` moves consensus state. Calls
//! aimed at `SP000...` are redirected to the twin, which is the direct-call
//! half of the fix; the other half is the deployment-time source remap in
//! `clarinet-deployments`.

use clarity::types::StacksEpochId;
use clarity::vm::types::TupleData;
use clarity::vm::{ClarityName, EvaluationResult, SymbolicExpression, Value};
use clarity_repl::repl::boot::{BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS};
use clarity_repl::repl::post_conditions::PostConditionCheck;
use clarity_repl::repl::session::CallKind;
use clarity_repl::repl::settings::Account;
use clarity_repl::repl::{Session, SessionSettings};

mod common;
use common::{mock_node, remote_session};

const WALLET: &str = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
const BALANCE: u64 = 100_000_000_000;
const STACKED: u128 = 90_000_000_000;

fn session_with_wallet() -> Session {
    let mut session = Session::new(SessionSettings {
        initial_accounts: vec![Account {
            address: WALLET.to_owned(),
            balance: BALANCE,
            name: "wallet_1".to_owned(),
        }],
        ..Default::default()
    });

    session.update_epoch(StacksEpochId::Epoch24);
    session.advance_burn_chain_tip(1);
    session
}

/// `(stack-stx amount pox-addr start-burn-ht lock-period)` on pox-3.
fn stack_stx_args(amount: u128) -> Vec<SymbolicExpression> {
    let pox_addr = Value::Tuple(
        TupleData::from_data(vec![
            (
                ClarityName::from_literal("version"),
                Value::buff_from_byte(0x00_u8),
            ),
            (
                ClarityName::from_literal("hashbytes"),
                Value::buff_from(vec![0x01; 20]).unwrap(),
            ),
        ])
        .unwrap(),
    );

    vec![
        SymbolicExpression::atom_value(Value::UInt(amount)),
        SymbolicExpression::atom_value(pox_addr),
        SymbolicExpression::atom_value(Value::UInt(1)), // start-burn-ht
        SymbolicExpression::atom_value(Value::UInt(1)), // lock-period
    ]
}

#[track_caller]
fn snippet_value(result: EvaluationResult) -> Value {
    match result {
        EvaluationResult::Snippet(snippet) => snippet.result,
        EvaluationResult::Contract(_) => panic!("expected a snippet result"),
    }
}

#[track_caller]
fn locked_amount(session: &mut Session, address: &str) -> u128 {
    let result = session
        .eval(format!("(get locked (stx-account '{address}))"), false)
        .expect("stx-account should evaluate")
        .into_inner();
    snippet_value(result.result)
        .expect_u128()
        .expect("locked should be a uint")
}

#[track_caller]
fn stack_stx(session: &mut Session, pox_deployer: &str) -> Value {
    let result = session
        .call_contract_fn(
            &format!("{pox_deployer}.pox-3"),
            "stack-stx",
            &stack_stx_args(STACKED),
            WALLET,
            false,
            false,
            CallKind::Transaction,
            PostConditionCheck::Unchecked,
        )
        .expect("stack-stx should execute");
    snippet_value(result.result)
}

#[test]
fn stack_stx_locks_stx_through_either_boot_address() {
    for pox_deployer in [BOOT_TESTNET_ADDRESS, BOOT_MAINNET_ADDRESS] {
        let mut session = session_with_wallet();

        let value = stack_stx(&mut session, pox_deployer);
        assert!(
            matches!(&value, Value::Response(response) if response.committed),
            "stack-stx on {pox_deployer}.pox-3 should succeed, got {value}"
        );

        assert_eq!(
            locked_amount(&mut session, WALLET),
            STACKED,
            "a successful stack-stx on {pox_deployer}.pox-3 should lock STX"
        );
    }
}

/// The redirect is a visible rewrite of the call target, not a hidden context
/// switch: the lock lands on the contract the caller can read back.
#[test]
fn stacking_through_either_address_shares_one_state() {
    let mut session = session_with_wallet();
    stack_stx(&mut session, BOOT_MAINNET_ADDRESS);

    // A second `stack-stx`, this time spelled with the testnet address, must
    // see the lock the first one took out.
    let value = stack_stx(&mut session, BOOT_TESTNET_ADDRESS);
    assert!(
        matches!(&value, Value::Response(response) if !response.committed),
        "an already-stacked wallet should be rejected whichever address is \
         used, got {value}"
    );
}

/// Under mainnet execution simulation the remote node holds the real mainnet
/// boot contracts, so the target must be left alone.
///
/// MXS deploys no boot contracts locally — every lookup goes to the node — so
/// the assertion is on which contract the session asks the node for.
#[test]
fn mxs_does_not_redirect_the_mainnet_boot_address() {
    let (mut server, api_url) = mock_node(1);

    // The lookup we actually care about. Nothing serves it, so the call fails —
    // but only after asking for the *mainnet* contract.
    let mainnet_lookup = server
        .mock(
            "GET",
            mockito::Matcher::Regex(format!(
                r"^/v2/clarity/metadata/{BOOT_MAINNET_ADDRESS}/pox-3/.*$"
            )),
        )
        .with_status(404)
        .expect_at_least(1)
        .create();
    let testnet_lookup = server
        .mock(
            "GET",
            mockito::Matcher::Regex(format!(
                r"^/v2/clarity/metadata/{BOOT_TESTNET_ADDRESS}/pox-3/.*$"
            )),
        )
        .with_status(404)
        .expect(0)
        .create();

    let mut session = remote_session(api_url);

    let _ = session.call_contract_fn(
        &format!("{BOOT_MAINNET_ADDRESS}.pox-3"),
        "stack-stx",
        &stack_stx_args(STACKED),
        WALLET,
        false,
        false,
        CallKind::Transaction,
        PostConditionCheck::Unchecked,
    );

    mainnet_lookup.assert();
    testnet_lookup.assert();
}
