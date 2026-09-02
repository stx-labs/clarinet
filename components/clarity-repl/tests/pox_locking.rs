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
use clarity_repl::repl::session::CallKind;
use clarity_repl::repl::settings::{Account, ApiUrl, RemoteDataSettings};
use clarity_repl::repl::{Session, SessionSettings};

const WALLET: &str = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
const BALANCE: u64 = 100_000_000_000;
const STACKED: u128 = 90_000_000_000;

fn session_with_wallet(settings: SessionSettings) -> Session {
    let mut session = Session::new(SessionSettings {
        initial_accounts: vec![Account {
            address: WALLET.to_owned(),
            balance: BALANCE,
            name: "wallet_1".to_owned(),
        }],
        ..settings
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
        )
        .expect("stack-stx should execute");
    snippet_value(result.result)
}

#[track_caller]
fn assert_stack_stx_locks(pox_deployer: &str) {
    let mut session = session_with_wallet(SessionSettings::default());

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

#[test]
fn stack_stx_locks_stx_with_testnet_boot_address() {
    assert_stack_stx_locks(BOOT_TESTNET_ADDRESS);
}

#[test]
fn stack_stx_locks_stx_with_mainnet_boot_address() {
    assert_stack_stx_locks(BOOT_MAINNET_ADDRESS);
}

/// The redirect is a visible rewrite of the call target, not a hidden context
/// switch: the lock lands on the contract the caller can read back.
#[test]
fn stacking_through_either_address_shares_one_state() {
    let mut session = session_with_wallet(SessionSettings::default());
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
    let mut server = mockito::Server::new();

    // Anything not mocked below answers 404 rather than mockito's default 501,
    // which the client would treat as retryable and sleep 3s over.
    let _catch_all = server
        .mock("GET", mockito::Matcher::Any)
        .with_status(404)
        .expect_at_least(0)
        .create();

    #[rustfmt::skip]
    let info = indoc::indoc!(r#"
        {
            "peer_version": 402653196,
            "pox_consensus": "0ce291b675bb0148b435a884e250aafc3fd6bc86",
            "burn_block_height": 882262,
            "stable_pox_consensus": "f517f5aced5be836f9fe10980ff06108a6a2acec",
            "stable_burn_block_height": 882255,
            "network_id": 1,
            "parent_network_id": 3652501241,
            "stacks_tip_height": 556946,
            "stacks_tip": "70526983b920b31d5e0d65750033a4dc2f328f31a3ffeb1f8780bfb164d50502",
            "stacks_tip_consensus_hash": "0ce291b675bb0148b435a884e250aafc3fd6bc86",
            "genesis_chainstate_hash": "74237aa39aa50a83de11a4f53e9d3bb7d43461d1de9873f402e5453ae60bc59b",
            "unanchored_tip": null,
            "unanchored_seq": null,
            "tenure_height": 184037,
            "is_fully_synced": true,
            "node_public_key": "02e0ce39375d699d164f90cc815427943c5acccca02069e394f9ed28d2c2bca317",
            "node_public_key_hash": "d5b1f3c7f9b2ffa8ac610170d1352550d240197c",
            "stackerdbs": []
        }
    "#);
    let _info = server
        .mock("GET", "/v2/info")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(info)
        .create();

    #[rustfmt::skip]
    let block = indoc::indoc!(r#"
        {
            "canonical": true,
            "height": 556946,
            "hash": "0xaff3b535a135348ed00023ec1bdc3da9005253a9ce80a4906ade03ea6685d342",
            "block_time": 1735934294,
            "block_time_iso": "2025-01-03T19:58:14.000Z",
            "tenure_height": 184037,
            "index_block_hash": "0x201cf66636e693d95998b40ddd0cbe038432806046eed11866052f15a9fa8fc5",
            "parent_block_hash": "0x94c3d8f56ed2e1093f26089572af9cc5d5b097d461dcc184196f1ee2070de063",
            "parent_index_block_hash": "0x1969bdddb9902162f5fdd2ff49cabb30300a9819c89bedd4c27fed82f8c9cf4b",
            "burn_block_time": 1735451504,
            "burn_block_time_iso": "2024-12-29T05:51:44.000Z",
            "burn_block_hash": "0x57f3e2bd4519e4263353bf6b7614a9cee7f2d36fe61409852d42e41afe5e6cad",
            "burn_block_height": 882262,
            "miner_txid": "0x5fb426cf9eb4577b545bd731634886d5bd5c9d40d573e2cdb95100f483913491",
            "tx_count": 2
        }
    "#);
    let _blocks = server
        .mock(
            "GET",
            mockito::Matcher::Regex(r"^/extended/v2/blocks/.*$".to_string()),
        )
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(block)
        .expect_at_least(0)
        .create();

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

    let mut session = Session::new(SessionSettings {
        repl_settings: clarity_repl::repl::Settings {
            remote_data: RemoteDataSettings {
                enabled: true,
                api_url: ApiUrl(server.url()),
                initial_height: Some(556946),
                use_mainnet_wallets: false,
            },
            ..Default::default()
        },
        ..Default::default()
    });

    let _ = session.call_contract_fn(
        &format!("{BOOT_MAINNET_ADDRESS}.pox-3"),
        "stack-stx",
        &stack_stx_args(STACKED),
        WALLET,
        false,
        false,
        CallKind::Transaction,
    );

    mainnet_lookup.assert();
    testnet_lookup.assert();
}
