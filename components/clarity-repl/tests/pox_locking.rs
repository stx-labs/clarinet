//! Stacking must lock STX whichever of the two boot addresses the PoX contract
//! was called through — see <https://github.com/stx-labs/clarinet/issues/2491>.

use clarity::types::StacksEpochId;
use clarity::vm::types::TupleData;
use clarity::vm::{ClarityName, EvaluationResult, SymbolicExpression, Value};
use clarity_repl::repl::boot::{BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS};
use clarity_repl::repl::settings::Account;
use clarity_repl::repl::{Session, SessionSettings};

const WALLET: &str = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
const BALANCE: u64 = 100_000_000_000;
const STACKED: u128 = 90_000_000_000;

fn session_with_wallet(epoch: StacksEpochId) -> Session {
    let mut session = Session::new(SessionSettings {
        initial_accounts: vec![Account {
            address: WALLET.to_owned(),
            balance: BALANCE,
            name: "wallet_1".to_owned(),
        }],
        ..Default::default()
    });

    session.update_epoch(epoch);
    session.advance_burn_chain_tip(1);
    session
}

/// `(stack-stx amount pox-addr start-burn-ht lock-period)` on pox-3
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
fn assert_stack_stx_locks(pox_deployer: &str) {
    let mut session = session_with_wallet(StacksEpochId::Epoch24);

    let result = session
        .call_contract_fn(
            &format!("{pox_deployer}.pox-3"),
            "stack-stx",
            &stack_stx_args(STACKED),
            WALLET,
            false,
            false,
        )
        .expect("stack-stx should execute");

    let value = snippet_value(result.result);
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

/// Without the lock there is no stacking entry in the asset map, so the
/// `(with-staking ...)` allowance has nothing to check and never fires.
mod pox_5_stake {
    use clarity::consts::CHAIN_ID_TESTNET;
    use clarity::types::PrivateKey;
    use clarity::util::hash::Sha256Sum;
    use clarity::util::secp256k1::{Secp256k1PrivateKey, Secp256k1PublicKey};
    use clarity::vm::types::PrincipalData;
    use clarity::vm::ContractName;
    use clarity_repl::repl::{ClarityCodeSource, ClarityContract, ContractDeployer, Epoch};
    use clarity_types::types::QualifiedContractIdentifier;
    use clarity_types::ClarityVersion;

    use super::*;

    const DEPLOYER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
    const SIGNER_NAME: &str = "minimal-signer";
    const PROBE_NAME: &str = "allowance-probe";
    /// over pox-5's `SIGNER_SET_MIN_USTX` (50k STX)
    const STAKE: u128 = 51_000_000_000;
    const FUNDED: u64 = 60_000_000_000;
    const AUTH_ID: u128 = 1;

    /// The smallest signer-manager pox-5 accepts, so the probe next door
    /// performs a real stake.
    const SIGNER_SOURCE: &str = r#"
(use-trait signer-mgr 'SP000000000000000000002Q6VF78.pox-5.signer-manager-trait)
(impl-trait 'SP000000000000000000002Q6VF78.pox-5.signer-manager-trait)

(define-public (validate-stake!
        (staker principal) (first-index uint) (num-indexes uint)
        (amount-ustx uint) (amount-sats uint) (is-bond bool)
        (signer-calldata (optional (buff 500))))
    (ok true))

(define-public (register-self
        (signer-manager <signer-mgr>) (signer-key (buff 33))
        (auth-id uint) (signer-sig (buff 65)))
    (begin
        (try! (contract-call? 'SP000000000000000000002Q6VF78.pox-5 grant-signer-key
            signer-key current-contract auth-id signer-sig))
        (contract-call? 'SP000000000000000000002Q6VF78.pox-5 register-signer
            signer-manager signer-key)))
"#;

    /// Two entry points that differ only in the allowance they declare.
    const PROBE_SOURCE: &str = r#"
(use-trait signer-mgr 'SP000000000000000000002Q6VF78.pox-5.signer-manager-trait)
(define-constant POX5 'SP000000000000000000002Q6VF78.pox-5)
(define-constant NUM-CYCLES u1)

(define-public (stake-declared (signer <signer-mgr>) (amount uint))
    (as-contract? ((with-staking amount))
        (try! (contract-call? POX5 stake signer amount NUM-CYCLES burn-block-height none))))

(define-public (stake-underdeclared (signer <signer-mgr>) (amount uint))
    (as-contract? ((with-staking u1))
        (try! (contract-call? POX5 stake signer amount NUM-CYCLES burn-block-height none))))
"#;

    fn contract(name: &str, source: &str) -> ClarityContract {
        ClarityContract {
            code_source: ClarityCodeSource::ContractInMemory(source.to_owned()),
            deployer: ContractDeployer::Address(DEPLOYER.to_owned()),
            name: name.to_owned(),
            epoch: Epoch::Specific(StacksEpochId::Epoch40),
            clarity_version: ClarityVersion::Clarity6,
            skip_analysis: false,
        }
    }

    fn signer_principal() -> PrincipalData {
        PrincipalData::Contract(QualifiedContractIdentifier::new(
            PrincipalData::parse_standard_principal(DEPLOYER).unwrap(),
            ContractName::from_literal(SIGNER_NAME),
        ))
    }

    /// SIP-018 hash of pox-5's `grant-authorization` message, matching
    /// `get-signer-grant-message-hash` in pox-5.clar.
    fn grant_message_hash() -> Sha256Sum {
        let domain = Value::Tuple(
            TupleData::from_data(vec![
                (
                    ClarityName::from_literal("name"),
                    Value::string_ascii_from_bytes("pox-5-signer".into()).unwrap(),
                ),
                (
                    ClarityName::from_literal("version"),
                    Value::string_ascii_from_bytes("1.0.0".into()).unwrap(),
                ),
                (
                    ClarityName::from_literal("chain-id"),
                    Value::UInt(CHAIN_ID_TESTNET.into()),
                ),
            ])
            .unwrap(),
        );
        let data = Value::Tuple(
            TupleData::from_data(vec![
                (
                    ClarityName::from_literal("topic"),
                    Value::string_ascii_from_bytes("grant-authorization".into()).unwrap(),
                ),
                (
                    ClarityName::from_literal("signer-manager"),
                    Value::Principal(signer_principal()),
                ),
                (ClarityName::from_literal("auth-id"), Value::UInt(AUTH_ID)),
            ])
            .unwrap(),
        );

        let message = [
            b"SIP018".as_ref(),
            Sha256Sum::from_data(&domain.serialize_to_vec().unwrap()).as_bytes(),
            Sha256Sum::from_data(&data.serialize_to_vec().unwrap()).as_bytes(),
        ]
        .concat();
        Sha256Sum::from_data(&message)
    }

    /// Deploys the contracts, registers the signer through the real pox-5 grant
    /// path and funds the probe contract.
    fn staged_session() -> Session {
        let mut session = Session::new(SessionSettings {
            initial_accounts: vec![Account {
                address: DEPLOYER.to_owned(),
                balance: BALANCE,
                name: "deployer".to_owned(),
            }],
            ..Default::default()
        });

        for epoch in [
            StacksEpochId::Epoch25,
            StacksEpochId::Epoch30,
            StacksEpochId::Epoch34,
            StacksEpochId::Epoch40,
        ] {
            session.update_epoch(epoch);
            session.advance_burn_chain_tip(10);
        }

        // mimics stacks-core's `initialize_epoch_4_0`
        session
            .call_contract_fn(
                &format!("{BOOT_MAINNET_ADDRESS}.pox-5"),
                "set-burnchain-parameters",
                &[
                    SymbolicExpression::atom_value(Value::UInt(0)), // first-burn-height
                    SymbolicExpression::atom_value(Value::UInt(10)), // prepare-cycle-length
                    SymbolicExpression::atom_value(Value::UInt(100)), // reward-cycle-length
                    SymbolicExpression::atom_value(Value::UInt(1)), // begin-pox5-reward-cycle
                ],
                BOOT_MAINNET_ADDRESS,
                false,
                false,
            )
            .expect("set-burnchain-parameters should execute");

        session.set_tx_sender(DEPLOYER);
        for (name, source) in [(SIGNER_NAME, SIGNER_SOURCE), (PROBE_NAME, PROBE_SOURCE)] {
            let result = session
                .deploy_contract(&contract(name, source), false, None)
                .unwrap_or_else(|diagnostics| panic!("{name} should deploy: {diagnostics:?}"));
            assert!(
                result.diagnostics.is_empty(),
                "{name} should deploy cleanly: {:?}",
                result.diagnostics
            );
        }

        let signer_key = Secp256k1PrivateKey::from_seed(&[0x11; 32]);
        let signature = signer_key
            .sign(grant_message_hash().as_bytes())
            .expect("grant message should be signable");
        let signer_pubkey = Secp256k1PublicKey::from_private(&signer_key);

        let result = session
            .call_contract_fn(
                &format!("{DEPLOYER}.{SIGNER_NAME}"),
                "register-self",
                &[
                    SymbolicExpression::atom_value(Value::Principal(signer_principal())),
                    SymbolicExpression::atom_value(
                        Value::buff_from(signer_pubkey.to_bytes_compressed()).unwrap(),
                    ),
                    SymbolicExpression::atom_value(Value::UInt(AUTH_ID)),
                    SymbolicExpression::atom_value(Value::buff_from(signature.to_rsv()).unwrap()),
                ],
                DEPLOYER,
                false,
                false,
            )
            .expect("register-self should execute");
        let value = snippet_value(result.result);
        assert!(
            matches!(&value, Value::Response(response) if response.committed),
            "signer registration should succeed, got {value}"
        );

        session.set_tx_sender(DEPLOYER);
        session
            .stx_transfer(FUNDED, &format!("{DEPLOYER}.{PROBE_NAME}"))
            .expect("funding the probe contract should execute");

        session
    }

    fn stake(session: &mut Session, method: &str) -> Value {
        let signer = SymbolicExpression::atom_value(Value::Principal(signer_principal()));
        let result = session
            .call_contract_fn(
                &format!("{DEPLOYER}.{PROBE_NAME}"),
                method,
                &[signer, SymbolicExpression::atom_value(Value::UInt(STAKE))],
                DEPLOYER,
                false,
                false,
            )
            .unwrap_or_else(|diagnostics| panic!("{method} should execute: {diagnostics:?}"));
        snippet_value(result.result)
    }

    #[test]
    fn stake_locks_the_staked_stx() {
        let mut session = staged_session();

        let value = stake(&mut session, "stake-declared");
        assert!(
            matches!(&value, Value::Response(response) if response.committed),
            "stake-declared should succeed, got {value}"
        );

        assert_eq!(
            locked_amount(&mut session, &format!("{DEPLOYER}.{PROBE_NAME}")),
            STAKE,
            "a successful pox-5 stake should lock the staked STX"
        );
    }

    #[test]
    fn under_declared_with_staking_allowance_aborts() {
        let mut session = staged_session();

        let value = stake(&mut session, "stake-underdeclared");
        assert!(
            matches!(&value, Value::Response(response) if !response.committed),
            "staking {STAKE} under a `(with-staking u1)` allowance should abort, got {value}"
        );

        assert_eq!(
            locked_amount(&mut session, &format!("{DEPLOYER}.{PROBE_NAME}")),
            0,
            "an aborted stake should leave nothing locked"
        );
    }
}
