use clarity::types::StacksEpochId;
use clarity::util::hash::hex_bytes;
use clarity::vm::{ClarityVersion, EvaluationResult, Value};
use clarity_repl::repl::{self, ClarityContract, Session, SessionSettings};
use indoc::indoc;

#[track_caller]
fn eval_snippet(session: &mut Session, snippet: &str) -> Value {
    let execution_res = session.eval(snippet.to_string(), false).unwrap();
    match execution_res.result {
        EvaluationResult::Contract(_) => unreachable!(),
        EvaluationResult::Snippet(res) => res.result,
    }
}

fn init_session_epoch_24() -> Session {
    use StacksEpochId::*;
    let mut session = Session::new(SessionSettings::default());

    // move through certain epochs to populate the datastore
    let epochs = [Epoch21, Epoch24];
    epochs.iter().for_each(|epoch| {
        session.update_epoch(*epoch);
        session.advance_burn_chain_tip(10);
    });
    session
}

fn init_session() -> Session {
    use StacksEpochId::*;
    let mut session = Session::new(SessionSettings::default());

    // move through certain epochs to populate the datastore
    let epochs = [Epoch24, Epoch25, Epoch30, Epoch32];
    epochs.iter().for_each(|epoch| {
        session.update_epoch(*epoch);
        session.advance_burn_chain_tip(10);
    });
    session
}

#[test]
fn it_can_get_heights() {
    let mut session = init_session();

    let result = eval_snippet(&mut session, "stacks-block-height");
    assert_eq!(result, Value::UInt(42));
    let result = eval_snippet(&mut session, "burn-block-height");
    assert_eq!(result, Value::UInt(42));
    let result = eval_snippet(&mut session, "tenure-height");
    assert_eq!(result, Value::UInt(42));

    session.advance_burn_chain_tip(1);
    let _ = session.advance_stacks_chain_tip(1);

    let result = eval_snippet(&mut session, "stacks-block-height");
    assert_eq!(result, Value::UInt(44));
    let result = eval_snippet(&mut session, "burn-block-height");
    assert_eq!(result, Value::UInt(43));
    let result = eval_snippet(&mut session, "tenure-height");
    assert_eq!(result, Value::UInt(43));
}

#[test]
fn it_handles_clarity2_block_height_in_epoch3() {
    let mut session = init_session();
    let epoch = session.get_epoch();
    assert_eq!(epoch, "Current epoch: 3.2");

    let tenure_height = eval_snippet(&mut session, "tenure-height");
    assert_eq!(tenure_height, Value::UInt(42));

    let deployer = "ST23YMXQ25679FCF71F8FRGYPQBZQJFJWA4GFT84T";

    // block-height is a Clarity 1 & 2 keyword
    // in epoch 3.2, it should return the tenure-height
    let contract = ClarityContract {
        name: "gbh".into(),
        code_source: repl::ClarityCodeSource::ContractInMemory(
            "(define-read-only (get-block-height) block-height)".into(),
        ),
        deployer: repl::ContractDeployer::Address(deployer.into()),
        clarity_version: ClarityVersion::Clarity2,
        epoch: repl::Epoch::Specific(StacksEpochId::Epoch32),
    };
    let result = session.deploy_contract(&contract, false, None, true);
    assert!(result.is_ok());

    let snippet = format!("(contract-call? '{deployer}.gbh get-block-height)");
    let result = eval_snippet(&mut session, &snippet);
    assert_eq!(result, Value::UInt(42));

    session.advance_burn_chain_tip(1);
    let snippet = format!("(contract-call? '{deployer}.gbh get-block-height)");
    let result = eval_snippet(&mut session, &snippet);
    assert_eq!(result, Value::UInt(43));
}

#[test]
fn it_handles_clarity2_get_block_info_in_epoch2() {
    let mut session = init_session_epoch_24();
    let epoch = session.get_epoch();
    assert_eq!(epoch, "Current epoch: 2.4");
    let block_height = eval_snippet(&mut session, "block-height");
    println!("block-height: {block_height:?}");
    assert!(block_height == Value::UInt(20));

    let deployer = "ST23YMXQ25679FCF71F8FRGYPQBZQJFJWA4GFT84T";

    let contract = indoc!(
        "(define-read-only (get-block-hash (h uint))
            (get-block-info? id-header-hash h)
        )
        (define-read-only (get-burn-block-hash (h uint))
            (get-block-info? burnchain-header-hash h)
        )"
    );
    let contract = ClarityContract {
        name: "gbh".into(),
        code_source: repl::ClarityCodeSource::ContractInMemory(contract.into()),
        deployer: repl::ContractDeployer::Address(deployer.into()),
        clarity_version: ClarityVersion::Clarity2,
        epoch: repl::Epoch::Specific(StacksEpochId::Epoch24),
    };
    let result = session.deploy_contract(&contract, false, None, true);
    assert!(result.is_ok());

    let snippet = format!("(contract-call? '{deployer}.gbh get-block-hash u9)");
    let result = eval_snippet(&mut session, &snippet);
    let expected_hash = Value::buff_from(
        hex_bytes("d3128940fbe65bd02156e79e09b8f84cf889c7353c9cd16e7f43a3f60902ca90").unwrap(),
    )
    .unwrap();
    assert_eq!(result, Value::some(expected_hash).unwrap());

    let snippet = format!("(contract-call? '{deployer}.gbh get-burn-block-hash u9)");
    let result = eval_snippet(&mut session, &snippet);
    let expected_hash = Value::buff_from(
        hex_bytes("02128940fbe65bd02156e79e09b8f84cf889c7353c9cd16e7f43a3f60902ca90").unwrap(),
    )
    .unwrap();
    assert_eq!(result, Value::some(expected_hash).unwrap());
}

#[test]
fn it_handles_clarity2_get_block_info_in_epoch3() {
    // using mainnet data for this test to ease testing on epoch 2.x
    let mut session = init_session();
    let epoch = session.get_epoch();
    assert_eq!(epoch, "Current epoch: 3.2");

    let tenure_height = eval_snippet(&mut session, "tenure-height");
    assert_eq!(tenure_height, Value::UInt(42));

    let deployer = "ST23YMXQ25679FCF71F8FRGYPQBZQJFJWA4GFT84T";

    let contract = indoc!(
        "(define-read-only (get-block-hash (h uint))
            (get-block-info? id-header-hash h)
        )
        (define-read-only (get-burn-block-hash (h uint))
            (get-block-info? burnchain-header-hash h)
        )"
    );
    let contract = ClarityContract {
        name: "gbh".into(),
        code_source: repl::ClarityCodeSource::ContractInMemory(contract.into()),
        deployer: repl::ContractDeployer::Address(deployer.into()),
        clarity_version: ClarityVersion::Clarity2,
        epoch: repl::Epoch::Specific(StacksEpochId::Epoch32),
    };
    let result = session.deploy_contract(&contract, false, None, true);
    assert!(result.is_ok());

    let snippet = format!("(contract-call? '{deployer}.gbh get-block-hash u41)");
    let result = eval_snippet(&mut session, &snippet);
    let expected_hash = Value::buff_from(
        hex_bytes("00e1081e314df590500b7cce0b16a143ccc6f8c877fb014a3f5cd974e692e2cd").unwrap(),
    )
    .unwrap();
    assert_eq!(result, Value::some(expected_hash).unwrap());

    let snippet = format!("(contract-call? '{deployer}.gbh get-burn-block-hash u41)");
    let result = eval_snippet(&mut session, &snippet);
    let expected_hash = Value::buff_from(
        hex_bytes("02e1081e314df590500b7cce0b16a143ccc6f8c877fb014a3f5cd974e692e2cd").unwrap(),
    )
    .unwrap();
    assert_eq!(result, Value::some(expected_hash).unwrap());
}
