use std::hint::black_box;

use clarity::vm::{EvaluationResult, ExecutionResult, SymbolicExpression, Value as ClarityValue};
use clarity_repl::repl::{
    ClarityCodeSource, ClarityContract, ContractDeployer, Epoch, Session, SessionSettings,
    DEFAULT_CLARITY_VERSION, DEFAULT_EPOCH,
};
use clarity_types::types::QualifiedContractIdentifier;
use divan::Bencher;

fn init_session() -> Session {
    let mut session = Session::new(SessionSettings::default());
    session.update_epoch(DEFAULT_EPOCH);
    session.advance_burn_chain_tip(1);
    assert_eq!(session.interpreter.get_block_height(), 2);

    let src = [
        "(define-data-var buff-data (buff 32) 0x01)",
        "(define-map history uint (buff 32))",
        "(define-read-only (noop-ro (i uint) (d (buff 32)))",
        "  (ok true)",
        ")",
        "(define-public (noop-pub (i uint) (d (buff 32)))",
        "  (ok true)",
        ")",
        "(define-public (save (i uint) (d (buff 32)))",
        "  (begin",
        "    (map-insert history i d)",
        "    (ok (var-set buff-data d))",
        "  )",
        ")",
        "(define-read-only (read-ab (height uint))",
        "  (at-block (unwrap-panic (get-stacks-block-info? id-header-hash height)) (var-get buff-data))",
        ")",

        // this is a simplified function that computes the fibonacci sequence
        // by only storing the current value for the current block height
        "(define-data-var current uint u1)",
        "(define-constant deployed-at stacks-block-height)",
        "(define-public (fib)",
        "  (let ((previous (if (<= (- stacks-block-height u2) deployed-at)",
        "    u0",
        "    (at-block",
        "      (unwrap! (get-stacks-block-info? id-header-hash (- stacks-block-height u2)) (err u1))",
        "      (var-get current))",
        "    )",
        "  ))",
        "    (ok (var-set current (+ previous (var-get current))))",
        "  )",
        ")",
    ]
    .join("\n");

    let contract = ClarityContract {
        code_source: ClarityCodeSource::ContractInMemory(src),
        name: "contract".into(),
        deployer: ContractDeployer::DefaultDeployer,
        clarity_version: DEFAULT_CLARITY_VERSION,
        epoch: Epoch::Specific(DEFAULT_EPOCH),
    };

    let _ = session.deploy_contract(&contract, false, None);

    let _ = session.deploy_contract(&contract, false, None);
    session.advance_burn_chain_tip(1);

    assert_eq!(session.interpreter.get_block_height(), 3);
    session
}

fn call_fn(
    session: &mut Session,
    func: &str,
    args: &[ClarityValue],
    advance_chain: bool,
) -> ClarityValue {
    let ExecutionResult { result, .. } = session
        .call_contract_fn(
            "contract",
            func,
            &args
                .iter()
                .map(|v: &ClarityValue| SymbolicExpression::atom_value(v.clone()))
                .collect::<Vec<SymbolicExpression>>(),
            "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM",
            false,
            false,
        )
        .unwrap();

    let v = match &result {
        EvaluationResult::Snippet(r) => r.result.clone(),
        EvaluationResult::Contract(_contract) => {
            unreachable!();
        }
    };
    if advance_chain {
        let _ = session.advance_stacks_chain_tip(1);
    }
    v
}

#[divan::bench(sample_count = 10_000)]
fn simnet_noop_read_only(bencher: Bencher) {
    let mut session = init_session();
    let initial_block_height = session.interpreter.get_block_height();
    let mut i: u32 = 0;

    bencher.bench_local(|| {
        let buff = ClarityValue::buff_from(i.to_be_bytes().to_vec()).unwrap();
        let args = [ClarityValue::UInt(black_box(i.into())), buff];
        let result = call_fn(black_box(&mut session), "noop-ro", &args, false);
        assert_eq!(
            black_box(initial_block_height),
            session.interpreter.get_block_height()
        );
        assert_eq!(result, ClarityValue::okay_true());

        i += 1;
    });
}

#[divan::bench(sample_count = 10_000)]
fn simnet_noop_public(bencher: Bencher) {
    let mut session = init_session();
    let initial_block_height = session.interpreter.get_block_height();
    let mut i: u32 = 0;

    bencher.bench_local(|| {
        let buff = ClarityValue::buff_from(i.to_be_bytes().to_vec()).unwrap();
        let args = [ClarityValue::UInt(black_box(i).into()), buff];
        let result = call_fn(black_box(&mut session), "noop-pub", &args, true);

        assert_eq!(
            black_box(initial_block_height + i + 1),
            session.interpreter.get_block_height()
        );
        assert_eq!(result, ClarityValue::okay_true());
        i += 1;
    });
}

#[divan::bench(sample_count = 10_000)]
fn simnet_save(bencher: Bencher) {
    let mut session = init_session();
    let initial_block_height = session.interpreter.get_block_height();
    let mut i: u32 = 0;

    let mut start = std::time::Instant::now();

    bencher.bench_local(|| {
        let buff = ClarityValue::buff_from(i.to_be_bytes().to_vec()).unwrap();
        let args = [ClarityValue::UInt(black_box(i).into()), buff];
        let result = call_fn(black_box(&mut session), "save", &args, true);

        assert_eq!(
            black_box(initial_block_height + i + 1),
            session.interpreter.get_block_height()
        );
        assert_eq!(result, ClarityValue::okay_true());

        if i.is_multiple_of(1000) {
            #[allow(unused_variables)]
            let elapsed = std::time::Instant::now().duration_since(start);
            // println!("{}: {}", i, elapsed.as_millis());
            start = std::time::Instant::now();
        }
        i += 1;
    });

    let contract_id =
        QualifiedContractIdentifier::parse("ST000000000000000000002AMW42H.contract").unwrap();
    let contract_data = session
        .interpreter
        .get_data_var(&contract_id, "buff-data")
        .unwrap();

    let buff = ClarityValue::buff_from((i - 1).to_be_bytes().to_vec()).unwrap();
    let expected = format!("0x{}", buff.serialize_to_hex().unwrap());
    assert_eq!(contract_data, expected);
}

// this bench performs a lot of `at-block` read, but only 2 blocks behind
// so it's expected to be fast and of constant speed
#[divan::bench(sample_count = 185)]
fn simnet_compute_fib(bencher: Bencher) {
    let mut session = init_session();
    let mut i: u32 = 0;

    bencher.bench_local(|| {
        let buff = ClarityValue::buff_from(i.to_be_bytes().to_vec()).unwrap();
        let args = [ClarityValue::UInt(black_box(i).into()), buff];

        let _ = call_fn(black_box(&mut session), "save", &args, true);

        let result = call_fn(black_box(&mut session), "fib", &[], false);
        assert_eq!(result, ClarityValue::okay_true());

        i += 1;
    });
}

// perform reads at the beginning of the chain
// increasing the time it takes to read the data as the chain grows
#[divan::bench(sample_count = 10_000)]
fn simnet_save_read_at_block(bencher: Bencher) {
    let mut session = init_session();
    let initial_block_height: u32 = session.interpreter.get_block_height();
    let mut i: u32 = 0;

    let mut start = std::time::Instant::now();

    bencher.bench_local(|| {
        let buff = ClarityValue::buff_from(i.to_be_bytes().to_vec()).unwrap();
        let args = [ClarityValue::UInt(black_box(i).into()), buff];

        let _ = call_fn(black_box(&mut session), "save", &args, true);

        let result = call_fn(
            black_box(&mut session),
            "read-ab",
            &[ClarityValue::UInt((initial_block_height - 1).into())],
            false,
        );
        assert_eq!(result, ClarityValue::buff_from_byte(0x01));

        assert_eq!(
            black_box(initial_block_height + i + 1),
            session.interpreter.get_block_height()
        );

        if i.is_multiple_of(1000) {
            #[allow(unused_variables)]
            let elapsed = std::time::Instant::now().duration_since(start);
            // println!("{}: {}", i, elapsed.as_millis());
            start = std::time::Instant::now();
        }
        i += 1;
    });
}

fn main() {
    // simnet_benchmark();
    divan::main();
}
