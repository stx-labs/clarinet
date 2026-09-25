//! POC driver: deploy the clarity-wasm fixture into a Simnet-like session,
//! run a scripted call list and print JSON to stdout.
//!
//! cargo run -p clarity-repl --features clarity-wasm --example wasm_poc
//! CLARINET_CLARITY_WASM=1 cargo run -p clarity-repl --features clarity-wasm --example wasm_poc

use clarity::types::StacksEpochId;
use clarity::vm::database::clarity_db::ContractDataVarName;
use clarity::vm::database::{ClarityBackingStore, ClarityDatabase, StoreType};
use clarity::vm::{ClarityVersion, EvaluationResult, ExecutionResult, SymbolicExpression};
use clarity_repl::repl::post_conditions::PostConditionCheck;
use clarity_repl::repl::session::CallKind;
use clarity_repl::repl::settings::Account;
use clarity_repl::repl::{
    ClarityCodeSource, ClarityContract, ContractDeployer, Epoch, Session, SessionSettings,
};
use clarity_repl::utils::serialize_event;
use clarity_types::types::QualifiedContractIdentifier;
use clarity_types::Value;
use serde_json::{json, Value as Json};

const DEPLOYER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";
const WALLET_1: &str = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";
const EPOCH: StacksEpochId = StacksEpochId::Epoch31;
const CLARITY_VERSION: ClarityVersion = ClarityVersion::Clarity3;

fn result_json(result: Result<ExecutionResult, String>) -> Json {
    match result {
        Ok(r) => {
            let value = match &r.result {
                EvaluationResult::Snippet(s) => s.result.to_string(),
                EvaluationResult::Contract(c) => format!("{:?}", c.result),
            };
            json!({
                "result": value,
                "events": r.events.iter().map(serialize_event).collect::<Vec<_>>(),
                "cost": r.cost.map(|c| serde_json::to_value(c).unwrap()),
            })
        }
        Err(e) => json!({ "error": e }),
    }
}

fn diags(d: &[clarity::vm::diagnostic::Diagnostic]) -> String {
    d.iter()
        .map(|d| d.message.to_string())
        .collect::<Vec<_>>()
        .join(" | ")
}

fn has_wasm_module(session: &mut Session, id: &QualifiedContractIdentifier) -> bool {
    let key = ClarityDatabase::make_metadata_key(
        StoreType::Contract,
        ContractDataVarName::Contract.as_str(),
    );
    let raw = session
        .interpreter
        .clarity_datastore
        .get_metadata(id, &key)
        .unwrap()
        .unwrap_or_else(|| panic!("{id} not deployed"));
    let contract: Json = serde_json::from_str(&raw).unwrap();
    contract["contract_context"]
        .get("wasm_module")
        .is_some_and(|m| !m.is_null())
}

fn main() {
    let wasm_mode = std::env::var("CLARINET_CLARITY_WASM").is_ok_and(|v| v == "1");
    let fixture = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../poc/clarity-wasm-fixture/contracts"
    );

    let accounts =
        [("deployer", DEPLOYER), ("wallet_1", WALLET_1)].map(|(name, address)| Account {
            address: address.into(),
            balance: 100_000_000_000_000,
            name: name.into(),
        });
    let settings = SessionSettings {
        include_costs: true,
        initial_deployer: Some(accounts[0].clone()),
        initial_accounts: accounts.to_vec(),
        ..Default::default()
    };
    let mut session = Session::new(settings);
    session.update_epoch(EPOCH);
    session.advance_burn_chain_tip(1);

    let mut deploys = vec![];
    for name in ["counter", "caller"] {
        let src = std::fs::read_to_string(format!("{fixture}/{name}.clar")).unwrap();
        let contract = ClarityContract {
            code_source: ClarityCodeSource::ContractInMemory(src),
            name: name.into(),
            deployer: ContractDeployer::Address(DEPLOYER.into()),
            clarity_version: CLARITY_VERSION,
            epoch: Epoch::Specific(EPOCH),
            skip_analysis: false,
        };
        let res = session
            .deploy_contract(&contract, true, None, PostConditionCheck::Unchecked)
            .map(|r| r.execution_result)
            .map_err(|d| diags(&d));
        deploys.push(json!({ "contract": name, "deploy": result_json(res) }));
    }

    let u = |n: u128| SymbolicExpression::atom_value(Value::UInt(n));
    let counter = format!("{DEPLOYER}.counter");
    let caller = format!("{DEPLOYER}.caller");
    #[rustfmt::skip]
    let script: Vec<(&str, &str, &str, Vec<SymbolicExpression>, &str)> = vec![
        ("public ok",                   &counter, "increment",      vec![],           DEPLOYER),
        ("public ok with arg",          &counter, "add",            vec![u(3)],       DEPLOYER),
        ("public err",                  &counter, "add",            vec![u(11)],      DEPLOYER),
        ("read-only getter",            &counter, "get-count",      vec![],           DEPLOYER),
        ("read-only map getter",        &counter, "get-total",      vec![SymbolicExpression::atom_value(Value::Principal(clarity_types::types::PrincipalData::parse(DEPLOYER).unwrap()))], DEPLOYER),
        ("cross-contract public",       &caller,  "bump-counter",   vec![],           WALLET_1),
        ("cross-contract public err",   &caller,  "add-to-counter", vec![u(50)],      WALLET_1),
        ("cross-contract public ok",    &caller,  "add-to-counter", vec![u(4)],       WALLET_1),
        ("cross-contract read-only",    &caller,  "read-counter",   vec![],           WALLET_1),
        ("pox-4 get-pox-info",          &caller,  "pox-info",       vec![],           WALLET_1),
        ("pox-4 read-only with arg",    &caller,  "reward-cycle-of", vec![u(5000)],   WALLET_1),
        ("runtime err unwrap-panic",    &caller,  "unwrap-none",    vec![],           WALLET_1),
        ("runtime err unwrap-panic ro", &counter, "panic-none",     vec![],           WALLET_1),
        ("runtime err div by zero",     &caller,  "divide-by-zero", vec![u(7)],       WALLET_1),
        ("runtime err div by zero ro",  &counter, "divide",         vec![u(7), u(0)], WALLET_1),
        ("read-only divide ok",         &counter, "divide",         vec![u(7), u(2)], WALLET_1),
        ("mutate 1",                    &counter, "increment",      vec![],           WALLET_1),
        ("mutate 2",                    &caller,  "bump-counter",   vec![],           DEPLOYER),
        ("mutate 3",                    &counter, "add",            vec![u(10)],      WALLET_1),
        ("read back count",             &counter, "get-count",      vec![],           DEPLOYER),
        ("read back total deployer",    &counter, "get-total",      vec![SymbolicExpression::atom_value(Value::Principal(clarity_types::types::PrincipalData::parse(DEPLOYER).unwrap()))], DEPLOYER),
        ("read back total wallet_1",    &counter, "get-total",      vec![SymbolicExpression::atom_value(Value::Principal(clarity_types::types::PrincipalData::parse(WALLET_1).unwrap()))], DEPLOYER),
    ];

    let mut calls = vec![];
    for (label, contract, method, args, sender) in script {
        let read_only = method.starts_with("get-")
            || [
                "read-counter",
                "pox-info",
                "reward-cycle-of",
                "divide",
                "panic-none",
            ]
            .contains(&method);
        let kind = if read_only {
            CallKind::NonceFree
        } else {
            CallKind::Transaction
        };
        let res = session
            .call_contract_fn(
                contract,
                method,
                &args,
                sender,
                false,
                true,
                kind,
                PostConditionCheck::Unchecked,
            )
            .map_err(|e| diags(&e.diagnostics));
        calls.push(json!({
            "label": label,
            "call": format!("{contract}::{method}"),
            "sender": sender,
            "outcome": result_json(res),
            "trace": session.last_contract_call_trace.clone(),
        }));
    }

    let snippets = [
        "(contract-call? .counter get-count)",
        "(contract-call? .caller bump-counter)",
        "(print (contract-call? .counter get-total tx-sender))",
    ];
    let mut evals = vec![];
    for snippet in snippets {
        let res = session
            .eval(snippet.to_string(), true)
            .map(|r| r.execution_result)
            .map_err(|d| diags(&d));
        evals.push(json!({ "snippet": snippet, "outcome": result_json(res) }));
    }

    let mut modules = serde_json::Map::new();
    for name in ["counter", "caller"] {
        let id = QualifiedContractIdentifier::parse(&format!("{DEPLOYER}.{name}")).unwrap();
        let present = has_wasm_module(&mut session, &id);
        assert_eq!(present, wasm_mode, "{id}: wasm_module present = {present}");
        modules.insert(name.into(), json!(present));
    }
    let pox4 = QualifiedContractIdentifier::parse("ST000000000000000000002AMW42H.pox-4").unwrap();
    let pox4_wasm = has_wasm_module(&mut session, &pox4);
    assert!(!pox4_wasm, "boot contracts must stay interpreted");
    modules.insert("pox-4".into(), json!(pox4_wasm));

    let out = json!({
        "wasm_module": modules,
        "deploys": deploys,
        "calls": calls,
        "snippets": evals,
    });
    println!("{}", serde_json::to_string_pretty(&out).unwrap());
}
