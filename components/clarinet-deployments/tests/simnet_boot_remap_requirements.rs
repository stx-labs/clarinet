//! Requirements and project contracts must share simnet's testnet boot state.
//! Cache fixtures exercise requirement loading without network access.
//!
//! A fix must rewrite requirements before building their ASTs, only for
//! `Environment::Simnet` with remote data disabled, and backfill legacy plans.

use std::fs;
use std::path::Path;

use clarinet_deployments::types::{DeploymentSpecification, TransactionSpecification};
use clarinet_deployments::{
    generate_default_deployment, initiate_session_from_manifest, setup_session_with_deployment,
    update_session_with_deployment_plan,
};
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity::util::hash::to_hex;
use clarity::util::secp256k1::{Secp256k1PrivateKey, Secp256k1PublicKey};
use clarity::vm::types::PrincipalData;
use clarity::vm::{EvaluationResult, Value};
use clarity_repl::repl::boot::{BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS};
use clarity_repl::repl::post_conditions::PostConditionCheck;
use clarity_repl::repl::session::CallKind;
use clarity_repl::repl::Session;
use clarity_repl::utils::Environment;
use indoc::formatdoc;
use stacks_codec::transaction::{
    AssetInfo, FungibleConditionCode, PostConditionPrincipal, TransactionPostCondition,
    TransactionPostConditionMode,
};
use stacks_common::types::PrivateKey;
use tempfile::TempDir;

const TEST_MNEMONIC: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";

const DEPLOYER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

const REQUIREMENT_DEPLOYER: &str = "SP2X0TZ59D5SZ8ACQ6YMCHHNR2ZN51Z32E2CJ173";

const REQUIREMENT_STACKER: &str = r#"
(define-public (stack (amount uint))
    (as-contract
        (contract-call? 'SP000000000000000000002Q6VF78.pox-3 stack-stx
            amount
            { version: 0x00, hashbytes: 0x0101010101010101010101010101010101010101 }
            burn-block-height
            u1)))
"#;

const REQUIREMENT_WRITER: &str = r#"
(define-public (submit)
    (contract-call? 'SP000000000000000000002Q6VF78.cost-voting submit-proposal
        tx-sender "foo" tx-sender "bar"))
(define-public (vote)
    (contract-call? 'SP000000000000000000002Q6VF78.cost-voting vote-proposal u0 u100))
(define-public (withdraw)
    (contract-call? 'SP000000000000000000002Q6VF78.cost-voting withdraw-votes u0 u100))
(define-read-only (votes)
    (contract-call? 'SP000000000000000000002Q6VF78.cost-voting get-principal-votes tx-sender u0))
"#;

const PROJECT_READER: &str = r#"
(define-read-only (read-proposal)
    (contract-call? 'SP000000000000000000002Q6VF78.cost-voting get-proposal u0))
"#;

fn write_project(root: &Path, project: &[(&str, &str)], requirements: &[(&str, &str)]) {
    fs::create_dir_all(root.join("settings")).unwrap();
    fs::create_dir_all(root.join("contracts")).unwrap();
    fs::create_dir_all(root.join(".cache/requirements")).unwrap();

    let requirement_ids = requirements
        .iter()
        .map(|(name, _)| format!("{{ contract_id = \"{REQUIREMENT_DEPLOYER}.{name}\" }}"))
        .collect::<Vec<_>>()
        .join(", ");

    let project_entries = project
        .iter()
        .map(|(name, _)| {
            formatdoc!(
                r#"
                [contracts.{name}]
                path = "contracts/{name}.clar"
                clarity_version = 2
                epoch = 2.4
                "#
            )
        })
        .collect::<String>();

    #[rustfmt::skip]
    let manifest = formatdoc!(r#"
        [project]
        name = "boot-remap-requirements-test"
        authors = []
        description = ""
        telemetry = false
        cache_dir = "./.cache"
        requirements = [{requirement_ids}]

        {project_entries}
    "#);

    #[rustfmt::skip]
    let devnet_settings = formatdoc!(r#"
        [network]
        name = "devnet"
        deployment_fee_rate = 10

        [accounts.deployer]
        mnemonic = "{TEST_MNEMONIC}"
        balance = 100_000_000_000_000
    "#);

    fs::write(root.join("Clarinet.toml"), manifest).unwrap();
    fs::write(root.join("settings/Devnet.toml"), devnet_settings).unwrap();

    for (name, source) in project {
        fs::write(root.join(format!("contracts/{name}.clar")), source).unwrap();
    }

    for (name, source) in requirements {
        let stem = format!(".cache/requirements/{REQUIREMENT_DEPLOYER}.{name}");
        fs::write(root.join(format!("{stem}.clar")), source).unwrap();
        fs::write(
            root.join(format!("{stem}.json")),
            r#"{"epoch":"Epoch24","clarity_version":"Clarity2"}"#,
        )
        .unwrap();
    }
}

struct Project {
    _temp_dir: TempDir,
    manifest: ProjectManifest,
}

impl Project {
    fn new(project: &[(&str, &str)], requirements: &[(&str, &str)]) -> Self {
        let temp_dir = TempDir::new().unwrap();
        write_project(temp_dir.path(), project, requirements);

        let manifest =
            ProjectManifest::from_location(&temp_dir.path().join("Clarinet.toml"), true).unwrap();

        Self {
            _temp_dir: temp_dir,
            manifest,
        }
    }

    async fn generate(&self) -> DeploymentSpecification {
        let (deployment, artifacts, _) = generate_default_deployment(
            &self.manifest,
            &StacksNetwork::Simnet,
            false,
            None,
            None,
            Environment::Simnet,
        )
        .await
        .expect("simnet deployment plan should be generated");
        assert!(artifacts.success, "{:?}", artifacts.diags);
        deployment
    }

    async fn deployed_session(&self) -> Session {
        let deployment = self.generate().await;
        let mut session = initiate_session_from_manifest(&self.manifest);
        let results = update_session_with_deployment_plan(&mut session, &deployment, None);
        for (id, result) in results {
            assert!(result.is_ok(), "{id}: {result:?}");
        }
        session.set_tx_sender(DEPLOYER);
        session
    }
}

#[track_caller]
fn publish(
    deployment: &DeploymentSpecification,
    contract_name: &str,
) -> (String, Vec<(String, String)>) {
    let spec = deployment
        .plan
        .batches
        .iter()
        .flat_map(|batch| &batch.transactions)
        .find_map(|tx| match tx {
            TransactionSpecification::EmulatedContractPublish(spec)
                if spec.contract_name.as_str() == contract_name =>
            {
                Some(spec)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("no emulated-contract-publish for {contract_name}"));

    let remap = spec
        .remap_principals
        .iter()
        .map(|(from, to)| (from.to_address(), to.to_address()))
        .collect();

    (spec.source.clone(), remap)
}

#[track_caller]
fn snippet_value(result: EvaluationResult) -> Value {
    match result {
        EvaluationResult::Snippet(snippet) => snippet.result,
        EvaluationResult::Contract(_) => panic!("expected a snippet result"),
    }
}

#[track_caller]
fn eval(session: &mut Session, snippet: &str) -> Value {
    let result = session
        .eval(snippet.to_string(), false)
        .unwrap_or_else(|diags| panic!("{snippet} should evaluate, got {diags:?}"))
        .into_inner();
    snippet_value(result.result)
}

#[track_caller]
fn locked_amount(session: &mut Session, principal: &str) -> u128 {
    eval(session, &format!("(get locked (stx-account '{principal}))"))
        .expect_u128()
        .expect("locked should be a uint")
}

#[tokio::test]
async fn a_requirement_calling_the_mainnet_pox_address_locks_stx() {
    let project = Project::new(&[], &[("stacker", REQUIREMENT_STACKER)]);
    let mut session = project.deployed_session().await;

    assert_eq!(
        stack_in_session(&mut session),
        90_000_000_000,
        "a requirement naming the mainnet PoX address must lock STX, exactly as a \
         manifest contract does"
    );
}

#[tokio::test]
async fn a_requirement_and_a_project_contract_share_boot_state() {
    let project = Project::new(
        &[("reader", PROJECT_READER)],
        &[("writer", REQUIREMENT_WRITER)],
    );
    let mut session = project.deployed_session().await;

    let submitted = eval(
        &mut session,
        &format!("(contract-call? '{REQUIREMENT_DEPLOYER}.writer submit)"),
    );
    assert_eq!(submitted, Value::okay(Value::UInt(0)).unwrap());

    let via_project = eval(
        &mut session,
        &format!("(contract-call? '{DEPLOYER}.reader read-proposal)"),
    );

    // Bypass user-snippet remapping to identify which copy holds the write.
    let on_mainnet_twin = eval(
        &mut session,
        &format!("(contract-call? '{BOOT_MAINNET_ADDRESS}.cost-voting get-proposal u0)"),
    );
    let on_testnet_twin = eval(
        &mut session,
        &format!("(contract-call? '{BOOT_TESTNET_ADDRESS}.cost-voting get-proposal u0)"),
    );

    assert!(
        matches!(&via_project, Value::Optional(option) if option.data.is_some()),
        "a project contract must observe the boot state a requirement wrote through the \
         same spelling, got {via_project}\n  \
         {BOOT_MAINNET_ADDRESS}.cost-voting holds: {on_mainnet_twin}\n  \
         {BOOT_TESTNET_ADDRESS}.cost-voting holds: {on_testnet_twin}"
    );
    assert_eq!(via_project, on_testnet_twin);
    let proposal = via_project
        .expect_optional()
        .unwrap()
        .unwrap()
        .expect_tuple()
        .unwrap();
    for (field, expected) in [
        (
            "function-contract",
            Value::Principal(PrincipalData::parse(DEPLOYER).unwrap()),
        ),
        (
            "cost-function-contract",
            Value::Principal(PrincipalData::parse(DEPLOYER).unwrap()),
        ),
        (
            "function-name",
            Value::string_ascii_from_bytes(b"foo".to_vec()).unwrap(),
        ),
        (
            "cost-function-name",
            Value::string_ascii_from_bytes(b"bar".to_vec()).unwrap(),
        ),
    ] {
        assert_eq!(proposal.get(field).unwrap(), &expected, "{field}");
    }
}

#[track_caller]
fn stack_in_session(session: &mut Session) -> u128 {
    let stacker = format!("{REQUIREMENT_DEPLOYER}.stacker");
    let stacked = 90_000_000_000_u128;

    session.set_tx_sender(DEPLOYER);
    session
        .stx_transfer(100_000_000_000, &stacker, PostConditionCheck::Unchecked)
        .expect("funding the stacker requirement should succeed");

    let value = eval(
        session,
        &format!("(contract-call? '{stacker} stack u{stacked})"),
    );
    assert!(
        matches!(&value, Value::Response(response) if response.committed),
        "stack should succeed, got {value}"
    );

    locked_amount(session, &stacker)
}

#[tokio::test]
async fn a_legacy_plan_still_rewrites_a_requirement() {
    let project = Project::new(&[], &[("stacker", REQUIREMENT_STACKER)]);
    let mut deployment = project.generate().await;

    // Legacy plans reload original source and have no remap marker.
    for batch in deployment.plan.batches.iter_mut() {
        for tx in batch.transactions.iter_mut() {
            if let TransactionSpecification::EmulatedContractPublish(spec) = tx {
                spec.remap_principals.clear();
                spec.source = REQUIREMENT_STACKER.to_string();
            }
        }
    }

    let artifacts = setup_session_with_deployment(
        &project.manifest,
        &mut deployment,
        None,
        false,
        Environment::Simnet,
    );
    assert!(artifacts.success, "the stale plan should still deploy");

    let mut session = artifacts.session;
    assert_eq!(
        stack_in_session(&mut session),
        90_000_000_000,
        "a plan predating the field must lock STX for a requirement too, or loading a \
         plan from disk and generating one afresh describe different code"
    );
}

#[tokio::test]
async fn onchain_analysis_preserves_requirement_source() {
    let project = Project::new(&[], &[("writer", REQUIREMENT_WRITER)]);
    let (mut deployment, generated, _) = generate_default_deployment(
        &project.manifest,
        &StacksNetwork::Simnet,
        false,
        None,
        None,
        Environment::OnChain,
    )
    .await
    .unwrap();
    assert!(generated.success, "{:?}", generated.diags);
    let (source, remap) = publish(&deployment, "writer");
    assert_eq!(source, REQUIREMENT_WRITER);
    assert!(remap.is_empty());
    let deployed = setup_session_with_deployment(
        &project.manifest,
        &mut deployment,
        Some(&generated.asts),
        false,
        Environment::OnChain,
    );
    assert!(deployed.success, "{:?}", deployed.diags);
}

#[tokio::test]
async fn generated_dependencies_and_project_asts_share_requirement_boot_state() {
    let project = Project::new(
        &[("reader", PROJECT_READER)],
        &[("writer", REQUIREMENT_WRITER)],
    );
    let (mut deployment, generated, _) = generate_default_deployment(
        &project.manifest,
        &StacksNetwork::Simnet,
        false,
        None,
        None,
        Environment::Simnet,
    )
    .await
    .unwrap();
    assert!(generated.success, "{:?}", generated.diags);
    let writer_id = clarity::vm::types::QualifiedContractIdentifier::parse(&format!(
        "{REQUIREMENT_DEPLOYER}.writer"
    ))
    .unwrap();
    // Requirement ASTs feed dependency analysis; only project ASTs are returned.
    let deps = &generated.deps[&writer_id];
    assert!(deps
        .iter()
        .any(|d| d.contract_id.to_string() == format!("{BOOT_TESTNET_ADDRESS}.cost-voting")));
    assert!(!deps
        .iter()
        .any(|d| d.contract_id.to_string() == format!("{BOOT_MAINNET_ADDRESS}.cost-voting")));
    let reader_id =
        clarity::vm::types::QualifiedContractIdentifier::parse(&format!("{DEPLOYER}.reader"))
            .unwrap();
    assert!(generated.asts.contains_key(&reader_id));
    let mut deployed = setup_session_with_deployment(
        &project.manifest,
        &mut deployment,
        Some(&generated.asts),
        false,
        Environment::Simnet,
    );
    assert!(deployed.success, "{:?}", deployed.diags);
    let session = &mut deployed.session;
    session.set_tx_sender(DEPLOYER);
    assert_eq!(
        eval(session, &format!("(contract-call? '{writer_id} submit)")),
        Value::okay(Value::UInt(0)).unwrap()
    );
    let value = eval(
        session,
        &format!("(contract-call? '{reader_id} read-proposal)"),
    );
    assert!(
        value.clone().expect_optional().unwrap().is_some(),
        "{value}"
    );
    assert_eq!(
        value,
        eval(
            session,
            &format!("(contract-call? '{BOOT_TESTNET_ADDRESS}.cost-voting get-proposal u0)")
        )
    );
}

#[tokio::test]
async fn requirement_remap_is_recorded_and_survives_plan_round_trip() {
    let project = Project::new(
        &[("reader", PROJECT_READER)],
        &[("stacker", REQUIREMENT_STACKER)],
    );
    let generated = project.generate().await;
    let expected_remap = vec![(BOOT_MAINNET_ADDRESS.into(), BOOT_TESTNET_ADDRESS.into())];
    for (name, original) in [("reader", PROJECT_READER), ("stacker", REQUIREMENT_STACKER)] {
        let (source, remap) = publish(&generated, name);
        assert_eq!(
            source,
            original.replace(BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS),
            "{name}"
        );
        assert_eq!(remap, expected_remap, "{name}");
    }
    let root = &project.manifest.root_dir;
    let path = root.join("plan.yaml");
    fs::write(&path, generated.to_file_content(root).unwrap()).unwrap();
    let reloaded = clarinet_deployments::load_deployment(root, &path).unwrap();
    let (source, remap) = publish(&reloaded, "stacker");
    // Reload reads the unchanged cache file; only the serialized marker survives.
    assert_eq!(source, REQUIREMENT_STACKER);
    assert_eq!(remap, expected_remap);
    let mut session = initiate_session_from_manifest(&project.manifest);
    let results = update_session_with_deployment_plan(&mut session, &reloaded, None);
    for (id, result) in results {
        assert!(result.is_ok(), "{id}: {result:?}");
    }
    assert_eq!(stack_in_session(&mut session), 90_000_000_000);
}

fn withdrawal_conditions(ft_amount: u64) -> PostConditionCheck {
    let boot = PrincipalData::parse_standard_principal(BOOT_MAINNET_ADDRESS).unwrap();
    let sender = PrincipalData::parse_standard_principal(DEPLOYER).unwrap();
    PostConditionCheck::Checked {
        conditions: vec![
            TransactionPostCondition::STX(
                PostConditionPrincipal::Contract(
                    boot.clone().into(),
                    "cost-voting".try_into().unwrap(),
                ),
                FungibleConditionCode::SentEq,
                100,
            ),
            TransactionPostCondition::Fungible(
                PostConditionPrincipal::Standard(sender.clone().into()),
                AssetInfo {
                    contract_address: boot.into(),
                    contract_name: "cost-voting".try_into().unwrap(),
                    asset_name: "cost-vote-token".try_into().unwrap(),
                },
                FungibleConditionCode::SentEq,
                ft_amount,
            ),
        ],
        mode: TransactionPostConditionMode::Deny,
        origin: sender.into(),
    }
}

async fn check_requirement_withdrawal(ft_amount: u64, should_commit: bool) {
    let project = Project::new(&[], &[("writer", REQUIREMENT_WRITER)]);
    let mut session = project.deployed_session().await;
    let writer = format!("{REQUIREMENT_DEPLOYER}.writer");
    assert_eq!(
        eval(&mut session, &format!("(contract-call? '{writer} submit)")),
        Value::okay(Value::UInt(0)).unwrap()
    );
    assert_eq!(
        eval(&mut session, &format!("(contract-call? '{writer} vote)")),
        Value::okay(Value::Bool(true)).unwrap()
    );
    let balance_before = eval(&mut session, &format!("(stx-get-balance '{DEPLOYER})"))
        .expect_u128()
        .unwrap();
    let result = session.call_contract_fn(
        &writer,
        "withdraw",
        &[],
        DEPLOYER,
        false,
        false,
        CallKind::Transaction,
        withdrawal_conditions(ft_amount),
    );
    if should_commit {
        assert_eq!(
            snippet_value(result.unwrap().result),
            Value::okay(Value::Bool(true)).unwrap()
        );
    } else {
        let failure = result.expect_err("an incorrect asset amount must abort");
        assert!(
            failure
                .diagnostics
                .iter()
                .any(|d| d.message.contains("Post-condition")),
            "{:?}",
            failure.diagnostics
        );
    }
    assert_eq!(
        eval(&mut session, &format!("(contract-call? '{writer} votes)")),
        Value::some(Value::UInt(if should_commit { 0 } else { 100 })).unwrap()
    );
    let balance_after = eval(&mut session, &format!("(stx-get-balance '{DEPLOYER})"))
        .expect_u128()
        .unwrap();
    assert_eq!(
        balance_after,
        balance_before + if should_commit { 100 } else { 0 }
    );
}

#[tokio::test]
async fn matching_boot_asset_post_conditions_commit_through_a_requirement() {
    check_requirement_withdrawal(100, true).await;
}

#[tokio::test]
async fn incorrect_boot_asset_post_conditions_roll_back_a_requirement() {
    check_requirement_withdrawal(99, false).await;
}

const REQUIREMENT_STAKER_V5: &str = r#"
(use-trait signer-mgr 'SP000000000000000000002Q6VF78.pox-5.signer-manager-trait)
(define-public (stake (signer <signer-mgr>) (amount uint) (allowance uint))
    (restrict-assets? tx-sender ((with-stacking allowance))
        (try! (contract-call? 'SP000000000000000000002Q6VF78.pox-5 stake
            signer amount u1 burn-block-height none))))
"#;

// Grant and registration must originate from the manager, not the test wallet.
const REQUIREMENT_SIGNER: &str = r#"
(use-trait signer-mgr 'SP000000000000000000002Q6VF78.pox-5.signer-manager-trait)
(impl-trait 'SP000000000000000000002Q6VF78.pox-5.signer-manager-trait)
(define-public (validate-stake! (staker principal) (first-index uint) (num-indexes uint)
    (amount-ustx uint) (amount-sats uint) (is-bond bool) (signer-calldata (optional (buff 500))))
    (ok true))
(define-public (grant (key (buff 33)) (signature (buff 65)))
    (contract-call? 'SP000000000000000000002Q6VF78.pox-5 grant-signer-key
        key 'SP2X0TZ59D5SZ8ACQ6YMCHHNR2ZN51Z32E2CJ173.signer u1 signature))
(define-public (register (self <signer-mgr>) (key (buff 33)))
    (contract-call? 'SP000000000000000000002Q6VF78.pox-5 register-signer self key))
(define-read-only (grant-hash)
    (contract-call? 'SP000000000000000000002Q6VF78.pox-5 get-signer-grant-message-hash
        'SP2X0TZ59D5SZ8ACQ6YMCHHNR2ZN51Z32E2CJ173.signer u1))
"#;

async fn pox5_requirement_session() -> Session {
    let project = Project::new(
        &[],
        &[
            ("signer", REQUIREMENT_SIGNER),
            ("staker", REQUIREMENT_STAKER_V5),
        ],
    );
    for name in ["signer", "staker"] {
        fs::write(
            project.manifest.root_dir.join(format!(
                ".cache/requirements/{REQUIREMENT_DEPLOYER}.{name}.json"
            )),
            r#"{"epoch":"Epoch40","clarity_version":"Clarity4"}"#,
        )
        .unwrap();
    }
    let mut session = project.deployed_session().await;
    let signer = format!("{REQUIREMENT_DEPLOYER}.signer");
    // Ask the requirement for the hash so setup uses the same PoX copy as stake.
    let hash = eval(
        &mut session,
        &format!("(contract-call? '{signer} grant-hash)"),
    )
    .expect_buff(32)
    .unwrap();
    let key = Secp256k1PrivateKey::from_hex(
        "7287ba251d44a4d3fd9276c88ce34c5c52a038955511cccaf77e61068649c17801",
    )
    .unwrap();
    let public = to_hex(&Secp256k1PublicKey::from_private(&key).to_bytes_compressed());
    let signature = to_hex(&key.sign(&hash).unwrap().to_rsv());
    for call in [
        format!("(contract-call? '{signer} grant 0x{public} 0x{signature})"),
        format!("(contract-call? '{signer} register '{signer} 0x{public})"),
    ] {
        let result = eval(&mut session, &call);
        assert!(
            matches!(&result, Value::Response(r) if r.committed),
            "{call}: {result}"
        );
    }
    session
}

#[tokio::test]
async fn insufficient_clarity_stacking_allowance_aborts_through_a_requirement() {
    let mut session = pox5_requirement_session().await;
    let value = eval(&mut session, &format!("(contract-call? '{REQUIREMENT_DEPLOYER}.staker stake '{REQUIREMENT_DEPLOYER}.signer u50000000000 u1)"));
    assert_eq!(value, Value::error(Value::UInt(0)).unwrap());
    assert_eq!(locked_amount(&mut session, DEPLOYER), 0);
    // A sufficient retry must succeed: the rejected stake must leave no state behind.
    let value = eval(&mut session, &format!("(contract-call? '{REQUIREMENT_DEPLOYER}.staker stake '{REQUIREMENT_DEPLOYER}.signer u50000000000 u50000000000)"));
    assert!(
        matches!(&value, Value::Response(r) if r.committed),
        "{value}"
    );
    assert_eq!(locked_amount(&mut session, DEPLOYER), 50_000_000_000);
}
