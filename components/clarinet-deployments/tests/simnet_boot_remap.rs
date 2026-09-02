//! Simnet rewrites mainnet boot-contract principals to their testnet twins at
//! deployment time — see <https://github.com/stx-labs/clarinet/issues/2491>.
//!
//! Simnet deploys the boot contracts under both addresses, but its chain state
//! is testnet-flavored: stacks-core's PoX handler keys off
//! `GlobalContext::mainnet`, so `stack-stx` through `SP000....pox-N` never
//! locked any STX. The rewrite happens while the plan is generated, is
//! recorded on the `emulated-contract-publish` transaction, and is re-applied
//! when a plan is deployed straight off disk.

use std::fs;
use std::path::Path;

use clarinet_deployments::types::{DeploymentSpecification, TransactionSpecification};
use clarinet_deployments::{
    generate_default_deployment, initiate_session_from_manifest, setup_session_with_deployment,
    update_session_with_deployment_plan,
};
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarity::vm::{EvaluationResult, Value};
use clarity_repl::repl::boot::{BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS, SBTC_MAINNET_ADDRESS};
use clarity_repl::repl::Session;
use clarity_repl::utils::Environment;
use indoc::formatdoc;
use tempfile::TempDir;

/// Well-known Clarinet test mnemonic, matching the generated settings files.
const TEST_MNEMONIC: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";

/// `deployer`, derived from `TEST_MNEMONIC`.
const DEPLOYER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

/// A contract that stacks its own balance through the *mainnet* pox-3 address,
/// which is what a developer writing for mainnet would naturally type.
///
/// `as-contract` makes the contract both `tx-sender` and `contract-caller`, so
/// pox-3 locks the contract's own STX.
const STACKER_SOURCE: &str = r#"
(define-public (stack (amount uint))
    (as-contract
        (contract-call? 'SP000000000000000000002Q6VF78.pox-3 stack-stx
            amount
            { version: 0x00, hashbytes: 0x0101010101010101010101010101010101010101 }
            burn-block-height
            u1)))
"#;

/// Write a minimal simnet project with a single contract.
fn write_project(root: &Path, contract_name: &str, source: &str, extra_manifest: &str) {
    fs::create_dir_all(root.join("settings")).unwrap();
    fs::create_dir_all(root.join("contracts")).unwrap();

    #[rustfmt::skip]
    let manifest = formatdoc!(r#"
        [project]
        name = "boot-remap-test"
        authors = []
        description = ""
        telemetry = false

        [contracts.{contract_name}]
        path = "contracts/{contract_name}.clar"
        clarity_version = 2
        epoch = 2.4
        {extra_manifest}
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
    fs::write(root.join(format!("contracts/{contract_name}.clar")), source).unwrap();
}

struct Project {
    _temp_dir: TempDir,
    manifest: ProjectManifest,
}

impl Project {
    fn new(contract_name: &str, source: &str, extra_manifest: &str) -> Self {
        let temp_dir = TempDir::new().unwrap();
        write_project(temp_dir.path(), contract_name, source, extra_manifest);

        // `true` keeps any `[repl.remote_data]` block in the manifest live;
        // `false` would silently disable MXS and make the MXS tests vacuous.
        let manifest =
            ProjectManifest::from_location(&temp_dir.path().join("Clarinet.toml"), true).unwrap();

        Self {
            _temp_dir: temp_dir,
            manifest,
        }
    }

    fn root(&self) -> &Path {
        &self.manifest.root_dir
    }

    async fn generate(&self) -> DeploymentSpecification {
        let (deployment, _artifacts, _) = generate_default_deployment(
            &self.manifest,
            &StacksNetwork::Simnet,
            false,
            None,
            None,
            Environment::Simnet,
        )
        .await
        .expect("simnet deployment plan should be generated");
        deployment
    }
}

/// The single `emulated-contract-publish` in `deployment`, as
/// `(source, recorded remap pairs)`.
fn publish(deployment: &DeploymentSpecification) -> (String, Vec<(String, String)>) {
    let spec = deployment
        .plan
        .batches
        .iter()
        .flat_map(|batch| &batch.transactions)
        .find_map(|tx| match tx {
            TransactionSpecification::EmulatedContractPublish(spec) => Some(spec),
            _ => None,
        })
        .expect("simnet plan should contain an emulated-contract-publish transaction");

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
fn locked_amount(session: &mut Session, principal: &str) -> u128 {
    let result = session
        .eval(format!("(get locked (stx-account '{principal}))"), false)
        .expect("stx-account should evaluate")
        .into_inner();
    snippet_value(result.result)
        .expect_u128()
        .expect("locked should be a uint")
}

/// Deploy `deployment` and have the stacker contract lock 90k STX.
#[track_caller]
fn stack_through_the_deployed_contract(
    manifest: &ProjectManifest,
    deployment: &DeploymentSpecification,
) -> u128 {
    let mut session = initiate_session_from_manifest(manifest);
    update_session_with_deployment_plan(&mut session, deployment, None);
    stack_in_session(&mut session)
}

/// Fund the already-deployed stacker contract and have it stack.
#[track_caller]
fn stack_in_session(session: &mut Session) -> u128 {
    let stacker = format!("{DEPLOYER}.stacker");
    let stacked = 90_000_000_000_u128;

    // The contract stacks its own balance, so it needs one first.
    session.set_tx_sender(DEPLOYER);
    session
        .stx_transfer(100_000_000_000, &stacker)
        .expect("funding the stacker contract should succeed");

    let amount = session.eval_clarity_string(&format!("u{stacked}"));
    let result = session
        .call_contract_fn(
            &stacker,
            "stack",
            &[amount],
            DEPLOYER,
            false,
            false,
            clarity_repl::repl::session::CallKind::Transaction,
        )
        .expect("stack should execute");

    let value = snippet_value(result.result);
    assert!(
        matches!(&value, Value::Response(response) if response.committed),
        "stack should succeed, got {value}"
    );

    locked_amount(session, &stacker)
}

/// The behavior #2491 reported missing: a contract that names the mainnet PoX
/// address must actually lock STX in simnet.
#[tokio::test]
async fn deploying_a_mainnet_pox_caller_locks_stx() {
    let project = Project::new("stacker", STACKER_SOURCE, "");
    let deployment = project.generate().await;

    let locked = stack_through_the_deployed_contract(&project.manifest, &deployment);
    assert_eq!(
        locked, 90_000_000_000,
        "stacking through {BOOT_MAINNET_ADDRESS}.pox-3 should lock the contract's STX"
    );
}

/// The same lock must happen when the plan is round-tripped through disk,
/// where the source is re-read raw from the `.clar` file and only the recorded
/// `remap-principals` says what to rewrite.
#[tokio::test]
async fn a_plan_reloaded_from_disk_still_locks_stx() {
    let project = Project::new("stacker", STACKER_SOURCE, "");
    let generated = project.generate().await;

    let plan_path = project.root().join("plan.yaml");
    fs::write(
        &plan_path,
        generated.to_file_content(project.root()).unwrap(),
    )
    .unwrap();
    let reloaded = clarinet_deployments::load_deployment(project.root(), &plan_path)
        .expect("the written plan should load");

    // Proof that the rewrite is driven by the plan and not by leftover state:
    // the reloaded source is the raw file, mainnet address and all.
    let (source, remap) = publish(&reloaded);
    assert!(
        source.contains(BOOT_MAINNET_ADDRESS),
        "a reloaded plan holds the raw contract source"
    );
    assert_eq!(
        remap,
        vec![(
            BOOT_MAINNET_ADDRESS.to_string(),
            BOOT_TESTNET_ADDRESS.to_string()
        )],
        "the remap must survive the round-trip through the plan file"
    );

    let locked = stack_through_the_deployed_contract(&project.manifest, &reloaded);
    assert_eq!(locked, 90_000_000_000);
}

#[tokio::test]
async fn the_plan_records_and_applies_the_remap() {
    let project = Project::new("stacker", STACKER_SOURCE, "");
    let deployment = project.generate().await;
    let (source, remap) = publish(&deployment);

    assert_eq!(
        remap,
        vec![(
            BOOT_MAINNET_ADDRESS.to_string(),
            BOOT_TESTNET_ADDRESS.to_string()
        )],
        "the plan must record which principal was rewritten"
    );
    assert!(
        source.contains(&format!("'{BOOT_TESTNET_ADDRESS}.pox-3")),
        "the deployed source must call the testnet pox-3"
    );
    assert!(
        !source.contains(BOOT_MAINNET_ADDRESS),
        "no mainnet boot principal should survive in the deployed source"
    );

    // Same length, so every diagnostic span and coverage offset still lines up
    // with the file on disk.
    assert_eq!(source.len(), STACKER_SOURCE.len());
}

/// The user-facing half of "visible": the remap shows up in the plan file.
#[tokio::test]
async fn the_plan_file_shows_the_remap() {
    let project = Project::new("stacker", STACKER_SOURCE, "");
    let deployment = project.generate().await;

    let plan = String::from_utf8(deployment.to_file_content(project.root()).unwrap()).unwrap();
    assert!(
        plan.contains("remap-principals:"),
        "the plan file should show the remap:\n{plan}"
    );
    assert!(
        plan.contains(&format!("{BOOT_MAINNET_ADDRESS}: {BOOT_TESTNET_ADDRESS}")),
        "the plan file should name both principals:\n{plan}"
    );
}

#[tokio::test]
async fn source_using_the_testnet_address_is_untouched() {
    let testnet_source = STACKER_SOURCE.replace(BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS);
    let project = Project::new("stacker", &testnet_source, "");
    let deployment = project.generate().await;
    let (source, remap) = publish(&deployment);

    assert_eq!(source, testnet_source, "the source must be passed through");
    assert!(
        remap.is_empty(),
        "nothing was rewritten, so nothing should be recorded"
    );

    // And it still locks, which is what the mainnet spelling is being made to
    // match.
    let locked = stack_through_the_deployed_contract(&project.manifest, &deployment);
    assert_eq!(locked, 90_000_000_000);
}

/// sBTC lives at `SM3VDXK3...` and requirement contracts at their own
/// deployers; neither has a testnet twin in simnet, so both are out of scope.
#[tokio::test]
async fn sbtc_and_requirement_principals_are_not_remapped() {
    let source = formatdoc!(
        r#"
        (define-constant SBTC '{SBTC_MAINNET_ADDRESS}.sbtc-token)
        (define-constant NFT 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait)
        (define-constant BURN '{BOOT_MAINNET_ADDRESS})
        (define-read-only (get-all) {{ sbtc: SBTC, nft: NFT, burn: BURN }})
    "#
    );
    let project = Project::new("refs", &source, "");
    let deployment = project.generate().await;
    let (deployed_source, remap) = publish(&deployment);

    assert_eq!(deployed_source, source);
    assert!(
        remap.is_empty(),
        "sBTC, requirement and bare burn-address principals must be left alone"
    );
}

/// `clarinet check` generates a simnet plan for the on-chain environment too,
/// to analyse the contract as it will be published. The mainnet addresses are
/// the correct ones there, so that pass must not rewrite.
#[tokio::test]
async fn the_on_chain_environment_is_not_remapped() {
    let project = Project::new("stacker", STACKER_SOURCE, "");
    let (deployment, _artifacts, _) = generate_default_deployment(
        &project.manifest,
        &StacksNetwork::Simnet,
        false,
        None,
        None,
        Environment::OnChain,
    )
    .await
    .expect("on-chain-environment plan should be generated");

    let (source, remap) = publish(&deployment);
    assert_eq!(
        source, STACKER_SOURCE,
        "the on-chain check pass must analyse the unrewritten source"
    );
    assert!(remap.is_empty(), "and must record no remap");
}

/// A plan written before `remap-principals` existed records nothing, which is
/// shaped exactly like a requirement. Project contracts must still be
/// rewritten, or loading such a plan silently reintroduces the bug.
#[tokio::test]
async fn a_plan_without_the_recorded_remap_still_locks_stx() {
    let project = Project::new("stacker", STACKER_SOURCE, "");
    let mut deployment = project.generate().await;

    // Strip the field the way a pre-PR plan would have it, and reset the
    // source to what re-reading the `.clar` file yields.
    for batch in deployment.plan.batches.iter_mut() {
        for tx in batch.transactions.iter_mut() {
            if let TransactionSpecification::EmulatedContractPublish(spec) = tx {
                spec.remap_principals.clear();
                spec.source = STACKER_SOURCE.to_string();
            }
        }
    }

    // `setup_session_with_deployment` is the CLI's load-from-disk entry point.
    let artifacts = setup_session_with_deployment(&project.manifest, &mut deployment, None, false);
    assert!(artifacts.success, "the stale plan should still deploy");

    let (_, remap) = publish(&deployment);
    assert_eq!(
        remap,
        vec![(
            BOOT_MAINNET_ADDRESS.to_string(),
            BOOT_TESTNET_ADDRESS.to_string()
        )],
        "the marker must be re-derived for a project contract"
    );

    let mut session = artifacts.session;
    assert_eq!(
        stack_in_session(&mut session),
        90_000_000_000,
        "a plan predating the field must still lock STX"
    );
}

/// MXS reads real mainnet state, where the mainnet addresses are the correct
/// ones, so the plan must neither rewrite nor record anything.
#[tokio::test]
async fn mxs_preserves_the_mainnet_principal() {
    let project = Project::new(
        "stacker",
        STACKER_SOURCE,
        "\n[repl.remote_data]\nenabled = true\napi_url = \"https://api.hiro.so\"\ninitial_height = 522000\n",
    );
    assert!(
        project.manifest.repl_settings.remote_data.enabled,
        "the fixture must actually have MXS enabled"
    );

    let deployment = project.generate().await;
    let (source, remap) = publish(&deployment);

    assert_eq!(
        source, STACKER_SOURCE,
        "MXS must deploy the source verbatim"
    );
    assert!(
        remap.is_empty(),
        "MXS must not record a boot-contract remap"
    );
}

/// Regenerating the plan must produce byte-identical output, otherwise
/// `clarinet` would prompt to overwrite the plan on every run.
#[tokio::test]
async fn regenerating_the_plan_is_stable() {
    let project = Project::new("stacker", STACKER_SOURCE, "");

    let first = project
        .generate()
        .await
        .to_file_content(project.root())
        .unwrap();
    let second = project
        .generate()
        .await
        .to_file_content(project.root())
        .unwrap();

    assert_eq!(
        String::from_utf8(first).unwrap(),
        String::from_utf8(second).unwrap(),
        "plan generation must be deterministic"
    );
}

/// Deploying the same plan twice into fresh sessions must give the same
/// result — the rewrite carries no state between runs.
#[tokio::test]
async fn redeploying_is_idempotent() {
    let project = Project::new("stacker", STACKER_SOURCE, "");
    let deployment = project.generate().await;

    let first = stack_through_the_deployed_contract(&project.manifest, &deployment);
    let second = stack_through_the_deployed_contract(&project.manifest, &deployment);
    assert_eq!(first, second);
    assert_eq!(first, 90_000_000_000);
}
