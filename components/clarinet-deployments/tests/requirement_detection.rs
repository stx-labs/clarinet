//! Requirements are detected from what the project contracts reference.
//! Cache fixtures keep the tests offline; the mock API serves nothing.

use std::fs;
use std::path::Path;

use clarinet_deployments::types::{DeploymentSpecification, TransactionSpecification};
use clarinet_deployments::{generate_default_deployment, setup_session_with_deployment};
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarinet_utils::DEFAULT_DEPLOYER_MNEMONIC as TEST_MNEMONIC;
use clarity_repl::utils::Environment;
use indoc::formatdoc;
use mockito::Server;
use tempfile::TempDir;

const EXTERNAL_DEPLOYER: &str = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9";

const ROUTER: &str = r#"
(define-trait greeter ((greet () (response (string-ascii 8) uint))))
(define-public (route (target <greeter>))
    (contract-call? target greet))
"#;

const GREETER: &str = r#"
(impl-trait .router.greeter)
(define-public (greet) (ok "hi"))
"#;

/// Write a project with a single `caller` contract, and cache the given
/// external contracts so they resolve without network access.
fn write_project(root: &Path, caller: &str, cached: &[(&str, &str)]) {
    fs::create_dir_all(root.join("settings")).unwrap();
    fs::create_dir_all(root.join("contracts")).unwrap();
    fs::create_dir_all(root.join(".cache/requirements")).unwrap();

    #[rustfmt::skip]
    let manifest = formatdoc!(r#"
        [project]
        name = "requirement-detection-test"
        authors = []
        description = ""
        telemetry = false
        cache_dir = "./.cache"

        [contracts.caller]
        path = "contracts/caller.clar"
        epoch = "latest"
    "#);

    #[rustfmt::skip]
    let devnet_settings = formatdoc!(r#"
        [network]
        name = "devnet"

        [accounts.deployer]
        mnemonic = "{TEST_MNEMONIC}"
        balance = 100_000_000_000_000
    "#);

    fs::write(root.join("Clarinet.toml"), manifest).unwrap();
    fs::write(root.join("settings/Devnet.toml"), devnet_settings).unwrap();
    fs::write(root.join("contracts/caller.clar"), caller).unwrap();

    for (name, source) in cached {
        let stem = format!(".cache/requirements/{EXTERNAL_DEPLOYER}.{name}");
        fs::write(root.join(format!("{stem}.clar")), source).unwrap();
        fs::write(
            root.join(format!("{stem}.json")),
            r#"{"epoch":"Epoch24","clarity_version":"Clarity2"}"#,
        )
        .unwrap();
    }
}

async fn generate(
    caller: &str,
    cached: &[(&str, &str)],
) -> (TempDir, ProjectManifest, DeploymentSpecification) {
    let server = Server::new_async().await;
    let temp_dir = TempDir::new().unwrap();
    write_project(temp_dir.path(), caller, cached);
    let manifest =
        ProjectManifest::from_location(&temp_dir.path().join("Clarinet.toml"), false).unwrap();

    let (deployment, artifacts, _) = generate_default_deployment(
        &manifest,
        &StacksNetwork::Simnet,
        false,
        None,
        Some(&server.url()),
        Environment::Simnet,
    )
    .await
    .expect("deployment plan should be generated");
    assert!(artifacts.success, "{:?}", artifacts.diags);

    (temp_dir, manifest, deployment)
}

fn published_requirements(deployment: &DeploymentSpecification) -> Vec<String> {
    deployment
        .plan
        .batches
        .iter()
        .flat_map(|batch| &batch.transactions)
        .filter_map(|tx| match tx {
            TransactionSpecification::EmulatedContractPublish(spec)
                if spec.emulated_sender.to_string() == EXTERNAL_DEPLOYER =>
            {
                Some(spec.contract_name.to_string())
            }
            _ => None,
        })
        .collect()
}

#[tokio::test]
async fn a_contract_passed_to_an_external_trait_parameter_is_a_requirement() {
    // `greeter` is never referenced directly: only `router`'s signature tells
    // that the principal is a trait argument, so it is found in a second pass.
    let caller = format!(
        "(define-public (call) (contract-call? '{EXTERNAL_DEPLOYER}.router route '{EXTERNAL_DEPLOYER}.greeter))\n"
    );
    let (_temp_dir, manifest, mut deployment) =
        generate(&caller, &[("router", ROUTER), ("greeter", GREETER)]).await;

    assert_eq!(published_requirements(&deployment), ["router", "greeter"]);

    let deployed =
        setup_session_with_deployment(&manifest, &mut deployment, None, true, Environment::Simnet);
    assert!(deployed.success, "{:?}", deployed.diags);
}

#[tokio::test]
async fn an_unretrievable_reference_is_left_to_the_analysis() {
    let caller =
        format!("(define-read-only (call) (contract-call? '{EXTERNAL_DEPLOYER}.missing greet))\n");
    let (_temp_dir, manifest, mut deployment) = generate(&caller, &[]).await;

    assert!(published_requirements(&deployment).is_empty());

    let deployed =
        setup_session_with_deployment(&manifest, &mut deployment, None, true, Environment::Simnet);
    assert!(
        deployed
            .diags
            .values()
            .flatten()
            .any(|diag| diag.message.contains("use of unresolved contract")),
        "{:?}",
        deployed.diags
    );
}
