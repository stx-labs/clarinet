//! Contracts remapped to a contract already deployed on the target network:
//! by the project's `[remap]` entries, or by the built-in defaults (sBTC on
//! testnet).

use std::collections::BTreeMap;
use std::fs;

use clarinet_deployments::generate_default_deployment;
use clarinet_deployments::types::{
    DeploymentSpecification, DeploymentSpecificationFile, TransactionSpecification,
};
use clarinet_files::{ProjectManifest, StacksNetwork};
use clarinet_utils::DEFAULT_DEPLOYER_MNEMONIC as TEST_MNEMONIC;
use clarity_repl::repl::boot::SBTC_BOOT_CONTRACTS;
use clarity_repl::repl::ClarityCodeSource;
use clarity_repl::utils::Environment;
use indoc::{formatdoc, indoc};
use mockito::{Server, ServerGuard};
use tempfile::TempDir;

/// sBTC's mainnet and testnet deployers, written out rather than imported so
/// the test pins the values instead of following edits to the constants.
const SBTC_MAINNET_DEPLOYER: &str = "SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4";
const SBTC_TESTNET_DEPLOYER: &str = "SN3VMHXEN64ZZF71JQ5VESXDWTR301XTTXGF4J8F1";

const NFT_TRAIT: &str = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait";
const TESTNET_NFT_TRAIT: &str = "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.nft-trait";

/// The remap keys off the identifier, not the body, so a dependency-free
/// source keeps the tests fast.
const REQUIREMENT_SOURCE: &str = "(define-read-only (get-one) (ok u1))";

struct Project {
    dir: TempDir,
}

impl Project {
    /// A project with `project_toml` appended to its `[project]` table and an
    /// optional `app` contract.
    fn new(project_toml: &str, app_source: Option<&str>) -> Self {
        let dir = TempDir::new().unwrap();
        let root = dir.path();
        fs::create_dir_all(root.join("settings")).unwrap();
        fs::create_dir_all(root.join("contracts")).unwrap();

        let contracts = match app_source {
            Some(source) => {
                fs::write(root.join("contracts/app.clar"), source).unwrap();
                indoc! {r#"
                    [contracts.app]
                    path = "contracts/app.clar"
                    clarity_version = 4
                    epoch = 3.3
                "#}
            }
            None => "",
        };

        #[rustfmt::skip]
        let manifest = formatdoc!(r#"
            [project]
            name = "remap-test"
            authors = []
            description = ""
            telemetry = false
            {project_toml}

            {contracts}
        "#);
        fs::write(root.join("Clarinet.toml"), manifest).unwrap();

        for network in ["Testnet", "Mainnet"] {
            #[rustfmt::skip]
            let settings = formatdoc!(r#"
                [network]
                name = "{}"
                deployment_fee_rate = 10

                [accounts.deployer]
                mnemonic = "{TEST_MNEMONIC}"
            "#, network.to_lowercase());
            fs::write(root.join(format!("settings/{network}.toml")), settings).unwrap();
        }

        Self { dir }
    }

    async fn generate(
        &self,
        network: StacksNetwork,
        server: &ServerGuard,
    ) -> DeploymentSpecification {
        let manifest =
            ProjectManifest::from_location(&self.dir.path().join("Clarinet.toml"), false).unwrap();
        let (deployment, _, _) = generate_default_deployment(
            &manifest,
            &network,
            false,
            None,
            Some(&server.url()),
            Environment::OnChain,
        )
        .await
        .expect("deployment plan should be generated");
        deployment
    }
}

/// Serve each of `contract_ids` from a mock API so the tests stay offline.
async fn mock_contracts(contract_ids: &[&str]) -> ServerGuard {
    let sources: Vec<_> = contract_ids
        .iter()
        .map(|contract_id| (*contract_id, REQUIREMENT_SOURCE))
        .collect();
    mock_contract_sources(&sources).await
}

/// Serve each `(contract_id, source)` from a mock API.
async fn mock_contract_sources(contracts: &[(&str, &str)]) -> ServerGuard {
    // An epoch 3.0 height on mainnet.
    mock_contract_sources_at(contracts, 175232).await
}

/// Serve each `(contract_id, source)` as published at `block_height`.
async fn mock_contract_sources_at(contracts: &[(&str, &str)], block_height: u32) -> ServerGuard {
    let mut server = Server::new_async().await;
    for (contract_id, source) in contracts {
        server
            .mock(
                "GET",
                format!("/extended/v1/contract/{contract_id}").as_str(),
            )
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                serde_json::json!({
                    "source_code": source,
                    "block_height": block_height,
                    "clarity_version": 3
                })
                .to_string(),
            )
            .create_async()
            .await;
    }
    server
}

/// A project contract calling `get-one` on each of `contract_ids`.
fn calls(contract_ids: &[&str]) -> String {
    contract_ids
        .iter()
        .enumerate()
        .map(|(i, contract_id)| {
            format!("(define-read-only (call-{i}) (contract-call? '{contract_id} get-one))\n")
        })
        .collect()
}

fn requirement_publishes(deployment: &DeploymentSpecification) -> Vec<String> {
    deployment
        .plan
        .batches
        .iter()
        .flat_map(|batch| &batch.transactions)
        .filter_map(|tx| match tx {
            TransactionSpecification::RequirementPublish(spec) => {
                Some(spec.contract_id.to_string())
            }
            _ => None,
        })
        .collect()
}

fn contract_remaps(deployment: &DeploymentSpecification) -> BTreeMap<String, String> {
    deployment
        .contract_remaps
        .as_ref()
        .map(|remaps| remaps.pairs().collect())
        .unwrap_or_default()
}

fn sbtc_remaps(names: &[&str]) -> BTreeMap<String, String> {
    names
        .iter()
        .map(|name| {
            (
                format!("{SBTC_MAINNET_DEPLOYER}.{name}"),
                format!("{SBTC_TESTNET_DEPLOYER}.{name}"),
            )
        })
        .collect()
}

#[tokio::test]
async fn testnet_override_replaces_the_requirement_publish() {
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{NFT_TRAIT}"

            [remap]
            "{NFT_TRAIT}" = {{ testnet = "{TESTNET_NFT_TRAIT}" }}
        "#},
        Some(&calls(&[NFT_TRAIT])),
    );
    let server = mock_contracts(&[NFT_TRAIT, TESTNET_NFT_TRAIT]).await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(requirement_publishes(&deployment), Vec::<String>::new());
    assert_eq!(
        contract_remaps(&deployment),
        BTreeMap::from([(NFT_TRAIT.to_string(), TESTNET_NFT_TRAIT.to_string())])
    );
}

#[tokio::test]
async fn contract_remaps_round_trip_through_the_plan_file() {
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{NFT_TRAIT}"

            [remap]
            "{NFT_TRAIT}" = {{ testnet = "{TESTNET_NFT_TRAIT}" }}
        "#},
        Some(&calls(&[NFT_TRAIT])),
    );
    let server = mock_contracts(&[NFT_TRAIT, TESTNET_NFT_TRAIT]).await;
    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    let content = deployment.to_file_content(project.dir.path()).unwrap();
    let content = String::from_utf8(content).unwrap();
    assert!(
        content.contains(&formatdoc! {"
            contract-remaps:
              {NFT_TRAIT}: {TESTNET_NFT_TRAIT}
        "}),
        "{content}"
    );

    let file = DeploymentSpecificationFile::from_file_content(&content).unwrap();
    let reloaded = DeploymentSpecification::from_specifications(
        &file,
        &StacksNetwork::Testnet,
        project.dir.path(),
        None,
    )
    .unwrap();
    assert_eq!(reloaded.contract_remaps, deployment.contract_remaps);
}

#[tokio::test]
async fn requirement_without_override_is_still_redeployed() {
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{NFT_TRAIT}"
        "#},
        None,
    );
    let server = mock_contracts(&[NFT_TRAIT]).await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(requirement_publishes(&deployment), vec![NFT_TRAIT]);
    assert!(
        deployment.contract_remaps.is_none(),
        "a plan that references no remapped contract omits the remap section"
    );
    let content = deployment.to_file_content(project.dir.path()).unwrap();
    assert!(!String::from_utf8(content)
        .unwrap()
        .contains("contract-remaps"));
}

#[tokio::test]
async fn sbtc_requirement_points_at_the_sbtc_testnet_deployment() {
    let sbtc_deposit = format!("{SBTC_MAINNET_DEPLOYER}.sbtc-deposit");
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{sbtc_deposit}"
        "#},
        Some(&calls(&[&sbtc_deposit])),
    );
    let server = mock_contracts(&[]).await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(
        requirement_publishes(&deployment),
        Vec::<String>::new(),
        "sBTC is already on testnet and must not be redeployed"
    );
    assert_eq!(
        contract_remaps(&deployment),
        sbtc_remaps(&["sbtc-deposit"]),
        "only the sBTC contracts the published sources mention are recorded"
    );
}

#[tokio::test]
async fn sbtc_referenced_from_project_code_is_remapped() {
    // The registry is only used as data, which creates no dependency edge,
    // but its identifier is still rewritten in the published source.
    let source = formatdoc! {"
        (define-constant registry '{SBTC_MAINNET_DEPLOYER}.sbtc-registry)
        (define-read-only (get-registry) registry)
        (define-read-only (token-name) (contract-call? '{SBTC_MAINNET_DEPLOYER}.sbtc-token get-name))
    "};
    let project = Project::new("", Some(&source));
    let server = mock_contracts(&[]).await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(
        contract_remaps(&deployment),
        sbtc_remaps(&["sbtc-registry", "sbtc-token"])
    );
}

#[tokio::test]
async fn remaps_referenced_by_a_redeployed_requirement_are_recorded() {
    // `wrapper` is redeployed under the deployer; the trait it calls, at the
    // same issuer, points at an existing testnet contract instead.
    let wrapper = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.wrapper";
    let canonical = "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.canonical-trait";
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{wrapper}"

            [[project.requirements]]
            contract_id = "{NFT_TRAIT}"

            [remap]
            "{NFT_TRAIT}" = {{ testnet = "{canonical}" }}
        "#},
        None,
    );
    let wrapper_source = calls(&[NFT_TRAIT]);
    let server = mock_contract_sources(&[
        (wrapper, &wrapper_source),
        (NFT_TRAIT, REQUIREMENT_SOURCE),
        (canonical, REQUIREMENT_SOURCE),
    ])
    .await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(requirement_publishes(&deployment), vec![wrapper]);
    assert_eq!(
        contract_remaps(&deployment),
        BTreeMap::from([(NFT_TRAIT.to_string(), canonical.to_string())])
    );
}

/// A redeployed requirement that reaches a remapped contract only through a
/// relative reference still records the remap.
#[tokio::test]
async fn relative_references_select_their_remap() {
    let wrapper = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.wrapper";
    let base = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.base";
    let canonical = "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.canonical-base";
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{wrapper}"

            [[project.requirements]]
            contract_id = "{base}"

            [remap]
            "{base}" = {{ testnet = "{canonical}" }}
        "#},
        None,
    );
    let wrapper_source = "(define-read-only (call-base) (contract-call? .base get-one))";
    let server = mock_contract_sources(&[
        (wrapper, wrapper_source),
        (base, REQUIREMENT_SOURCE),
        (canonical, REQUIREMENT_SOURCE),
    ])
    .await;
    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(requirement_publishes(&deployment), vec![wrapper]);
    assert_eq!(
        contract_remaps(&deployment),
        BTreeMap::from([(base.to_string(), canonical.to_string())])
    );
}

const ROUTER: &str = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.router";
const POOL: &str = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.pool";
const TESTNET_ROUTER: &str = "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.router";

/// Serve `router`, its testnet copy, and `pool`. `router` calls `pool`.
async fn mock_router_and_pool() -> ServerGuard {
    let router_source = format!("{REQUIREMENT_SOURCE}\n{}", calls(&[POOL]));
    mock_contract_sources(&[
        (ROUTER, &router_source),
        (POOL, REQUIREMENT_SOURCE),
        (TESTNET_ROUTER, &router_source),
    ])
    .await
}

#[tokio::test]
async fn dependencies_of_a_remapped_requirement_are_not_redeployed() {
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{ROUTER}"

            [remap]
            "{ROUTER}" = {{ testnet = "{TESTNET_ROUTER}" }}
        "#},
        Some(&calls(&[ROUTER])),
    );
    let server = mock_router_and_pool().await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(requirement_publishes(&deployment), Vec::<String>::new());
}

#[tokio::test]
async fn a_dependency_also_used_directly_is_still_redeployed() {
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{ROUTER}"

            [remap]
            "{ROUTER}" = {{ testnet = "{TESTNET_ROUTER}" }}
        "#},
        Some(&calls(&[ROUTER, POOL])),
    );
    let server = mock_router_and_pool().await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(requirement_publishes(&deployment), vec![POOL]);
}

/// The bundled source of the sBTC contract `name`.
fn sbtc_source(name: &str) -> String {
    SBTC_BOOT_CONTRACTS
        .iter()
        .find_map(|(id, (contract, _))| match &contract.code_source {
            ClarityCodeSource::ContractInMemory(source) if id.name.as_str() == name => {
                Some(source.clone())
            }
            _ => None,
        })
        .unwrap()
}

/// Serve the bundled sBTC sources at the mainnet deployer, plus the testnet
/// `sbtc-deposit`.
async fn mock_sbtc() -> ServerGuard {
    let names = ["sbtc-registry", "sbtc-token", "sbtc-deposit"];
    let mut contracts: Vec<_> = names
        .iter()
        .map(|name| (format!("{SBTC_MAINNET_DEPLOYER}.{name}"), sbtc_source(name)))
        .collect();
    contracts.push((
        format!("{SBTC_TESTNET_DEPLOYER}.sbtc-deposit"),
        sbtc_source("sbtc-deposit"),
    ));
    let contracts: Vec<_> = contracts
        .iter()
        .map(|(id, source)| (id.as_str(), source.as_str()))
        .collect();
    mock_contract_sources(&contracts).await
}

#[tokio::test]
async fn partially_remapped_sbtc_generates_a_plan() {
    let sbtc_deposit = format!("{SBTC_MAINNET_DEPLOYER}.sbtc-deposit");
    let project = Project::new(
        &formatdoc! {r#"
            default_contract_remaps = false

            [[project.requirements]]
            contract_id = "{sbtc_deposit}"

            [remap]
            "{sbtc_deposit}" = {{ testnet = "{SBTC_TESTNET_DEPLOYER}.sbtc-deposit" }}
        "#},
        Some(&format!(
            "(define-read-only (header) (contract-call? '{sbtc_deposit} get-burn-header u1))"
        )),
    );
    let server = mock_sbtc().await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(requirement_publishes(&deployment), Vec::<String>::new());
}

#[tokio::test]
async fn a_redeployed_sbtc_contract_keeps_its_dependencies() {
    let sbtc_deposit = format!("{SBTC_MAINNET_DEPLOYER}.sbtc-deposit");
    let sbtc_token = format!("{SBTC_MAINNET_DEPLOYER}.sbtc-token");
    let project = Project::new(
        &formatdoc! {r#"
            default_contract_remaps = false

            [[project.requirements]]
            contract_id = "{sbtc_deposit}"

            [remap]
            "{sbtc_deposit}" = {{ testnet = "{SBTC_TESTNET_DEPLOYER}.sbtc-deposit" }}
        "#},
        Some(&format!(
            "(define-read-only (name) (contract-call? '{sbtc_token} get-name))"
        )),
    );
    let server = mock_sbtc().await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(
        requirement_publishes(&deployment),
        vec![format!("{SBTC_MAINNET_DEPLOYER}.sbtc-registry"), sbtc_token]
    );
}

#[tokio::test]
async fn testnet_requirement_is_used_in_place() {
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{TESTNET_NFT_TRAIT}"
        "#},
        Some(&calls(&[TESTNET_NFT_TRAIT])),
    );
    // An epoch 3.2 height on testnet, before the project contract's epoch.
    let server = mock_contract_sources_at(&[(TESTNET_NFT_TRAIT, REQUIREMENT_SOURCE)], 730).await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(requirement_publishes(&deployment), Vec::<String>::new());
    assert_eq!(
        contract_remaps(&deployment),
        BTreeMap::from([(TESTNET_NFT_TRAIT.to_string(), TESTNET_NFT_TRAIT.to_string())])
    );
}

#[tokio::test]
async fn an_unlisted_testnet_dependency_is_used_in_place() {
    const WRAPPER: &str = "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.wrapper";
    const BASE: &str = "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.base";
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{WRAPPER}"
        "#},
        Some(&calls(&[WRAPPER, BASE])),
    );
    let wrapper_source = format!("{REQUIREMENT_SOURCE}\n{}", calls(&[BASE]));
    // An epoch 3.2 height on testnet, before the project contract's epoch.
    let server = mock_contract_sources_at(
        &[(WRAPPER, &wrapper_source), (BASE, REQUIREMENT_SOURCE)],
        730,
    )
    .await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(requirement_publishes(&deployment), Vec::<String>::new());
    assert_eq!(
        contract_remaps(&deployment),
        BTreeMap::from([
            (BASE.to_string(), BASE.to_string()),
            (WRAPPER.to_string(), WRAPPER.to_string()),
        ])
    );
}

#[tokio::test]
async fn opting_out_of_default_remaps_redeploys_sbtc() {
    let sbtc_deposit = format!("{SBTC_MAINNET_DEPLOYER}.sbtc-deposit");
    let project = Project::new(
        &formatdoc! {r#"
            default_contract_remaps = false

            [[project.requirements]]
            contract_id = "{sbtc_deposit}"
        "#},
        None,
    );
    let server = mock_sbtc().await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(
        requirement_publishes(&deployment),
        ["sbtc-registry", "sbtc-token", "sbtc-deposit"]
            .map(|name| format!("{SBTC_MAINNET_DEPLOYER}.{name}"))
    );
    // Empty rather than absent, so deploying skips the legacy sBTC rewrite.
    let content = deployment.to_file_content(project.dir.path()).unwrap();
    assert!(String::from_utf8(content)
        .unwrap()
        .contains("contract-remaps: {}"));
}

#[tokio::test]
async fn project_override_beats_the_default() {
    let sbtc_token = format!("{SBTC_MAINNET_DEPLOYER}.sbtc-token");
    let mock_token = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.sbtc-token-mock";
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{sbtc_token}"

            [remap]
            "{sbtc_token}" = {{ testnet = "{mock_token}" }}
        "#},
        Some(&calls(&[
            &sbtc_token,
            &format!("{SBTC_MAINNET_DEPLOYER}.sbtc-registry"),
        ])),
    );
    let server = mock_contracts(&[mock_token]).await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    let remaps = contract_remaps(&deployment);
    assert_eq!(
        remaps.get(&sbtc_token).map(String::as_str),
        Some(mock_token)
    );
    assert_eq!(
        remaps.get(&format!("{SBTC_MAINNET_DEPLOYER}.sbtc-registry")),
        Some(&format!("{SBTC_TESTNET_DEPLOYER}.sbtc-registry")),
        "the defaults the project does not override still apply"
    );
}

#[tokio::test]
async fn mainnet_override_is_recorded() {
    let mainnet_trait = "SP3K8BC0PPEVCV7NZ6QSRWPQ2JE9E5B6N3PA0KBR9.nft-trait";
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{NFT_TRAIT}"

            [remap]
            "{NFT_TRAIT}" = {{ mainnet = "{mainnet_trait}" }}
        "#},
        Some(&calls(&[NFT_TRAIT])),
    );
    let server = mock_contracts(&[NFT_TRAIT, mainnet_trait]).await;

    let deployment = project.generate(StacksNetwork::Mainnet, &server).await;

    assert_eq!(
        contract_remaps(&deployment),
        BTreeMap::from([(NFT_TRAIT.to_string(), mainnet_trait.to_string())])
    );
}

#[tokio::test]
async fn an_address_remap_covers_every_contract_of_that_address() {
    // The issuers of `ROUTER` and `TESTNET_ROUTER`.
    let address = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9";
    let testnet_address = "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT";
    let project = Project::new(
        &formatdoc! {r#"
            [[project.requirements]]
            contract_id = "{ROUTER}"

            [remap]
            "{address}" = {{ testnet = "{testnet_address}" }}
        "#},
        Some(&calls(&[ROUTER])),
    );
    let server = mock_router_and_pool().await;

    let deployment = project.generate(StacksNetwork::Testnet, &server).await;

    assert_eq!(
        requirement_publishes(&deployment),
        Vec::<String>::new(),
        "neither `router` nor the `pool` it calls is redeployed"
    );
    assert_eq!(
        contract_remaps(&deployment),
        BTreeMap::from([(address.to_string(), testnet_address.to_string())])
    );

    let content = deployment.to_file_content(project.dir.path()).unwrap();
    let content = String::from_utf8(content).unwrap();
    assert!(
        content.contains(&formatdoc! {"
            contract-remaps:
              {address}: {testnet_address}
        "}),
        "{content}"
    );
    let file = DeploymentSpecificationFile::from_file_content(&content).unwrap();
    let reloaded = DeploymentSpecification::from_specifications(
        &file,
        &StacksNetwork::Testnet,
        project.dir.path(),
        None,
    )
    .unwrap();
    assert_eq!(reloaded.contract_remaps, deployment.contract_remaps);
}
