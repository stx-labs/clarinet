use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{LazyLock, Mutex};
use std::time::Duration;

use clarinet_defaults::{DEFAULT_CLARITY_VERSION, DEFAULT_EPOCH};
use clarinet_files::{paths, FileAccessor};
use clarity::types::chainstate::StacksAddress;
use clarity::types::{Address, StacksEpochId};
use clarity::vm::types::QualifiedContractIdentifier;
use clarity::vm::ClarityVersion;
use clarity_repl::repl::clarity_version_from_u8;
use clarity_repl::repl::remote_data::epoch_for_height;
use reqwest;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContractMetadata {
    pub epoch: StacksEpochId,
    pub clarity_version: ClarityVersion,
}

impl Default for ContractMetadata {
    fn default() -> Self {
        ContractMetadata {
            epoch: DEFAULT_EPOCH,
            clarity_version: DEFAULT_CLARITY_VERSION,
        }
    }
}

/// Bounds a contract fetch, so an unreachable API can't stall plan generation.
const FETCH_TIMEOUT: Duration = Duration::from_secs(30);

/// How long a failed lookup of a detected dependency is remembered.
const UNAVAILABLE_TTL_SECS: f64 = 300.0;

/// Failed lookups of detected dependencies, keyed by API and contract, with the
/// time of the failure in seconds.
static UNAVAILABLE: LazyLock<Mutex<HashMap<(Option<String>, QualifiedContractIdentifier), f64>>> =
    LazyLock::new(Mutex::default);

/// Shared so contract fetches reuse connections.
static CLIENT: LazyLock<reqwest::Client> = LazyLock::new(reqwest::Client::new);

fn now_secs() -> f64 {
    #[cfg(target_arch = "wasm32")]
    {
        js_sys::Date::now() / 1000.0
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or(0.0, |elapsed| elapsed.as_secs_f64())
    }
}

/// [`retrieve_contract`] for a dependency detected in the project's contracts
/// rather than declared. A failure is remembered for a few minutes, so a
/// reference that can't be resolved doesn't hit the network again on every
/// plan generation (each LSP rebuild, each `clarinet check`).
pub async fn retrieve_detected_contract(
    contract_id: &QualifiedContractIdentifier,
    cache_location: &Path,
    file_accessor: &Option<&dyn FileAccessor>,
    api_base_url: Option<&str>,
) -> Result<(String, StacksEpochId, ClarityVersion, PathBuf), String> {
    let key = (api_base_url.map(str::to_string), contract_id.clone());
    let failed_at = UNAVAILABLE
        .lock()
        .ok()
        .and_then(|failed| failed.get(&key).copied());
    if failed_at.is_some_and(|failed_at| now_secs() - failed_at < UNAVAILABLE_TTL_SECS) {
        return Err(format!("contract {contract_id} was recently unavailable"));
    }

    let contract =
        retrieve_contract(contract_id, cache_location, file_accessor, api_base_url).await;
    if let Ok(mut failed) = UNAVAILABLE.lock() {
        match contract {
            Ok(_) => failed.remove(&key),
            Err(_) => failed.insert(key, now_secs()),
        };
    }
    contract
}

pub async fn retrieve_contract(
    contract_id: &QualifiedContractIdentifier,
    cache_location: &Path,
    file_accessor: &Option<&dyn FileAccessor>,
    api_base_url: Option<&str>,
) -> Result<(String, StacksEpochId, ClarityVersion, PathBuf), String> {
    let contract_deployer = contract_id.issuer.to_address();
    let contract_name = contract_id.name.to_string();

    let requirements_dir = cache_location.join("requirements");
    let contract_location =
        requirements_dir.join(format!("{contract_deployer}.{contract_name}.clar"));
    let metadata_location =
        requirements_dir.join(format!("{contract_deployer}.{contract_name}.json"));

    let (contract_source, metadata_json) = match file_accessor {
        None => (
            paths::read_content_as_utf8(&contract_location),
            paths::read_content_as_utf8(&metadata_location),
        ),
        Some(file_accessor) => (
            file_accessor
                .read_file(contract_location.to_string_lossy().into_owned())
                .await,
            file_accessor
                .read_file(metadata_location.to_string_lossy().into_owned())
                .await,
        ),
    };

    if let (Ok(contract_source), Ok(metadata_json)) = (contract_source, metadata_json) {
        let metadata: ContractMetadata = serde_json::from_str(&metadata_json)
            .map_err(|e| format!("Unable to parse metadata file: {e}"))?;

        log::debug!("requirement cache hit: {contract_deployer}.{contract_name}");
        return Ok((
            contract_source,
            metadata.epoch,
            metadata.clarity_version,
            contract_location,
        ));
    }

    let is_mainnet = StacksAddress::from_string(&contract_deployer)
        .unwrap()
        .is_mainnet();

    let api_base_url = api_base_url.unwrap_or_else(|| default_api_base_url(is_mainnet));
    log::debug!("fetching requirement {contract_deployer}.{contract_name} from {api_base_url}");
    let contract = fetch_contract(api_base_url, &contract_deployer, &contract_name).await?;

    let epoch = epoch_for_height(is_mainnet, contract.block_height);
    let clarity_version = match contract.clarity_version {
        Some(v) => {
            clarity_version_from_u8(v).ok_or_else(|| format!("Unsupported clarity_version: {v}"))?
        }
        None => ClarityVersion::default_for_epoch(epoch),
    };

    match file_accessor {
        None => {
            paths::write_content(&contract_location, contract.source_code.as_bytes())?;
            paths::write_content(
                &metadata_location,
                serde_json::to_string_pretty(&ContractMetadata {
                    epoch,
                    clarity_version,
                })
                .unwrap()
                .as_bytes(),
            )?;
        }
        Some(file_accessor) => {
            file_accessor
                .write_file(
                    contract_location.to_string_lossy().into_owned(),
                    contract.source_code.as_bytes(),
                )
                .await?;
            file_accessor
                .write_file(
                    metadata_location.to_string_lossy().into_owned(),
                    serde_json::to_string_pretty(&ContractMetadata {
                        epoch,
                        clarity_version,
                    })
                    .unwrap()
                    .as_bytes(),
                )
                .await?;
        }
    };

    Ok((
        contract.source_code,
        epoch,
        clarity_version,
        contract_location,
    ))
}

#[allow(dead_code)]
#[derive(Deserialize, Debug, Default, Clone)]
struct Contract {
    source_code: String,
    block_height: u32,
    clarity_version: Option<u8>,
}

fn default_api_base_url(is_mainnet: bool) -> &'static str {
    if is_mainnet {
        "https://api.hiro.so"
    } else {
        "https://api.testnet.hiro.so"
    }
}

async fn fetch_contract(
    api_base_url: &str,
    deployer: &str,
    name: &str,
) -> Result<Contract, String> {
    let url = format!("{api_base_url}/extended/v1/contract/{deployer}.{name}");
    let response = CLIENT
        .get(&url)
        .timeout(FETCH_TIMEOUT)
        .send()
        .await
        .map_err(|e| format!("Unable to retrieve contract {url}: {e}"))?;

    let status = response.status();
    if !status.is_success() {
        return Err(format!("Unable to retrieve contract {url}: {status}"));
    }

    response
        .json()
        .await
        .map_err(|e| format!("Unable to parse contract json data {url}: {e}"))
}

#[cfg(test)]
mod tests {
    use mockito::Server;

    use super::*;

    const TEST_DEPLOYER: &str = "SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4";
    const TEST_CONTRACT_NAME: &str = "test-contract";
    const TEST_SOURCE: &str = "(define-public (hello) (ok u1))";

    #[tokio::test]
    async fn test_fetch_contract_from_mock_server() {
        let mut server = Server::new_async().await;

        let mock = server
            .mock(
                "GET",
                format!("/extended/v1/contract/{TEST_DEPLOYER}.{TEST_CONTRACT_NAME}").as_str(),
            )
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                serde_json::json!({
                    "source_code": TEST_SOURCE,
                    "block_height": 175232,
                    "clarity_version": 3
                })
                .to_string(),
            )
            .create_async()
            .await;

        let contract = fetch_contract(&server.url(), TEST_DEPLOYER, TEST_CONTRACT_NAME)
            .await
            .unwrap();

        assert_eq!(contract.source_code, TEST_SOURCE);
        assert_eq!(contract.block_height, 175232);
        assert_eq!(contract.clarity_version, Some(3));
        mock.assert_async().await;
    }

    #[tokio::test]
    async fn test_fetch_contract_returns_error_on_404() {
        let mut server = Server::new_async().await;

        let mock = server
            .mock(
                "GET",
                format!("/extended/v1/contract/{TEST_DEPLOYER}.{TEST_CONTRACT_NAME}").as_str(),
            )
            .with_status(404)
            .create_async()
            .await;

        let result = fetch_contract(&server.url(), TEST_DEPLOYER, TEST_CONTRACT_NAME).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("404"));
        mock.assert_async().await;
    }

    #[tokio::test]
    async fn test_retrieve_contract_fetches_and_caches() {
        let mut server = Server::new_async().await;

        let mock = server
            .mock(
                "GET",
                format!("/extended/v1/contract/{TEST_DEPLOYER}.{TEST_CONTRACT_NAME}").as_str(),
            )
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(
                serde_json::json!({
                    "source_code": TEST_SOURCE,
                    "block_height": 175232,
                    "clarity_version": 3
                })
                .to_string(),
            )
            .expect(1)
            .create_async()
            .await;

        let cache_dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(cache_dir.path().join("requirements")).unwrap();

        let contract_id =
            QualifiedContractIdentifier::parse(&format!("{TEST_DEPLOYER}.{TEST_CONTRACT_NAME}"))
                .unwrap();

        // First call: fetches from mock server and caches
        let (source, _epoch, clarity_version, location) =
            retrieve_contract(&contract_id, cache_dir.path(), &None, Some(&server.url()))
                .await
                .unwrap();

        assert_eq!(source, TEST_SOURCE);
        assert_eq!(clarity_version, ClarityVersion::Clarity3);
        assert!(location.to_string_lossy().contains(TEST_CONTRACT_NAME));
        mock.assert_async().await;

        // Verify cache files were written
        let cached_source = std::fs::read_to_string(
            cache_dir
                .path()
                .join("requirements")
                .join(format!("{TEST_DEPLOYER}.{TEST_CONTRACT_NAME}.clar")),
        )
        .unwrap();
        assert_eq!(cached_source, TEST_SOURCE);

        // Second call: should use cache (mock expects exactly 1 call)
        let (source2, _, clarity_version2, _) =
            retrieve_contract(&contract_id, cache_dir.path(), &None, Some(&server.url()))
                .await
                .unwrap();

        assert_eq!(source2, TEST_SOURCE);
        assert_eq!(clarity_version2, ClarityVersion::Clarity3);
    }

    #[tokio::test]
    async fn test_detected_contract_failure_is_remembered() {
        let mut server = Server::new_async().await;
        let not_found = server
            .mock(
                "GET",
                format!("/extended/v1/contract/{TEST_DEPLOYER}.{TEST_CONTRACT_NAME}").as_str(),
            )
            .with_status(404)
            .expect(1)
            .create_async()
            .await;

        let cache_dir = tempfile::tempdir().unwrap();
        let contract_id =
            QualifiedContractIdentifier::parse(&format!("{TEST_DEPLOYER}.{TEST_CONTRACT_NAME}"))
                .unwrap();
        for _ in 0..2 {
            let result = retrieve_detected_contract(
                &contract_id,
                cache_dir.path(),
                &None,
                Some(&server.url()),
            )
            .await;
            assert!(result.is_err());
        }

        not_found.assert_async().await;
    }
}
