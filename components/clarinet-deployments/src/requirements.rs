use std::path::{Path, PathBuf};

use clarinet_defaults::{DEFAULT_CLARITY_VERSION, DEFAULT_EPOCH};
use clarinet_files::{paths, FileAccessor};
use clarity_repl::clarity::chainstate::StacksAddress;
use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
use clarity_repl::clarity::{Address, ClarityVersion, StacksEpochId};
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
                .read_file(contract_location.to_string_lossy().to_string())
                .await,
            file_accessor
                .read_file(metadata_location.to_string_lossy().to_string())
                .await,
        ),
    };

    if let (Ok(contract_source), Ok(metadata_json)) = (contract_source, metadata_json) {
        let metadata: ContractMetadata = serde_json::from_str(&metadata_json)
            .map_err(|e| format!("Unable to parse metadata file: {e}"))?;

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
                    contract_location.to_string_lossy().to_string(),
                    contract.source_code.as_bytes(),
                )
                .await?;
            file_accessor
                .write_file(
                    metadata_location.to_string_lossy().to_string(),
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
    let response = reqwest::get(&url)
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
}
