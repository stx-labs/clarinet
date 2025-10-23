use clarinet_files::{FileAccessor, FileLocation};
use clarity_repl::clarity::chainstate::StacksAddress;
use clarity_repl::clarity::vm::types::QualifiedContractIdentifier;
use clarity_repl::clarity::{Address, ClarityVersion, StacksEpochId};
use clarity_repl::repl::remote_data::epoch_for_height;
use clarity_repl::repl::{DEFAULT_CLARITY_VERSION, DEFAULT_EPOCH};
use reqwest;

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
    cache_location: &FileLocation,
    file_accessor: &Option<&dyn FileAccessor>,
) -> Result<(String, StacksEpochId, ClarityVersion, FileLocation), String> {
    let contract_deployer = contract_id.issuer.to_address();
    let contract_name = contract_id.name.to_string();

    let mut contract_location = cache_location.clone();
    contract_location.append_path("requirements")?;
    let mut metadata_location = contract_location.clone();
    contract_location.append_path(&format!("{contract_deployer}.{contract_name}.clar"))?;
    metadata_location.append_path(&format!("{contract_deployer}.{contract_name}.json"))?;

    let (contract_source, metadata_json) = match file_accessor {
        None => (
            contract_location.read_content_as_utf8(),
            metadata_location.read_content_as_utf8(),
        ),
        Some(file_accessor) => (
            file_accessor.read_file(contract_location.to_string()).await,
            file_accessor.read_file(metadata_location.to_string()).await,
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
    let stacks_node_addr = if is_mainnet {
        "https://api.hiro.so".to_string()
    } else {
        "https://api.testnet.hiro.so".to_string()
    };

    let request_url = format!(
        "{stacks_node_addr}/v2/contracts/source/{contract_deployer}/{contract_name}?proof=0"
    );

    let contract = fetch_contract(request_url).await?;
    let epoch = epoch_for_height(is_mainnet, contract.publish_height);
    let clarity_version = match contract.clarity_version {
        Some(1) => ClarityVersion::Clarity1,
        Some(2) => ClarityVersion::Clarity2,
        Some(3) => ClarityVersion::Clarity3,
        Some(4) => ClarityVersion::Clarity4,
        Some(_) => {
            return Err("unable to parse clarity_version (can either be '1' or '2'".to_string())
        }
        None => ClarityVersion::default_for_epoch(epoch),
    };

    match file_accessor {
        None => {
            contract_location.write_content(contract.source.as_bytes())?;
            metadata_location.write_content(
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
                .write_file(contract_location.to_string(), contract.source.as_bytes())
                .await?;
            file_accessor
                .write_file(
                    metadata_location.to_string(),
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

    Ok((contract.source, epoch, clarity_version, contract_location))
}

#[allow(dead_code)]
#[derive(Deserialize, Debug, Default, Clone)]
struct Contract {
    source: String,
    publish_height: u32,
    clarity_version: Option<u8>,
}

async fn fetch_contract(request_url: String) -> Result<Contract, String> {
    let response = reqwest::get(&request_url)
        .await
        .map_err(|e| format!("Unable to retrieve contract {request_url}: {e}"))?;

    let status = response.status();
    if !status.is_success() {
        return Err(format!(
            "Unable to retrieve contract {request_url}: {status}"
        ));
    }

    response
        .json()
        .await
        .map_err(|e| format!("Unable to parse contract json data {request_url}: {e}"))
}
