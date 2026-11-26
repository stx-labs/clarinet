use std::collections::{BTreeMap, VecDeque};
use std::sync::mpsc::{Receiver, Sender};

use bitcoincore_rpc::{Auth, Client};
use clarinet_files::{AccountConfig, DevnetConfig, NetworkManifest, StacksNetwork};
use clarinet_utils::get_bip32_keys_from_mnemonic;
use clarity::codec::StacksMessageCodec;
use clarity::types::chainstate::StacksAddress;
use clarity::types::StacksEpochId;
use clarity::util::secp256k1::{MessageSignature, Secp256k1PrivateKey, Secp256k1PublicKey};
use clarity::vm::types::{PrincipalData, QualifiedContractIdentifier, StandardPrincipalData};
use clarity::vm::{ClarityName, ClarityVersion, ContractName, EvaluationResult, Value};
use clarity_repl::repl::boot::{
    remap_mainnet_boot_principals, SBTC_CONTRACTS_NAMES, SBTC_MAINNET_ADDRESS, SBTC_TESTNET_ADDRESS,
};
use clarity_repl::repl::{is_deployable_clarity_version, Session, SessionSettings};
use reqwest::Url;
use stacks_codec::strings::StacksString;
use stacks_codec::transaction::{
    SinglesigHashMode, SinglesigSpendingCondition, StacksTransaction, TokenTransferMemo,
    TransactionAnchorMode, TransactionAuth, TransactionContractCall, TransactionPayload,
    TransactionPostConditionMode, TransactionPublicKeyEncoding, TransactionSmartContract,
    TransactionSpendingCondition, TransactionVersion,
};
use stacks_rpc_client::crypto::compute_stacks_address;
use stacks_rpc_client::StacksRpc;
use strum::IntoEnumIterator;

mod bitcoin_deployment;

use crate::source_remap::{qualify_remapped_relative_references, remap_contract_ids};
use crate::types::{
    planned_batch_epoch, remap_principal_pairs, ContractRemaps, DeploymentSpecification, EpochSpec,
    RequirementPublishSpecification, TransactionSpecification,
};

/// Contract-ID remappings keyed by the identifier they replace, so a later
/// requirement transaction overrides a seeded pair instead of competing with it.
type ContractIdsToRemap = BTreeMap<String, String>;

/// The sBTC contract-ID remappings a legacy testnet plan starts with.
///
/// Plans now carry their remaps in `contract-remaps`; this covers plans
/// written before that section existed. Devnet's sBTC mappings are added
/// later from its requirement transactions, and the mainnet boot principals
/// are handled separately by [`remap_deployment_source`].
fn sbtc_contract_ids_to_remap(network: &StacksNetwork) -> ContractIdsToRemap {
    if !matches!(network, StacksNetwork::Testnet) {
        return ContractIdsToRemap::new();
    }

    SBTC_CONTRACTS_NAMES
        .iter()
        .map(|contract_name| {
            (
                format!("{SBTC_MAINNET_ADDRESS}.{contract_name}"),
                format!("{SBTC_TESTNET_ADDRESS}.{contract_name}"),
            )
        })
        .collect()
}

/// The remappings a deployment starts with: the plan's own, or the legacy
/// sBTC ones for a plan that records none.
fn initial_contract_ids_to_remap(deployment: &DeploymentSpecification) -> ContractIdsToRemap {
    match &deployment.contract_remaps {
        Some(remaps) => remaps.pairs().collect(),
        None => sbtc_contract_ids_to_remap(&deployment.network),
    }
}

/// Rewrite the contract references a non-mainnet deployment needs.
///
/// Explicit contract mappings take precedence over the boot-principal fallback.
fn remap_deployment_source<S: AsRef<str>>(
    source: &str,
    pairs: impl IntoIterator<Item = (S, S)>,
) -> String {
    let remapped = remap_contract_ids(source, pairs);
    remap_mainnet_boot_principals(&remapped).unwrap_or(remapped)
}

/// A requirement's source as published under the deployer. Relative
/// references to remapped contracts are qualified first, so the rewrite
/// reaches them instead of leaving them to resolve to the deployer.
fn requirement_publish_source(
    tx: &RequirementPublishSpecification,
    remaps: Option<&ContractRemaps>,
    contracts_ids_to_remap: &ContractIdsToRemap,
) -> String {
    let qualified = remaps.and_then(|remaps| {
        qualify_remapped_relative_references(&tx.source, &tx.contract_id, remaps)
    });
    remap_deployment_source(
        qualified.as_deref().unwrap_or(&tx.source),
        remap_principal_pairs(&tx.remap_principals).chain(
            contracts_ids_to_remap
                .iter()
                .map(|(from, to)| (from.clone(), to.clone())),
        ),
    )
}

fn get_btc_secret_key(account: &AccountConfig) -> bitcoincore_rpc::bitcoin::secp256k1::SecretKey {
    use bitcoincore_rpc::bitcoin::secp256k1::SecretKey;
    let keys = get_bip32_keys_from_mnemonic(&account.mnemonic, &account.derivation).unwrap();
    SecretKey::from_slice(&keys.secret_key).unwrap()
}

fn get_keypair(account: &AccountConfig) -> (Secp256k1PrivateKey, Secp256k1PublicKey) {
    let keys = get_bip32_keys_from_mnemonic(&account.mnemonic, &account.derivation).unwrap();
    (
        Secp256k1PrivateKey::from_slice(&keys.secret_key).unwrap(),
        Secp256k1PublicKey::from_slice(&keys.public_key).unwrap(),
    )
}

fn anchor_mode(anchor_block_only: bool) -> TransactionAnchorMode {
    if anchor_block_only {
        TransactionAnchorMode::OnChainOnly
    } else {
        TransactionAnchorMode::Any
    }
}

fn sign_transaction_payload(
    account: &AccountConfig,
    payload: TransactionPayload,
    nonce: u64,
    tx_fee: u64,
    anchor_mode: TransactionAnchorMode,
    network: &StacksNetwork,
) -> Result<StacksTransaction, String> {
    let (secret_key, public_key) = get_keypair(account);
    let signer_addr =
        compute_stacks_address(&public_key, matches!(network, StacksNetwork::Mainnet));

    let spending_condition = TransactionSpendingCondition::Singlesig(SinglesigSpendingCondition {
        signer: signer_addr.bytes().clone(),
        nonce,
        tx_fee,
        hash_mode: SinglesigHashMode::P2PKH,
        key_encoding: TransactionPublicKeyEncoding::Compressed,
        signature: MessageSignature::empty(),
    });

    let auth = TransactionAuth::Standard(spending_condition);
    let unsigned_tx = StacksTransaction {
        version: match network {
            StacksNetwork::Mainnet => TransactionVersion::Mainnet,
            _ => TransactionVersion::Testnet,
        },
        chain_id: match network {
            StacksNetwork::Mainnet => 0x00000001,
            _ => 0x80000000,
        },
        auth,
        anchor_mode,
        post_condition_mode: TransactionPostConditionMode::Allow,
        post_conditions: vec![],
        payload,
    };

    let mut unsigned_tx_bytes = vec![];
    unsigned_tx
        .consensus_serialize(&mut unsigned_tx_bytes)
        .expect("FATAL: invalid transaction");

    let mut signed_tx = unsigned_tx;
    let sighash = signed_tx.sign_begin();
    signed_tx.sign_next_origin(&sighash, &secret_key).unwrap();
    Ok(signed_tx)
}

fn encode_contract_call(
    contract_id: &QualifiedContractIdentifier,
    function_name: ClarityName,
    function_args: Vec<Value>,
    account: &AccountConfig,
    nonce: u64,
    tx_fee: u64,
    anchor_mode: TransactionAnchorMode,
    network: &StacksNetwork,
) -> Result<StacksTransaction, String> {
    let payload = TransactionContractCall {
        contract_name: contract_id.name.clone(),
        address: StacksAddress::from(contract_id.issuer.clone()),
        function_name,
        function_args,
    };
    sign_transaction_payload(
        account,
        TransactionPayload::ContractCall(payload),
        nonce,
        tx_fee,
        anchor_mode,
        network,
    )
}

fn encode_stx_transfer(
    recipient: PrincipalData,
    amount: u64,
    memo: [u8; 34],
    account: &AccountConfig,
    nonce: u64,
    tx_fee: u64,
    anchor_mode: TransactionAnchorMode,
    network: &StacksNetwork,
) -> Result<StacksTransaction, String> {
    let payload = TransactionPayload::TokenTransfer(recipient, amount, TokenTransferMemo(memo));
    sign_transaction_payload(account, payload, nonce, tx_fee, anchor_mode, network)
}

fn encode_contract_publish(
    contract_name: &ContractName,
    source: &str,
    clarity_version: Option<ClarityVersion>,
    account: &AccountConfig,
    nonce: u64,
    tx_fee: u64,
    anchor_mode: TransactionAnchorMode,
    network: &StacksNetwork,
) -> Result<StacksTransaction, String> {
    let payload = TransactionSmartContract {
        name: contract_name.clone(),
        code_body: StacksString::from_str(source).unwrap(),
    };
    sign_transaction_payload(
        account,
        TransactionPayload::SmartContract(payload, clarity_version),
        nonce,
        tx_fee,
        anchor_mode,
        network,
    )
}

#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug)]
pub enum TransactionStatus {
    Queued,
    Encoded(StacksTransaction, TransactionCheck),
    Broadcasted(TransactionCheck, String),
    Confirmed,
    Error(String),
}

#[derive(Clone, Debug)]
pub struct TransactionTracker {
    pub index: usize,
    pub name: String,
    pub status: TransactionStatus,
}

#[derive(Clone, Debug)]
pub enum TransactionCheck {
    NonceCheck(StandardPrincipalData, u64),
    ContractPublish(StandardPrincipalData, ContractName),
    BtcTransfer,
}

#[allow(clippy::large_enum_variant)]
#[derive(Clone, Debug)]
pub enum DeploymentEvent {
    TransactionUpdate(TransactionTracker),
    Interrupted(String),
    DeploymentCompleted,
}

pub enum DeploymentCommand {
    Start,
}

fn publish_clarity_version(
    version: ClarityVersion,
    epoch: StacksEpochId,
) -> Result<Option<ClarityVersion>, String> {
    if !is_deployable_clarity_version(version, epoch) {
        let epoch_default = ClarityVersion::default_for_epoch(epoch);
        return Err(format!(
            "cannot publish {version} in epoch {epoch}; the target uses {epoch_default}. \
             Test the contract under the target Clarity version, update the manifest, and regenerate the deployment plan"
        ));
    }
    // Before 2.1 there is no versioned payload; later epochs may reject one.
    Ok(
        (epoch >= StacksEpochId::Epoch21 && !epoch.rejects_versioned_smart_contracts())
            .then_some(version),
    )
}

fn public_network_epoch(
    deployment: &DeploymentSpecification,
    stacks_rpc: &StacksRpc,
) -> Result<Option<StacksEpochId>, String> {
    let has_publishes = deployment.plan.batches.iter().any(|batch| {
        batch.transactions.iter().any(|tx| {
            matches!(
                tx,
                TransactionSpecification::ContractPublish(_)
                    | TransactionSpecification::RequirementPublish(_)
            )
        })
    });
    if has_publishes
        && matches!(
            deployment.network,
            StacksNetwork::Mainnet | StacksNetwork::Testnet
        )
    {
        stacks_rpc
            .get_pox_info()
            .map(|pox| Some(pox.current_epoch))
            .map_err(|e| format!("unable to determine target network epoch: {e}"))
    } else {
        Ok(None)
    }
}

fn target_epoch(
    network: &StacksNetwork,
    batch_epoch: Option<EpochSpec>,
    live_epoch: Option<StacksEpochId>,
) -> StacksEpochId {
    match (network, live_epoch) {
        (StacksNetwork::Devnet, Some(active)) => {
            planned_batch_epoch(batch_epoch, network).max(active)
        }
        (_, Some(active)) => active,
        (_, None) => planned_batch_epoch(batch_epoch, network),
    }
}

fn devnet_epoch(config: &DevnetConfig, height: u64) -> StacksEpochId {
    EpochSpec::iter()
        .filter(|epoch| {
            epoch
                .start_height(config)
                .is_some_and(|start| start <= height)
        })
        .map(StacksEpochId::from)
        .max()
        .unwrap_or(StacksEpochId::Epoch20)
}

pub fn update_deployment_costs(
    deployment: &mut DeploymentSpecification,
    priority: usize,
) -> Result<(), String> {
    let stacks_node_url = deployment
        .stacks_node
        .as_ref()
        .expect("unable to get stacks node rcp address");
    let stacks_rpc = StacksRpc::new(stacks_node_url);
    let mut session = Session::new(SessionSettings::default());
    let live_epoch = public_network_epoch(deployment, &stacks_rpc)?;

    for batch_spec in deployment.plan.batches.iter_mut() {
        let epoch = target_epoch(&deployment.network, batch_spec.epoch, live_epoch);
        for transaction in batch_spec.transactions.iter_mut() {
            match transaction {
                TransactionSpecification::StxTransfer(tx) => {
                    let transaction_payload = TransactionPayload::TokenTransfer(
                        tx.recipient.clone(),
                        tx.mstx_amount,
                        TokenTransferMemo(tx.memo),
                    );

                    match stacks_rpc.estimate_transaction_fee(&transaction_payload, priority) {
                        Ok(fee) => {
                            tx.cost = fee;
                        }
                        Err(e) => {
                            println!("unable to estimate fee for transaction: {e}");
                            continue;
                        }
                    };
                }
                TransactionSpecification::ContractCall(tx) => {
                    let function_args = tx
                        .parameters
                        .iter()
                        .map(|value| {
                            let annotated = session.eval(value.to_string(), false).unwrap();
                            match annotated.into_inner().result {
                                EvaluationResult::Snippet(result) => result.result,
                                _ => unreachable!("Contract result from snippet"),
                            }
                        })
                        .collect::<Vec<_>>();

                    let transaction_payload =
                        TransactionPayload::ContractCall(TransactionContractCall {
                            contract_name: tx.contract_id.name.clone(),
                            address: StacksAddress::from(tx.contract_id.issuer.clone()),
                            function_name: tx.method.clone(),
                            function_args,
                        });

                    match stacks_rpc.estimate_transaction_fee(&transaction_payload, priority) {
                        Ok(fee) => {
                            tx.cost = fee;
                        }
                        Err(e) => {
                            println!("unable to estimate fee for transaction: {e}");
                            continue;
                        }
                    };
                }
                TransactionSpecification::ContractPublish(tx) => {
                    let transaction_payload = TransactionPayload::SmartContract(
                        TransactionSmartContract {
                            name: tx.contract_name.clone(),
                            code_body: StacksString::from_str(&tx.source).unwrap(),
                        },
                        publish_clarity_version(tx.clarity_version, epoch)
                            .map_err(|e| format!("{}: {e}", tx.contract_name))?,
                    );

                    match stacks_rpc.estimate_transaction_fee(&transaction_payload, priority) {
                        Ok(fee) => {
                            tx.cost = fee;
                        }
                        Err(e) => {
                            println!("unable to estimate fee for transaction: {e}");
                            continue;
                        }
                    };
                }
                TransactionSpecification::RequirementPublish(_)
                | TransactionSpecification::BtcTransfer(_)
                | TransactionSpecification::EmulatedContractPublish(_)
                | TransactionSpecification::EmulatedContractCall(_) => continue,
            };
        }
    }
    Ok(())
}

/// One second apart. Devnet's RPC can lag the first block events by a few seconds.
const NODE_RPC_RETRIES: usize = 30;

/// The node has already answered, so these only absorb a transient blip.
const NODE_READ_RETRIES: usize = 3;

fn wait_for_node(stacks_rpc: &StacksRpc) -> Result<(), String> {
    stacks_rpc
        .call_with_retry(|rpc| rpc.get_info(), NODE_RPC_RETRIES)
        .map(|_| ())
        .map_err(|e| format!("unable to reach the stacks node at {}: {e}", stacks_rpc.url))
}

/// Read from the node first, so accounts that already transacted in a snapshot
/// start from their real nonce.
fn next_nonce(
    cached_nonces: &mut BTreeMap<String, u64>,
    stacks_rpc: &StacksRpc,
    address: &str,
) -> Result<u64, String> {
    let nonce = match cached_nonces.get(address) {
        Some(nonce) => *nonce,
        None => stacks_rpc
            .call_with_retry(|rpc| rpc.get_nonce(address), NODE_READ_RETRIES)
            .map_err(|e| format!("unable to retrieve nonce for {address}: {e}"))?,
    };
    cached_nonces.insert(address.to_string(), nonce + 1);
    Ok(nonce)
}

pub fn is_contract_published(
    stacks_rpc: &StacksRpc,
    deployer: &str,
    contract_name: &str,
) -> Result<bool, String> {
    stacks_rpc
        .call_with_retry(
            |rpc| rpc.get_contract_source(deployer, contract_name),
            NODE_READ_RETRIES,
        )
        .map(|contract| contract.is_some())
        .map_err(|e| {
            format!("unable to check whether {deployer}.{contract_name} is published: {e}")
        })
}

/// `get_initial_transactions_trackers` must skip exactly these too: the dashboard
/// pairs its rows with `TransactionTracker::index`.
fn is_emulated(transaction: &TransactionSpecification) -> bool {
    matches!(
        transaction,
        TransactionSpecification::EmulatedContractPublish(_)
            | TransactionSpecification::EmulatedContractCall(_)
    )
}

fn encode_transactions(
    deployment: &DeploymentSpecification,
    network_manifest: &NetworkManifest,
    stacks_rpc: &StacksRpc,
    bitcoin_node_url: &str,
    deployment_event_tx: &Sender<DeploymentEvent>,
) -> Result<VecDeque<(EpochSpec, Vec<TransactionTracker>)>, String> {
    wait_for_node(stacks_rpc)?;
    let devnet = match deployment.network {
        StacksNetwork::Devnet => Some(
            network_manifest
                .devnet
                .as_ref()
                .ok_or("missing devnet epoch schedule")?,
        ),
        _ => None,
    };
    let live_epoch = match devnet {
        Some(config) => {
            let height = stacks_rpc
                .get_info()
                .map_err(|e| format!("unable to read devnet burn height: {e}"))?
                .burn_block_height;
            Some(devnet_epoch(config, height))
        }
        None => public_network_epoch(deployment, stacks_rpc)?,
    };

    // Preflight all publishes before reading nonces, signing, or sending BTC.
    let mut published_requirements = VecDeque::new();
    for batch in &deployment.plan.batches {
        if let (Some(config), Some(epoch)) = (devnet, batch.epoch) {
            if epoch.start_height(config).is_none() {
                return Err(format!("devnet epoch {epoch:?} is not configured"));
            }
        }
        let epoch = target_epoch(&deployment.network, batch.epoch, live_epoch);
        for tx in &batch.transactions {
            match tx {
                TransactionSpecification::ContractPublish(tx) => {
                    publish_clarity_version(tx.clarity_version, epoch)
                        .map_err(|e| format!("{}: {e}", tx.contract_name))?;
                }
                TransactionSpecification::RequirementPublish(tx) => {
                    if matches!(deployment.network, StacksNetwork::Mainnet) {
                        panic!("Deployment specification malformed - requirements publish not supported on mainnet");
                    }
                    let published = is_contract_published(
                        stacks_rpc,
                        &tx.remap_sender.to_address(),
                        &tx.contract_id.name,
                    )?;
                    if !published {
                        publish_clarity_version(tx.clarity_version, epoch)
                            .map_err(|e| format!("requirement {}: {e}", tx.contract_id))?;
                    }
                    published_requirements.push_back(published);
                }
                _ => {}
            }
        }
    }

    let network = &deployment.network;
    let stx_accounts_lookup: BTreeMap<&str, &AccountConfig> = network_manifest
        .accounts
        .values()
        .map(|account| (account.stx_address.as_str(), account))
        .collect();
    let btc_accounts_lookup: BTreeMap<&str, &AccountConfig> = network_manifest
        .accounts
        .values()
        .map(|account| (account.btc_address.as_str(), account))
        .collect();
    let mut cached_nonces = BTreeMap::new();
    // Only needed to coerce contract-call arguments, and costly to build.
    let mut session: Option<Session> = None;
    let mut contracts_ids_to_remap = initial_contract_ids_to_remap(deployment);
    let mut batches = VecDeque::new();
    let mut next_index = 0;

    for batch_spec in deployment.plan.batches.iter() {
        let epoch: EpochSpec = target_epoch(network, batch_spec.epoch, live_epoch).into();
        let mut batch = Vec::new();
        for transaction in batch_spec.transactions.iter() {
            if is_emulated(transaction) {
                continue;
            }
            let index = next_index;
            next_index += 1;

            let tracker = match transaction {
                TransactionSpecification::StxTransfer(tx) => {
                    let issuer_address = tx.expected_sender.to_address();
                    let nonce = next_nonce(&mut cached_nonces, stacks_rpc, &issuer_address)?;
                    let account = stx_accounts_lookup.get(issuer_address.as_str()).unwrap();

                    let transaction = encode_stx_transfer(
                        tx.recipient.clone(),
                        tx.mstx_amount,
                        tx.memo,
                        account,
                        nonce,
                        tx.cost,
                        anchor_mode(tx.anchor_block_only),
                        network,
                    )
                    .map_err(|e| format!("unable to encode stx_transfer ({e})"))?;

                    let name = format!(
                        "STX transfer ({}µSTX from {} to {})",
                        tx.mstx_amount, issuer_address, tx.recipient,
                    );
                    let check = TransactionCheck::NonceCheck(tx.expected_sender.clone(), nonce);
                    TransactionTracker {
                        index,
                        name,
                        status: TransactionStatus::Encoded(transaction, check),
                    }
                }
                TransactionSpecification::BtcTransfer(tx) => {
                    let url = Url::parse(bitcoin_node_url).expect("Url malformatted");
                    let auth = match url.password() {
                        Some(password) => {
                            Auth::UserPass(url.username().to_string(), password.to_string())
                        }
                        None => Auth::None,
                    };
                    let bitcoin_node_rpc_url = format!(
                        "{}://{}:{}",
                        url.scheme(),
                        url.host().expect("Host unknown"),
                        url.port_or_known_default().expect("Protocol unknown")
                    );
                    let bitcoin_rpc = Client::new(&bitcoin_node_rpc_url, auth.clone()).unwrap();

                    let bitcoin_node_wallet_rpc_url = match &network_manifest.devnet {
                        Some(devnet) => {
                            format!("{bitcoin_node_rpc_url}/wallet/{}", devnet.miner_wallet_name)
                        }
                        None => format!("{bitcoin_node_rpc_url}/wallet/"),
                    };
                    let bitcoin_node_wallet_rpc =
                        Client::new(&bitcoin_node_wallet_rpc_url, auth).unwrap();

                    let account = btc_accounts_lookup
                        .get(tx.expected_sender.as_str())
                        .unwrap();
                    let secret_key = get_btc_secret_key(account);
                    let _ = bitcoin_deployment::send_transaction_spec(
                        &bitcoin_rpc,
                        &bitcoin_node_wallet_rpc,
                        tx,
                        &secret_key,
                    );
                    continue;
                }
                TransactionSpecification::ContractCall(tx) => {
                    let issuer_address = tx.expected_sender.to_address();
                    let nonce = next_nonce(&mut cached_nonces, stacks_rpc, &issuer_address)?;
                    let account = stx_accounts_lookup.get(issuer_address.as_str()).unwrap();

                    let session =
                        session.get_or_insert_with(|| Session::new(SessionSettings::default()));
                    let function_args = tx
                        .parameters
                        .iter()
                        .map(|value| {
                            let execution =
                                session.eval(value.to_string(), false).map_err(|_| {
                                    format!(
                                    "unable to process contract-call {}::{}: argument {} invalid",
                                    tx.contract_id, tx.method, value
                                )
                                })?;
                            match execution.into_inner().result {
                                EvaluationResult::Snippet(result) => Ok(result.result),
                                _ => unreachable!("Contract result from snippet"),
                            }
                        })
                        .collect::<Result<Vec<_>, String>>()?;

                    let transaction = encode_contract_call(
                        &tx.contract_id,
                        tx.method.clone(),
                        function_args,
                        account,
                        nonce,
                        tx.cost,
                        anchor_mode(tx.anchor_block_only),
                        network,
                    )
                    .map_err(|e| {
                        format!(
                            "unable to encode contract_call {}::{} ({})",
                            tx.contract_id, tx.method, e
                        )
                    })?;

                    let name = format!(
                        "Call ({} {} {})",
                        tx.contract_id,
                        tx.method,
                        tx.parameters.join(" ")
                    );
                    let check = TransactionCheck::NonceCheck(tx.expected_sender.clone(), nonce);
                    TransactionTracker {
                        index,
                        name,
                        status: TransactionStatus::Encoded(transaction, check),
                    }
                }
                TransactionSpecification::ContractPublish(tx) => {
                    let issuer_address = tx.expected_sender.to_address();
                    let nonce = next_nonce(&mut cached_nonces, stacks_rpc, &issuer_address)?;
                    let account = stx_accounts_lookup.get(issuer_address.as_str()).unwrap();
                    // Relative references (`.name`) in project contracts are not
                    // remapped, by design: they always resolve to the project's
                    // deployer. Only requirements get their relative references
                    // qualified first (see `requirement_publish_source`).
                    let source = match network {
                        StacksNetwork::Devnet | StacksNetwork::Testnet => {
                            remap_deployment_source(&tx.source, contracts_ids_to_remap.iter())
                        }
                        // Mainnet boot principals are already correct; only the
                        // plan's own remaps apply.
                        StacksNetwork::Mainnet => {
                            remap_contract_ids(&tx.source, contracts_ids_to_remap.iter())
                        }
                        StacksNetwork::Simnet => tx.source.clone(),
                    };

                    let clarity_version = publish_clarity_version(tx.clarity_version, epoch.into())
                        .map_err(|e| format!("{}: {e}", tx.contract_name))?;

                    let transaction = encode_contract_publish(
                        &tx.contract_name,
                        &source,
                        clarity_version,
                        account,
                        nonce,
                        tx.cost,
                        anchor_mode(tx.anchor_block_only),
                        network,
                    )
                    .map_err(|e| {
                        format!(
                            "unable to encode contract_publish {} ({})",
                            tx.contract_name, e
                        )
                    })?;

                    let name = format!("Publish {}.{}", tx.expected_sender, tx.contract_name);
                    let check = TransactionCheck::ContractPublish(
                        tx.expected_sender.clone(),
                        tx.contract_name.clone(),
                    );
                    TransactionTracker {
                        index,
                        name,
                        status: TransactionStatus::Encoded(transaction, check),
                    }
                }
                TransactionSpecification::RequirementPublish(tx) => {
                    let old_contract_id = tx.contract_id.to_string();
                    let new_contract_id = QualifiedContractIdentifier::new(
                        tx.remap_sender.clone(),
                        tx.contract_id.name.clone(),
                    )
                    .to_string();
                    contracts_ids_to_remap.insert(old_contract_id, new_contract_id);

                    let issuer_address = tx.remap_sender.to_address();
                    if published_requirements
                        .pop_front()
                        .expect("every requirement was preflighted in plan order")
                    {
                        continue;
                    }
                    let clarity_version = publish_clarity_version(tx.clarity_version, epoch.into())
                        .map_err(|e| format!("requirement {}: {e}", tx.contract_id))?;

                    let nonce = next_nonce(&mut cached_nonces, stacks_rpc, &issuer_address)?;
                    let account = stx_accounts_lookup.get(issuer_address.as_str()).unwrap();

                    let source = requirement_publish_source(
                        tx,
                        deployment.contract_remaps.as_ref(),
                        &contracts_ids_to_remap,
                    );

                    let transaction = encode_contract_publish(
                        &tx.contract_id.name,
                        &source,
                        clarity_version,
                        account,
                        nonce,
                        tx.cost,
                        TransactionAnchorMode::OnChainOnly,
                        network,
                    )?;

                    let name = format!("Publish {}.{}", tx.remap_sender, tx.contract_id.name);
                    let check = TransactionCheck::ContractPublish(
                        tx.remap_sender.clone(),
                        tx.contract_id.name.clone(),
                    );
                    TransactionTracker {
                        index,
                        name,
                        status: TransactionStatus::Encoded(transaction, check),
                    }
                }
                TransactionSpecification::EmulatedContractPublish(_)
                | TransactionSpecification::EmulatedContractCall(_) => unreachable!(),
            };

            batch.push(tracker.clone());
            let _ = deployment_event_tx.send(DeploymentEvent::TransactionUpdate(tracker));
        }

        batches.push_back((epoch, batch));
    }

    Ok(batches)
}

pub fn apply_on_chain_deployment(
    network_manifest: NetworkManifest,
    deployment: DeploymentSpecification,
    deployment_event_tx: Sender<DeploymentEvent>,
    deployment_command_rx: Receiver<DeploymentCommand>,
    override_bitcoin_rpc_url: Option<String>,
    override_stacks_rpc_url: Option<String>,
) {
    let delay_between_checks: u64 = if matches!(deployment.network, StacksNetwork::Devnet) {
        1
    } else {
        10
    };

    let stacks_node_url = override_stacks_rpc_url
        .or_else(|| deployment.stacks_node.clone())
        .expect("unable to get stacks node rcp address");

    let stacks_rpc = StacksRpc::new(&stacks_node_url);

    let bitcoin_node_url = override_bitcoin_rpc_url
        .or_else(|| deployment.bitcoin_node.clone())
        .expect("unable to get bitcoin node rcp address");

    // Encoding reads from the node, so it cannot start before the node is up.
    let Ok(_cmd) = deployment_command_rx.recv() else {
        let _ = deployment_event_tx.send(DeploymentEvent::Interrupted(
            "deployment aborted - broken channel".to_string(),
        ));
        return;
    };

    let batches = match encode_transactions(
        &deployment,
        &network_manifest,
        &stacks_rpc,
        &bitcoin_node_url,
        &deployment_event_tx,
    ) {
        Ok(batches) => batches,
        Err(e) => {
            let _ = deployment_event_tx.send(DeploymentEvent::Interrupted(e));
            return;
        }
    };

    // Phase 2: we submit all the transactions previously encoded,
    // and wait for their inclusion in a block before moving to the next batch.
    let mut current_block_height = 0;
    let mut current_bitcoin_block_height = 0;
    for (epoch, batch) in batches.into_iter() {
        if deployment.network == StacksNetwork::Devnet {
            // Devnet only: ensure we've reached the appropriate epoch for this batch
            let devnet = network_manifest.devnet.as_ref().unwrap();
            let Some(after_bitcoin_block) = epoch.start_height(devnet) else {
                let _ = deployment_event_tx.send(DeploymentEvent::Interrupted(format!(
                    "devnet epoch {epoch:?} is not configured"
                )));
                return;
            };
            let mut epoch_transition_successful =
                current_bitcoin_block_height > after_bitcoin_block;

            while !epoch_transition_successful {
                let (bitcoin_block_tip, stacks_block_tip) = match stacks_rpc.get_info() {
                    Ok(info) => {
                        if info.stacks_tip_height == 0 {
                            // Always loop if we have not yet seen the genesis block.
                            std::thread::sleep(std::time::Duration::from_secs(
                                delay_between_checks,
                            ));
                            continue;
                        }
                        (info.burn_block_height, info.stacks_tip_height)
                    }
                    Err(_e) => {
                        std::thread::sleep(std::time::Duration::from_secs(delay_between_checks));
                        continue;
                    }
                };

                // If no bitcoin block has been mined since `delay_between_checks`,
                // avoid flooding the stacks-node with status update requests.
                if bitcoin_block_tip <= current_bitcoin_block_height {
                    std::thread::sleep(std::time::Duration::from_secs(delay_between_checks));
                    continue;
                }

                current_bitcoin_block_height = bitcoin_block_tip;

                // If no stacks block has been mined despite the new bitcoin block,
                // avoid flooding the stacks-node with status update requests.
                if stacks_block_tip <= current_block_height {
                    std::thread::sleep(std::time::Duration::from_secs(delay_between_checks));
                    continue;
                }

                current_block_height = stacks_block_tip;

                if current_bitcoin_block_height > after_bitcoin_block {
                    epoch_transition_successful = true;
                } else {
                    std::thread::sleep(std::time::Duration::from_secs(delay_between_checks));
                }
            }

            let height = match stacks_rpc.get_info() {
                Ok(info) => info.burn_block_height,
                Err(e) => {
                    let _ = deployment_event_tx.send(DeploymentEvent::Interrupted(format!(
                        "unable to determine devnet epoch before broadcasting: {e}"
                    )));
                    return;
                }
            };
            let active = devnet_epoch(devnet, height);
            for tracker in &batch {
                if let TransactionStatus::Encoded(transaction, _) = &tracker.status {
                    if let TransactionPayload::SmartContract(_, wire_version) = &transaction.payload
                    {
                        let version = wire_version
                            .unwrap_or_else(|| ClarityVersion::default_for_epoch(epoch.into()));
                        if publish_clarity_version(version, active) != Ok(*wire_version) {
                            let _ = deployment_event_tx.send(DeploymentEvent::Interrupted(
                                format!("devnet advanced to epoch {active}; regenerate the deployment plan before publishing"),
                            ));
                            return;
                        }
                    }
                }
            }
        }
        // Activation can happen while waiting for an earlier batch to confirm.
        match public_network_epoch(&deployment, &stacks_rpc) {
            Ok(Some(active)) if active != StacksEpochId::from(epoch) => {
                let _ = deployment_event_tx.send(DeploymentEvent::Interrupted(
                    "target network epoch changed; regenerate the deployment plan before publishing".into(),
                ));
                return;
            }
            Err(e) => {
                let _ = deployment_event_tx.send(DeploymentEvent::Interrupted(e));
                return;
            }
            _ => {}
        }

        let mut ongoing_batch = BTreeMap::new();
        for mut tracker in batch.into_iter() {
            let TransactionStatus::Encoded(transaction, check) = tracker.status else {
                unreachable!();
            };
            match stacks_rpc.post_transaction(&transaction) {
                Ok(res) => {
                    tracker.status = TransactionStatus::Broadcasted(check, res.txid.clone());

                    let _ = deployment_event_tx
                        .send(DeploymentEvent::TransactionUpdate(tracker.clone()));
                    ongoing_batch.insert(res.txid, tracker);
                }
                Err(e) => {
                    let message = format!("unable to post transaction\n{e}");
                    tracker.status = TransactionStatus::Error(message.clone());

                    let _ = deployment_event_tx
                        .send(DeploymentEvent::TransactionUpdate(tracker.clone()));
                    let _ = deployment_event_tx.send(DeploymentEvent::Interrupted(message));
                    return;
                }
            };
        }
        let mut last_stacks_chain_check_at_height = 0;
        let mut last_bitcoin_chain_check_at_height = 0;

        loop {
            let (bitcoin_tip_height, stacks_tip_height) = match stacks_rpc.get_info() {
                Ok(info) => (info.burn_block_height, info.stacks_tip_height),
                _ => {
                    std::thread::sleep(std::time::Duration::from_secs(delay_between_checks));
                    continue;
                }
            };

            let mut keep_looping = false;

            // Handle Stacks releated checks
            if stacks_tip_height > last_stacks_chain_check_at_height {
                for tracker in ongoing_batch.values_mut() {
                    let TransactionStatus::Broadcasted(brodcasting_status, _) = &tracker.status
                    else {
                        continue;
                    };

                    match &brodcasting_status {
                        TransactionCheck::ContractPublish(deployer, contract_name) => {
                            let deployer_address = deployer.to_address();
                            match stacks_rpc.get_contract_source(&deployer_address, contract_name) {
                                Ok(Some(_)) => {
                                    tracker.status = TransactionStatus::Confirmed;
                                    let _ = deployment_event_tx
                                        .send(DeploymentEvent::TransactionUpdate(tracker.clone()));
                                }
                                Ok(None) | Err(_) => {
                                    keep_looping = true;
                                    break;
                                }
                            }
                        }
                        TransactionCheck::NonceCheck(tx_sender, expected_nonce) => {
                            let tx_sender_address = tx_sender.to_address();
                            let res = stacks_rpc.get_nonce(&tx_sender_address);
                            if let Ok(current_nonce) = res {
                                if current_nonce.gt(expected_nonce) {
                                    tracker.status = TransactionStatus::Confirmed;
                                    let _ = deployment_event_tx
                                        .send(DeploymentEvent::TransactionUpdate(tracker.clone()));
                                } else {
                                    keep_looping = true;
                                    break;
                                }
                            }
                        }
                        _ => {}
                    }
                }
            } else {
                std::thread::sleep(std::time::Duration::from_secs(delay_between_checks));
                continue;
            }

            // Handle Bitcoin releated checks
            if bitcoin_tip_height > last_bitcoin_chain_check_at_height {
                for tracker in ongoing_batch.values_mut() {
                    let TransactionStatus::Broadcasted(brodcasting_status, _) = &tracker.status
                    else {
                        continue;
                    };
                    match &brodcasting_status {
                        TransactionCheck::BtcTransfer => {
                            // TODO
                        }
                        TransactionCheck::ContractPublish(_, _)
                        | TransactionCheck::NonceCheck(_, _) => {}
                    }
                }
            } else {
                std::thread::sleep(std::time::Duration::from_secs(delay_between_checks));
                continue;
            }

            last_stacks_chain_check_at_height = stacks_tip_height;
            last_bitcoin_chain_check_at_height = bitcoin_tip_height;

            if !keep_looping {
                break;
            }
        }
    }

    let _ = deployment_event_tx.send(DeploymentEvent::DeploymentCompleted);
}

pub fn get_initial_transactions_trackers(
    deployment: &DeploymentSpecification,
) -> Vec<TransactionTracker> {
    deployment
        .plan
        .batches
        .iter()
        .flat_map(|batch_spec| batch_spec.transactions.iter())
        .filter(|transaction| !is_emulated(transaction))
        .enumerate()
        .map(|(index, transaction)| {
            let name = match transaction {
                TransactionSpecification::ContractCall(tx) => {
                    format!("Contract call {}::{}", tx.contract_id, tx.method)
                }
                TransactionSpecification::ContractPublish(tx) => format!(
                    "Contract publish {}.{}",
                    tx.expected_sender.to_address(),
                    tx.contract_name
                ),
                TransactionSpecification::RequirementPublish(tx) => {
                    if !matches!(
                        deployment.network,
                        StacksNetwork::Devnet | StacksNetwork::Testnet
                    ) {
                        panic!("Deployment specification malformed - requirements publish not supported on mainnet");
                    }
                    format!(
                        "Contract publish {}.{}",
                        tx.remap_sender.to_address(),
                        tx.contract_id.name
                    )
                }
                TransactionSpecification::BtcTransfer(tx) => format!(
                    "BTC transfer {} send {} satoshis to {}",
                    tx.expected_sender, tx.sats_amount, tx.recipient
                ),
                TransactionSpecification::StxTransfer(tx) => format!(
                    "STX transfer {} send {} µSTC to {}",
                    tx.expected_sender.to_address(),
                    tx.mstx_amount,
                    tx.recipient,
                ),
                TransactionSpecification::EmulatedContractPublish(_)
                | TransactionSpecification::EmulatedContractCall(_) => unreachable!(),
            };
            TransactionTracker {
                index,
                name,
                status: TransactionStatus::Queued,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use clarity_repl::repl::boot::{BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS};
    use stacks_rpc_client::rpc_client::{NodeInfo, PoxInfo};

    use super::*;
    use crate::types::{ContractPublishSpecification, TransactionsBatchSpecification};

    fn test_network_manifest() -> NetworkManifest {
        let mut file = toml::from_str(&format!(
            "[network]\nname = 'devnet'\n[accounts.deployer]\nmnemonic = '{}'\nbalance = 1000000000\n[devnet]\n",
            clarinet_utils::DEFAULT_DEPLOYER_MNEMONIC,
        )).unwrap();
        NetworkManifest::from_network_manifest_file(
            &mut file,
            &StacksNetwork::Devnet.get_networks(),
            false,
            None,
        )
        .unwrap()
    }

    fn pox_response(epoch: StacksEpochId) -> serde_json::Value {
        serde_json::json!({
            "contract_id": "ST000000000000000000002AMW42H.pox-4",
            "pox_activation_threshold_ustx": 0,
            "first_burnchain_block_height": 0,
            "current_burnchain_block_height": 200,
            "current_epoch": epoch,
            "prepare_phase_block_length": 50,
            "reward_phase_block_length": 100,
            "reward_slots": 1,
            "reward_cycle_id": 1,
            "reward_cycle_length": 150,
            "total_liquid_supply_ustx": 0,
            "current_cycle": {"id": 1, "min_threshold_ustx": 0, "stacked_ustx": 0, "is_pox_active": true},
            "next_cycle": {"min_threshold_ustx": 0, "stacked_ustx": 0, "blocks_until_prepare_phase": 1, "blocks_until_reward_phase": 2},
            "epochs": [
                {"epoch_id": epoch, "start_height": 100, "end_height": 300},
                {"epoch_id": "Epoch42", "start_height": 300, "end_height": 400}
            ]
        })
    }

    #[test]
    fn estimates_and_signs_the_same_epoch_appropriate_publish() {
        for (epoch, version, wire_version) in [
            (StacksEpochId::Epoch20, ClarityVersion::Clarity1, None),
            (
                StacksEpochId::Epoch21,
                ClarityVersion::Clarity2,
                Some(ClarityVersion::Clarity2),
            ),
            (
                StacksEpochId::Epoch40,
                ClarityVersion::Clarity6,
                Some(ClarityVersion::Clarity6),
            ),
            (StacksEpochId::Epoch41, ClarityVersion::Clarity7, None),
        ] {
            let mut server = mockito::Server::new();
            let manifest = test_network_manifest();
            let account = &manifest.accounts["deployer"];
            let name = ContractName::try_from("epoch-test").unwrap();
            let source = "(define-read-only (get-value) u1)";
            let expected = TransactionPayload::SmartContract(
                TransactionSmartContract {
                    name: name.clone(),
                    code_body: StacksString::from_str(source).unwrap(),
                },
                wire_version,
            );
            let expected_hex = clarity::util::hash::to_hex(&expected.serialize_to_vec());
            let fees = server
                .mock("POST", "/v2/fees/transaction")
                .match_request(move |request| {
                    let body: serde_json::Value =
                        serde_json::from_slice(request.body().unwrap()).unwrap();
                    body["transaction_payload"] == expected_hex
                })
                .with_header("content-type", "application/json")
                .with_body(r#"{"estimations":[{"fee":123}]}"#)
                .expect(1)
                .create();
            let pox = server
                .mock("GET", "/v2/pox")
                .with_header("content-type", "application/json")
                .with_body(pox_response(epoch).to_string())
                .expect(2)
                .create();
            let _info = server
                .mock("GET", "/v2/info")
                .with_header("content-type", "application/json")
                .with_body(serde_json::to_string(&NodeInfo::default()).unwrap())
                .create();
            let _nonce = server
                .mock(
                    "GET",
                    format!("/v2/accounts/{}", account.stx_address).as_str(),
                )
                .with_header("content-type", "application/json")
                .with_body(r#"{"balance":"0","nonce":0,"balance_proof":"","nonce_proof":""}"#)
                .create();
            let mut deployment = deployment_with_remaps(StacksNetwork::Testnet, None);
            deployment.stacks_node = Some(server.url());
            // A saved plan cannot choose an epoch older than the live node.
            deployment
                .plan
                .batches
                .push(TransactionsBatchSpecification {
                    id: 0,
                    epoch: Some(EpochSpec::Epoch2_0),
                    transactions: vec![TransactionSpecification::ContractPublish(
                        ContractPublishSpecification {
                            contract_name: name,
                            expected_sender: PrincipalData::parse_standard_principal(
                                &account.stx_address,
                            )
                            .unwrap(),
                            location: "epoch-test.clar".into(),
                            source: source.into(),
                            clarity_version: version,
                            cost: 0,
                            anchor_block_only: true,
                        },
                    )],
                });
            update_deployment_costs(&mut deployment, 0).unwrap();
            let (events, _) = std::sync::mpsc::channel();
            let encoded = encode_transactions(
                &deployment,
                &manifest,
                &StacksRpc::new(&server.url()),
                "http://localhost:18443",
                &events,
            )
            .unwrap();
            let TransactionStatus::Encoded(tx, _) = &encoded[0].1[0].status else {
                panic!("publish was not encoded");
            };
            let decoded =
                StacksTransaction::consensus_deserialize(&mut tx.serialize_to_vec().as_slice())
                    .unwrap();
            assert_eq!(decoded.payload, expected);
            assert_eq!(decoded.get_tx_fee(), 123);
            fees.assert();
            pox.assert();
        }
    }

    #[test]
    fn rejects_old_plan_before_signing_against_epoch_41() {
        let mut server = mockito::Server::new();
        let manifest = test_network_manifest();
        let _info = server
            .mock("GET", "/v2/info")
            .with_header("content-type", "application/json")
            .with_body(serde_json::to_string(&NodeInfo::default()).unwrap())
            .create();
        let _pox = server
            .mock("GET", "/v2/pox")
            .with_header("content-type", "application/json")
            .with_body(pox_response(StacksEpochId::Epoch41).to_string())
            .create();
        let nonce = server
            .mock("GET", mockito::Matcher::Regex("/v2/accounts/.*".into()))
            .expect(0)
            .create();
        let mut deployment = deployment_with_remaps(StacksNetwork::Mainnet, None);
        deployment
            .plan
            .batches
            .push(TransactionsBatchSpecification {
                id: 0,
                epoch: Some(EpochSpec::Epoch4_0),
                transactions: vec![TransactionSpecification::ContractPublish(
                    ContractPublishSpecification {
                        contract_name: ContractName::try_from("old-contract").unwrap(),
                        expected_sender: PrincipalData::parse_standard_principal(TEST_DEPLOYER)
                            .unwrap(),
                        location: "old-contract.clar".into(),
                        source: "(define-read-only (get-value) u1)".into(),
                        clarity_version: ClarityVersion::Clarity6,
                        cost: 0,
                        anchor_block_only: true,
                    },
                )],
            });
        let (events, _) = std::sync::mpsc::channel();
        let error = encode_transactions(
            &deployment,
            &manifest,
            &StacksRpc::new(&server.url()),
            "http://localhost:18443",
            &events,
        )
        .unwrap_err();
        assert!(error.contains("old-contract"), "{error}");
        assert!(error.contains("Clarity 6"), "{error}");
        nonce.assert();
    }

    fn requirement_deployment(
        network: StacksNetwork,
        version: ClarityVersion,
    ) -> (DeploymentSpecification, NetworkManifest) {
        let mut manifest = test_network_manifest();
        manifest.devnet.as_mut().unwrap().epoch_4_1 = Some(0);
        let mut deployment = deployment_with_remaps(network, None);
        deployment
            .plan
            .batches
            .push(TransactionsBatchSpecification {
                id: 0,
                epoch: Some(EpochSpec::Epoch4_1),
                transactions: vec![TransactionSpecification::RequirementPublish(
                    RequirementPublishSpecification {
                        contract_id: QualifiedContractIdentifier::parse(&format!(
                            "{SBTC_MAINNET_ADDRESS}.requirement"
                        ))
                        .unwrap(),
                        remap_sender: PrincipalData::parse_standard_principal(TEST_DEPLOYER)
                            .unwrap(),
                        remap_principals: BTreeMap::new(),
                        source: "(define-read-only (get-value) u1)".into(),
                        clarity_version: version,
                        cost: 0,
                        location: "requirement.clar".into(),
                    },
                )],
            });
        (deployment, manifest)
    }

    fn mock_epoch_41_node(server: &mut mockito::Server) {
        server
            .mock("GET", "/v2/info")
            .with_header("content-type", "application/json")
            .with_body(serde_json::to_string(&NodeInfo::default()).unwrap())
            .create();
        server
            .mock("GET", "/v2/pox")
            .with_header("content-type", "application/json")
            .with_body(pox_response(StacksEpochId::Epoch41).to_string())
            .create();
    }

    #[test]
    fn preflights_requirements_before_btc_transfers() {
        for network in [StacksNetwork::Testnet, StacksNetwork::Devnet] {
            for (status, expected_error) in [(404, "Clarity 6"), (500, "unable to check whether")] {
                let mut server = mockito::Server::new();
                mock_epoch_41_node(&mut server);
                let (mut deployment, manifest) =
                    requirement_deployment(network.clone(), ClarityVersion::Clarity6);
                deployment.plan.batches[0].transactions.insert(
                    0,
                    TransactionSpecification::BtcTransfer(crate::types::BtcTransferSpecification {
                        expected_sender: manifest.accounts["deployer"].btc_address.clone(),
                        recipient: manifest.accounts["deployer"].btc_address.clone(),
                        sats_amount: 1,
                        sats_per_byte: 1,
                    }),
                );
                let presence = server
                    .mock(
                        "GET",
                        format!("/v2/contracts/source/{TEST_DEPLOYER}/requirement").as_str(),
                    )
                    .with_status(status)
                    .expect(if status == 404 {
                        1
                    } else {
                        NODE_READ_RETRIES + 1
                    })
                    .create();
                let nonce = server
                    .mock("GET", mockito::Matcher::Regex("/v2/accounts/.*".into()))
                    .expect(0)
                    .create();
                let btc = server
                    .mock("POST", mockito::Matcher::Any)
                    .expect(0)
                    .create();
                let (events, received) = std::sync::mpsc::channel();
                let error = encode_transactions(
                    &deployment,
                    &manifest,
                    &StacksRpc::new(&server.url()),
                    &server.url(),
                    &events,
                )
                .unwrap_err();
                assert!(error.contains(expected_error), "{error}");
                assert!(received.try_iter().next().is_none());
                presence.assert();
                nonce.assert();
                btc.assert();
            }
        }
    }

    #[test]
    fn preflights_requirements_once_and_preserves_remaps() {
        for network in [StacksNetwork::Testnet, StacksNetwork::Devnet] {
            for (published, version) in [
                (true, ClarityVersion::Clarity6),
                (false, ClarityVersion::Clarity7),
            ] {
                let mut server = mockito::Server::new();
                mock_epoch_41_node(&mut server);
                let (mut deployment, manifest) = requirement_deployment(network.clone(), version);
                let source = format!(
                    "(define-read-only (get-value) (contract-call? '{SBTC_MAINNET_ADDRESS}.requirement get-value))"
                );
                deployment.plan.batches[0].transactions.push(
                    TransactionSpecification::ContractPublish(ContractPublishSpecification {
                        contract_name: ContractName::try_from("consumer").unwrap(),
                        expected_sender: PrincipalData::parse_standard_principal(TEST_DEPLOYER)
                            .unwrap(),
                        location: "consumer.clar".into(),
                        source: source.clone(),
                        clarity_version: ClarityVersion::Clarity7,
                        cost: 0,
                        anchor_block_only: true,
                    }),
                );
                let presence = server
                    .mock(
                        "GET",
                        format!("/v2/contracts/source/{TEST_DEPLOYER}/requirement").as_str(),
                    )
                    .with_status(if published { 200 } else { 404 })
                    .with_header("content-type", "application/json")
                    .with_body(
                        r#"{"source":"(define-read-only (get-value) u1)","publish_height":1}"#,
                    )
                    .expect(1)
                    .create();
                let nonce = server
                    .mock("GET", format!("/v2/accounts/{TEST_DEPLOYER}").as_str())
                    .with_header("content-type", "application/json")
                    .with_body(r#"{"balance":"0","nonce":0,"balance_proof":"","nonce_proof":""}"#)
                    .expect(1)
                    .create();
                let (events, _) = std::sync::mpsc::channel();
                let encoded = encode_transactions(
                    &deployment,
                    &manifest,
                    &StacksRpc::new(&server.url()),
                    "http://localhost:18443",
                    &events,
                )
                .unwrap();
                let trackers = &encoded[0].1;
                assert_eq!(trackers.len(), if published { 1 } else { 2 });
                if !published {
                    let TransactionStatus::Encoded(tx, _) = &trackers[0].status else {
                        panic!("requirement was not encoded");
                    };
                    assert_eq!(
                        tx.payload,
                        TransactionPayload::SmartContract(
                            TransactionSmartContract {
                                name: ContractName::try_from("requirement").unwrap(),
                                code_body: StacksString::from_str(
                                    "(define-read-only (get-value) u1)"
                                )
                                .unwrap(),
                            },
                            None,
                        )
                    );
                }
                let consumer = trackers.last().unwrap();
                assert_eq!(consumer.index, 1);
                let TransactionStatus::Encoded(tx, _) = &consumer.status else {
                    panic!("consumer was not encoded");
                };
                assert_eq!(
                    tx.payload,
                    TransactionPayload::SmartContract(
                        TransactionSmartContract {
                            name: ContractName::try_from("consumer").unwrap(),
                            code_body: StacksString::from_str(
                                &source.replace(SBTC_MAINNET_ADDRESS, TEST_DEPLOYER)
                            )
                            .unwrap(),
                        },
                        None,
                    )
                );
                presence.assert();
                nonce.assert();
            }
        }
    }

    #[test]
    fn rejects_unknown_or_missing_current_epoch() {
        let mut response = pox_response(StacksEpochId::Epoch40);
        response["current_epoch"] = "Epoch42".into();
        assert!(serde_json::from_value::<PoxInfo>(response.clone()).is_err());
        response.as_object_mut().unwrap().remove("current_epoch");
        assert!(serde_json::from_value::<PoxInfo>(response).is_err());
    }

    #[test]
    fn uses_node_current_epoch_for_staggered_public_networks() {
        let mut testnet = pox_response(StacksEpochId::Epoch41);
        // The node's active epoch is authoritative, not the schedule or burn height.
        testnet["epochs"] = serde_json::json!([
            {"epoch_id":"Epoch40", "start_height":100, "end_height":300},
            {"epoch_id":"Epoch42", "start_height":300, "end_height":400}
        ]);
        let pox: PoxInfo = serde_json::from_value(testnet.clone()).unwrap();
        assert_eq!(pox.current_epoch, StacksEpochId::Epoch41);
        testnet["current_epoch"] = serde_json::json!(StacksEpochId::Epoch40);
        let pox: PoxInfo = serde_json::from_value(testnet).unwrap();
        assert_eq!(pox.current_epoch, StacksEpochId::Epoch40);
        assert_eq!(
            target_epoch(&StacksNetwork::Testnet, None, Some(StacksEpochId::Epoch41)),
            StacksEpochId::Epoch41
        );
        assert_eq!(
            target_epoch(&StacksNetwork::Mainnet, None, Some(StacksEpochId::Epoch40)),
            StacksEpochId::Epoch40
        );
    }

    #[test]
    fn rewrites_sbtc_contract_references_for_testnet_deployments() {
        assert_ne!(SBTC_TESTNET_ADDRESS, SBTC_MAINNET_ADDRESS);

        let source = format!(
            "(contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-registry get-bitcoin-wallet-public-key)\n\
             (contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-token get-name)\n\
             (contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-deposit get-deposit-status u1)\n\
             (contract-call? 'SP000000000000000000002Q6VF78.unrelated get-name)"
        );

        let remapped =
            remap_deployment_source(&source, sbtc_contract_ids_to_remap(&StacksNetwork::Testnet));

        assert_eq!(
            remapped,
            format!(
                "(contract-call? '{SBTC_TESTNET_ADDRESS}.sbtc-registry get-bitcoin-wallet-public-key)\n\
                 (contract-call? '{SBTC_TESTNET_ADDRESS}.sbtc-token get-name)\n\
                 (contract-call? '{SBTC_TESTNET_ADDRESS}.sbtc-deposit get-deposit-status u1)\n\
                 (contract-call? 'SP000000000000000000002Q6VF78.unrelated get-name)"
            )
        );
    }

    /// Default deployer from the generated Devnet settings.
    const TEST_DEPLOYER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

    /// Build the mapping added for a Devnet sBTC requirement.
    fn devnet_requirement_mapping(contract_name: &str) -> (String, String) {
        let deployer = PrincipalData::parse_standard_principal(TEST_DEPLOYER).unwrap();
        (
            format!("{SBTC_MAINNET_ADDRESS}.{contract_name}"),
            QualifiedContractIdentifier::new(
                deployer,
                ContractName::try_from(contract_name.to_string()).unwrap(),
            )
            .to_string(),
        )
    }

    #[test]
    fn boot_contract_references_are_rewritten_on_every_non_mainnet_network() {
        for network in [StacksNetwork::Devnet, StacksNetwork::Testnet] {
            let source = format!("(contract-call? '{BOOT_MAINNET_ADDRESS}.pox-4 get-pox-info)");
            let remapped = remap_deployment_source(&source, sbtc_contract_ids_to_remap(&network));

            assert_eq!(
                remapped,
                format!("(contract-call? '{BOOT_TESTNET_ADDRESS}.pox-4 get-pox-info)"),
                "boot contracts live at the testnet boot address on {network:?}"
            );

            // Sharing simnet's helper means these deployments also stop
            // rewriting boot IDs that are not principal literals.
            let quoted = format!(r#"(define-constant S "{BOOT_MAINNET_ADDRESS}.pox-4")"#);
            assert_eq!(
                remap_deployment_source(&quoted, sbtc_contract_ids_to_remap(&network)),
                quoted,
                "a boot ID inside a string literal is not a contract reference"
            );
        }
    }

    /// The whole per-network picture for a source naming both a boot contract
    /// and sBTC at their mainnet addresses.
    #[test]
    fn boot_and_sbtc_rewrites_per_network() {
        let source = format!(
            "(contract-call? '{BOOT_MAINNET_ADDRESS}.pox-4 get-pox-info)\n\
             (contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-token get-name)"
        );

        // Testnet: boot moves to its twin, sBTC moves to the testnet deployer.
        assert_eq!(
            remap_deployment_source(&source, sbtc_contract_ids_to_remap(&StacksNetwork::Testnet)),
            format!(
                "(contract-call? '{BOOT_TESTNET_ADDRESS}.pox-4 get-pox-info)\n\
                 (contract-call? '{SBTC_TESTNET_ADDRESS}.sbtc-token get-name)"
            ),
        );

        // Devnet: boot moves; sBTC is left for the requirement arm, which
        // inserts its pair as it walks the plan.
        assert_eq!(
            remap_deployment_source(&source, sbtc_contract_ids_to_remap(&StacksNetwork::Devnet)),
            format!(
                "(contract-call? '{BOOT_TESTNET_ADDRESS}.pox-4 get-pox-info)\n\
                 (contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-token get-name)"
            ),
        );
    }

    #[test]
    fn sbtc_contract_references_are_not_rewritten_for_devnet_deployments() {
        let contract_ids = sbtc_contract_ids_to_remap(&StacksNetwork::Devnet);

        for contract_name in SBTC_CONTRACTS_NAMES {
            let source_id = format!("{SBTC_MAINNET_ADDRESS}.{contract_name}");
            assert!(
                !contract_ids.iter().any(|(old, _)| *old == source_id),
                "Devnet must not use the testnet sBTC destination"
            );
        }
    }

    #[test]
    fn devnet_sbtc_mapping_has_a_single_destination() {
        // The requirement mapping must be the only destination for this source.
        let mut contract_ids = sbtc_contract_ids_to_remap(&StacksNetwork::Devnet);
        let (source_id, deployer_id) = devnet_requirement_mapping("sbtc-token");
        contract_ids.insert(source_id.clone(), deployer_id.clone());

        let source = format!("(contract-call? '{source_id} get-name)");
        assert_eq!(
            remap_contract_ids(&source, contract_ids.iter()),
            format!("(contract-call? '{deployer_id} get-name)"),
            "devnet must rewrite sBTC references to the locally deployed contract"
        );
    }

    #[test]
    fn requirement_arm_overrides_a_seeded_pair() {
        // A requirement's local destination replaces any legacy sBTC seed.
        let mut contract_ids = sbtc_contract_ids_to_remap(&StacksNetwork::Testnet);
        let (source_id, deployer_id) = devnet_requirement_mapping("sbtc-token");
        contract_ids.insert(source_id.clone(), deployer_id.clone());

        let source = format!("(contract-call? '{source_id} get-name)");
        assert_eq!(
            remap_contract_ids(&source, contract_ids.iter()),
            format!("(contract-call? '{deployer_id} get-name)"),
        );
    }

    fn deployment_with_remaps(
        network: StacksNetwork,
        contract_remaps: Option<ContractRemaps>,
    ) -> DeploymentSpecification {
        DeploymentSpecification {
            id: 0,
            name: "test".into(),
            network,
            stacks_node: None,
            bitcoin_node: None,
            genesis: None,
            plan: crate::types::TransactionPlanSpecification { batches: vec![] },
            contracts: BTreeMap::new(),
            contract_remaps,
        }
    }

    #[test]
    fn plan_remaps_replace_the_legacy_sbtc_seed() {
        let nft_trait = QualifiedContractIdentifier::parse(
            "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait",
        )
        .unwrap();
        let testnet_trait =
            QualifiedContractIdentifier::parse(&format!("{TEST_DEPLOYER}.nft-trait")).unwrap();
        let deployment = deployment_with_remaps(
            StacksNetwork::Testnet,
            Some(ContractRemaps::from_iter([(
                nft_trait.clone(),
                testnet_trait.clone(),
            )])),
        );

        assert_eq!(
            initial_contract_ids_to_remap(&deployment),
            BTreeMap::from([(nft_trait.to_string(), testnet_trait.to_string())]),
        );
    }

    #[test]
    fn only_a_legacy_plan_keeps_the_sbtc_seed() {
        let legacy = deployment_with_remaps(StacksNetwork::Testnet, None);
        assert_eq!(
            initial_contract_ids_to_remap(&legacy),
            sbtc_contract_ids_to_remap(&StacksNetwork::Testnet),
        );
        let opted_out =
            deployment_with_remaps(StacksNetwork::Testnet, Some(ContractRemaps::default()));
        assert!(initial_contract_ids_to_remap(&opted_out).is_empty());
    }

    #[test]
    fn explicit_boot_contract_override_wins_over_the_boot_principal_fallback() {
        let source = format!(
            "(contract-call? '{BOOT_MAINNET_ADDRESS}.pox-4 get-pox-info)\n\
             (contract-call? '{BOOT_MAINNET_ADDRESS}.costs-3 get-runtime)"
        );
        let destination = format!("{TEST_DEPLOYER}.custom-pox");
        assert_eq!(
            remap_deployment_source(
                &source,
                [(format!("{BOOT_MAINNET_ADDRESS}.pox-4"), destination.clone())],
            ),
            format!(
                "(contract-call? '{destination} get-pox-info)\n\
                 (contract-call? '{BOOT_TESTNET_ADDRESS}.costs-3 get-runtime)"
            ),
        );
    }

    #[test]
    fn relative_references_in_a_requirement_reach_their_remap() {
        let original = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9";
        let destination = format!("{TEST_DEPLOYER}.mock-base");
        let deployer = PrincipalData::parse_standard_principal(TEST_DEPLOYER).unwrap();
        let remaps = ContractRemaps::from_iter([(
            QualifiedContractIdentifier::parse(&format!("{original}.base")).unwrap(),
            QualifiedContractIdentifier::parse(&destination).unwrap(),
        )]);
        let tx = RequirementPublishSpecification {
            contract_id: QualifiedContractIdentifier::parse(&format!("{original}.consumer"))
                .unwrap(),
            remap_sender: deployer.clone(),
            remap_principals: BTreeMap::from([(
                PrincipalData::parse_standard_principal(original).unwrap(),
                deployer,
            )]),
            source: "(use-trait t .base.trait) (contract-call? .base f) (contract-call? .other f)"
                .into(),
            clarity_version: ClarityVersion::Clarity3,
            cost: 0,
            location: "consumer.clar".into(),
        };
        let deployment = deployment_with_remaps(StacksNetwork::Testnet, Some(remaps));
        assert_eq!(
            requirement_publish_source(
                &tx,
                deployment.contract_remaps.as_ref(),
                &initial_contract_ids_to_remap(&deployment),
            ),
            format!(
                "(use-trait t '{destination}.trait) (contract-call? '{destination} f) (contract-call? .other f)"
            ),
        );
    }
}
