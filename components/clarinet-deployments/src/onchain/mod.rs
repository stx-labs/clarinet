use std::collections::{BTreeMap, HashSet, VecDeque};
use std::sync::mpsc::{Receiver, Sender};

use bitcoincore_rpc::{Auth, Client};
use clarinet_defaults::DEFAULT_EPOCH;
use clarinet_files::{AccountConfig, NetworkManifest, StacksNetwork};
use clarinet_utils::get_bip32_keys_from_mnemonic;
use clarity::codec::StacksMessageCodec;
use clarity::types::chainstate::StacksAddress;
use clarity::util::secp256k1::{MessageSignature, Secp256k1PrivateKey, Secp256k1PublicKey};
use clarity::vm::types::{PrincipalData, QualifiedContractIdentifier, StandardPrincipalData};
use clarity::vm::{ClarityName, ClarityVersion, ContractName, EvaluationResult, Value};
use clarity_repl::repl::boot::{
    BOOT_CONTRACTS_NAMES, BOOT_MAINNET_ADDRESS, BOOT_TESTNET_ADDRESS, SBTC_CONTRACTS_NAMES,
    SBTC_MAINNET_ADDRESS, SBTC_TESTNET_ADDRESS,
};
use clarity_repl::repl::{Session, SessionSettings};
use libsecp256k1::PublicKey;
use reqwest::Url;
use stacks_codec::strings::StacksString;
use stacks_codec::transaction::{
    SinglesigHashMode, SinglesigSpendingCondition, StacksTransaction, TokenTransferMemo,
    TransactionAnchorMode, TransactionAuth, TransactionContractCall, TransactionPayload,
    TransactionPostConditionMode, TransactionPublicKeyEncoding, TransactionSmartContract,
    TransactionSpendingCondition, TransactionVersion,
};
use stacks_common::address::{
    AddressHashMode, C32_ADDRESS_VERSION_MAINNET_SINGLESIG, C32_ADDRESS_VERSION_TESTNET_SINGLESIG,
};
use stacks_rpc_client::StacksRpc;

mod bitcoin_deployment;

use crate::types::{DeploymentSpecification, EpochSpec, TransactionSpecification};

/// Return the initial contract-ID remappings for `network`.
/// Devnet sBTC mappings are added later from its requirement transactions.
fn boot_contract_ids_to_remap(network: &StacksNetwork) -> HashSet<(String, String)> {
    let mut contract_ids = HashSet::new();

    for contract_name in BOOT_CONTRACTS_NAMES {
        contract_ids.insert((
            format!("{BOOT_MAINNET_ADDRESS}.{contract_name}"),
            format!("{BOOT_TESTNET_ADDRESS}.{contract_name}"),
        ));
    }

    if matches!(network, StacksNetwork::Testnet) {
        for contract_name in SBTC_CONTRACTS_NAMES {
            contract_ids.insert((
                format!("{SBTC_MAINNET_ADDRESS}.{contract_name}"),
                format!("{SBTC_TESTNET_ADDRESS}.{contract_name}"),
            ));
        }
    }

    contract_ids
}

fn remap_contract_ids(source: &str, contract_ids: &HashSet<(String, String)>) -> String {
    let mut source = source.to_string();
    for (old_contract_id, new_contract_id) in contract_ids {
        let mut matched_indices = source
            .match_indices(old_contract_id)
            .map(|(i, _)| i)
            .collect::<Vec<usize>>();
        matched_indices.reverse();
        for index in matched_indices {
            source.replace_range(index..index + old_contract_id.len(), new_contract_id);
        }
    }
    source
}

fn get_btc_secret_key(account: &AccountConfig) -> bitcoincore_rpc::bitcoin::secp256k1::SecretKey {
    use bitcoincore_rpc::bitcoin::secp256k1::SecretKey;
    let (secret_bytes, _) =
        get_bip32_keys_from_mnemonic(&account.mnemonic, "", &account.derivation).unwrap();
    SecretKey::from_slice(&secret_bytes).unwrap()
}

fn get_keypair(account: &AccountConfig) -> (Secp256k1PrivateKey, PublicKey) {
    let (secret_bytes, public_key) =
        get_bip32_keys_from_mnemonic(&account.mnemonic, "", &account.derivation).unwrap();
    let wrapped_secret_key = Secp256k1PrivateKey::from_slice(&secret_bytes).unwrap();
    (wrapped_secret_key, public_key)
}

fn get_stacks_address(public_key: &PublicKey, network: &StacksNetwork) -> StacksAddress {
    let wrapped_public_key =
        Secp256k1PublicKey::from_slice(&public_key.serialize_compressed()).unwrap();

    StacksAddress::from_public_keys(
        match network {
            StacksNetwork::Mainnet => C32_ADDRESS_VERSION_MAINNET_SINGLESIG,
            _ => C32_ADDRESS_VERSION_TESTNET_SINGLESIG,
        },
        &AddressHashMode::SerializeP2PKH,
        1,
        &vec![wrapped_public_key],
    )
    .unwrap()
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
    let signer_addr = get_stacks_address(&public_key, network);

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

    for batch_spec in deployment.plan.batches.iter_mut() {
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
                        None,
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

/// Polls (one second apart) before giving up on the node. On devnet its RPC can
/// lag a few seconds behind the first block events.
const NODE_RPC_RETRIES: usize = 30;

/// The node just answered `/v2/info`, so the reads that follow only have to
/// absorb a transient blip.
const NODE_READ_RETRIES: usize = 3;

fn wait_for_node(stacks_rpc: &StacksRpc) -> Result<(), String> {
    stacks_rpc
        .call_with_retry(|rpc| rpc.get_info(), NODE_RPC_RETRIES)
        .map(|_| ())
        .map_err(|e| format!("unable to reach the stacks node at {}: {e}", stacks_rpc.url))
}

/// The first nonce of each account is read from the node, so accounts that
/// already transacted, as in a devnet snapshot, start from their actual nonce.
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

/// Emulated transactions never reach the chain. Both this walk and
/// `get_initial_transactions_trackers` skip them and index everything else alike.
fn is_emulated(transaction: &TransactionSpecification) -> bool {
    matches!(
        transaction,
        TransactionSpecification::EmulatedContractPublish(_)
            | TransactionSpecification::EmulatedContractCall(_)
    )
}

/// Encode and sign every transaction of the plan, in order, grouped by batch epoch.
fn encode_transactions(
    deployment: &DeploymentSpecification,
    network_manifest: &NetworkManifest,
    stacks_rpc: &StacksRpc,
    bitcoin_node_url: &str,
    deployment_event_tx: &Sender<DeploymentEvent>,
) -> Result<VecDeque<(EpochSpec, Vec<TransactionTracker>)>, String> {
    wait_for_node(stacks_rpc)?;

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
    let mut contracts_ids_to_remap = boot_contract_ids_to_remap(network);
    let mut batches = VecDeque::new();
    let mut next_index = 0;

    for batch_spec in deployment.plan.batches.iter() {
        let epoch = batch_spec.epoch.unwrap_or(DEFAULT_EPOCH.into());
        let mut batch = Vec::new();
        for transaction in batch_spec.transactions.iter() {
            if is_emulated(transaction) {
                continue;
            }
            // Every remaining transaction consumes an index, published or not, because
            // `get_initial_transactions_trackers` numbers them all and the dashboard
            // indexes its rows by it.
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
                        name: name.clone(),
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

                    let bitcoin_node_wallet_rpc_url = format!(
                        "{}://{}:{}/wallet/",
                        url.scheme(),
                        url.host().expect("Host unknown"),
                        url.port_or_known_default().expect("Protocol unknown")
                    );
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
                        name: name.clone(),
                        status: TransactionStatus::Encoded(transaction, check),
                    }
                }
                TransactionSpecification::ContractPublish(tx) => {
                    let issuer_address = tx.expected_sender.to_address();
                    let nonce = next_nonce(&mut cached_nonces, stacks_rpc, &issuer_address)?;
                    let account = stx_accounts_lookup.get(issuer_address.as_str()).unwrap();
                    let source =
                        if matches!(network, StacksNetwork::Devnet | StacksNetwork::Testnet) {
                            remap_contract_ids(&tx.source, &contracts_ids_to_remap)
                        } else {
                            tx.source.clone()
                        };

                    let clarity_version = if epoch >= EpochSpec::Epoch2_1 {
                        Some(tx.clarity_version)
                    } else {
                        None
                    };

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
                        name: name.clone(),
                        status: TransactionStatus::Encoded(transaction, check),
                    }
                }
                TransactionSpecification::RequirementPublish(tx) => {
                    if matches!(network, StacksNetwork::Mainnet) {
                        panic!("Deployment specification malformed - requirements publish not supported on mainnet");
                    }
                    let old_contract_id = tx.contract_id.to_string();
                    let new_contract_id = QualifiedContractIdentifier::new(
                        tx.remap_sender.clone(),
                        tx.contract_id.name.clone(),
                    )
                    .to_string();
                    contracts_ids_to_remap.insert((old_contract_id, new_contract_id));

                    // Already published by a previous testnet run, or by the snapshot.
                    let issuer_address = tx.remap_sender.to_address();
                    if is_contract_published(stacks_rpc, &issuer_address, &tx.contract_id.name)? {
                        continue;
                    }

                    let nonce = next_nonce(&mut cached_nonces, stacks_rpc, &issuer_address)?;
                    let account = stx_accounts_lookup.get(issuer_address.as_str()).unwrap();

                    // Remapping principals - This is happening
                    let source = tx
                        .remap_principals
                        .iter()
                        .map(|(src, dst)| (src.to_address(), dst.to_address()))
                        .chain(contracts_ids_to_remap.iter().cloned())
                        .fold(tx.source.clone(), |source, (src, dst)| {
                            source.replace(&src, &dst)
                        });

                    let transaction = encode_contract_publish(
                        &tx.contract_id.name,
                        &source,
                        None,
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
                        name: name.clone(),
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

    // Encoding reads nonces and published contracts from the node, so it cannot
    // start before the node is up. On devnet, Start signals the chain is mining.
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
            let after_bitcoin_block = match epoch {
                EpochSpec::Epoch2_0 => devnet.epoch_2_0,
                EpochSpec::Epoch2_05 => devnet.epoch_2_05,
                EpochSpec::Epoch2_1 => devnet.epoch_2_1,
                EpochSpec::Epoch2_2 => devnet.epoch_2_2,
                EpochSpec::Epoch2_3 => devnet.epoch_2_3,
                EpochSpec::Epoch2_4 => devnet.epoch_2_4,
                EpochSpec::Epoch2_5 => devnet.epoch_2_5,
                EpochSpec::Epoch3_0 => devnet.epoch_3_0,
                EpochSpec::Epoch3_1 => devnet.epoch_3_1,
                EpochSpec::Epoch3_2 => devnet.epoch_3_2,
                EpochSpec::Epoch3_3 => devnet.epoch_3_3,
                EpochSpec::Epoch3_4 => devnet.epoch_3_4,
                // Clarinet always keeps optional support for a future epoch
                EpochSpec::Epoch4_0 => devnet.epoch_4_0.unwrap_or(u64::MAX),
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
    let mut index = 0;
    let mut trackers = vec![];
    for batch_spec in deployment.plan.batches.iter() {
        for transaction in batch_spec.transactions.iter() {
            let tracker = match transaction {
                TransactionSpecification::ContractCall(tx) => TransactionTracker {
                    index,
                    name: format!("Contract call {}::{}", tx.contract_id, tx.method),
                    status: TransactionStatus::Queued,
                },
                TransactionSpecification::ContractPublish(tx) => TransactionTracker {
                    index,
                    name: format!(
                        "Contract publish {}.{}",
                        tx.expected_sender.to_address(),
                        tx.contract_name
                    ),
                    status: TransactionStatus::Queued,
                },
                TransactionSpecification::RequirementPublish(tx) => {
                    if !matches!(
                        deployment.network,
                        StacksNetwork::Devnet | StacksNetwork::Testnet
                    ) {
                        panic!("Deployment specification malformed - requirements publish not supported on mainnet");
                    }
                    TransactionTracker {
                        index,
                        name: format!(
                            "Contract publish {}.{}",
                            tx.remap_sender.to_address(),
                            tx.contract_id.name
                        ),
                        status: TransactionStatus::Queued,
                    }
                }
                TransactionSpecification::BtcTransfer(tx) => TransactionTracker {
                    index,
                    name: format!(
                        "BTC transfer {} send {} satoshis to {}",
                        tx.expected_sender, tx.sats_amount, tx.recipient
                    ),
                    status: TransactionStatus::Queued,
                },
                TransactionSpecification::StxTransfer(tx) => TransactionTracker {
                    index,
                    name: format!(
                        "STX transfer {} send {} µSTC to {}",
                        tx.expected_sender.to_address(),
                        tx.mstx_amount,
                        tx.recipient,
                    ),
                    status: TransactionStatus::Queued,
                },
                TransactionSpecification::EmulatedContractPublish(_)
                | TransactionSpecification::EmulatedContractCall(_) => continue,
            };
            trackers.push(tracker);
            index += 1;
        }
    }
    trackers
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rewrites_sbtc_contract_references_for_testnet_deployments() {
        assert_ne!(SBTC_TESTNET_ADDRESS, SBTC_MAINNET_ADDRESS);

        let source = format!(
            "(contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-registry get-bitcoin-wallet-public-key)\n\
             (contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-token get-name)\n\
             (contract-call? '{SBTC_MAINNET_ADDRESS}.sbtc-deposit get-deposit-status u1)\n\
             (contract-call? 'SP000000000000000000002Q6VF78.unrelated get-name)"
        );

        let remapped = remap_contract_ids(
            &source,
            &boot_contract_ids_to_remap(&StacksNetwork::Testnet),
        );

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
            let remapped = remap_contract_ids(&source, &boot_contract_ids_to_remap(&network));

            assert_eq!(
                remapped,
                format!("(contract-call? '{BOOT_TESTNET_ADDRESS}.pox-4 get-pox-info)"),
                "boot contracts live at the testnet boot address on {network:?}"
            );
        }
    }

    #[test]
    fn sbtc_contract_references_are_not_rewritten_for_devnet_deployments() {
        let contract_ids = boot_contract_ids_to_remap(&StacksNetwork::Devnet);

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
        let mut contract_ids = boot_contract_ids_to_remap(&StacksNetwork::Devnet);
        let (source_id, deployer_id) = devnet_requirement_mapping("sbtc-token");
        contract_ids.insert((source_id.clone(), deployer_id.clone()));

        assert_eq!(
            contract_ids
                .iter()
                .filter(|(old, _)| *old == source_id)
                .count(),
            1,
            "{source_id} must have exactly one destination"
        );

        let source = format!("(contract-call? '{source_id} get-name)");
        assert_eq!(
            remap_contract_ids(&source, &contract_ids),
            format!("(contract-call? '{deployer_id} get-name)"),
            "devnet must rewrite sBTC references to the locally deployed contract"
        );
    }

    #[test]
    fn testnet_sbtc_mapping_is_unchanged_by_the_requirement_arm() {
        // The requirement arm re-inserts the same Testnet pair.
        let mut contract_ids = boot_contract_ids_to_remap(&StacksNetwork::Testnet);
        let before = contract_ids.len();

        let source_id = format!("{SBTC_MAINNET_ADDRESS}.sbtc-token");
        let testnet_id = format!("{SBTC_TESTNET_ADDRESS}.sbtc-token");
        contract_ids.insert((source_id.clone(), testnet_id.clone()));

        assert_eq!(
            contract_ids.len(),
            before,
            "the pair should already be present"
        );
        assert_eq!(
            contract_ids
                .iter()
                .filter(|(old, _)| *old == source_id)
                .count(),
            1
        );
    }
}
