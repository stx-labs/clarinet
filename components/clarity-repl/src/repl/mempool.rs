//! Session-local admission and queuing, separate from block execution.

use std::collections::{BTreeMap, HashMap, VecDeque};

use clarity::vm::clarity::ClarityConnection;
use clarity_types::types::{PrincipalData, QualifiedContractIdentifier};
use serde::{Serialize, Serializer};
use stacks_codec::transaction::{StacksTransaction, TransactionPayload};
use stacks_common::types::chainstate::Txid;

use super::interpreter::ClarityInterpreter;
use super::raw_transaction::{decode, RawTransactionError};
use super::Session;
use crate::utils::serialize_display;

#[derive(Clone, Default)]
pub(crate) struct Mempool {
    transactions: VecDeque<PendingTransaction>,
}

#[derive(Clone)]
struct PendingTransaction {
    txid: Txid,
    tx: StacksTransaction,
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum AdmissionResult {
    Accepted {
        txid: Txid,
    },
    Rejected {
        error: &'static str,
        reason: &'static str,
        #[serde(skip_serializing_if = "AdmissionData::is_none")]
        reason_data: AdmissionData,
        #[serde(skip_serializing_if = "Option::is_none")]
        txid: Option<Txid>,
    },
}

#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum AdmissionData {
    None,
    Message {
        #[serde(serialize_with = "serialize_display")]
        message: Box<AdmissionMessage>,
    },
    Funds {
        #[serde(serialize_with = "serialize_balance")]
        expected: u128,
        #[serde(serialize_with = "serialize_balance")]
        actual: u128,
    },
    Fee {
        expected: u64,
        actual: u64,
    },
    Nonce {
        expected: u64,
        actual: u64,
        #[serde(serialize_with = "serialize_display")]
        principal: Box<PrincipalData>,
        is_origin: bool,
        #[serde(skip_serializing_if = "Option::is_none")]
        message: Option<&'static str>,
    },
    Contract {
        #[serde(serialize_with = "serialize_display")]
        contract_identifier: QualifiedContractIdentifier,
    },
}

impl AdmissionData {
    fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum AdmissionMessage {
    #[error("{0}")]
    Static(&'static str),
    #[error("{0}")]
    Connection(String),
    #[error(transparent)]
    Transaction(#[from] RawTransactionError),
    #[error(transparent)]
    Database(#[from] clarity::vm::errors::VmExecutionError),
    #[error(transparent)]
    Argument(#[from] clarity::vm::analysis::errors::StaticCheckError),
}

fn serialize_balance<S: Serializer>(value: &u128, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.collect_str(&format_args!("0x{value:032x}"))
}

// TODO(upstream): Extract stackslib's MemPoolRejection and into_json mapping
// from node-specific errors so Wasm can share these rejection codes and data.
#[derive(Debug)]
struct AdmissionError {
    reason: &'static str,
    data: AdmissionData,
}

impl AdmissionError {
    fn new(reason: &'static str) -> Self {
        Self {
            reason,
            data: AdmissionData::None,
        }
    }

    fn message(reason: &'static str, message: impl Into<AdmissionMessage>) -> Self {
        Self {
            reason,
            data: AdmissionData::Message {
                message: Box::new(message.into()),
            },
        }
    }
}

fn require_funds(
    db: &mut clarity::vm::database::ClarityDatabase,
    principal: &PrincipalData,
    expected: u128,
) -> Result<(), AdmissionError> {
    let mut balance = db
        .get_stx_balance_snapshot(principal)
        .map_err(|e| AdmissionError::message("ServerFailureDatabase", e))?;
    if !balance
        .can_transfer(expected)
        .map_err(|e| AdmissionError::message("ServerFailureDatabase", e))?
    {
        let actual = balance
            .get_available_balance()
            .map_err(|e| AdmissionError::message("ServerFailureDatabase", e))?;
        return Err(AdmissionError {
            reason: "NotEnoughFunds",
            data: AdmissionData::Funds { expected, actual },
        });
    }
    Ok(())
}

fn check_nonce_dependencies(
    db: &mut clarity::vm::database::ClarityDatabase,
    pending: &VecDeque<PendingTransaction>,
    candidate: &StacksTransaction,
) -> Result<(), AdmissionError> {
    let count = pending.len() + 1;
    let mut reservations: HashMap<PrincipalData, BTreeMap<u64, usize>> = HashMap::new();
    let mut current_nonces = HashMap::new();
    for (index, tx) in pending
        .iter()
        .map(|entry| &entry.tx)
        .chain(std::iter::once(candidate))
        .enumerate()
    {
        let nonces = std::iter::once((
            PrincipalData::from(tx.origin_address()),
            tx.get_origin_nonce(),
        ))
        .chain(
            tx.sponsor_address()
                .map(|address| (address.into(), tx.get_payer().nonce())),
        );
        let mut stale = false;
        for (principal, nonce) in nonces.clone() {
            let current = match current_nonces.entry(principal) {
                std::collections::hash_map::Entry::Occupied(entry) => *entry.get(),
                std::collections::hash_map::Entry::Vacant(entry) => {
                    let current = db
                        .get_account_nonce(entry.key())
                        .map_err(|e| AdmissionError::message("ServerFailureDatabase", e))?;
                    *entry.insert(current)
                }
            };
            if nonce < current {
                stale = true;
                break;
            }
        }
        // Mining removes stale entries without executing them, so they cannot
        // impose dependencies on transactions that are still executable.
        if stale {
            continue;
        }
        for (principal, nonce) in nonces {
            reservations
                .entry(principal)
                .or_default()
                .insert(nonce, index);
        }
    }

    let mut dependents = vec![Vec::new(); count];
    let mut prerequisites = vec![0; count];
    for account in reservations.values() {
        // Missing nonces may arrive later, but existing reservations must execute
        // in nonce order. Adjacent reservations capture that ordering transitively.
        let mut ordered = account.values().copied();
        if let Some(mut previous) = ordered.next() {
            for next in ordered {
                dependents[previous].push(next);
                prerequisites[next] += 1;
                previous = next;
            }
        }
    }
    let mut ready: Vec<_> = prerequisites
        .iter()
        .enumerate()
        .filter_map(|(index, &remaining)| (remaining == 0).then_some(index))
        .collect();
    let mut visited = 0;
    while let Some(index) = ready.pop() {
        visited += 1;
        for &dependent in &dependents[index] {
            prerequisites[dependent] -= 1;
            if prerequisites[dependent] == 0 {
                ready.push(dependent);
            }
        }
    }
    if visited != count {
        return Err(AdmissionError::message(
            "ConflictingNonceInMempool",
            AdmissionMessage::Static("Transaction would create a circular nonce dependency"),
        ));
    }
    Ok(())
}

impl Session {
    /// Submit to the mempool without executing or advancing the chain.
    pub fn submit_transaction(&mut self, raw_hex: &str) -> AdmissionResult {
        let tx = match decode(raw_hex) {
            Ok(tx) => tx,
            Err(error) => {
                return AdmissionResult::Rejected {
                    error: "transaction rejected",
                    reason: "Deserialization",
                    reason_data: AdmissionData::Message {
                        message: Box::new(error.into()),
                    },
                    txid: None,
                }
            }
        };
        let txid = tx.txid();
        if self
            .mempool
            .transactions
            .iter()
            .any(|pending| pending.txid == txid)
        {
            return AdmissionResult::Accepted { txid };
        }
        match self
            .interpreter
            .check_mempool_admission(&tx, &self.mempool.transactions)
        {
            Ok(()) => {
                self.mempool.transactions.push_back(PendingTransaction {
                    txid: txid.clone(),
                    tx,
                });
                AdmissionResult::Accepted { txid }
            }
            Err(error) => AdmissionResult::Rejected {
                error: "transaction rejected",
                reason: error.reason,
                reason_data: error.data,
                txid: Some(txid),
            },
        }
    }

    /// Select ready or stale transactions; leave non-stale nonce gaps queued.
    pub fn next_mempool_transaction(&mut self) -> Option<StacksTransaction> {
        let index =
            self.mempool
                .transactions
                .iter()
                .position(|PendingTransaction { tx, .. }| {
                    let mut ready = true;
                    for (principal, nonce) in
                        std::iter::once((tx.origin_address().into(), tx.get_origin_nonce())).chain(
                            tx.sponsor_address()
                                .map(|address| (address.into(), tx.get_payer().nonce())),
                        )
                    {
                        let Ok(current) = self.interpreter.get_nonce(&principal) else {
                            return true;
                        };
                        // A stale nonce invalidates the transaction even if the other
                        // account has a gap; select it so mining rejects and removes it.
                        match nonce.cmp(&current) {
                            std::cmp::Ordering::Less => return true,
                            std::cmp::Ordering::Equal => {}
                            std::cmp::Ordering::Greater => ready = false,
                        }
                    }
                    ready
                })?;
        self.mempool
            .transactions
            .remove(index)
            .map(|pending| pending.tx)
    }
}

impl ClarityInterpreter {
    // TODO(upstream): Extract StacksChainState::can_include_tx into a Wasm-compatible
    // API over ClarityConnection. Making it public in stackslib alone is insufficient.
    fn check_mempool_admission(
        &mut self,
        tx: &StacksTransaction,
        pending: &VecDeque<PendingTransaction>,
    ) -> Result<(), AdmissionError> {
        match &tx.payload {
            TransactionPayload::Coinbase(..) => {
                return Err(AdmissionError::new("NoCoinbaseViaMempool"))
            }
            TransactionPayload::TenureChange(..) => {
                return Err(AdmissionError::new("NoTenureChangeViaMempool"))
            }
            TransactionPayload::PoisonMicroblock(..) => {
                return Err(AdmissionError::message(
                    "ServerFailureOther",
                    AdmissionMessage::Static(
                        "PoisonMicroblock transactions not accepted via mempool",
                    ),
                ))
            }
            _ => {}
        }
        self.check_raw_transaction(tx)
            .map_err(|e| AdmissionError::message("SignatureValidation", e))?;
        // Pinned stacks-node policy: MINIMUM_TX_FEE = 1 and
        // MINIMUM_TX_FEE_RATE_PER_BYTE = 1 (stackslib/db/blocks.rs).
        // TODO(upstream): Export these constants and can_include_tx's fee predicate
        // from a Wasm-compatible crate instead of embedding the policy here.
        let minimum_fee = tx.tx_len();
        if tx.get_tx_fee() < minimum_fee {
            return Err(AdmissionError {
                reason: "FeeTooLow",
                data: AdmissionData::Fee {
                    expected: minimum_fee,
                    actual: tx.get_tx_fee(),
                },
            });
        }
        let epoch = self.datastore.get_current_epoch();
        let address_version = match &tx.payload {
            TransactionPayload::TokenTransfer(recipient, ..) => Some(recipient.version()),
            TransactionPayload::ContractCall(call) => Some(call.address.version()),
            _ => None,
        };
        if let Some(version) = address_version {
            // TODO(upstream): Expose StacksChainState::is_valid_address_version
            // outside stackslib; is_mainnet() does not reject unknown testnet versions.
            use stacks_common::address::{
                C32_ADDRESS_VERSION_MAINNET_MULTISIG, C32_ADDRESS_VERSION_MAINNET_SINGLESIG,
                C32_ADDRESS_VERSION_TESTNET_MULTISIG, C32_ADDRESS_VERSION_TESTNET_SINGLESIG,
            };
            let supported = if self.transaction_config(epoch).mainnet {
                [
                    C32_ADDRESS_VERSION_MAINNET_SINGLESIG,
                    C32_ADDRESS_VERSION_MAINNET_MULTISIG,
                ]
            } else {
                [
                    C32_ADDRESS_VERSION_TESTNET_SINGLESIG,
                    C32_ADDRESS_VERSION_TESTNET_MULTISIG,
                ]
            };
            if !supported.contains(&version) {
                return Err(AdmissionError::new("BadAddressVersionByte"));
            }
        }
        let mut connection = self
            .get_transaction_connection(epoch, false, vec![])
            .map_err(|e| {
                AdmissionError::message("ServerFailureDatabase", AdmissionMessage::Connection(e))
            })?;
        let origin: PrincipalData = tx.origin_address().into();
        let sponsor = tx.sponsor_address().map(PrincipalData::from);
        let payer = sponsor.as_ref().unwrap_or(&origin);
        if sponsor.as_ref() == Some(&origin) && tx.get_origin_nonce() != tx.get_payer().nonce() {
            return Err(AdmissionError {
                reason: "BadNonce",
                data: AdmissionData::Nonce {
                    expected: tx.get_origin_nonce(),
                    actual: tx.get_payer().nonce(),
                    principal: Box::new(origin.clone()),
                    is_origin: false,
                    message: Some(
                        "Self-sponsored transactions must use equal origin and sponsor nonces",
                    ),
                },
            });
        }
        connection.with_clarity_db_readonly(|db| -> Result<(), AdmissionError> {
            // TODO(upstream): Share check_transaction_nonces and can_include_tx's
            // chaining policy once their node-specific account/error types are extracted.
            for (principal, actual, is_origin) in sponsor
                .as_ref()
                .map(|principal| (principal, tx.get_payer().nonce(), false))
                .into_iter()
                .chain(std::iter::once((&origin, tx.get_origin_nonce(), true)))
            {
                let expected = db
                    .get_account_nonce(principal)
                    .map_err(|e| AdmissionError::message("ServerFailureDatabase", e))?;
                if actual < expected || actual == u64::MAX {
                    return Err(AdmissionError {
                        reason: "BadNonce",
                        data: AdmissionData::Nonce {
                            expected,
                            actual,
                            principal: Box::new(principal.clone()),
                            is_origin,
                            message: None,
                        },
                    });
                }
                // Pinned node's MAXIMUM_MEMPOOL_TX_CHAINING is 25; its upper
                // bound is the mined nonce + 1 + that limit. Import the constant
                // when the upstream chaining policy is available to Wasm.
                let maximum = expected.saturating_add(26);
                if actual > maximum {
                    return Err(AdmissionError {
                        reason: "TooMuchChaining",
                        data: AdmissionData::Nonce {
                            expected: maximum,
                            actual,
                            principal: Box::new(principal.clone()),
                            is_origin,
                            message: Some("Nonce would exceed chaining limit in mempool"),
                        },
                    });
                }
                // TODO(upstream): Separate MemPoolDB::try_add_tx's conflict/replacement
                // policy from SQLite and fork lookup before sharing it with this queue.
                if pending.iter().any(|PendingTransaction { tx: queued, .. }| {
                    (queued.get_origin_nonce() == actual
                        && PrincipalData::from(queued.origin_address()) == *principal)
                        || queued.sponsor_address().is_some_and(|address| {
                            queued.get_payer().nonce() == actual
                                && PrincipalData::from(address) == *principal
                        })
                }) {
                    return Err(AdmissionError::new("ConflictingNonceInMempool"));
                }
            }
            check_nonce_dependencies(db, pending, tx)?;
            if let TransactionPayload::TokenTransfer(recipient, amount, _) = &tx.payload {
                // TODO(upstream): Extract can_admit_mempool_semantic for the
                // self-transfer and zero-amount rules, preserving node check ordering.
                if recipient == &origin {
                    return Err(AdmissionError::new("TransferRecipientCannotEqualSender"));
                }
                if *amount == 0 {
                    return Err(AdmissionError::new("TransferAmountMustBePositive"));
                }
                if !tx.post_conditions.is_empty() {
                    return Err(AdmissionError::message(
                        "ServerFailureOther",
                        AdmissionMessage::Static("native transfers do not support post-conditions"),
                    ));
                }
                if payer == &origin {
                    return require_funds(
                        db,
                        &origin,
                        u128::from(*amount) + u128::from(tx.get_tx_fee()),
                    );
                }
                require_funds(db, &origin, u128::from(*amount))?;
            }
            require_funds(db, payer, u128::from(tx.get_tx_fee()))
        })?;
        // can_include_tx is private to stackslib; its type-checking APIs are
        // available here and keep trait casts and epoch-specific types upstream.
        connection.with_analysis_db_readonly(|db| match &tx.payload {
            TransactionPayload::ContractCall(call) => {
                let id = call.to_clarity_contract_id();
                let function = db
                    .get_public_function_type(&id, &call.function_name, &epoch)
                    .map_err(|_| AdmissionError::new("NoSuchContract"))?
                    .ok_or_else(|| AdmissionError::new("NoSuchPublicFunction"))?;
                let version = db
                    .get_clarity_version(&id)
                    .map_err(|_| AdmissionError::new("NoSuchContract"))?;
                function
                    .check_args_by_allowing_trait_cast(db, &call.function_args, epoch, version)
                    .map(|_| ())
                    .map_err(|e| AdmissionError::message("BadFunctionArgument", e))
            }
            TransactionPayload::SmartContract(contract, _) => {
                let id = QualifiedContractIdentifier::new(
                    tx.origin_address().into(),
                    contract.name.clone(),
                );
                if db.has_contract(&id) {
                    Err(AdmissionError {
                        reason: "ContractAlreadyExists",
                        data: AdmissionData::Contract {
                            contract_identifier: id,
                        },
                    })
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        })
    }
}
