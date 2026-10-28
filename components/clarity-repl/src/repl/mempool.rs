//! Session-local admission and queuing, separate from block execution.

use std::collections::VecDeque;

use clarity::vm::clarity::ClarityConnection;
use clarity_types::types::{PrincipalData, QualifiedContractIdentifier};
use serde::Serialize;
use serde_json::{json, Value};
use stacks_codec::transaction::{StacksTransaction, TransactionPayload};
use stacks_common::types::chainstate::Txid;

use super::interpreter::ClarityInterpreter;
use super::raw_transaction::decode;
use super::Session;

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
        error: String,
        reason: &'static str,
        #[serde(skip_serializing_if = "Value::is_null")]
        reason_data: Value,
        #[serde(skip_serializing_if = "Option::is_none")]
        txid: Option<Txid>,
    },
}

// TODO(upstream): Extract stackslib's MemPoolRejection and into_json mapping
// from node-specific errors so Wasm can share these rejection codes and data.
#[derive(Debug)]
struct AdmissionError {
    reason: &'static str,
    data: Value,
}

impl AdmissionError {
    fn new(reason: &'static str) -> Self {
        Self {
            reason,
            data: Value::Null,
        }
    }

    fn message(reason: &'static str, message: impl ToString) -> Self {
        Self {
            reason,
            data: json!({ "message": message.to_string() }),
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
            data: json!({ "expected": format!("0x{expected:032x}"), "actual": format!("0x{actual:032x}") }),
        });
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
                    error: "transaction rejected".into(),
                    reason: "Deserialization",
                    reason_data: json!({ "message": error }),
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
                error: "transaction rejected".into(),
                reason: error.reason,
                reason_data: error.data,
                txid: Some(txid),
            },
        }
    }

    /// Select a ready transaction in submission order; leave nonce gaps queued.
    pub fn next_mempool_transaction(&mut self) -> Option<StacksTransaction> {
        let index =
            self.mempool
                .transactions
                .iter()
                .position(|PendingTransaction { tx, .. }| {
                    let mut ready = |principal, nonce| {
                        self.interpreter
                            .get_nonce(&principal)
                            .map_or(true, |current| nonce <= current)
                    };
                    ready(tx.origin_address().into(), tx.get_origin_nonce())
                        && tx
                            .sponsor_address()
                            .is_none_or(|address| ready(address.into(), tx.get_payer().nonce()))
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
                    "PoisonMicroblock transactions not accepted via mempool",
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
                data: json!({ "expected": minimum_fee, "actual": tx.get_tx_fee() }),
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
            .map_err(|e| AdmissionError::message("ServerFailureDatabase", e))?;
        let origin: PrincipalData = tx.origin_address().into();
        let payer: PrincipalData = tx
            .sponsor_address()
            .unwrap_or_else(|| tx.origin_address())
            .into();
        connection.with_clarity_db_readonly(|db| -> Result<(), AdmissionError> {
            // TODO(upstream): Share check_transaction_nonces and can_include_tx's
            // chaining policy once their node-specific account/error types are extracted.
            for (principal, actual, is_origin) in tx
                .sponsor_address()
                .map(|_| (&payer, tx.get_payer().nonce(), false))
                .into_iter()
                .chain(std::iter::once((&origin, tx.get_origin_nonce(), true)))
            {
                let expected = db
                    .get_account_nonce(principal)
                    .map_err(|e| AdmissionError::message("ServerFailureDatabase", e))?;
                if actual < expected || actual == u64::MAX {
                    return Err(AdmissionError {
                        reason: "BadNonce",
                        data: json!({
                            "expected": expected, "actual": actual,
                            "principal": principal.to_string(), "is_origin": is_origin,
                        }),
                    });
                }
                // Pinned node's MAXIMUM_MEMPOOL_TX_CHAINING is 25; its upper
                // bound is the mined nonce + 1 + that limit. Import the constant
                // when the upstream chaining policy is available to Wasm.
                let maximum = expected.saturating_add(26);
                if actual > maximum {
                    return Err(AdmissionError {
                        reason: "TooMuchChaining",
                        data: json!({
                            "message": "Nonce would exceed chaining limit in mempool",
                            "expected": maximum, "actual": actual,
                            "principal": principal.to_string(), "is_origin": is_origin,
                        }),
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
                        "native transfers do not support post-conditions",
                    ));
                }
                let spent = u128::from(*amount)
                    + if payer == origin {
                        u128::from(tx.get_tx_fee())
                    } else {
                        0
                    };
                require_funds(db, &origin, spent)?;
            }
            require_funds(db, &payer, u128::from(tx.get_tx_fee()))?;
            Ok(())
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
                        data: json!({ "contract_identifier": id.to_string() }),
                    })
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        })
    }
}
