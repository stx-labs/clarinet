//! Signed transaction admission and execution over the shared Clarity frame.

use std::collections::BTreeSet;
use std::{iter, mem};

use clarity::vm::clarity::{ClarityConnection, ClarityError, TransactionConnection};
use clarity::vm::database::ClarityDatabase;
use clarity::vm::errors::VmExecutionError;
use clarity::vm::events::StacksTransactionEvent;
use clarity::vm::hooks::EvalHook;
use clarity::vm::resource_limiter::ResourceBudget;
use clarity::vm::{
    ClarityVersion, ContractEvaluationResult, CostSynthesis, EvaluationResult, ExecutionResult,
    ParsedContract, SnippetEvaluationResult,
};
use clarity_types::effects::AssetMap;
use clarity_types::types::{
    BoundedErrorString, BuffData, PrincipalData, QualifiedContractIdentifier, ResponseData,
};
use clarity_types::Value;
use stacks_codec::transaction::{
    StacksTransaction, TransactionAuthVerificationMode, TransactionContractCall,
    TransactionPayload, MAX_TRANSACTION_LEN,
};
use stacks_codec::StacksMessageCodec;

use super::interpreter::{public_function_args, BlockInclusion, ClarityInterpreter, NonceCharge};
use super::post_conditions::PostConditionCheck;
use super::transaction::SimnetTransactionConnection;

#[derive(Debug, Clone)]
pub struct RawTransactionMetadata {
    pub txid: String,
    pub fee: u64,
    pub nonce: u64,
    /// Preserve the signed call for SDK cost reports without decoding it again.
    pub contract_call: Option<TransactionContractCall>,
}

#[derive(Debug)]
pub enum RawTransactionOutcome {
    Included {
        metadata: RawTransactionMetadata,
        result: RawTransactionResult,
    },
    Rejected {
        metadata: Option<RawTransactionMetadata>,
        error: String,
    },
}

#[derive(Debug)]
pub enum RawTransactionResult {
    Success(Box<ExecutionResult>),
    Failure {
        kind: RawFailureKind,
        error: String,
        output: Option<Box<Value>>,
        events: Vec<StacksTransactionEvent>,
        cost: CostSynthesis,
    },
}

#[derive(Debug, Clone, Copy, strum::IntoStaticStr)]
pub enum RawFailureKind {
    #[strum(serialize = "abort_by_response")]
    Response,
    #[strum(serialize = "runtime_error")]
    Runtime,
    #[strum(serialize = "analysis_error")]
    Analysis,
    #[strum(serialize = "abort_by_post_condition")]
    PostCondition,
}

impl From<&StacksTransaction> for RawTransactionMetadata {
    fn from(tx: &StacksTransaction) -> Self {
        Self {
            txid: tx.txid().to_string(),
            fee: tx.get_tx_fee(),
            nonce: tx.get_origin_nonce(),
            contract_call: match &tx.payload {
                TransactionPayload::ContractCall(call) => Some(call.clone()),
                _ => None,
            },
        }
    }
}

impl RawTransactionOutcome {
    pub(crate) fn rejected(tx: Option<&StacksTransaction>, error: String) -> Self {
        Self::Rejected {
            metadata: tx.map(RawTransactionMetadata::from),
            error,
        }
    }

    pub fn is_included(&self) -> bool {
        matches!(self, Self::Included { .. })
    }
}

impl ClarityInterpreter {
    pub(crate) fn execute_transaction(
        &mut self,
        tx: &StacksTransaction,
        hooks: Vec<&mut dyn EvalHook>,
    ) -> RawTransactionOutcome {
        let result = self.execute_raw_transaction(tx, hooks);
        let metadata = RawTransactionMetadata::from(tx);
        match result {
            Ok(result) => RawTransactionOutcome::Included { metadata, result },
            Err(error) => RawTransactionOutcome::Rejected {
                metadata: Some(metadata),
                error,
            },
        }
    }

    pub(crate) fn check_raw_transaction(&self, tx: &StacksTransaction) -> Result<(), String> {
        let epoch = self.datastore.get_current_epoch();
        let config = self.transaction_config(epoch);
        // TODO(upstream): Extract StacksChainState::process_transaction_precheck
        // and StacksBlock::validate_transaction_static_epoch into Wasm-compatible
        // helpers so their ordering and epoch gates need not be mirrored here.
        if !tx.auth.is_supported_in_epoch(epoch) {
            return Err("authentication mode is unsupported in this epoch".into());
        }
        let verification = if epoch.allows_tx_signatures_with_high_s() {
            TransactionAuthVerificationMode::AllowHighS
        } else {
            TransactionAuthVerificationMode::EnforceLowS
        };
        tx.verify(verification).map_err(|e| e.to_string())?;
        if tx.chain_id != config.chain_id || tx.is_mainnet() != config.mainnet {
            return Err("transaction network or chain ID does not match this session".into());
        }
        stacks_transactions::check_post_conditions_supported_in_epoch(
            &tx.post_conditions,
            &tx.post_condition_mode,
            epoch,
        )
        .map_err(|e| e.to_string())?;
        if let TransactionPayload::SmartContract(_, Some(version)) = &tx.payload {
            stacks_transactions::check_versioned_deploy_supported_in_epoch(*version, epoch)
                .map_err(|e| e.to_string())?;
        }
        if !matches!(
            tx.payload,
            TransactionPayload::TokenTransfer(..)
                | TransactionPayload::ContractCall(..)
                | TransactionPayload::SmartContract(..)
        ) {
            return Err("transaction payload is unsupported in simnet".into());
        }
        Ok(())
    }

    fn execute_raw_transaction(
        &mut self,
        tx: &StacksTransaction,
        hooks: Vec<&mut dyn EvalHook>,
    ) -> Result<RawTransactionResult, String> {
        self.check_raw_transaction(tx)?;
        let epoch = self.datastore.get_current_epoch();
        let origin: PrincipalData = tx.origin_address().into();
        let sponsor: Option<PrincipalData> = tx.sponsor_address().map(Into::into);
        let payer = sponsor.as_ref().unwrap_or(&origin);
        let mut connection = self.get_transaction_connection(epoch, true, hooks)?;
        connection.begin();
        let admission = admit(connection.db(), tx, &origin, payer);
        let nonces = match admission {
            Ok(nonces) => nonces,
            Err(error) => {
                connection.settle(&NonceCharge::Free, BlockInclusion::Rejected)?;
                connection.did_complete(Err(error.clone()));
                return Err(error);
            }
        };
        let mut analysis_failure = false;
        let mut callback_error = None;
        let conditions = PostConditionCheck::Checked {
            conditions: tx.post_conditions.clone(),
            mode: tx.post_condition_mode,
            origin: origin.clone(),
        };
        let payload = execute_payload(
            &mut connection,
            tx,
            &origin,
            sponsor.as_ref(),
            conditions.abort_callback(epoch, &mut callback_error),
            &mut analysis_failure,
        );
        let cost = CostSynthesis::from_cost_tracker(connection.cost_tracker());
        let mut result = match payload {
            Ok((result, events)) => Ok(RawTransactionResult::Success(Box::new(ExecutionResult {
                result,
                events,
                cost: Some(cost),
                diagnostics: vec![],
            }))),
            Err(error) => {
                let message = error.to_string();
                let (kind, output, events) = match &error {
                    ClarityError::AbortedByCallback {
                        output, tx_events, ..
                    } => (
                        RawFailureKind::PostCondition,
                        output.clone(),
                        tx_events.clone(),
                    ),
                    _ if analysis_failure => (RawFailureKind::Analysis, None, vec![]),
                    _ => (RawFailureKind::Runtime, None, vec![]),
                };
                let inclusion = if analysis_failure {
                    BlockInclusion::from_analysis_error(error, epoch)
                } else {
                    BlockInclusion::from_runtime_error(error, epoch)
                };
                if inclusion.is_included() {
                    Ok(RawTransactionResult::Failure {
                        kind,
                        error: message,
                        output,
                        events,
                        cost,
                    })
                } else {
                    Err(message)
                }
            }
        };
        if let Some(error) = callback_error {
            result = Err(error);
        }
        let (mut credits, mut debits) = (vec![], vec![]);
        if let Ok(RawTransactionResult::Success(execution)) = &mut result {
            (execution.events, credits, debits) = Self::process_events(&mut execution.events);
        }
        // STX reporting must come from committed balances, including remote
        // accounts that were never seeded in the local asset mirror.
        let is_stx = |(_, token, _): &mut (String, String, u128)| token == "STX";
        let stx_accounts: BTreeSet<_> = credits
            .extract_if(.., is_stx)
            .chain(debits.extract_if(.., is_stx))
            .map(|(account, ..)| account)
            .chain(iter::once(payer.to_string()))
            .collect();
        let mut stx_balances = vec![];
        if result.is_ok() {
            match commit_accounts(connection.db(), stx_accounts, nonces) {
                Ok(balances) => stx_balances = balances,
                Err(error) => result = Err(error.to_string()),
            }
        }
        connection.settle(&NonceCharge::Free, BlockInclusion::from(result.is_ok()))?;
        match &mut result {
            Ok(RawTransactionResult::Success(execution)) => connection.did_complete(Ok(execution)),
            Ok(RawTransactionResult::Failure { error, .. }) | Err(error) => {
                connection.did_complete(Err(error.clone()))
            }
        }
        drop(connection);
        let mut result = result?;
        // Only committed payload events update the reporting mirror.
        for (account, token, amount) in credits {
            self.credit_token(account, token, amount);
        }
        for (account, token, amount) in debits {
            self.debit_token(account, token, amount);
        }
        for (account, balance) in stx_balances {
            self.set_stx_reporting_balance(account, balance);
        }
        if let RawTransactionResult::Success(execution) = &mut result {
            if let EvaluationResult::Snippet(SnippetEvaluationResult {
                result:
                    output @ Value::Response(ResponseData {
                        committed: false, ..
                    }),
            }) = &mut execution.result
            {
                result = RawTransactionResult::Failure {
                    kind: RawFailureKind::Response,
                    error: "contract returned an error response".into(),
                    output: Some(Box::new(mem::replace(output, Value::none()))),
                    events: mem::take(&mut execution.events),
                    cost: execution.cost.take().expect("raw transactions track costs"),
                };
            }
        }
        Ok(result)
    }
}

pub(crate) fn decode(raw_hex: &str) -> Result<StacksTransaction, String> {
    let hex = raw_hex.strip_prefix("0x").unwrap_or(raw_hex);
    if hex.len() > MAX_TRANSACTION_LEN as usize * 2 {
        return Err("transaction exceeds maximum length".into());
    }
    let bytes =
        clarity::util::hash::hex_bytes(hex).map_err(|e| format!("invalid transaction hex: {e}"))?;
    let mut input = bytes.as_slice();
    let tx = StacksTransaction::consensus_deserialize(&mut input)
        .map_err(|e| format!("invalid transaction: {e}"))?;
    if !input.is_empty() {
        return Err("trailing bytes after transaction".into());
    }
    Ok(tx)
}

fn admit(
    db: &mut ClarityDatabase,
    tx: &StacksTransaction,
    origin: &PrincipalData,
    payer: &PrincipalData,
) -> Result<Vec<(PrincipalData, u64)>, String> {
    // TODO(upstream): Reuse StacksChainState::check_transaction_nonces once its
    // account/error types and helper are available outside native-only stackslib.
    let origin_nonce = db.get_account_nonce(origin).map_err(|e| e.to_string())?;
    // stacks-node validates a sponsor's nonce before the origin's.
    if tx.sponsor_address().is_some() {
        let payer_nonce = db.get_account_nonce(payer).map_err(|e| e.to_string())?;
        if payer_nonce != tx.get_payer().nonce() {
            return Err(format!("sponsor nonce mismatch: expected {payer_nonce}"));
        }
    }
    if origin_nonce != tx.get_origin_nonce() {
        return Err(format!("origin nonce mismatch: expected {origin_nonce}"));
    }
    let mut nonces = vec![(
        origin.clone(),
        origin_nonce.checked_add(1).ok_or("origin nonce overflow")?,
    )];
    if payer != origin {
        nonces.push((
            payer.clone(),
            tx.get_payer()
                .nonce()
                .checked_add(1)
                .ok_or("sponsor nonce overflow")?,
        ));
    }
    let mut balance = db
        .get_stx_balance_snapshot(payer)
        .map_err(|e| e.to_string())?;
    let fee = u128::from(tx.get_tx_fee());
    if !balance.can_transfer(fee).map_err(|e| e.to_string())? {
        return Err("insufficient unlocked balance for transaction fee".into());
    }
    // TODO: Before epoch 2.1 the node charges fees after payload execution.
    balance.debit(fee).map_err(|e| e.to_string())?;
    balance.save().map_err(|e| e.to_string())?;
    Ok(nonces)
}

/// Read the committed STX balances to report, then charge the admitted nonces.
fn commit_accounts(
    db: &mut ClarityDatabase,
    stx_accounts: BTreeSet<String>,
    nonces: Vec<(PrincipalData, u64)>,
) -> Result<Vec<(String, u128)>, VmExecutionError> {
    let balances = stx_accounts
        .into_iter()
        .map(|account| {
            let principal = PrincipalData::parse(&account)?;
            let balance = db
                .get_account_stx_balance(&principal)?
                .get_total_balance()?;
            Ok((account, balance))
        })
        .collect::<Result<_, VmExecutionError>>()?;
    for (principal, nonce) in nonces {
        db.set_account_nonce(&principal, nonce)?;
    }
    Ok(balances)
}

fn execute_payload(
    connection: &mut SimnetTransactionConnection,
    tx: &StacksTransaction,
    origin: &PrincipalData,
    sponsor: Option<&PrincipalData>,
    callback: impl FnOnce(&AssetMap, &mut ClarityDatabase) -> Option<BoundedErrorString>,
    analysis_failure: &mut bool,
) -> Result<(EvaluationResult, Vec<StacksTransactionEvent>), ClarityError> {
    let epoch = connection.get_epoch();
    let budget = ResourceBudget::unlimited();
    match &tx.payload {
        TransactionPayload::TokenTransfer(recipient, amount, memo) => {
            if !tx.post_conditions.is_empty() || recipient == origin || *amount == 0 {
                return Err(ClarityError::BadTransaction(
                    "invalid native transfer".into(),
                ));
            }
            let (result, _, events) = connection
                .run_stx_transfer(
                    origin,
                    recipient,
                    (*amount).into(),
                    &BuffData {
                        data: memo.0.to_vec(),
                    },
                )
                .map_err(|e| ClarityError::BadTransaction(e.to_string()))?;
            Ok((
                EvaluationResult::Snippet(SnippetEvaluationResult { result }),
                events,
            ))
        }
        TransactionPayload::ContractCall(call) => {
            let (result, _, events) = connection.run_contract_call(
                origin,
                sponsor,
                &call.to_clarity_contract_id(),
                &call.function_name,
                &call.function_args,
                callback,
                &budget,
            )?;
            Ok((
                EvaluationResult::Snippet(SnippetEvaluationResult { result }),
                events,
            ))
        }
        TransactionPayload::SmartContract(contract, version) => {
            let id =
                QualifiedContractIdentifier::new(tx.origin_address().into(), contract.name.clone());
            if connection.db().has_contract(&id) {
                return Err(ClarityError::BadTransaction(format!(
                    "Duplicate contract '{id}'"
                )));
            }
            let source = contract.code_body.to_string();
            let version = version.unwrap_or(ClarityVersion::default_for_epoch(epoch));
            let (ast, analysis) = connection
                .analyze_smart_contract(&id, version, &source, &budget)
                .inspect_err(|_| *analysis_failure = true)?;
            let (_, events) = connection.initialize_smart_contract(
                &id,
                version,
                &ast,
                &source,
                sponsor.cloned(),
                callback,
                &budget,
            )?;
            let stored = connection.with_clarity_db_readonly(|db| db.get_contract(&id))?;
            let function_args = public_function_args(&stored);
            connection.save_analysis(&id, &analysis)?;
            let contract = ParsedContract {
                contract_identifier: id.to_string(),
                code: source,
                function_args,
                ast,
                analysis,
            };
            Ok((
                EvaluationResult::Contract(ContractEvaluationResult {
                    result: Some(Value::okay_true()),
                    contract,
                }),
                events,
            ))
        }
        _ => Err(ClarityError::BadTransaction("unsupported payload".into())),
    }
}

#[cfg(test)]
mod tests {
    use clarity::types::StacksEpochId;
    use serde_json::Value as Json;
    use stacks_codec::transaction::TransactionVersion;

    use super::*;
    use crate::repl::{Session, SessionSettings};

    fn fixtures() -> Json {
        serde_json::from_str(include_str!(
            "../../../clarinet-sdk/node/tests/fixtures/raw-transactions/transactions.json"
        ))
        .unwrap()
    }

    fn principal(fixtures: &Json, key: &str) -> PrincipalData {
        PrincipalData::parse(fixtures[key].as_str().unwrap()).unwrap()
    }

    fn session(fixtures: &Json) -> Session {
        let mut session = Session::new(SessionSettings {
            epoch_id: Some(StacksEpochId::Epoch34),
            ..SessionSettings::default()
        });
        for key in ["sender", "sponsor"] {
            session
                .interpreter
                .mint_stx_balance(principal(fixtures, key), 1_000_000)
                .unwrap();
        }
        session
    }

    fn broadcast(session: &mut Session, fixtures: &Json, name: &str) -> RawTransactionOutcome {
        session.execute_raw_transaction(fixtures["transactions"][name]["hex"].as_str().unwrap())
    }

    fn balance(session: &mut Session, principal: &PrincipalData) -> u128 {
        let epoch = session.interpreter.datastore.get_current_epoch();
        let mut context = session
            .interpreter
            .get_global_context(epoch, false)
            .unwrap();
        context.begin();
        let balance = context
            .database
            .get_stx_balance_snapshot(principal)
            .unwrap()
            .get_available_balance()
            .unwrap();
        context.roll_back().unwrap();
        balance
    }

    #[test]
    fn signed_fixtures_match_stacks_js_and_settle_on_native() {
        let fixtures = fixtures();
        for name in ["transfer", "deploy", "sponsored", "same_sponsor"] {
            let mut session = session(&fixtures);
            let outcome = broadcast(&mut session, &fixtures, name);
            let RawTransactionOutcome::Included {
                metadata,
                result: RawTransactionResult::Success(_),
            } = outcome
            else {
                panic!("{name}: {outcome:?}");
            };
            assert_eq!(
                metadata.txid,
                fixtures["transactions"][name]["txid"].as_str().unwrap()
            );
            let sender = principal(&fixtures, "sender");
            assert_eq!(session.interpreter.get_nonce(&sender).unwrap(), 1);
            assert!(!broadcast(&mut session, &fixtures, name).is_included());
        }
    }

    #[test]
    fn rejected_payload_rolls_back_fee_and_nonce() {
        let fixtures = fixtures();
        for name in [
            "wrong_nonce",
            "wrong_network",
            "zero",
            "self",
            "insufficient",
            "insufficient_fee",
        ] {
            let mut session = session(&fixtures);
            let sender = principal(&fixtures, "sender");
            let outcome = broadcast(&mut session, &fixtures, name);
            assert!(!outcome.is_included(), "{name}: {outcome:?}");
            assert_eq!(balance(&mut session, &sender), 1_000_000);
            assert_eq!(session.interpreter.get_nonce(&sender).unwrap(), 0);
        }
    }

    #[test]
    fn included_failures_keep_fee_and_nonce() {
        let fixtures = fixtures();
        for name in ["fail", "crash", "abort"] {
            let mut session = session(&fixtures);
            assert!(broadcast(&mut session, &fixtures, "deploy").is_included());
            let outcome = broadcast(&mut session, &fixtures, name);
            assert!(
                matches!(
                    outcome,
                    RawTransactionOutcome::Included {
                        result: RawTransactionResult::Failure { .. },
                        ..
                    }
                ),
                "{name}: {outcome:?}"
            );
            let sender = principal(&fixtures, "sender");
            assert_eq!(balance(&mut session, &sender), 999_980);
            assert_eq!(session.interpreter.get_nonce(&sender).unwrap(), 2);
        }
    }

    fn signed_hex(mut tx: StacksTransaction) -> String {
        use stacks_common::types::chainstate::StacksPrivateKey;

        let key = StacksPrivateKey::from_hex(&format!("{:064x}01", 1)).unwrap();
        tx.sign_next_origin(&tx.sign_begin(), &key).unwrap();
        tx.verify(TransactionAuthVerificationMode::EnforceLowS)
            .unwrap();
        clarity::util::hash::to_hex(&tx.serialize_to_vec())
    }

    #[test]
    fn mempool_policy_does_not_change_signed_block_execution() {
        use stacks_codec::strings::StacksString;

        let fixtures = fixtures();
        let mut session = session(&fixtures);
        let sender = principal(&fixtures, "sender");
        let mut deployment =
            decode(fixtures["transactions"]["deploy"]["hex"].as_str().unwrap()).unwrap();
        let TransactionPayload::SmartContract(contract, _) = &mut deployment.payload else {
            panic!("deployment fixture");
        };
        contract.code_body =
            StacksString::from_str("(define-read-only (increment) (ok u1))").unwrap();
        assert!(session
            .execute_raw_transaction_in_next_block(&signed_hex(deployment))
            .is_included());
        let height = session
            .interpreter
            .datastore
            .get_current_stacks_block_height();
        let funds = balance(&mut session, &sender);
        let raw = fixtures["transactions"]["increment"]["hex"]
            .as_str()
            .unwrap();
        let mut tx = decode(raw).unwrap();
        tx.set_tx_fee(1000);
        let raw = signed_hex(tx);
        let rejected = session.submit_transaction(&raw);
        assert!(matches!(
            rejected,
            crate::repl::mempool::AdmissionResult::Rejected {
                reason: "NoSuchPublicFunction",
                ..
            }
        ));
        assert_eq!(session.interpreter.get_nonce(&sender).unwrap(), 1);
        assert_eq!(balance(&mut session, &sender), funds);
        assert_eq!(
            session
                .interpreter
                .datastore
                .get_current_stacks_block_height(),
            height
        );

        // A miner can include the same signed bytes despite mempool policy.
        assert!(matches!(
            session.execute_raw_transaction(&raw),
            RawTransactionOutcome::Included {
                result: RawTransactionResult::Success(_),
                ..
            }
        ));
        assert_eq!(session.interpreter.get_nonce(&sender).unwrap(), 2);
        assert_eq!(balance(&mut session, &sender), funds - 1000);
    }

    #[test]
    fn mempool_admission_is_read_only_and_duplicates_are_idempotent() {
        use crate::repl::mempool::AdmissionResult;
        let fixtures = fixtures();
        let mut session = session(&fixtures);
        let sender = principal(&fixtures, "sender");
        let mut tx = decode(
            fixtures["transactions"]["transfer"]["hex"]
                .as_str()
                .unwrap(),
        )
        .unwrap();
        tx.set_tx_fee(1000);
        let raw = signed_hex(tx);
        let txid = decode(&raw).unwrap().txid();
        for _ in 0..2 {
            let result = session.submit_transaction(&raw);
            assert!(
                matches!(&result, AdmissionResult::Accepted { txid: actual } if actual == &txid)
            );
            assert_eq!(
                serde_json::to_value(&result).unwrap(),
                serde_json::json!({ "txid": txid.to_string() })
            );
            assert_eq!(balance(&mut session, &sender), 1_000_000);
            assert_eq!(session.interpreter.get_nonce(&sender).unwrap(), 0);
        }
        let queued = session.next_mempool_transaction().unwrap();
        assert!(session.next_mempool_transaction().is_none());
        assert!(session.execute_transaction(&queued).is_included());
        assert_eq!(balance(&mut session, &sender), 998_900);
        assert_eq!(session.interpreter.get_nonce(&sender).unwrap(), 1);
        let result = session.submit_transaction(&raw);
        assert!(matches!(
            &result,
            AdmissionResult::Rejected {
                reason: "BadNonce",
                txid: Some(actual),
                ..
            } if actual == &txid
        ));
        assert_eq!(
            serde_json::to_value(&result).unwrap()["txid"],
            txid.to_string()
        );
    }

    #[test]
    fn nonce_overflow_rejects_before_fee_debit() {
        let fixtures = fixtures();
        let mut session = session(&fixtures);
        let sender = principal(&fixtures, "sender");
        session.interpreter.force_nonce(&sender, u64::MAX);
        assert!(!broadcast(&mut session, &fixtures, "overflow_nonce").is_included());
        assert_eq!(balance(&mut session, &sender), 1_000_000);
        assert_eq!(session.interpreter.get_nonce(&sender).unwrap(), u64::MAX);
    }

    #[test]
    fn locked_stx_cannot_pay_fees() {
        let fixtures = fixtures();
        let mut session = session(&fixtures);
        let sender = principal(&fixtures, "sender");
        let epoch = session.interpreter.datastore.get_current_epoch();
        {
            let mut context = session
                .interpreter
                .get_global_context(epoch, false)
                .unwrap();
            context.begin();
            let mut snapshot = context.database.get_stx_balance_snapshot(&sender).unwrap();
            snapshot.lock_tokens_v5(1_000_000, 1_000_000).unwrap();
            snapshot.save().unwrap();
            context.commit().unwrap();
        }
        assert!(!broadcast(&mut session, &fixtures, "transfer").is_included());
        assert_eq!(balance(&mut session, &sender), 0);
        assert_eq!(session.interpreter.get_nonce(&sender).unwrap(), 0);
    }

    #[test]
    fn reporting_balances_do_not_require_locally_seeded_accounts() {
        let fixtures = fixtures();
        let mut session = Session::new(SessionSettings {
            epoch_id: Some(StacksEpochId::Epoch34),
            ..SessionSettings::default()
        });
        let sender = principal(&fixtures, "sender");
        {
            let mut context = session
                .interpreter
                .get_global_context(StacksEpochId::Epoch34, false)
                .unwrap();
            context.begin();
            let mut snapshot = context.database.get_stx_balance_snapshot(&sender).unwrap();
            snapshot.credit(1_000_000).unwrap();
            snapshot.save().unwrap();
            context.commit().unwrap();
        }
        assert!(broadcast(&mut session, &fixtures, "transfer").is_included());
        assert_eq!(
            session.interpreter.get_assets_maps()["STX"][&sender.to_string()],
            999_890
        );
    }

    #[test]
    fn unsupported_signed_payload_is_rejected() {
        use stacks_codec::transaction::CoinbasePayload;
        use stacks_common::types::chainstate::StacksPrivateKey;
        let fixtures = fixtures();
        let mut session = session(&fixtures);
        let original = decode(
            fixtures["transactions"]["transfer"]["hex"]
                .as_str()
                .unwrap(),
        )
        .unwrap();
        let mut tx = StacksTransaction::new(
            TransactionVersion::Testnet,
            original.auth,
            TransactionPayload::Coinbase(CoinbasePayload([0; 32]), None, None),
        );
        tx.chain_id = clarity::consts::CHAIN_ID_TESTNET;
        let key = StacksPrivateKey::from_hex(&format!("{:064x}01", 1)).unwrap();
        tx.sign_next_origin(&tx.sign_begin(), &key).unwrap();
        tx.verify(TransactionAuthVerificationMode::EnforceLowS)
            .unwrap();
        let error = session
            .interpreter
            .execute_raw_transaction(&tx, vec![])
            .unwrap_err();
        assert!(error.contains("unsupported in simnet"));
    }

    #[test]
    fn decode_requires_one_complete_bounded_transaction() {
        let fixtures = fixtures();
        let hex = fixtures["transactions"]["transfer"]["hex"]
            .as_str()
            .unwrap();
        assert!(decode(&format!("{hex}00")).is_err());
        assert!(decode("not hex").is_err());
        assert!(decode(&"00".repeat(MAX_TRANSACTION_LEN as usize + 1)).is_err());
        assert_eq!(
            decode(&format!("0x{hex}")).unwrap().txid(),
            decode(hex).unwrap().txid()
        );
    }
}
