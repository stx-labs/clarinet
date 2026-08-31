use clarity::types::StacksEpochId;
use clarity::util::hash::hex_bytes;
use clarity_types::effects::AssetMap;
use clarity_types::types::PrincipalData;
use stacks_codec::transaction::{TransactionPostCondition, TransactionPostConditionMode};
use stacks_codec::StacksMessageCodec;

/// Parse a post-condition mode as the SDK spells it.
pub fn parse_post_condition_mode(mode: &str) -> Result<TransactionPostConditionMode, String> {
    match mode {
        "allow" => Ok(TransactionPostConditionMode::Allow),
        "deny" => Ok(TransactionPostConditionMode::Deny),
        "originator" => Ok(TransactionPostConditionMode::Originator),
        other => Err(format!(
            "invalid post-condition mode '{other}': expected 'allow', 'deny' or 'originator'"
        )),
    }
}

/// The post-condition check to apply to a transaction's asset movement.
///
/// Mirrors [`super::interpreter::NonceCharge`]: the caller names what the
/// execution owes, and the interpreter applies it inside the execution's own
/// database transaction.
#[derive(Debug, Clone, Default, PartialEq)]
pub enum PostConditionCheck {
    /// Asset movement is unconstrained, as it is for a read-only call or for a
    /// transaction that carries no post-conditions.
    #[default]
    Unchecked,
    /// Constrain asset movement. A violation aborts the payload but still
    /// consumes a nonce, the way mainnet treats an `AbortedByCallback`.
    Checked {
        conditions: Vec<TransactionPostCondition>,
        mode: TransactionPostConditionMode,
        /// The principal whose *unlisted* asset movement `Deny` and
        /// `Originator` modes constrain.
        origin: PrincipalData,
    },
}

impl PostConditionCheck {
    /// Decode consensus-serialized post-conditions.
    ///
    /// The SDK sends the same bytes a wallet would put on the wire, so simnet
    /// accepts exactly the input mainnet does.
    pub fn from_hex(
        conditions: &[String],
        mode: TransactionPostConditionMode,
        origin: PrincipalData,
    ) -> Result<Self, String> {
        let conditions = conditions
            .iter()
            .map(|hex| {
                let bytes = hex_bytes(hex.strip_prefix("0x").unwrap_or(hex))
                    .map_err(|e| format!("invalid post-condition hex: {e}"))?;
                TransactionPostCondition::consensus_deserialize(&mut bytes.as_slice())
                    .map_err(|e| format!("invalid post-condition: {e}"))
            })
            .collect::<Result<_, _>>()?;

        Ok(Self::Checked {
            conditions,
            mode,
            origin,
        })
    }

    /// Reject a transaction whose post-conditions this epoch does not support.
    ///
    /// Mainnet does this in `process_transaction_precheck`, before any of the
    /// transaction runs, and treats a failure as `InvalidStacksTransaction` —
    /// so the caller must reject rather than abort, consuming no nonce.
    pub fn validate_for_epoch(&self, epoch: StacksEpochId) -> Result<(), String> {
        let Self::Checked {
            conditions, mode, ..
        } = self
        else {
            return Ok(());
        };

        stacks_transactions::check_post_conditions_supported_in_epoch(conditions, mode, epoch)
            .map_err(|reason| format!("Invalid Stacks transaction: {reason}"))
    }

    /// Why `asset_map` violates this check, if it does.
    ///
    /// `Ok(None)` means the transaction may commit. `Err` means the check
    /// itself could not run, which mainnet treats as fatal.
    pub fn evaluate(
        &self,
        asset_map: &AssetMap,
        epoch: StacksEpochId,
    ) -> Result<Option<String>, String> {
        let Self::Checked {
            conditions,
            mode,
            origin,
        } = self
        else {
            return Ok(None);
        };

        stacks_transactions::check_transaction_postconditions(
            conditions, mode, origin, asset_map, epoch,
        )
        .map_err(|e| format!("failed to evaluate post-conditions: {e}"))
    }
}

#[cfg(test)]
mod tests {
    use clarity::util::hash::to_hex;
    use stacks_codec::transaction::{FungibleConditionCode, PostConditionPrincipal};

    use super::*;

    const SENDER: &str = "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5";

    fn principal(address: &str) -> PrincipalData {
        PrincipalData::parse(address).expect("BUG: not a principal")
    }

    /// A condition requiring `address` to send exactly `amount` uSTX.
    fn sends_exactly(address: &str, amount: u64) -> TransactionPostCondition {
        TransactionPostCondition::STX(
            PostConditionPrincipal::Standard(match principal(address) {
                PrincipalData::Standard(standard) => standard.into(),
                PrincipalData::Contract(_) => unreachable!("BUG: not a standard principal"),
            }),
            FungibleConditionCode::SentEq,
            amount,
        )
    }

    fn hex_of(condition: &TransactionPostCondition) -> String {
        let mut bytes = vec![];
        condition
            .consensus_serialize(&mut bytes)
            .expect("BUG: failed to serialize to a vec");
        to_hex(&bytes)
    }

    fn stx_sent(address: &str, amount: u128) -> AssetMap {
        let mut asset_map = AssetMap::new();
        asset_map
            .add_stx_transfer(&principal(address), amount)
            .expect("BUG: failed to record an STX transfer");
        asset_map
    }

    fn checked(conditions: Vec<TransactionPostCondition>) -> PostConditionCheck {
        PostConditionCheck::Checked {
            conditions,
            mode: TransactionPostConditionMode::Deny,
            origin: principal(SENDER),
        }
    }

    #[test]
    fn parses_every_mode_the_sdk_can_send() {
        assert_eq!(
            parse_post_condition_mode("allow"),
            Ok(TransactionPostConditionMode::Allow)
        );
        assert_eq!(
            parse_post_condition_mode("deny"),
            Ok(TransactionPostConditionMode::Deny)
        );
        assert_eq!(
            parse_post_condition_mode("originator"),
            Ok(TransactionPostConditionMode::Originator)
        );
        assert!(parse_post_condition_mode("Deny").is_err());
        assert!(parse_post_condition_mode("").is_err());
    }

    #[test]
    fn decodes_conditions_with_or_without_the_hex_prefix() {
        let expected = sends_exactly(SENDER, 100);
        let hex = hex_of(&expected);

        for encoded in [hex.clone(), format!("0x{hex}")] {
            let check = PostConditionCheck::from_hex(
                &[encoded.clone()],
                TransactionPostConditionMode::Deny,
                principal(SENDER),
            )
            .unwrap_or_else(|e| panic!("{encoded} should decode: {e}"));

            assert_eq!(check, checked(vec![expected.clone()]));
        }
    }

    #[test]
    fn rejects_input_that_is_not_a_post_condition() {
        let bad_hex = PostConditionCheck::from_hex(
            &["nothex".to_string()],
            TransactionPostConditionMode::Deny,
            principal(SENDER),
        );
        assert!(bad_hex.is_err_and(|e| e.contains("invalid post-condition hex")));

        // Well-formed hex, but 0xff is not a post-condition type byte.
        let bad_condition = PostConditionCheck::from_hex(
            &["ff".to_string()],
            TransactionPostConditionMode::Deny,
            principal(SENDER),
        );
        assert!(bad_condition.is_err_and(|e| e.contains("invalid post-condition")));
    }

    #[test]
    fn an_unchecked_transaction_never_fails() {
        let check = PostConditionCheck::Unchecked;

        assert_eq!(check.validate_for_epoch(StacksEpochId::Epoch21), Ok(()));
        assert_eq!(
            check.evaluate(&stx_sent(SENDER, 100), StacksEpochId::Epoch21),
            Ok(None)
        );
    }

    #[test]
    fn originator_mode_is_rejected_before_the_epoch_supports_it() {
        let check = PostConditionCheck::Checked {
            conditions: vec![],
            mode: TransactionPostConditionMode::Originator,
            origin: principal(SENDER),
        };

        assert!(check
            .validate_for_epoch(StacksEpochId::Epoch33)
            .is_err_and(|e| e.contains("Originator post-condition mode is not supported")));
        assert_eq!(check.validate_for_epoch(StacksEpochId::Epoch34), Ok(()));
    }

    #[test]
    fn a_satisfied_condition_passes_and_a_violated_one_reports_why() {
        let check = checked(vec![sends_exactly(SENDER, 100)]);

        assert_eq!(
            check.evaluate(&stx_sent(SENDER, 100), StacksEpochId::Epoch33),
            Ok(None)
        );

        let violation = check
            .evaluate(&stx_sent(SENDER, 99), StacksEpochId::Epoch33)
            .expect("the check should run");
        assert!(violation.is_some_and(|reason| reason.contains("Post-condition check failure")));
    }

    #[test]
    fn deny_mode_rejects_movement_no_condition_covers() {
        let check = checked(vec![]);

        // Nothing moved, so there is nothing to leave unchecked.
        assert_eq!(
            check.evaluate(&AssetMap::new(), StacksEpochId::Epoch33),
            Ok(None)
        );

        let violation = check
            .evaluate(&stx_sent(SENDER, 1), StacksEpochId::Epoch33)
            .expect("the check should run");
        assert!(violation.is_some(), "unlisted movement should be denied");
    }

    #[test]
    fn allow_mode_permits_movement_no_condition_covers() {
        let check = PostConditionCheck::Checked {
            conditions: vec![],
            mode: TransactionPostConditionMode::Allow,
            origin: principal(SENDER),
        };

        assert_eq!(
            check.evaluate(&stx_sent(SENDER, 1), StacksEpochId::Epoch33),
            Ok(None)
        );
    }
}
