use clarity::types::StacksEpochId;
use clarity::util::hash::hex_bytes;
use clarity_types::effects::AssetMap;
use clarity_types::types::PrincipalData;
use stacks_codec::transaction::{
    AssetInfo, PostConditionPrincipal, TransactionPostCondition, TransactionPostConditionMode,
};
use stacks_codec::StacksMessageCodec;

use super::boot::remap_mainnet_boot_stacks_address;

/// Default to rejecting asset movement that no condition covers.
pub const DEFAULT_POST_CONDITION_MODE: TransactionPostConditionMode =
    TransactionPostConditionMode::Deny;

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
#[derive(Debug, Clone, Default, PartialEq)]
pub enum PostConditionCheck {
    /// Asset movement is unconstrained.
    #[default]
    Unchecked,
    /// Constrain asset movement.
    Checked {
        conditions: Vec<TransactionPostCondition>,
        mode: TransactionPostConditionMode,
        /// The principal whose *unlisted* asset movement `Deny` and
        /// `Originator` modes constrain.
        origin: PrincipalData,
    },
}

impl PostConditionCheck {
    /// Whether this check contains any declared post-conditions.
    pub fn has_conditions(&self) -> bool {
        matches!(self, Self::Checked { conditions, .. } if !conditions.is_empty())
    }

    /// Move any mainnet boot principal in these conditions onto its testnet
    /// twin, matching the redirect the session applies to the call itself.
    ///
    /// Without this the two spellings are not equivalent. `bns` issues an NFT
    /// and `cost-voting` an FT, so a call written against `SP000....bns` with
    /// a post-condition on `SP000....bns::names` executes `ST000....bns`, the
    /// asset moves under `ST000....bns::names`, and the condition can never
    /// match — a correct post-condition would abort the transaction.
    ///
    /// Only the boot contracts move: sBTC is deployed at its mainnet address
    /// and has no twin, so `sbtc-token` asset identifiers are left alone.
    pub fn remap_mainnet_boot_principals(self) -> Self {
        let Self::Checked {
            conditions,
            mode,
            origin,
        } = self
        else {
            return self;
        };

        let conditions = conditions.into_iter().map(remap_condition).collect();
        Self::Checked {
            conditions,
            mode,
            origin,
        }
    }

    /// Decode consensus-serialized post-conditions.
    ///
    /// The SDK sends the same encoding used on the transaction wire.
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

                let mut remaining = bytes.as_slice();
                let condition = TransactionPostCondition::consensus_deserialize(&mut remaining)
                    .map_err(|e| format!("invalid post-condition: {e}"))?;

                // Each SDK value must contain exactly one condition.
                if !remaining.is_empty() {
                    return Err(format!(
                        "invalid post-condition: {} trailing byte(s)",
                        remaining.len()
                    ));
                }
                Ok(condition)
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
    /// Unsupported conditions make the transaction invalid before execution.
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

/// Redirect a post-condition principal naming a mainnet boot contract.
fn remap_principal(principal: PostConditionPrincipal) -> PostConditionPrincipal {
    match principal {
        PostConditionPrincipal::Contract(address, name) => {
            let address = remap_mainnet_boot_stacks_address(&address, &name).unwrap_or(address);
            PostConditionPrincipal::Contract(address, name)
        }
        // A standard principal carries no contract name, so it can never name
        // a boot contract; `Origin` is resolved against the sender.
        other => other,
    }
}

/// Redirect an asset identifier naming a mainnet boot contract.
fn remap_asset(asset: AssetInfo) -> AssetInfo {
    let AssetInfo {
        contract_address,
        contract_name,
        asset_name,
    } = asset;

    let contract_address = remap_mainnet_boot_stacks_address(&contract_address, &contract_name)
        .unwrap_or(contract_address);

    AssetInfo {
        contract_address,
        contract_name,
        asset_name,
    }
}

fn remap_condition(condition: TransactionPostCondition) -> TransactionPostCondition {
    use TransactionPostCondition::*;

    match condition {
        STX(principal, code, amount) => STX(remap_principal(principal), code, amount),
        Fungible(principal, asset, code, amount) => {
            Fungible(remap_principal(principal), remap_asset(asset), code, amount)
        }
        Nonfungible(principal, asset, value, code) => {
            Nonfungible(remap_principal(principal), remap_asset(asset), value, code)
        }
        Staking(principal, code, amount) => Staking(remap_principal(principal), code, amount),
        Pox(principal, code) => Pox(remap_principal(principal), code),
    }
}

#[cfg(test)]
mod boot_remap_tests {
    use clarity::types::chainstate::StacksAddress;
    use clarity_types::types::Value;
    use clarity_types::{ClarityName, ContractName};
    use stacks_codec::transaction::NonfungibleConditionCode;

    use super::*;
    use crate::repl::boot::{
        BOOT_MAINNET_STACKS_ADDRESS, BOOT_TESTNET_STACKS_ADDRESS, SBTC_MAINNET_ADDRESS,
    };

    fn asset(address: StacksAddress, contract: &str, asset_name: &str) -> AssetInfo {
        AssetInfo {
            contract_address: address,
            contract_name: ContractName::try_from(contract).unwrap(),
            asset_name: ClarityName::try_from(asset_name).unwrap(),
        }
    }

    fn nft(address: StacksAddress, contract: &str, asset_name: &str) -> PostConditionCheck {
        PostConditionCheck::Checked {
            conditions: vec![TransactionPostCondition::Nonfungible(
                PostConditionPrincipal::Contract(
                    address.clone(),
                    ContractName::try_from(contract).unwrap(),
                ),
                asset(address, contract, asset_name),
                Value::UInt(1),
                NonfungibleConditionCode::Sent,
            )],
            mode: TransactionPostConditionMode::Deny,
            origin: PrincipalData::parse("ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5").unwrap(),
        }
    }

    /// `bns` issues the `names` NFT, so a condition spelled with the mainnet
    /// boot address has to follow the call onto the testnet twin.
    #[test]
    fn a_boot_asset_condition_moves_to_the_testnet_twin() {
        let remapped = nft(BOOT_MAINNET_STACKS_ADDRESS.clone(), "bns", "names")
            .remap_mainnet_boot_principals();

        assert_eq!(
            remapped,
            nft(BOOT_TESTNET_STACKS_ADDRESS.clone(), "bns", "names"),
            "both the condition principal and the asset identifier move"
        );
    }

    /// sBTC is deployed at its mainnet address and has no twin, so its assets
    /// must be left exactly as written.
    #[test]
    fn an_sbtc_asset_condition_is_untouched() {
        let sbtc = StacksAddress::from(
            PrincipalData::parse_standard_principal(SBTC_MAINNET_ADDRESS).unwrap(),
        );
        let check = nft(sbtc, "sbtc-token", "sbtc-token");

        assert_eq!(check.clone().remap_mainnet_boot_principals(), check);
    }

    /// A contract name that is not a boot contract must not be rewritten just
    /// because the address matches — the burn address is a real principal.
    #[test]
    fn a_non_boot_contract_at_the_burn_address_is_untouched() {
        let check = nft(
            BOOT_MAINNET_STACKS_ADDRESS.clone(),
            "not-a-boot-contract",
            "thing",
        );

        assert_eq!(check.clone().remap_mainnet_boot_principals(), check);
    }

    /// `Unchecked` has nothing to walk.
    #[test]
    fn unchecked_is_a_no_op() {
        assert_eq!(
            PostConditionCheck::Unchecked.remap_mainnet_boot_principals(),
            PostConditionCheck::Unchecked
        );
    }
}

#[cfg(test)]
mod tests {
    use clarity::util::hash::to_hex;
    use stacks_codec::transaction::{
        FungibleConditionCode, PostConditionPrincipal, PoxConditionCode,
    };

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
                std::slice::from_ref(&encoded),
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
    fn rejects_a_condition_with_trailing_bytes() {
        let trailing = format!("{}00", hex_of(&sends_exactly(SENDER, 100)));

        let check = PostConditionCheck::from_hex(
            &[trailing],
            TransactionPostConditionMode::Deny,
            principal(SENDER),
        );

        assert!(check.is_err_and(|e| e.contains("1 trailing byte(s)")));
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

    #[test]
    fn decodes_and_epoch_gates_staking_and_pox_conditions() {
        let conditions = vec![
            TransactionPostCondition::Staking(
                PostConditionPrincipal::Origin,
                FungibleConditionCode::SentLe,
                100,
            ),
            TransactionPostCondition::Pox(
                PostConditionPrincipal::Origin,
                PoxConditionCode::MaybePerformed,
            ),
        ];
        let encoded = conditions.iter().map(hex_of).collect::<Vec<_>>();
        let check = PostConditionCheck::from_hex(
            &encoded,
            TransactionPostConditionMode::Deny,
            principal(SENDER),
        )
        .expect("staking and PoX conditions should decode");

        assert_eq!(check, checked(conditions));
        assert!(check
            .validate_for_epoch(StacksEpochId::Epoch34)
            .is_err_and(|e| e.contains("Staking/Pox post-condition is not supported")));
        assert_eq!(check.validate_for_epoch(StacksEpochId::Epoch40), Ok(()));
    }
}
