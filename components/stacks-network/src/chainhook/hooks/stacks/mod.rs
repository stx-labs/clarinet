use std::collections::{BTreeMap, HashMap};
use std::io::Cursor;
use std::time::Duration;

use crate::chainhook::types::{
    BlockIdentifier, StacksChainEvent, StacksNetwork, StacksNonConsensusEventData,
    StacksTransactionData, StacksTransactionEvent, StacksTransactionEventPayload,
    StacksTransactionKind, TransactionIdentifier,
};
use clarity::codec::StacksMessageCodec;
use clarity::vm::types::{
    CharType, PrincipalData, QualifiedContractIdentifier, SequenceData, Value as ClarityValue,
};
use clarity::vm::ClarityName;
use hiro_system_kit::slog;
use regex::Regex;
use reqwest::{Client, Method, RequestBuilder};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};

use super::types::{
    append_error_context, validate_txid, BlockIdentifierIndexRule, ChainhookInstance,
    ExactMatchingRule, HookAction,
};
use crate::chainhook::observer::EventObserverConfig;
use crate::chainhook::utils::{AbstractStacksBlock, Context, MAX_BLOCK_HEIGHTS_ENTRIES};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct StacksChainhookSpecification {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blocks: Option<Vec<u64>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_block: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_block: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expire_after_occurrence: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capture_all_events: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub decode_clarity_values: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_contract_abi: Option<bool>,
    #[serde(rename = "if_this")]
    pub predicate: StacksPredicate,
    #[serde(rename = "then_that")]
    pub action: HookAction,
}

impl StacksChainhookSpecification {
    pub fn new(predicate: StacksPredicate, action: HookAction) -> Self {
        StacksChainhookSpecification {
            blocks: None,
            start_block: None,
            end_block: None,
            expire_after_occurrence: None,
            capture_all_events: None,
            include_contract_abi: None,
            decode_clarity_values: None,
            predicate,
            action,
        }
    }

    pub fn blocks(&mut self, blocks: Vec<u64>) -> &mut Self {
        self.blocks = Some(blocks);
        self
    }

    pub fn start_block(&mut self, start_block: u64) -> &mut Self {
        self.start_block = Some(start_block);
        self
    }

    pub fn end_block(&mut self, end_block: u64) -> &mut Self {
        self.end_block = Some(end_block);
        self
    }

    pub fn expire_after_occurrence(&mut self, occurrence: u64) -> &mut Self {
        self.expire_after_occurrence = Some(occurrence);
        self
    }

    pub fn capture_all_events(&mut self, do_capture: bool) -> &mut Self {
        self.capture_all_events = Some(do_capture);
        self
    }

    pub fn include_contract_abi(&mut self, do_include: bool) -> &mut Self {
        self.include_contract_abi = Some(do_include);
        self
    }

    pub fn decode_clarity_values(&mut self, do_decode: bool) -> &mut Self {
        self.decode_clarity_values = Some(do_decode);
        self
    }

    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = vec![];
        if let Err(e) = self.action.validate() {
            errors.append(&mut append_error_context("invalid 'then_that' value", e));
        }
        if let Err(e) = self.predicate.validate() {
            errors.append(&mut append_error_context("invalid 'if_this' value", e));
        }

        if let Some(end_block) = self.end_block {
            let start_block = self.start_block.unwrap_or(0);
            if start_block > end_block {
                errors.push(
                    "Chainhook specification field `end_block` should be greater than `start_block`.".into()
                );
            }
            if (end_block - start_block) > MAX_BLOCK_HEIGHTS_ENTRIES {
                errors.push(format!("Chainhook specification exceeds max number of blocks to scan. Maximum: {}, Attempted: {}", MAX_BLOCK_HEIGHTS_ENTRIES, (end_block - start_block)));
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

/// Maps some [StacksChainhookSpecification] to a corresponding [StacksNetwork]. This allows maintaining one
/// serialized predicate file for a given predicate on each network.
///
/// ### Examples
/// Given some file `predicate.json`:
/// ```json
/// {
///   "uuid": "my-id",
///   "name": "My Predicate",
///   "chain": "stacks",
///   "version": 1,
///   "networks": {
///     "devnet": {
///       // ...
///     },
///     "testnet": {
///       // ...
///     },
///     "mainnet": {
///       // ...
///     }
///   }
/// }
/// ```
/// You can deserialize the file to this type and create a [StacksChainhookInstance] for the desired network:
/// ```
/// use chainhook_sdk::chainhooks::stacks::StacksChainhookSpecificationNetworkMap;
/// use chainhook_sdk::chainhooks::stacks::StacksChainhookInstance;
/// use crate::chainhook::types::StacksNetwork;
///
/// fn get_predicate(network: &StacksNetwork) -> Result<StacksChainhookInstance, String> {
///     let json_predicate =
///         std::fs::read_to_string("./predicate.json").expect("Unable to read file");
///     let hook_map: StacksChainhookSpecificationNetworkMap =
///         serde_json::from_str(&json_predicate).expect("Unable to parse Chainhook map");
///     hook_map.into_specification_for_network(network)
/// }
///
/// ```
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct StacksChainhookSpecificationNetworkMap {
    pub uuid: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner_uuid: Option<String>,
    pub name: String,
    pub version: u32,
    pub networks: BTreeMap<StacksNetwork, StacksChainhookSpecification>,
}

impl StacksChainhookSpecificationNetworkMap {
    pub fn into_specification_for_network(
        mut self,
        network: &StacksNetwork,
    ) -> Result<StacksChainhookInstance, String> {
        let spec = self
            .networks
            .remove(network)
            .ok_or("Network unknown".to_string())?;
        Ok(StacksChainhookInstance {
            uuid: self.uuid,
            owner_uuid: self.owner_uuid,
            name: self.name,
            network: network.clone(),
            version: self.version,
            start_block: spec.start_block,
            end_block: spec.end_block,
            blocks: spec.blocks,
            capture_all_events: spec.capture_all_events,
            decode_clarity_values: spec.decode_clarity_values,
            expire_after_occurrence: spec.expire_after_occurrence,
            include_contract_abi: spec.include_contract_abi,
            predicate: spec.predicate,
            action: spec.action,
            enabled: false,
            expired_at: None,
        })
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct StacksChainhookInstance {
    pub uuid: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub owner_uuid: Option<String>,
    pub name: String,
    pub network: StacksNetwork,
    pub version: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blocks: Option<Vec<u64>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_block: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_block: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expire_after_occurrence: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capture_all_events: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub decode_clarity_values: Option<bool>,
    pub include_contract_abi: Option<bool>,
    #[serde(rename = "predicate")]
    pub predicate: StacksPredicate,
    pub action: HookAction,
    pub enabled: bool,
    pub expired_at: Option<u64>,
}

impl StacksChainhookInstance {
    pub fn key(&self) -> String {
        ChainhookInstance::stacks_key(&self.uuid)
    }

    pub fn is_predicate_targeting_block_header(&self) -> bool {
        matches!(&self.predicate, StacksPredicate::BlockHeight(_))
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "scope")]
pub enum StacksPredicate {
    BlockHeight(BlockIdentifierIndexRule),
    ContractDeployment(StacksContractDeploymentPredicate),
    ContractCall(StacksContractCallBasedPredicate),
    PrintEvent(StacksPrintEventBasedPredicate),
    FtEvent(StacksFtEventBasedPredicate),
    NftEvent(StacksNftEventBasedPredicate),
    StxEvent(StacksStxEventBasedPredicate),
    Txid(ExactMatchingRule),
    #[cfg(feature = "stacks-signers")]
    SignerMessage(StacksSignerMessagePredicate),
}

impl StacksPredicate {
    pub fn validate(&self) -> Result<(), Vec<String>> {
        match self {
            StacksPredicate::BlockHeight(height) => {
                if let Err(e) = height.validate() {
                    return Err(append_error_context(
                        "invalid predicate for scope 'block_height'",
                        vec![e],
                    ));
                }
            }
            StacksPredicate::ContractDeployment(predicate) => {
                if let Err(e) = predicate.validate() {
                    return Err(append_error_context(
                        "invalid predicate for scope 'contract_deployment'",
                        vec![e],
                    ));
                }
            }
            StacksPredicate::ContractCall(predicate) => {
                if let Err(e) = predicate.validate() {
                    return Err(append_error_context(
                        "invalid predicate for scope 'contract_call'",
                        e,
                    ));
                }
            }
            StacksPredicate::PrintEvent(predicate) => {
                if let Err(e) = predicate.validate() {
                    return Err(append_error_context(
                        "invalid predicate for scope 'print_event'",
                        e,
                    ));
                }
            }
            StacksPredicate::FtEvent(_) => {}
            StacksPredicate::NftEvent(_) => {}
            StacksPredicate::StxEvent(_) => {}
            StacksPredicate::Txid(ExactMatchingRule::Equals(txid)) => {
                if let Err(e) = validate_txid(txid) {
                    return Err(append_error_context(
                        "invalid predicate for scope 'txid'",
                        vec![e],
                    ));
                }
            }
            #[cfg(feature = "stacks-signers")]
            StacksPredicate::SignerMessage(StacksSignerMessagePredicate::FromSignerPubKey(_)) => {
                // TODO(rafaelcr): Validate pubkey format
            }
            #[cfg(feature = "stacks-signers")]
            StacksPredicate::SignerMessage(StacksSignerMessagePredicate::AfterTimestamp(_)) => {}
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum StacksSignerMessagePredicate {
    AfterTimestamp(u64),
    FromSignerPubKey(String),
}

impl StacksSignerMessagePredicate {
    // TODO(rafaelcr): Write validators
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct StacksContractCallBasedPredicate {
    pub contract_identifier: String,
    pub method: String,
}

fn validate_contract_identifier(id: &str) -> Result<(), String> {
    if let Err(e) = QualifiedContractIdentifier::parse(id) {
        return Err(format!("invalid contract identifier: {}", e));
    }
    Ok(())
}

impl StacksContractCallBasedPredicate {
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = vec![];

        if let Err(e) = validate_contract_identifier(&self.contract_identifier) {
            errors.push(e);
        }
        if let Err(e) = ClarityName::try_from(self.method.clone()) {
            errors.push(format!("invalid contract method: {:?}", e));
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum StacksContractDeploymentPredicate {
    Deployer(String),
    ImplementTrait(StacksTrait),
}

impl StacksContractDeploymentPredicate {
    pub fn validate(&self) -> Result<(), String> {
        match self {
            StacksContractDeploymentPredicate::Deployer(deployer) => {
                if !deployer.eq("*") {
                    if let Err(e) = PrincipalData::parse_standard_principal(deployer) {
                        return Err(format!(
                            "contract deployer must be a valid Stacks address: {}",
                            e
                        ));
                    }
                }
            }
            StacksContractDeploymentPredicate::ImplementTrait(_) => {}
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum StacksTrait {
    Sip09,
    Sip10,
    #[serde(rename = "*")]
    Any,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
#[serde(untagged)]
pub enum StacksPrintEventBasedPredicate {
    Contains {
        contract_identifier: String,
        contains: String,
    },
    MatchesRegex {
        contract_identifier: String,
        #[serde(rename = "matches_regex")]
        regex: String,
    },
}

impl StacksPrintEventBasedPredicate {
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = vec![];
        match self {
            StacksPrintEventBasedPredicate::Contains {
                contract_identifier,
                ..
            } => {
                if !contract_identifier.eq("*") {
                    if let Err(e) = validate_contract_identifier(contract_identifier) {
                        errors.push(e);
                    }
                }
            }
            StacksPrintEventBasedPredicate::MatchesRegex {
                contract_identifier,
                regex,
            } => {
                if !contract_identifier.eq("*") {
                    if let Err(e) = validate_contract_identifier(contract_identifier) {
                        errors.push(e);
                    }
                }
                if let Err(e) = Regex::new(regex) {
                    errors.push(format!("invalid regex: {}", e))
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct StacksFtEventBasedPredicate {
    pub asset_identifier: String,
    pub actions: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct StacksNftEventBasedPredicate {
    pub asset_identifier: String,
    pub actions: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct StacksStxEventBasedPredicate {
    pub actions: Vec<String>,
}

#[derive(Clone)]
pub struct StacksTriggerChainhook<'a> {
    pub chainhook: &'a StacksChainhookInstance,
    pub apply: Vec<(Vec<&'a StacksTransactionData>, &'a dyn AbstractStacksBlock)>,
    pub rollback: Vec<(Vec<&'a StacksTransactionData>, &'a dyn AbstractStacksBlock)>,
    pub events: Vec<&'a StacksNonConsensusEventData>,
}

#[derive(Clone, Debug)]
pub struct StacksApplyTransactionPayload {
    pub block_identifier: BlockIdentifier,
    pub transactions: Vec<StacksTransactionData>,
}

#[derive(Clone, Debug)]
pub struct StacksRollbackTransactionPayload {
    pub block_identifier: BlockIdentifier,
    pub transactions: Vec<StacksTransactionData>,
}

#[derive(Clone, Debug)]
pub struct StacksChainhookPayload {
    pub uuid: String,
}

#[derive(Clone, Debug)]
pub struct StacksChainhookOccurrencePayload {
    pub apply: Vec<StacksApplyTransactionPayload>,
    pub rollback: Vec<StacksRollbackTransactionPayload>,
    pub events: Vec<StacksNonConsensusEventData>,
    pub chainhook: StacksChainhookPayload,
}

impl StacksChainhookOccurrencePayload {
    pub fn from_trigger(trigger: StacksTriggerChainhook<'_>) -> StacksChainhookOccurrencePayload {
        StacksChainhookOccurrencePayload {
            apply: trigger
                .apply
                .into_iter()
                .map(|(transactions, block)| {
                    let transactions = transactions.into_iter().cloned().collect::<Vec<_>>();
                    StacksApplyTransactionPayload {
                        block_identifier: block.get_identifier().clone(),
                        transactions,
                    }
                })
                .collect::<Vec<_>>(),
            rollback: trigger
                .rollback
                .into_iter()
                .map(|(transactions, block)| {
                    let transactions = transactions.into_iter().cloned().collect::<Vec<_>>();
                    StacksRollbackTransactionPayload {
                        block_identifier: block.get_identifier().clone(),
                        transactions,
                    }
                })
                .collect::<Vec<_>>(),
            chainhook: StacksChainhookPayload {
                uuid: trigger.chainhook.uuid.clone(),
            },
            events: trigger.events.into_iter().cloned().collect::<Vec<_>>(),
        }
    }
}
pub enum StacksChainhookOccurrence {
    Http(Box<RequestBuilder>, StacksChainhookOccurrencePayload),
    File(String, Vec<u8>),
    Data(StacksChainhookOccurrencePayload),
}

impl<'a> StacksTriggerChainhook<'a> {
    pub fn should_decode_clarity_value(&self) -> bool {
        self.chainhook.decode_clarity_values.unwrap_or(false)
    }
}

pub fn evaluate_stacks_chainhooks_on_chain_event<'a>(
    chain_event: &'a StacksChainEvent,
    active_chainhooks: Vec<&'a StacksChainhookInstance>,
    ctx: &Context,
) -> (
    Vec<StacksTriggerChainhook<'a>>,
    BTreeMap<&'a str, &'a BlockIdentifier>,
    BTreeMap<&'a str, &'a BlockIdentifier>,
) {
    let mut triggered_predicates = vec![];
    let mut evaluated_predicates = BTreeMap::new();
    let mut expired_predicates = BTreeMap::new();
    match chain_event {
        StacksChainEvent::ChainUpdatedWithBlocks(update) => {
            for chainhook in active_chainhooks.iter() {
                let mut apply = vec![];
                let mut rollback = vec![];
                for block_update in update.new_blocks.iter() {
                    evaluated_predicates.insert(
                        chainhook.uuid.as_str(),
                        &block_update.block.block_identifier,
                    );

                    for parents_microblock_to_apply in
                        block_update.parent_microblocks_to_apply.iter()
                    {
                        let (mut occurrences, mut expirations) =
                            evaluate_stacks_chainhook_on_blocks(
                                vec![parents_microblock_to_apply],
                                chainhook,
                                ctx,
                            );
                        apply.append(&mut occurrences);
                        expired_predicates.append(&mut expirations);
                    }
                    for parents_microblock_to_rolllback in
                        block_update.parent_microblocks_to_rollback.iter()
                    {
                        let (mut occurrences, mut expirations) =
                            evaluate_stacks_chainhook_on_blocks(
                                vec![parents_microblock_to_rolllback],
                                chainhook,
                                ctx,
                            );
                        rollback.append(&mut occurrences);
                        expired_predicates.append(&mut expirations);
                    }

                    let (mut occurrences, mut expirations) = evaluate_stacks_chainhook_on_blocks(
                        vec![&block_update.block],
                        chainhook,
                        ctx,
                    );
                    apply.append(&mut occurrences);
                    expired_predicates.append(&mut expirations);
                }
                if !apply.is_empty() || !rollback.is_empty() {
                    triggered_predicates.push(StacksTriggerChainhook {
                        chainhook,
                        apply,
                        rollback,
                        events: vec![],
                    })
                }
            }
        }
        StacksChainEvent::ChainUpdatedWithMicroblocks(update) => {
            for chainhook in active_chainhooks.iter() {
                let mut apply = vec![];
                let rollback = vec![];

                for microblock_to_apply in update.new_microblocks.iter() {
                    evaluated_predicates.insert(
                        chainhook.uuid.as_str(),
                        &microblock_to_apply.metadata.anchor_block_identifier,
                    );

                    let (mut occurrences, mut expirations) = evaluate_stacks_chainhook_on_blocks(
                        vec![microblock_to_apply],
                        chainhook,
                        ctx,
                    );
                    apply.append(&mut occurrences);
                    expired_predicates.append(&mut expirations);
                }
                if !apply.is_empty() || !rollback.is_empty() {
                    triggered_predicates.push(StacksTriggerChainhook {
                        chainhook,
                        apply,
                        rollback,
                        events: vec![],
                    })
                }
            }
        }
        StacksChainEvent::ChainUpdatedWithMicroblocksReorg(update) => {
            for chainhook in active_chainhooks.iter() {
                let mut apply = vec![];
                let mut rollback = vec![];

                for microblock_to_apply in update.microblocks_to_apply.iter() {
                    evaluated_predicates.insert(
                        chainhook.uuid.as_str(),
                        &microblock_to_apply.metadata.anchor_block_identifier,
                    );
                    let (mut occurrences, mut expirations) = evaluate_stacks_chainhook_on_blocks(
                        vec![microblock_to_apply],
                        chainhook,
                        ctx,
                    );
                    apply.append(&mut occurrences);
                    expired_predicates.append(&mut expirations);
                }
                for microblock_to_rollback in update.microblocks_to_rollback.iter() {
                    let (mut occurrences, mut expirations) = evaluate_stacks_chainhook_on_blocks(
                        vec![microblock_to_rollback],
                        chainhook,
                        ctx,
                    );
                    rollback.append(&mut occurrences);
                    expired_predicates.append(&mut expirations);
                }
                if !apply.is_empty() || !rollback.is_empty() {
                    triggered_predicates.push(StacksTriggerChainhook {
                        chainhook,
                        apply,
                        rollback,
                        events: vec![],
                    })
                }
            }
        }
        StacksChainEvent::ChainUpdatedWithReorg(update) => {
            for chainhook in active_chainhooks.iter() {
                let mut apply = vec![];
                let mut rollback = vec![];

                for block_update in update.blocks_to_apply.iter() {
                    evaluated_predicates.insert(
                        chainhook.uuid.as_str(),
                        &block_update.block.block_identifier,
                    );
                    for parents_microblock_to_apply in
                        block_update.parent_microblocks_to_apply.iter()
                    {
                        let (mut occurrences, mut expirations) =
                            evaluate_stacks_chainhook_on_blocks(
                                vec![parents_microblock_to_apply],
                                chainhook,
                                ctx,
                            );
                        apply.append(&mut occurrences);
                        expired_predicates.append(&mut expirations);
                    }

                    let (mut occurrences, mut expirations) = evaluate_stacks_chainhook_on_blocks(
                        vec![&block_update.block],
                        chainhook,
                        ctx,
                    );
                    apply.append(&mut occurrences);
                    expired_predicates.append(&mut expirations);
                }
                for block_update in update.blocks_to_rollback.iter() {
                    for parents_microblock_to_rollback in
                        block_update.parent_microblocks_to_rollback.iter()
                    {
                        let (mut occurrences, mut expirations) =
                            evaluate_stacks_chainhook_on_blocks(
                                vec![parents_microblock_to_rollback],
                                chainhook,
                                ctx,
                            );
                        rollback.append(&mut occurrences);
                        expired_predicates.append(&mut expirations);
                    }
                    let (mut occurrences, mut expirations) = evaluate_stacks_chainhook_on_blocks(
                        vec![&block_update.block],
                        chainhook,
                        ctx,
                    );
                    rollback.append(&mut occurrences);
                    expired_predicates.append(&mut expirations);
                }
                if !apply.is_empty() || !rollback.is_empty() {
                    triggered_predicates.push(StacksTriggerChainhook {
                        chainhook,
                        apply,
                        rollback,
                        events: vec![],
                    })
                }
            }
        }
        #[cfg(feature = "stacks-signers")]
        StacksChainEvent::ChainUpdatedWithNonConsensusEvents(data) => {
            if let Some(first_event) = data.events.first() {
                for chainhook in active_chainhooks.iter() {
                    evaluated_predicates
                        .insert(chainhook.uuid.as_str(), &first_event.received_at_block);
                    let (occurrences, mut expirations) =
                        evaluate_stacks_predicate_on_non_consensus_events(
                            &data.events,
                            chainhook,
                            ctx,
                        );
                    expired_predicates.append(&mut expirations);
                    if !occurrences.is_empty() {
                        triggered_predicates.push(StacksTriggerChainhook {
                            chainhook,
                            apply: vec![],
                            rollback: vec![],
                            events: occurrences,
                        });
                    }
                }
            }
        }
        #[cfg(not(feature = "stacks-signers"))]
        StacksChainEvent::ChainUpdatedWithNonConsensusEvents(_) => {}
    }
    (
        triggered_predicates,
        evaluated_predicates,
        expired_predicates,
    )
}

pub fn evaluate_stacks_chainhook_on_blocks<'a>(
    blocks: Vec<&'a dyn AbstractStacksBlock>,
    chainhook: &'a StacksChainhookInstance,
    ctx: &Context,
) -> (
    Vec<(Vec<&'a StacksTransactionData>, &'a dyn AbstractStacksBlock)>,
    BTreeMap<&'a str, &'a BlockIdentifier>,
) {
    let mut occurrences = vec![];
    let mut expired_predicates = BTreeMap::new();
    let end_block = chainhook.end_block.unwrap_or(u64::MAX);
    for block in blocks {
        if end_block >= block.get_identifier().index {
            let mut hits = vec![];
            if chainhook.is_predicate_targeting_block_header() {
                if evaluate_stacks_predicate_on_block(block, chainhook, ctx) {
                    for tx in block.get_transactions().iter() {
                        hits.push(tx);
                    }
                }
            } else {
                for tx in block.get_transactions().iter() {
                    if evaluate_stacks_predicate_on_transaction(tx, chainhook, ctx) {
                        hits.push(tx);
                    }
                }
            }
            if !hits.is_empty() {
                occurrences.push((hits, block));
            }
        } else {
            expired_predicates.insert(chainhook.uuid.as_str(), block.get_identifier());
        }
    }
    (occurrences, expired_predicates)
}

pub fn evaluate_stacks_predicate_on_block<'a>(
    block: &'a dyn AbstractStacksBlock,
    chainhook: &'a StacksChainhookInstance,
    _ctx: &Context,
) -> bool {
    match &chainhook.predicate {
        StacksPredicate::BlockHeight(BlockIdentifierIndexRule::Between(a, b)) => {
            block.get_identifier().index.gt(a) && block.get_identifier().index.lt(b)
        }
        StacksPredicate::BlockHeight(BlockIdentifierIndexRule::HigherThan(a)) => {
            block.get_identifier().index.gt(a)
        }
        StacksPredicate::BlockHeight(BlockIdentifierIndexRule::LowerThan(a)) => {
            block.get_identifier().index.lt(a)
        }
        StacksPredicate::BlockHeight(BlockIdentifierIndexRule::Equals(a)) => {
            block.get_identifier().index.eq(a)
        }
        StacksPredicate::ContractDeployment(_)
        | StacksPredicate::ContractCall(_)
        | StacksPredicate::FtEvent(_)
        | StacksPredicate::NftEvent(_)
        | StacksPredicate::StxEvent(_)
        | StacksPredicate::PrintEvent(_)
        | StacksPredicate::Txid(_) => unreachable!(),
        #[cfg(feature = "stacks-signers")]
        StacksPredicate::SignerMessage(_) => false,
    }
}

#[cfg(feature = "stacks-signers")]
pub fn evaluate_stacks_predicate_on_non_consensus_events<'a>(
    events: &'a Vec<StacksNonConsensusEventData>,
    chainhook: &'a StacksChainhookInstance,
    _ctx: &Context,
) -> (
    Vec<&'a StacksNonConsensusEventData>,
    BTreeMap<&'a str, &'a BlockIdentifier>,
) {
    let mut occurrences = vec![];
    let expired_predicates = BTreeMap::new();
    for event in events {
        match &chainhook.predicate {
            StacksPredicate::SignerMessage(StacksSignerMessagePredicate::AfterTimestamp(
                timestamp,
            )) => {
                if event.received_at_ms >= *timestamp {
                    occurrences.push(event);
                }
            }
            StacksPredicate::SignerMessage(StacksSignerMessagePredicate::FromSignerPubKey(_)) => {
                // TODO(rafaelcr): Evaluate on pubkey
            }
            StacksPredicate::BlockHeight(_)
            | StacksPredicate::ContractDeployment(_)
            | StacksPredicate::ContractCall(_)
            | StacksPredicate::FtEvent(_)
            | StacksPredicate::NftEvent(_)
            | StacksPredicate::StxEvent(_)
            | StacksPredicate::PrintEvent(_)
            | StacksPredicate::Txid(_) => {}
        };
    }
    (occurrences, expired_predicates)
}

pub fn evaluate_stacks_predicate_on_transaction<'a>(
    transaction: &'a StacksTransactionData,
    chainhook: &'a StacksChainhookInstance,
    ctx: &Context,
) -> bool {
    match &chainhook.predicate {
        StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::Deployer(
            expected_deployer,
        )) => match &transaction.metadata.kind {
            StacksTransactionKind::ContractDeployment(actual_deployment) => {
                if expected_deployer.eq("*") {
                    true
                } else {
                    actual_deployment
                        .contract_identifier
                        .starts_with(expected_deployer)
                }
            }
            _ => false,
        },
        StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::ImplementTrait(
            _stacks_trait,
        )) => match &transaction.metadata.kind {
            StacksTransactionKind::ContractDeployment(_actual_deployment) => {
                ctx.try_log(|logger| {
                    slog::warn!(
                        logger,
                        "StacksContractDeploymentPredicate::ImplementTrait uninmplemented"
                    )
                });
                false
            }
            _ => false,
        },
        StacksPredicate::ContractCall(expected_contract_call) => match &transaction.metadata.kind {
            StacksTransactionKind::ContractCall(actual_contract_call) => {
                actual_contract_call
                    .contract_identifier
                    .eq(&expected_contract_call.contract_identifier)
                    && actual_contract_call
                        .method
                        .eq(&expected_contract_call.method)
            }
            _ => false,
        },
        StacksPredicate::FtEvent(expected_event) => {
            let expecting_mint = expected_event.actions.contains(&"mint".to_string());
            let expecting_transfer = expected_event.actions.contains(&"transfer".to_string());
            let expecting_burn = expected_event.actions.contains(&"burn".to_string());

            for event in transaction.metadata.receipt.events.iter() {
                match (
                    &event.event_payload,
                    expecting_mint,
                    expecting_transfer,
                    expecting_burn,
                ) {
                    (StacksTransactionEventPayload::FTMintEvent(ft_event), true, _, _) => {
                        if ft_event
                            .asset_class_identifier
                            .eq(&expected_event.asset_identifier)
                        {
                            return true;
                        }
                    }
                    (StacksTransactionEventPayload::FTTransferEvent(ft_event), _, true, _) => {
                        if ft_event
                            .asset_class_identifier
                            .eq(&expected_event.asset_identifier)
                        {
                            return true;
                        }
                    }
                    (StacksTransactionEventPayload::FTBurnEvent(ft_event), _, _, true) => {
                        if ft_event
                            .asset_class_identifier
                            .eq(&expected_event.asset_identifier)
                        {
                            return true;
                        }
                    }
                    _ => continue,
                }
            }
            false
        }
        StacksPredicate::NftEvent(expected_event) => {
            let expecting_mint = expected_event.actions.contains(&"mint".to_string());
            let expecting_transfer = expected_event.actions.contains(&"transfer".to_string());
            let expecting_burn = expected_event.actions.contains(&"burn".to_string());

            for event in transaction.metadata.receipt.events.iter() {
                match (
                    &event.event_payload,
                    expecting_mint,
                    expecting_transfer,
                    expecting_burn,
                ) {
                    (StacksTransactionEventPayload::NFTMintEvent(nft_event), true, _, _) => {
                        if nft_event
                            .asset_class_identifier
                            .eq(&expected_event.asset_identifier)
                        {
                            return true;
                        }
                    }
                    (StacksTransactionEventPayload::NFTTransferEvent(nft_event), _, true, _) => {
                        if nft_event
                            .asset_class_identifier
                            .eq(&expected_event.asset_identifier)
                        {
                            return true;
                        }
                    }
                    (StacksTransactionEventPayload::NFTBurnEvent(nft_event), _, _, true) => {
                        if nft_event
                            .asset_class_identifier
                            .eq(&expected_event.asset_identifier)
                        {
                            return true;
                        }
                    }
                    _ => continue,
                }
            }
            false
        }
        StacksPredicate::StxEvent(expected_event) => {
            let expecting_mint = expected_event.actions.contains(&"mint".to_string());
            let expecting_transfer = expected_event.actions.contains(&"transfer".to_string());
            let expecting_lock = expected_event.actions.contains(&"lock".to_string());
            let expecting_burn = expected_event.actions.contains(&"burn".to_string());

            for event in transaction.metadata.receipt.events.iter() {
                match (
                    &event.event_payload,
                    expecting_mint,
                    expecting_transfer,
                    expecting_lock,
                    expecting_burn,
                ) {
                    (StacksTransactionEventPayload::STXMintEvent(_), true, _, _, _) => return true,
                    (StacksTransactionEventPayload::STXTransferEvent(_), _, true, _, _) => {
                        return true
                    }
                    (StacksTransactionEventPayload::STXLockEvent(_), _, _, true, _) => return true,
                    (StacksTransactionEventPayload::STXBurnEvent(_), _, _, _, true) => return true,
                    _ => continue,
                }
            }
            false
        }
        StacksPredicate::PrintEvent(expected_event) => {
            for event in transaction.metadata.receipt.events.iter() {
                if let StacksTransactionEventPayload::SmartContractEvent(actual) =
                    &event.event_payload
                {
                    if actual.topic == "print" {
                        match expected_event {
                            StacksPrintEventBasedPredicate::Contains {
                                contract_identifier,
                                contains,
                            } => {
                                if contract_identifier == &actual.contract_identifier
                                    || contract_identifier == "*"
                                {
                                    if contains == "*" {
                                        return true;
                                    }
                                    let value = format!(
                                        "{}",
                                        expect_decoded_clarity_value(&actual.hex_value)
                                    );
                                    if value.contains(contains) {
                                        return true;
                                    }
                                }
                            }
                            StacksPrintEventBasedPredicate::MatchesRegex {
                                contract_identifier,
                                regex,
                            } => {
                                if contract_identifier == &actual.contract_identifier
                                    || contract_identifier == "*"
                                {
                                    if let Ok(regex) = Regex::new(regex) {
                                        let value = format!(
                                            "{}",
                                            expect_decoded_clarity_value(&actual.hex_value)
                                        );
                                        if regex.is_match(&value) {
                                            return true;
                                        }
                                    } else {
                                        ctx.try_log(|logger| {
                                            slog::error!(logger, "unable to parse print_event matching rule as regex")
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
            false
        }
        StacksPredicate::Txid(ExactMatchingRule::Equals(txid)) => {
            txid.eq(&transaction.transaction_identifier.hash)
        }
        StacksPredicate::BlockHeight(_) => unreachable!(),
        #[cfg(feature = "stacks-signers")]
        StacksPredicate::SignerMessage(_) => false,
    }
}

fn serialize_stacks_non_consensus_event(
    event: &StacksNonConsensusEventData,
    _ctx: &Context,
) -> serde_json::Value {
    use crate::chainhook::types::StacksNonConsensusEventPayloadData;

    let payload = match &event.payload {
        StacksNonConsensusEventPayloadData::SignerMessage(chunk) => {
            json!({"type": "SignerMessage", "data": chunk})
        }
    };
    json!({
        "payload": payload,
        "received_at_ms": event.received_at_ms,
        "received_at_block": event.received_at_block,
    })
}

fn serialize_stacks_block(
    block: &dyn AbstractStacksBlock,
    transactions: Vec<&StacksTransactionData>,
    decode_clarity_values: bool,
    include_contract_abi: bool,
    ctx: &Context,
) -> serde_json::Value {
    json!({
        "block_identifier": block.get_identifier(),
        "parent_block_identifier": block.get_parent_identifier(),
        "timestamp": block.get_timestamp(),
        "transactions": transactions.into_iter().map(|transaction| {
            serialize_stacks_transaction(transaction, decode_clarity_values, include_contract_abi, ctx)
        }).collect::<Vec<_>>(),
        "metadata": block.get_serialized_metadata(),
    })
}

fn serialize_stacks_transaction(
    transaction: &StacksTransactionData,
    decode_clarity_values: bool,
    include_contract_abi: bool,
    ctx: &Context,
) -> serde_json::Value {
    let mut json = json!({
        "transaction_identifier": transaction.transaction_identifier,
        "operations": transaction.operations,
        "metadata": {
            "success": transaction.metadata.success,
            "raw_tx": transaction.metadata.raw_tx,
            "result": if decode_clarity_values {
                serialized_decoded_clarity_value(&transaction.metadata.result, ctx)
            } else  {
                json!(transaction.metadata.result)
            },
            "sender": transaction.metadata.sender,
            "nonce": transaction.metadata.nonce,
            "fee": transaction.metadata.fee,
            "kind": transaction.metadata.kind,
            "receipt": {
                "mutated_contracts_radius": transaction.metadata.receipt.mutated_contracts_radius,
                "mutated_assets_radius": transaction.metadata.receipt.mutated_assets_radius,
                "contract_calls_stack": transaction.metadata.receipt.contract_calls_stack,
                "events": transaction.metadata.receipt.events.iter().map(|event| {
                    if decode_clarity_values { serialized_event_with_decoded_clarity_value(event, ctx) } else { json!(event) }
                }).collect::<Vec<serde_json::Value>>(),
            },
            "description": transaction.metadata.description,
            "sponsor": transaction.metadata.sponsor,
            "execution_cost": transaction.metadata.execution_cost,
            "position": transaction.metadata.position
        },
    });
    if include_contract_abi {
        if let Some(abi) = &transaction.metadata.contract_abi {
            json["metadata"]["contract_abi"] = json!(abi);
        }
    }
    json
}

pub fn serialized_event_with_decoded_clarity_value(
    event: &StacksTransactionEvent,
    ctx: &Context,
) -> serde_json::Value {
    match &event.event_payload {
        StacksTransactionEventPayload::STXTransferEvent(payload) => {
            json!({
                "type": "STXTransferEvent",
                "data": payload,
                "position": event.position
            })
        }
        StacksTransactionEventPayload::STXMintEvent(payload) => {
            json!({
                "type": "STXMintEvent",
                "data": payload,
                "position": event.position
            })
        }
        StacksTransactionEventPayload::STXLockEvent(payload) => {
            json!({
                "type": "STXLockEvent",
                "data": payload,
                "position": event.position
            })
        }
        StacksTransactionEventPayload::STXBurnEvent(payload) => {
            json!({
                "type": "STXBurnEvent",
                "data": payload,
                "position": event.position
            })
        }
        StacksTransactionEventPayload::NFTTransferEvent(payload) => {
            json!({
                "type": "NFTTransferEvent",
                "data": {
                    "asset_class_identifier": payload.asset_class_identifier,
                    "asset_identifier": serialized_decoded_clarity_value(&payload.hex_asset_identifier, ctx),
                    "sender": payload.sender,
                    "recipient": payload.recipient,
                },
                "position": event.position
            })
        }
        StacksTransactionEventPayload::NFTMintEvent(payload) => {
            json!({
                "type": "NFTMintEvent",
                "data": {
                    "asset_class_identifier": payload.asset_class_identifier,
                    "asset_identifier": serialized_decoded_clarity_value(&payload.hex_asset_identifier, ctx),
                    "recipient": payload.recipient,
                },
                "position": event.position
            })
        }
        StacksTransactionEventPayload::NFTBurnEvent(payload) => {
            json!({
                "type": "NFTBurnEvent",
                "data": {
                    "asset_class_identifier": payload.asset_class_identifier,
                    "asset_identifier": serialized_decoded_clarity_value(&payload.hex_asset_identifier, ctx),
                    "sender": payload.sender,
                },
                "position": event.position
            })
        }
        StacksTransactionEventPayload::FTTransferEvent(payload) => {
            json!({
                "type": "FTTransferEvent",
                "data": payload,
                "position": event.position
            })
        }
        StacksTransactionEventPayload::FTMintEvent(payload) => {
            json!({
                "type": "FTMintEvent",
                "data": payload,
                "position": event.position
            })
        }
        StacksTransactionEventPayload::FTBurnEvent(payload) => {
            json!({
                "type": "FTBurnEvent",
                "data": payload,
                "position": event.position
            })
        }
        StacksTransactionEventPayload::DataVarSetEvent(payload) => {
            json!({
                "type": "DataVarSetEvent",
                "data": {
                    "contract_identifier": payload.contract_identifier,
                    "var": payload.var,
                    "new_value": serialized_decoded_clarity_value(&payload.hex_new_value, ctx),
                },
                "position": event.position
            })
        }
        StacksTransactionEventPayload::DataMapInsertEvent(payload) => {
            json!({
                "type": "DataMapInsertEvent",
                "data": {
                    "contract_identifier": payload.contract_identifier,
                    "map": payload.map,
                    "inserted_key": serialized_decoded_clarity_value(&payload.hex_inserted_key, ctx),
                    "inserted_value": serialized_decoded_clarity_value(&payload.hex_inserted_value, ctx),
                },
                "position": event.position
            })
        }
        StacksTransactionEventPayload::DataMapUpdateEvent(payload) => {
            json!({
                "type": "DataMapUpdateEvent",
                "data": {
                    "contract_identifier": payload.contract_identifier,
                    "map": payload.map,
                    "key": serialized_decoded_clarity_value(&payload.hex_key, ctx),
                    "new_value": serialized_decoded_clarity_value(&payload.hex_new_value, ctx),
                },
                "position": event.position
            })
        }
        StacksTransactionEventPayload::DataMapDeleteEvent(payload) => {
            json!({
                "type": "DataMapDeleteEvent",
                "data": {
                    "contract_identifier": payload.contract_identifier,
                    "map": payload.map,
                    "deleted_key": serialized_decoded_clarity_value(&payload.hex_deleted_key, ctx),
                },
                "position": event.position
            })
        }
        StacksTransactionEventPayload::SmartContractEvent(payload) => {
            json!({
                "type": "SmartContractEvent",
                "data": {
                    "contract_identifier": payload.contract_identifier,
                    "topic": payload.topic,
                    "value": serialized_decoded_clarity_value(&payload.hex_value, ctx),
                },
                "position": event.position
            })
        }
    }
}

pub fn expect_decoded_clarity_value(hex_value: &str) -> ClarityValue {
    try_decode_clarity_value(hex_value)
        .expect("unable to decode clarity value emitted by stacks-node")
}

pub fn try_decode_clarity_value(hex_value: &str) -> Option<ClarityValue> {
    let hex_value = hex_value.strip_prefix("0x")?;
    let value_bytes = hex::decode(hex_value).ok()?;
    ClarityValue::consensus_deserialize(&mut Cursor::new(&value_bytes)).ok()
}

pub fn serialized_decoded_clarity_value(hex_value: &str, ctx: &Context) -> serde_json::Value {
    let hex_value = match hex_value.strip_prefix("0x") {
        Some(hex_value) => hex_value,
        _ => return json!(hex_value.to_string()),
    };
    let value_bytes = match hex::decode(hex_value) {
        Ok(bytes) => bytes,
        _ => return json!(hex_value.to_string()),
    };

    match ClarityValue::consensus_deserialize(&mut Cursor::new(&value_bytes)) {
        Ok(value) => serialize_to_json(&value),
        Err(e) => {
            ctx.try_log(|logger| {
                slog::error!(logger, "unable to deserialize clarity value {:?}", e)
            });
            json!(hex_value.to_string())
        }
    }
}

pub fn serialize_to_json(value: &ClarityValue) -> serde_json::Value {
    match value {
        ClarityValue::Int(int) => json!(int),
        ClarityValue::UInt(int) => json!(int),
        ClarityValue::Bool(boolean) => json!(boolean),
        ClarityValue::Principal(principal_data) => json!(format!("{}", principal_data)),
        ClarityValue::Sequence(SequenceData::Buffer(vec_bytes)) => {
            json!(format!("0x{}", &vec_bytes))
        }
        ClarityValue::Sequence(SequenceData::String(CharType::ASCII(string))) => {
            json!(String::from_utf8(string.data.clone()).unwrap())
        }
        ClarityValue::Sequence(SequenceData::String(CharType::UTF8(string))) => {
            let mut result = String::new();
            for c in string.data.iter() {
                if c.len() > 1 {
                    result.push_str(core::str::from_utf8(c).unwrap());
                } else {
                    result.push(c[0] as char)
                }
            }
            json!(result)
        }
        ClarityValue::Optional(opt_data) => match &opt_data.data {
            None => serde_json::Value::Null,
            Some(value) => serialize_to_json(value),
        },
        ClarityValue::Response(res_data) => {
            json!({
                "result": {
                    "success": res_data.committed,
                    "value": serialize_to_json(&res_data.data),
                }
            })
        }
        ClarityValue::Tuple(data) => {
            let mut map = serde_json::Map::new();
            for (name, value) in data.data_map.iter() {
                map.insert(name.to_string(), serialize_to_json(value));
            }
            json!(map)
        }
        ClarityValue::Sequence(SequenceData::List(list_data)) => {
            let mut list = vec![];
            for value in list_data.data.iter() {
                list.push(serialize_to_json(value));
            }
            json!(list)
        }
        ClarityValue::CallableContract(callable) => {
            json!(format!("{}", callable.contract_identifier))
        }
    }
}

pub fn serialize_stacks_payload_to_json<'a>(
    trigger: StacksTriggerChainhook<'a>,
    _proofs: &HashMap<&'a TransactionIdentifier, String>,
    ctx: &Context,
) -> JsonValue {
    let decode_clarity_values = trigger.should_decode_clarity_value();
    let include_contract_abi = trigger.chainhook.include_contract_abi.unwrap_or(false);
    json!({
        "apply": trigger.apply.into_iter().map(|(transactions, block)| {
            serialize_stacks_block(block, transactions, decode_clarity_values, include_contract_abi, ctx)
        }).collect::<Vec<_>>(),
        "rollback": trigger.rollback.into_iter().map(|(transactions, block)| {
            serialize_stacks_block(block, transactions, decode_clarity_values, include_contract_abi, ctx)
        }).collect::<Vec<_>>(),
        "events": trigger.events.into_iter().map(|event| serialize_stacks_non_consensus_event(event, ctx)).collect::<Vec<_>>(),
        "chainhook": {
            "uuid": trigger.chainhook.uuid,
            "predicate": trigger.chainhook.predicate,
            "is_streaming_blocks": trigger.chainhook.enabled
        }
    })
}

pub fn handle_stacks_hook_action<'a>(
    trigger: StacksTriggerChainhook<'a>,
    proofs: &HashMap<&'a TransactionIdentifier, String>,
    config: &EventObserverConfig,
    ctx: &Context,
) -> Result<StacksChainhookOccurrence, String> {
    match &trigger.chainhook.action {
        HookAction::HttpPost(http) => {
            let mut client_builder = Client::builder();
            if let Some(timeout) = config.predicates_config.payload_http_request_timeout_ms {
                client_builder = client_builder.timeout(Duration::from_millis(timeout));
            }
            let client = client_builder
                .build()
                .map_err(|e| format!("unable to build http client: {}", e))?;
            let host = http.url.to_string();
            let method = Method::POST;
            let body = serde_json::to_vec(&serialize_stacks_payload_to_json(
                trigger.clone(),
                proofs,
                ctx,
            ))
            .map_err(|e| format!("unable to serialize payload {}", e))?;
            Ok(StacksChainhookOccurrence::Http(
                Box::new(
                    client
                        .request(method, &host)
                        .header("Content-Type", "application/json")
                        .header("Authorization", http.authorization_header.clone())
                        .body(body),
                ),
                StacksChainhookOccurrencePayload::from_trigger(trigger),
            ))
        }
        HookAction::FileAppend(disk) => {
            let bytes = serde_json::to_vec(&serialize_stacks_payload_to_json(trigger, proofs, ctx))
                .map_err(|e| format!("unable to serialize payload {}", e))?;
            Ok(StacksChainhookOccurrence::File(
                disk.path.to_string(),
                bytes,
            ))
        }
        HookAction::Noop => Ok(StacksChainhookOccurrence::Data(
            StacksChainhookOccurrencePayload::from_trigger(trigger),
        )),
    }
}

#[cfg(test)]
pub mod tests;
