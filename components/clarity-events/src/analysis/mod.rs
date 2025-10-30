use std::collections::{BTreeMap, HashMap};

use clarity_repl::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use clarity_repl::clarity::analysis::type_checker::v2_05::TypeChecker;
use clarity_repl::clarity::util::hash;
use clarity_repl::clarity::vm::analysis::types::ContractAnalysis;
use clarity_repl::clarity::vm::types::{
    AssetIdentifier, BuffData, CharType, PrincipalData, QualifiedContractIdentifier, SequenceData,
    SequenceSubtype, StringSubtype, TypeSignature, Value,
};
use clarity_repl::clarity::vm::{ClarityName, SymbolicExpression};
use clarity_repl::clarity::{ClarityVersion, SymbolicExpressionType};
use clarity_repl::repl::clarity_values::value_to_string;
use serde::ser::SerializeMap;
use serde::{Serialize, Serializer};
use serde_json::Value as JsonValue;

pub struct Settings {
    pub clarity_version: ClarityVersion,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StacksTransactionEvent {
    SmartContractEvent(SmartContractEventData),
    STXEvent(STXEventType),
    NFTEvent(NFTEventType),
    FTEvent(FTEventType),
}

impl Serialize for StacksTransactionEvent {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        match &self {
            StacksTransactionEvent::SmartContractEvent(data) => {
                map.serialize_entry("event_type", "print")?;
                map.serialize_entry("print", data)?;
            }
            StacksTransactionEvent::STXEvent(data) => match &data {
                STXEventType::STXBurnEvent(_sub_data) => {
                    map.serialize_entry("event_type", "burn_stx_event")?;
                }
                STXEventType::STXLockEvent(_sub_data) => {
                    map.serialize_entry("event_type", "lock_stx_event")?;
                }
                STXEventType::STXMintEvent(_sub_data) => {
                    map.serialize_entry("event_type", "mint_stx_event")?;
                }
                STXEventType::STXTransferEvent(_sub_data) => {
                    map.serialize_entry("event_type", "transfer_stx_event")?;
                }
            },
            StacksTransactionEvent::NFTEvent(data) => match &data {
                NFTEventType::NFTBurnEvent(sub_data) => {
                    map.serialize_entry("event_type", "burn_nft_event")?;
                    map.serialize_entry("burn_nft_event", sub_data)?;
                }
                NFTEventType::NFTMintEvent(sub_data) => {
                    map.serialize_entry("event_type", "mint_nft_event")?;
                    map.serialize_entry("mint_nft_event", sub_data)?;
                }
                NFTEventType::NFTTransferEvent(sub_data) => {
                    map.serialize_entry("event_type", "transfer_nft_event")?;
                    map.serialize_entry("transfer_nft_event", sub_data)?;
                }
            },
            StacksTransactionEvent::FTEvent(data) => match &data {
                FTEventType::FTBurnEvent(sub_data) => {
                    map.serialize_entry("event_type", "burn_ft_event")?;
                    map.serialize_entry("burn_ft_event", sub_data)?;
                }
                FTEventType::FTMintEvent(sub_data) => {
                    map.serialize_entry("event_type", "mint_ft_event")?;
                    map.serialize_entry("mint_ft_event", sub_data)?;
                }
                FTEventType::FTTransferEvent(sub_data) => {
                    map.serialize_entry("event_type", "transfer_ft_event")?;
                    map.serialize_entry("transfer_ft_event", sub_data)?;
                }
            },
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum STXEventType {
    STXTransferEvent(STXTransferEventData),
    STXMintEvent(STXMintEventData),
    STXBurnEvent(STXBurnEventData),
    STXLockEvent(STXLockEventData),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum NFTEventType {
    NFTTransferEvent(NFTTransferEventData),
    NFTMintEvent(NFTMintEventData),
    NFTBurnEvent(NFTBurnEventData),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum FTEventType {
    FTTransferEvent(FTTransferEventData),
    FTMintEvent(FTMintEventData),
    FTBurnEvent(FTBurnEventData),
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct STXTransferEventData {
    pub sender: Option<PrincipalData>,
    pub recipient: Option<PrincipalData>,
    pub amount: Option<u128>,
    pub memo: Option<BuffData>,
}

impl Serialize for STXTransferEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        if let Some(ref sender) = self.sender {
            map.serialize_entry("sender", &sender.to_string())?;
        }
        if let Some(ref recipient) = self.recipient {
            map.serialize_entry("recipient", &recipient.to_string())?;
        }
        if let Some(ref amount) = self.amount {
            map.serialize_entry("amount", &amount)?;
        }
        if let Some(ref memo) = self.memo {
            map.serialize_entry("memo", &memo)?;
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct STXMintEventData {
    pub recipient: Option<PrincipalData>,
    pub amount: Option<u128>,
}

impl Serialize for STXMintEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        if let Some(ref recipient) = self.recipient {
            map.serialize_entry("recipient", &recipient.to_string())?;
        }
        if let Some(ref amount) = self.amount {
            map.serialize_entry("amount", &amount)?;
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct STXLockEventData {
    pub locked_amount: Option<u128>,
    pub unlock_height: Option<u64>,
    pub locked_address: Option<PrincipalData>,
    pub contract_identifier: Option<QualifiedContractIdentifier>,
}

impl Serialize for STXLockEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        if let Some(ref locked_amount) = self.locked_amount {
            map.serialize_entry("locked_amount", &locked_amount.to_string())?;
        }
        if let Some(ref unlock_height) = self.unlock_height {
            map.serialize_entry("unlock_height", &unlock_height.to_string())?;
        }
        if let Some(ref locked_address) = self.locked_address {
            map.serialize_entry("locked_address", &locked_address)?;
        }
        if let Some(ref contract_identifier) = self.contract_identifier {
            map.serialize_entry("contract_identifier", &contract_identifier.to_string())?;
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct STXBurnEventData {
    pub sender: Option<PrincipalData>,
    pub amount: Option<u128>,
}

impl Serialize for STXBurnEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        if let Some(ref sender) = self.sender {
            map.serialize_entry("sender", &sender.to_string())?;
        }
        if let Some(ref amount) = self.amount {
            map.serialize_entry("amount", &amount)?;
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NFTTransferEventData {
    pub asset_identifier: AssetIdentifier,
    pub sender: Option<PrincipalData>,
    pub recipient: Option<PrincipalData>,
    pub value: Option<JsonValue>,
}

impl NFTTransferEventData {
    pub fn default(asset_identifier: AssetIdentifier) -> NFTTransferEventData {
        NFTTransferEventData {
            asset_identifier,
            sender: None,
            recipient: None,
            value: None,
        }
    }
}

impl Serialize for NFTTransferEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("asset_identifier", &self.asset_identifier.to_string())?;
        if let Some(ref sender) = self.sender {
            map.serialize_entry("sender", &sender.to_string())?;
        }
        if let Some(ref recipient) = self.recipient {
            map.serialize_entry("sender", &recipient.to_string())?;
        }
        if let Some(ref value) = self.value {
            map.serialize_entry("value", value)?;
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NFTMintEventData {
    pub asset_identifier: AssetIdentifier,
    pub recipient: Option<PrincipalData>,
    pub value: Option<JsonValue>,
}

impl NFTMintEventData {
    pub fn default(asset_identifier: AssetIdentifier) -> NFTMintEventData {
        NFTMintEventData {
            asset_identifier,
            recipient: None,
            value: None,
        }
    }
}

impl Serialize for NFTMintEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("asset_identifier", &self.asset_identifier.to_string())?;
        if let Some(ref recipient) = self.recipient {
            map.serialize_entry("sender", &recipient.to_string())?;
        }
        if let Some(ref value) = self.value {
            map.serialize_entry("value", &value)?;
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NFTBurnEventData {
    pub asset_identifier: AssetIdentifier,
    pub sender: Option<PrincipalData>,
    pub value: Option<JsonValue>,
}

impl NFTBurnEventData {
    pub fn default(asset_identifier: AssetIdentifier) -> NFTBurnEventData {
        NFTBurnEventData {
            asset_identifier,
            sender: None,
            value: None,
        }
    }
}

impl Serialize for NFTBurnEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("asset_identifier", &self.asset_identifier.to_string())?;
        if let Some(ref sender) = self.sender {
            map.serialize_entry("sender", &sender.to_string())?;
        }
        if let Some(ref value) = self.value {
            map.serialize_entry("value", value)?;
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FTTransferEventData {
    pub asset_identifier: AssetIdentifier,
    pub sender: Option<PrincipalData>,
    pub recipient: Option<PrincipalData>,
    pub amount: Option<u128>,
}

impl FTTransferEventData {
    pub fn default(asset_identifier: AssetIdentifier) -> FTTransferEventData {
        FTTransferEventData {
            asset_identifier,
            sender: None,
            recipient: None,
            amount: None,
        }
    }
}

impl Serialize for FTTransferEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("asset_identifier", &self.asset_identifier.to_string())?;
        if let Some(ref sender) = self.sender {
            map.serialize_entry("sender", &sender.to_string())?;
        }
        if let Some(ref recipient) = self.recipient {
            map.serialize_entry("sender", &recipient.to_string())?;
        }
        if let Some(ref amount) = self.amount {
            map.serialize_entry("amount", amount)?;
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FTMintEventData {
    pub asset_identifier: AssetIdentifier,
    pub recipient: Option<PrincipalData>,
    pub amount: Option<u128>,
}

impl FTMintEventData {
    pub fn default(asset_identifier: AssetIdentifier) -> FTMintEventData {
        FTMintEventData {
            asset_identifier,
            recipient: None,
            amount: None,
        }
    }
}

impl Serialize for FTMintEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("asset_identifier", &self.asset_identifier.to_string())?;
        if let Some(ref recipient) = self.recipient {
            map.serialize_entry("sender", &recipient.to_string())?;
        }
        if let Some(ref amount) = self.amount {
            map.serialize_entry("amount", amount)?;
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FTBurnEventData {
    pub asset_identifier: AssetIdentifier,
    pub sender: Option<PrincipalData>,
    pub amount: Option<u128>,
}

impl FTBurnEventData {
    pub fn default(asset_identifier: AssetIdentifier) -> FTBurnEventData {
        FTBurnEventData {
            asset_identifier,
            sender: None,
            amount: None,
        }
    }
}

impl Serialize for FTBurnEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("asset_identifier", &self.asset_identifier.to_string())?;
        if let Some(ref sender) = self.sender {
            map.serialize_entry("sender", &sender.to_string())?;
        }
        if let Some(ref amount) = self.amount {
            map.serialize_entry("amount", amount)?;
        }
        map.end()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SmartContractEventData {
    pub data_type: JsonValue,
}

impl Serialize for SmartContractEventData {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry("data_type", &self.data_type)?;
        map.end()
    }
}

pub struct EventCollector<'a, 'b> {
    pub event_map: BTreeMap<Option<ClarityName>, Vec<StacksTransactionEvent>>,
    pub settings: Settings,
    pub type_checker: TypeChecker<'a, 'b>,
    pub last_entries: Vec<StacksTransactionEvent>,
    pub contract_identifier: Option<QualifiedContractIdentifier>,
}

impl<'a, 'b> EventCollector<'a, 'b> {
    pub fn new(settings: Settings, type_checker: TypeChecker<'a, 'b>) -> EventCollector<'a, 'b> {
        let mut event_map = BTreeMap::new();
        event_map.insert(None, vec![]);
        Self {
            event_map,
            settings,
            type_checker,
            last_entries: vec![],
            contract_identifier: None,
        }
    }

    pub fn run<'c>(
        &'c mut self,
        contract_analysis: &'c mut ContractAnalysis,
    ) -> BTreeMap<Option<ClarityName>, Vec<StacksTransactionEvent>> {
        let _ = self.type_checker.run(contract_analysis);
        self.contract_identifier = Some(contract_analysis.contract_identifier.clone());
        traverse(self, &contract_analysis.expressions);
        self.event_map.clone()
    }

    pub fn add_method(&mut self, method: ClarityName) {
        let mut events = vec![];
        events.append(&mut self.last_entries);
        self.event_map.insert(Some(method), events);
    }

    pub fn add_event(&mut self, event: StacksTransactionEvent) {
        self.last_entries.push(event);
    }

    pub fn create_asset_identifier(&self, asset_name: ClarityName) -> AssetIdentifier {
        AssetIdentifier {
            contract_identifier: self.contract_identifier.clone().unwrap(),
            asset_name,
        }
    }
}

pub fn serialize_type_signature(
    type_signature: &TypeSignature,
    expr: &SymbolicExpression,
) -> JsonValue {
    match (&type_signature, &expr.expr) {
        (TypeSignature::BoolType, SymbolicExpressionType::AtomValue(value))
        | (TypeSignature::BoolType, SymbolicExpressionType::LiteralValue(value)) => {
            json!({
                "value": value.to_string(),
                "type": "boolean",
            })
        }
        (TypeSignature::BoolType, _) => {
            json!({
                "type": "boolean",
            })
        }
        (TypeSignature::UIntType, SymbolicExpressionType::AtomValue(value))
        | (TypeSignature::UIntType, SymbolicExpressionType::LiteralValue(value)) => {
            json!({
                "value": value.to_string(),
                "type": "uint",
            })
        }
        (TypeSignature::UIntType, _) => {
            json!({
                "type": "uint",
            })
        }
        (TypeSignature::PrincipalType, SymbolicExpressionType::AtomValue(value))
        | (TypeSignature::PrincipalType, SymbolicExpressionType::LiteralValue(value)) => {
            json!({
                "value": value_to_string(value),
                "type": "principal",
            })
        }
        (TypeSignature::PrincipalType, _) => {
            json!({
                "type": "principal",
            })
        }
        (
            TypeSignature::SequenceType(SequenceSubtype::StringType(StringSubtype::ASCII(_))),
            SymbolicExpressionType::AtomValue(value),
        )
        | (
            TypeSignature::SequenceType(SequenceSubtype::StringType(StringSubtype::ASCII(_))),
            SymbolicExpressionType::LiteralValue(value),
        ) => {
            json!({
                "value": value.clone().expect_ascii().expect("failed to parse ascii"),
                "type": "string",
            })
        }
        (TypeSignature::SequenceType(SequenceSubtype::StringType(StringSubtype::ASCII(_))), _) => {
            json!({
                "type": "string",
            })
        }
        (
            TypeSignature::SequenceType(SequenceSubtype::StringType(StringSubtype::UTF8(_))),
            SymbolicExpressionType::AtomValue(value),
        )
        | (
            TypeSignature::SequenceType(SequenceSubtype::StringType(StringSubtype::UTF8(_))),
            SymbolicExpressionType::LiteralValue(value),
        ) => {
            if let Value::Sequence(SequenceData::String(CharType::UTF8(data))) = value {
                let mut result = String::new();
                for c in data.data.iter() {
                    if c.len() > 1 {
                        // We escape extended charset
                        result.push_str(&format!("\\u{{{}}}", hash::to_hex(&c[..])));
                    } else {
                        result.push(c[0] as char)
                    }
                }
                json!({
                    "value": result,
                    "type": "string",
                })
            } else {
                json!({
                    "type": "string",
                })
            }
        }
        (TypeSignature::SequenceType(SequenceSubtype::StringType(StringSubtype::UTF8(_))), _) => {
            json!({
                "type": "string",
            })
        }
        (TypeSignature::TupleType(tuple_signature), _) => {
            let mut tuple = BTreeMap::new();
            let comps = expr.match_list().unwrap();
            for ((key, type_signature), expr) in
                tuple_signature.get_type_map().iter().zip(comps[1..].iter())
            {
                let value = &expr.match_list().unwrap()[1];
                tuple.insert(
                    key.to_string(),
                    serialize_type_signature(type_signature, value),
                );
            }
            json!(tuple)
        }
        _ => json!(""),
    }
}

impl ASTVisitor<'_> for EventCollector<'_, '_> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.settings.clarity_version
    }

    fn visit_define_public(
        &mut self,
        _expr: &SymbolicExpression,
        name: &ClarityName,
        _parameters: Option<Vec<TypedVar<'_>>>,
        _body: &SymbolicExpression,
    ) -> bool {
        self.add_method(name.clone());
        true
    }

    fn visit_define_private(
        &mut self,
        _expr: &SymbolicExpression,
        name: &ClarityName,
        _parameters: Option<Vec<TypedVar<'_>>>,
        _body: &SymbolicExpression,
    ) -> bool {
        self.add_method(name.clone());
        true
    }

    fn visit_define_read_only(
        &mut self,
        _expr: &SymbolicExpression,
        name: &ClarityName,
        _parameters: Option<Vec<TypedVar<'_>>>,
        _body: &SymbolicExpression,
    ) -> bool {
        self.add_method(name.clone());
        true
    }

    fn visit_print(&mut self, expr: &SymbolicExpression, value: &SymbolicExpression) -> bool {
        let value_type_shape = self
            .type_checker
            .type_map
            .get_type_expected(expr)
            .expect("unable to infer value's type shape");

        let data = SmartContractEventData {
            data_type: serialize_type_signature(value_type_shape, value),
        };
        self.add_event(StacksTransactionEvent::SmartContractEvent(data));
        true
    }

    fn visit_stx_burn(
        &mut self,
        _expr: &SymbolicExpression,
        amount: &SymbolicExpression,
        sender: &SymbolicExpression,
    ) -> bool {
        let mut data = STXBurnEventData::default();
        match &amount.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.amount = Some(value.clone().expect_u128().expect("failed to parse u128"));
            }
            _ => {}
        }
        match &sender.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.sender = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        self.add_event(StacksTransactionEvent::STXEvent(
            STXEventType::STXBurnEvent(data),
        ));
        true
    }

    fn visit_stx_transfer(
        &mut self,
        _expr: &SymbolicExpression,
        amount: &SymbolicExpression,
        sender: &SymbolicExpression,
        recipient: &SymbolicExpression,
        _memo: Option<&SymbolicExpression>,
    ) -> bool {
        let mut data = STXTransferEventData::default();
        match &amount.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.amount = Some(value.clone().expect_u128().expect("failed to parse u128"));
            }
            _ => {}
        }
        match &sender.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.sender = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        match &recipient.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.recipient = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        self.add_event(StacksTransactionEvent::STXEvent(
            STXEventType::STXTransferEvent(data),
        ));
        true
    }

    fn visit_ft_burn(
        &mut self,
        _expr: &SymbolicExpression,
        token: &ClarityName,
        amount: &SymbolicExpression,
        sender: &SymbolicExpression,
    ) -> bool {
        let mut data = FTBurnEventData::default(self.create_asset_identifier(token.clone()));
        match &amount.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.amount = Some(value.clone().expect_u128().expect("failed to parse u128"));
            }
            _ => {}
        }
        match &sender.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.sender = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        self.add_event(StacksTransactionEvent::FTEvent(FTEventType::FTBurnEvent(
            data,
        )));
        true
    }

    fn visit_ft_transfer(
        &mut self,
        _expr: &SymbolicExpression,
        token: &ClarityName,
        amount: &SymbolicExpression,
        sender: &SymbolicExpression,
        recipient: &SymbolicExpression,
    ) -> bool {
        let mut data = FTTransferEventData::default(self.create_asset_identifier(token.clone()));
        match &amount.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.amount = Some(value.clone().expect_u128().expect("failed to parse u128"));
            }
            _ => {}
        }
        match &sender.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.sender = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        match &recipient.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.recipient = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        self.add_event(StacksTransactionEvent::FTEvent(
            FTEventType::FTTransferEvent(data),
        ));
        true
    }

    fn visit_ft_mint(
        &mut self,
        _expr: &SymbolicExpression,
        token: &ClarityName,
        amount: &SymbolicExpression,
        recipient: &SymbolicExpression,
    ) -> bool {
        let mut data = FTMintEventData::default(self.create_asset_identifier(token.clone()));
        match &amount.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.amount = Some(value.clone().expect_u128().expect("failed to parse u128"));
            }
            _ => {}
        }
        match &recipient.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.recipient = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        self.add_event(StacksTransactionEvent::FTEvent(FTEventType::FTMintEvent(
            data,
        )));
        true
    }

    fn visit_nft_burn(
        &mut self,
        _expr: &SymbolicExpression,
        token: &ClarityName,
        identifier: &SymbolicExpression,
        sender: &SymbolicExpression,
    ) -> bool {
        let type_signature = self
            .type_checker
            .type_map
            .get_type_expected(identifier)
            .expect("unable to infer value's type shape");

        let mut data = NFTBurnEventData::default(self.create_asset_identifier(token.clone()));
        match &identifier.expr {
            SymbolicExpressionType::AtomValue(_value)
            | SymbolicExpressionType::LiteralValue(_value) => {
                data.value = Some(serialize_type_signature(type_signature, identifier));
            }
            _ => {}
        }
        match &sender.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.sender = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        self.add_event(StacksTransactionEvent::NFTEvent(
            NFTEventType::NFTBurnEvent(data),
        ));

        true
    }

    fn visit_nft_transfer(
        &mut self,
        _expr: &SymbolicExpression,
        token: &ClarityName,
        identifier: &SymbolicExpression,
        sender: &SymbolicExpression,
        recipient: &SymbolicExpression,
    ) -> bool {
        let type_signature = self
            .type_checker
            .type_map
            .get_type_expected(identifier)
            .expect("unable to infer value's type shape");

        let mut data = NFTTransferEventData::default(self.create_asset_identifier(token.clone()));
        match &identifier.expr {
            SymbolicExpressionType::AtomValue(_value)
            | SymbolicExpressionType::LiteralValue(_value) => {
                data.value = Some(serialize_type_signature(type_signature, identifier));
            }
            _ => {}
        }
        match &sender.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.sender = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        match &recipient.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.recipient = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        self.add_event(StacksTransactionEvent::NFTEvent(
            NFTEventType::NFTTransferEvent(data),
        ));

        true
    }

    fn visit_nft_mint(
        &mut self,
        _expr: &SymbolicExpression,
        token: &ClarityName,
        identifier: &SymbolicExpression,
        recipient: &SymbolicExpression,
    ) -> bool {
        let type_signature = self
            .type_checker
            .type_map
            .get_type_expected(identifier)
            .expect("unable to infer value's type shape");

        let mut data = NFTTransferEventData::default(self.create_asset_identifier(token.clone()));
        match &identifier.expr {
            SymbolicExpressionType::AtomValue(_value)
            | SymbolicExpressionType::LiteralValue(_value) => {
                data.value = Some(serialize_type_signature(type_signature, identifier));
            }
            _ => {}
        }
        match &recipient.expr {
            SymbolicExpressionType::AtomValue(value)
            | SymbolicExpressionType::LiteralValue(value) => {
                data.recipient = Some(
                    value
                        .clone()
                        .expect_principal()
                        .expect("failed to parse principal"),
                );
            }
            _ => {}
        }
        self.add_event(StacksTransactionEvent::NFTEvent(
            NFTEventType::NFTTransferEvent(data),
        ));

        true
    }

    fn visit_var_set(
        &mut self,
        _expr: &SymbolicExpression,
        _name: &ClarityName,
        _value: &SymbolicExpression,
    ) -> bool {
        true
    }

    fn visit_map_set(
        &mut self,
        _expr: &SymbolicExpression,
        _name: &ClarityName,
        _key: &HashMap<Option<&ClarityName>, &SymbolicExpression>,
        _value: &HashMap<Option<&ClarityName>, &SymbolicExpression>,
    ) -> bool {
        true
    }

    fn visit_map_insert(
        &mut self,
        _expr: &SymbolicExpression,
        _name: &ClarityName,
        _key: &HashMap<Option<&ClarityName>, &SymbolicExpression>,
        _value: &HashMap<Option<&ClarityName>, &SymbolicExpression>,
    ) -> bool {
        true
    }

    fn visit_map_delete(
        &mut self,
        _expr: &SymbolicExpression,
        _name: &ClarityName,
        _key: &HashMap<Option<&ClarityName>, &SymbolicExpression>,
    ) -> bool {
        true
    }

    fn visit_dynamic_contract_call(
        &mut self,
        _expr: &SymbolicExpression,
        _trait_ref: &SymbolicExpression,
        _function_name: &ClarityName,
        _args: &[SymbolicExpression],
    ) -> bool {
        true
    }
}
