use js_sys::wasm_bindgen;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(typescript_custom_section)]
const EPOCH_STRING: &'static str = r#"export type EpochString =
  | "2.0"
  | "2.05"
  | "2.1"
  | "2.2"
  | "2.3"
  | "2.4"
  | "2.5"
  | "3.0"
  | "3.1"
  | "3.2"
  | "3.3";
"#;

// CONTRACT AST

#[wasm_bindgen(typescript_custom_section)]
const ATOM_STRING: &'static str = r#"type Atom = {
  Atom: String;
};"#;

#[wasm_bindgen(typescript_custom_section)]
const ATOM_VALUE_STRING: &'static str = r#"type AtomValue = {
  AtomValue: any;
};"#;

#[wasm_bindgen(typescript_custom_section)]
const LIST_STRING: &'static str = r#"type List = {
  List: Expression[];
};"#;

#[wasm_bindgen(typescript_custom_section)]
const LITERAL_VALUE_STRING: &'static str = r#"type LiteralValue = {
  LiteralValue: any;
};"#;

#[wasm_bindgen(typescript_custom_section)]
const FIELD_STRING: &'static str = r#"type Field = {
  Field: any;
};"#;

#[wasm_bindgen(typescript_custom_section)]
const TRAIT_REFERENCE_STRING: &'static str = r#"type TraitReference = {
  TraitReference: any;
};"#;

#[wasm_bindgen(typescript_custom_section)]
const EXPRESSION_TYPE_STRING: &'static str =
    r#"type ExpressionType = Atom | AtomValue | List | LiteralValue | Field | TraitReference;"#;

#[wasm_bindgen(typescript_custom_section)]
const SPAN_STRING: &'static str = r#"type Span = {
  start_line: number;
  start_column: number;
  end_line: number;
  end_column: number;
};"#;

#[wasm_bindgen(typescript_custom_section)]
const EXPRESSION_STRING: &'static str = r#"type Expression = {
  expr: ExpressionType;
  id: number;
  span: Span;
};"#;

// To avoid collision with the Rust type ContractAST, prefix with the conventional typescript I
#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_AST_STRING: &'static str = r#"type IContractAST = {
  contract_identifier: any;
  pre_expressions: any[];
  expressions: Expression[];
  top_level_expression_sorting: number[];
  referenced_traits: Map<any, any>;
  implemented_traits: any[];
};"#;

// CONTRACT INTERFACE

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_FUNCTION_ACCESS_STRING: &'static str =
    r#"type ContractInterfaceFunctionAccess = "private" | "public" | "read_only";"#;

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_TUPLE_ENTRY_TYPE_STRING: &'static str =
    r#"type ContractInterfaceTupleEntryType = { name: string; type: ContractInterfaceAtomType };"#;

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_ATOM_TYPE_STRING: &'static str = r#"type ContractInterfaceAtomType =
  | "none"
  | "int128"
  | "uint128"
  | "bool"
  | "principal"
  | { buffer: { length: number } }
  | { "string-utf8": { length: number } }
  | { "string-ascii": { length: number } }
  | { tuple: ContractInterfaceTupleEntryType[] }
  | { optional: ContractInterfaceAtomType }
  | { response: { ok: ContractInterfaceAtomType; error: ContractInterfaceAtomType } }
  | { list: { type: ContractInterfaceAtomType; length: number } }
  | "trait_reference";"#;

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_FUNCTION_ARG_STRING: &'static str =
    r#"type ContractInterfaceFunctionArg = { name: string; type: ContractInterfaceAtomType };"#;

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_FUNCTION_OUTPUT_STRING: &'static str =
    r#"type ContractInterfaceFunctionOutput = { type: ContractInterfaceAtomType };"#;

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_FUNCTION_STRING: &'static str = r#"type ContractInterfaceFunction = {
  name: string;
  access: ContractInterfaceFunctionAccess;
  args: ContractInterfaceFunctionArg[];
  outputs: ContractInterfaceFunctionOutput;
};"#;

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_VARIABLE_ACCESS_STRING: &'static str =
    r#"type ContractInterfaceVariableAccess = "constant" | "variable";"#;

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_VARIABLE_STRING: &'static str = r#"type ContractInterfaceVariable = {
  name: string;
  type: ContractInterfaceAtomType;
  access: ContractInterfaceVariableAccess;
};"#;

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_MAP_STRING: &'static str = r#"type ContractInterfaceMap = {
  name: string;
  key: ContractInterfaceAtomType;
  value: ContractInterfaceAtomType;
};"#;

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_FUNGIBLE_TOKENS_STRING: &'static str =
    r#"type ContractInterfaceFungibleTokens = { name: string };"#;

#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_NON_FUNGIBLE_TOKENS_STRING: &'static str = r#"type ContractInterfaceNonFungibleTokens = { name: string; type: ContractInterfaceAtomType };"#;

#[wasm_bindgen(typescript_custom_section)]
const STACKS_EPOCH_ID_STRING: &'static str = r#"export type StacksEpochId =
  | "Epoch10"
  | "Epoch20"
  | "Epoch2_05"
  | "Epoch21"
  | "Epoch22"
  | "Epoch23"
  | "Epoch24"
  | "Epoch25"
  | "Epoch30"
  | "Epoch31"
  | "Epoch32"
  | "Epoch33";"#;

#[wasm_bindgen(typescript_custom_section)]
const CLARITY_VERSION_STRING: &'static str =
    r#"export type ClarityVersionString = "Clarity1" | "Clarity2" | "Clarity3"| "Clarity4";"#;

// To avoid collision with the Rust type ContractAST, prefix with the conventional typescript I
#[wasm_bindgen(typescript_custom_section)]
const CONTRACT_INTERFACE_STRING: &'static str = r#"export type IContractInterface = {
  functions: ContractInterfaceFunction[];
  variables: ContractInterfaceVariable[];
  maps: ContractInterfaceMap[];
  fungible_tokens: ContractInterfaceFungibleTokens[];
  non_fungible_tokens: ContractInterfaceNonFungibleTokens[];
  epoch: StacksEpochId;
  clarity_version: ClarityVersionString;
};"#;
