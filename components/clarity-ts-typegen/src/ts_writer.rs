use std::collections::BTreeSet;
use std::fmt::Write;

use clarity::vm::analysis::contract_interface_builder::{
    ContractInterface, ContractInterfaceAtomType, ContractInterfaceFunction,
    ContractInterfaceFunctionAccess, ContractInterfaceVariableAccess,
};

use super::naming::{clarity_to_camel, clarity_to_pascal};

/// Generate the full TypeScript file content for a single contract.
pub fn generate_contract_types(contract_name: &str, interface: &ContractInterface) -> String {
    let mut imports = BTreeSet::new();
    let mut body = String::new();

    let pascal_name = clarity_to_pascal(contract_name);

    // --- Functions (public + read_only only) ---
    let public_fns: Vec<&ContractInterfaceFunction> = interface
        .functions
        .iter()
        .filter(|f| {
            matches!(
                f.access,
                ContractInterfaceFunctionAccess::public
                    | ContractInterfaceFunctionAccess::read_only
            )
        })
        .collect();

    if !public_fns.is_empty() {
        writeln!(body, "// Functions\n").unwrap();
        for func in &public_fns {
            let type_prefix = clarity_to_pascal(&func.name);

            // Args type
            if func.args.is_empty() {
                writeln!(
                    body,
                    "export type {type_prefix}Args = Record<string, never>;"
                )
                .unwrap();
            } else {
                write!(body, "export type {type_prefix}Args = {{ ").unwrap();
                for (i, arg) in func.args.iter().enumerate() {
                    if i > 0 {
                        write!(body, "; ").unwrap();
                    }
                    let field_name = clarity_to_camel(&arg.name);
                    let ts_type = atom_type_to_ts(&arg.type_f, &mut imports);
                    write!(body, "{field_name}: {ts_type}").unwrap();
                }
                writeln!(body, " }};").unwrap();
            }

            // Return type
            let return_type = atom_type_to_ts(&func.outputs.type_f, &mut imports);
            writeln!(body, "export type {type_prefix}Return = {return_type};\n").unwrap();
        }
    }

    // --- Maps ---
    if !interface.maps.is_empty() {
        writeln!(body, "// Maps\n").unwrap();
        for map in &interface.maps {
            let type_prefix = clarity_to_pascal(&map.name);
            let key_type = atom_type_to_ts(&map.key, &mut imports);
            let value_type = atom_type_to_ts(&map.value, &mut imports);
            writeln!(body, "export type {type_prefix}MapKey = {key_type};").unwrap();
            writeln!(body, "export type {type_prefix}MapValue = {value_type};\n").unwrap();
        }
    }

    // --- Variables (data-var) ---
    let vars: Vec<_> = interface
        .variables
        .iter()
        .filter(|v| matches!(v.access, ContractInterfaceVariableAccess::variable))
        .collect();
    if !vars.is_empty() {
        writeln!(body, "// Variables\n").unwrap();
        for var in &vars {
            let type_name = clarity_to_pascal(&var.name);
            let ts_type = atom_type_to_ts(&var.type_f, &mut imports);
            writeln!(body, "export type {type_name}Variable = {ts_type};\n").unwrap();
        }
    }

    // --- Constants ---
    let constants: Vec<_> = interface
        .variables
        .iter()
        .filter(|v| matches!(v.access, ContractInterfaceVariableAccess::constant))
        .collect();
    if !constants.is_empty() {
        writeln!(body, "// Constants\n").unwrap();
        for constant in &constants {
            let type_name = clarity_to_pascal(&constant.name);
            let ts_type = atom_type_to_ts(&constant.type_f, &mut imports);
            writeln!(body, "export type {type_name}Constant = {ts_type};\n").unwrap();
        }
    }

    // --- Non-Fungible Tokens ---
    if !interface.non_fungible_tokens.is_empty() {
        writeln!(body, "// Non-Fungible Tokens\n").unwrap();
        for nft in &interface.non_fungible_tokens {
            let type_name = clarity_to_pascal(&nft.name);
            let ts_type = atom_type_to_ts(&nft.type_f, &mut imports);
            writeln!(body, "export type {type_name}NftAsset = {ts_type};\n").unwrap();
        }
    }

    // --- Contract interface type ---
    writeln!(body, "// Contract\n").unwrap();
    writeln!(body, "export interface {pascal_name}Contract {{").unwrap();

    // functions
    if public_fns.is_empty() {
        writeln!(body, "  functions: Record<string, never>;").unwrap();
    } else {
        writeln!(body, "  functions: {{").unwrap();
        for func in &public_fns {
            let key = clarity_to_camel(&func.name);
            let type_prefix = clarity_to_pascal(&func.name);
            writeln!(
                body,
                "    {key}: {{ args: {type_prefix}Args; return: {type_prefix}Return }};"
            )
            .unwrap();
        }
        writeln!(body, "  }};").unwrap();
    }

    // maps
    if interface.maps.is_empty() {
        writeln!(body, "  maps: Record<string, never>;").unwrap();
    } else {
        writeln!(body, "  maps: {{").unwrap();
        for map in &interface.maps {
            let key = clarity_to_camel(&map.name);
            let type_prefix = clarity_to_pascal(&map.name);
            writeln!(
                body,
                "    {key}: {{ key: {type_prefix}MapKey; value: {type_prefix}MapValue }};"
            )
            .unwrap();
        }
        writeln!(body, "  }};").unwrap();
    }

    // variables
    if vars.is_empty() {
        writeln!(body, "  variables: Record<string, never>;").unwrap();
    } else {
        writeln!(body, "  variables: {{").unwrap();
        for var in &vars {
            let key = clarity_to_camel(&var.name);
            let type_name = clarity_to_pascal(&var.name);
            writeln!(body, "    {key}: {type_name}Variable;").unwrap();
        }
        writeln!(body, "  }};").unwrap();
    }

    // constants
    if constants.is_empty() {
        writeln!(body, "  constants: Record<string, never>;").unwrap();
    } else {
        writeln!(body, "  constants: {{").unwrap();
        for constant in &constants {
            let key = clarity_to_camel(&constant.name);
            let type_name = clarity_to_pascal(&constant.name);
            writeln!(body, "    {key}: {type_name}Constant;").unwrap();
        }
        writeln!(body, "  }};").unwrap();
    }

    // fungible tokens
    if interface.fungible_tokens.is_empty() {
        writeln!(body, "  fungibleTokens: never;").unwrap();
    } else {
        let ft_union: Vec<String> = interface
            .fungible_tokens
            .iter()
            .map(|ft| format!("\"{}\"", ft.name))
            .collect();
        writeln!(body, "  fungibleTokens: {};", ft_union.join(" | ")).unwrap();
    }

    // non-fungible tokens
    if interface.non_fungible_tokens.is_empty() {
        writeln!(body, "  nonFungibleTokens: Record<string, never>;").unwrap();
    } else {
        writeln!(body, "  nonFungibleTokens: {{").unwrap();
        for nft in &interface.non_fungible_tokens {
            let key = clarity_to_camel(&nft.name);
            let type_name = clarity_to_pascal(&nft.name);
            writeln!(body, "    {key}: {type_name}NftAsset;").unwrap();
        }
        writeln!(body, "  }};").unwrap();
    }

    writeln!(body, "}}").unwrap();

    // --- Assemble final file ---
    let mut output = String::new();
    writeln!(output, "// This file is auto-generated by clarinet typegen").unwrap();
    writeln!(output, "// Contract: {contract_name}\n").unwrap();

    // Import block (only types actually used)
    if !imports.is_empty() {
        let import_list: Vec<&str> = imports.iter().copied().collect();
        writeln!(output, "import type {{").unwrap();
        for imp in &import_list {
            writeln!(output, "  {imp},").unwrap();
        }
        writeln!(output, "}} from \"@stacks/transactions\";\n").unwrap();
    }

    output.push_str(&body);
    output
}

/// Convert a `ContractInterfaceAtomType` to its TypeScript ClarityValue type string.
/// Tracks which imports are needed in the `imports` set.
fn atom_type_to_ts(ty: &ContractInterfaceAtomType, imports: &mut BTreeSet<&str>) -> String {
    match ty {
        ContractInterfaceAtomType::none => {
            imports.insert("NoneCV");
            "NoneCV".to_string()
        }
        ContractInterfaceAtomType::int128 => {
            imports.insert("IntCV");
            "IntCV".to_string()
        }
        ContractInterfaceAtomType::uint128 => {
            imports.insert("UIntCV");
            "UIntCV".to_string()
        }
        ContractInterfaceAtomType::bool => {
            imports.insert("BooleanCV");
            "BooleanCV".to_string()
        }
        ContractInterfaceAtomType::principal => {
            imports.insert("PrincipalCV");
            "PrincipalCV".to_string()
        }
        ContractInterfaceAtomType::buffer { .. } => {
            imports.insert("BufferCV");
            "BufferCV".to_string()
        }
        ContractInterfaceAtomType::string_utf8 { .. } => {
            imports.insert("StringUtf8CV");
            "StringUtf8CV".to_string()
        }
        ContractInterfaceAtomType::string_ascii { .. } => {
            imports.insert("StringAsciiCV");
            "StringAsciiCV".to_string()
        }
        ContractInterfaceAtomType::tuple(entries) => {
            imports.insert("TupleCV");
            let fields: Vec<String> = entries
                .iter()
                .map(|entry| {
                    let field_name = clarity_to_camel(&entry.name);
                    let field_type = atom_type_to_ts(&entry.type_f, imports);
                    format!("{field_name}: {field_type}")
                })
                .collect();
            format!("TupleCV<{{ {} }}>", fields.join("; "))
        }
        ContractInterfaceAtomType::optional(inner) => {
            imports.insert("NoneCV");
            imports.insert("SomeCV");
            let inner_type = atom_type_to_ts(inner, imports);
            format!("NoneCV | SomeCV<{inner_type}>")
        }
        ContractInterfaceAtomType::response { ok, error } => {
            imports.insert("ResponseOkCV");
            imports.insert("ResponseErrorCV");
            let ok_type = atom_type_to_ts(ok, imports);
            let err_type = atom_type_to_ts(error, imports);
            format!("ResponseOkCV<{ok_type}> | ResponseErrorCV<{err_type}>")
        }
        ContractInterfaceAtomType::list { type_f, .. } => {
            imports.insert("ListCV");
            let element_type = atom_type_to_ts(type_f, imports);
            format!("ListCV<{element_type}>")
        }
        ContractInterfaceAtomType::trait_reference => {
            imports.insert("ClarityValue");
            "ClarityValue".to_string()
        }
    }
}

/// Generate the index.ts content that re-exports all contract type files.
pub fn generate_index(contract_names: &[&str]) -> String {
    let mut output = String::new();
    writeln!(output, "// This file is auto-generated by clarinet typegen\n").unwrap();
    for name in contract_names {
        writeln!(output, "export * from \"./{name}\";").unwrap();
    }
    output
}

#[cfg(test)]
mod tests {
    use clarity::types::StacksEpochId;
    use clarity::vm::analysis::contract_interface_builder::*;
    use clarity::vm::ClarityVersion;

    use super::*;

    fn make_interface(
        functions: Vec<ContractInterfaceFunction>,
        variables: Vec<ContractInterfaceVariable>,
        maps: Vec<ContractInterfaceMap>,
        fungible_tokens: Vec<ContractInterfaceFungibleTokens>,
        non_fungible_tokens: Vec<ContractInterfaceNonFungibleTokens>,
    ) -> ContractInterface {
        ContractInterface {
            functions,
            variables,
            maps,
            fungible_tokens,
            non_fungible_tokens,
            epoch: StacksEpochId::Epoch25,
            clarity_version: ClarityVersion::Clarity2,
        }
    }

    #[test]
    fn test_atom_type_primitives() {
        let mut imports = BTreeSet::new();
        assert_eq!(
            atom_type_to_ts(&ContractInterfaceAtomType::uint128, &mut imports),
            "UIntCV"
        );
        assert_eq!(
            atom_type_to_ts(&ContractInterfaceAtomType::int128, &mut imports),
            "IntCV"
        );
        assert_eq!(
            atom_type_to_ts(&ContractInterfaceAtomType::bool, &mut imports),
            "BooleanCV"
        );
        assert_eq!(
            atom_type_to_ts(&ContractInterfaceAtomType::principal, &mut imports),
            "PrincipalCV"
        );
        assert_eq!(
            atom_type_to_ts(&ContractInterfaceAtomType::none, &mut imports),
            "NoneCV"
        );
        assert!(imports.contains("UIntCV"));
        assert!(imports.contains("IntCV"));
        assert!(imports.contains("BooleanCV"));
        assert!(imports.contains("PrincipalCV"));
        assert!(imports.contains("NoneCV"));
    }

    #[test]
    fn test_atom_type_buffer_and_strings() {
        let mut imports = BTreeSet::new();
        assert_eq!(
            atom_type_to_ts(&ContractInterfaceAtomType::buffer { length: 32 }, &mut imports),
            "BufferCV"
        );
        assert_eq!(
            atom_type_to_ts(
                &ContractInterfaceAtomType::string_ascii { length: 100 },
                &mut imports
            ),
            "StringAsciiCV"
        );
        assert_eq!(
            atom_type_to_ts(
                &ContractInterfaceAtomType::string_utf8 { length: 100 },
                &mut imports
            ),
            "StringUtf8CV"
        );
    }

    #[test]
    fn test_atom_type_optional() {
        let mut imports = BTreeSet::new();
        let ty =
            ContractInterfaceAtomType::optional(Box::new(ContractInterfaceAtomType::uint128));
        assert_eq!(
            atom_type_to_ts(&ty, &mut imports),
            "NoneCV | SomeCV<UIntCV>"
        );
        assert!(imports.contains("NoneCV"));
        assert!(imports.contains("SomeCV"));
        assert!(imports.contains("UIntCV"));
    }

    #[test]
    fn test_atom_type_response() {
        let mut imports = BTreeSet::new();
        let ty = ContractInterfaceAtomType::response {
            ok: Box::new(ContractInterfaceAtomType::bool),
            error: Box::new(ContractInterfaceAtomType::uint128),
        };
        assert_eq!(
            atom_type_to_ts(&ty, &mut imports),
            "ResponseOkCV<BooleanCV> | ResponseErrorCV<UIntCV>"
        );
    }

    #[test]
    fn test_atom_type_list() {
        let mut imports = BTreeSet::new();
        let ty = ContractInterfaceAtomType::list {
            type_f: Box::new(ContractInterfaceAtomType::principal),
            length: 10,
        };
        assert_eq!(
            atom_type_to_ts(&ty, &mut imports),
            "ListCV<PrincipalCV>"
        );
    }

    #[test]
    fn test_atom_type_tuple() {
        let mut imports = BTreeSet::new();
        let ty = ContractInterfaceAtomType::tuple(vec![
            ContractInterfaceTupleEntryType {
                name: "token-id".to_string(),
                type_f: ContractInterfaceAtomType::uint128,
            },
            ContractInterfaceTupleEntryType {
                name: "owner".to_string(),
                type_f: ContractInterfaceAtomType::principal,
            },
        ]);
        assert_eq!(
            atom_type_to_ts(&ty, &mut imports),
            "TupleCV<{ tokenId: UIntCV; owner: PrincipalCV }>"
        );
    }

    #[test]
    fn test_atom_type_nested() {
        let mut imports = BTreeSet::new();
        // list of optional uint
        let ty = ContractInterfaceAtomType::list {
            type_f: Box::new(ContractInterfaceAtomType::optional(Box::new(
                ContractInterfaceAtomType::uint128,
            ))),
            length: 5,
        };
        assert_eq!(
            atom_type_to_ts(&ty, &mut imports),
            "ListCV<NoneCV | SomeCV<UIntCV>>"
        );
    }

    #[test]
    fn test_atom_type_trait_reference() {
        let mut imports = BTreeSet::new();
        assert_eq!(
            atom_type_to_ts(&ContractInterfaceAtomType::trait_reference, &mut imports),
            "ClarityValue"
        );
        assert!(imports.contains("ClarityValue"));
    }

    #[test]
    fn test_generate_contract_types_basic() {
        let interface = make_interface(
            vec![
                ContractInterfaceFunction {
                    name: "increment".to_string(),
                    access: ContractInterfaceFunctionAccess::public,
                    args: vec![ContractInterfaceFunctionArg {
                        name: "step".to_string(),
                        type_f: ContractInterfaceAtomType::uint128,
                    }],
                    outputs: ContractInterfaceFunctionOutput {
                        type_f: ContractInterfaceAtomType::response {
                            ok: Box::new(ContractInterfaceAtomType::bool),
                            error: Box::new(ContractInterfaceAtomType::none),
                        },
                    },
                },
                ContractInterfaceFunction {
                    name: "get-counter".to_string(),
                    access: ContractInterfaceFunctionAccess::read_only,
                    args: vec![],
                    outputs: ContractInterfaceFunctionOutput {
                        type_f: ContractInterfaceAtomType::uint128,
                    },
                },
                // Private functions should be excluded
                ContractInterfaceFunction {
                    name: "helper".to_string(),
                    access: ContractInterfaceFunctionAccess::private,
                    args: vec![],
                    outputs: ContractInterfaceFunctionOutput {
                        type_f: ContractInterfaceAtomType::bool,
                    },
                },
            ],
            vec![],
            vec![],
            vec![],
            vec![],
        );

        let output = generate_contract_types("counter", &interface);
        assert!(output.contains("export type IncrementArgs = { step: UIntCV };"));
        assert!(output.contains(
            "export type IncrementReturn = ResponseOkCV<BooleanCV> | ResponseErrorCV<NoneCV>;"
        ));
        assert!(output.contains("export type GetCounterArgs = Record<string, never>;"));
        assert!(output.contains("export type GetCounterReturn = UIntCV;"));
        assert!(output.contains("export interface CounterContract {"));
        // Private function should not appear
        assert!(!output.contains("helper"));
    }

    #[test]
    fn test_generate_contract_types_with_maps_and_vars() {
        let interface = make_interface(
            vec![],
            vec![
                ContractInterfaceVariable {
                    name: "counter".to_string(),
                    type_f: ContractInterfaceAtomType::uint128,
                    access: ContractInterfaceVariableAccess::variable,
                },
                ContractInterfaceVariable {
                    name: "err-unauthorized".to_string(),
                    type_f: ContractInterfaceAtomType::response {
                        ok: Box::new(ContractInterfaceAtomType::none),
                        error: Box::new(ContractInterfaceAtomType::uint128),
                    },
                    access: ContractInterfaceVariableAccess::constant,
                },
            ],
            vec![ContractInterfaceMap {
                name: "balances".to_string(),
                key: ContractInterfaceAtomType::principal,
                value: ContractInterfaceAtomType::uint128,
            }],
            vec![],
            vec![],
        );

        let output = generate_contract_types("my-contract", &interface);
        assert!(output.contains("export type BalancesMapKey = PrincipalCV;"));
        assert!(output.contains("export type BalancesMapValue = UIntCV;"));
        assert!(output.contains("export type CounterVariable = UIntCV;"));
        assert!(output.contains(
            "export type ErrUnauthorizedConstant = ResponseOkCV<NoneCV> | ResponseErrorCV<UIntCV>;"
        ));
        assert!(output.contains("export interface MyContractContract {"));
    }

    #[test]
    fn test_generate_index() {
        let output = generate_index(&["counter", "my-token"]);
        assert!(output.contains("export * from \"./counter\";"));
        assert!(output.contains("export * from \"./my-token\";"));
    }

    #[test]
    fn test_import_block_only_used_types() {
        let interface = make_interface(
            vec![ContractInterfaceFunction {
                name: "get-value".to_string(),
                access: ContractInterfaceFunctionAccess::read_only,
                args: vec![],
                outputs: ContractInterfaceFunctionOutput {
                    type_f: ContractInterfaceAtomType::uint128,
                },
            }],
            vec![],
            vec![],
            vec![],
            vec![],
        );

        let output = generate_contract_types("simple", &interface);
        // Should only import UIntCV
        assert!(output.contains("  UIntCV,"));
        // Should not import unused types
        assert!(!output.contains("BooleanCV"));
        assert!(!output.contains("PrincipalCV"));
    }
}
