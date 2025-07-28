use clarity::vm::{
    functions::NativeFunctions,
    types::{BurnBlockInfoProperty, StacksBlockInfoProperty, TypeSignature},
};
use std::{collections::HashMap, sync::LazyLock};

pub static STD_PKG_NAME: &str = "clarity";

pub static KEYWORDS_TYPES: LazyLock<HashMap<&str, (&str, TypeSignature)>> = LazyLock::new(|| {
    use clarity::vm::types::TypeSignature::*;
    HashMap::from([
        // chain
        ("chainId", ("chain-id", UIntType)),
        ("isInMainnet", ("is-in-mainnet", BoolType)),
        ("isInRegtest", ("is-in-regtest", BoolType)),
        // heights
        ("burnBlockHeight", ("burn-block-height", UIntType)),
        ("stacksBlockHeight", ("stacks-block-height", UIntType)),
        ("tenureHeight", ("tenure-height", UIntType)),
        // call er context
        ("contractCaller", ("contract-caller", PrincipalType)),
        ("txSender", ("tx-sender", PrincipalType)),
        (
            "txSponsor",
            ("tx-sponsor?", OptionalType(PrincipalType.into())),
        ),
        // stx
        ("stxLiquidSupply", ("stx-liquid-supply", UIntType)),
    ])
});

#[allow(dead_code)]
pub fn get_std() -> Vec<String> {
    use NativeFunctions::*;

    // The ignored functions are not callable as clarity-std functions
    // but instead (will) have their own implementation in the ts-to-clar codebase
    #[rustfmt::skip]
    let ignored_functions = [
        // math operators
        Add, Subtract, Multiply, Divide, Modulo, Power, Sqrti, Log2,
        // conditions
        If,
        // boolean operators
        Or, And, Not, Equals,
        // comparison operators
        CmpGeq, CmpLeq, CmpLess, CmpGreater,
        // bitwise operators
        BitwiseAnd, BitwiseOr, BitwiseXor, BitwiseNot,
        // utils
        Let, Begin,
        // aliases
        ElementAtAlias, IndexOfAlias,
        // types
        ListCons, TupleCons,
        // tuples
        TupleGet, TupleMerge,
        // data-var
        FetchVar, SetVar,
        // data-map
        FetchEntry, SetEntry, InsertEntry, DeleteEntry,
        // ft
        MintToken, BurnToken, GetTokenBalance, GetTokenSupply, TransferToken,
        // nft
        MintAsset, BurnAsset, GetAssetOwner, TransferAsset,
    ];

    NativeFunctions::ALL
        .iter()
        .filter(|f| !ignored_functions.contains(f))
        .map(|f| f.to_string())
        .collect::<Vec<String>>()
}

/*
std: [
    // done
    "to-int",
    "to-uint",
    "print",

    // todo
    "get-stacks-block-info?",
    "get-tenure-info?",
    "get-burn-block-info?",
    "to-consensus-buff?",
    "from-consensus-buff?",

    // to consider
    "buff-to-int-le",
    "buff-to-uint-le",
    "buff-to-int-be",
    "buff-to-uint-be",
    "sha256",
    "sha512",
    "sha512/256",
    "keccak256",
    "secp256k1-recover?",
    "secp256k1-verify",

    // not todo
    "map",
    "fold",
    "append",
    "concat",
    "as-max-len?",
    "len",
    "element-at",
    "index-of",
    "is-standard",
    "principal-destruct?",
    "principal-construct?",
    "string-to-int?",
    "string-to-uint?",
    "int-to-ascii",
    "int-to-utf8",
    "hash160",
    "contract-call?",
    "as-contract",
    "contract-of",
    "principal-of?",
    "at-block",
    "get-block-info?",
    "err",
    "ok",
    "some",
    "default-to",
    "asserts!",
    "unwrap!",
    "unwrap-err!",
    "unwrap-panic",
    "unwrap-err-panic",
    "match",
    "try!",
    "is-ok",
    "is-none",
    "is-err",
    "is-some",
    "filter",
    "stx-get-balance",
    "stx-transfer?",
    "stx-transfer-memo?",
    "stx-burn?",
    "stx-account",
    "bit-shift-left",
    "bit-shift-right",
    "bit-xor",
    "slice?",
    "replace-at?",
] */

#[allow(dead_code)]
pub enum Parameter {
    Any,
    Value(TypeSignature),
    Identifiers(&'static [&'static str]),
}

pub struct StdFunction {
    pub name: &'static str,
    pub parameters: Vec<(&'static str, Parameter)>,
    // return type might not be needed at all in transpiler at some point
    // because the type might just be avaible in the ts ast
    // making it Optional instead of overengineering it for now
    pub _return_type: Option<TypeSignature>,
}

pub static FUNCTIONS: LazyLock<HashMap<&str, StdFunction>> = LazyLock::new(|| {
    use NativeFunctions::*;
    use TypeSignature::*;

    HashMap::from([
        (
            "toInt",
            StdFunction {
                name: ToInt.get_name_str(),
                parameters: vec![("u", Parameter::Value(UIntType))],
                _return_type: Some(IntType),
            },
        ),
        (
            "toUint",
            StdFunction {
                name: ToUInt.get_name_str(),
                parameters: vec![("i", Parameter::Value(IntType))],
                _return_type: Some(UIntType),
            },
        ),
        (
            "print",
            StdFunction {
                name: Print.get_name_str(),
                parameters: vec![("expr", Parameter::Any)],
                _return_type: None,
            },
        ),
        (
            "getStacksBlockInfo",
            StdFunction {
                name: GetStacksBlockInfo.get_name_str(),
                parameters: vec![
                    (
                        "prop-name",
                        Parameter::Identifiers(StacksBlockInfoProperty::ALL_NAMES),
                    ),
                    ("stacks-block-height", Parameter::Value(UIntType)),
                ],
                _return_type: None,
            },
        ),
        (
            "getBurnBlockInfo",
            StdFunction {
                name: GetBurnBlockInfo.get_name_str(),
                parameters: vec![
                    (
                        "prop-name",
                        Parameter::Identifiers(BurnBlockInfoProperty::ALL_NAMES),
                    ),
                    ("burn-block-height", Parameter::Value(UIntType)),
                ],
                _return_type: None,
            },
        ),
        (
            "getTenureInfo",
            StdFunction {
                name: GetTenureInfo.get_name_str(),
                parameters: vec![("tenure-height", Parameter::Value(UIntType))],
                _return_type: None,
            },
        ),
    ])
});

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_functions() {
        assert_eq!(FUNCTIONS.len(), 6);

        // println!("Functions: {:?}", FUNCTIONS.keys().collect::<Vec<_>>());
    }
}
