use clarity::vm::functions::NativeFunctions;
use std::sync::OnceLock;

pub static STD_PKG_NAME: &str = "@hirosystems/clarity-std";

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

static STD_FUNCTIONS: OnceLock<Vec<String>> = OnceLock::new();

pub fn get_std_functions() -> &'static Vec<String> {
    STD_FUNCTIONS.get_or_init(|| get_std())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_std() {
        let functions = get_std_functions();
        // println!("std: {:#?}", functions);
        assert_eq!(functions.len(), 66);
    }
}
