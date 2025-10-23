#![allow(unused_variables)]
#![allow(clippy::get_first)]

use std::collections::HashMap;
use std::sync::LazyLock;

use clarity::vm::functions::define::DefineFunctions;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::SymbolicExpressionType::*;
use clarity::vm::representations::{Span, TraitDefinition};
use clarity::vm::{ClarityName, ClarityVersion, SymbolicExpression, SymbolicExpressionType};
use clarity_types::types::{PrincipalData, QualifiedContractIdentifier, TraitIdentifier, Value};

#[derive(Clone)]
pub struct TypedVar<'a> {
    pub name: &'a ClarityName,
    pub type_expr: &'a SymbolicExpression,
    pub decl_span: Span,
}

// Since the AST Visitor may be used before other checks have been performed,
// we may need a default value for some expressions. This can be used for a
// missing `ClarityName`.
pub static DEFAULT_NAME: LazyLock<ClarityName> =
    LazyLock::new(|| ClarityName::from("placeholder__"));
pub static DEFAULT_EXPR: LazyLock<SymbolicExpression> =
    LazyLock::new(|| SymbolicExpression::atom(DEFAULT_NAME.clone()));

pub trait ASTVisitor<'a> {
    fn traverse_expr(&mut self, expr: &'a SymbolicExpression) -> bool {
        match &expr.expr {
            AtomValue(value) => self.visit_atom_value(expr, value),
            Atom(name) => self.visit_atom(expr, name),
            List(exprs) => self.traverse_list(expr, exprs),
            LiteralValue(value) => self.visit_literal_value(expr, value),
            Field(field) => self.visit_field(expr, field),
            TraitReference(name, trait_def) => self.visit_trait_reference(expr, name, trait_def),
        }
    }

    // AST level traverse/visit methods

    fn traverse_list(
        &mut self,
        expr: &'a SymbolicExpression,
        list: &'a [SymbolicExpression],
    ) -> bool {
        let mut rv = true;
        if let Some((function_name, args)) = list.split_first() {
            if let Some(function_name) = function_name.match_atom() {
                if let Some(define_function) = DefineFunctions::lookup_by_name(function_name) {
                    rv = match define_function {
                        DefineFunctions::Constant => self.traverse_define_constant(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        DefineFunctions::PrivateFunction
                        | DefineFunctions::ReadOnlyFunction
                        | DefineFunctions::PublicFunction => {
                            match args.get(0).unwrap_or(&DEFAULT_EXPR).match_list() {
                                Some(signature) => {
                                    let name = signature
                                        .get(0)
                                        .and_then(|n| n.match_atom())
                                        .unwrap_or(&DEFAULT_NAME);
                                    let params = match signature.len() {
                                        0 | 1 => None,
                                        _ => match_pairs_list(&signature[1..]),
                                    };
                                    let body = args.get(1).unwrap_or(&DEFAULT_EXPR);

                                    match define_function {
                                        DefineFunctions::PrivateFunction => {
                                            self.traverse_define_private(expr, name, params, body)
                                        }
                                        DefineFunctions::ReadOnlyFunction => {
                                            self.traverse_define_read_only(expr, name, params, body)
                                        }
                                        DefineFunctions::PublicFunction => {
                                            self.traverse_define_public(expr, name, params, body)
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                                _ => false,
                            }
                        }
                        DefineFunctions::NonFungibleToken => self.traverse_define_nft(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        DefineFunctions::FungibleToken => self.traverse_define_ft(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1),
                        ),
                        DefineFunctions::Map => self.traverse_define_map(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                        ),
                        DefineFunctions::PersistedVariable => self.traverse_define_data_var(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                        ),
                        DefineFunctions::Trait => {
                            let params = if !args.is_empty() { &args[1..] } else { &[] };
                            self.traverse_define_trait(
                                expr,
                                args.get(0)
                                    .unwrap_or(&DEFAULT_EXPR)
                                    .match_atom()
                                    .unwrap_or(&DEFAULT_NAME),
                                params,
                            )
                        }
                        DefineFunctions::UseTrait => self.traverse_use_trait(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_field()
                                .unwrap_or(&TraitIdentifier {
                                    contract_identifier: QualifiedContractIdentifier::transient(),
                                    name: DEFAULT_NAME.clone(),
                                }),
                        ),
                        DefineFunctions::ImplTrait => self.traverse_impl_trait(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_field()
                                .unwrap_or(&TraitIdentifier {
                                    contract_identifier: QualifiedContractIdentifier::transient(),
                                    name: DEFAULT_NAME.clone(),
                                }),
                        ),
                    };
                } else if let Some(native_function) = NativeFunctions::lookup_by_name_at_version(
                    function_name,
                    &ClarityVersion::latest(), // FIXME(brice): this should probably be passed in
                ) {
                    use clarity::vm::functions::NativeFunctions::*;
                    rv = match native_function {
                        Add | Subtract | Multiply | Divide | Modulo | Power | Sqrti | Log2 => {
                            self.traverse_arithmetic(expr, native_function, args)
                        }
                        BitwiseXor => self.traverse_binary_bitwise(
                            expr,
                            native_function,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        CmpLess | CmpLeq | CmpGreater | CmpGeq | Equals => {
                            self.traverse_comparison(expr, native_function, args)
                        }
                        And | Or => self.traverse_lazy_logical(expr, native_function, args),
                        Not => self.traverse_logical(expr, native_function, args),
                        ToInt | ToUInt => {
                            self.traverse_int_cast(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                        If => self.traverse_if(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                        ),
                        Let => {
                            let bindings = match_pairs(args.get(0).unwrap_or(&DEFAULT_EXPR))
                                .unwrap_or_default();
                            let params = if !args.is_empty() { &args[1..] } else { &[] };
                            self.traverse_let(expr, &bindings, params)
                        }
                        ElementAt | ElementAtAlias => self.traverse_element_at(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        IndexOf | IndexOfAlias => self.traverse_index_of(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        Map => {
                            let name = args
                                .get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME);
                            let params = if !args.is_empty() { &args[1..] } else { &[] };
                            self.traverse_map(expr, name, params)
                        }
                        Fold => {
                            let name = args
                                .get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME);
                            self.traverse_fold(
                                expr,
                                name,
                                args.get(1).unwrap_or(&DEFAULT_EXPR),
                                args.get(2).unwrap_or(&DEFAULT_EXPR),
                            )
                        }
                        Append => self.traverse_append(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        Concat => self.traverse_concat(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        AsMaxLen => {
                            match args.get(1).unwrap_or(&DEFAULT_EXPR).match_literal_value() {
                                Some(Value::UInt(length)) => self.traverse_as_max_len(
                                    expr,
                                    args.get(0).unwrap_or(&DEFAULT_EXPR),
                                    *length,
                                ),
                                _ => false,
                            }
                        }
                        Len => self.traverse_len(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        ListCons => self.traverse_list_cons(expr, args),
                        FetchVar => self.traverse_var_get(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                        ),
                        SetVar => self.traverse_var_set(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        FetchEntry => {
                            let name = args
                                .get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME);
                            let key = match_tuple(args.get(1).unwrap_or(&DEFAULT_EXPR))
                                .unwrap_or_else(|| {
                                    let mut tuple_map = HashMap::new();
                                    tuple_map.insert(None, args.get(1).unwrap_or(&DEFAULT_EXPR));
                                    tuple_map
                                });
                            self.traverse_map_get(expr, name, &key)
                        }
                        SetEntry => {
                            let name = args
                                .get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME);
                            let key = match_tuple(args.get(1).unwrap_or(&DEFAULT_EXPR))
                                .unwrap_or_else(|| {
                                    let mut tuple_map = HashMap::new();
                                    tuple_map.insert(None, args.get(1).unwrap_or(&DEFAULT_EXPR));
                                    tuple_map
                                });
                            let value = match_tuple(args.get(2).unwrap_or(&DEFAULT_EXPR))
                                .unwrap_or_else(|| {
                                    let mut tuple_map = HashMap::new();
                                    tuple_map.insert(None, args.get(2).unwrap_or(&DEFAULT_EXPR));
                                    tuple_map
                                });
                            self.traverse_map_set(expr, name, &key, &value)
                        }
                        InsertEntry => {
                            let name = args
                                .get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME);
                            let key = match_tuple(args.get(1).unwrap_or(&DEFAULT_EXPR))
                                .unwrap_or_else(|| {
                                    let mut tuple_map = HashMap::new();
                                    tuple_map.insert(None, args.get(1).unwrap_or(&DEFAULT_EXPR));
                                    tuple_map
                                });
                            let value = match_tuple(args.get(2).unwrap_or(&DEFAULT_EXPR))
                                .unwrap_or_else(|| {
                                    let mut tuple_map = HashMap::new();
                                    tuple_map.insert(None, args.get(2).unwrap_or(&DEFAULT_EXPR));
                                    tuple_map
                                });
                            self.traverse_map_insert(expr, name, &key, &value)
                        }
                        DeleteEntry => {
                            let name = args
                                .get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME);
                            let key = match_tuple(args.get(1).unwrap_or(&DEFAULT_EXPR))
                                .unwrap_or_else(|| {
                                    let mut tuple_map = HashMap::new();
                                    tuple_map.insert(None, args.get(1).unwrap_or(&DEFAULT_EXPR));
                                    tuple_map
                                });
                            self.traverse_map_delete(expr, name, &key)
                        }
                        TupleCons => {
                            self.traverse_tuple(expr, &match_tuple(expr).unwrap_or_default())
                        }
                        TupleGet => self.traverse_get(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        TupleMerge => self.traverse_merge(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        Begin => self.traverse_begin(expr, args),
                        Hash160 | Sha256 | Sha512 | Sha512Trunc256 | Keccak256 => self
                            .traverse_hash(
                                expr,
                                native_function,
                                args.get(0).unwrap_or(&DEFAULT_EXPR),
                            ),
                        Secp256k1Recover => self.traverse_secp256k1_recover(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        Secp256k1Verify => self.traverse_secp256k1_verify(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                        ),
                        Print => self.traverse_print(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        ContractCall => {
                            let function_name = args
                                .get(1)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME);
                            let params = if args.len() >= 2 { &args[2..] } else { &[] };
                            if let SymbolicExpressionType::LiteralValue(Value::Principal(
                                PrincipalData::Contract(ref contract_identifier),
                            )) = args.get(0).unwrap_or(&DEFAULT_EXPR).expr
                            {
                                self.traverse_static_contract_call(
                                    expr,
                                    contract_identifier,
                                    function_name,
                                    params,
                                )
                            } else {
                                self.traverse_dynamic_contract_call(
                                    expr,
                                    args.get(0).unwrap_or(&DEFAULT_EXPR),
                                    function_name,
                                    params,
                                )
                            }
                        }
                        AsContract => {
                            self.traverse_as_contract(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                        ContractOf => {
                            self.traverse_contract_of(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                        PrincipalOf => {
                            self.traverse_principal_of(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                        AtBlock => self.traverse_at_block(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        GetBlockInfo => self.traverse_get_block_info(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        GetStacksBlockInfo => self.traverse_get_stacks_block_info(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        GetTenureInfo => self.traverse_get_tenure_info(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),

                        ConsError => self.traverse_err(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        ConsOkay => self.traverse_ok(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        ConsSome => self.traverse_some(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        DefaultTo => self.traverse_default_to(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        Asserts => self.traverse_asserts(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        UnwrapRet => self.traverse_unwrap(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        Unwrap => {
                            self.traverse_unwrap_panic(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                        IsOkay => self.traverse_is_ok(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        IsNone => self.traverse_is_none(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        IsErr => self.traverse_is_err(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        IsSome => self.traverse_is_some(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        Filter => self.traverse_filter(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        UnwrapErrRet => self.traverse_unwrap_err(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        UnwrapErr => self.traverse_unwrap_err(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        Match => {
                            if args.len() == 4 {
                                self.traverse_match_option(
                                    expr,
                                    args.get(0).unwrap_or(&DEFAULT_EXPR),
                                    args.get(1)
                                        .unwrap_or(&DEFAULT_EXPR)
                                        .match_atom()
                                        .unwrap_or(&DEFAULT_NAME),
                                    args.get(2).unwrap_or(&DEFAULT_EXPR),
                                    args.get(3).unwrap_or(&DEFAULT_EXPR),
                                )
                            } else {
                                self.traverse_match_response(
                                    expr,
                                    args.get(0).unwrap_or(&DEFAULT_EXPR),
                                    args.get(1)
                                        .unwrap_or(&DEFAULT_EXPR)
                                        .match_atom()
                                        .unwrap_or(&DEFAULT_NAME),
                                    args.get(2).unwrap_or(&DEFAULT_EXPR),
                                    args.get(3)
                                        .unwrap_or(&DEFAULT_EXPR)
                                        .match_atom()
                                        .unwrap_or(&DEFAULT_NAME),
                                    args.get(4).unwrap_or(&DEFAULT_EXPR),
                                )
                            }
                        }
                        TryRet => self.traverse_try(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        StxBurn => self.traverse_stx_burn(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        StxTransfer | StxTransferMemo => self.traverse_stx_transfer(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                            args.get(3),
                        ),
                        GetStxBalance => self
                            .traverse_stx_get_balance(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        BurnToken => self.traverse_ft_burn(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                        ),
                        TransferToken => self.traverse_ft_transfer(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                            args.get(3).unwrap_or(&DEFAULT_EXPR),
                        ),
                        GetTokenBalance => self.traverse_ft_get_balance(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        GetTokenSupply => self.traverse_ft_get_supply(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                        ),
                        MintToken => self.traverse_ft_mint(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                        ),
                        BurnAsset => self.traverse_nft_burn(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                        ),
                        TransferAsset => self.traverse_nft_transfer(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                            args.get(3).unwrap_or(&DEFAULT_EXPR),
                        ),
                        MintAsset => self.traverse_nft_mint(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                        ),
                        GetAssetOwner => self.traverse_nft_get_owner(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        BuffToIntLe | BuffToUIntLe | BuffToIntBe | BuffToUIntBe => {
                            self.traverse_buff_cast(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                        IsStandard => {
                            self.traverse_is_standard(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                        PrincipalDestruct => self.traverse_principal_destruct(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                        ),
                        PrincipalConstruct => self.traverse_principal_construct(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2),
                        ),
                        StringToInt | StringToUInt => {
                            self.traverse_string_to_int(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                        IntToAscii | IntToUtf8 => {
                            self.traverse_int_to_string(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                        ToAscii => {
                            self.traverse_to_ascii(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                        GetBurnBlockInfo => self.traverse_get_burn_block_info(
                            expr,
                            args.get(0)
                                .unwrap_or(&DEFAULT_EXPR)
                                .match_atom()
                                .unwrap_or(&DEFAULT_NAME),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        StxGetAccount => self
                            .traverse_stx_get_account(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        Slice => self.traverse_slice(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                        ),
                        ToConsensusBuff => self
                            .traverse_to_consensus_buff(expr, args.get(0).unwrap_or(&DEFAULT_EXPR)),
                        FromConsensusBuff => self.traverse_from_consensus_buff(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        ReplaceAt => self.traverse_replace_at(
                            expr,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                            args.get(2).unwrap_or(&DEFAULT_EXPR),
                        ),
                        BitwiseAnd | BitwiseOr | BitwiseNot | BitwiseXor2 => {
                            self.traverse_bitwise(expr, native_function, args)
                        }
                        BitwiseLShift | BitwiseRShift => self.traverse_bit_shift(
                            expr,
                            native_function,
                            args.get(0).unwrap_or(&DEFAULT_EXPR),
                            args.get(1).unwrap_or(&DEFAULT_EXPR),
                        ),
                        ContractHash => {
                            self.traverse_contract_hash(expr, args.get(0).unwrap_or(&DEFAULT_EXPR))
                        }
                    };
                } else {
                    rv = self.traverse_call_user_defined(expr, function_name, args);
                }
            }
        }

        rv && self.visit_list(expr, list)
    }

    fn visit_list(&mut self, expr: &'a SymbolicExpression, list: &'a [SymbolicExpression]) -> bool {
        true
    }

    fn visit_atom_value(&mut self, expr: &'a SymbolicExpression, value: &Value) -> bool {
        true
    }

    fn visit_atom(&mut self, expr: &'a SymbolicExpression, atom: &'a ClarityName) -> bool {
        true
    }

    fn visit_literal_value(&mut self, expr: &'a SymbolicExpression, value: &Value) -> bool {
        true
    }

    fn visit_field(&mut self, expr: &'a SymbolicExpression, field: &TraitIdentifier) -> bool {
        true
    }

    fn visit_trait_reference(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        trait_def: &TraitDefinition,
    ) -> bool {
        true
    }

    // Higher level traverse/visit methods

    fn traverse_define_constant(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_define_constant(expr, name, value)
    }

    fn visit_define_constant(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(body) && self.visit_define_private(expr, name, parameters, body)
    }

    fn visit_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(body) && self.visit_define_read_only(expr, name, parameters, body)
    }

    fn visit_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(body) && self.visit_define_public(expr, name, parameters, body)
    }

    fn visit_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_nft(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        nft_type: &'a SymbolicExpression,
    ) -> bool {
        self.visit_define_nft(expr, name, nft_type)
    }

    fn visit_define_nft(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        nft_type: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_ft(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        supply: Option<&'a SymbolicExpression>,
    ) -> bool {
        if let Some(supply_expr) = supply {
            if !self.traverse_expr(supply_expr) {
                return false;
            }
        }

        self.visit_define_ft(expr, name, supply)
    }

    fn visit_define_ft(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        supply: Option<&'a SymbolicExpression>,
    ) -> bool {
        true
    }

    fn traverse_define_map(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key_type: &'a SymbolicExpression,
        value_type: &'a SymbolicExpression,
    ) -> bool {
        self.visit_define_map(expr, name, key_type, value_type)
    }

    fn visit_define_map(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key_type: &'a SymbolicExpression,
        value_type: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_data_var(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        data_type: &'a SymbolicExpression,
        initial: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(initial) && self.visit_define_data_var(expr, name, data_type, initial)
    }

    fn visit_define_data_var(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        data_type: &'a SymbolicExpression,
        initial: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        functions: &'a [SymbolicExpression],
    ) -> bool {
        self.visit_define_trait(expr, name, functions)
    }

    fn visit_define_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        functions: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_use_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        trait_identifier: &TraitIdentifier,
    ) -> bool {
        self.visit_use_trait(expr, name, trait_identifier)
    }

    fn visit_use_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        trait_identifier: &TraitIdentifier,
    ) -> bool {
        true
    }

    fn traverse_impl_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        trait_identifier: &TraitIdentifier,
    ) -> bool {
        self.visit_impl_trait(expr, trait_identifier)
    }

    fn visit_impl_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        trait_identifier: &TraitIdentifier,
    ) -> bool {
        true
    }

    fn traverse_arithmetic(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        for operand in operands {
            if !self.traverse_expr(operand) {
                return false;
            }
        }
        self.visit_arithmetic(expr, func, operands)
    }

    fn visit_arithmetic(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_binary_bitwise(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        lhs: &'a SymbolicExpression,
        rhs: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(lhs)
            && self.traverse_expr(rhs)
            && self.visit_binary_bitwise(expr, func, lhs, rhs)
    }

    fn visit_binary_bitwise(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        lhs: &'a SymbolicExpression,
        rhs: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_comparison(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        for operand in operands {
            if !self.traverse_expr(operand) {
                return false;
            }
        }
        self.visit_comparison(expr, func, operands)
    }

    fn visit_comparison(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_lazy_logical(
        &mut self,
        expr: &'a SymbolicExpression,
        function: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        for operand in operands {
            if !self.traverse_expr(operand) {
                return false;
            }
        }
        self.visit_lazy_logical(expr, function, operands)
    }

    fn visit_lazy_logical(
        &mut self,
        expr: &'a SymbolicExpression,
        function: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_logical(
        &mut self,
        expr: &'a SymbolicExpression,
        function: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        for operand in operands {
            if !self.traverse_expr(operand) {
                return false;
            }
        }
        self.visit_logical(expr, function, operands)
    }

    fn visit_logical(
        &mut self,
        expr: &'a SymbolicExpression,
        function: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_int_cast(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input) && self.visit_int_cast(expr, input)
    }

    fn visit_int_cast(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_if(
        &mut self,
        expr: &'a SymbolicExpression,
        cond: &'a SymbolicExpression,
        then_expr: &'a SymbolicExpression,
        else_expr: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(cond)
            && self.traverse_expr(then_expr)
            && self.traverse_expr(else_expr)
            && self.visit_if(expr, cond, then_expr, else_expr)
    }

    fn visit_if(
        &mut self,
        expr: &'a SymbolicExpression,
        cond: &'a SymbolicExpression,
        then_expr: &'a SymbolicExpression,
        else_expr: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_var_get(&mut self, expr: &'a SymbolicExpression, name: &'a ClarityName) -> bool {
        self.visit_var_get(expr, name)
    }

    fn visit_var_get(&mut self, expr: &'a SymbolicExpression, name: &'a ClarityName) -> bool {
        true
    }

    fn traverse_var_set(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_var_set(expr, name, value)
    }

    fn visit_var_set(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_map_get(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        for val in key.values() {
            if !self.traverse_expr(val) {
                return false;
            }
        }
        self.visit_map_get(expr, name, key)
    }

    fn visit_map_get(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        true
    }

    fn traverse_map_set(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        for key_val in key.values() {
            if !self.traverse_expr(key_val) {
                return false;
            }
        }
        for val_val in value.values() {
            if !self.traverse_expr(val_val) {
                return false;
            }
        }
        self.visit_map_set(expr, name, key, value)
    }

    fn visit_map_set(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        true
    }

    fn traverse_map_insert(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        for key_val in key.values() {
            if !self.traverse_expr(key_val) {
                return false;
            }
        }
        for val_val in value.values() {
            if !self.traverse_expr(val_val) {
                return false;
            }
        }
        self.visit_map_insert(expr, name, key, value)
    }

    fn visit_map_insert(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        true
    }

    fn traverse_map_delete(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        for val in key.values() {
            if !self.traverse_expr(val) {
                return false;
            }
        }
        self.visit_map_delete(expr, name, key)
    }

    fn visit_map_delete(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        true
    }

    fn traverse_tuple(
        &mut self,
        expr: &'a SymbolicExpression,
        values: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        for val in values.values() {
            if !self.traverse_expr(val) {
                return false;
            }
        }
        self.visit_tuple(expr, values)
    }

    fn visit_tuple(
        &mut self,
        expr: &'a SymbolicExpression,
        values: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        true
    }

    fn traverse_get(
        &mut self,
        expr: &'a SymbolicExpression,
        key: &'a ClarityName,
        tuple: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(tuple) && self.visit_get(expr, key, tuple)
    }

    fn visit_get(
        &mut self,
        expr: &'a SymbolicExpression,
        key: &'a ClarityName,
        tuple: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_merge(
        &mut self,
        expr: &'a SymbolicExpression,
        tuple1: &'a SymbolicExpression,
        tuple2: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(tuple1)
            && self.traverse_expr(tuple2)
            && self.visit_merge(expr, tuple1, tuple2)
    }

    fn visit_merge(
        &mut self,
        expr: &'a SymbolicExpression,
        tuple1: &'a SymbolicExpression,
        tuple2: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_begin(
        &mut self,
        expr: &'a SymbolicExpression,
        statements: &'a [SymbolicExpression],
    ) -> bool {
        for stmt in statements {
            if !self.traverse_expr(stmt) {
                return false;
            }
        }
        self.visit_begin(expr, statements)
    }

    fn visit_begin(
        &mut self,
        expr: &'a SymbolicExpression,
        statements: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_hash(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_hash(expr, func, value)
    }

    fn visit_hash(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_secp256k1_recover(
        &mut self,
        expr: &'a SymbolicExpression,
        hash: &'a SymbolicExpression,
        signature: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(hash)
            && self.traverse_expr(signature)
            && self.visit_secp256k1_recover(expr, hash, signature)
    }

    fn visit_secp256k1_recover(
        &mut self,
        expr: &'a SymbolicExpression,
        hash: &SymbolicExpression,
        signature: &SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_secp256k1_verify(
        &mut self,
        expr: &'a SymbolicExpression,
        hash: &'a SymbolicExpression,
        signature: &'a SymbolicExpression,
        public_key: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(hash)
            && self.traverse_expr(signature)
            && self.visit_secp256k1_verify(expr, hash, signature, public_key)
    }

    fn visit_secp256k1_verify(
        &mut self,
        expr: &'a SymbolicExpression,
        hash: &SymbolicExpression,
        signature: &SymbolicExpression,
        public_key: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_print(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_print(expr, value)
    }

    fn visit_print(&mut self, expr: &'a SymbolicExpression, value: &'a SymbolicExpression) -> bool {
        true
    }

    fn traverse_static_contract_call(
        &mut self,
        expr: &'a SymbolicExpression,
        contract_identifier: &'a QualifiedContractIdentifier,
        function_name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        for arg in args.iter() {
            if !self.traverse_expr(arg) {
                return false;
            }
        }
        self.visit_static_contract_call(expr, contract_identifier, function_name, args)
    }

    fn visit_static_contract_call(
        &mut self,
        expr: &'a SymbolicExpression,
        contract_identifier: &'a QualifiedContractIdentifier,
        function_name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_dynamic_contract_call(
        &mut self,
        expr: &'a SymbolicExpression,
        trait_ref: &'a SymbolicExpression,
        function_name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        self.traverse_expr(trait_ref);
        for arg in args.iter() {
            if !self.traverse_expr(arg) {
                return false;
            }
        }
        self.visit_dynamic_contract_call(expr, trait_ref, function_name, args)
    }

    fn visit_dynamic_contract_call(
        &mut self,
        expr: &'a SymbolicExpression,
        trait_ref: &'a SymbolicExpression,
        function_name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_as_contract(
        &mut self,
        expr: &'a SymbolicExpression,
        inner: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(inner) && self.visit_as_contract(expr, inner)
    }

    fn visit_as_contract(
        &mut self,
        expr: &'a SymbolicExpression,
        inner: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_contract_of(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(name) && self.visit_contract_of(expr, name)
    }

    fn visit_contract_of(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_principal_of(
        &mut self,
        expr: &'a SymbolicExpression,
        public_key: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(public_key) && self.visit_principal_of(expr, public_key)
    }

    fn visit_principal_of(
        &mut self,
        expr: &'a SymbolicExpression,
        public_key: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_at_block(
        &mut self,
        expr: &'a SymbolicExpression,
        block: &'a SymbolicExpression,
        inner: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(block)
            && self.traverse_expr(inner)
            && self.visit_at_block(expr, block, inner)
    }

    fn visit_at_block(
        &mut self,
        expr: &'a SymbolicExpression,
        block: &'a SymbolicExpression,
        inner: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_get_block_info(
        &mut self,
        expr: &'a SymbolicExpression,
        prop_name: &'a ClarityName,
        block: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(block) && self.visit_get_block_info(expr, prop_name, block)
    }

    fn visit_get_block_info(
        &mut self,
        expr: &'a SymbolicExpression,
        prop_name: &'a ClarityName,
        block: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_get_stacks_block_info(
        &mut self,
        expr: &'a SymbolicExpression,
        prop_name: &'a ClarityName,
        block: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(block) && self.visit_get_stacks_block_info(expr, prop_name, block)
    }

    fn visit_get_stacks_block_info(
        &mut self,
        expr: &'a SymbolicExpression,
        prop_name: &'a ClarityName,
        block: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_get_tenure_info(
        &mut self,
        expr: &'a SymbolicExpression,
        prop_name: &'a ClarityName,
        block: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(block) && self.visit_get_tenure_info(expr, prop_name, block)
    }

    fn visit_get_tenure_info(
        &mut self,
        expr: &'a SymbolicExpression,
        prop_name: &'a ClarityName,
        block: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_err(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_err(expr, value)
    }

    fn visit_err(&mut self, expr: &'a SymbolicExpression, value: &'a SymbolicExpression) -> bool {
        true
    }

    fn traverse_ok(&mut self, expr: &'a SymbolicExpression, value: &'a SymbolicExpression) -> bool {
        self.traverse_expr(value) && self.visit_ok(expr, value)
    }

    fn visit_ok(&mut self, expr: &'a SymbolicExpression, value: &'a SymbolicExpression) -> bool {
        true
    }

    fn traverse_some(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_some(expr, value)
    }

    fn visit_some(&mut self, expr: &'a SymbolicExpression, value: &'a SymbolicExpression) -> bool {
        true
    }

    fn traverse_default_to(
        &mut self,
        expr: &'a SymbolicExpression,
        default: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(default)
            && self.traverse_expr(value)
            && self.visit_default_to(expr, default, value)
    }

    fn visit_default_to(
        &mut self,
        expr: &'a SymbolicExpression,
        default: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_unwrap(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
        throws: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input)
            && self.traverse_expr(throws)
            && self.visit_unwrap(expr, input, throws)
    }

    fn visit_unwrap(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
        throws: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_unwrap_err(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
        throws: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input)
            && self.traverse_expr(throws)
            && self.visit_unwrap_err(expr, input, throws)
    }

    fn visit_unwrap_err(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
        throws: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_is_ok(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_is_ok(expr, value)
    }

    fn visit_is_ok(&mut self, expr: &'a SymbolicExpression, value: &'a SymbolicExpression) -> bool {
        true
    }

    fn traverse_is_none(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_is_none(expr, value)
    }

    fn visit_is_none(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_is_err(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_is_err(expr, value)
    }

    fn visit_is_err(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_is_some(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_is_some(expr, value)
    }

    fn visit_is_some(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_filter(
        &mut self,
        expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        sequence: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(sequence) && self.visit_filter(expr, func, sequence)
    }

    fn visit_filter(
        &mut self,
        expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        sequence: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_unwrap_panic(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input) && self.visit_unwrap_panic(expr, input)
    }

    fn visit_unwrap_panic(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_unwrap_err_panic(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input) && self.visit_unwrap_err_panic(expr, input)
    }

    fn visit_unwrap_err_panic(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_match_option(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
        some_name: &'a ClarityName,
        some_branch: &'a SymbolicExpression,
        none_branch: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input)
            && self.traverse_expr(some_branch)
            && self.traverse_expr(none_branch)
            && self.visit_match_option(expr, input, some_name, some_branch, none_branch)
    }

    fn visit_match_option(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
        some_name: &'a ClarityName,
        some_branch: &'a SymbolicExpression,
        none_branch: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_match_response(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
        ok_name: &'a ClarityName,
        ok_branch: &'a SymbolicExpression,
        err_name: &'a ClarityName,
        err_branch: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input)
            && self.traverse_expr(ok_branch)
            && self.traverse_expr(err_branch)
            && self.visit_match_response(expr, input, ok_name, ok_branch, err_name, err_branch)
    }

    fn visit_match_response(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
        ok_name: &'a ClarityName,
        ok_branch: &'a SymbolicExpression,
        err_name: &'a ClarityName,
        err_branch: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_try(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input) && self.visit_try(expr, input)
    }

    fn visit_try(&mut self, expr: &'a SymbolicExpression, input: &'a SymbolicExpression) -> bool {
        true
    }

    fn traverse_asserts(
        &mut self,
        expr: &'a SymbolicExpression,
        cond: &'a SymbolicExpression,
        thrown: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(cond)
            && self.traverse_expr(thrown)
            && self.visit_asserts(expr, cond, thrown)
    }

    fn visit_asserts(
        &mut self,
        expr: &'a SymbolicExpression,
        cond: &'a SymbolicExpression,
        thrown: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_stx_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(amount)
            && self.traverse_expr(sender)
            && self.visit_stx_burn(expr, amount, sender)
    }

    fn visit_stx_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_stx_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
        memo: Option<&'a SymbolicExpression>,
    ) -> bool {
        self.traverse_expr(amount)
            && self.traverse_expr(sender)
            && self.traverse_expr(recipient)
            && (memo.is_none() || self.traverse_expr(memo.unwrap()))
            && self.visit_stx_transfer(expr, amount, sender, recipient, memo)
    }

    fn visit_stx_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
        memo: Option<&'a SymbolicExpression>,
    ) -> bool {
        true
    }

    fn traverse_stx_get_balance(
        &mut self,
        expr: &'a SymbolicExpression,
        owner: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(owner) && self.visit_stx_get_balance(expr, owner)
    }

    fn visit_stx_get_balance(
        &mut self,
        expr: &'a SymbolicExpression,
        owner: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_ft_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(amount)
            && self.traverse_expr(sender)
            && self.visit_ft_burn(expr, token, amount, sender)
    }

    fn visit_ft_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_ft_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(amount)
            && self.traverse_expr(sender)
            && self.traverse_expr(recipient)
            && self.visit_ft_transfer(expr, token, amount, sender, recipient)
    }

    fn visit_ft_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        amount: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_ft_get_balance(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        owner: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(owner) && self.visit_ft_get_balance(expr, token, owner)
    }

    fn visit_ft_get_balance(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        owner: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_ft_get_supply(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
    ) -> bool {
        self.visit_ft_get_supply(expr, token)
    }

    fn visit_ft_get_supply(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
    ) -> bool {
        true
    }

    fn traverse_ft_mint(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        amount: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(amount)
            && self.traverse_expr(recipient)
            && self.visit_ft_mint(expr, token, amount, recipient)
    }

    fn visit_ft_mint(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        amount: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_nft_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(identifier)
            && self.traverse_expr(sender)
            && self.visit_nft_burn(expr, token, identifier, sender)
    }

    fn visit_nft_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_nft_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(identifier)
            && self.traverse_expr(sender)
            && self.traverse_expr(recipient)
            && self.visit_nft_transfer(expr, token, identifier, sender, recipient)
    }

    fn visit_nft_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
        sender: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_nft_mint(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(identifier)
            && self.traverse_expr(recipient)
            && self.visit_nft_mint(expr, token, identifier, recipient)
    }

    fn visit_nft_mint(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
        recipient: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_nft_get_owner(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(identifier) && self.visit_nft_get_owner(expr, token, identifier)
    }

    fn visit_nft_get_owner(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        identifier: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_let(
        &mut self,
        expr: &'a SymbolicExpression,
        bindings: &HashMap<&'a ClarityName, &'a SymbolicExpression>,
        body: &'a [SymbolicExpression],
    ) -> bool {
        for val in bindings.values() {
            if !self.traverse_expr(val) {
                return false;
            }
        }
        for expr in body {
            if !self.traverse_expr(expr) {
                return false;
            }
        }
        self.visit_let(expr, bindings, body)
    }

    fn visit_let(
        &mut self,
        expr: &'a SymbolicExpression,
        bindings: &HashMap<&'a ClarityName, &'a SymbolicExpression>,
        body: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_map(
        &mut self,
        expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        sequences: &'a [SymbolicExpression],
    ) -> bool {
        for sequence in sequences {
            if !self.traverse_expr(sequence) {
                return false;
            }
        }
        self.visit_map(expr, func, sequences)
    }

    fn visit_map(
        &mut self,
        expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        sequences: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_fold(
        &mut self,
        expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        sequence: &'a SymbolicExpression,
        initial: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(sequence)
            && self.traverse_expr(initial)
            && self.visit_fold(expr, func, sequence, initial)
    }

    fn visit_fold(
        &mut self,
        expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        sequence: &'a SymbolicExpression,
        initial: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_append(
        &mut self,
        expr: &'a SymbolicExpression,
        list: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(list)
            && self.traverse_expr(value)
            && self.visit_append(expr, list, value)
    }

    fn visit_append(
        &mut self,
        expr: &'a SymbolicExpression,
        list: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_concat(
        &mut self,
        expr: &'a SymbolicExpression,
        lhs: &'a SymbolicExpression,
        rhs: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(lhs) && self.traverse_expr(rhs) && self.visit_concat(expr, lhs, rhs)
    }

    fn visit_concat(
        &mut self,
        expr: &'a SymbolicExpression,
        lhs: &'a SymbolicExpression,
        rhs: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_as_max_len(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
        length: u128,
    ) -> bool {
        self.traverse_expr(sequence) && self.visit_as_max_len(expr, sequence, length)
    }

    fn visit_as_max_len(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
        length: u128,
    ) -> bool {
        true
    }

    fn traverse_len(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(sequence) && self.visit_len(expr, sequence)
    }

    fn visit_len(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_element_at(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
        index: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(sequence)
            && self.traverse_expr(index)
            && self.visit_element_at(expr, sequence, index)
    }

    fn visit_element_at(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
        index: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_index_of(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
        item: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(sequence)
            && self.traverse_expr(item)
            && self.visit_element_at(expr, sequence, item)
    }

    fn visit_index_of(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
        item: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_list_cons(
        &mut self,
        expr: &'a SymbolicExpression,
        args: &'a [SymbolicExpression],
    ) -> bool {
        for arg in args.iter() {
            if !self.traverse_expr(arg) {
                return false;
            }
        }
        self.visit_list_cons(expr, args)
    }

    fn visit_list_cons(
        &mut self,
        expr: &'a SymbolicExpression,
        args: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_call_user_defined(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        for arg in args.iter() {
            if !self.traverse_expr(arg) {
                return false;
            }
        }
        self.visit_call_user_defined(expr, name, args)
    }

    fn visit_call_user_defined(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        args: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_buff_cast(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input) && self.visit_buff_cast(expr, input)
    }

    fn visit_buff_cast(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_is_standard(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(value) && self.visit_is_standard(expr, value)
    }

    fn visit_is_standard(
        &mut self,
        expr: &'a SymbolicExpression,
        value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_principal_destruct(
        &mut self,
        expr: &'a SymbolicExpression,
        principal: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(principal) && self.visit_principal_destruct(expr, principal)
    }

    fn visit_principal_destruct(
        &mut self,
        expr: &'a SymbolicExpression,
        principal: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_principal_construct(
        &mut self,
        expr: &'a SymbolicExpression,
        buff1: &'a SymbolicExpression,
        buff20: &'a SymbolicExpression,
        contract: Option<&'a SymbolicExpression>,
    ) -> bool {
        self.traverse_expr(buff1)
            && self.traverse_expr(buff20)
            && (contract.is_none() || self.traverse_expr(contract.unwrap()))
            && self.visit_principal_construct(expr, buff1, buff20, contract)
    }

    fn visit_principal_construct(
        &mut self,
        expr: &'a SymbolicExpression,
        buff1: &'a SymbolicExpression,
        buff20: &'a SymbolicExpression,
        contract: Option<&'a SymbolicExpression>,
    ) -> bool {
        true
    }

    fn traverse_string_to_int(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input) && self.visit_string_to_int(expr, input)
    }

    fn visit_string_to_int(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_int_to_string(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input) && self.visit_int_to_string(expr, input)
    }

    fn visit_int_to_string(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_to_ascii(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input) && self.visit_to_ascii(expr, input)
    }

    fn visit_to_ascii(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_stx_get_account(
        &mut self,
        expr: &'a SymbolicExpression,
        owner: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(owner) && self.visit_stx_get_account(expr, owner)
    }

    fn visit_stx_get_account(
        &mut self,
        expr: &'a SymbolicExpression,
        owner: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_slice(
        &mut self,
        expr: &'a SymbolicExpression,
        seq: &'a SymbolicExpression,
        left: &'a SymbolicExpression,
        right: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(seq)
            && self.traverse_expr(left)
            && self.traverse_expr(right)
            && self.visit_slice(expr, seq, left, right)
    }

    fn visit_slice(
        &mut self,
        expr: &'a SymbolicExpression,
        seq: &'a SymbolicExpression,
        left: &'a SymbolicExpression,
        right: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_get_burn_block_info(
        &mut self,
        expr: &'a SymbolicExpression,
        prop_name: &'a ClarityName,
        block: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(block) && self.visit_get_burn_block_info(expr, prop_name, block)
    }

    fn visit_get_burn_block_info(
        &mut self,
        expr: &'a SymbolicExpression,
        prop_name: &'a ClarityName,
        block: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_to_consensus_buff(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input) && self.visit_to_consensus_buff(expr, input)
    }

    fn visit_to_consensus_buff(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_from_consensus_buff(
        &mut self,
        expr: &'a SymbolicExpression,
        type_expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(type_expr)
            && self.traverse_expr(input)
            && self.visit_from_consensus_buff(expr, type_expr, input)
    }

    fn visit_from_consensus_buff(
        &mut self,
        expr: &'a SymbolicExpression,
        type_expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_bitwise(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        for operand in operands {
            if !self.traverse_expr(operand) {
                return false;
            }
        }
        self.visit_bitwise(expr, func, operands)
    }

    fn visit_bitwise(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        operands: &'a [SymbolicExpression],
    ) -> bool {
        true
    }

    fn traverse_replace_at(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
        index: &'a SymbolicExpression,
        element: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(sequence)
            && self.traverse_expr(index)
            && self.traverse_expr(element)
            && self.visit_replace_at(expr, sequence, element, index)
    }

    fn visit_replace_at(
        &mut self,
        expr: &'a SymbolicExpression,
        sequence: &'a SymbolicExpression,
        index: &'a SymbolicExpression,
        element: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_bit_shift(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        input: &'a SymbolicExpression,
        shamt: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input)
            && self.traverse_expr(shamt)
            && self.visit_bit_shift(expr, func, input, shamt)
    }

    fn visit_bit_shift(
        &mut self,
        expr: &'a SymbolicExpression,
        func: NativeFunctions,
        input: &'a SymbolicExpression,
        shamt: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_contract_hash(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_expr(input) && self.visit_contract_hash(expr, input)
    }

    fn visit_contract_hash(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
    ) -> bool {
        true
    }
}

pub fn traverse<'a>(visitor: &mut impl ASTVisitor<'a>, exprs: &'a [SymbolicExpression]) -> bool {
    exprs.iter().all(|expr| visitor.traverse_expr(expr))
}

fn match_tuple(
    expr: &SymbolicExpression,
) -> Option<HashMap<Option<&ClarityName>, &SymbolicExpression>> {
    if let Some(list) = expr.match_list() {
        if let Some((function_name, args)) = list.split_first() {
            if let Some(function_name) = function_name.match_atom() {
                if NativeFunctions::lookup_by_name_at_version(
                    function_name,
                    &clarity::vm::ClarityVersion::latest(),
                ) == Some(NativeFunctions::TupleCons)
                {
                    let mut tuple_map = HashMap::new();
                    for element in args {
                        let pair = element.match_list().unwrap_or_default();
                        if pair.len() != 2 {
                            return None;
                        }
                        tuple_map.insert(pair[0].match_atom(), &pair[1]);
                    }
                    return Some(tuple_map);
                }
            }
        }
    }
    None
}

fn match_pairs(expr: &SymbolicExpression) -> Option<HashMap<&ClarityName, &SymbolicExpression>> {
    let list = expr.match_list()?;
    let mut tuple_map = HashMap::new();
    for pair_list in list {
        let pair = pair_list.match_list()?;
        if pair.len() != 2 {
            return None;
        }
        tuple_map.insert(pair[0].match_atom()?, &pair[1]);
    }
    Some(tuple_map)
}

fn match_pairs_list(list: &[SymbolicExpression]) -> Option<Vec<TypedVar<'_>>> {
    let mut vars = Vec::new();
    for pair_list in list {
        let pair = pair_list.match_list()?;
        if pair.len() != 2 {
            return None;
        }
        vars.push(TypedVar {
            name: pair[0].match_atom()?,
            type_expr: &pair[1],
            decl_span: pair[0].span.clone(),
        });
    }
    Some(vars)
}
