use clarity::vm::costs::cost_functions::ClarityCostFunction;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::types::{TypeSignature, Value};
use clarity::vm::variables::NativeVariables;
use clarity::vm::version::ClarityVersion;
use clarity_types::execution_cost::ExecutionCost;
use clarity_types::representations::SymbolicExpression;
use stacks_common::types::StacksEpochId;

use super::cost_analysis::{StaticCost, UserArgumentsContext};
use super::cost_functions::{from_native_function, ClarityCostFunctionExt};

// Constants for tuple serialization overhead
const TUPLE_LENGTH_ENCODING_BYTES: u64 = 4;
const TUPLE_FIELD_OVERHEAD_BYTES: u64 = 2;

/// Get the serialized size of a reserved variable based on its type
fn get_reserved_variable_size(native_var: NativeVariables) -> Option<(u64, u64)> {
    match native_var {
        NativeVariables::TxSender
        | NativeVariables::ContractCaller
        | NativeVariables::CurrentContract => {
            // Reserved variables are always standard principals (not contract principals)
            // Standard principal serializes as: 1 byte version + 20 bytes hash = 21 bytes
            // TypeSignature::PrincipalType.min_size() returns 20 (hash part), add 1 for version byte
            let principal_min =
                u64::from(TypeSignature::PrincipalType.min_size().unwrap_or(20)) + 1;
            // Since reserved variables are always standard principals, min and max are the same
            Some((principal_min, principal_min))
        }
        NativeVariables::BlockHeight
        | NativeVariables::StacksBlockHeight
        | NativeVariables::TenureHeight
        | NativeVariables::BurnBlockHeight
        | NativeVariables::TotalLiquidMicroSTX
        | NativeVariables::ChainId
        | NativeVariables::StacksBlockTime => {
            // UIntType has the same min and max size
            let uint_size = u64::from(TypeSignature::UIntType.size().unwrap_or(16));
            Some((uint_size, uint_size))
        }
        NativeVariables::NativeTrue | NativeVariables::NativeFalse => {
            // BoolType has the same min and max size
            let bool_size = u64::from(TypeSignature::BoolType.size().unwrap_or(1));
            Some((bool_size, bool_size))
        }
        _ => None,
    }
}

/// Infer the type of a field value from a tuple binding
/// Checks reserved variables, user arguments, and falls back to type inference
fn infer_field_type_from_binding(
    binding_pair: &[SymbolicExpression],
    epoch: StacksEpochId,
    user_args: Option<&UserArgumentsContext>,
) -> Option<TypeSignature> {
    if binding_pair.len() != 2 {
        return None;
    }

    // Try to infer type from the value expression
    if let Some(value_atom) = binding_pair[1].match_atom() {
        let clarity_version = ClarityVersion::default_for_epoch(epoch);

        // Check if it's a reserved variable
        if let Some(native_var) =
            NativeVariables::lookup_by_name_at_version(value_atom.as_str(), &clarity_version)
        {
            match native_var {
                NativeVariables::TxSender
                | NativeVariables::ContractCaller
                | NativeVariables::CurrentContract => {
                    return Some(TypeSignature::PrincipalType);
                }
                NativeVariables::TxSponsor => {
                    return TypeSignature::new_option(TypeSignature::PrincipalType).ok();
                }
                NativeVariables::BlockHeight
                | NativeVariables::StacksBlockHeight
                | NativeVariables::TenureHeight
                | NativeVariables::BurnBlockHeight
                | NativeVariables::TotalLiquidMicroSTX
                | NativeVariables::ChainId
                | NativeVariables::StacksBlockTime => {
                    return Some(TypeSignature::UIntType);
                }
                NativeVariables::NativeTrue | NativeVariables::NativeFalse => {
                    return Some(TypeSignature::BoolType);
                }
                NativeVariables::Regtest | NativeVariables::Mainnet => {
                    return Some(TypeSignature::BoolType);
                }
                NativeVariables::NativeNone => {
                    return TypeSignature::new_option(TypeSignature::NoType).ok();
                }
            }
        }

        // Check if it's a user function argument
        if let Some(arg_type) = user_args.and_then(|ua| ua.get_argument_type(value_atom)) {
            return Some(arg_type.clone());
        }
    }

    // Fallback: try to infer from expression
    super::cost_analysis::infer_type_from_expression(&binding_pair[1], epoch).ok()
}

/// Infer tuple size from a tuple expression by examining its bindings
/// Returns (min_size, max_size) for the serialized tuple
fn infer_tuple_size_from_expression(
    tuple_expr: &SymbolicExpression,
    epoch: StacksEpochId,
    user_args: Option<&UserArgumentsContext>,
) -> (u64, u64) {
    if let Some(tuple_list) = tuple_expr.match_list() {
        // Check if first element is 'tuple' to confirm it's a tuple expression
        if tuple_list
            .first()
            .and_then(|first| first.match_atom())
            .map(|atom| atom.as_str() == "tuple")
            .unwrap_or(false)
        {
            // It's a tuple, infer types from bindings
            let mut tuple_min_size = TUPLE_LENGTH_ENCODING_BYTES;
            let mut tuple_max_size = TUPLE_LENGTH_ENCODING_BYTES;

            // Add field overhead: 2 bytes per field
            let binding_count = tuple_list.len().saturating_sub(1);
            tuple_min_size =
                tuple_min_size.saturating_add(binding_count as u64 * TUPLE_FIELD_OVERHEAD_BYTES);
            tuple_max_size =
                tuple_max_size.saturating_add(binding_count as u64 * TUPLE_FIELD_OVERHEAD_BYTES);

            // Process each binding: (field_name field_value)
            for binding in tuple_list.iter().skip(1) {
                if let Some(binding_pair) = binding.match_list() {
                    if let Some(field_name) = binding_pair[0].match_atom() {
                        let name_len = field_name.len() as u64;

                        // Try to infer field type
                        if let Some(field_type) =
                            infer_field_type_from_binding(binding_pair, epoch, user_args)
                        {
                            // Use TypeSignature min_size and max_size
                            let field_min = u64::from(field_type.min_size().unwrap_or(0));
                            let field_max = u64::from(field_type.size().unwrap_or(0));
                            tuple_min_size = tuple_min_size
                                .saturating_add(field_min)
                                .saturating_add(name_len);
                            tuple_max_size = tuple_max_size
                                .saturating_add(field_max)
                                .saturating_add(name_len);
                        } else if let Some(literal_value) = binding_pair[1]
                            .match_atom_value()
                            .or_else(|| binding_pair[1].match_literal_value())
                        {
                            // Fallback: use literal value size
                            if let Ok(size) = literal_value.size() {
                                let s = u64::from(size);
                                tuple_min_size =
                                    tuple_min_size.saturating_add(s).saturating_add(name_len);
                                tuple_max_size =
                                    tuple_max_size.saturating_add(s).saturating_add(name_len);
                            }
                        }
                    }
                }
            }

            return (tuple_min_size, tuple_max_size);
        } else {
            // Not a tuple, try to infer type from the expression itself
            if let Ok(expr_type) =
                super::cost_analysis::infer_type_from_expression(tuple_expr, epoch)
            {
                let expr_min = u64::from(expr_type.min_size().unwrap_or(0));
                let expr_max = u64::from(expr_type.size().unwrap_or(0));
                return (expr_min, expr_max);
            }
        }
    }

    // Try atom (variable reference) via user_args
    if let Some(atom_name) = tuple_expr.match_atom() {
        if let Some(type_sig) = user_args.and_then(|ua| ua.get_argument_type(atom_name)) {
            let min = u64::from(type_sig.min_size().unwrap_or(0));
            let max = u64::from(type_sig.size().unwrap_or(0));
            return (min, max);
        }
    }

    // Fallback: try literal value size
    if let Some(literal_value) = tuple_expr
        .match_atom_value()
        .or_else(|| tuple_expr.match_literal_value())
    {
        if let Ok(size) = literal_value.size() {
            let s = u64::from(size);
            return (s, s);
        }
    }

    // No size could be determined from the expression alone.
    // Callers that have map_types in UserArgumentsContext will use the declared
    // map type before reaching this fallback, so returning (0, 0) here only
    // affects calls without that context (e.g. FetchEntry, which accepts any key).
    eprintln!(
        "Warning: inference failed for tuple expression `{}`",
        tuple_expr
    );
    (0, 0)
}

/// Get min/max serialized sizes from an argument expression
/// Checks literal values, user arguments, and reserved variables
fn get_argument_sizes(
    arg: &SymbolicExpression,
    epoch: StacksEpochId,
    user_args: Option<&UserArgumentsContext>,
) -> (Option<u64>, Option<u64>) {
    // Try literal value first
    if let Some(value) = arg.match_atom_value().or_else(|| arg.match_literal_value()) {
        if let Ok(size) = value.size() {
            let s = u64::from(size);
            return (Some(s), Some(s));
        }
    }

    // Try variable name
    if let Some(var_name) = arg.match_atom() {
        if let Some(type_sig) = user_args.and_then(|ua| ua.get_argument_type(var_name)) {
            let min = type_sig.min_size().map(u64::from).unwrap_or(0);
            let max = type_sig.size().map(u64::from).unwrap_or(0);
            return (Some(min), Some(max));
        }

        // Try reserved variables
        let clarity_version = ClarityVersion::default_for_epoch(epoch);
        if let Some(native_var) =
            NativeVariables::lookup_by_name_at_version(var_name.as_str(), &clarity_version)
        {
            if let Some((min, max)) = get_reserved_variable_size(native_var) {
                return (Some(min), Some(max));
            }
        }
    }

    (None, None)
}

/// Resolve the TypeSignature of a data variable from its name in args.
fn resolve_data_var_type<'a>(
    args: &[SymbolicExpression],
    user_args: Option<&'a UserArgumentsContext>,
) -> Option<&'a TypeSignature> {
    let var_name = args.first().and_then(|arg| arg.match_atom())?;
    user_args?.get_data_var_type(var_name)
}

/// Resolve key and value TypeSignatures for a map, trying environment first then user context.
fn resolve_map_types(
    args: &[SymbolicExpression],
    env: Option<&clarity::vm::contexts::Environment>,
    user_args: Option<&UserArgumentsContext>,
) -> Option<(TypeSignature, TypeSignature)> {
    let map_name = args.first().and_then(|arg| arg.match_atom())?;

    // Try environment first
    if let Some(map_metadata) = env.and_then(|e| e.contract_context.meta_data_map.get(map_name)) {
        return Some((map_metadata.key_type.clone(), map_metadata.value_type.clone()));
    }

    // Fallback to user_args
    let (key_type, value_type) = user_args?.get_map_type(map_name)?;
    Some((key_type.clone(), value_type.clone()))
}

pub fn get_cost_for_special_function(
    native_function: NativeFunctions,
    args: &[SymbolicExpression],
    epoch: StacksEpochId,
    user_args: Option<&UserArgumentsContext>,
    env: Option<&clarity::vm::contexts::Environment>,
) -> StaticCost {
    match native_function {
        NativeFunctions::Let => {
            let cost = cost_binding_list_len(args, epoch);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::If => {
            let cost = ClarityCostFunction::If
                .eval_for_epoch(0, epoch)
                .unwrap_or(ExecutionCost::ZERO);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::TupleCons => {
            // TupleCons cost is based on the number of fields (arguments)
            let binding_len = args.len() as u64;
            let cost = ClarityCostFunction::TupleCons
                .eval_for_epoch(binding_len, epoch)
                .unwrap_or(ExecutionCost::ZERO);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::ContractCall => {
            let cost = contract_call_cost(args, epoch);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::ListCons => {
            let cost = cost_list_cons(args, epoch);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::TupleGet => {
            let cost = cost_tuple_get(args, epoch);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::Append => {
            let cost = cost_append(args, epoch);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::Concat => {
            let cost = cost_concat(args, epoch);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::Slice => {
            let cost = cost_slice(args, epoch);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::ReplaceAt => {
            let cost = cost_replace_at(args, epoch);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::FetchVar => {
            let cost = cost_fetch_var(args, epoch, env, user_args);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::SetVar => cost_set_var(args, epoch, user_args),
        NativeFunctions::FetchEntry => cost_fetch_entry(args, epoch, env, user_args),
        NativeFunctions::SetEntry => cost_set_entry(args, epoch, env, user_args),
        NativeFunctions::InsertEntry => cost_insert_entry(args, epoch, env, user_args),
        NativeFunctions::DeleteEntry => cost_delete_entry(args, epoch, env, user_args),
        NativeFunctions::Print => {
            let cost = cost_print(args, epoch);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::ToAscii => {
            let cost = cost_to_ascii(args, epoch);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
        NativeFunctions::CmpGeq
        | NativeFunctions::CmpLeq
        | NativeFunctions::CmpGreater
        | NativeFunctions::CmpLess => cost_comparison(native_function, args, epoch, user_args),
        NativeFunctions::Equals => cost_equals(args, epoch, user_args),
        NativeFunctions::MintAsset => cost_mint_asset(args, epoch, user_args),
        NativeFunctions::Hash160
        | NativeFunctions::Sha256
        | NativeFunctions::Sha512
        | NativeFunctions::Sha512Trunc256
        | NativeFunctions::Keccak256 => {
            // Runtime charges based on serialized_size (cost_input_sized_vararg),
            // which includes a type prefix byte that value.size() omits.
            let cost_fn = from_native_function(native_function);
            let (min_size, max_size) = args
                .first()
                .map(|arg| {
                    // For literals, use serialized_size to match runtime behavior
                    if let Some(value) =
                        arg.match_atom_value().or_else(|| arg.match_literal_value())
                    {
                        if let Ok(size) = value.serialized_size() {
                            let s = u64::from(size);
                            return (Some(s), Some(s));
                        }
                    }
                    // For variables, fall back to get_argument_sizes
                    get_argument_sizes(arg, epoch, user_args)
                })
                .unwrap_or((None, None));
            let fallback = u64::try_from(args.len()).unwrap_or(0);
            let min_size = min_size.unwrap_or(fallback);
            let max_size = max_size.unwrap_or(fallback);
            let min_cost = cost_fn
                .eval_for_epoch(min_size, epoch)
                .unwrap_or(ExecutionCost::ZERO);
            let max_cost = cost_fn
                .eval_for_epoch(max_size, epoch)
                .unwrap_or(ExecutionCost::ZERO);
            StaticCost {
                min: min_cost,
                max: max_cost,
            }
        }
        native_function => {
            let cost_fn = from_native_function(native_function);
            if matches!(cost_fn, ClarityCostFunction::Unimplemented) {
                eprintln!(
                    "warning: no cost function implemented for {native_function:?}, using zero cost",
                );
                return StaticCost::ZERO;
            }
            let cost = cost_fn
                .eval_for_epoch(args.len() as u64, epoch)
                .unwrap_or(ExecutionCost::ZERO);
            StaticCost {
                min: cost.clone(),
                max: cost,
            }
        }
    }
}

// contract-call cost is static (134 in epoch 3+), the second part (load
// contract) is from `execute_contract` and the function application is in
// `execute_apply`
pub fn contract_call_cost(args: &[SymbolicExpression], epoch: StacksEpochId) -> ExecutionCost {
    // if epoch is less than 3.3, the argument type is used, but after 3.3 the argument size is used
    // For static analysis, we extract literal values from the arguments and calculate their sizes
    // args structure: [contract-identifier-or-trait-ref, function-name, arg1, arg2, ...]
    //   - args[0]: contract identifier (principal) for static dispatch, or trait reference (atom) for dynamic dispatch
    //   - args[1]: function name
    //   - args[2..]: actual function arguments to the called contract function
    // We need to process args[2..] (the actual function arguments)

    if epoch < StacksEpochId::Epoch33 {
        // Before epoch 3.3, the function application cost (in execute_apply) was based on
        // parameter types, not argument sizes. However, the base contract-call cost itself
        // is static and doesn't depend on arguments - the cost function ignores the parameter.
        // The runtime also passes 0 here (see special_contract_call in database.rs:70).
        ClarityCostFunction::ContractCall
            .eval_for_epoch(0, epoch)
            .unwrap_or(ExecutionCost::ZERO)
    } else {
        // After epoch 3.3, use actual argument sizes
        // Extract literal values from args[2..] and sum their sizes
        let rest_args = args.get(2..).unwrap_or(&[]);
        let mut total_size = 0_u64;

        for arg in rest_args {
            // Try to extract literal value from the symbolic expression
            if let Some(value) = arg.match_atom_value().or_else(|| arg.match_literal_value()) {
                // Calculate the size of the value
                // Value::size() returns u32, so we convert to u64
                if let Ok(size) = value.size() {
                    total_size = total_size.saturating_add(u64::from(size));
                }
            }
        }

        ClarityCostFunction::ContractCall
            .eval_for_epoch(total_size, epoch)
            .unwrap_or(ExecutionCost::ZERO)
    }
}

pub fn cost_binding_list_len(args: &[SymbolicExpression], epoch: StacksEpochId) -> ExecutionCost {
    let binding_len = args
        .first()
        .and_then(|e| e.match_list())
        .map(|binding_list| binding_list.len() as u64)
        .unwrap_or(0);
    ClarityCostFunction::Let
        .eval_for_epoch(binding_len, epoch)
        .unwrap_or(ExecutionCost::ZERO)
}

// ListCons cost is based on sum of argument sizes
pub fn cost_list_cons(args: &[SymbolicExpression], epoch: StacksEpochId) -> ExecutionCost {
    let mut total_size = 0u64;
    for arg in args {
        if let Some(value) = arg.match_atom_value().or_else(|| arg.match_literal_value()) {
            if let Ok(size) = value.size() {
                total_size = total_size.saturating_add(u64::from(size));
            }
        }
    }
    ClarityCostFunction::ListCons
        .eval_for_epoch(total_size, epoch)
        .unwrap_or(ExecutionCost::ZERO)
}

// TupleGet cost is based on tuple length
// For static analysis, we try to extract the tuple from args[1] if it's a literal
pub fn cost_tuple_get(args: &[SymbolicExpression], epoch: StacksEpochId) -> ExecutionCost {
    let tuple_len = args
        .get(1)
        .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
        .and_then(|value| {
            if let Value::Tuple(tuple_data) = value {
                Some(tuple_data.len())
            } else {
                None
            }
        })
        .unwrap_or(0);
    ClarityCostFunction::TupleGet
        .eval_for_epoch(tuple_len, epoch)
        .unwrap_or(ExecutionCost::ZERO)
}

// Append cost is based on max of entry type size and element type size
// For static analysis, we try to extract sizes from literal values
pub fn cost_append(args: &[SymbolicExpression], epoch: StacksEpochId) -> ExecutionCost {
    let size = args
        .first()
        .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
        .and_then(|seq_value| {
            args.get(1)
                .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
                .and_then(|elem_value| {
                    // Try to get sizes from values
                    let seq_size = u64::from(seq_value.size().ok()?);
                    let elem_size = u64::from(elem_value.size().ok()?);
                    Some(std::cmp::max(seq_size, elem_size))
                })
        })
        .unwrap_or(0);
    ClarityCostFunction::Append
        .eval_for_epoch(size, epoch)
        .unwrap_or(ExecutionCost::ZERO)
}

// Concat cost is based on sum of sequence sizes
// Epoch-dependent: v200 uses value sizes, v205 uses sequence lengths
pub fn cost_concat(args: &[SymbolicExpression], epoch: StacksEpochId) -> ExecutionCost {
    let clarity_version = ClarityVersion::default_for_epoch(epoch);
    let size = if clarity_version >= ClarityVersion::Clarity2 {
        // v205: use sequence lengths
        args.first()
            .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
            .and_then(|seq1| {
                args.get(1)
                    .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
                    .and_then(|seq2| {
                        if let (Value::Sequence(s1), Value::Sequence(s2)) = (seq1, seq2) {
                            Some((s1.len() as u64).saturating_add(s2.len() as u64))
                        } else {
                            None
                        }
                    })
            })
            .unwrap_or(1) // fallback to 1 on error (matches runtime behavior)
    } else {
        // v200: use value sizes
        args.first()
            .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
            .and_then(|seq1| {
                args.get(1)
                    .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
                    .and_then(|seq2| {
                        let size1 = u64::from(seq1.size().ok()?);
                        let size2 = u64::from(seq2.size().ok()?);
                        size1.checked_add(size2)
                    })
            })
            .unwrap_or(0)
    };
    ClarityCostFunction::Concat
        .eval_for_epoch(size, epoch)
        .unwrap_or(ExecutionCost::ZERO)
}

// Slice cost is based on (right_position - left_position) * element_size
// For static analysis, we try to extract positions from args
pub fn cost_slice(args: &[SymbolicExpression], epoch: StacksEpochId) -> ExecutionCost {
    let size = args
        .get(1)
        .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
        .and_then(|left_val| {
            args.get(2)
                .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
                .and_then(|right_val| {
                    args.first()
                        .and_then(|arg| {
                            arg.match_atom_value().or_else(|| arg.match_literal_value())
                        })
                        .and_then(|seq_val| {
                            if let (Value::UInt(left), Value::UInt(right), Value::Sequence(seq)) =
                                (left_val, right_val, seq_val)
                            {
                                if right >= left {
                                    let slice_len = (right - left) as u64;
                                    let elem_size = u64::from(seq.element_size().ok()?);
                                    Some(slice_len * elem_size)
                                } else {
                                    Some(0)
                                }
                            } else {
                                None
                            }
                        })
                })
        })
        .unwrap_or(0);
    ClarityCostFunction::Slice
        .eval_for_epoch(size, epoch)
        .unwrap_or(ExecutionCost::ZERO)
}

// ReplaceAt cost is based on sequence type size
pub fn cost_replace_at(args: &[SymbolicExpression], epoch: StacksEpochId) -> ExecutionCost {
    let size = args
        .first()
        .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
        .and_then(|seq_val| {
            if let Value::Sequence(seq) = seq_val {
                // Try to get element size from sequence
                seq.element_size().ok().map(u64::from)
            } else {
                None
            }
        })
        .unwrap_or(0);
    ClarityCostFunction::ReplaceAt
        .eval_for_epoch(size, epoch)
        .unwrap_or(ExecutionCost::ZERO)
}

// FetchVar cost uses the serialized size of the stored variable's value type
pub fn cost_fetch_var(
    args: &[SymbolicExpression],
    epoch: StacksEpochId,
    _env: Option<&clarity::vm::contexts::Environment>,
    user_args: Option<&UserArgumentsContext>,
) -> ExecutionCost {
    let size = resolve_data_var_type(args, user_args)
        .and_then(|type_sig| type_sig.max_serialized_size().ok())
        .map(u64::from)
        .unwrap_or(0);
    ClarityCostFunction::FetchVar
        .eval_for_epoch(size, epoch)
        .unwrap_or(ExecutionCost::ZERO)
}

// SetVar cost uses the serialized size of the variable's value type.
// Runtime charges based on serialized_byte_len of the stored value (includes type prefix).
pub fn cost_set_var(
    args: &[SymbolicExpression],
    epoch: StacksEpochId,
    user_args: Option<&UserArgumentsContext>,
) -> StaticCost {
    // SetVar args: [var-name, value]
    let (min_size, max_size) = resolve_data_var_type(args, user_args)
        .map(|type_sig| {
            let min = u64::from(type_sig.min_size().unwrap_or(0));
            let max = type_sig
                .max_serialized_size()
                .ok()
                .map(u64::from)
                .unwrap_or(0);
            (min, max)
        })
        .unwrap_or((0, 0));

    let min_cost = ClarityCostFunction::SetVar
        .eval_for_epoch(min_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);
    let max_cost = ClarityCostFunction::SetVar
        .eval_for_epoch(max_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);

    StaticCost {
        min: min_cost,
        max: max_cost,
    }
}

// FetchEntry cost uses the serialized sizes of the map's key and value types.
// Runtime charges based on serialized_byte_len of the result:
//   key not found → key_serialized_size only
//   key found     → key_serialized_size + value_serialized_size
pub fn cost_fetch_entry(
    args: &[SymbolicExpression],
    epoch: StacksEpochId,
    env: Option<&clarity::vm::contexts::Environment>,
    user_args: Option<&UserArgumentsContext>,
) -> StaticCost {
    // FetchEntry args: [map-name, key]
    let (min_size, max_size) = resolve_map_types(args, env, user_args)
        .map(|(key_type, value_type)| {
            // min: key not found case, only key size charged
            let min = u64::from(key_type.min_size().unwrap_or(0));
            // max: key found case, key + Optional(value) sizes charged
            let key_max = u64::from(key_type.max_serialized_size().unwrap_or(0));
            let value_max = TypeSignature::new_option(value_type)
                .ok()
                .and_then(|t| t.max_serialized_size().ok())
                .map(u64::from)
                .unwrap_or(0);
            (min, key_max + value_max)
        })
        .unwrap_or((0, 0));

    let min_cost = ClarityCostFunction::FetchEntry
        .eval_for_epoch(min_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);
    let max_cost = ClarityCostFunction::FetchEntry
        .eval_for_epoch(max_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);

    StaticCost {
        min: min_cost,
        max: max_cost,
    }
}

// SetEntry cost is epoch-dependent and uses result size
// For static analysis, we calculate min/max sizes from TypeSignature
pub fn cost_set_entry(
    args: &[SymbolicExpression],
    epoch: StacksEpochId,
    env: Option<&clarity::vm::contexts::Environment>,
    user_args: Option<&UserArgumentsContext>,
) -> StaticCost {
    // SetEntry args: [map-name, key, value]
    let (min_size, max_size) = if args.len() >= 3 {
        resolve_map_types(args, env, user_args)
            .map(|(key_type, value_type)| {
                let key_min = u64::from(key_type.min_size().unwrap_or(0));
                let key_max = u64::from(key_type.max_serialized_size().unwrap_or(0));
                let value_min = u64::from(value_type.min_size().unwrap_or(0));
                let value_max = TypeSignature::new_option(value_type)
                    .ok()
                    .and_then(|t| t.max_serialized_size().ok())
                    .map(u64::from)
                    .unwrap_or(0);
                (key_min.saturating_add(value_min), key_max.saturating_add(value_max))
            })
            .unwrap_or_else(|| {
                // Last resort: infer types from the key/value expressions themselves
                let (key_min, key_max) =
                    infer_tuple_size_from_expression(&args[1], epoch, user_args);
                let (value_min, value_max) =
                    infer_tuple_size_from_expression(&args[2], epoch, user_args);
                (key_min.saturating_add(value_min), key_max.saturating_add(value_max))
            })
    } else {
        (0, 0)
    };

    let min_cost = ClarityCostFunction::SetEntry
        .eval_for_epoch(min_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);
    let max_cost = ClarityCostFunction::SetEntry
        .eval_for_epoch(max_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);

    StaticCost {
        min: min_cost,
        max: max_cost,
    }
}

// InsertEntry cost is epoch-dependent and uses result size
// Note: InsertEntry uses SetEntry cost function
pub fn cost_insert_entry(
    args: &[SymbolicExpression],
    epoch: StacksEpochId,
    env: Option<&clarity::vm::contexts::Environment>,
    user_args: Option<&UserArgumentsContext>,
) -> StaticCost {
    // InsertEntry uses the same cost calculation as SetEntry
    cost_set_entry(args, epoch, env, user_args)
}

// DeleteEntry cost is epoch-dependent and uses result size
// Note: DeleteEntry uses SetEntry cost function
pub fn cost_delete_entry(
    args: &[SymbolicExpression],
    epoch: StacksEpochId,
    env: Option<&clarity::vm::contexts::Environment>,
    user_args: Option<&UserArgumentsContext>,
) -> StaticCost {
    // DeleteEntry uses the same cost calculation as SetEntry
    cost_set_entry(args, epoch, env, user_args)
}

// Print cost is based on input size
pub fn cost_print(args: &[SymbolicExpression], epoch: StacksEpochId) -> ExecutionCost {
    let size = args
        .first()
        .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
        .and_then(|value| value.size().ok().map(u64::from))
        .unwrap_or(0);
    ClarityCostFunction::Print
        .eval_for_epoch(size, epoch)
        .unwrap_or(ExecutionCost::ZERO)
}

// ToAscii cost is based on value size
pub fn cost_to_ascii(args: &[SymbolicExpression], epoch: StacksEpochId) -> ExecutionCost {
    let size = args
        .first()
        .and_then(|arg| arg.match_atom_value().or_else(|| arg.match_literal_value()))
        .and_then(|value| value.size().ok().map(u64::from))
        .unwrap_or(0);
    ClarityCostFunction::ToAscii
        .eval_for_epoch(size, epoch)
        .unwrap_or(ExecutionCost::ZERO)
}

// Comparison functions (Geq, Leq, Ge, Le) are epoch-dependent:
// v1 uses args.len(), v2 uses min(a.size(), b.size())
pub fn cost_comparison(
    native_function: NativeFunctions,
    args: &[SymbolicExpression],
    epoch: StacksEpochId,
    user_args: Option<&UserArgumentsContext>,
) -> StaticCost {
    let clarity_version = ClarityVersion::default_for_epoch(epoch);
    let cost_fn = match native_function {
        NativeFunctions::CmpGeq => ClarityCostFunction::Geq,
        NativeFunctions::CmpLeq => ClarityCostFunction::Leq,
        NativeFunctions::CmpGreater => ClarityCostFunction::Ge,
        NativeFunctions::CmpLess => ClarityCostFunction::Le,
        _ => {
            return StaticCost {
                min: ExecutionCost::ZERO,
                max: ExecutionCost::ZERO,
            };
        }
    };

    let (min_size, max_size) = if clarity_version >= ClarityVersion::Clarity2 {
        // v2 min(a.size(), b.size()) - use min_size and max_size from TypeSignature
        // Try to get min/max sizes from literal values first, then from variable types
        let (min_a, max_a) = args
            .first()
            .map(|arg| get_argument_sizes(arg, epoch, user_args))
            .unwrap_or((None, None));
        let (min_b, max_b) = args
            .get(1)
            .map(|arg| get_argument_sizes(arg, epoch, user_args))
            .unwrap_or((None, None));

        // For v2, cost is based on min(a.size(), b.size())
        // We need min of mins and min of maxes for the cost calculation
        let min_size = match (min_a, min_b) {
            (Some(a), Some(b)) => std::cmp::min(a, b),
            (Some(a), None) => a,
            (None, Some(b)) => b,
            (None, None) => args.len() as u64, // fallback
        };

        let max_size = match (max_a, max_b) {
            (Some(a), Some(b)) => std::cmp::min(a, b), // Still min for comparison cost
            (Some(a), None) => a,
            (None, Some(b)) => b,
            (None, None) => args.len() as u64, // fallback
        };

        (min_size, max_size)
    } else {
        // v1 - use args.len() for both min and max
        let size = args.len() as u64;
        (size, size)
    };

    let min_cost = cost_fn
        .eval_for_epoch(min_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);
    let max_cost = cost_fn
        .eval_for_epoch(max_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);

    StaticCost {
        min: min_cost,
        max: max_cost,
    }
}

// Equals cost is epoch-dependent and uses sum of all argument sizes
// For static analysis, we calculate min/max sizes from TypeSignature
pub fn cost_equals(
    args: &[SymbolicExpression],
    epoch: StacksEpochId,
    user_args: Option<&UserArgumentsContext>,
) -> StaticCost {
    // Sum all argument sizes (min and max separately)
    let mut total_min_size = 0u64;
    let mut total_max_size = 0u64;
    let mut any_resolved = false;

    for arg in args.iter() {
        let (min_size, max_size) = get_argument_sizes(arg, epoch, user_args);
        if let Some(min) = min_size {
            total_min_size = total_min_size.saturating_add(min);
            any_resolved = true;
        }
        if let Some(max) = max_size {
            total_max_size = total_max_size.saturating_add(max);
            any_resolved = true;
        }
    }

    // Fallback to args.len() if we couldn't determine any sizes
    if !any_resolved {
        let size = args.len() as u64;
        total_min_size = size;
        total_max_size = size;
    }

    let min_cost = ClarityCostFunction::Eq
        .eval_for_epoch(total_min_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);
    let max_cost = ClarityCostFunction::Eq
        .eval_for_epoch(total_max_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);

    StaticCost {
        min: min_cost,
        max: max_cost,
    }
}

// MintAsset cost is based on asset identifier size.
// args layout (function name already stripped): [asset-class, asset-identifier, recipient]
// Note: from v2.05 onwards the VM uses Value::serialized_size() which includes the
// consensus type-prefix byte, while get_argument_sizes uses Value::size() /
// TypeSignature::size() which do not.  We add 1 to compensate.
pub fn cost_mint_asset(
    args: &[SymbolicExpression],
    epoch: StacksEpochId,
    user_args: Option<&UserArgumentsContext>,
) -> StaticCost {
    let (min_size, max_size) = args
        .get(1)
        .map(|arg| get_argument_sizes(arg, epoch, user_args))
        .unwrap_or((None, None));

    // +1 for consensus serialization type-prefix byte (v2.05+ uses serialized_size)
    let min_size = min_size.map_or(1, |s| s + 1);
    let max_size = max_size.map_or(min_size, |s| s + 1);

    let min_cost = ClarityCostFunction::NftMint
        .eval_for_epoch(min_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);
    let max_cost = ClarityCostFunction::NftMint
        .eval_for_epoch(max_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);

    StaticCost {
        min: min_cost,
        max: max_cost,
    }
}
