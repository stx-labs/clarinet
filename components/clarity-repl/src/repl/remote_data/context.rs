use clarity::types::StacksEpochId;
use clarity::vm::callables::{DefineType, DefinedFunction};
use clarity::vm::costs::LimitedCostTracker;
use clarity::vm::errors::{CheckErrors, InterpreterResult as Result, SyntaxBindingErrorType};
use clarity::vm::functions::define::DefineFunctionsParsed;
use clarity::vm::types::parse_name_type_pairs;
use clarity::vm::{ClarityName, ContractContext, SymbolicExpression};

fn handle_function(
    epoch_id: &StacksEpochId,
    signature: &[SymbolicExpression],
    body: SymbolicExpression,
    define_type: DefineType,
    context_name: &str,
    cost_tracker: &mut LimitedCostTracker,
) -> Result<(ClarityName, DefinedFunction)> {
    let (function_symbol, arg_symbols) = signature
        .split_first()
        .ok_or(CheckErrors::DefineFunctionBadSignature)?;
    let function_name = function_symbol
        .match_atom()
        .ok_or(CheckErrors::ExpectedName)?;
    let arguments = parse_name_type_pairs::<_, CheckErrors>(
        *epoch_id,
        arg_symbols,
        SyntaxBindingErrorType::Eval,
        cost_tracker,
    )?;
    Ok((
        function_name.clone(),
        DefinedFunction::new(arguments, body, define_type, function_name, context_name),
    ))
}

// this is a simplified version of `clarity::vm::eval_all`
// that doesn't evaluate the expressions, but only gets the types
pub fn set_functions_in_contract_context(
    expressions: &[SymbolicExpression],
    contract_context: &mut ContractContext,
    epoch_id: &StacksEpochId,
) -> Result<()> {
    let context_name = contract_context.contract_identifier.to_string();
    let mut ct = LimitedCostTracker::Free;

    for exp in expressions {
        let try_define_exp = DefineFunctionsParsed::try_parse(exp);
        if let Ok(Some(define_exp)) = try_define_exp {
            match define_exp {
                DefineFunctionsParsed::PrivateFunction { signature, body } => {
                    let (name, func) = handle_function(
                        epoch_id,
                        signature,
                        body.clone(),
                        DefineType::Private,
                        &context_name,
                        &mut ct,
                    )?;
                    contract_context.functions.insert(name.clone(), func);
                }
                DefineFunctionsParsed::ReadOnlyFunction { signature, body } => {
                    let (name, func) = handle_function(
                        epoch_id,
                        signature,
                        body.clone(),
                        DefineType::ReadOnly,
                        &context_name,
                        &mut ct,
                    )?;
                    contract_context.functions.insert(name.clone(), func);
                }
                DefineFunctionsParsed::PublicFunction { signature, body } => {
                    let (name, func) = handle_function(
                        epoch_id,
                        signature,
                        body.clone(),
                        DefineType::Public,
                        &context_name,
                        &mut ct,
                    )?;
                    contract_context.functions.insert(name.clone(), func);
                }
                DefineFunctionsParsed::Constant { .. }
                | DefineFunctionsParsed::PersistedVariable { .. }
                | DefineFunctionsParsed::Map { .. }
                | DefineFunctionsParsed::NonFungibleToken { .. }
                | DefineFunctionsParsed::BoundedFungibleToken { .. }
                | DefineFunctionsParsed::UnboundedFungibleToken { .. }
                | DefineFunctionsParsed::Trait { .. }
                | DefineFunctionsParsed::ImplTrait { .. }
                | DefineFunctionsParsed::UseTrait { .. } => {}
            }
        }
    }

    Ok(())
}
