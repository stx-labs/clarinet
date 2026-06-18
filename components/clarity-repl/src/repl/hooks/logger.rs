use clarity::vm::contexts::{ExecutionState, InvocationContext, LocalContext};
use clarity::vm::errors::VmExecutionError;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::{
    EvalHook, ExecutionResult, SymbolicExpression, SymbolicExpressionType, ValueRef,
};

use crate::repl::boot::{
    BOOT_MAINNET_PRINCIPAL, BOOT_TESTNET_PRINCIPAL, SBTC_MAINNET_ADDRESS_PRINCIPAL,
    SBTC_TESTNET_ADDRESS_PRINCIPAL,
};
use crate::repl::clarity_values::value_to_string;
use crate::repl::settings::LogPrintEvents;

#[derive(Clone)]
pub struct LoggerHook {
    mode: LogPrintEvents,
}

impl LoggerHook {
    pub fn new(mode: LogPrintEvents) -> Self {
        Self { mode }
    }
}

impl EvalHook for LoggerHook {
    fn will_begin_eval(
        &mut self,
        _: &mut ExecutionState,
        _: &InvocationContext,
        _: &LocalContext,
        _: &SymbolicExpression,
    ) {
    }

    fn did_finish_eval<'a>(
        &mut self,
        _env: &mut ExecutionState,
        invoke_ctx: &'a InvocationContext,
        _context: &'a LocalContext,
        expr: &SymbolicExpression,
        res: &Result<ValueRef<'a>, VmExecutionError>,
    ) {
        let SymbolicExpressionType::List(list) = &expr.expr else {
            return;
        };
        let Some((function_name, _args)) = list.split_first() else {
            return;
        };
        let Some(function_name) = function_name.match_atom() else {
            return;
        };

        if let Some(NativeFunctions::Print) = NativeFunctions::lookup_by_name(function_name) {
            let contract_id = &invoke_ctx.contract_context.contract_identifier;

            if self.mode == LogPrintEvents::ProjectOnly {
                let issuer = &contract_id.issuer;
                if issuer == &*BOOT_TESTNET_PRINCIPAL
                    || issuer == &*BOOT_MAINNET_PRINCIPAL
                    || issuer == &*SBTC_TESTNET_ADDRESS_PRINCIPAL
                    || issuer == &*SBTC_MAINNET_ADDRESS_PRINCIPAL
                {
                    return;
                }
            }

            let contract_name = &contract_id.name;
            let span = &expr.span;
            let line_annotation = format!("({}:{})", contract_name, span.start_line);

            match res {
                Ok(value) => {
                    let value_str = value_to_string(value.as_ref());
                    uprint!("{value_str} {line_annotation}");
                }
                Err(err) => {
                    uprint!("{err} {line_annotation}");
                }
            }
        }
    }

    fn did_complete(&mut self, _: Result<&mut ExecutionResult, String>) {}
}
