use std::collections::HashMap;

use clarity::vm::contexts::{ExecutionState, InvocationContext, LocalContext};
use clarity::vm::errors::VmExecutionError;
use clarity::vm::events::{FTEventType, NFTEventType, STXEventType, StacksTransactionEvent};
use clarity::vm::functions::define::DefineFunctions;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::{
    eval, ClarityVersion, EvalHook, SymbolicExpression, SymbolicExpressionType, ValueRef,
};
use clarity_types::types::PrincipalData;
use serde::{Deserialize, Serialize};

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum TraceKind {
    Call,
    Return,
    Event,
    Error,
    /// A `let`-binding value captured during evaluation. `function` holds the
    /// variable name and `value` holds the evaluated result.
    Var,
}

/// A single entry in a structured execution trace.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TraceEntry {
    pub kind: TraceKind,
    /// Call-stack depth at the time of this entry (0 = top-level call).
    pub depth: usize,
    /// Contract name (short form, without deployer prefix for brevity).
    pub contract: String,
    /// Function name; empty for `Event` and `Error` entries.
    #[serde(skip_serializing_if = "str::is_empty")]
    pub function: String,
    pub line: u32,
    pub column: u32,
    /// Argument values (stringified Clarity values), present on `Call` entries.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub args: Vec<String>,
    /// Return value (for `Return`) or event description (for `Event`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub value: Option<String>,
    /// Error message, present on `Error` entries.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

// ---------------------------------------------------------------------------
// Internal state
// ---------------------------------------------------------------------------

/// Info needed to emit a `Return` entry once a function call completes.
/// Parallel to the `stack` of expression IDs.
struct ReturnInfo {
    contract: String,
    function: String,
    depth: usize,
    line: u32,
    column: u32,
}

/// Metadata for a buffered call that is still collecting argument values.
/// Parallel to `pending_arg_ids`.
struct PendingCallMeta {
    contract: String,
    function: String,
    depth: usize,
    line: u32,
    column: u32,
    collected_args: Vec<String>,
}

// ---------------------------------------------------------------------------
// Hook
// ---------------------------------------------------------------------------

/// An [`EvalHook`] that emits a structured [`TraceEntry`] stream.
///
/// The design mirrors [`TracerHook`]:
/// - `stack` / `return_info` track active function-call expressions so we can
///   emit `Return` entries when they complete.
/// - `pending_arg_ids` / `pending_call_meta` buffer calls until all argument
///   expressions have been evaluated, letting us include arg values in `Call`
///   entries.
pub struct AgentTraceHook {
    pub entries: Vec<TraceEntry>,
    /// Current call-stack depth.
    depth: usize,
    /// Expression IDs of active function calls (mirrors TracerHook.stack).
    stack: Vec<u64>,
    /// Parallel to `stack` — metadata for building `Return` entries.
    return_info: Vec<ReturnInfo>,
    /// Pending arg IDs per buffered call (mirrors TracerHook.pending_args).
    pending_arg_ids: Vec<Vec<u64>>,
    /// Parallel to `pending_arg_ids` — metadata for building `Call` entries.
    pending_call_meta: Vec<PendingCallMeta>,
    nb_of_emitted_events: usize,
    error_recorded: bool,
    /// Maps a binding value expression ID to its variable name for `let` forms.
    /// Populated in `will_begin_eval` when a `let` is encountered, consumed in
    /// `did_finish_eval` to emit `Var` trace entries.
    binding_names: HashMap<u64, String>,
}

impl Default for AgentTraceHook {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
            depth: 0,
            stack: vec![u64::MAX],
            return_info: Vec::new(),
            pending_arg_ids: Vec::new(),
            pending_call_meta: Vec::new(),
            nb_of_emitted_events: 0,
            error_recorded: false,
            binding_names: HashMap::new(),
        }
    }
}

impl AgentTraceHook {
    pub fn new() -> Self {
        Self::default()
    }
}

impl EvalHook for AgentTraceHook {
    fn will_begin_eval(
        &mut self,
        env: &mut ExecutionState,
        invoke_ctx: &InvocationContext,
        context: &LocalContext,
        expr: &SymbolicExpression,
    ) {
        let SymbolicExpressionType::List(list) = &expr.expr else {
            return;
        };
        let Some((head, args)) = list.split_first() else {
            return;
        };
        let Some(function_name) = head.match_atom() else {
            return;
        };

        if DefineFunctions::lookup_by_name(function_name).is_some() {
            return;
        }

        // Special case: register `let` binding IDs so we can emit Var entries.
        if matches!(
            NativeFunctions::lookup_by_name_at_version(function_name, &ClarityVersion::latest()),
            Some(NativeFunctions::Let)
        ) {
            // args[0] = binding list: a List of (name value) pairs.
            if let Some(SymbolicExpressionType::List(bindings)) = args.first().map(|a| &a.expr) {
                for binding in bindings.iter() {
                    if let SymbolicExpressionType::List(pair) = &binding.expr {
                        if let Some((name_expr, rest)) = pair.split_first() {
                            if let (Some(name), Some(val_expr)) =
                                (name_expr.match_atom(), rest.first())
                            {
                                self.binding_names.insert(val_expr.id, name.to_string());
                            }
                        }
                    }
                }
            }
            return;
        }

        let current_depth = self.depth;

        let (contract, function, arg_ids) = if let Some(native) =
            NativeFunctions::lookup_by_name_at_version(function_name, &ClarityVersion::latest())
        {
            match native {
                NativeFunctions::ContractCall => {
                    // args[0] = callee contract, args[1] = function name, args[2..] = call args
                    let callee = args
                        .first()
                        .filter(|a| a.match_atom().is_some())
                        .and_then(|a| eval(a, env, invoke_ctx, context).ok())
                        .map(|v| v.as_ref().to_string())
                        .unwrap_or_else(|| "?".to_string());
                    let fn_name = args
                        .get(1)
                        .and_then(|a| a.match_atom())
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| "?".to_string());
                    let ids = args
                        .get(2..)
                        .unwrap_or(&[])
                        .iter()
                        .map(|a| a.id)
                        .collect::<Vec<_>>();
                    (callee, fn_name, ids)
                }
                NativeFunctions::Fold | NativeFunctions::Map => {
                    // args[0] = callback name atom, args[1..] = sequence(s) [+ initial acc for fold].
                    // Both fold and map dispatch the callback via apply_evaluated (bypassing hooks),
                    // so individual iterations are invisible. We emit one entry for the whole
                    // operation using a "fold:name" / "map:name" prefix to make this clear.
                    let op = if matches!(native, NativeFunctions::Fold) {
                        "fold"
                    } else {
                        "map"
                    };
                    let callback_name = args
                        .first()
                        .and_then(|a| a.match_atom())
                        .map(|s| format!("{op}:{s}"))
                        .unwrap_or_else(|| format!("{op}:?"));
                    let contract = invoke_ctx
                        .contract_context
                        .contract_identifier
                        .name
                        .to_string();
                    let ids = args
                        .get(1..)
                        .unwrap_or(&[])
                        .iter()
                        .map(|a| a.id)
                        .collect::<Vec<_>>();
                    (contract, callback_name, ids)
                }
                _ => return,
            }
        } else {
            // User-defined function in the current contract.
            let contract = invoke_ctx
                .contract_context
                .contract_identifier
                .name
                .to_string();
            let ids = args.iter().map(|a| a.id).collect::<Vec<_>>();
            (contract, function_name.to_string(), ids)
        };

        self.depth += 1;

        // Always push to the return-tracking stacks.
        self.stack.push(expr.id);
        self.return_info.push(ReturnInfo {
            contract: contract.clone(),
            function: function.clone(),
            depth: current_depth,
            line: expr.span.start_line,
            column: expr.span.start_column,
        });

        if arg_ids.is_empty() {
            // No args to collect — emit Call immediately.
            self.entries.push(TraceEntry {
                kind: TraceKind::Call,
                depth: current_depth,
                contract,
                function,
                line: expr.span.start_line,
                column: expr.span.start_column,
                args: vec![],
                value: None,
                error: None,
            });
        } else {
            // Buffer until all arg expressions have been evaluated.
            self.pending_call_meta.push(PendingCallMeta {
                contract,
                function,
                depth: current_depth,
                line: expr.span.start_line,
                column: expr.span.start_column,
                collected_args: vec![],
            });
            self.pending_arg_ids.push(arg_ids);
        }
    }

    fn did_finish_eval<'a>(
        &mut self,
        env: &mut ExecutionState,
        invoke_ctx: &'a InvocationContext,
        _context: &'a LocalContext,
        expr: &SymbolicExpression,
        res: &Result<ValueRef<'a>, VmExecutionError>,
    ) {
        let current_contract = || {
            invoke_ctx
                .contract_context
                .contract_identifier
                .name
                .to_string()
        };

        // 1. Collect newly emitted events.
        let emitted_events = env
            .global_context
            .event_batches
            .iter()
            .flat_map(|b| &b.0.events)
            .collect::<Vec<_>>();
        for event in emitted_events.iter().skip(self.nb_of_emitted_events) {
            self.entries.push(TraceEntry {
                kind: TraceKind::Event,
                depth: self.depth,
                contract: current_contract(),
                function: String::new(),
                line: 0,
                column: 0,
                args: vec![],
                value: Some(format_event(event)),
                error: None,
            });
        }
        self.nb_of_emitted_events = emitted_events.len();

        // 2. Record the first error with its source location.
        if let Err(e) = res {
            if !self.error_recorded {
                self.error_recorded = true;
                self.entries.push(TraceEntry {
                    kind: TraceKind::Error,
                    depth: self.depth.saturating_sub(1),
                    contract: current_contract(),
                    function: String::new(),
                    line: expr.span.start_line,
                    column: expr.span.start_column,
                    args: vec![],
                    value: None,
                    error: Some(e.to_string()),
                });
            }
        }

        // 3. Detect function return: mirrors TracerHook's stack check.
        if let Some(&last_id) = self.stack.last() {
            if last_id == expr.id {
                self.stack.pop();
                if let Some(info) = self.return_info.pop() {
                    self.depth = self.depth.saturating_sub(1);
                    self.entries.push(TraceEntry {
                        kind: TraceKind::Return,
                        depth: info.depth,
                        contract: info.contract,
                        function: info.function,
                        line: info.line,
                        column: info.column,
                        args: vec![],
                        value: res.as_ref().ok().map(|v| v.as_ref().to_string()),
                        error: None,
                    });
                }
            }
        }

        // 4. Collect argument values for buffered calls: mirrors TracerHook's
        //    pending_args check.
        if let Some(arg_stack) = self.pending_arg_ids.last_mut() {
            if let Some((&first_id, rest)) = arg_stack.split_first() {
                if first_id == expr.id {
                    if let (Ok(value), Some(meta)) = (res, self.pending_call_meta.last_mut()) {
                        meta.collected_args.push(value.as_ref().to_string());
                    }

                    if rest.is_empty() {
                        // All args collected — emit the Call entry.
                        if let Some(meta) = self.pending_call_meta.pop() {
                            self.entries.push(TraceEntry {
                                kind: TraceKind::Call,
                                depth: meta.depth,
                                contract: meta.contract,
                                function: meta.function,
                                line: meta.line,
                                column: meta.column,
                                args: meta.collected_args,
                                value: None,
                                error: None,
                            });
                        }
                        self.pending_arg_ids.pop();
                    } else {
                        arg_stack.remove(0);
                    }
                }
            }
        }

        // 5. Emit a Var entry for any let-binding whose value just finished.
        if let Some(var_name) = self.binding_names.remove(&expr.id) {
            if let Ok(value) = res {
                self.entries.push(TraceEntry {
                    kind: TraceKind::Var,
                    depth: self.depth,
                    contract: current_contract(),
                    function: var_name,
                    line: expr.span.start_line,
                    column: expr.span.start_column,
                    args: vec![],
                    value: Some(value.as_ref().to_string()),
                    error: None,
                });
            }
        }
    }

    fn did_complete(
        &mut self,
        _result: core::result::Result<&mut clarity::vm::ExecutionResult, String>,
    ) {
        // All entries have been emitted incrementally via will_begin_eval /
        // did_finish_eval; nothing to do here.
    }
}

// ---------------------------------------------------------------------------
// Event formatting
// ---------------------------------------------------------------------------

fn format_event(event: &StacksTransactionEvent) -> String {
    match event {
        StacksTransactionEvent::SmartContractEvent(data) => {
            format!("print: {}", data.value)
        }
        StacksTransactionEvent::STXEvent(kind) => match kind {
            STXEventType::STXTransferEvent(d) => format!(
                "stx_transfer: {} from {} to {}",
                d.amount,
                shorten(&d.sender),
                shorten(&d.recipient),
            ),
            STXEventType::STXMintEvent(d) => {
                format!("stx_mint: {} to {}", d.amount, shorten(&d.recipient))
            }
            STXEventType::STXBurnEvent(d) => {
                format!("stx_burn: {} from {}", d.amount, shorten(&d.sender))
            }
            STXEventType::STXLockEvent(d) => format!(
                "stx_lock: {} for {} until block {}",
                d.locked_amount,
                shorten(&d.locked_address),
                d.unlock_height,
            ),
        },
        StacksTransactionEvent::NFTEvent(kind) => match kind {
            NFTEventType::NFTMintEvent(d) => format!(
                "nft_mint: {} to {}",
                d.asset_identifier.asset_name,
                shorten(&d.recipient),
            ),
            NFTEventType::NFTTransferEvent(d) => format!(
                "nft_transfer: {} from {} to {}",
                d.asset_identifier.asset_name,
                shorten(&d.sender),
                shorten(&d.recipient),
            ),
            NFTEventType::NFTBurnEvent(d) => format!(
                "nft_burn: {} from {}",
                d.asset_identifier.asset_name,
                shorten(&d.sender),
            ),
        },
        StacksTransactionEvent::FTEvent(kind) => match kind {
            FTEventType::FTMintEvent(d) => format!(
                "ft_mint: {} {} to {}",
                d.amount,
                d.asset_identifier.asset_name,
                shorten(&d.recipient),
            ),
            FTEventType::FTTransferEvent(d) => format!(
                "ft_transfer: {} {} from {} to {}",
                d.amount,
                d.asset_identifier.asset_name,
                shorten(&d.sender),
                shorten(&d.recipient),
            ),
            FTEventType::FTBurnEvent(d) => format!(
                "ft_burn: {} {} from {}",
                d.amount,
                d.asset_identifier.asset_name,
                shorten(&d.sender),
            ),
        },
    }
}

fn shorten(principal: &PrincipalData) -> String {
    let s = principal.to_string();
    format!("{}…{}", &s[..4], &s[s.len() - 4..])
}
