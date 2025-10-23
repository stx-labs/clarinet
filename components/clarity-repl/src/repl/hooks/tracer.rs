use clarity::vm::contexts::{Environment, LocalContext};
use clarity::vm::errors::Error;
use clarity::vm::events::StacksTransactionEvent;
use clarity::vm::functions::define::DefineFunctions;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::{
    eval, ClarityVersion, EvalHook, EvaluationResult, SymbolicExpression, SymbolicExpressionType,
};
use clarity_types::types::{
    PrincipalData, QualifiedContractIdentifier, StandardPrincipalData, Value,
};

pub struct TracerErrorOutput {
    contract_id: QualifiedContractIdentifier,
    expr: SymbolicExpression,
    error: String,
}

impl std::fmt::Display for TracerErrorOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let header = format!(
            "Error occured in {}:{}:{}",
            self.contract_id.name, self.expr.span.start_line, self.expr.span.start_column
        );
        let header_underline = "=".repeat(header.len());
        write!(
            f,
            "\n{}\n{}\nExpression:\n{}\nError: {}",
            header, header_underline, self.expr, self.error
        )
    }
}

pub struct TracerHook {
    pub output: Vec<String>,
    pub error: Option<TracerErrorOutput>,
    stack: Vec<u64>,
    pending_call_string: Vec<String>,
    pending_args: Vec<Vec<u64>>,
    nb_of_emitted_events: usize,
}

impl Default for TracerHook {
    fn default() -> Self {
        Self {
            output: Vec::new(),
            error: None,
            stack: vec![u64::MAX],
            pending_call_string: Vec::new(),
            pending_args: Vec::new(),
            nb_of_emitted_events: 0,
        }
    }
}

impl TracerHook {
    pub fn new() -> Self {
        Self::default()
    }

    fn add_to_output(&mut self, s: String) {
        self.output.push(s);
    }
}

impl EvalHook for TracerHook {
    fn will_begin_eval(
        &mut self,
        env: &mut Environment,
        context: &LocalContext,
        expr: &SymbolicExpression,
    ) {
        let SymbolicExpressionType::List(list) = &expr.expr else {
            return;
        };
        if let Some((function_name, args)) = list.split_first() {
            if let Some(function_name) = function_name.match_atom() {
                if DefineFunctions::lookup_by_name(function_name).is_some() {
                    return;
                } else if let Some(native_function) = NativeFunctions::lookup_by_name_at_version(
                    function_name,
                    &ClarityVersion::latest(),
                ) {
                    match native_function {
                        NativeFunctions::ContractCall => {
                            let mut call = format!(
                                "{}├── {}  {}\n",
                                "│   ".repeat(
                                    self.stack
                                        .len()
                                        .saturating_sub(self.pending_call_string.len())
                                        .saturating_sub(1)
                                ),
                                expr,
                                black!(
                                    "{}:{}:{}",
                                    env.contract_context.contract_identifier.name,
                                    expr.span.start_line,
                                    expr.span.start_column,
                                ),
                            );

                            let mut lines = Vec::new();
                            if args[0].match_atom().is_some() {
                                let callee = if let Ok(value) = eval(&args[0], env, context) {
                                    value.to_string()
                                } else {
                                    "?".to_string()
                                };
                                lines.push(format!(
                                    "{}│ {}",
                                    "│   ".repeat(
                                        self.stack
                                            .len()
                                            .saturating_sub(self.pending_call_string.len())
                                    ),
                                    purple!("↳ callee: {callee}"),
                                ));
                            }

                            if !args.is_empty() {
                                lines.push(format!(
                                    "{}│ {}",
                                    "│   ".repeat(
                                        self.stack
                                            .len()
                                            .saturating_sub(self.pending_call_string.len())
                                    ),
                                    purple!("↳ args:"),
                                ));
                                call.push_str(lines.join("\n").as_str());
                                self.pending_call_string.push(call);
                                self.pending_args
                                    .push(args[2..].iter().map(|arg| arg.id).collect());
                            } else {
                                self.add_to_output(format!(
                                    "{}{}",
                                    "│   ".repeat(
                                        self.stack
                                            .len()
                                            .saturating_sub(self.pending_call_string.len())
                                    ),
                                    call
                                ));
                            }
                        }
                        _ => return,
                    }
                } else {
                    // Call user-defined function
                    let mut call = format!(
                        "{}├── {}  {}\n",
                        "│   ".repeat(
                            self.stack
                                .len()
                                .saturating_sub(self.pending_call_string.len())
                                .saturating_sub(1)
                        ),
                        expr,
                        black!(
                            "{}:{}:{}",
                            env.contract_context.contract_identifier.name,
                            expr.span.start_line,
                            expr.span.start_column,
                        ),
                    );
                    call.push_str(
                        format!(
                            "{}│ {}",
                            "│   ".repeat(
                                self.stack
                                    .len()
                                    .saturating_sub(self.pending_call_string.len())
                            ),
                            purple!("↳ args:"),
                        )
                        .as_str(),
                    );
                    if !args.is_empty() {
                        self.pending_call_string.push(call);
                        self.pending_args
                            .push(args.iter().map(|arg| arg.id).collect());
                    } else {
                        self.add_to_output(format!(
                            "{}{}",
                            "│   ".repeat(
                                self.stack
                                    .len()
                                    .saturating_sub(self.pending_call_string.len())
                            ),
                            call
                        ));
                    }
                }
            }
        }
        self.stack.push(expr.id);
    }

    fn did_finish_eval(
        &mut self,
        env: &mut Environment,
        _context: &LocalContext,
        expr: &SymbolicExpression,
        res: &Result<Value, Error>,
    ) {
        if let Err(e) = res {
            if self.error.is_none() {
                self.error = Some(TracerErrorOutput {
                    contract_id: env.contract_context.contract_identifier.clone(),
                    expr: expr.clone(),
                    error: e.to_string(),
                });
            }
        }

        // Print events as they are emitted
        let emitted_events = env
            .global_context
            .event_batches
            .iter()
            .flat_map(|b| &b.events)
            .collect::<Vec<_>>();
        if emitted_events.len() > self.nb_of_emitted_events {
            for event in emitted_events.iter().skip(self.nb_of_emitted_events) {
                let event_message = format_event_data(event);
                self.add_to_output(format!(
                    "{}│ {}",
                    "│   ".repeat(
                        self.stack
                            .len()
                            .saturating_sub(self.pending_call_string.len())
                            .saturating_sub(1),
                    ),
                    black!("✸ {event_message}"),
                ));
            }
            self.nb_of_emitted_events = emitted_events.len();
        }

        if let Some(last) = self.stack.last() {
            if *last == expr.id {
                if let Ok(value) = res {
                    self.add_to_output(format!(
                        "{}└── {}",
                        "│   ".repeat(
                            self.stack
                                .len()
                                .saturating_sub(self.pending_call_string.len())
                                .saturating_sub(1)
                        ),
                        blue!("{value}")
                    ));
                }
                self.stack.pop();
            }
        }

        // Collect argument values
        if let Some(arg_stack) = self.pending_args.last_mut() {
            if let Some((arg, rest)) = arg_stack.split_first() {
                if *arg == expr.id {
                    if let Ok(value) = res {
                        self.pending_call_string
                            .last_mut()
                            .unwrap()
                            .push_str(format!(" {value}").as_str());
                    }

                    // If this was the last argument, print the pending call and pop the stack
                    if rest.is_empty() {
                        let pending = self.pending_call_string.pop().unwrap();
                        self.add_to_output(pending.clone());
                        self.pending_args.pop();
                    } else {
                        arg_stack.remove(0);
                    }
                }
            }
        }
    }

    fn did_complete(
        &mut self,
        result: core::result::Result<&mut clarity::vm::ExecutionResult, String>,
    ) {
        match result {
            Ok(result) => {
                match &result.result {
                    EvaluationResult::Contract(contract_result) => {
                        if let Some(value) = &contract_result.result {
                            self.add_to_output(format!("└── {}", blue!("{value}")));
                        }
                    }
                    EvaluationResult::Snippet(snippet_result) => {
                        self.add_to_output(format!("└── {}", blue!("{}", snippet_result.result)));
                    }
                };
            }
            Err(e) => {
                if e.contains("Runtime error while interpreting") {
                    self.add_to_output(format!("Error: {}", e.split(':').next().unwrap_or(&e)));
                    return;
                }
                self.add_to_output(format!("Error: {}", e));
            }
        }
    }
}

fn format_event_data(event: &StacksTransactionEvent) -> String {
    use clarity::vm::events::*;
    match event {
        StacksTransactionEvent::SmartContractEvent(data) => {
            format!("print event: {}", data.value)
        }
        StacksTransactionEvent::STXEvent(stxevent_type) => match stxevent_type {
            STXEventType::STXTransferEvent(data) => {
                format!(
                    "stx transfer event: {} STX from {} to {}",
                    data.amount,
                    shorten_principal(&data.sender),
                    shorten_principal(&data.recipient)
                )
            }
            STXEventType::STXMintEvent(data) => {
                format!(
                    "stx mint event: {} STX to {}",
                    data.amount,
                    shorten_principal(&data.recipient)
                )
            }
            STXEventType::STXBurnEvent(data) => {
                format!(
                    "stx burn event: {} STX from {}",
                    data.amount,
                    shorten_principal(&data.sender)
                )
            }
            STXEventType::STXLockEvent(data) => {
                format!(
                    "stx lock event: {} STX for {} until {}",
                    data.locked_amount,
                    shorten_principal(&data.locked_address),
                    data.unlock_height
                )
            }
        },
        StacksTransactionEvent::NFTEvent(nftevent_type) => match nftevent_type {
            NFTEventType::NFTMintEvent(data) => format!(
                "nft mint event: mint {} to {}",
                data.asset_identifier.asset_name,
                shorten_principal(&data.recipient)
            ),
            NFTEventType::NFTTransferEvent(data) => format!(
                "nft transfer event: transfer {} from {} to {}",
                data.asset_identifier.asset_name,
                shorten_principal(&data.sender),
                shorten_principal(&data.recipient)
            ),
            NFTEventType::NFTBurnEvent(data) => format!(
                "nft burn event: burn {} from {}",
                data.asset_identifier.asset_name,
                shorten_principal(&data.sender)
            ),
        },
        StacksTransactionEvent::FTEvent(ftevent_type) => match ftevent_type {
            FTEventType::FTMintEvent(data) => format!(
                "ft mint event: mint {} {} to {}",
                data.amount,
                data.asset_identifier.asset_name,
                shorten_principal(&data.recipient)
            ),
            FTEventType::FTTransferEvent(data) => format!(
                "ft transfer event: transfer {} {} from {} to {}",
                data.amount,
                data.asset_identifier.asset_name,
                shorten_principal(&data.sender),
                shorten_principal(&data.recipient)
            ),
            FTEventType::FTBurnEvent(data) => format!(
                "ft burn event: burn {} {} from {}",
                data.amount,
                data.asset_identifier.asset_name,
                shorten_principal(&data.sender)
            ),
        },
    }
}

fn shorten_principal(principal: &PrincipalData) -> String {
    match principal {
        PrincipalData::Standard(standard) => shorten_standard(standard),
        PrincipalData::Contract(contract) => {
            format!("{}.{}", shorten_standard(&contract.issuer), contract.name)
        }
    }
}

fn shorten_standard(principal: &StandardPrincipalData) -> String {
    let str = principal.to_string();
    format!("{}...{}", &str[..4], &str[str.len() - 4..])
}
