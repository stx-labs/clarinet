use std::collections::HashMap;
use std::path::PathBuf;

use clarity::vm::callables::FunctionIdentifier;
use clarity::vm::contexts::{ContractContext, Environment, GlobalContext, LocalContext};
use clarity::vm::errors::Error;
use clarity::vm::representations::Span;
use clarity::vm::{EvalHook, EvaluationResult, ExecutionResult, SymbolicExpression};
use clarity_types::types::{
    PrincipalData, QualifiedContractIdentifier, SequenceData, StandardPrincipalData,
};
use clarity_types::Value;
use debug_types::events::*;
use debug_types::requests::*;
use debug_types::responses::*;
use debug_types::types::*;
use debug_types::*;
use futures::{SinkExt, StreamExt};
use tokio;
use tokio::io::{Stdin, Stdout};
use tokio::runtime::Runtime;
use tokio_util::codec::{FramedRead, FramedWrite};

use self::codec::{DebugAdapterCodec, ParseError};
use super::{extract_watch_variable, AccessType, DebugState, State};
use crate::repl::DEFAULT_EPOCH;

pub mod codec;

/*
 * DAP Session:
 *      VSCode                    DAPDebugger
 *        |                            |
 *        |--- initialize ------------>|
 *        |<-- initialize response ----|
 *        |--- launch ---------------->|
 *        |<-- launch response --------|
 *        |<-- initialized event ------|
 *        |<-- stopped event ----------|
 *        |--- set breakpoints ------->|
 *        |<-- set bps response -------|
 *        |--- threads --------------->|
 *        |<-- threads response -------|
 *        |--- set exception bps ----->|
 *        |<-- set exc bps response ---|
 *        |--- threads --------------->|
 *        |<-- threads response -------|
 */

struct Current {
    expr_id: u64,
    stack: Vec<FunctionIdentifier>,
}

pub struct DAPDebugger {
    rt: Runtime,
    default_sender: Option<StandardPrincipalData>,
    pub path_to_contract_id: HashMap<PathBuf, QualifiedContractIdentifier>,
    pub contract_id_to_path: HashMap<QualifiedContractIdentifier, PathBuf>,
    reader: FramedRead<Stdin, DebugAdapterCodec<ProtocolMessage>>,
    writer: FramedWrite<Stdout, DebugAdapterCodec<ProtocolMessage>>,
    state: Option<DebugState>,
    send_seq: i64,
    launched: Option<(String, String)>,
    launch_seq: i64,
    current: Option<Current>,
    init_complete: bool,

    stack_frames: HashMap<FunctionIdentifier, StackFrame>,
    scopes: HashMap<i32, Vec<Scope>>,
    variables: HashMap<i32, Vec<Variable>>,
}

impl Default for DAPDebugger {
    fn default() -> Self {
        Self::new()
    }
}

impl DAPDebugger {
    pub fn new() -> Self {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();

        let reader = FramedRead::new(stdin, DebugAdapterCodec::<ProtocolMessage>::default());
        let writer = FramedWrite::new(stdout, DebugAdapterCodec::<ProtocolMessage>::default());
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        Self {
            rt,
            default_sender: None,
            path_to_contract_id: HashMap::new(),
            contract_id_to_path: HashMap::new(),
            reader,
            writer,
            state: None,
            send_seq: 0,
            launched: None,
            launch_seq: 0,
            current: None,
            init_complete: false,
            stack_frames: HashMap::new(),
            scopes: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    fn get_state(&mut self) -> &mut DebugState {
        self.state.as_mut().unwrap()
    }

    // Process all messages before launching the REPL
    pub fn init(&mut self) -> Result<(String, String), ParseError> {
        while self.launched.is_none() {
            match self.wait_for_command(None, None) {
                Ok(_) => (),
                Err(e) => return Err(e),
            }
        }
        Ok(self.launched.take().unwrap())
    }

    // Successful result boolean indicates if execution should continue based on the message received
    fn wait_for_command(
        &mut self,
        env: Option<&mut Environment>,
        context: Option<&LocalContext>,
    ) -> Result<bool, ParseError> {
        if let Some(msg) = self.rt.block_on(self.reader.next()) {
            match msg {
                Ok(msg) => {
                    use debug_types::MessageKind::*;
                    Ok(match msg.message {
                        Request(command) => self.handle_request(env, context, msg.seq, command),
                        Response(response) => {
                            self.handle_response(msg.seq, response);
                            false
                        }
                        Event(event) => {
                            self.handle_event(msg.seq, event);
                            false
                        }
                    })
                }
                Err(e) => Err(e),
            }
        } else {
            Ok(true)
        }
    }

    fn send_response(&mut self, response: Response) {
        let message = ProtocolMessage {
            seq: self.send_seq,
            message: MessageKind::Response(response),
        };

        match self.rt.block_on(self.writer.send(message)) {
            Ok(_) => (),
            Err(e) => {
                // If we can't send, there's not really anything else we can do.
                println!("{} send_response: {}", red!("error:"), e);
            }
        };

        self.send_seq += 1;
    }

    fn send_event(&mut self, body: EventBody) {
        let message = ProtocolMessage {
            seq: self.send_seq,
            message: MessageKind::Event(Event { body: Some(body) }),
        };

        match self.rt.block_on(self.writer.send(message)) {
            Ok(_) => (),
            Err(e) => {
                // If we can't send, there's not really anything else we can do.
                println!("{} send_event: {}", red!("error:"), e);
            }
        };

        self.send_seq += 1;
    }

    pub fn handle_request(
        &mut self,
        env: Option<&mut Environment>,
        context: Option<&LocalContext>,
        seq: i64,
        command: RequestCommand,
    ) -> bool {
        use debug_types::requests::RequestCommand::*;

        match command {
            Initialize(arguments) => self.initialize(seq, arguments),
            Launch(arguments) => self.launch(seq, arguments),
            ConfigurationDone => self.configuration_done(seq),
            SetBreakpoints(arguments) => self.set_breakpoints(seq, arguments),
            SetExceptionBreakpoints(arguments) => self.set_exception_breakpoints(seq, arguments),
            Disconnect(arguments) => self.quit(seq, arguments),
            Threads => self.threads(seq),
            StackTrace(arguments) => self.stack_trace(seq, arguments),
            Scopes(arguments) => self.scopes(seq, arguments),
            Variables(arguments) => self.variables(seq, arguments),
            StepIn(arguments) => self.step_in(seq, arguments),
            StepOut(arguments) => self.step_out(seq, arguments),
            Next(arguments) => self.next(seq, arguments),
            Continue(arguments) => self.continue_(seq, arguments),
            Pause(arguments) => self.pause(seq, arguments),
            Evaluate(arguments) => self.evaluate(seq, arguments, env, context),
            _ => {
                self.send_response(Response {
                    request_seq: seq,
                    success: false,
                    message: Some("unsupported request".to_string()),
                    body: None,
                });
                false
            }
        }
    }

    pub fn handle_event(&mut self, seq: i64, _event: Event) {
        let response = Response {
            request_seq: seq,
            success: true,
            message: None,
            body: None,
        };
        self.send_response(response);
    }

    pub fn handle_response(&mut self, seq: i64, _response: Response) {
        let response = Response {
            request_seq: seq,
            success: true,
            message: None,
            body: None,
        };
        self.send_response(response);
    }

    // Request handlers

    fn initialize(&mut self, seq: i64, _arguments: InitializeRequestArguments) -> bool {
        let capabilities = Capabilities {
            supports_configuration_done_request: Some(true),
            supports_function_breakpoints: Some(true),
            supports_step_in_targets_request: Some(true),
            support_terminate_debuggee: Some(true),
            supports_loaded_sources_request: Some(true),
            supports_data_breakpoints: Some(true),
            supports_breakpoint_locations_request: Some(true),
            supports_conditional_breakpoints: None,
            supports_hit_conditional_breakpoints: None,
            supports_evaluate_for_hovers: None,
            exception_breakpoint_filters: None,
            supports_step_back: None,
            supports_set_variable: None,
            supports_restart_frame: None,
            supports_goto_targets_request: None,
            supports_completions_request: None,
            completion_trigger_characters: None,
            supports_modules_request: None,
            additional_module_columns: None,
            supported_checksum_algorithms: None,
            supports_restart_request: None,
            supports_exception_options: None,
            supports_value_formatting_options: None,
            supports_exception_info_request: None,
            support_suspend_debuggee: None,
            supports_delayed_stack_trace_loading: None,
            supports_log_points: None,
            supports_terminate_threads_request: None,
            supports_set_expression: None,
            supports_terminate_request: None,
            supports_read_memory_request: None,
            supports_write_memory_request: None,
            supports_disassemble_request: None,
            supports_cancel_request: None,
            supports_clipboard_context: None,
            supports_stepping_granularity: None,
            supports_instruction_breakpoints: None,
            supports_exception_filter_options: None,
            supports_single_thread_execution_requests: None,
        };

        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::Initialize(InitializeResponse {
                capabilities,
            })),
        });

        false
    }

    pub fn log<S: Into<String>>(&mut self, message: S) {
        self.send_event(EventBody::Output(OutputEvent {
            category: Some(Category::Console),
            output: message.into(),
            group: None,
            variables_reference: None,
            source: None,
            line: None,
            column: None,
            data: None,
        }));
    }

    pub fn stdout<S: Into<String>>(&mut self, message: S) {
        self.send_event(EventBody::Output(OutputEvent {
            category: Some(Category::Stdout),
            output: message.into(),
            group: None,
            variables_reference: None,
            source: None,
            line: None,
            column: None,
            data: None,
        }));
    }

    pub fn stderr<S: Into<String>>(&mut self, message: S) {
        self.send_event(EventBody::Output(OutputEvent {
            category: Some(Category::Stderr),
            output: message.into(),
            group: None,
            variables_reference: None,
            source: None,
            line: None,
            column: None,
            data: None,
        }));
    }

    fn launch(&mut self, seq: i64, arguments: LaunchRequestArguments) -> bool {
        // Verify that the manifest and expression were specified
        let Some(manifest) = arguments.manifest else {
            self.send_response(Response {
                request_seq: seq,
                success: false,
                message: Some("manifest must be specified".to_string()),
                body: None,
            });
            return false;
        };
        let Some(expression) = arguments.expression else {
            self.send_response(Response {
                request_seq: seq,
                success: false,
                message: Some("expression to debug must be specified".to_string()),
                body: None,
            });
            return false;
        };

        let contract_id = QualifiedContractIdentifier::transient();
        self.state = Some(DebugState::new(&contract_id, &expression));
        self.launched = Some((manifest, expression));

        self.launch_seq = seq;

        false
    }

    fn configuration_done(&mut self, seq: i64) -> bool {
        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::ConfigurationDone),
        });

        // Now that configuration is done, we can respond to the launch
        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::Launch),
        });

        false
    }

    fn set_breakpoints(&mut self, seq: i64, arguments: SetBreakpointsArguments) -> bool {
        let mut results = vec![];
        if let Some(breakpoints) = arguments.breakpoints {
            let Some(contract_id) = self
                .path_to_contract_id
                .get(&PathBuf::from(arguments.source.path.as_ref().unwrap()))
            else {
                self.send_response(Response {
                    request_seq: seq,
                    success: false,
                    message: Some(format!(
                        "contract not found for path {}\nmap: {:?}",
                        arguments.source.path.clone().unwrap(),
                        self.path_to_contract_id
                    )),
                    body: None,
                });
                return false;
            };
            let source = super::Source {
                name: contract_id.clone(),
            };
            for breakpoint in breakpoints {
                let column = breakpoint.column.unwrap_or(0);
                let source_breakpoint = super::Breakpoint {
                    id: 0,
                    data: super::BreakpointData::Source(super::SourceBreakpoint {
                        line: breakpoint.line,
                        column: breakpoint.column,
                    }),
                    source: source.clone(),
                    span: Some(Span {
                        start_line: breakpoint.line,
                        start_column: column,
                        end_line: breakpoint.line,
                        end_column: column,
                    }),
                };
                let id = self.get_state().add_breakpoint(source_breakpoint);
                results.push(Breakpoint {
                    id: Some(id),
                    verified: true,
                    message: breakpoint.log_message,
                    source: Some(arguments.source.clone()),
                    line: Some(breakpoint.line),
                    column: breakpoint.column,
                    end_line: Some(breakpoint.line),
                    end_column: breakpoint.column,
                    instruction_reference: None,
                    offset: None,
                });
            }
        };

        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::SetBreakpoints(SetBreakpointsResponse {
                breakpoints: results,
            })),
        });

        false
    }

    fn set_exception_breakpoints(
        &mut self,
        seq: i64,
        _arguments: SetExceptionBreakpointsArguments,
    ) -> bool {
        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::SetExceptionBreakpoints(
                SetExceptionBreakpointsResponse { breakpoints: None },
            )),
        });

        false
    }

    fn threads(&mut self, seq: i64) -> bool {
        // There is only ever 1 thread
        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::Threads(ThreadsResponse {
                threads: vec![Thread {
                    id: 0,
                    name: "main".to_string(),
                }],
            })),
        });

        // VSCode doesn't seem to want to send us a ConfigurationDone request,
        // so assume that this is the end of configuration instead. This is an
        // ugly hack and should be changed!
        if !self.init_complete {
            self.send_response(Response {
                request_seq: self.launch_seq,
                success: true,
                message: None,
                body: Some(ResponseBody::Launch),
            });

            let stopped = StoppedEvent {
                reason: StoppedReason::Entry,
                description: None,
                thread_id: Some(0),
                preserve_focus_hint: None,
                text: Some("Stopped at start!!!".to_string()),
                all_threads_stopped: None,
                hit_breakpoint_ids: None,
            };

            self.send_event(EventBody::Stopped(stopped));
            self.init_complete = true;
        }

        false
    }

    fn stack_trace(&mut self, seq: i64, _arguments: StackTraceArguments) -> bool {
        let current = self.current.as_ref().unwrap();
        let frames: Vec<_> = current
            .stack
            .iter()
            .rev()
            .filter(|function| !function.to_string().starts_with("_native_:"))
            .map(|function| self.stack_frames[function].clone())
            .collect();

        let len = current.stack.len() as i32;
        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::StackTrace(StackTraceResponse {
                stack_frames: frames,
                total_frames: Some(len),
            })),
        });
        false
    }

    fn scopes(&mut self, seq: i64, arguments: ScopesArguments) -> bool {
        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::Scopes(ScopesResponse {
                scopes: self.scopes[&arguments.frame_id].clone(),
            })),
        });
        false
    }

    fn variables(&mut self, seq: i64, arguments: VariablesArguments) -> bool {
        let variables = match self.variables.get(&arguments.variables_reference) {
            Some(variables) => variables.clone(),
            None => {
                self.log("unknown variable reference");
                Vec::new()
            }
        };

        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::Variables(VariablesResponse { variables })),
        });
        false
    }

    fn step_in(&mut self, seq: i64, _arguments: StepInArguments) -> bool {
        self.get_state().step_in();

        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::StepIn),
        });
        true
    }

    fn step_out(&mut self, seq: i64, _arguments: StepOutArguments) -> bool {
        self.get_state().finish();

        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::StepOut),
        });
        true
    }

    fn next(&mut self, seq: i64, _arguments: NextArguments) -> bool {
        let expr_id = self.current.as_ref().unwrap().expr_id;
        self.get_state().step_over(expr_id);

        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::Next),
        });
        true
    }

    fn continue_(&mut self, seq: i64, _arguments: ContinueArguments) -> bool {
        self.get_state().continue_execution();

        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::Continue(ContinueResponse {
                all_threads_continued: None,
            })),
        });
        true
    }

    fn pause(&mut self, seq: i64, _arguments: PauseArguments) -> bool {
        self.get_state().pause();

        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::Pause),
        });
        true
    }

    fn evaluate(
        &mut self,
        seq: i64,
        arguments: EvaluateArguments,
        env: Option<&mut Environment>,
        context: Option<&LocalContext>,
    ) -> bool {
        let (Some(env), Some(context)) = (env, context) else {
            self.log("cannot evaluate an expression before initialization is complete");
            return true;
        };

        // Evaluate expressions coming from the `watch` context are handled
        // differently. These can be references to contract variables in the
        // format `principal.contract.variable` or `.contract.variable`. A
        // breakpoint should be added if one does not already exist, then the
        // value should be retrieved.
        let response = match arguments.context {
            Some(EvalContext::Watch) => {
                match extract_watch_variable(
                    env,
                    &arguments.expression,
                    self.default_sender.as_ref(),
                ) {
                    Ok((contract, name)) => {
                        let contract_id = &contract.contract_context.contract_identifier;
                        // Add the watchpoint (if one isn't already there)
                        let exists = if let Some(set) = self
                            .get_state()
                            .watch_variables
                            .get(&(contract_id.clone(), name.to_string()))
                        {
                            !set.is_empty()
                        } else {
                            false
                        };
                        if !exists {
                            self.get_state()
                                .add_watchpoint(contract_id, name, AccessType::Write);
                        }

                        if let Some(data_types) = contract.contract_context.meta_data_var.get(name)
                        {
                            let value = env
                                .global_context
                                .database
                                .lookup_variable(contract_id, name, data_types, &DEFAULT_EPOCH)
                                .unwrap();
                            Response {
                                request_seq: seq,
                                success: true,
                                message: None,
                                body: Some(ResponseBody::Evaluate(EvaluateResponse {
                                    result: value.to_string(),
                                    result_type: Some(format!("{}", data_types.value_type)),
                                    presentation_hint: Some(VariablePresentationHint {
                                        kind: Some(VariableKind::Property),
                                        attributes: Some(vec![
                                            VariableAttribute::HasDataBreakpoint,
                                        ]),
                                        visibility: None,
                                        lazy: None,
                                    }),
                                    variables_reference: 0,
                                    named_variables: None,
                                    indexed_variables: None,
                                    memory_reference: None,
                                })),
                            }
                        } else if let Some(data_types) =
                            contract.contract_context.meta_data_map.get(name)
                        {
                            let map_type =
                                format!("{{{}: {}}}", data_types.key_type, data_types.value_type);
                            Response {
                                request_seq: seq,
                                success: true,
                                message: None,
                                body: Some(ResponseBody::Evaluate(EvaluateResponse {
                                    result: map_type.clone(),
                                    result_type: Some(map_type),
                                    presentation_hint: Some(VariablePresentationHint {
                                        kind: Some(VariableKind::Data),
                                        attributes: Some(vec![
                                            VariableAttribute::HasDataBreakpoint,
                                        ]),
                                        visibility: None,
                                        lazy: None,
                                    }),
                                    variables_reference: 0,
                                    named_variables: None,
                                    indexed_variables: None,
                                    memory_reference: None,
                                })),
                            }
                        } else {
                            Response {
                                request_seq: seq,
                                success: false,
                                message: Some("undefined".to_string()),
                                body: None,
                            }
                        }
                    }
                    Err(e) => Response {
                        request_seq: seq,
                        success: false,
                        message: Some(e),
                        body: None,
                    },
                }
            }
            _ => match self
                .get_state()
                .evaluate(env, context, &arguments.expression)
            {
                Ok(value) => Response {
                    request_seq: seq,
                    success: true,
                    message: None,
                    body: Some(ResponseBody::Evaluate(EvaluateResponse {
                        result: value.to_string(),
                        result_type: Some(type_for_value(&value)),
                        presentation_hint: None,
                        variables_reference: 0,
                        named_variables: None,
                        indexed_variables: None,
                        memory_reference: None,
                    })),
                },
                Err(errors) => Response {
                    request_seq: seq,
                    success: false,
                    message: Some(errors.join("\n")),
                    body: None,
                },
            },
        };

        self.send_response(response);
        false
    }

    fn quit(&mut self, seq: i64, _arguments: DisconnectArguments) -> bool {
        self.get_state().quit();

        self.send_response(Response {
            request_seq: seq,
            success: true,
            message: None,
            body: Some(ResponseBody::Disconnect),
        });
        true
    }

    fn save_scopes_for_frame(
        &mut self,
        stack_frame: &StackFrame,
        local_context: &LocalContext,
        contract_context: &ContractContext,
        global_context: &mut GlobalContext,
    ) {
        let mut scopes = Vec::new();
        let mut current_context = Some(local_context);

        // Local variable scopes
        while let Some(ctx) = current_context {
            let scope_id = stack_frame.id * 1000 + (scopes.len() as i32);
            scopes.push(Scope {
                name: if ctx.depth() == 0 {
                    "Arguments"
                } else {
                    "Locals"
                }
                .to_string(),
                presentation_hint: if ctx.depth() == 0 {
                    Some(PresentationHint::Arguments)
                } else {
                    Some(PresentationHint::Locals)
                },
                variables_reference: scope_id,
                named_variables: Some(ctx.variables.len() + ctx.callable_contracts.len()),
                indexed_variables: None,
                expensive: false,
                source: stack_frame.source.clone(),
                line: None,
                column: None,
                end_line: None,
                end_column: None,
            });

            let mut variables = Vec::new();
            for (name, value) in &ctx.variables {
                variables.push(Variable {
                    name: name.to_string(),
                    value: value.to_string(),
                    var_type: Some(type_for_value(value)),
                    presentation_hint: None,
                    evaluate_name: None,
                    variables_reference: 0,
                    named_variables: None,
                    indexed_variables: None,
                    memory_reference: None,
                });
            }
            for (name, callable) in &ctx.callable_contracts {
                variables.push(Variable {
                    name: name.to_string(),
                    value: format!("{}", callable.contract_identifier),
                    var_type: Some(format!("{}", callable.trait_identifier.as_ref().unwrap())),
                    presentation_hint: None,
                    evaluate_name: None,
                    variables_reference: 0,
                    named_variables: None,
                    indexed_variables: None,
                    memory_reference: None,
                });
            }
            self.variables.insert(scope_id, variables);

            current_context = ctx.parent;
        }

        // Contract global scope
        let scope_id = stack_frame.id * 1000 + (scopes.len() as i32);
        scopes.push(Scope {
            name: "Contract Variables".to_string(),
            presentation_hint: None,
            variables_reference: scope_id,
            named_variables: Some(contract_context.variables.len()),
            indexed_variables: None,
            expensive: true,
            source: stack_frame.source.clone(),
            line: None,
            column: None,
            end_line: None,
            end_column: None,
        });
        let mut variables = Vec::new();

        // Constants
        for (name, value) in &contract_context.variables {
            variables.push(Variable {
                name: name.to_string(),
                value: value.to_string(),
                var_type: Some(type_for_value(value)),
                presentation_hint: None,
                evaluate_name: None,
                variables_reference: 0,
                named_variables: None,
                indexed_variables: None,
                memory_reference: None,
            });
        }

        // Variables
        for (name, metadata) in &contract_context.meta_data_var {
            let data_types = contract_context.meta_data_var.get(name).unwrap();
            let value = global_context
                .database
                .lookup_variable(
                    &contract_context.contract_identifier,
                    name.as_str(),
                    data_types,
                    &DEFAULT_EPOCH,
                )
                .unwrap();
            variables.push(Variable {
                name: name.to_string(),
                value: value.to_string(),
                var_type: Some(format!("{}", metadata.value_type)),
                presentation_hint: None,
                evaluate_name: None,
                variables_reference: 0,
                named_variables: None,
                indexed_variables: None,
                memory_reference: None,
            });
        }

        // Maps
        for (name, metadata) in &contract_context.meta_data_map {
            // We do not grab any values for maps. Users can query map values in the console.
            let map_type = format!("{{{}: {}}}", metadata.key_type, metadata.value_type);
            variables.push(Variable {
                name: name.to_string(),
                value: map_type.clone(),
                var_type: Some(map_type),
                presentation_hint: None,
                evaluate_name: None,
                variables_reference: 0,
                named_variables: None,
                indexed_variables: None,
                memory_reference: None,
            });
        }
        self.variables.insert(scope_id, variables);

        self.scopes.insert(stack_frame.id, scopes);
    }
}

impl EvalHook for DAPDebugger {
    fn will_begin_eval(
        &mut self,
        env: &mut Environment,
        context: &LocalContext,
        expr: &SymbolicExpression,
    ) {
        let source = Source {
            name: Some(env.contract_context.contract_identifier.to_string()),
            path: Some(
                match self
                    .contract_id_to_path
                    .get(&env.contract_context.contract_identifier)
                {
                    Some(path) => path.to_str().unwrap().to_string(),
                    _ => "debugger".to_string(),
                },
            ),
            source_reference: None,
            presentation_hint: None,
            origin: None,
            sources: None,
            adapter_data: None,
            checksums: None,
        };

        let stack_trace = env.call_stack.make_stack_trace();

        // Find the current function frame, ignoring builtin functions.
        let mut current_function = None;
        for function in stack_trace.iter().rev() {
            if !function.to_string().starts_with("_native_:") {
                current_function = Some(function);
                break;
            }
        }
        if let Some(current_function) = current_function {
            if let Some(mut stack_top) = self.stack_frames.remove(current_function) {
                stack_top.line = expr.span.start_line;
                stack_top.column = expr.span.start_column;
                stack_top.end_line = Some(expr.span.end_line);
                stack_top.end_column = Some(expr.span.end_column);

                self.save_scopes_for_frame(
                    &stack_top,
                    context,
                    env.contract_context,
                    env.global_context,
                );
                self.stack_frames
                    .insert(current_function.clone(), stack_top);
            } else {
                let stack_frame = StackFrame {
                    id: (stack_trace.len() as i32 + 1),
                    name: current_function.to_string(),
                    source: Some(source),
                    line: expr.span.start_line,
                    column: expr.span.start_column,
                    end_line: Some(expr.span.end_line),
                    end_column: Some(expr.span.end_column),
                    can_restart: None,
                    instruction_pointer_reference: None,
                    module_id: None,
                    presentation_hint: Some(PresentationHint::Normal),
                };
                self.save_scopes_for_frame(
                    &stack_frame,
                    context,
                    env.contract_context,
                    env.global_context,
                );

                self.stack_frames
                    .insert(current_function.clone(), stack_frame);
            }
        }

        if !self.get_state().will_begin_eval(env, context, expr) {
            if self.get_state().state == State::Start {
                // Sending this initialized event triggers the configuration
                // (e.g. setting breakpoints), after which the ConfigurationDone
                // request should be sent, but it's not, so there is an ugly
                // hack in threads to handle that.
                self.default_sender = Some(match &env.sender {
                    Some(sender) => match sender {
                        PrincipalData::Standard(standard) => standard.clone(),
                        PrincipalData::Contract(contract) => contract.issuer.clone(),
                    },
                    None => StandardPrincipalData::transient(),
                });
                self.send_event(EventBody::Initialized);
            } else {
                let mut stopped = StoppedEvent {
                    reason: StoppedReason::Entry,
                    description: None,
                    thread_id: Some(0),
                    preserve_focus_hint: None,
                    text: None,
                    all_threads_stopped: None,
                    hit_breakpoint_ids: None,
                };

                match self.get_state().state {
                    State::Start => {
                        stopped.reason = StoppedReason::Entry;
                    }
                    State::Break(breakpoint) => {
                        stopped.reason = StoppedReason::Breakpoint;
                        stopped.hit_breakpoint_ids = Some(vec![breakpoint]);
                    }
                    State::DataBreak(breakpoint, _access_type) => {
                        stopped.reason = StoppedReason::DataBreakpoint;
                        stopped.hit_breakpoint_ids = Some(vec![breakpoint]);
                    }
                    State::Finished | State::StepIn | State::StepOver(_) => {
                        stopped.reason = StoppedReason::Step;
                    }
                    State::Pause => {
                        stopped.reason = StoppedReason::Pause;
                    }
                    _ => unreachable!("Unexpected state"),
                };
                self.send_event(EventBody::Stopped(stopped));
            }

            // Save the current state, which may be needed to respond to incoming requests
            self.current = Some(Current {
                expr_id: expr.id,
                stack: stack_trace,
            });

            let mut proceed = false;
            while !proceed {
                proceed = match self.wait_for_command(Some(env), Some(context)) {
                    Ok(proceed) => proceed,
                    Err(e) => {
                        self.log(format!("error: {e}"));
                        false
                    }
                };
            }
            self.current = None;
        } else {
            // TODO: If there is already a message waiting, process it before
            //       continuing. This would be needed for a pause request.
            //       Something with self.reader.poll_read() maybe?
        }
    }

    fn did_finish_eval(
        &mut self,
        env: &mut Environment,
        context: &LocalContext,
        expr: &SymbolicExpression,
        res: &Result<Value, Error>,
    ) {
        self.get_state().did_finish_eval(env, context, expr, res);
    }

    fn did_complete(&mut self, result: Result<&mut ExecutionResult, String>) {
        match result {
            Ok(result) => {
                self.log("Execution completed.\n");
                if !result.events.is_empty() {
                    self.log("\nEvents emitted:\n");
                    for event in &result.events {
                        self.stdout(format!("{}\n", crate::utils::serialize_event(event)));
                    }
                }

                match &result.result {
                    EvaluationResult::Snippet(snippet) => {
                        self.log("\nReturn value:");
                        self.stdout(format!("{}\n", snippet.result))
                    }
                    EvaluationResult::Contract(contract) => {
                        self.log("\nContract published.");
                        if let Some(value) = &contract.result {
                            self.log("\nReturn value:");
                            self.stdout(format!("{value}\n"))
                        }
                    }
                }
            }
            Err(e) => self.stderr(e),
        }
    }
}

fn type_for_value(value: &Value) -> String {
    match value {
        Value::Int(_) => "int".to_string(),
        Value::UInt(_) => "uint".to_string(),
        Value::Bool(_) => "bool".to_string(),
        Value::Tuple(data) => format!("{}", data.type_signature),
        Value::Principal(_) => "principal".to_string(),
        Value::Optional(opt_data) => format!(
            "{}",
            opt_data
                .type_signature()
                .expect("failed to format optional value")
        ),
        Value::Response(res_data) => format!(
            "{}",
            res_data
                .type_signature()
                .expect("failed to format response value")
        ),
        Value::Sequence(SequenceData::Buffer(_)) => "buff".to_string(),
        Value::Sequence(SequenceData::String(_)) => "string".to_string(),
        Value::Sequence(SequenceData::List(_)) => "list".to_string(),
        Value::CallableContract(callable) => {
            if let Some(trait_id) = &callable.trait_identifier {
                format!("<{trait_id}>")
            } else {
                "principal".to_string()
            }
        }
    }
}
