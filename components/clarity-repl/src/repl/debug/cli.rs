use clarity::vm::contexts::{Environment, LocalContext};
use clarity::vm::errors::Error;
use clarity::vm::representations::Span;
use clarity::vm::{ContractName, EvalHook, SymbolicExpression};
use clarity_types::types::QualifiedContractIdentifier;
use clarity_types::Value;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use super::{
    AccessType, Breakpoint, BreakpointData, DebugState, FunctionBreakpoint, Source,
    SourceBreakpoint, State,
};
use crate::repl::debug::extract_watch_variable;

const HISTORY_FILE: Option<&'static str> = option_env!("CLARITY_DEBUG_HISTORY_FILE");

pub struct CLIDebugger {
    editor: DefaultEditor,
    state: DebugState,
}

impl CLIDebugger {
    pub fn new(contract_id: &QualifiedContractIdentifier, snippet: &str) -> Self {
        let mut editor = DefaultEditor::new().expect("Failed to initialize cli");
        editor
            .load_history(HISTORY_FILE.unwrap_or(".debug_history"))
            .ok();

        Self {
            editor,
            state: DebugState::new(contract_id, snippet),
        }
    }

    fn prompt(&mut self, env: &mut Environment, context: &LocalContext, expr: &SymbolicExpression) {
        let prompt = black!("(debug) ");
        loop {
            let readline = self.editor.readline(&prompt);
            let resume = match readline {
                Ok(mut command) => {
                    if command.is_empty() {
                        if let Some(prev) = self.editor.history().iter().last() {
                            command.clone_from(prev)
                        }
                    }
                    let _ = self.editor.add_history_entry(&command);
                    self.handle_command(&command, env, context, expr)
                }
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                    println!("Use \"q\" or \"quit\" to exit debug mode");
                    false
                }
                Err(err) => {
                    println!("Error: {err:?}");
                    false
                }
            };

            if resume {
                break;
            }
        }
        self.editor
            .save_history(HISTORY_FILE.unwrap_or(".debug_history"))
            .unwrap();
    }

    fn print_source_from_str(&self, contract_id: &str, contract_source: &str, span: Span) {
        if span.start_line != 0 {
            println!(
                "{}:{}:{}",
                blue!(contract_id),
                span.start_line,
                span.start_column
            );
            let lines: Vec<&str> = contract_source.lines().collect();
            let first_line = (span.start_line - 1).saturating_sub(3) as usize;
            let last_line = std::cmp::min(lines.len(), span.start_line as usize + 3);
            #[allow(clippy::needless_range_loop)]
            for line in first_line..last_line {
                if line == (span.start_line as usize - 1) {
                    print!("{}", blue!("-> "));
                } else {
                    print!("   ");
                }
                println!("{} {}", black!("{: <6}", line + 1), lines[line]);
                if line == (span.start_line as usize - 1) {
                    println!(
                        "{}",
                        blue!("          {: <1$}^", "", (span.start_column - 1) as usize)
                    );
                }
            }
        } else {
            println!("{}", yellow!("source information unknown"));
        }
    }

    // Print the source of the current expr (if it has a valid span).
    fn print_source(&mut self, env: &mut Environment, expr: &SymbolicExpression) {
        let contract_id = &env.contract_context.contract_identifier;
        if contract_id == &self.state.debug_cmd_contract {
            self.print_source_from_str(
                "<command>",
                &self.state.debug_cmd_source,
                expr.span.clone(),
            );
        } else {
            match env.global_context.database.get_contract_src(contract_id) {
                Some(contract_source) => {
                    self.print_source_from_str(
                        &contract_id.to_string(),
                        &contract_source,
                        expr.span.clone(),
                    );
                }
                None => {
                    println!("{}", yellow!("source not found"));
                    println!(
                        "{}:{}:{}",
                        contract_id, expr.span.start_line, expr.span.start_column
                    );
                }
            }
        }
    }

    // Returns a bool which indicates if execution should resume (true) or if
    // it should wait for input (false).
    fn handle_command(
        &mut self,
        command: &str,
        env: &mut Environment,
        context: &LocalContext,
        expr: &SymbolicExpression,
    ) -> bool {
        let (cmd, args) = match command.split_once(' ') {
            None => (command, ""),
            Some((cmd, args)) => (cmd, args),
        };
        match cmd {
            "h" | "help" => {
                print_help(args);
                false
            }
            "r" | "run" | "c" | "continue" => {
                self.state.continue_execution();
                true
            }
            "n" | "next" => {
                self.state.step_over(expr.id);
                true
            }
            "s" | "step" => {
                self.state.step_in();
                true
            }
            "f" | "finish" => {
                self.state.finish();
                true
            }
            "b" | "break" => {
                self.break_command(args, env);
                false
            }
            "w" | "watch" => {
                self.watch_command(args, env, AccessType::Write);
                false
            }
            "rw" | "rwatch" => {
                self.watch_command(args, env, AccessType::Read);
                false
            }
            "aw" | "awatch" => {
                self.watch_command(args, env, AccessType::ReadWrite);
                false
            }
            "p" | "print" => {
                match self.state.evaluate(env, context, args) {
                    Ok(value) => {
                        println!("{value}");
                    }
                    Err(errors) => {
                        for e in errors {
                            println!("{e}");
                        }
                    }
                }
                false
            }
            "q" | "quit" => {
                self.state.quit();
                true
            }
            _ => {
                println!("Unknown command");
                print_help("");
                false
            }
        }
    }

    fn break_command(&mut self, args: &str, env: &mut Environment) {
        if args.is_empty() {
            println!("{}", format_err!("invalid break command"));
            print_help_breakpoint();
            return;
        }

        let arg_list: Vec<&str> = args.split_ascii_whitespace().collect();
        match arg_list[0] {
            "l" | "list" => {
                if self.state.breakpoints.is_empty() {
                    println!("No breakpoints set.")
                } else {
                    for breakpoint in self.state.breakpoints.values() {
                        println!("{breakpoint}");
                    }
                }
            }
            "del" | "delete" => {
                // if no argument is passed, delete all watchpoints
                if arg_list.len() == 1 {
                    self.state.delete_all_breakpoints();
                } else {
                    let Ok(id) = arg_list[1].parse::<usize>() else {
                        println!("{}", format_err!("unable to parse breakpoint identifier"));
                        return;
                    };
                    if self.state.delete_breakpoint(id) {
                        println!("breakpoint deleted");
                    } else {
                        println!(
                            "{} '{id}' is not a currently valid breakpoint id",
                            red!("error:"),
                        );
                    }
                }
            }
            _ => {
                if arg_list.len() != 1 {
                    println!("{}", format_err!("invalid break command"));
                    print_help_breakpoint();
                    return;
                }

                if args.contains(':') {
                    // Handle source breakpoints
                    // - contract:line:column
                    // - contract:line
                    // - :line
                    let parts: Vec<&str> = args.split(':').collect();
                    if parts.len() < 2 || parts.len() > 3 {
                        println!("{}", format_err!("invalid breakpoint format"));
                        print_help_breakpoint();
                        return;
                    }

                    let contract_id = if parts[0].is_empty() {
                        env.contract_context.contract_identifier.clone()
                    } else {
                        let contract_parts: Vec<&str> = parts[0].split('.').collect();
                        if contract_parts.len() != 2 {
                            println!("{}", format_err!("invalid breakpoint format"));
                            print_help_breakpoint();
                            return;
                        }
                        if contract_parts[0].is_empty() {
                            QualifiedContractIdentifier::new(
                                env.contract_context.contract_identifier.issuer.clone(),
                                ContractName::from(contract_parts[1]),
                            )
                        } else {
                            match QualifiedContractIdentifier::parse(parts[0]) {
                                Ok(contract_identifier) => contract_identifier,
                                Err(e) => {
                                    println!(
                                        "{} unable to parse breakpoint contract identifier: {}",
                                        red!("error:"),
                                        e
                                    );
                                    print_help_breakpoint();
                                    return;
                                }
                            }
                        }
                    };

                    let Ok(line) = parts[1].parse::<u32>() else {
                        println!("{}", format_err!("invalid breakpoint format"),);
                        print_help_breakpoint();
                        return;
                    };

                    let column = if parts.len() == 3 {
                        match parts[2].parse::<u32>() {
                            Ok(column) => column,
                            Err(_) => {
                                println!("{}", format_err!("invalid breakpoint format"),);
                                print_help_breakpoint();
                                return;
                            }
                        }
                    } else {
                        0
                    };

                    self.state.add_breakpoint(Breakpoint {
                        id: 0,
                        data: BreakpointData::Source(SourceBreakpoint {
                            line,
                            column: if column == 0 { None } else { Some(column) },
                        }),
                        source: Source { name: contract_id },
                        span: Some(Span {
                            start_line: line,
                            start_column: column,
                            end_line: line,
                            end_column: column,
                        }),
                    });
                } else {
                    // Handle function breakpoints
                    // - principal.contract.function
                    // - .contract.function
                    // - function
                    let parts: Vec<&str> = args.split('.').collect();
                    let (contract_id, function_name) = match parts.len() {
                        1 => (env.contract_context.contract_identifier.clone(), parts[0]),
                        3 => {
                            let contract_id = if parts[0].is_empty() {
                                QualifiedContractIdentifier::new(
                                    env.contract_context.contract_identifier.issuer.clone(),
                                    ContractName::from(parts[1]),
                                )
                            } else {
                                match QualifiedContractIdentifier::parse(
                                    args.rsplit_once('.').unwrap().0,
                                ) {
                                    Ok(contract_identifier) => contract_identifier,
                                    Err(e) => {
                                        println!(
                                            "{} unable to parse breakpoint contract identifier: {}",
                                            red!("error:"),
                                            e
                                        );
                                        print_help_breakpoint();
                                        return;
                                    }
                                }
                            };
                            (contract_id, parts[2])
                        }
                        _ => {
                            println!("{}", format_err!("invalid breakpoint format"),);
                            print_help_breakpoint();
                            return;
                        }
                    };

                    let contract = match env.global_context.database.get_contract(&contract_id) {
                        Ok(contract) => contract,
                        Err(e) => {
                            println!("{}", format_err!(e));
                            return;
                        }
                    };
                    let function = match contract.contract_context.lookup_function(function_name) {
                        None => {
                            println!("{}", format_err!("no such function"));
                            return;
                        }
                        Some(function) => function,
                    };

                    self.state.add_breakpoint(Breakpoint {
                        id: 0,
                        data: BreakpointData::Function(FunctionBreakpoint {
                            name: function_name.to_string(),
                        }),
                        source: Source { name: contract_id },
                        span: Some(function.get_span()),
                    });
                }
            }
        }
    }

    fn watch_command(&mut self, args: &str, env: &mut Environment, access_type: AccessType) {
        if args.is_empty() {
            println!("{}", format_err!("invalid watch command"));
            print_help_watchpoint();
            return;
        }

        let arg_list: Vec<&str> = args.split_ascii_whitespace().collect();
        match arg_list[0] {
            "l" | "list" => {
                if self.state.watchpoints.is_empty() {
                    println!("No watchpoints set.")
                } else {
                    for watchpoint in self.state.watchpoints.values() {
                        println!("{watchpoint}");
                    }
                }
            }
            "del" | "delete" => {
                // if no argument is passed, delete all watchpoints
                if arg_list.len() == 1 {
                    self.state.delete_all_watchpoints();
                } else {
                    let Ok(id) = arg_list[1].parse::<usize>() else {
                        println!("{}", format_err!("unable to parse watchpoint identifier"));
                        return;
                    };
                    if self.state.delete_watchpoint(id) {
                        println!("watchpoint deleted");
                    } else {
                        println!(
                            "{} '{id}' is not a currently valid watchpoint id",
                            red!("error:"),
                        );
                    }
                }
            }
            _ => {
                if arg_list.len() != 1 {
                    println!("{}", format_err!("invalid watch command"));
                    print_help_watchpoint();
                    return;
                }

                match extract_watch_variable(env, args, None) {
                    Ok((contract, name)) => self.state.add_watchpoint(
                        &contract.contract_context.contract_identifier,
                        name,
                        access_type,
                    ),
                    Err(e) => {
                        println!("{}", format_err!(e));
                        print_help_watchpoint();
                    }
                };
            }
        }
    }
}

impl EvalHook for CLIDebugger {
    fn will_begin_eval(
        &mut self,
        env: &mut Environment,
        context: &LocalContext,
        expr: &SymbolicExpression,
    ) {
        if !self.state.will_begin_eval(env, context, expr) {
            match self.state.state {
                State::Break(id) => println!("{} hit breakpoint {}", black!("*"), id),
                State::DataBreak(id, access_type) => println!(
                    "{} hit watchpoint {} ({})",
                    black!("*"),
                    id,
                    if access_type == AccessType::Read {
                        "read"
                    } else {
                        "write"
                    }
                ),
                _ => (),
            }
            self.print_source(env, expr);
            self.prompt(env, context, expr);
        }
    }

    fn did_finish_eval(
        &mut self,
        env: &mut Environment,
        context: &LocalContext,
        expr: &SymbolicExpression,
        res: &Result<Value, Error>,
    ) {
        if self.state.did_finish_eval(env, context, expr, res) {
            match res {
                Ok(value) => println!("{}: {}", green!("Return value"), black!("{value}")),
                Err(e) => println!("{}", format_err!(e)),
            }
        }
    }

    fn did_complete(
        &mut self,
        _result: core::result::Result<&mut clarity::vm::ExecutionResult, String>,
    ) {
    }
}

fn print_help(args: &str) {
    match args {
        "b" | "breakpoint" => print_help_breakpoint(),
        "w" | "watch" | "aw" | "awatch" | "rw" | "rwatch" => print_help_watchpoint(),
        _ => print_help_main(),
    }
}

fn print_help_main() {
    println!(
        r#"Debugger commands:
  aw | awatch       -- Read/write watchpoint, see `help watch' for details)
  b  | breakpoint   -- Commands for operating on breakpoints (see 'help b' for details)
  c  | continue     -- Continue execution until next breakpoint or completion
  f  | finish       -- Continue execution until returning from the current expression
  n  | next         -- Single step, stepping over sub-expressions
  p  | print <expr> -- Evaluate an expression and print the result
  q  | quit         -- Quit the debugger
  r  | run          -- Begin execution
  rw | rwatch       -- Read watchpoint, see `help watch' for details)
  s  | step         -- Single step, stepping into sub-expressions
  w  | watch        -- Commands for operating on watchpoints (see 'help w' for details)
"#
    );
}

fn print_help_breakpoint() {
    println!(
        r#"Set a breakpoint using 'b' or 'break' and one of these formats
  b <principal?>.<contract>:<linenum>:<colnum>
    SP000000000000000000002Q6VF78.bns:604:9
        Break at line 604, column 9 of the bns contract deployed by
          SP000000000000000000002Q6VF78

  b <principal?>.<contract>:<linenum>
    .my-contract:193
        Break at line 193 of the my-contract contract deployed by the current
          tx-sender

  b :<linenum>:<colnum>
    :12:4
        Break at line 12, column 4 of the current contract

  b :<linenum>
    :12
        Break at line 12 of the current contract

  b <principal>.<contract>.<function>
    SP000000000000000000002Q6VF78.bns.name-preorder
        Break at the function name-preorder from the bns contract deployed by
          SP000000000000000000002Q6VF78

  b .<contract>.<function>
    .foo.do-something
        Break at the function 'do-something from the 'foo' contract deployed by
          the current principal

  b <function>
    take-action
        Break at the function 'take-action' current contract

List current breakpoints
  b list
  b l

Delete a breakpoint using its identifier
  b delete <breakpoint-id>
  b del <breakpoint-id>

Delete all breakpoints
  b delete
  b del
"#
    );
}

fn print_help_watchpoint() {
    println!(
        r#"Set a watchpoint using 'w' or 'watch' and one of these formats
  w <principal>.<contract>.<name>
    SP000000000000000000002Q6VF78.bns.owner-name
        Break on writes to the map 'owner-name' from the 'bns' contract
          deployed by SP000000000000000000002Q6VF78
  w .<contract>.<name>
    .foo.bar
        Break on writes to the variable 'bar' from the 'foo' contract
          deployed by the current principal
  w <name>
    something
        Watch the variable 'something' from the current contract

Default watchpoints break when the variable or map is written. Using the same
formats, the command 'rwatch' sets a read watchpoint to break when the variable
or map is read, and 'awatch' sets a read/write watchpoint to break on read or
write.

List current watchpoints
  w list
  w l

Delete a watchpoint using its identifier
  w delete <watchpoint-id>
  w del <watchpoint-id>

Delete all watchpoints
  w delete
  w del
"#
    );
}
