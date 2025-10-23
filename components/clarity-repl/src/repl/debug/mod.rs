use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt::Display;

use clarity::vm::ast::build_ast_with_diagnostics;
use clarity::vm::contexts::{Environment, LocalContext};
use clarity::vm::contracts::Contract;
use clarity::vm::diagnostic::Level;
use clarity::vm::errors::Error;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::{Span, SymbolicExpression};
use clarity::vm::{eval, ClarityVersion, ContractName, SymbolicExpressionType};
use clarity_types::types::{QualifiedContractIdentifier, StandardPrincipalData};
use clarity_types::Value;

use crate::repl::diagnostic::output_diagnostic;

#[cfg(not(target_arch = "wasm32"))]
pub mod cli;
#[cfg(feature = "dap")]
pub mod dap;

#[derive(Clone)]
pub struct Source {
    name: QualifiedContractIdentifier,
}

impl Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub struct Breakpoint {
    id: usize,
    data: BreakpointData,
    source: Source,
    span: Option<Span>,
}

impl Display for Breakpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}{}", self.id, self.source, self.data)
    }
}

pub enum BreakpointData {
    Source(SourceBreakpoint),
    Function(FunctionBreakpoint),
    Data(DataBreakpoint),
}

impl Display for BreakpointData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BreakpointData::Source(source) => write!(f, "{source}"),
            BreakpointData::Function(function) => write!(f, "{function}"),
            BreakpointData::Data(data) => write!(f, "{data}"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct SourceBreakpoint {
    line: u32,
    column: Option<u32>,
}

impl Display for SourceBreakpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let column = if let Some(column) = self.column {
            format!(":{column}")
        } else {
            String::new()
        };
        write!(f, ":{}{}", self.line, column)
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum AccessType {
    Read,
    Write,
    ReadWrite,
}

impl Display for AccessType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AccessType::Read => write!(f, "(r)"),
            AccessType::Write => write!(f, "(w)"),
            AccessType::ReadWrite => write!(f, "(rw)"),
        }
    }
}

pub struct DataBreakpoint {
    name: String,
    access_type: AccessType,
}

impl Display for DataBreakpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".{} {}", self.name, self.access_type)
    }
}

pub struct FunctionBreakpoint {
    name: String,
}

impl Display for FunctionBreakpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".{}", self.name)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum State {
    Start,
    Continue,
    StepOver(u64),
    StepIn,
    Finish(u64),
    Finished,
    Break(usize),
    DataBreak(usize, AccessType),
    Pause,
    Quit,
}

struct ExprState {
    id: u64,
    active_breakpoints: Vec<usize>,
}

impl ExprState {
    pub fn new(id: u64) -> ExprState {
        ExprState {
            id,
            active_breakpoints: Vec::new(),
        }
    }
}

impl PartialEq for ExprState {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

pub struct DebugState {
    breakpoints: BTreeMap<usize, Breakpoint>,
    watchpoints: BTreeMap<usize, Breakpoint>,
    break_locations: HashMap<QualifiedContractIdentifier, HashSet<usize>>,
    watch_variables: HashMap<(QualifiedContractIdentifier, String), HashSet<usize>>,
    active_breakpoints: HashSet<usize>,
    state: State,
    stack: Vec<ExprState>,
    unique_id: usize,
    debug_cmd_contract: QualifiedContractIdentifier,
    debug_cmd_source: String,
}

impl DebugState {
    pub fn new(contract_id: &QualifiedContractIdentifier, snippet: &str) -> DebugState {
        DebugState {
            breakpoints: BTreeMap::new(),
            watchpoints: BTreeMap::new(),
            break_locations: HashMap::new(),
            watch_variables: HashMap::new(),
            active_breakpoints: HashSet::new(),
            state: State::Start,
            stack: Vec::new(),
            unique_id: 0,
            debug_cmd_contract: contract_id.clone(),
            debug_cmd_source: snippet.to_string(),
        }
    }

    fn get_unique_id(&mut self) -> usize {
        self.unique_id += 1;
        self.unique_id
    }

    fn continue_execution(&mut self) {
        self.state = State::Continue;
    }

    fn step_over(&mut self, id: u64) {
        self.state = State::StepOver(id);
    }

    fn step_in(&mut self) {
        self.state = State::StepIn;
    }

    fn finish(&mut self) {
        if self.stack.len() >= 2 {
            self.state = State::Finish(self.stack[self.stack.len() - 2].id);
        } else {
            self.state = State::Continue;
        }
    }

    fn quit(&mut self) {
        self.state = State::Quit;
    }

    fn add_breakpoint(&mut self, mut breakpoint: Breakpoint) -> usize {
        let id = self.get_unique_id();
        breakpoint.id = id;

        if let Some(set) = self.break_locations.get_mut(&breakpoint.source.name) {
            set.insert(breakpoint.id);
        } else {
            let mut set = HashSet::new();
            set.insert(id);
            self.break_locations
                .insert(breakpoint.source.name.clone(), set);
        }

        self.breakpoints.insert(id, breakpoint);
        id
    }

    fn delete_all_breakpoints(&mut self) {
        for breakpoint in self.breakpoints.values() {
            let set = self
                .break_locations
                .get_mut(&breakpoint.source.name)
                .unwrap();
            set.remove(&breakpoint.id);
        }
        self.breakpoints.clear();
    }

    fn delete_breakpoint(&mut self, id: usize) -> bool {
        if let Some(breakpoint) = self.breakpoints.remove(&id) {
            let set = self
                .break_locations
                .get_mut(&breakpoint.source.name)
                .unwrap();
            set.remove(&breakpoint.id);
            true
        } else {
            false
        }
    }

    fn add_watchpoint(
        &mut self,
        contract_id: &QualifiedContractIdentifier,
        name: &str,
        access_type: AccessType,
    ) {
        let breakpoint = Breakpoint {
            id: self.get_unique_id(),
            data: BreakpointData::Data(DataBreakpoint {
                name: name.to_string(),
                access_type,
            }),
            source: Source {
                name: contract_id.clone(),
            },
            span: None,
        };
        let name = match &breakpoint.data {
            BreakpointData::Data(data) => data.name.clone(),
            _ => panic!("called add_watchpoint with non-data breakpoint"),
        };

        let key = (breakpoint.source.name.clone(), name);
        if let Some(set) = self.watch_variables.get_mut(&key) {
            set.insert(breakpoint.id);
        } else {
            let mut set = HashSet::new();
            set.insert(breakpoint.id);
            self.watch_variables.insert(key, set);
        }

        self.watchpoints.insert(breakpoint.id, breakpoint);
    }

    fn delete_all_watchpoints(&mut self) {
        for breakpoint in self.watchpoints.values() {
            let name = match &breakpoint.data {
                BreakpointData::Data(data) => data.name.clone(),
                _ => continue,
            };
            let set = self
                .watch_variables
                .get_mut(&(breakpoint.source.name.clone(), name))
                .unwrap();
            set.remove(&breakpoint.id);
        }
        self.watchpoints.clear();
    }

    fn delete_watchpoint(&mut self, id: usize) -> bool {
        if let Some(breakpoint) = self.watchpoints.remove(&id) {
            let name = match breakpoint.data {
                BreakpointData::Data(data) => data.name,
                _ => panic!("called delete_watchpoint with non-data breakpoint"),
            };
            let set = self
                .watch_variables
                .get_mut(&(breakpoint.source.name, name))
                .unwrap();
            set.remove(&breakpoint.id);
            true
        } else {
            false
        }
    }

    fn pause(&mut self) {
        self.state = State::Pause;
    }

    fn evaluate(
        &mut self,
        env: &mut Environment,
        context: &LocalContext,
        snippet: &str,
    ) -> Result<Value, Vec<String>> {
        let contract_id = QualifiedContractIdentifier::transient();
        let lines = snippet.lines();
        let formatted_lines: Vec<String> = lines.map(|l| l.to_string()).collect();
        let (ast, diagnostics, success) = build_ast_with_diagnostics(
            &contract_id,
            snippet,
            &mut (),
            ClarityVersion::default_for_epoch(env.global_context.epoch_id),
            env.global_context.epoch_id,
        );
        if ast.expressions.len() != 1 {
            return Err(vec!["expected a single expression".to_string()]);
        }
        if !success {
            let mut errors = Vec::new();
            for diagnostic in diagnostics.into_iter().filter(|d| d.level == Level::Error) {
                errors.append(&mut output_diagnostic(
                    &diagnostic,
                    "print expression",
                    &formatted_lines,
                ));
            }
            return Err(errors);
        }

        eval(&ast.expressions[0], env, context).map_err(|e| vec![format_err!(e)])
    }

    fn did_hit_source_breakpoint(
        &self,
        contract_id: &QualifiedContractIdentifier,
        span: &Span,
    ) -> Option<usize> {
        if let Some(set) = self.break_locations.get(contract_id) {
            for id in set {
                // Don't break in a subexpression of an expression which has
                // already triggered this breakpoint
                if self.active_breakpoints.contains(id) {
                    continue;
                }

                let Some(breakpoint) = self.breakpoints.get(id) else {
                    panic!("internal error: breakpoint {} not found", id);
                };

                if let Some(break_span) = &breakpoint.span {
                    if break_span.start_line == span.start_line
                        && (break_span.start_column == 0
                            || break_span.start_column == span.start_column)
                    {
                        return Some(breakpoint.id);
                    }
                }
            }
        }
        None
    }

    fn did_hit_data_breakpoint(
        &self,
        contract_id: &QualifiedContractIdentifier,
        expr: &SymbolicExpression,
    ) -> Option<(usize, AccessType)> {
        match &expr.expr {
            SymbolicExpressionType::List(list) => {
                // Check if we hit a data breakpoint
                if let Some((function_name, args)) = list.split_first() {
                    if let Some(function_name) = function_name.match_atom() {
                        if let Some(native_function) = NativeFunctions::lookup_by_name_at_version(
                            function_name,
                            &ClarityVersion::latest(),
                        ) {
                            use clarity::vm::functions::NativeFunctions::*;
                            if let Some((name, access_type)) = match native_function {
                                FetchVar => Some((
                                    args[0].match_atom().unwrap().to_string(),
                                    AccessType::Read,
                                )),
                                SetVar => Some((
                                    args[0].match_atom().unwrap().to_string(),
                                    AccessType::Write,
                                )),
                                FetchEntry => Some((
                                    args[0].match_atom().unwrap().to_string(),
                                    AccessType::Read,
                                )),
                                SetEntry => Some((
                                    args[0].match_atom().unwrap().to_string(),
                                    AccessType::Write,
                                )),
                                InsertEntry => Some((
                                    args[0].match_atom().unwrap().to_string(),
                                    AccessType::Write,
                                )),
                                DeleteEntry => Some((
                                    args[0].match_atom().unwrap().to_string(),
                                    AccessType::Write,
                                )),
                                _ => None,
                            } {
                                let key = (contract_id.clone(), name);
                                if let Some(set) = self.watch_variables.get(&key) {
                                    for id in set {
                                        let Some(watchpoint) = self.watchpoints.get(id) else {
                                            panic!("internal error: watchpoint {} not found", id);
                                        };

                                        if let BreakpointData::Data(data) = &watchpoint.data {
                                            match (data.access_type, access_type) {
                                                (AccessType::Read, AccessType::Read)
                                                | (AccessType::Write, AccessType::Write)
                                                | (AccessType::ReadWrite, AccessType::Read)
                                                | (AccessType::ReadWrite, AccessType::Write) => {
                                                    return Some((watchpoint.id, access_type))
                                                }
                                                _ => (),
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    // Returns a bool which indicates if execution should resume (true) or if
    // it should wait for input (false).
    fn will_begin_eval(
        &mut self,
        env: &mut Environment,
        _context: &LocalContext,
        expr: &SymbolicExpression,
    ) -> bool {
        self.stack.push(ExprState::new(expr.id));

        // If user quit debug session, we can't stop executing, but don't do anything else.
        if self.state == State::Quit {
            return true;
        }

        // Check if we have hit a source breakpoint
        if let Some(breakpoint) =
            self.did_hit_source_breakpoint(&env.contract_context.contract_identifier, &expr.span)
        {
            self.active_breakpoints.insert(breakpoint);
            let top = self.stack.last_mut().unwrap();
            top.active_breakpoints.push(breakpoint);

            self.state = State::Break(breakpoint);
        }

        // Always skip over non-list expressions (values).
        match expr.expr {
            SymbolicExpressionType::List(_) => (),
            _ => return true,
        };

        if let Some((watchpoint, access_type)) =
            self.did_hit_data_breakpoint(&env.contract_context.contract_identifier, expr)
        {
            self.state = State::DataBreak(watchpoint, access_type);
        }

        match self.state {
            State::Continue | State::Quit | State::Finish(_) => return true,
            State::StepOver(step_over_id) => {
                if self.stack.iter().any(|state| state.id == step_over_id) {
                    // We're still inside the expression which should be stepped over,
                    // so return to execution.
                    return true;
                }
            }
            State::Start
            | State::StepIn
            | State::Break(_)
            | State::DataBreak(..)
            | State::Pause
            | State::Finished => (),
        };

        false
    }

    // Returns a bool which indicates if the result should be printed (finish)
    fn did_finish_eval(
        &mut self,
        _env: &mut Environment,
        _context: &LocalContext,
        expr: &SymbolicExpression,
        _res: &Result<Value, Error>,
    ) -> bool {
        let state = self.stack.pop().unwrap();
        assert_eq!(state.id, expr.id);

        // Remove any active breakpoints for this expression
        for breakpoint in state.active_breakpoints {
            self.active_breakpoints.remove(&breakpoint);
        }

        // Only print the returned value if this resolves a finish command
        match self.state {
            State::Finish(finish_id) if finish_id == state.id => {
                self.state = State::Finished;
                true
            }
            _ => false,
        }
    }
}

pub fn extract_watch_variable<'a>(
    env: &mut Environment,
    expr: &'a str,
    default_sender: Option<&StandardPrincipalData>,
) -> Result<(Contract, &'a str), String> {
    // Syntax could be:
    // - principal.contract.name
    // - .contract.name
    // - name
    let parts: Vec<&str> = expr.split('.').collect();
    let (contract_id, name) = match parts.len() {
        1 => {
            if default_sender.is_some() {
                return Err("must use qualified name".to_string());
            } else {
                (env.contract_context.contract_identifier.clone(), parts[0])
            }
        }
        3 => {
            let contract_id = if parts[0].is_empty() {
                if let Some(sender) = default_sender {
                    QualifiedContractIdentifier::new(sender.clone(), ContractName::from(parts[1]))
                } else {
                    QualifiedContractIdentifier::new(
                        env.contract_context.contract_identifier.issuer.clone(),
                        ContractName::from(parts[1]),
                    )
                }
            } else {
                match QualifiedContractIdentifier::parse(expr.rsplit_once('.').unwrap().0) {
                    Ok(contract_identifier) => contract_identifier,
                    Err(e) => {
                        return Err(format!(
                            "unable to parse watchpoint contract identifier: {e}"
                        ));
                    }
                }
            };
            (contract_id, parts[2])
        }
        _ => return Err("invalid watchpoint format".to_string()),
    };

    let contract = if env.global_context.database.has_contract(&contract_id) {
        env.global_context
            .database
            .get_contract(&contract_id)
            .map_err(|e| e.to_string())?
    } else {
        return Err(format!("{contract_id} does not exist"));
    };

    if !contract.contract_context.meta_data_var.contains_key(name)
        && !contract.contract_context.meta_data_map.contains_key(name)
    {
        return Err(format!("no such variable: {contract_id}.{name}"));
    }

    Ok((contract, name))
}
