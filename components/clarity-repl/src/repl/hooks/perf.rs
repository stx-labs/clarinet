use std::fmt::Display;
use std::io::Write;

use clarity::vm::contexts::{Environment, LocalContext};
use clarity::vm::costs::ExecutionCost;
use clarity::vm::errors::Error;
use clarity::vm::{EvalHook, SymbolicExpression, SymbolicExpressionType};
use clarity_types::types::{QualifiedContractIdentifier, Value};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CostField {
    Runtime,
    ReadLength,
    ReadCount,
    WriteLength,
    WriteCount,
}

impl From<&str> for CostField {
    fn from(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "runtime" => CostField::Runtime,
            "read_length" | "readlength" => CostField::ReadLength,
            "read_count" | "readcount" => CostField::ReadCount,
            "write_length" | "writelength" => CostField::WriteLength,
            "write_count" | "writecount" => CostField::WriteCount,
            _ => {
                eprintln!("Using 'runtime' cost field. {} is not one of runtime, read_length, read_count, write_length, write_count", s);
                CostField::Runtime
            }
        }
    }
}

impl CostField {
    pub fn get_value(&self, cost: &ExecutionCost) -> u64 {
        match self {
            CostField::Runtime => cost.runtime,
            CostField::ReadLength => cost.read_length,
            CostField::ReadCount => cost.read_count,
            CostField::WriteLength => cost.write_length,
            CostField::WriteCount => cost.write_count,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            CostField::Runtime => "runtime",
            CostField::ReadLength => "read_length",
            CostField::ReadCount => "read_count",
            CostField::WriteLength => "write_length",
            CostField::WriteCount => "write_count",
        }
    }
}

struct StackEntry {
    contract: QualifiedContractIdentifier,
    function: String,
    expr_id: u64,
    line: u32,
    column: u32,
    cost_before: ExecutionCost,
    cost_descendents: ExecutionCost,
}

impl Display for StackEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}{}:{}:{}",
            self.contract, self.function, self.line, self.column,
        )
    }
}

/// A writer that can also be read from, used for WASM mode
#[cfg(target_arch = "wasm32")]
struct ReadableWriter {
    buffer: Vec<u8>,
}

#[cfg(target_arch = "wasm32")]
impl ReadableWriter {
    fn new() -> Self {
        Self { buffer: Vec::new() }
    }

    /// Get the written data as a string
    pub fn get_data(&self) -> Option<String> {
        String::from_utf8(self.buffer.clone()).ok()
    }

    /// Clear the buffer
    pub fn clear_buffer(&mut self) {
        self.buffer.clear();
    }
}

#[cfg(target_arch = "wasm32")]
impl Write for ReadableWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.buffer.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

enum PerfWriter {
    #[cfg(not(target_arch = "wasm32"))]
    File(std::fs::File),
    #[cfg(target_arch = "wasm32")]
    Readable(ReadableWriter),
}

impl Write for PerfWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            #[cfg(not(target_arch = "wasm32"))]
            PerfWriter::File(file) => file.write(buf),
            #[cfg(target_arch = "wasm32")]
            PerfWriter::Readable(writer) => writer.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            #[cfg(not(target_arch = "wasm32"))]
            PerfWriter::File(file) => file.flush(),
            #[cfg(target_arch = "wasm32")]
            PerfWriter::Readable(writer) => writer.flush(),
        }
    }
}

pub struct PerfHook {
    /// Writer for outputting performance metrics
    writer: PerfWriter,
    /// Stack of expressions
    expr_stack: Vec<StackEntry>,
    /// Specific cost field to track
    cost_field: CostField,
    /// Sum of all expression costs tracked
    total_expression_costs: ExecutionCost,
    /// Contract identifier for overhead tracking
    contract_identifier: Option<QualifiedContractIdentifier>,
    /// Collected performance data to rewrite with overhead
    collected_data: Vec<(String, u64)>,
}

static OVERHEAD_HEADER: &str = "contract-loading-cost";

impl Clone for PerfHook {
    fn clone(&self) -> Self {
        PerfHook::new(self.cost_field)
    }
}

impl PerfHook {
    pub fn new(cost_field: CostField) -> PerfHook {
        Self::new_with_filename(cost_field, "perf.data")
    }

    pub fn new_with_filename(cost_field: CostField, _filename: &str) -> PerfHook {
        let writer = {
            #[cfg(target_arch = "wasm32")]
            {
                PerfWriter::Readable(ReadableWriter::new())
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                PerfWriter::File(
                    std::fs::File::create(_filename).expect("Failed to create perf output file"),
                )
            }
        };
        PerfHook {
            writer,
            expr_stack: Vec::new(),
            cost_field,
            total_expression_costs: ExecutionCost::ZERO,
            contract_identifier: None,
            collected_data: Vec::new(),
        }
    }

    /// Get the performance data buffer (WASM mode) or None (non-WASM mode)
    pub fn get_buffer_data(&self) -> Option<String> {
        match &self.writer {
            #[cfg(target_arch = "wasm32")]
            PerfWriter::Readable(writer) => writer.get_data(),
            #[cfg(not(target_arch = "wasm32"))]
            PerfWriter::File(_) => None,
        }
    }

    /// Clear the performance data buffer (WASM mode only)
    pub fn clear_buffer(&mut self) {
        #[cfg(target_arch = "wasm32")]
        {
            let PerfWriter::Readable(ref mut writer) = &mut self.writer;
            writer.clear_buffer();
        }
    }
}

impl Default for PerfHook {
    fn default() -> Self {
        Self::new(CostField::Runtime)
    }
}

impl EvalHook for PerfHook {
    fn will_begin_eval(
        &mut self,
        env: &mut Environment,
        _context: &LocalContext,
        expr: &SymbolicExpression,
    ) {
        let contract = &env.contract_context.contract_identifier;

        // Store contract identifier for overhead tracking
        if self.contract_identifier.is_none() {
            self.contract_identifier = Some(contract.clone());
        }

        // Find the current function name in the call stack
        let call_stack = env.call_stack.make_stack_trace();
        let mut function = String::new();
        for identifier in call_stack.iter().rev() {
            let s = identifier.to_string();
            if s.starts_with("_native_") {
                continue;
            }

            if let Some(f) = s.strip_prefix(&format!("{contract}:")) {
                function = format!(":{f}");
            } else {
                break;
            }
        }

        // If the expression is a list, extract the function name and append it
        // to the function string
        if let SymbolicExpressionType::List(list) = &expr.expr {
            if let Some((function_name, _args)) = list.split_first() {
                if let Some(function_name) = function_name.match_atom() {
                    function = format!("{function}:{function_name}");
                }
            }
        }

        // Record the cost before evaluating this expression
        let cost_before = env.global_context.cost_track.get_total();

        let line = expr.span.start_line;
        let column = expr.span.start_column;

        self.expr_stack.push(StackEntry {
            contract: contract.clone(),
            function,
            expr_id: expr.id,
            line,
            column,
            cost_before,
            cost_descendents: ExecutionCost::ZERO,
        });
    }

    fn did_finish_eval(
        &mut self,
        env: &mut Environment,
        _context: &LocalContext,
        expr: &SymbolicExpression,
        _res: &Result<Value, Error>,
    ) {
        // Get the full call stack as a string
        let call_stack = self
            .expr_stack
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(";");

        // Pop the last entry from the expression stack
        let entry = self.expr_stack.pop().expect("expr stack underflow");
        assert_eq!(
            entry.expr_id, expr.id,
            "Expression ID mismatch: expected {}, got {}",
            entry.expr_id, expr.id
        );

        // Get the current cost so we can compute the overhead
        let mut cost = env.global_context.cost_track.get_total();

        // Subtract the cost before evaluation and the cost of descendents
        cost.sub(&entry.cost_before)
            .expect("cost diff calculation failed");
        cost.sub(&entry.cost_descendents)
            .expect("cost diff calculation failed");

        self.total_expression_costs
            .add(&cost)
            .expect("cost addition failed");

        let cost_value = self.cost_field.get_value(&cost);
        self.collected_data.push((call_stack, cost_value));

        if let Some(parent) = self.expr_stack.last_mut() {
            parent
                .cost_descendents
                .add(&cost)
                .expect("cost addition failed");
            parent
                .cost_descendents
                .add(&entry.cost_descendents)
                .expect("cost addition failed");
        }
    }

    fn did_complete(
        &mut self,
        result: core::result::Result<&mut clarity::vm::ExecutionResult, String>,
    ) {
        if let Ok(execution_result) = result {
            if let Some(total_cost) = &execution_result.cost {
                let total_runtime = total_cost.total.runtime;
                let expression_runtime = self.total_expression_costs.runtime;
                let overhead_runtime = total_runtime.saturating_sub(expression_runtime);

                // Prepare writer based on target architecture
                #[cfg(target_arch = "wasm32")]
                {
                    self.clear_buffer();
                }

                #[cfg(not(target_arch = "wasm32"))]
                let mut file =
                    std::fs::File::create("perf.data").expect("Failed to create perf output file");

                // Get a mutable reference to the appropriate writer
                let writer = {
                    #[cfg(target_arch = "wasm32")]
                    {
                        &mut self.writer
                    }
                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        &mut file
                    }
                };

                // Write performance data (target-agnostic)
                if overhead_runtime > 0 {
                    if let Some(ref contract) = self.contract_identifier {
                        writeln!(
                            writer,
                            "{}:{} {}",
                            contract, OVERHEAD_HEADER, overhead_runtime
                        )
                        .expect("Failed to write overhead to perf output");
                    }
                }

                for (call_stack, cost) in &self.collected_data {
                    let final_call_stack =
                        if overhead_runtime > 0 && self.contract_identifier.is_some() {
                            format!(
                                "{};{}",
                                self.contract_identifier.as_ref().unwrap(),
                                call_stack
                            )
                        } else {
                            call_stack.clone()
                        };
                    writeln!(writer, "{} {}", final_call_stack, cost)
                        .expect("Failed to write to perf output");
                }
            }
        }
    }
}
