use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use clarity::vm::ast::ContractAST;
use clarity::vm::functions::define::DefineFunctionsParsed;
use clarity::vm::functions::NativeFunctions::{self, Filter, Fold, Map};
use clarity::vm::{EvalHook, SymbolicExpression};
use clarity_types::types::QualifiedContractIdentifier;

type ExprCoverage = HashMap<u64, u64>;
type ExecutableLines = HashMap<u32, Vec<u64>>;
type ExecutableBranches = HashMap<u64, Vec<(u32, u64)>>;

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct CoverageReport {
    test_name: String,
    coverage: HashMap<QualifiedContractIdentifier, ExprCoverage>,
}

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct CoverageHook {
    pub reports: Vec<CoverageReport>,
    current_test_name: Option<String>,
    contracts_coverage: HashMap<QualifiedContractIdentifier, ExprCoverage>,
}

// LCOV format:
// TN: test name
// SF: source file path
// FN: line number,function name
// FNDA: execution count, function name
// FNF: number functions found
// FNH: number functions hit
// DA: line data: line number, hit count
// BRF: number branches found
// BRH: number branches hit
// BRDA: branch data: line number, expr_id, branch_nb, hit count

impl CoverageHook {
    pub fn new() -> Self {
        Self {
            reports: vec![],
            current_test_name: None,
            contracts_coverage: HashMap::new(),
        }
    }

    pub fn set_current_test_name(&mut self, test_name: String) {
        self.current_test_name = Some(test_name);
    }

    fn clear(&mut self) {
        self.current_test_name = None;
        self.contracts_coverage.clear();
        self.reports.clear();
    }

    pub fn collect_lcov_content(
        &mut self,
        asts: &BTreeMap<QualifiedContractIdentifier, ContractAST>,
        contract_paths: &BTreeMap<String, String>,
    ) -> String {
        let mut file_content = String::new();

        let mut filtered_asts = HashMap::new();
        for (contract_id, ast) in asts.iter() {
            let contract_name = contract_id.name.to_string();
            if contract_paths.contains_key(&contract_name) {
                filtered_asts.insert(
                    contract_name,
                    (
                        contract_id,
                        retrieve_functions(&ast.expressions),
                        retrieve_executable_lines_and_branches(&ast.expressions),
                    ),
                );
            }
        }

        // for consistency in the result, we use a btreemap instead of a hashmap
        let reports_per_tests: BTreeMap<&String, Vec<&CoverageReport>> =
            self.reports
                .iter()
                .fold(BTreeMap::new(), |mut acc, report| {
                    acc.entry(&report.test_name).or_default().push(report);
                    acc
                });

        for (test_name, test_reports) in reports_per_tests.iter() {
            file_content.push_str(&format!("TN:{}\n", **test_name));
            for (contract_name, contract_path) in contract_paths.iter() {
                file_content.push_str(&format!("SF:{contract_path}\n"));

                if let Some((contract_id, functions, executable)) = filtered_asts.get(contract_name)
                {
                    for (function, line_start, _) in functions.iter() {
                        file_content.push_str(&format!("FN:{line_start},{function}\n"));
                    }
                    let (executable_lines, executables_branches) = executable;

                    let mut function_hits = BTreeMap::new();
                    let mut line_execution_counts = BTreeMap::new();

                    let mut branches = HashSet::new();
                    let mut branches_hits = HashSet::new();
                    let mut branch_execution_counts = BTreeMap::new();

                    for report in test_reports {
                        if let Some(coverage) = report.coverage.get(contract_id) {
                            let mut local_function_hits = BTreeSet::new();

                            for (line, expr_ids) in executable_lines.iter() {
                                // in case of code branches on the line
                                // retrieve the expression with the most hits
                                let mut counts = vec![];
                                for id in expr_ids {
                                    if let Some(c) = coverage.get(id) {
                                        counts.push(*c);
                                    }
                                }
                                let count = counts.iter().max().unwrap_or(&0);

                                let total_count = line_execution_counts.entry(line).or_insert(0);
                                *total_count += count;

                                if count == &0 {
                                    continue;
                                }

                                for (function, line_start, line_end) in functions.iter() {
                                    if line >= line_start && line <= line_end {
                                        local_function_hits.insert(function);
                                        // functions hits must have a matching line hit
                                        // if we hit a line inside a function, make sure to count one line hit
                                        if line > line_start {
                                            let hit_count = line_execution_counts.get(&line_start);
                                            if hit_count.is_none() || hit_count == Some(&0) {
                                                line_execution_counts.insert(line_start, 1);
                                            }
                                        }
                                    }
                                }
                            }

                            for (expr_id, args) in executables_branches.iter() {
                                for (i, (line, arg_expr_id)) in args.iter().enumerate() {
                                    let count = coverage.get(arg_expr_id).unwrap_or(&0);

                                    branches.insert(arg_expr_id);
                                    if count > &0 {
                                        branches_hits.insert(arg_expr_id);
                                    }

                                    let total_count = branch_execution_counts
                                        .entry((line, expr_id, i))
                                        .or_insert(0);
                                    *total_count += count;
                                }
                            }

                            for function in local_function_hits.into_iter() {
                                let hits = function_hits.entry(function).or_insert(0);
                                *hits += 1
                            }
                        }
                    }

                    for (function, hits) in function_hits.iter() {
                        file_content.push_str(&format!("FNDA:{hits},{function}\n"));
                    }
                    file_content.push_str(&format!("FNF:{}\n", functions.len()));
                    file_content.push_str(&format!("FNH:{}\n", function_hits.len()));

                    for (line, count) in line_execution_counts.iter() {
                        // the ast can contain elements with a span starting at line 0 that we want to ignore
                        if line > &&0 {
                            file_content.push_str(&format!("DA:{line},{count}\n"));
                        }
                    }

                    file_content.push_str(&format!("BRF:{}\n", branches.len()));
                    file_content.push_str(&format!("BRH:{}\n", branches_hits.len()));

                    for ((line, block_id, branch_nb), count) in branch_execution_counts.iter() {
                        // the ast can contain elements with a span starting at line 0 that we want to ignore
                        if line > &&0 {
                            file_content
                                .push_str(&format!("BRDA:{line},{block_id},{branch_nb},{count}\n"));
                        }
                    }
                }
                file_content.push_str("end_of_record\n");
            }
        }

        self.clear();

        file_content
    }
}

impl EvalHook for CoverageHook {
    fn will_begin_eval(
        &mut self,
        env: &mut clarity::vm::Environment,
        _context: &clarity::vm::LocalContext,
        expr: &SymbolicExpression,
    ) {
        let contract = &env.contract_context.contract_identifier;
        let mut contract_report = self.contracts_coverage.remove(contract).unwrap_or_default();
        report_eval(&mut contract_report, expr);
        self.contracts_coverage
            .insert(contract.clone(), contract_report);
    }

    fn did_finish_eval(
        &mut self,
        _env: &mut clarity::vm::Environment,
        _context: &clarity::vm::LocalContext,
        _expr: &SymbolicExpression,
        _res: &Result<clarity::vm::Value, clarity::vm::errors::Error>,
    ) {
    }

    fn did_complete(&mut self, _result: Result<&mut clarity::vm::ExecutionResult, String>) {
        self.reports.push(CoverageReport {
            test_name: self.current_test_name.clone().unwrap_or_default(),
            coverage: self.contracts_coverage.drain().collect(),
        });
    }
}

fn retrieve_functions(exprs: &[SymbolicExpression]) -> Vec<(String, u32, u32)> {
    let mut functions = vec![];
    for cur_expr in exprs.iter() {
        if let Some(define_expr) = DefineFunctionsParsed::try_parse(cur_expr).ok().flatten() {
            match define_expr {
                DefineFunctionsParsed::PrivateFunction { signature, body: _ }
                | DefineFunctionsParsed::PublicFunction { signature, body: _ }
                | DefineFunctionsParsed::ReadOnlyFunction { signature, body: _ } => {
                    let expr = signature.first().expect("Invalid function signature");
                    let function_name = expr.match_atom().expect("Invalid function signature");

                    functions.push((
                        function_name.to_string(),
                        cur_expr.span.start_line,
                        cur_expr.span.end_line,
                    ));
                }
                _ => {}
            }
            continue;
        }
    }
    functions
}

fn retrieve_executable_lines_and_branches(
    exprs: &[SymbolicExpression],
) -> (ExecutableLines, ExecutableBranches) {
    let mut lines: ExecutableLines = HashMap::new();
    let mut branches: ExecutableBranches = HashMap::new();

    for expression in exprs.iter() {
        let mut frontier = vec![expression];

        while let Some(cur_expr) = frontier.pop() {
            // Only consider functions declaration and body (ignore arguments)
            if let Some(define_expr) = DefineFunctionsParsed::try_parse(cur_expr).ok().flatten() {
                match define_expr {
                    DefineFunctionsParsed::PrivateFunction { signature, body }
                    | DefineFunctionsParsed::PublicFunction { signature, body }
                    | DefineFunctionsParsed::ReadOnlyFunction { signature, body } => {
                        if let Some(function_name) = signature.first() {
                            frontier.push(function_name);
                        }
                        frontier.push(body);
                    }
                    _ => {}
                }

                continue;
            }

            if let Some(children) = cur_expr.match_list() {
                if let Some((func, args)) = try_parse_native_func(children) {
                    // handle codes branches
                    // (if, asserts!, and, or, match)
                    match func {
                        NativeFunctions::If | NativeFunctions::Asserts => {
                            let (_cond, args) = args.split_first().unwrap();
                            branches.insert(
                                cur_expr.id,
                                args.iter()
                                    .map(|a| {
                                        let expr = extract_expr_from_list(a);
                                        (expr.span.start_line, expr.id)
                                    })
                                    .collect(),
                            );
                        }
                        NativeFunctions::And | NativeFunctions::Or => {
                            branches.insert(
                                cur_expr.id,
                                args.iter()
                                    .map(|a| {
                                        let expr = extract_expr_from_list(a);
                                        (expr.span.start_line, expr.id)
                                    })
                                    .collect(),
                            );
                        }
                        NativeFunctions::Match => {
                            // for match ignore bindings children - some, ok, err
                            if args.len() == 4 || args.len() == 5 {
                                let input = args.first().unwrap();
                                let left_branch = args.get(2).unwrap();
                                let right_branch = args.last().unwrap();

                                let match_branches = [left_branch, right_branch];
                                branches.insert(
                                    cur_expr.id,
                                    match_branches
                                        .iter()
                                        .map(|a| {
                                            let expr = extract_expr_from_list(a);
                                            (expr.span.start_line, expr.id)
                                        })
                                        .collect(),
                                );

                                frontier.extend([input]);
                                frontier.extend(match_branches);
                            }
                            continue;
                        }
                        _ => {}
                    };
                };

                // don't count list expressions as a whole, just their children
                frontier.extend(children);
            } else {
                let line = cur_expr.span.start_line;
                if let Some(line) = lines.get_mut(&line) {
                    line.push(cur_expr.id);
                } else {
                    lines.insert(line, vec![cur_expr.id]);
                }
            }
        }
    }
    (lines, branches)
}

fn try_parse_native_func(
    expr: &[SymbolicExpression],
) -> Option<(NativeFunctions, &[SymbolicExpression])> {
    let (name, args) = expr.split_first()?;
    let atom = name.match_atom()?;
    let func = NativeFunctions::lookup_by_name(atom)?;
    Some((func, args))
}

fn report_eval(expr_coverage: &mut ExprCoverage, expr: &SymbolicExpression) {
    if let Some(children) = expr.match_list() {
        if let Some((func, args)) = try_parse_native_func(children) {
            if matches!(func, Fold | Map | Filter) {
                if let Some(iterator_func) = args.first() {
                    report_eval(expr_coverage, iterator_func);
                }
            }
        }
        if let Some(func_expr) = children.first() {
            report_eval(expr_coverage, func_expr);
        }
        return;
    }
    let count = expr_coverage.entry(expr.id).or_insert(0);
    *count += 1;
}

// because list expressions are not considered as evaluated
// this helpers returns evaluatable expr from list
fn extract_expr_from_list(expr: &SymbolicExpression) -> SymbolicExpression {
    if let Some(first) = expr.match_list().and_then(|l| l.first()) {
        return extract_expr_from_list(first);
    }
    expr.to_owned()
}
