// Static cost analysis for Clarity contracts

use std::collections::{BTreeMap, HashMap, HashSet};

use clarity::vm::ast::build_ast;
use clarity::vm::contexts::Environment;
use clarity::vm::costs::cost_functions::ClarityCostFunction;
use clarity::vm::costs::ExecutionCost;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::representations::{ClarityName, SymbolicExpression, SymbolicExpressionType};
use clarity::vm::types::{
    parse_name_type_pairs, QualifiedContractIdentifier, TupleTypeSignature, TypeSignature,
    TypeSignatureExt,
};
use clarity::vm::variables::lookup_reserved_variable;
use clarity::vm::{ClarityVersion, LocalContext, Value};
use clarity_types::errors::analysis::SyntaxBindingErrorType;
use clarity_types::types::TraitIdentifier;
use stacks_common::types::StacksEpochId;

use super::cost_functions::ClarityCostFunctionExt;
use super::error::StaticCostError;
use super::{
    calculate_function_cost, calculate_function_cost_from_native_function,
    calculate_total_cost_with_branching, calculate_value_cost, TraitCount, TraitCountCollector,
    TraitCountContext, TraitCountPropagator, TraitCountVisitor,
};
// TODO:
// contract-call? - get source from database
// unwrap evaluates both branches (https://github.com/clarity-lang/reference/issues/59)

const FUNCTION_DEFINITION_KEYWORDS: &[&str] =
    &["define-public", "define-private", "define-read-only"];

pub(crate) fn is_function_definition(function_name: &str) -> bool {
    FUNCTION_DEFINITION_KEYWORDS.contains(&function_name)
}

#[derive(Debug, Clone)]
pub enum CostExprNode {
    // Native Clarity functions
    NativeFunction(NativeFunctions),
    // Non-native expressions
    AtomValue(Value),
    Atom(ClarityName),
    FieldIdentifier(TraitIdentifier),
    TraitReference(ClarityName),
    // User function arguments
    UserArgument(ClarityName, TypeSignature), // (argument_name, argument_type)
    // User-defined functions
    UserFunction(ClarityName),
    // A list expression whose head is itself a list (i.e. not a named function call)
    NestedExpression,
}

#[derive(Debug, Clone)]
pub struct CostAnalysisNode {
    pub expr: CostExprNode,
    pub cost: StaticCost,
    pub children: Vec<CostAnalysisNode>,
}

impl CostAnalysisNode {
    pub fn new(expr: CostExprNode, cost: StaticCost, children: Vec<CostAnalysisNode>) -> Self {
        Self {
            expr,
            cost,
            children,
        }
    }

    pub fn leaf(expr: CostExprNode, cost: StaticCost) -> Self {
        Self {
            expr,
            cost,
            children: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub struct StaticCost {
    pub min: ExecutionCost,
    pub max: ExecutionCost,
}

impl StaticCost {
    pub const ZERO: StaticCost = StaticCost {
        min: ExecutionCost::ZERO,
        max: ExecutionCost::ZERO,
    };
}

#[derive(Debug, Clone, Default)]
pub struct UserArgumentsContext {
    /// Map from argument name to argument type
    pub arguments: HashMap<ClarityName, TypeSignature>,
    /// Map from map-name to (key_type, value_type), pre-populated from define-map declarations
    pub map_types: HashMap<ClarityName, (TypeSignature, TypeSignature)>,
    /// Map from data-var name to its value type, pre-populated from define-data-var declarations
    pub data_var_types: HashMap<ClarityName, TypeSignature>,
}

impl UserArgumentsContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_argument(&mut self, name: ClarityName, arg_type: TypeSignature) {
        self.arguments.insert(name, arg_type);
    }

    pub fn is_user_argument(&self, name: &ClarityName) -> bool {
        self.arguments.contains_key(name)
    }

    pub fn get_argument_type(&self, name: &ClarityName) -> Option<&TypeSignature> {
        self.arguments.get(name)
    }

    pub fn add_map_type(
        &mut self,
        name: ClarityName,
        key_type: TypeSignature,
        value_type: TypeSignature,
    ) {
        self.map_types.insert(name, (key_type, value_type));
    }

    pub fn get_map_type(&self, name: &ClarityName) -> Option<&(TypeSignature, TypeSignature)> {
        self.map_types.get(name)
    }

    pub fn add_data_var_type(&mut self, name: ClarityName, value_type: TypeSignature) {
        self.data_var_types.insert(name, value_type);
    }

    pub fn get_data_var_type(&self, name: &ClarityName) -> Option<&TypeSignature> {
        self.data_var_types.get(name)
    }
}

/// A type to track summed execution costs for different paths
/// This allows us to compute min and max costs across different execution paths
#[derive(Debug, Clone, Default)]
pub struct SummingExecutionCost {
    pub costs: Vec<ExecutionCost>,
}

impl SummingExecutionCost {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_cost(&mut self, cost: ExecutionCost) {
        self.costs.push(cost);
    }

    pub fn add_summing(&mut self, other: &SummingExecutionCost) {
        self.costs.extend(other.costs.clone());
    }

    /// minimum cost across all paths
    pub fn min(&self) -> ExecutionCost {
        self.costs
            .iter()
            .cloned()
            .reduce(|acc, cost| ExecutionCost {
                runtime: acc.runtime.min(cost.runtime),
                write_length: acc.write_length.min(cost.write_length),
                write_count: acc.write_count.min(cost.write_count),
                read_length: acc.read_length.min(cost.read_length),
                read_count: acc.read_count.min(cost.read_count),
            })
            .unwrap_or(ExecutionCost::ZERO)
    }

    /// maximum cost across all paths
    pub fn max(&self) -> ExecutionCost {
        self.costs
            .iter()
            .cloned()
            .reduce(|acc, cost| ExecutionCost {
                runtime: acc.runtime.max(cost.runtime),
                write_length: acc.write_length.max(cost.write_length),
                write_count: acc.write_count.max(cost.write_count),
                read_length: acc.read_length.max(cost.read_length),
                read_count: acc.read_count.max(cost.read_count),
            })
            .unwrap_or(ExecutionCost::ZERO)
    }

    pub fn add_all(&self) -> ExecutionCost {
        self.costs
            .iter()
            .fold(ExecutionCost::ZERO, |mut acc, cost| {
                saturating_add_cost(&mut acc, cost);
                acc
            })
    }
}

impl From<ExecutionCost> for SummingExecutionCost {
    fn from(cost: ExecutionCost) -> Self {
        Self { costs: vec![cost] }
    }
}

use super::saturating_add_cost;

fn make_ast(
    source: &str,
    epoch: StacksEpochId,
    clarity_version: &ClarityVersion,
) -> Result<clarity::vm::ast::ContractAST, StaticCostError> {
    let contract_identifier = QualifiedContractIdentifier::transient();
    let mut cost_tracker = ();
    build_ast(
        &contract_identifier,
        source,
        &mut cost_tracker,
        *clarity_version,
        epoch,
    )
    .map_err(|e| StaticCostError::AstParse(format!("{e:?}")))
}

/// Static execution cost for functions within Environment.
/// Returns the static cost for each function in the contract.
/// {some-function-name: (StaticCost, Some({some-function-name: (1,1)}))}
pub fn static_cost(
    env: &mut Environment,
    contract_identifier: &QualifiedContractIdentifier,
) -> Result<HashMap<String, (StaticCost, Option<TraitCount>)>, StaticCostError> {
    let contract_source = env
        .global_context
        .database
        .get_contract_src(contract_identifier)
        .ok_or_else(|| StaticCostError::ContractNotFound(contract_identifier.to_string()))?;

    let contract = env
        .global_context
        .database
        .get_contract(contract_identifier)
        .map_err(|e| StaticCostError::ContractLoad(format!("{e:?}")))?;

    let clarity_version = contract.contract_context.get_clarity_version();

    let epoch = env.global_context.epoch_id;
    let ast = make_ast(&contract_source, epoch, clarity_version)?;

    static_cost_from_ast_with_source(&ast, clarity_version, epoch, Some(&contract_source), env)
}

/// Extract function signature arguments from a function definition expression.
///
/// Function definition structure: (define-public (function-name (arg1 type1) (arg2 type2)) body)
/// Returns (2, [(arg1, type1), (arg2, type2)]) where args excludes the function name.
///
/// Note: Returns `Some((0, &[]))` for functions with no arguments
fn extract_function_signature(expr: &SymbolicExpression) -> Option<(usize, &[SymbolicExpression])> {
    let list = expr.match_list()?;
    if list.len() < 2 {
        return None;
    }

    let signature = list[1].match_list()?;
    if signature.len() <= 1 {
        // Only function name (or empty)
        return Some((0, &[]));
    }

    // Skip the first element (function name), get the rest (arguments)
    let args = &signature[1..];
    Some((args.len(), args))
}

/// Compute overhead costs for executing a function.
///
/// When a function is called, there are overhead costs that are charged
/// before the function body is executed:
///
/// 1. **Contract Loading Cost** (`cost_load_contract`):
///    - Charged when loading the contract from storage to execute a function
///
/// 2. **Function Application Cost** (`cost_user_function_application`):
///    - Charged when applying a user-defined function (not native functions)
///
/// 3. **Inner Type Check Cost** (`cost_inner_type_check_cost`):
///    - Charged when type checking the arguments of a user-defined function
fn compute_function_overhead_costs(
    contract_size: Option<u64>,
    function_name: &str,
    ast_expressions: &[SymbolicExpression],
    epoch: StacksEpochId,
) -> StaticCost {
    let mut overhead = StaticCost::ZERO;

    // Find the function definition in the AST that matches the function name
    let function_def = ast_expressions.iter().find(|expr| {
        extract_function_name(expr)
            .map(|name| name == function_name)
            .unwrap_or(false)
    });

    // Extract the function signature (argument list) from the function definition
    let (arg_count, signature_args) = function_def
        .and_then(extract_function_signature)
        .unwrap_or((0, &[]));

    // cost_load_contract
    if let Some(size) = contract_size {
        let load_cost = ClarityCostFunction::LoadContract
            .eval_for_epoch(size, epoch)
            .unwrap_or(ExecutionCost::ZERO);
        saturating_add_cost(&mut overhead.min, &load_cost);
        saturating_add_cost(&mut overhead.max, &load_cost);
    }

    // cost_user_function_application
    let application_cost = ClarityCostFunction::UserFunctionApplication
        .eval_for_epoch(arg_count as u64, epoch)
        .unwrap_or(ExecutionCost::ZERO);
    saturating_add_cost(&mut overhead.min, &application_cost);
    saturating_add_cost(&mut overhead.max, &application_cost);

    // Parse function signature for type checking costs
    if !signature_args.is_empty() {
        match parse_name_type_pairs::<(), clarity_types::errors::CommonCheckErrorKind>(
            epoch,
            signature_args,
            SyntaxBindingErrorType::Eval,
            &mut (),
        ) {
            Ok(sigs) => {
                for (_, sig) in sigs.iter() {
                    let type_check_cost = ClarityCostFunction::InnerTypeCheckCost
                        .eval_for_epoch(u64::from(sig.size().unwrap_or(0)), epoch)
                        .unwrap_or(ExecutionCost::ZERO);
                    let type_check_min_cost = ClarityCostFunction::InnerTypeCheckCost
                        .eval_for_epoch(u64::from(sig.min_size().unwrap_or(0)), epoch)
                        .unwrap_or(ExecutionCost::ZERO);
                    saturating_add_cost(&mut overhead.min, &type_check_min_cost);
                    saturating_add_cost(&mut overhead.max, &type_check_cost);
                }
            }
            Err(_e) => {
                // This should probably only be hit for contracts that don't
                // type check anyway. We continue so the other costs are still
                // calculated.
            }
        }
    }

    overhead
}

pub fn static_cost_from_ast(
    contract_ast: &clarity::vm::ast::ContractAST,
    clarity_version: &ClarityVersion,
    epoch: StacksEpochId,
    env: &mut Environment,
) -> Result<HashMap<String, (StaticCost, Option<TraitCount>)>, StaticCostError> {
    static_cost_from_ast_with_source(contract_ast, clarity_version, epoch, None, env)
}

pub fn static_cost_from_ast_with_source(
    contract_ast: &clarity::vm::ast::ContractAST,
    clarity_version: &ClarityVersion,
    epoch: StacksEpochId,
    contract_source: Option<&str>,
    env: &mut Environment,
) -> Result<HashMap<String, (StaticCost, Option<TraitCount>)>, StaticCostError> {
    let contract_size = contract_source.map(|s| s.len() as u64);

    let cost_trees_with_traits =
        static_cost_tree_from_ast(contract_ast, clarity_version, epoch, env)?;

    let trait_count = cost_trees_with_traits
        .values()
        .next()
        .and_then(|(_, trait_count)| trait_count.clone());

    // Convert CostAnalysisNode to StaticCost and add overhead costs
    let costs: HashMap<String, StaticCost> = cost_trees_with_traits
        .iter()
        .map(|(name, (cost_analysis_node, _))| {
            let summing_cost = calculate_total_cost_with_branching(cost_analysis_node);
            let mut static_cost: StaticCost = summing_cost.into();

            let overhead = compute_function_overhead_costs(
                contract_size,
                name,
                &contract_ast.expressions,
                epoch,
            );
            saturating_add_cost(&mut static_cost.min, &overhead.min);
            saturating_add_cost(&mut static_cost.max, &overhead.max);

            (name.clone(), static_cost)
        })
        .collect();

    Ok(costs
        .into_iter()
        .map(|(name, cost)| (name, (cost, trait_count.clone())))
        .collect())
}

pub fn static_cost_tree_from_ast(
    ast: &clarity::vm::ast::ContractAST,
    clarity_version: &ClarityVersion,
    epoch: StacksEpochId,
    env: &mut Environment,
) -> Result<HashMap<String, (CostAnalysisNode, Option<TraitCount>)>, StaticCostError> {
    let exprs = &ast.expressions;
    let mut user_args = UserArgumentsContext::new();
    let mut costs_map: HashMap<String, Option<StaticCost>> = HashMap::new();
    let mut costs: HashMap<String, Option<CostAnalysisNode>> = HashMap::new();
    // first pass: extract function names and collect define-map key/value types
    {
        let mut free_tracker = clarity::vm::costs::LimitedCostTracker::new_free();
        for expr in exprs {
            if let Some(list) = expr.match_list() {
                let head = list.first().and_then(|f| f.match_atom());
                if head.map(|a| a.as_str() == "define-map").unwrap_or(false) && list.len() >= 4 {
                    if let Some(map_name) = list[1].match_atom() {
                        let key_type =
                            TypeSignature::parse_type_repr(epoch, &list[2], &mut free_tracker).ok();
                        let value_type =
                            TypeSignature::parse_type_repr(epoch, &list[3], &mut free_tracker).ok();
                        if let (Some(k), Some(v)) = (key_type, value_type) {
                            user_args.add_map_type(map_name.clone(), k, v);
                        }
                    }
                }
                if head
                    .map(|a| a.as_str() == "define-data-var")
                    .unwrap_or(false)
                    && list.len() >= 3
                {
                    if let Some(var_name) = list[1].match_atom() {
                        if let Ok(value_type) =
                            TypeSignature::parse_type_repr(epoch, &list[2], &mut free_tracker)
                        {
                            user_args.add_data_var_type(var_name.clone(), value_type);
                        }
                    }
                }
                if let Some(function_name) = extract_function_name(expr) {
                    costs.insert(function_name.clone(), None);
                    costs_map.insert(function_name, None);
                }
            }
        }
    }
    // second pass computes the cost
    for expr in exprs {
        if let Some(function_name) = extract_function_name(expr) {
            let (_, cost_analysis_tree) = build_cost_analysis_tree(
                expr,
                &user_args,
                &costs_map,
                clarity_version,
                epoch,
                env,
                0,
            )?;
            // Compute static cost for this function so subsequent calls can look it up.
            // Include the overhead costs so that callers get the full cost
            // of invoking this function. Note: LookupFunction is NOT added here because
            // the dynamic VM does not charge it for user-defined function calls.
            let mut sc: StaticCost =
                calculate_total_cost_with_branching(&cost_analysis_tree).into();
            let overhead = compute_function_overhead_costs(None, &function_name, exprs, epoch);
            super::saturating_add_cost(&mut sc.min, &overhead.min);
            super::saturating_add_cost(&mut sc.max, &overhead.max);
            costs_map.insert(function_name.clone(), Some(sc));
            costs.insert(function_name, Some(cost_analysis_tree));
        }
    }

    // Build the final map with cost analysis nodes
    let cost_trees: HashMap<String, CostAnalysisNode> = costs
        .into_iter()
        .filter_map(|(name, cost)| cost.map(|c| (name, c)))
        .collect();

    // Compute trait_count while creating the root CostAnalysisNode
    let trait_count = get_trait_count(&cost_trees);

    // Return each node with its trait_count
    Ok(cost_trees
        .into_iter()
        .map(|(name, node)| (name, (node, trait_count.clone())))
        .collect())
}

/// Extract function name from a symbolic expression
fn extract_function_name(expr: &SymbolicExpression) -> Option<String> {
    expr.match_list().and_then(|list| {
        list.first()
            .and_then(|first| first.match_atom())
            .filter(|atom| is_function_definition(atom.as_str()))
            .and_then(|_| list.get(1))
            .and_then(|sig| sig.match_list())
            .and_then(|signature| signature.first())
            .and_then(|name| name.match_atom())
            .map(|name| name.to_string())
    })
}

pub fn build_cost_analysis_tree(
    expr: &SymbolicExpression,
    user_args: &UserArgumentsContext,
    cost_map: &HashMap<String, Option<StaticCost>>,
    clarity_version: &ClarityVersion,
    epoch: StacksEpochId,
    env: &mut Environment,
    let_depth: u64,
) -> Result<(Option<String>, CostAnalysisNode), StaticCostError> {
    match &expr.expr {
        SymbolicExpressionType::List(list) => {
            if let Some(function_name) = list.first().and_then(|first| first.match_atom()) {
                if is_function_definition(function_name.as_str()) {
                    let (returned_function_name, cost_analysis_tree) =
                        build_function_definition_cost_analysis_tree(
                            list,
                            user_args,
                            cost_map,
                            clarity_version,
                            epoch,
                            env,
                        )?;
                    Ok((Some(returned_function_name), cost_analysis_tree))
                } else {
                    let cost_analysis_tree = build_listlike_cost_analysis_tree(
                        list,
                        user_args,
                        cost_map,
                        clarity_version,
                        epoch,
                        env,
                        let_depth,
                    )?;
                    Ok((None, cost_analysis_tree))
                }
            } else {
                let cost_analysis_tree = build_listlike_cost_analysis_tree(
                    list,
                    user_args,
                    cost_map,
                    clarity_version,
                    epoch,
                    env,
                    let_depth,
                )?;
                Ok((None, cost_analysis_tree))
            }
        }
        SymbolicExpressionType::AtomValue(value) => {
            let cost = calculate_value_cost(value);
            Ok((
                None,
                CostAnalysisNode::leaf(CostExprNode::AtomValue(value.clone()), cost),
            ))
        }
        SymbolicExpressionType::LiteralValue(value) => {
            let cost = calculate_value_cost(value);
            Ok((
                None,
                CostAnalysisNode::leaf(CostExprNode::AtomValue(value.clone()), cost),
            ))
        }
        SymbolicExpressionType::Atom(name) => {
            // IF not reserved variable
            // lookup variable size cost
            // lookup variable depth cost

            let context = LocalContext::new();
            let cost = match lookup_reserved_variable(name, &context, env).unwrap_or(None) {
                Some(_value) => StaticCost::ZERO,
                None => {
                    // Try to get the TypeSignature from ContractAnalysis
                    let type_sig: Option<TypeSignature> = env
                        .global_context
                        .database
                        .load_contract_analysis(&env.contract_context.contract_identifier)
                        .ok()
                        .flatten()
                        .and_then(|analysis| {
                            // local/temporary variables
                            analysis
                                .variable_types
                                .get(name)
                                .or_else(|| {
                                    // contract storage variables
                                    analysis.persisted_variable_types.get(name)
                                })
                                .cloned()
                        });

                    // Calculate cost from type signature
                    // Note: In dynamic execution, LookupVariableDepth is charged BEFORE checking where
                    // the variable is found, so depth is charged for all variables (local and contract storage).
                    // However, the actual depth used depends on context.depth() at the lookup site.
                    // For now, we use let_depth which tracks let-binding depth
                    type_sig
                        .as_ref()
                        .map(|sig| calculate_variable_lookup_cost_from_type(sig, let_depth, epoch))
                        .unwrap_or(StaticCost::ZERO)
                }
            };
            let expr_node = parse_atom_expression(name, user_args);

            // If this is a UserArgument, recalculate cost using the argument type from user_args
            let final_cost = if let CostExprNode::UserArgument(ref arg_name, _) = expr_node {
                // Get the type from user_args and calculate cost from it
                if let Some(arg_type) = user_args.get_argument_type(arg_name) {
                    calculate_variable_lookup_cost_from_type(arg_type, let_depth, epoch)
                } else {
                    cost
                }
            } else {
                cost
            };

            Ok((None, CostAnalysisNode::leaf(expr_node, final_cost)))
        }
        SymbolicExpressionType::Field(field_identifier) => Ok((
            None,
            CostAnalysisNode::leaf(
                CostExprNode::FieldIdentifier(field_identifier.clone()),
                StaticCost::ZERO,
            ),
        )),
        SymbolicExpressionType::TraitReference(trait_name, _trait_definition) => Ok((
            None,
            CostAnalysisNode::leaf(
                CostExprNode::TraitReference(trait_name.clone()),
                StaticCost::ZERO,
            ),
        )),
    }
}

/// Calculate variable lookup cost from a TypeSignature
fn calculate_variable_lookup_cost_from_type(
    type_sig: &TypeSignature,
    let_depth: u64,
    epoch: StacksEpochId,
) -> StaticCost {
    let type_size = u64::from(type_sig.size().unwrap_or(0));
    let type_min_size = u64::from(type_sig.min_size().unwrap_or(0));

    let mut variable_size_cost = ClarityCostFunction::LookupVariableSize
        .eval_for_epoch(type_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);
    let mut variable_size_min_cost = ClarityCostFunction::LookupVariableSize
        .eval_for_epoch(type_min_size, epoch)
        .unwrap_or(ExecutionCost::ZERO);

    let lookup_variable_depth_cost = ClarityCostFunction::LookupVariableDepth
        .eval_for_epoch(let_depth, epoch)
        .unwrap_or(ExecutionCost::ZERO);

    saturating_add_cost(&mut variable_size_min_cost, &lookup_variable_depth_cost);
    saturating_add_cost(&mut variable_size_cost, &lookup_variable_depth_cost);

    StaticCost {
        min: variable_size_min_cost,
        max: variable_size_cost,
    }
}

/// Look up types for let-bound variables from the type_map in contract analysis.
/// Falls back to inferring types from literal values and TupleCons expressions when
/// type_map is not available.
/// Bindings are processed incrementally so later bindings may reference earlier ones.
/// Returns a context that includes `base_args` plus all newly-inferred bindings.
fn lookup_let_binding_types(
    binding_list: &[SymbolicExpression],
    contract_analysis: Option<&clarity::vm::analysis::ContractAnalysis>,
    base_args: &UserArgumentsContext,
    epoch: StacksEpochId,
) -> UserArgumentsContext {
    // Clone base_args so later bindings can reference outer args and earlier bindings.
    let mut context = base_args.clone();

    for binding in binding_list.iter() {
        if let Some(binding_pair) = binding.match_list() {
            if binding_pair.len() == 2 {
                if let Some(var_name) = binding_pair[0].match_atom() {
                    // Try to look up type from type_map first
                    let binding_type = contract_analysis
                        .and_then(|analysis| analysis.type_map.as_ref())
                        .and_then(|type_map| type_map.get_type_expected(&binding_pair[1]))
                        .cloned()
                        .or_else(|| {
                            // Fallback: infer type using the current accumulated context.
                            // This handles TupleCons expressions like {key: a} and allows
                            // later bindings to reference earlier ones.
                            infer_type_from_expression_with_args(&binding_pair[1], &context, epoch)
                                .ok()
                        });

                    if let Some(ty) = binding_type {
                        context.add_argument(var_name.clone(), ty);
                    }
                }
            }
        }
    }

    context
}

/// Infer type from a SymbolicExpression, using `user_args` for atom lookups and
/// TupleCons value expressions like `(tuple (key a))`.
fn infer_type_from_expression_with_args(
    expr: &SymbolicExpression,
    user_args: &UserArgumentsContext,
    epoch: StacksEpochId,
) -> Result<TypeSignature, StaticCostError> {
    match &expr.expr {
        SymbolicExpressionType::Atom(name) => {
            if let Some(t) = user_args.get_argument_type(name) {
                return Ok(t.clone());
            }
            infer_type_from_expression(expr, epoch)
        }
        SymbolicExpressionType::List(exprs) => {
            // Try TupleCons inference before falling back to type-annotation parsing.
            if let Some(tuple_type) = infer_tuple_type_from_tuplecons(exprs, user_args, epoch) {
                return Ok(tuple_type);
            }
            infer_type_from_expression(expr, epoch)
        }
        _ => infer_type_from_expression(expr, epoch),
    }
}

/// Try to infer a `TupleType` from a TupleCons list like `(tuple (key a))`.
/// Returns `None` if the expression is not a valid TupleCons or if any field type
/// cannot be determined.
fn infer_tuple_type_from_tuplecons(
    exprs: &[SymbolicExpression],
    user_args: &UserArgumentsContext,
    epoch: StacksEpochId,
) -> Option<TypeSignature> {
    // First element must be the atom "tuple"
    if exprs.first()?.match_atom()?.as_str() != "tuple" {
        return None;
    }

    let mut field_map: BTreeMap<ClarityName, TypeSignature> = BTreeMap::new();
    for field_expr in &exprs[1..] {
        let pair = field_expr.match_list()?;
        if pair.len() != 2 {
            return None;
        }
        let field_name = pair[0].match_atom()?.clone();
        let field_type = infer_type_from_expression_with_args(&pair[1], user_args, epoch).ok()?;
        field_map.insert(field_name, field_type);
    }

    TupleTypeSignature::try_from(field_map)
        .ok()
        .map(TypeSignature::TupleType)
}

/// Infer type from a SymbolicExpression by examining its structure.
/// This is a fallback when type_map is not available.
pub(crate) fn infer_type_from_expression(
    expr: &SymbolicExpression,
    epoch: StacksEpochId,
) -> Result<TypeSignature, StaticCostError> {
    match &expr.expr {
        SymbolicExpressionType::LiteralValue(value) => TypeSignature::literal_type_of(value)
            .map_err(|e| StaticCostError::TypeParse(format!("{e:?}"))),
        SymbolicExpressionType::AtomValue(value) => {
            TypeSignature::type_of(value).map_err(|e| StaticCostError::TypeParse(format!("{e:?}")))
        }
        SymbolicExpressionType::Atom(name) => {
            // Try to parse as a type name (e.g., "uint", "int", "bool")
            TypeSignature::parse_atom_type(name.as_str())
                .map_err(|e| StaticCostError::TypeParse(format!("{e:?}")))
        }
        SymbolicExpressionType::List(_) => {
            // Try to parse as a list type (e.g., "(list 10 uint)")
            // Use a free cost tracker since we're just parsing types
            let mut free_tracker = clarity::vm::costs::LimitedCostTracker::new_free();
            TypeSignature::parse_type_repr(epoch, expr, &mut free_tracker)
                .map_err(|e| StaticCostError::TypeParse(format!("{e:?}")))
        }
        SymbolicExpressionType::TraitReference(_, _) | SymbolicExpressionType::Field(_) => Err(
            StaticCostError::MalformedAst("cannot infer type from trait reference or field"),
        ),
    }
}

/// Parse an atom expression into an ExprNode
fn parse_atom_expression(name: &ClarityName, user_args: &UserArgumentsContext) -> CostExprNode {
    user_args
        .get_argument_type(name)
        .map(|arg_type| CostExprNode::UserArgument(name.clone(), arg_type.clone()))
        .unwrap_or_else(|| CostExprNode::Atom(name.clone()))
}

/// Build an expression tree for function definitions like (define-public (foo (a u64)) (ok a))
fn build_function_definition_cost_analysis_tree(
    list: &[SymbolicExpression],
    outer_user_args: &UserArgumentsContext,
    cost_map: &HashMap<String, Option<StaticCost>>,
    clarity_version: &ClarityVersion,
    epoch: StacksEpochId,
    env: &mut Environment,
) -> Result<(String, CostAnalysisNode), StaticCostError> {
    let define_type = list[0].match_atom().ok_or(StaticCostError::MalformedAst(
        "expected atom for define type",
    ))?;
    let signature = list[1].match_list().ok_or(StaticCostError::MalformedAst(
        "expected list for function signature",
    ))?;
    let body = &list[2];

    let mut children = Vec::new();
    // Start with contract-level context (carries map_types and data_var_types) but no argument bindings.
    let mut function_user_args = UserArgumentsContext {
        arguments: HashMap::new(),
        map_types: outer_user_args.map_types.clone(),
        data_var_types: outer_user_args.data_var_types.clone(),
    };

    // Process function arguments: (a u64)
    // Use a free cost tracker since we're just parsing types
    let mut free_tracker = clarity::vm::costs::LimitedCostTracker::new_free();
    for arg_expr in signature.iter().skip(1) {
        if let Some(arg_list) = arg_expr.match_list() {
            if arg_list.len() == 2 {
                let arg_name = arg_list[0]
                    .match_atom()
                    .ok_or(StaticCostError::MalformedAst(
                        "expected atom for argument name",
                    ))?;

                let arg_type_expr = &arg_list[1];

                // Parse the type from the AST to TypeSignature
                let arg_type =
                    TypeSignature::parse_type_repr(epoch, arg_type_expr, &mut free_tracker)
                        .map_err(|e| StaticCostError::TypeParse(format!("{e:?}")))?;

                // Add to function's user arguments context
                function_user_args.add_argument(arg_name.clone(), arg_type.clone());

                // Create UserArgument node
                children.push(CostAnalysisNode::leaf(
                    CostExprNode::UserArgument(arg_name.clone(), arg_type),
                    StaticCost::ZERO,
                ));
            }
        }
    }

    // Process the function body with the function's user arguments context
    let (_, mut body_tree) = build_cost_analysis_tree(
        body,
        &function_user_args,
        cost_map,
        clarity_version,
        epoch,
        env,
        0,
    )?;

    // If the function body is a `let` whose last child is `(ok ...)` wrapping a
    // non-storage expression, the dynamic VM does not charge OkCons — only the
    // LookupFunction cost for `ok` is incurred.  Reduce ConsOkay to just that
    // lookup cost in this case.
    if let CostExprNode::NativeFunction(NativeFunctions::Let) = &body_tree.expr {
        if let Some(last_child) = body_tree.children.last() {
            if let CostExprNode::NativeFunction(NativeFunctions::ConsOkay) = &last_child.expr {
                let has_storage_child = last_child.children.iter().any(|c| {
                    matches!(
                        &c.expr,
                        CostExprNode::NativeFunction(
                            NativeFunctions::SetEntry
                                | NativeFunctions::InsertEntry
                                | NativeFunctions::DeleteEntry
                                | NativeFunctions::SetVar
                                | NativeFunctions::FetchVar
                                | NativeFunctions::FetchEntry
                        )
                    )
                });
                if !has_storage_child {
                    let lookup_cost = ClarityCostFunction::LookupFunction
                        .eval_for_epoch(0, epoch)
                        .unwrap_or(ExecutionCost::ZERO);
                    let last_idx = body_tree.children.len() - 1;
                    body_tree.children[last_idx].cost = StaticCost {
                        min: lookup_cost.clone(),
                        max: lookup_cost,
                    };
                }
            }
        }
    }

    // If the body is a native function
    // exclude its execution cost but keep its lookup cost.
    if let CostExprNode::NativeFunction(native_fn) = &body_tree.expr {
        // ConsOkay, If, Match, and Begin should always charge their execution cost
        // control flow expressions are always executed
        if !matches!(
            native_fn,
            NativeFunctions::ConsOkay
                | NativeFunctions::If
                | NativeFunctions::Match
                | NativeFunctions::Begin
        ) {
            // Check if ALL children are nested expressions (not simple values)
            let all_nested_expressions = !body_tree.children.is_empty()
                && body_tree.children.iter().all(|child| {
                    matches!(
                        child.expr,
                        CostExprNode::NativeFunction(_) | CostExprNode::UserFunction(_)
                    )
                });
            if all_nested_expressions {
                // Keep only the lookup cost and exclude the function execution cost
                let lookup_cost = ClarityCostFunction::LookupFunction
                    .eval_for_epoch(0, epoch)
                    .unwrap_or(ExecutionCost::ZERO);
                body_tree.cost = StaticCost {
                    min: lookup_cost.clone(),
                    max: lookup_cost,
                };
            }
        }
    }

    children.push(body_tree);

    // Get the function name from the signature
    let function_name = signature[0]
        .match_atom()
        .ok_or(StaticCostError::MalformedAst(
            "expected atom for function name",
        ))?;

    // Create the function definition node with zero cost (function definitions themselves don't have execution cost)
    Ok((
        function_name.clone().to_string(),
        CostAnalysisNode::new(
            CostExprNode::UserFunction(define_type.clone()),
            StaticCost::ZERO,
            children,
        ),
    ))
}

/// Helper function to build expression trees for both lists and tuples
fn build_listlike_cost_analysis_tree(
    exprs: &[SymbolicExpression],
    user_args: &UserArgumentsContext,
    cost_map: &HashMap<String, Option<StaticCost>>,
    clarity_version: &ClarityVersion,
    epoch: StacksEpochId,
    env: &mut Environment,
    mut let_depth: u64,
) -> Result<CostAnalysisNode, StaticCostError> {
    let mut children = Vec::new();

    let (expr_node, cost) = match &exprs[0].expr {
        SymbolicExpressionType::List(_) => {
            let (_, nested_tree) = build_cost_analysis_tree(
                &exprs[0],
                user_args,
                cost_map,
                clarity_version,
                epoch,
                env,
                let_depth,
            )?;
            for expr in exprs[1..].iter() {
                let (_, child_tree) = build_cost_analysis_tree(
                    expr,
                    user_args,
                    cost_map,
                    clarity_version,
                    epoch,
                    env,
                    let_depth,
                )?;
                children.push(child_tree);
            }
            // Add the nested tree as a child (its cost will be included when summing children)
            children.insert(0, nested_tree);
            // The root cost is zero - the actual cost comes from the nested expression
            let expr_node = CostExprNode::NestedExpression;
            (expr_node, StaticCost::ZERO)
        }
        SymbolicExpressionType::Atom(name) => {
            // Try to get function name from first element
            // lookup the function as a native function first
            // special functions
            //   - let, etc use bindings lengths not argument lengths
            if let Some(native_function) =
                NativeFunctions::lookup_by_name_at_version(name.as_str(), clarity_version)
            {
                // Special handling for Let: increment depth before processing body
                if native_function == NativeFunctions::Let {
                    // Create extended context with let-bound variables
                    let mut extended_user_args = user_args.clone();

                    // Process bindings with current depth and extract variable types from contract analysis
                    if exprs.len() > 1 {
                        // Try to load contract analysis to get type information
                        let contract_analysis = env
                            .global_context
                            .database
                            .load_contract_analysis(&env.contract_context.contract_identifier)
                            .ok()
                            .flatten();

                        if let Some(binding_list) = exprs[1].match_list() {
                            // Look up types for let-bound variables from type_map.
                            // Builds incrementally so later bindings can reference earlier ones.
                            extended_user_args = lookup_let_binding_types(
                                binding_list,
                                contract_analysis.as_ref(),
                                user_args,
                                epoch,
                            );
                        }

                        let (_, binding_tree) = build_cost_analysis_tree(
                            &exprs[1],
                            &extended_user_args,
                            cost_map,
                            clarity_version,
                            epoch,
                            env,
                            let_depth + 1,
                        )?;
                        children.push(binding_tree);
                    }
                    // Increment depth before processing body (let creates a new context)
                    let_depth += 1;
                    for expr in exprs[2..].iter() {
                        let (_, child_tree) = build_cost_analysis_tree(
                            expr,
                            &extended_user_args,
                            cost_map,
                            clarity_version,
                            epoch,
                            env,
                            let_depth,
                        )?;
                        children.push(child_tree);
                    }
                } else if native_function == NativeFunctions::If {
                    // `If` creates a nested context
                    let nested_depth = let_depth + 1;
                    for expr in exprs[1..].iter() {
                        let (_, child_tree) = build_cost_analysis_tree(
                            expr,
                            user_args,
                            cost_map,
                            clarity_version,
                            epoch,
                            env,
                            nested_depth,
                        )?;
                        children.push(child_tree);
                    }
                } else {
                    // For other functions, build all children with current depth
                    for expr in exprs[1..].iter() {
                        let (_, child_tree) = build_cost_analysis_tree(
                            expr,
                            user_args,
                            cost_map,
                            clarity_version,
                            epoch,
                            env,
                            let_depth,
                        )?;
                        children.push(child_tree);
                    }
                }

                let cost = calculate_function_cost_from_native_function(
                    native_function,
                    children.len() as u64,
                    &exprs[1..],
                    epoch,
                    Some(user_args),
                    Some(env),
                )?;

                (CostExprNode::NativeFunction(native_function), cost)
            } else {
                for expr in exprs[1..].iter() {
                    let (_, child_tree) = build_cost_analysis_tree(
                        expr,
                        user_args,
                        cost_map,
                        clarity_version,
                        epoch,
                        env,
                        let_depth,
                    )?;
                    children.push(child_tree);
                }
                // If not a native function, check if it's a known user-defined function
                // or a callable (trait) argument used in call position.
                // Names not in cost_map and not callable args are tuple field names
                // or similar non-function atoms.
                let is_callable_arg = user_args.get_argument_type(name).is_some_and(|t| {
                    matches!(
                        t,
                        TypeSignature::CallableType(_) | TypeSignature::TraitReferenceType(_)
                    )
                });
                if cost_map.contains_key(name.as_str()) {
                    let expr_node = CostExprNode::UserFunction(name.clone());
                    let cost = calculate_function_cost(name.as_str(), cost_map)?;
                    (expr_node, cost)
                } else if is_callable_arg {
                    // Callable (trait) arguments used in call position — the actual
                    // cost depends on which contract implements the trait, so we
                    // can't determine it statically.
                    let expr_node = CostExprNode::UserFunction(name.clone());
                    (expr_node, StaticCost::ZERO)
                } else {
                    (CostExprNode::Atom(name.clone()), StaticCost::ZERO)
                }
            }
        }
        SymbolicExpressionType::AtomValue(value) => {
            for expr in exprs[1..].iter() {
                let (_, child_tree) = build_cost_analysis_tree(
                    expr,
                    user_args,
                    cost_map,
                    clarity_version,
                    epoch,
                    env,
                    let_depth,
                )?;
                children.push(child_tree);
            }
            let cost = calculate_value_cost(value);
            (CostExprNode::AtomValue(value.clone()), cost)
        }
        SymbolicExpressionType::TraitReference(trait_name, _trait_definition) => {
            for expr in exprs[1..].iter() {
                let (_, child_tree) = build_cost_analysis_tree(
                    expr,
                    user_args,
                    cost_map,
                    clarity_version,
                    epoch,
                    env,
                    let_depth,
                )?;
                children.push(child_tree);
            }
            (
                CostExprNode::TraitReference(trait_name.clone()),
                StaticCost::ZERO,
            )
        }
        SymbolicExpressionType::Field(field_identifier) => {
            for expr in exprs[1..].iter() {
                let (_, child_tree) = build_cost_analysis_tree(
                    expr,
                    user_args,
                    cost_map,
                    clarity_version,
                    epoch,
                    env,
                    let_depth,
                )?;
                children.push(child_tree);
            }
            (
                CostExprNode::FieldIdentifier(field_identifier.clone()),
                StaticCost::ZERO,
            )
        }
        SymbolicExpressionType::LiteralValue(value) => {
            for expr in exprs[1..].iter() {
                let (_, child_tree) = build_cost_analysis_tree(
                    expr,
                    user_args,
                    cost_map,
                    clarity_version,
                    epoch,
                    env,
                    let_depth,
                )?;
                children.push(child_tree);
            }
            let cost = calculate_value_cost(value);
            // TODO not sure if LiteralValue is needed in the CostExprNode types
            (CostExprNode::AtomValue(value.clone()), cost)
        }
    };

    // Zero out string literal costs for functions where string arguments have zero cost
    // because the function cost includes their processing (concat, len)
    if let CostExprNode::NativeFunction(native_function) = &expr_node {
        if matches!(
            native_function,
            NativeFunctions::Concat | NativeFunctions::Len
        ) {
            for child in &mut children {
                // Check if this child is a string literal value
                if let CostExprNode::AtomValue(Value::Sequence(_)) = &child.expr {
                    // Zero out the cost - the function cost already includes processing the string
                    child.cost = StaticCost::ZERO;
                }
            }
        }
    }

    Ok(CostAnalysisNode::new(expr_node, cost, children))
}

/// Recursively collect all user-defined function calls within a cost analysis tree.
fn collect_fn_calls(
    node: &CostAnalysisNode,
    visited_functions: &HashSet<String>,
    trait_names: &HashMap<ClarityName, String>,
    calls: &mut HashSet<String>,
) {
    if let CostExprNode::UserFunction(fn_name) = &node.expr {
        if !is_function_definition(fn_name.as_str())
            && !trait_names.contains_key(fn_name)
            && visited_functions.contains(fn_name.as_str())
        {
            calls.insert(fn_name.to_string());
        }
    }
    for child in &node.children {
        collect_fn_calls(child, visited_functions, trait_names, calls);
    }
}

/// DFS postorder visit for topological sort (callee before caller).
fn topo_visit(
    fn_name: &str,
    call_graph: &HashMap<String, HashSet<String>>,
    in_progress: &mut HashSet<String>,
    finished: &mut HashSet<String>,
    result: &mut Vec<String>,
) {
    if finished.contains(fn_name) || in_progress.contains(fn_name) {
        return;
    }
    in_progress.insert(fn_name.to_string());
    if let Some(callees) = call_graph.get(fn_name) {
        let mut sorted_callees: Vec<&String> = callees.iter().collect();
        sorted_callees.sort();
        for callee in sorted_callees {
            topo_visit(callee, call_graph, in_progress, finished, result);
        }
    }
    in_progress.remove(fn_name);
    finished.insert(fn_name.to_string());
    result.push(fn_name.to_string()); // postorder: callee before caller
}

/// makes sure when the propagator processes a caller, the callee's
/// trait counts are already finalized.
fn topo_order(
    costs: &HashMap<String, CostAnalysisNode>,
    visited_functions: &HashSet<String>,
    trait_names: &HashMap<ClarityName, String>,
) -> Vec<String> {
    let call_graph: HashMap<String, HashSet<String>> = costs
        .iter()
        .map(|(name, node)| {
            let mut calls = HashSet::new();
            collect_fn_calls(node, visited_functions, trait_names, &mut calls);
            (name.clone(), calls)
        })
        .collect();

    let mut result = Vec::new();
    let mut in_progress = HashSet::new();
    let mut finished = HashSet::new();

    // Deterministic starting order
    let mut fn_names: Vec<String> = costs.keys().cloned().collect();
    fn_names.sort();

    for fn_name in fn_names {
        topo_visit(
            &fn_name,
            &call_graph,
            &mut in_progress,
            &mut finished,
            &mut result,
        );
    }
    result
}

pub(crate) fn get_trait_count(costs: &HashMap<String, CostAnalysisNode>) -> Option<TraitCount> {
    // First pass: collect trait counts and trait names
    let mut collector = TraitCountCollector::new();
    // Track all function names upfront so we can identify function calls even before
    // the function definitions are fully processed
    for name in costs.keys() {
        collector.visited_functions.insert(name.clone());
    }
    for (name, cost_analysis_node) in costs.iter() {
        let context = TraitCountContext::new(name.clone(), (1, 1));
        collector.visit(cost_analysis_node, &context);
    }

    // Second pass: propagate trait counts through function calls.
    // Process functions in topological order (callees before callers) so that
    // when we propagate a callee's counts to a caller, the callee's counts are
    // already finalized. Without this ordering, HashMap iteration can visit a
    // caller before its callee, resulting in the callee's partial (first-pass
    // only) counts being propagated instead of the final counts.
    let sorted_fns = topo_order(costs, &collector.visited_functions, &collector.trait_names);
    let mut propagator = TraitCountPropagator::new(
        &mut collector.trait_counts,
        &collector.trait_names,
        &collector.visited_functions,
    );
    for fn_name in &sorted_fns {
        if let Some(cost_analysis_node) = costs.get(fn_name) {
            let context = TraitCountContext::new(fn_name.clone(), (1, 1));
            propagator.visit(cost_analysis_node, &context);
        }
    }

    Some(collector.trait_counts)
}

#[cfg(test)]
mod tests {

    use clarity::vm::contexts::{ContractContext, OwnedEnvironment};
    use clarity::vm::database::MemoryBackingStore;
    use clarity::vm::types::QualifiedContractIdentifier;

    use super::super::is_node_branching;
    use super::*;

    struct TestEnvironment {
        store: MemoryBackingStore,
    }

    impl TestEnvironment {
        fn new(_epoch: StacksEpochId, _clarity_version: ClarityVersion) -> Self {
            Self {
                store: MemoryBackingStore::new(),
            }
        }

        fn get_env(&mut self, epoch: StacksEpochId) -> OwnedEnvironment<'_, '_> {
            let mut db = self.store.as_clarity_db();
            db.begin();
            db.set_clarity_epoch_version(epoch).unwrap();
            db.commit().unwrap();
            OwnedEnvironment::new(db, epoch)
        }
    }

    fn create_test_env(epoch: StacksEpochId, clarity_version: ClarityVersion) -> TestEnvironment {
        TestEnvironment::new(epoch, clarity_version)
    }

    fn static_cost_native_test(
        source: &str,
        clarity_version: &ClarityVersion,
    ) -> Result<StaticCost, StaticCostError> {
        let cost_map: HashMap<String, Option<StaticCost>> = HashMap::new();

        let epoch = StacksEpochId::latest(); // XXX this should be matched with the clarity version
        let ast = make_ast(source, epoch, clarity_version)?;
        let exprs = &ast.expressions;
        let user_args = UserArgumentsContext::new();
        let expr = &exprs[0];
        let mut test_env = create_test_env(epoch, *clarity_version);
        let mut owned_env = test_env.get_env(epoch);
        owned_env.begin();
        let contract_context =
            ContractContext::new(QualifiedContractIdentifier::transient(), *clarity_version);
        let mut env = owned_env.get_exec_environment(None, None, &contract_context);
        let (_, cost_analysis_tree) = build_cost_analysis_tree(
            &expr,
            &user_args,
            &cost_map,
            clarity_version,
            epoch,
            &mut env,
            0,
        )?;

        let summing_cost = calculate_total_cost_with_branching(&cost_analysis_tree);
        Ok(summing_cost.into())
    }

    fn static_cost_test(
        source: &str,
        clarity_version: &ClarityVersion,
    ) -> Result<HashMap<String, StaticCost>, StaticCostError> {
        use clarity::vm::contexts::ContractContext;
        use clarity::vm::types::QualifiedContractIdentifier;

        let epoch = StacksEpochId::latest();
        let ast = make_ast(source, epoch, clarity_version)?;
        let mut test_env = create_test_env(epoch, *clarity_version);
        let mut owned_env = test_env.get_env(epoch);
        owned_env.begin();
        let contract_context =
            ContractContext::new(QualifiedContractIdentifier::transient(), *clarity_version);
        let mut env = owned_env.get_exec_environment(None, None, &contract_context);
        let costs = static_cost_from_ast(&ast, clarity_version, epoch, &mut env)?;
        Ok(costs
            .into_iter()
            .map(|(name, (cost, _trait_count))| (name, cost))
            .collect())
    }

    fn build_test_ast(src: &str) -> clarity::vm::ast::ContractAST {
        let contract_identifier = QualifiedContractIdentifier::transient();
        let mut cost_tracker = ();
        let ast = build_ast(
            &contract_identifier,
            src,
            &mut cost_tracker,
            ClarityVersion::Clarity3,
            StacksEpochId::latest(),
        )
        .unwrap();
        ast
    }

    #[test]
    fn test_constant() {
        let source = "9001";
        let cost = static_cost_native_test(source, &ClarityVersion::Clarity3).unwrap();
        assert_eq!(cost.min.runtime, 0);
        assert_eq!(cost.max.runtime, 0);
    }

    //  ----  ExprTreee building specific tests
    #[test]
    fn test_build_cost_analysis_tree_if_expression() {
        let src = "(if (> 3 0) (ok true) (ok false))";
        let ast = build_test_ast(src);
        let expr = &ast.expressions[0];
        let user_args = UserArgumentsContext::new();
        let cost_map = HashMap::new(); // Empty cost map for tests
        let epoch = StacksEpochId::Epoch32;
        let mut test_env = create_test_env(epoch, ClarityVersion::Clarity3);
        let mut owned_env = test_env.get_env(epoch);
        owned_env.begin();
        let contract_context = ContractContext::new(
            QualifiedContractIdentifier::transient(),
            ClarityVersion::Clarity3,
        );
        let mut env = owned_env.get_exec_environment(None, None, &contract_context);
        let (_, cost_tree) = build_cost_analysis_tree(
            expr,
            &user_args,
            &cost_map,
            &ClarityVersion::Clarity3,
            epoch,
            &mut env,
            0,
        )
        .unwrap();

        // Root should be an If node
        assert!(matches!(
            cost_tree.expr,
            CostExprNode::NativeFunction(NativeFunctions::If)
        ));
        assert!(is_node_branching(&cost_tree));
        assert_eq!(cost_tree.children.len(), 3);

        let gt_node = &cost_tree.children[0];
        assert!(matches!(
            gt_node.expr,
            CostExprNode::NativeFunction(NativeFunctions::CmpGreater)
        ));
        assert_eq!(gt_node.children.len(), 2);

        // The comparison node has 3 children: the function name, left operand, right operand
        let left_val = &gt_node.children[0];
        let right_val = &gt_node.children[1];
        assert!(matches!(left_val.expr, CostExprNode::AtomValue(_)));
        assert!(matches!(right_val.expr, CostExprNode::AtomValue(_)));

        let ok_true_node = &cost_tree.children[1];
        assert!(matches!(
            ok_true_node.expr,
            CostExprNode::NativeFunction(NativeFunctions::ConsOkay)
        ));
        assert_eq!(ok_true_node.children.len(), 1);

        let ok_false_node = &cost_tree.children[2];
        assert!(matches!(
            ok_false_node.expr,
            CostExprNode::NativeFunction(NativeFunctions::ConsOkay)
        ));
        assert_eq!(ok_false_node.children.len(), 1);
    }

    #[test]
    fn test_build_cost_analysis_tree_arithmetic() {
        let src = "(+ (* 2 3) (- 5 1))";
        let ast = build_test_ast(src);
        let expr = &ast.expressions[0];
        let user_args = UserArgumentsContext::new();
        let cost_map = HashMap::new(); // Empty cost map for tests
        let epoch = StacksEpochId::Epoch32;
        let mut test_env = create_test_env(epoch, ClarityVersion::Clarity3);
        let mut owned_env = test_env.get_env(epoch);
        owned_env.begin();
        let contract_context = ContractContext::new(
            QualifiedContractIdentifier::transient(),
            ClarityVersion::Clarity3,
        );
        let mut env = owned_env.get_exec_environment(None, None, &contract_context);
        let (_, cost_tree) = build_cost_analysis_tree(
            expr,
            &user_args,
            &cost_map,
            &ClarityVersion::Clarity3,
            epoch,
            &mut env,
            0,
        )
        .unwrap();

        assert!(matches!(
            cost_tree.expr,
            CostExprNode::NativeFunction(NativeFunctions::Add)
        ));
        assert!(!is_node_branching(&cost_tree));
        assert_eq!(cost_tree.children.len(), 2);

        let mul_node = &cost_tree.children[0];
        assert!(matches!(
            mul_node.expr,
            CostExprNode::NativeFunction(NativeFunctions::Multiply)
        ));
        assert_eq!(mul_node.children.len(), 2);

        let sub_node = &cost_tree.children[1];
        assert!(matches!(
            sub_node.expr,
            CostExprNode::NativeFunction(NativeFunctions::Subtract)
        ));
        assert_eq!(sub_node.children.len(), 2);
    }

    #[test]
    fn test_build_cost_analysis_tree_with_comments() {
        let src = ";; This is a comment\n(+ 5 ;; another comment\n7)";
        let ast = build_test_ast(src);
        let expr = &ast.expressions[0];
        let user_args = UserArgumentsContext::new();
        let cost_map = HashMap::new(); // Empty cost map for tests
        let epoch = StacksEpochId::Epoch32;
        let mut test_env = create_test_env(epoch, ClarityVersion::Clarity3);
        let mut owned_env = test_env.get_env(epoch);
        owned_env.begin();
        let contract_context = ContractContext::new(
            QualifiedContractIdentifier::transient(),
            ClarityVersion::Clarity3,
        );
        let mut env = owned_env.get_exec_environment(None, None, &contract_context);
        let (_, cost_tree) = build_cost_analysis_tree(
            expr,
            &user_args,
            &cost_map,
            &ClarityVersion::Clarity3,
            epoch,
            &mut env,
            0,
        )
        .unwrap();

        assert!(matches!(
            cost_tree.expr,
            CostExprNode::NativeFunction(NativeFunctions::Add)
        ));
        assert!(!is_node_branching(&cost_tree));
        assert_eq!(cost_tree.children.len(), 2);

        for child in &cost_tree.children {
            assert!(matches!(child.expr, CostExprNode::AtomValue(_)));
        }
    }

    #[test]
    fn test_function_with_multiple_arguments() {
        let src = r#"(define-public (add-two (x uint) (y uint)) (+ x y))"#;
        let ast = build_test_ast(src);
        let expr = &ast.expressions[0];
        let user_args = UserArgumentsContext::new();
        let cost_map = HashMap::new(); // Empty cost map for tests
        let epoch = StacksEpochId::Epoch32;
        let mut test_env = create_test_env(epoch, ClarityVersion::Clarity3);
        let mut owned_env = test_env.get_env(epoch);
        owned_env.begin();
        let contract_context = ContractContext::new(
            QualifiedContractIdentifier::transient(),
            ClarityVersion::Clarity3,
        );
        let mut env = owned_env.get_exec_environment(None, None, &contract_context);
        let (_, cost_tree) = build_cost_analysis_tree(
            expr,
            &user_args,
            &cost_map,
            &ClarityVersion::Clarity3,
            epoch,
            &mut env,
            0,
        )
        .unwrap();

        assert_eq!(cost_tree.children.len(), 3);

        // First child should be UserArgument for (x uint)
        let user_arg_x = &cost_tree.children[0];
        assert!(matches!(user_arg_x.expr, CostExprNode::UserArgument(_, _)));
        if let CostExprNode::UserArgument(arg_name, arg_type) = &user_arg_x.expr {
            assert_eq!(arg_name.as_str(), "x");
            assert_eq!(arg_type, &TypeSignature::UIntType);
        }

        // Second child should be UserArgument for (y u64)
        let user_arg_y = &cost_tree.children[1];
        assert!(matches!(user_arg_y.expr, CostExprNode::UserArgument(_, _)));
        if let CostExprNode::UserArgument(arg_name, arg_type) = &user_arg_y.expr {
            assert_eq!(arg_name.as_str(), "y");
            assert_eq!(arg_type, &TypeSignature::UIntType);
        }

        // Third child should be the function body (+ x y)
        let body_node = &cost_tree.children[2];
        assert!(matches!(
            body_node.expr,
            CostExprNode::NativeFunction(NativeFunctions::Add)
        ));
        assert_eq!(body_node.children.len(), 2);

        // Both arguments in the body should be UserArguments
        let arg_x_ref = &body_node.children[0];
        let arg_y_ref = &body_node.children[1];
        assert!(matches!(arg_x_ref.expr, CostExprNode::UserArgument(_, _)));
        assert!(matches!(arg_y_ref.expr, CostExprNode::UserArgument(_, _)));

        if let CostExprNode::UserArgument(name, arg_type) = &arg_x_ref.expr {
            assert_eq!(name.as_str(), "x");
            assert_eq!(arg_type, &TypeSignature::UIntType);
        }
        if let CostExprNode::UserArgument(name, arg_type) = &arg_y_ref.expr {
            assert_eq!(name.as_str(), "y");
            assert_eq!(arg_type, &TypeSignature::UIntType);
        }
    }

    #[test]
    fn test_static_cost_simple_addition() {
        let source = "(define-public (add (a uint) (b uint)) (+ a b))";
        let ast_cost = static_cost_test(source, &ClarityVersion::Clarity3).unwrap();

        assert_eq!(ast_cost.len(), 1);
        assert!(ast_cost.contains_key("add"));

        let add_cost = ast_cost.get("add").unwrap();
        assert!(add_cost.min.runtime > 0);
        assert!(add_cost.max.runtime > 0);
    }

    #[test]
    fn test_static_cost_multiple_functions() {
        let source = r#"
            (define-public (func1 (x uint)) (+ x 1))
            (define-private (func2 (y uint)) (* y 2))
        "#;
        let ast_cost = static_cost_test(source, &ClarityVersion::Clarity3).unwrap();

        assert_eq!(ast_cost.len(), 2);

        assert!(ast_cost.contains_key("func1"));
        assert!(ast_cost.contains_key("func2"));

        let func1_cost = ast_cost.get("func1").unwrap();
        let func2_cost = ast_cost.get("func2").unwrap();
        assert!(func1_cost.min.runtime > 0);
        assert!(func2_cost.min.runtime > 0);
    }

    #[test]
    fn test_extract_function_name_define_public() {
        let src = "(define-public (my-func (x uint)) (ok x))";
        let ast = build_test_ast(src);
        let expr = &ast.expressions[0];
        let result = extract_function_name(expr);
        assert_eq!(result, Some("my-func".to_string()));
    }

    #[test]
    fn test_extract_function_name_function_call_not_definition() {
        // function call (not a definition) should return None
        let src = "(my-func arg1 arg2)";
        let ast = build_test_ast(src);
        let expr = &ast.expressions[0];
        let result = extract_function_name(expr);
        assert_eq!(result, None);
    }

    /// Verify that hash function costs vary with input buffer size.
    /// The runtime charges based on the serialized size of the input value,
    /// so (sha256 0x00) should cost less than (sha256 0x0011223344...).
    /// If these fail, it means static analysis is passing arg_count (always 1)
    /// instead of the input byte length.
    #[test]
    fn test_hash_function_costs_vary_with_buffer_size() {
        let v = &ClarityVersion::Clarity3;

        // Small buffer (1 byte)
        let small = static_cost_native_test("(sha256 0x00)", v).unwrap();
        // Large buffer (32 bytes)
        let large = static_cost_native_test(
            "(sha256 0x0102030405060708091011121314151617181920212223242526272829303132)",
            v,
        )
        .unwrap();

        assert!(
            large.min.runtime > small.min.runtime,
            "sha256: 32-byte input should cost more than 1-byte input, \
             but got small={}, large={}; \
             static analysis likely passes arg_count instead of input size",
            small.min.runtime,
            large.min.runtime,
        );
    }

    #[test]
    fn test_hash160_cost_varies_with_buffer_size() {
        let v = &ClarityVersion::Clarity3;

        let small = static_cost_native_test("(hash160 0x00)", v).unwrap();
        let large = static_cost_native_test(
            "(hash160 0x0102030405060708091011121314151617181920212223242526272829303132)",
            v,
        )
        .unwrap();

        assert!(
            large.min.runtime > small.min.runtime,
            "hash160: 32-byte input should cost more than 1-byte input, \
             but got small={}, large={}; \
             static analysis likely passes arg_count instead of input size",
            small.min.runtime,
            large.min.runtime,
        );
    }

    #[test]
    fn test_keccak256_cost_varies_with_buffer_size() {
        let v = &ClarityVersion::Clarity3;

        let small = static_cost_native_test("(keccak256 0x00)", v).unwrap();
        let large = static_cost_native_test(
            "(keccak256 0x0102030405060708091011121314151617181920212223242526272829303132)",
            v,
        )
        .unwrap();

        assert!(
            large.min.runtime > small.min.runtime,
            "keccak256: 32-byte input should cost more than 1-byte input, \
             but got small={}, large={}; \
             static analysis likely passes arg_count instead of input size",
            small.min.runtime,
            large.min.runtime,
        );
    }
}
