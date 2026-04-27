use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use clarity::vm::contexts::{ContractContext, ExecutionState, InvocationContext, OwnedEnvironment};
use clarity::vm::costs::ExecutionCost;
use clarity::vm::database::MemoryBackingStore;
use clarity::vm::types::{
    ListData, ListTypeData, PrincipalData, QualifiedContractIdentifier, SequenceData,
    StandardPrincipalData, TypeSignature,
};
use clarity::vm::{ast, ClarityVersion, ContractName};
use clarity_static_cost::static_cost::{
    build_cost_analysis_tree, static_cost_from_ast, static_cost_from_ast_with_options,
    static_cost_from_ast_with_source, static_cost_tree_from_ast, AnalysisContext, CostAnalysisNode,
    CostExprNode, UserArgumentsContext,
};
use indoc::indoc;
#[cfg(test)]
use rstest::rstest;
use stacks_common::types::StacksEpochId;
#[cfg(test)]
use stackslib::chainstate::stacks::boot::{
    BOOT_CODE_COSTS, BOOT_CODE_COSTS_4, BOOT_CODE_COST_VOTING_TESTNET,
};
use stackslib::util_lib::boot::boot_code_id;

/// Execute a closure with an ExecutionState and InvocationContext for cost analysis.
/// Useful for static cost analysis tests that don't require a fully deployed
/// contract context.
fn with_cost_analysis_environment<F, R>(
    owned_env: &mut OwnedEnvironment,
    contract_identifier: &QualifiedContractIdentifier,
    clarity_version: ClarityVersion,
    f: F,
) -> R
where
    F: for<'a, 'b, 'c> FnOnce(&mut ExecutionState<'a, 'b, 'c>, &InvocationContext<'a>) -> R,
{
    let contract_context = ContractContext::new(contract_identifier.clone(), clarity_version);
    let (mut env, invoke_ctx) = owned_env.get_exec_environment(None, None, &contract_context);
    f(&mut env, &invoke_ctx)
}

#[test]
fn test_build_cost_analysis_tree_function_definition() {
    let src = indoc! {r#"
        (define-public (somefunc (a uint))
          (ok (+ a 1))
        )
    "#};

    let contract_id = QualifiedContractIdentifier::transient();
    let ast = ast::parse(
        &contract_id,
        src,
        ClarityVersion::Clarity3,
        StacksEpochId::Epoch32,
    )
    .expect("Failed to parse");

    let expr = &ast[0];
    let user_args = UserArgumentsContext::new();
    let cost_map = HashMap::new();

    let clarity_version = ClarityVersion::Clarity3;
    let epoch = StacksEpochId::Epoch32;

    // Create environment for cost analysis
    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);

    let result = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| {
            let function_defs = std::collections::HashMap::new();
            let trait_impls = std::collections::HashMap::new();
            let warnings = RefCell::new(Vec::new());
            let ctx = AnalysisContext {
                cost_map: &cost_map,
                function_defs: &function_defs,
                clarity_version: &clarity_version,
                epoch,
                invoke_ctx,
                trait_implementations: &trait_impls,
                warnings: &warnings,
                current_function: None,
            };
            build_cost_analysis_tree(expr, &user_args, &ctx, env, 0)
        },
    );

    match result {
        Ok((function_name, node)) => {
            assert_eq!(function_name, Some("somefunc".to_string()));
            assert!(matches!(node.expr, CostExprNode::UserFunction(_)));
        }
        Err(e) => {
            panic!("Expected Ok result, got error: {}", e);
        }
    }
}

#[test]
fn test_let_cost() {
    let src = indoc! {r#"
        (define-public (let (a uint))
          (let ((a 1) (b 2)) (+ a b))
        )
        (define-public (let2 (a uint))
          (let ((a 1) (b 2) (c 3)) (+ a b))
        )
    "#};

    let contract_id = QualifiedContractIdentifier::transient();
    let epoch = StacksEpochId::Epoch33;
    let clarity_version = ClarityVersion::Clarity4;
    let ast =
        clarity::vm::ast::build_ast(&contract_id, src, &mut (), clarity_version, epoch).unwrap();

    // Create environment for cost analysis
    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);

    let result = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| static_cost_from_ast(&ast, &clarity_version, epoch, env, invoke_ctx),
    )
    .unwrap();
    let (let_cost, _) = result.costs.get("let").unwrap();
    let (let2_cost, _) = result.costs.get("let2").unwrap();
    assert_ne!(let2_cost.min.runtime, let_cost.min.runtime);
}

#[test]
fn test_dependent_function_calls() {
    let src = indoc! {r#"
        (define-public (add-one (a uint))
          (begin
            (print "somefunc")
            (somefunc a)
          )
        )
        (define-private (somefunc (a uint))
          (ok (+ a 1))
        )
    "#};

    let contract_id = QualifiedContractIdentifier::transient();
    let epoch = StacksEpochId::Epoch32;
    let clarity_version = ClarityVersion::Clarity3;
    let ast = clarity::vm::ast::build_ast(
        &QualifiedContractIdentifier::transient(),
        src,
        &mut (),
        clarity_version,
        epoch,
    )
    .unwrap();

    // Create environment for cost analysis
    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);

    let result = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| static_cost_from_ast(&ast, &clarity_version, epoch, env, invoke_ctx),
    )
    .unwrap();

    let (add_one_cost, _) = result.costs.get("add-one").unwrap();
    let (somefunc_cost, _) = result.costs.get("somefunc").unwrap();

    assert!(add_one_cost.min.runtime >= somefunc_cost.min.runtime);
    assert!(add_one_cost.max.runtime >= somefunc_cost.max.runtime);
}

#[test]
fn test_get_trait_count_direct() {
    let src = indoc! {r#"
        (define-trait trait-name (
            (send (uint principal) (response uint uint))
        ))
        (define-public (something (trait <trait-name>) (addresses (list 10 principal)))
            (map (send u500 trait) addresses)
        )
        (define-private (send (trait <trait-name>) (addr principal)) (trait addr))
    "#};

    let contract_id = QualifiedContractIdentifier::transient();
    let epoch = StacksEpochId::Epoch32;
    let clarity_version = ClarityVersion::Clarity3;
    let ast =
        clarity::vm::ast::build_ast(&contract_id, src, &mut (), clarity_version, epoch).unwrap();

    // Create environment for cost analysis
    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);

    let (costs, _warnings) = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| {
            static_cost_tree_from_ast(
                &ast,
                &clarity_version,
                epoch,
                &HashMap::new(),
                env,
                invoke_ctx,
            )
        },
    )
    .unwrap();

    // Extract trait_count from the result (all entries have the same trait_count)
    let trait_count = costs
        .values()
        .next()
        .and_then(|(_, trait_count)| trait_count.clone());

    let expected = {
        let mut map = HashMap::new();
        map.insert("something".to_string(), (0, 10));
        map.insert("send".to_string(), (1, 1));
        Some(map)
    };

    assert_eq!(trait_count, expected);
}

#[rstest]
fn test_trait_counting() {
    // map, fold, filter over traits counting
    let src = indoc! {r#"
        (define-trait trait-name (
            (send (uint principal) (response uint uint))
        ))
        (define-public (something (trait <trait-name>) (addresses (list 10 principal)))
            (map (send u500 trait) addresses)
        )
        (define-private (send (trait <trait-name>) (addr principal)) (trait addr))
    "#};
    let contract_id = QualifiedContractIdentifier::local("trait-counting").unwrap();
    let epoch = StacksEpochId::Epoch32;
    let ast =
        clarity::vm::ast::build_ast(&contract_id, src, &mut (), ClarityVersion::Clarity3, epoch)
            .unwrap();

    // Create environment for cost analysis
    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);
    let result = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        ClarityVersion::Clarity3,
        |env, invoke_ctx| {
            static_cost_from_ast(&ast, &ClarityVersion::Clarity3, epoch, env, invoke_ctx)
        },
    )
    .unwrap();

    let send_trait_count_map = result.costs.get("send").unwrap().1.clone().unwrap();
    let send_trait_count = send_trait_count_map.get("send").unwrap();
    assert_eq!(send_trait_count.0, 1);
    assert_eq!(send_trait_count.1, 1);

    let something_trait_count_map = result.costs.get("something").unwrap().1.clone().unwrap();
    let something_trait_count = something_trait_count_map.get("something").unwrap();
    assert_eq!(something_trait_count.0, 0);
    assert_eq!(something_trait_count.1, 10);
}

/// Helper function to pretty print the cost tree with accumulated costs
fn print_cost_tree(node: &CostAnalysisNode, depth: usize) {
    let indent = "  ".repeat(depth);
    let node_name = match &node.expr {
        CostExprNode::NativeFunction(nf) => format!("NativeFunction({:?})", nf),
        CostExprNode::UserFunction(name) => format!("UserFunction({})", name),
        CostExprNode::UserArgument(name, _) => format!("UserArgument({})", name),
        CostExprNode::AtomValue(val) => format!("AtomValue({:?})", val),
        CostExprNode::Atom(name) => format!("Atom({})", name),
        CostExprNode::FieldIdentifier(fid) => format!("FieldIdentifier({:?})", fid),
        CostExprNode::TraitReference(name) => format!("TraitReference({})", name),
        CostExprNode::NestedExpression => "NestedExpression".to_string(),
    };

    // Calculate accumulated cost including children
    let mut child_total = 0u64;
    for child in &node.children {
        child_total += child.cost.min.runtime;
        for grandchild in &child.children {
            child_total += grandchild.cost.min.runtime;
        }
    }
    let total_with_children = node.cost.min.runtime + child_total;

    println!(
        "{}{} -> node_cost: {}, children_sum: {}, total: {}",
        indent, node_name, node.cost.min.runtime, child_total, total_with_children
    );
    for child in &node.children {
        print_cost_tree(child, depth + 1);
    }
}

/// Helper function to execute a contract function and return the execution cost
fn execute_contract_function_and_get_cost(
    env: &mut OwnedEnvironment,
    contract_id: &QualifiedContractIdentifier,
    function_name: &str,
    args: &[clarity_types::Value],
) -> ExecutionCost {
    let initial_cost = env.get_cost_total();

    let sender = PrincipalData::parse_qualified_contract_principal(
        "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.sender",
    )
    .unwrap();

    // Convert u64 arguments to Value::UInt, then to SymbolicExpression::atom_value
    use clarity::vm::representations::SymbolicExpression;
    let arg_exprs: Vec<SymbolicExpression> = args
        .iter()
        .map(|v| SymbolicExpression::atom_value(v.clone()))
        .collect();

    let _result =
        env.execute_transaction(sender, None, contract_id.clone(), function_name, &arg_exprs);

    let final_cost = env.get_cost_total();

    ExecutionCost {
        write_length: final_cost.write_length - initial_cost.write_length,
        write_count: final_cost.write_count - initial_cost.write_count,
        read_length: final_cost.read_length - initial_cost.read_length,
        read_count: final_cost.read_count - initial_cost.read_count,
        runtime: final_cost.runtime - initial_cost.runtime,
    }
}

/// Test for asserting that costs parsing works for pox-4 contract
#[test]
fn test_pox_4_costs() {
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let pox_4_path = workspace_root
        .join("clarity-repl")
        .join("src")
        .join("repl")
        .join("boot")
        .join("pox-4.clar");
    let contract_source = std::fs::read_to_string(&pox_4_path)
        .unwrap_or_else(|e| panic!("Failed to read pox-4.clar file at {:?}: {}", pox_4_path, e));

    let contract_id = QualifiedContractIdentifier::transient();
    let epoch = StacksEpochId::Epoch32;
    let clarity_version = ClarityVersion::Clarity3;

    let ast = clarity::vm::ast::build_ast(
        &contract_id,
        &contract_source,
        &mut (),
        clarity_version,
        epoch,
    )
    .expect("Failed to build AST from pox-4.clar");

    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);
    let result = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| static_cost_from_ast(&ast, &clarity_version, epoch, env, invoke_ctx),
    )
    .unwrap();

    // Check some functions in the cost map
    let key_functions = vec![
        "stack-stx",
        "delegate-stx",
        "get-stacker-info",
        "current-pox-reward-cycle",
        "stack-aggregation-commit",
        "stack-increase",
        "stack-extend",
    ];

    for function_name in key_functions {
        assert!(
            result.costs.contains_key(function_name),
            "Expected function '{}' to be present in cost map",
            function_name
        );

        let (_cost, _trait_count) = result
            .costs
            .get(function_name)
            .unwrap_or_else(|| panic!("Failed to get cost for function '{}'", function_name));
    }
}

// Helper function to run static and dynamic cost analysis on a contract function
// Returns Ok(()) if costs are within expected range, Err with message otherwise.
// When `exact` is true, asserts that min == max == dynamic for all fields
// (appropriate when all values are known statically with no user input).
#[cfg(test)]
fn run_cost_analysis_test(
    src: &str,
    function_name: &str,
    args: &[clarity_types::Value],
    epoch: StacksEpochId,
    clarity_version: ClarityVersion,
    exact: bool,
) -> Result<(), String> {
    let contract_id = QualifiedContractIdentifier::local("test-contract").unwrap();

    // Set up environment for dynamic cost analysis
    let mut memory_store = MemoryBackingStore::new();
    let mut db = memory_store.as_clarity_db();
    db.begin();
    db.set_clarity_epoch_version(epoch).unwrap();
    db.commit().unwrap();
    if epoch.clarity_uses_tip_burn_block() {
        db.begin();
        db.set_tenure_height(1).unwrap();
        db.commit().unwrap();
    }
    if epoch.uses_marfed_block_time() {
        db.begin();
        db.setup_block_metadata(Some(1)).unwrap();
        db.commit().unwrap();
    }

    // Initialize the costs and cost-voting contracts so we can use cost tracking
    // The cost tracker needs both contracts to be initialized
    // For epoch 33, we need costs-4 (not just costs)
    let costs_contract_id = boot_code_id("costs", false);
    let costs_4_contract_id = boot_code_id("costs-4", false);
    let cost_voting_contract_id = boot_code_id("cost-voting", false);
    {
        // Use a temporary free environment to initialize the contracts
        let mut temp_env = OwnedEnvironment::new(db, epoch);

        // Initialize costs contract (costs-1, needed as base)
        temp_env
            .initialize_versioned_contract(
                costs_contract_id.clone(),
                clarity_version,
                BOOT_CODE_COSTS,
                None,
            )
            .expect("Failed to initialize costs contract");

        // Initialize costs-4 contract (required for epoch 33)
        temp_env
            .initialize_versioned_contract(
                costs_4_contract_id.clone(),
                clarity_version,
                BOOT_CODE_COSTS_4,
                None,
            )
            .expect("Failed to initialize costs-4 contract");

        // Initialize cost-voting contract (required for cost tracker to read confirmed-proposal-count)
        temp_env
            .initialize_versioned_contract(
                cost_voting_contract_id.clone(),
                clarity_version,
                &BOOT_CODE_COST_VOTING_TESTNET.to_string(),
                None,
            )
            .expect("Failed to initialize cost-voting contract");

        // Extract the database from the environment
        let (extracted_db, _cost_tracker) = temp_env
            .destruct()
            .expect("Failed to extract database from environment");
        db = extracted_db;
    }

    // Now create environment with cost tracking enabled
    let mut owned_env = OwnedEnvironment::new_max_limit(db, epoch, false);

    // Deploy the contract
    owned_env
        .initialize_versioned_contract(contract_id.clone(), clarity_version, src, None)
        .expect("Failed to initialize contract");

    // Run dynamic cost analysis
    let dynamic_cost =
        execute_contract_function_and_get_cost(&mut owned_env, &contract_id, function_name, args);

    // Build AST for static cost analysis
    let ast = ast::build_ast(&contract_id, src, &mut (), clarity_version, epoch)
        .expect("Failed to build AST");

    // Run static cost analysis with source string for accurate contract size
    let static_cost_map = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| {
            static_cost_from_ast_with_source(
                &ast,
                &clarity_version,
                epoch,
                Some(src),
                env,
                invoke_ctx,
            )
        },
    )
    .expect("Failed to get static cost analysis");

    let (static_cost, _) = static_cost_map
        .costs
        .get(function_name)
        .unwrap_or_else(|| panic!("Function '{}' not found in static cost map", function_name));

    println!("\n=== Cost Analysis for {} ===", function_name);
    println!("static cost: {:?}", static_cost);
    println!("dynamic cost: {:?}", dynamic_cost);

    // Get the cost tree to debug and print it with values
    let (cost_trees_with_traits, _warnings) = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| {
            static_cost_tree_from_ast(
                &ast,
                &clarity_version,
                epoch,
                &HashMap::new(),
                env,
                invoke_ctx,
            )
        },
    )
    .expect("Failed to get static cost tree");
    if let Some((cost_tree, _)) = cost_trees_with_traits.get(function_name) {
        println!("\n=== Cost Tree for {} ===", function_name);
        print_cost_tree(cost_tree, 0);
    }

    let fields: &[(&str, fn(&ExecutionCost) -> u64)] = &[
        ("runtime", |c| c.runtime),
        ("write_length", |c| c.write_length),
        ("write_count", |c| c.write_count),
        ("read_length", |c| c.read_length),
        ("read_count", |c| c.read_count),
    ];

    for &(name, get) in fields {
        let min = get(&static_cost.min);
        let max = get(&static_cost.max);
        let dynamic = get(&dynamic_cost);

        if exact {
            if min != max {
                return Err(format!(
                    "{name}: expected exact cost but min {min} != max {max}"
                ));
            }
            if dynamic != min {
                return Err(format!(
                    "{name}: expected exact cost {min} but dynamic is {dynamic}"
                ));
            }
        } else {
            if min > max {
                return Err(format!("{name}: static min {min} should be <= max {max}"));
            }
            if dynamic < min {
                return Err(format!(
                    "{name}: dynamic {dynamic} is LESS than static min {min}"
                ));
            }
            if dynamic > max {
                return Err(format!(
                    "{name}: dynamic {dynamic} is MORE than static max {max}"
                ));
            }
        }
    }

    Ok(())
}

// given a contract source, run dynamic cost analysis on pre-determined input
// arguments, followed by static cost analysis on the same source and confirm
// that the dynamic cost is between the min/max static cost
#[test]
fn test_against_dynamic_cost_analysis() {
    let epoch = StacksEpochId::Epoch33;
    let clarity_version = ClarityVersion::Clarity4;

    let uint_value = [clarity_types::Value::UInt(1)];
    let multi_arg_value = [
        clarity_types::Value::string_ascii_from_bytes("".to_string().into_bytes()).unwrap(),
        clarity_types::Value::UInt(1),
    ];
    let value =
        [clarity_types::Value::string_ascii_from_bytes("".to_string().into_bytes()).unwrap()];
    let _max_value =
        [
            clarity_types::Value::string_ascii_from_bytes("aaaaaaaaaa".to_string().into_bytes())
                .unwrap(),
        ];
    let if_args = [
        clarity_types::Value::string_ascii_from_bytes("a".to_string().into_bytes()).unwrap(),
        clarity_types::Value::UInt(1),
    ];
    let list_32_uint_value = clarity_types::Value::Sequence(SequenceData::List(ListData {
        data: vec![clarity_types::Value::UInt(1); 32],
        type_signature: ListTypeData::new_list(TypeSignature::UIntType, 32).unwrap(),
    }));
    let list_32_uint_args = [list_32_uint_value];
    let allow_contract_caller_args = [
        clarity_types::Value::Principal(
            PrincipalData::Standard(StandardPrincipalData::transient()),
        ),
        clarity_types::Value::none(),
    ];
    // Define test cases as (source, function_name, args, exact)
    // When `exact` is true, asserts min == max == dynamic (all values known statically).
    let test_cases: Vec<(&str, &str, &[clarity_types::Value], bool)> = vec![
        (
            indoc! {r#"
                (define-public (let-func (a uint))
                  (let ((b 1))
                    (ok (+ a b))
                  )
                )
            "#},
            "let-func",
            &uint_value,
            false,
        ),
        (
            indoc! {r#"
                (define-public (if-func (a (string-ascii 10)) (b uint))
                  (if (> b u0)
                    (ok a)
                    (ok "aaaaaaaaaa")
                  )
                )
            "#},
            "if-func",
            &if_args,
            false,
        ),
        (
            indoc! {r#"
                (define-public (simple-str-min (a (string-ascii 10)))
                  (ok a)
                )
            "#},
            "simple-str-min",
            &value,
            false,
        ),
        (
            indoc! {r#"
                (define-public (simple-str (a (string-ascii 10)) (b uint))
                  (ok a)
                )
            "#},
            "simple-str",
            &multi_arg_value,
            false,
        ),
        (
            indoc! {r#"
                (define-public (simple-constant)
                  (ok u1)
                )
            "#},
            "simple-constant",
            &[],
            true,
        ),
        (
            indoc! {r#"
                (define-public (nested-ops)
                  (* (+ u1 u2) (- u3 u4))
                )
            "#},
            "nested-ops",
            &[],
            true,
        ),
        (
            indoc! {r#"
                (define-public (string-concat)
                  (ok (concat "hello" "world"))
                )
            "#},
            "string-concat",
            &[],
            true,
        ),
        (
            indoc! {r#"
                (define-public (string-len)
                  (ok (len "hello"))
                )
            "#},
            "string-len",
            &[],
            true,
        ),
        (
            indoc! {r#"
                (define-public (if-simple)
                  (if (> 3 0) (ok u1) (ok u2))
                )
            "#},
            "if-simple",
            &[],
            false,
        ),
        (
            indoc! {r#"
                (define-public (if-no-ok)
                  (if (> 3 0) u1 u2)
                )
            "#},
            "if-no-ok",
            &[],
            false,
        ),
        (
            indoc! {r#"
                (define-public (if-one-ok)
                  (if (> 3 0) (ok u1) u2)
                )
            "#},
            "if-one-ok",
            &[],
            false,
        ),
        (
            indoc! {r#"
                (define-public (if-other-branch-ok)
                  (if (> 3 0) u1 (ok u2))
                )
            "#},
            "if-other-branch-ok",
            &[],
            false,
        ),
        (
            indoc! {r#"
                (define-public (if-with-ok-concat)
                  (if (> 3 0) (ok (concat "hello" "world")) (ok u1))
                )
            "#},
            "if-with-ok-concat",
            &[],
            false,
        ),
        (
            indoc! {r#"
                (define-public (if-with-ok-string)
                  (if (> 3 0) (ok "asdf") (ok u1))
                )
            "#},
            "if-with-ok-string",
            &[],
            false,
        ),
        (
            indoc! {r#"
                (define-public (if-both-ok-concat)
                  (if (> 3 0) (ok (concat "hello" "world")) (ok (concat "foo" "bar")))
                )
            "#},
            "if-both-ok-concat",
            &[],
            false,
        ),
        (
            indoc! {r#"
                (define-public (branching)
                  (if (> 3 0) (ok (concat "hello" "world")) (ok "asdf"))
                )
            "#},
            "branching",
            &[],
            false,
        ),
        (
            indoc! {r#"
                (define-constant ERR_STACKING_PERMISSION_DENIED 9)
                (define-map allowance-contract-callers
                    { sender: principal, contract-caller: principal }
                    { until-burn-ht: (optional uint) })
                (define-public (allow-contract-caller (caller principal) (until-burn-ht (optional uint)))
                  (begin
                    (asserts! (is-eq tx-sender contract-caller)
                              (err ERR_STACKING_PERMISSION_DENIED))
                    (ok (map-set allowance-contract-callers
                               { sender: tx-sender, contract-caller: caller }
                               { until-burn-ht: until-burn-ht }))))
            "#},
            "allow-contract-caller",
            &allow_contract_caller_args,
            false,
        ),
        // let-bound variable used directly as map key atom (not in user_args)
        (
            indoc! {r#"
                (define-map my-map { key: uint } uint)
                (define-public (let-bound-key (a uint))
                  (let ((k { key: a }))
                    (ok (map-set my-map k u1))))
            "#},
            "let-bound-key",
            &uint_value,
            false,
        ),
        // contract data variable (var-get) used as map value — a list expression, not a tuple/atom
        (
            indoc! {r#"
                (define-data-var stored-val uint u0)
                (define-map my-map uint uint)
                (define-public (contract-data-val)
                  (ok (map-set my-map u1 (var-get stored-val))))
            "#},
            "contract-data-val",
            &[],
            false,
        ),
        // nft-mint? cost is based on asset-identifier size
        (
            indoc! {r#"
                (define-non-fungible-token my-nft uint)
                (define-public (mint-nft)
                  (nft-mint? my-nft u1 tx-sender))
            "#},
            "mint-nft",
            &[],
            true,
        ),
        // branching inside begin with subsequent siblings — verifies that
        // the cost of expressions after an if is included in the static estimate
        (
            indoc! {r#"
                (define-public (if-then-add)
                  (begin
                    (if (> u3 u0) u1 u2)
                    (ok (+ u1 u2))
                  )
                )
            "#},
            "if-then-add",
            &[],
            false,
        ),
        // --- hash function tests ---
        // sha256 with small (1-byte) buffer
        (
            indoc! {r#"
                (define-public (sha256-small)
                  (ok (sha256 0x00)))
            "#},
            "sha256-small",
            &[],
            true,
        ),
        // sha256 with large (32-byte) buffer
        (
            indoc! {r#"
                (define-public (sha256-large)
                  (ok (sha256 0x0102030405060708091011121314151617181920212223242526272829303132)))
            "#},
            "sha256-large",
            &[],
            true,
        ),
        // hash160 with small (1-byte) buffer
        (
            indoc! {r#"
                (define-public (hash160-small)
                  (ok (hash160 0x00)))
            "#},
            "hash160-small",
            &[],
            true,
        ),
        // hash160 with large (32-byte) buffer
        (
            indoc! {r#"
                (define-public (hash160-large)
                  (ok (hash160 0x0102030405060708091011121314151617181920212223242526272829303132)))
            "#},
            "hash160-large",
            &[],
            true,
        ),
        // keccak256 with small (1-byte) buffer
        (
            indoc! {r#"
                (define-public (keccak256-small)
                  (ok (keccak256 0x00)))
            "#},
            "keccak256-small",
            &[],
            true,
        ),
        // keccak256 with large (32-byte) buffer
        (
            indoc! {r#"
                (define-public (keccak256-large)
                  (ok (keccak256 0x0102030405060708091011121314151617181920212223242526272829303132)))
            "#},
            "keccak256-large",
            &[],
            true,
        ),
        // var-set with uint data variable
        (
            indoc! {r#"
                (define-data-var my-var uint u0)
                (define-public (set-var-uint)
                  (ok (var-set my-var u42)))
            "#},
            "set-var-uint",
            &[],
            true,
        ),
        // var-set with bool data variable
        (
            indoc! {r#"
                (define-data-var my-flag bool false)
                (define-public (set-var-bool)
                  (ok (var-set my-flag true)))
            "#},
            "set-var-bool",
            &[],
            true,
        ),
        // var-set with short string literal
        (
            indoc! {r#"
                (define-data-var message (string-ascii 521) "")
                (define-public (set-var-short)
                  (ok (var-set message "hello")))
            "#},
            "set-var-short",
            &[],
            true,
        ),
        // var-set with longer string literal
        (
            indoc! {r#"
                (define-data-var message (string-ascii 521) "")
                (define-public (set-var-long)
                  (ok (var-set message "hello world")))
            "#},
            "set-var-long",
            &[],
            true,
        ),
        // --- map-get? tests ---
        // map-get? with uint key/value (key not found)
        (
            indoc! {r#"
                (define-map my-map uint uint)
                (define-public (fetch-entry-uint)
                  (ok (map-get? my-map u1)))
            "#},
            "fetch-entry-uint",
            &[],
            false,
        ),
        // map-get? after map-set (key found)
        (
            indoc! {r#"
                (define-map my-map uint uint)
                (define-public (fetch-entry-after-set)
                  (begin
                    (map-set my-map u1 u42)
                    (ok (map-get? my-map u1))))
            "#},
            "fetch-entry-after-set",
            &[],
            false,
        ),
        // map over a list calling a private function with var-set
        (
            indoc! {r#"
                (define-data-var count uint u0)

                (define-private (add (n uint))
                  (begin
                    (var-set count (+ (var-get count) n))
                  )
                )

                (define-public (add-many-32 (ns (list 32 uint)))
                  (begin
                    (map add ns)
                    (ok true)
                  )
                )
            "#},
            "add-many-32",
            &list_32_uint_args,
            false,
        ),
        // call a function that maps over a list, passing a shorter literal list
        (
            indoc! {r#"
                (define-data-var count uint u0)

                (define-private (add (n uint))
                  (begin
                    (var-set count (+ (var-get count) n))
                  )
                )

                (define-public (add-many-64 (ns (list 64 uint)))
                  (begin
                    (map add ns)
                    (ok true)
                  )
                )

                (define-public (add-u1)
                  (add-many-64 (list u1))
                )
            "#},
            "add-u1",
            &[],
            true,
        ),
        // Buffer narrowing: pass a 1-byte literal to a function declared `(buff 128)`.
        // Static cost should match the dynamic cost of writing only 1 byte.
        (
            indoc! {r#"
                (define-data-var data-buff (buff 128) 0x)

                (define-private (write-buff (data (buff 128)))
                  (var-set data-buff data)
                )

                (define-public (do-write-buff)
                  (ok (write-buff 0x01))
                )
            "#},
            "do-write-buff",
            &[],
            true,
        ),
        // String narrowing: pass a short literal to a function declared `(string-ascii 100)`.
        (
            indoc! {r#"
                (define-data-var msg (string-ascii 100) "")

                (define-private (write-msg (s (string-ascii 100)))
                  (var-set msg s)
                )

                (define-public (do-write-msg)
                  (ok (write-msg "hi"))
                )
            "#},
            "do-write-msg",
            &[],
            true,
        ),
        // Map over a literal list: cost should be 3x per-iteration, not 64x.
        (
            indoc! {r#"
                (define-data-var count uint u0)

                (define-private (add (n uint))
                  (begin
                    (var-set count (+ (var-get count) n))
                  )
                )

                (define-public (map-literal)
                  (begin
                    (map add (list u1 u2 u3))
                    (ok true)
                  )
                )
            "#},
            "map-literal",
            &[],
            true,
        ),
        // `if true` folding: literal true selects the write branch.
        (
            indoc! {r#"
                (define-data-var data-buff (buff 128) 0x)

                (define-private (write-if-true (write bool) (data (buff 128)))
                  (if write (var-set data-buff data) false)
                )

                (define-public (do-write)
                  (ok (write-if-true true 0x01))
                )
            "#},
            "do-write",
            &[],
            true,
        ),
        // `if false` folding: literal false selects the no-op branch.
        (
            indoc! {r#"
                (define-data-var data-buff (buff 128) 0x)

                (define-private (write-if-true (write bool) (data (buff 128)))
                  (if write (var-set data-buff data) false)
                )

                (define-public (dont-write)
                  (ok (write-if-true false 0x00))
                )
            "#},
            "dont-write",
            &[],
            true,
        ),
        // Indirect constant: a parent function whose own arg is a constant still
        // folds when inlined into the child via known-value propagation.
        (
            indoc! {r#"
                (define-data-var data-buff (buff 128) 0x)

                (define-private (write-if-true (write bool) (data (buff 128)))
                  (if write (var-set data-buff data) false)
                )

                (define-private (always-true)
                  (write-if-true true 0x01)
                )

                (define-public (do-write-indirect)
                  (ok (always-true))
                )
            "#},
            "do-write-indirect",
            &[],
            true,
        ),
    ];

    let mut failures = Vec::new();
    for (src, function_name, args, exact) in test_cases {
        match run_cost_analysis_test(src, function_name, args, epoch, clarity_version, exact) {
            Ok(()) => println!("Passed: {}", function_name),
            Err(e) => {
                eprintln!("✗ Test case {} failed: {}", function_name, e);
                failures.push((function_name, e));
            }
        }
    }

    if !failures.is_empty() {
        let error_msg = failures
            .iter()
            .map(|(name, err)| format!("{}: {}", name, err))
            .collect::<Vec<_>>()
            .join("\n");
        panic!("{} test case(s) failed:\n{}", failures.len(), error_msg);
    }
}

/// Test that contract-call? includes the cost of the called function from
/// the target contract.
/// The static cost of the caller function should include the callee's cost,
/// and the dynamic cost should fall within the static min/max range.
#[test]
fn test_contract_call_includes_callee_cost() {
    let epoch = StacksEpochId::Epoch33;
    let clarity_version = ClarityVersion::Clarity4;

    let callee_src = indoc! {r#"
        (define-data-var count uint u0)

        (define-private (add (n uint))
          (begin
            (var-set count (+ (var-get count) n))
          )
        )

        (define-public (add-many-32 (ns (list 32 uint)))
          (begin
            (map add ns)
            (ok true)
          )
        )
    "#};

    let caller_src = indoc! {r#"
        (define-public (call-counter (ns (list 32 uint)))
          (contract-call? .counter add-many-32 ns)
        )
    "#};

    let deployer = StandardPrincipalData::transient();
    let callee_id =
        QualifiedContractIdentifier::new(deployer.clone(), ContractName::from_literal("counter"));
    let caller_id =
        QualifiedContractIdentifier::new(deployer, ContractName::from_literal("caller"));

    // Set up environment with cost tracking
    let mut memory_store = MemoryBackingStore::new();
    let mut db = memory_store.as_clarity_db();
    db.begin();
    db.set_clarity_epoch_version(epoch).unwrap();
    db.commit().unwrap();
    db.begin();
    db.set_tenure_height(1).unwrap();
    db.commit().unwrap();
    db.begin();
    db.setup_block_metadata(Some(1)).unwrap();
    db.commit().unwrap();

    let costs_contract_id = boot_code_id("costs", false);
    let costs_4_contract_id = boot_code_id("costs-4", false);
    let cost_voting_contract_id = boot_code_id("cost-voting", false);
    {
        let mut temp_env = OwnedEnvironment::new(db, epoch);
        temp_env
            .initialize_versioned_contract(
                costs_contract_id,
                clarity_version,
                BOOT_CODE_COSTS,
                None,
            )
            .expect("Failed to initialize costs contract");
        temp_env
            .initialize_versioned_contract(
                costs_4_contract_id,
                clarity_version,
                BOOT_CODE_COSTS_4,
                None,
            )
            .expect("Failed to initialize costs-4 contract");
        temp_env
            .initialize_versioned_contract(
                cost_voting_contract_id,
                clarity_version,
                &BOOT_CODE_COST_VOTING_TESTNET.to_string(),
                None,
            )
            .expect("Failed to initialize cost-voting contract");
        let (extracted_db, _) = temp_env.destruct().unwrap();
        db = extracted_db;
    }

    let mut owned_env = OwnedEnvironment::new_max_limit(db, epoch, false);

    // Deploy callee first, then caller
    owned_env
        .initialize_versioned_contract(callee_id.clone(), clarity_version, callee_src, None)
        .expect("Failed to deploy counter contract");
    owned_env
        .initialize_versioned_contract(caller_id.clone(), clarity_version, caller_src, None)
        .expect("Failed to deploy caller contract");

    // Run dynamic cost analysis on the caller (pass 32 elements to match the
    // max list length that static analysis uses for worst-case costing)
    let list_value = clarity_types::Value::Sequence(SequenceData::List(ListData {
        data: vec![clarity_types::Value::UInt(1); 32],
        type_signature: ListTypeData::new_list(TypeSignature::UIntType, 32).unwrap(),
    }));
    let dynamic_cost = execute_contract_function_and_get_cost(
        &mut owned_env,
        &caller_id,
        "call-counter",
        &[list_value],
    );

    // Run static cost analysis on the caller.
    // We need an active DB transaction so that get_contract_call_target_cost
    // can look up the callee contract source from the database.
    let caller_ast = ast::build_ast(&caller_id, caller_src, &mut (), clarity_version, epoch)
        .expect("Failed to build caller AST");
    owned_env.begin();
    let static_cost_map = {
        let contract_context = ContractContext::new(caller_id.clone(), clarity_version);
        let (mut env, invoke_ctx) = owned_env.get_exec_environment(None, None, &contract_context);
        static_cost_from_ast_with_source(
            &caller_ast,
            &clarity_version,
            epoch,
            Some(caller_src),
            &mut env,
            &invoke_ctx,
        )
        .expect("Failed to get static cost analysis")
    };
    let _ = owned_env.commit();

    let (static_cost, _) = static_cost_map
        .costs
        .get("call-counter")
        .expect("call-counter not found in static cost map");

    println!("\n=== Contract-call? cost test ===");
    println!("static cost: {:?}", static_cost);
    println!("dynamic cost: {:?}", dynamic_cost);

    assert!(
        static_cost.max.runtime > 1000,
        "Static cost should include callee function cost, got max runtime: {}",
        static_cost.max.runtime
    );

    // Dynamic cost should fall within the static range
    assert!(
        dynamic_cost.runtime >= static_cost.min.runtime,
        "Dynamic cost runtime {} is LESS than static min runtime {}",
        dynamic_cost.runtime,
        static_cost.min.runtime
    );
    assert!(
        dynamic_cost.runtime <= static_cost.max.runtime,
        "Dynamic cost runtime {} is MORE than static max runtime {}",
        dynamic_cost.runtime,
        static_cost.max.runtime
    );
}

#[test]
fn test_trait_counts_simplified() {
    // Simplified test case to debug trait counting
    // This focuses on the key scenarios:
    // 1. CONTEXT function with trait parameters and struct parameter with trait field
    // 2. mint function that calls CONTEXT and uses trait parameters
    let contract_src = indoc! {r#"
        (use-trait ft-trait       'SP2AKWJYC7BNY18W1XXKPGP0YVEK63QJG4793Z2D4.sip-010-trait-ft-standard.sip-010-trait)
        (use-trait lp-token-trait 'SP1Y5YSTAHZ88XYK1VPDH24GY0HPX5J4JECTMY4A1.ft-plus-trait.ft-plus-trait)
        (use-trait oracle-trait   'SP2AKWJYC7BNY18W1XXKPGP0YVEK63QJG4793Z2D4.oracle-trait.oracle-trait)

        (define-private (call-get-decimals (token <ft-trait>))
          (unwrap-panic (contract-call? token get-decimals)))

        (define-private
          (CONTEXT
            (base-token   <ft-trait>)
            (quote-token  <ft-trait>)
            (ctx          { oracle: <oracle-trait> }))
          (let ((base-decimals  (call-get-decimals base-token))
                (quote-decimals (call-get-decimals quote-token))
                (oracle1        (get oracle ctx))
                (price          (try! (contract-call? oracle1 price u1 u2))))
            (ok {
                base-decimals : base-decimals,
                quote-decimals: quote-decimals,
                price         : price,
                })))

        (define-public
          (mint
            (base-token   <ft-trait>)
            (quote-token  <ft-trait>)
            (lp-token     <lp-token-trait>)
            (ctx0         { oracle: <oracle-trait> }))
          (let ((ctx (try! (CONTEXT base-token quote-token ctx0))))
            (contract-call? .gl-core mint u1 base-token quote-token lp-token ctx)
          ))
    "#};

    let contract_id = QualifiedContractIdentifier::transient();
    let epoch = StacksEpochId::Epoch32;
    let clarity_version = ClarityVersion::Clarity3;
    let ast =
        clarity::vm::ast::build_ast(&contract_id, contract_src, &mut (), clarity_version, epoch)
            .expect("Failed to build AST");

    // Create environment for cost analysis
    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);

    let static_cost_map = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| static_cost_from_ast(&ast, &clarity_version, epoch, env, invoke_ctx),
    )
    .expect("Failed to get static cost analysis");

    let trait_count = static_cost_map
        .costs
        .values()
        .next()
        .and_then(|(_, trait_count)| trait_count.clone());

    // Assert that there is at least some payload for the trait counts when getting static_cost_from_ast
    assert!(trait_count.is_some());

    let trait_count_map = trait_count.unwrap();

    // CONTEXT should have:
    // - base-token passed to call-get-decimals = 1
    // - quote-token passed to call-get-decimals = 1
    // - call-get-decimals propagation (internal contract-call?) = 1 (for base-token) + 1 (for quote-token) = 2
    // - ctx oracle field access = 1
    // - oracle1 in contract-call? = 1
    // Total = 6
    assert!(trait_count_map.contains_key("CONTEXT"));
    let (_context_min, context_max) = trait_count_map.get("CONTEXT").unwrap();
    assert_eq!(*context_max, 6);

    // mint should have:
    // - CONTEXT propagation = 6
    // - base-token passed to CONTEXT = 1 (trait parameter passed to function that uses traits)
    // - quote-token passed to CONTEXT = 1 (trait parameter passed to function that uses traits)
    // - ctx0 passed to CONTEXT = 0 (struct type, not counted as trait parameter)
    // - base-token in contract-call? = 1 (trait parameter passed as argument)
    // - quote-token in contract-call? = 1 (trait parameter passed as argument)
    // - lp-token in contract-call? = 1 (trait parameter passed as argument)
    // - ctx in contract-call? = 0 (Atom/let-bound variable, not counted in contract-call? args)
    // Total = 6 + 1 + 1 + 0 + 1 + 1 + 1 + 0 = 11
    assert!(trait_count_map.contains_key("mint"));
    let (_mint_min, mint_max) = trait_count_map.get("mint").unwrap();
    assert_eq!(*mint_max, 11);
}

#[test]
fn test_trait_counts_let_bound_variable() {
    // Test that trait counts are correctly propagated when functions are called
    // inside let bindings.
    let contract_src = indoc! {r#"
        (use-trait ft-trait 'SP2AKWJYC7BNY18W1XXKPGP0YVEK63QJG4793Z2D4.sip-010-trait-ft-standard.sip-010-trait)

        (define-private (helper (token <ft-trait>))
          (contract-call? token get-decimals))

        (define-public (main (token <ft-trait>))
          (let ((result (try! (helper token))))
            (ok result)
          ))
    "#};

    let contract_id = QualifiedContractIdentifier::transient();
    let epoch = StacksEpochId::Epoch32;
    let clarity_version = ClarityVersion::Clarity3;
    let ast =
        clarity::vm::ast::build_ast(&contract_id, contract_src, &mut (), clarity_version, epoch)
            .expect("Failed to build AST");

    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);

    let static_cost_map = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| static_cost_from_ast(&ast, &clarity_version, epoch, env, invoke_ctx),
    )
    .expect("Failed to get static cost analysis");

    let trait_count = static_cost_map
        .costs
        .values()
        .next()
        .and_then(|(_, trait_count)| trait_count.clone());

    assert!(trait_count.is_some());
    let trait_count_map = trait_count.unwrap();

    // helper should have trait counts (it uses token in contract-call?)
    assert!(trait_count_map.contains_key("helper"));
    let (_helper_min, helper_max) = trait_count_map.get("helper").unwrap();
    assert_eq!(
        *helper_max, 1,
        "helper should count token passed to contract-call?"
    );

    // main should inherit trait counts from helper AND count token passed to helper
    // Breakdown:
    // - helper propagation: 1 (token used in contract-call?)
    // - token passed to helper: 1
    // Total = 2
    assert!(trait_count_map.contains_key("main"));
    let (_main_min, main_max) = trait_count_map.get("main").unwrap();
    assert_eq!(
        *main_max, 2,
        "main should inherit from helper and count token parameter"
    );
}

#[test]
fn test_trait_counts_for_gl_contract() {
    let contract_src = indoc! {r#"
        (use-trait ft-trait       'SP2AKWJYC7BNY18W1XXKPGP0YVEK63QJG4793Z2D4.sip-010-trait-ft-standard.sip-010-trait)
        (use-trait lp-token-trait 'SP1Y5YSTAHZ88XYK1VPDH24GY0HPX5J4JECTMY4A1.ft-plus-trait.ft-plus-trait)
        (use-trait oracle-trait   .gl-oracle-trait-pyth.oracle-trait)

        (define-constant err-lock        (err u701))
        (define-constant err-oracle      (err u702))
        (define-constant err-permissions (err u700))

        (define-private (call-get-decimals (token <ft-trait>))
          (unwrap-panic (contract-call? token get-decimals)))

        (define-data-var owner principal tx-sender)
        (define-read-only (get-owner) (var-get owner))
        (define-public (set-owner (new-owner principal))
          (begin
           (try! (OWNER))
           (ok (var-set owner new-owner)) ))

        (define-private
         (OWNER)
         (begin
          (asserts! (is-eq contract-caller (get-owner)) err-permissions)
          (ok true)))

        (define-data-var oracle principal .gl-oracle-pyth)
        (define-public (set-oracle (oracle0 <oracle-trait>))
          (begin
            (try! (OWNER))
            (ok (var-set oracle (contract-of oracle0)))))

        ;; (ctx0         { identifier: (buff 32), message: (buff 8192), oracle: <oracle-trait> }))

        (define-private
          (CONTEXT
            (base-token   <ft-trait>)
            (quote-token  <ft-trait>)
            (desired      uint)
            (slippage     uint)
            (ctx          {
                          identifier: (buff 32),
                          message   : (buff 8192),
                          oracle    : <oracle-trait>,
                          }))
          (let ((base-decimals  (call-get-decimals base-token))
                (quote-decimals (call-get-decimals quote-token))
                (oracle1        (get oracle ctx))
                (price          (try! (contract-call? oracle1 price quote-decimals desired slippage
                                        (get identifier ctx)
                                        (get message ctx)))) )

          (asserts! (is-eq (contract-of oracle1) (var-get oracle)) err-oracle)

          (ok {
              price         : price,
              base-decimals : base-decimals,
              quote-decimals: quote-decimals,
              })))

        (define-map LOCK principal uint)

        (define-private (check-unlocked)
          (if (is-eq
                (default-to u0 (map-get? LOCK tx-sender))
                stacks-block-height)
            err-lock
            (ok true)
          ))

        (define-private (lock)
          (begin
            (try! (check-unlocked))
            (ok (map-set LOCK  tx-sender stacks-block-height))))

        (define-public
          (mint
            (base-token   <ft-trait>)
            (quote-token  <ft-trait>)
            (lp-token     <lp-token-trait>)
            (base-amt     uint)
            (quote-amt    uint)
            (desired      uint)
            (slippage     uint)
            (ctx0         { identifier: (buff 32), message: (buff 8192), oracle: <oracle-trait> }))

            (let ((ctx (try! (CONTEXT base-token quote-token desired slippage ctx0))))
              (try! (lock))
              (contract-call? .gl-core mint u1 base-token quote-token lp-token base-amt quote-amt ctx)
            ))

        (define-public
          (burn
            (base-token   <ft-trait>)
            (quote-token  <ft-trait>)
            (lp-token    <lp-token-trait>)
            (lp-amt       uint)
            (desired      uint)
            (slippage     uint)
            (ctx0         { identifier: (buff 32), message: (buff 8192), oracle: <oracle-trait> }))

            (let ((ctx (try! (CONTEXT base-token quote-token desired slippage ctx0))))
              (try! (lock))
              (contract-call? .gl-core burn u1 base-token quote-token lp-token lp-amt ctx)
            ))

        (define-public
          (open
            (base-token   <ft-trait>)
            (quote-token  <ft-trait>)
            (long         bool)
            (collateral   uint)
            (leverage     uint)
            (desired      uint)
            (slippage     uint)
            (ctx0         { identifier: (buff 32), message: (buff 8192), oracle: <oracle-trait> }))

          (let ((ctx (try! (CONTEXT base-token quote-token desired slippage ctx0))))
            (try! (lock))
             (contract-call? .gl-core open u1 base-token quote-token long collateral leverage ctx)
          ))

        (define-public
          (close
            (base-token   <ft-trait>)
            (quote-token  <ft-trait>)
            (position-id  uint)
            (desired      uint)
            (slippage     uint)
            (ctx0         { identifier: (buff 32), message: (buff 8192), oracle: <oracle-trait> }))

            (let ((ctx (try! (CONTEXT base-token quote-token desired slippage ctx0))))
              (try! (lock))
              (contract-call? .gl-core close u1 base-token quote-token position-id ctx)
            ))

        (define-public
          (liquidate
            (base-token   <ft-trait>)
            (quote-token  <ft-trait>)
            (position-id  uint)
            (desired      uint)
            (slippage     uint)
            (ctx0         { identifier: (buff 32), message: (buff 8192), oracle: <oracle-trait> }))

          (let ((ctx (try! (CONTEXT base-token quote-token desired slippage ctx0))))
            (contract-call? .gl-core liquidate u1 base-token quote-token position-id ctx)
          ))
    "#};

    let contract_id = QualifiedContractIdentifier::transient();
    let epoch = StacksEpochId::Epoch32;
    let clarity_version = ClarityVersion::Clarity3;
    let ast =
        clarity::vm::ast::build_ast(&contract_id, contract_src, &mut (), clarity_version, epoch)
            .expect("Failed to build AST");

    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);

    let static_cost_map = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| static_cost_from_ast(&ast, &clarity_version, epoch, env, invoke_ctx),
    )
    .expect("Failed to get static cost analysis");

    let trait_count = static_cost_map
        .costs
        .values()
        .next()
        .and_then(|(_, trait_count)| trait_count.clone());

    // Assert that there is at least some payload for the trait counts when getting static_cost_from_ast
    assert!(trait_count.is_some());

    let trait_count_map = trait_count.unwrap();

    assert!(trait_count_map.contains_key("CONTEXT"));
    let (_context_min, context_max) = trait_count_map.get("CONTEXT").unwrap();
    assert_eq!(*context_max, 6);

    // mint uses ft-trait (twice), lp-token-trait (once), oracle-trait (once), and calls CONTEXT
    // Note: mint has parameters: base-token <ft-trait>, quote-token <ft-trait>, lp-token <lp-token-trait>,
    // and ctx0 with oracle: <oracle-trait>. It also calls CONTEXT which uses traits.
    // Breakdown:
    // - CONTEXT propagation: 6
    // - base-token passed to CONTEXT: 1
    // - quote-token passed to CONTEXT: 1
    // - base-token passed to contract-call?: 1 (counted in collector)
    // - quote-token passed to contract-call?: 1 (counted in collector)
    // - lp-token passed to contract-call?: 1 (counted in collector)
    // Total = 6 + 1 + 1 + 1 + 1 + 1 = 11
    assert!(trait_count_map.contains_key("mint"),);
    let (_mint_min, mint_max) = trait_count_map.get("mint").unwrap();
    assert_eq!(*mint_max, 11);
}

#[test]
fn test_empty_list_in_expression_does_not_panic() {
    // Regression test: an empty list `()` (e.g. as an argument to `as-contract?`)
    // caused an index-out-of-bounds panic in build_listlike_cost_analysis_tree.
    let src = indoc! {r#"
        (define-public (foo)
          (ok (list () u1 u2))
        )
    "#};

    let contract_id = QualifiedContractIdentifier::transient();
    let epoch = StacksEpochId::Epoch33;
    let clarity_version = ClarityVersion::Clarity4;
    let ast = clarity::vm::ast::build_ast(&contract_id, src, &mut (), clarity_version, epoch)
        .expect("Failed to parse");

    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);

    let result = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| static_cost_from_ast(&ast, &clarity_version, epoch, env, invoke_ctx),
    );

    // Should complete without panicking
    assert!(result.is_ok(), "Expected Ok, got: {:?}", result.err());
}

use clarity_static_cost::static_cost::{CostWarning, CostWarningKind};

/// Test that a contract with trait-based contract-call? whose target cannot be
/// resolved emits an UnresolvedTraitCall warning.
#[test]
fn test_unresolved_trait_call_emits_warning() {
    let src = indoc! {r#"
        (define-trait pool-trait (
            (get-balance (principal) (response uint uint))
        ))

        (define-public (check-balance (pool <pool-trait>) (who principal))
            (contract-call? pool get-balance who)
        )
    "#};

    let contract_id = QualifiedContractIdentifier::transient();
    let epoch = StacksEpochId::Epoch33;
    let clarity_version = ClarityVersion::Clarity4;
    let ast =
        clarity::vm::ast::build_ast(&contract_id, src, &mut (), clarity_version, epoch).unwrap();

    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);

    let result = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| static_cost_from_ast(&ast, &clarity_version, epoch, env, invoke_ctx),
    )
    .expect("static cost analysis should succeed");

    // Should have at least one warning about the unresolved trait call
    assert!(
        !result.warnings.is_empty(),
        "Expected warnings for unresolved trait-based contract-call?, got none"
    );

    let warning = &result.warnings[0];
    assert_eq!(warning.function_name, "check-balance");
    assert_eq!(
        warning.kind,
        CostWarningKind::UnresolvedTraitCall {
            target_variable: "pool".to_string(),
            called_function: "get-balance".to_string(),
        }
    );
}

/// Test that a fold over a function containing an unresolved trait call
/// emits a warning. This mimics the DLMM router pattern.
#[test]
fn test_fold_with_trait_call_emits_warning() {
    let src = indoc! {r#"
        (define-trait pool-trait (
            (swap (uint) (response {in: uint, out: uint} uint))
        ))

        (define-private (swap-step
            (bin-id int)
            (acc (response {pool: <pool-trait>, amount: uint, total: uint} uint))
        )
            (let (
                (data (unwrap! acc (err u1)))
                (pool (get pool data))
                (amount (get amount data))
            )
                (if (> amount u0)
                    (let ((result (try! (contract-call? pool swap amount))))
                        (ok {pool: pool, amount: (- amount (get in result)), total: (+ (get total data) (get out result))}))
                    (ok data))
            )
        )

        (define-constant BIN_RANGE (list 0 1 2 3 4 5 6 7 8 9))

        (define-public (swap-multi
            (pool <pool-trait>)
            (amount uint)
        )
            (let (
                (result (try! (fold swap-step BIN_RANGE (ok {pool: pool, amount: amount, total: u0}))))
            )
                (ok (get total result))
            )
        )
    "#};

    let contract_id = QualifiedContractIdentifier::transient();
    let epoch = StacksEpochId::Epoch33;
    let clarity_version = ClarityVersion::Clarity4;
    let ast =
        clarity::vm::ast::build_ast(&contract_id, src, &mut (), clarity_version, epoch).unwrap();

    let mut memory_store = MemoryBackingStore::new();
    let db = memory_store.as_clarity_db();
    let mut owned_env = OwnedEnvironment::new(db, epoch);

    let result = with_cost_analysis_environment(
        &mut owned_env,
        &contract_id,
        clarity_version,
        |env, invoke_ctx| static_cost_from_ast(&ast, &clarity_version, epoch, env, invoke_ctx),
    )
    .expect("static cost analysis should succeed");

    // The swap-step function has a trait-based contract-call? that can't be resolved
    let swap_step_warnings: Vec<&CostWarning> = result
        .warnings
        .iter()
        .filter(|w| w.function_name == "swap-step")
        .collect();

    assert!(
        !swap_step_warnings.is_empty(),
        "Expected warnings for swap-step's unresolved trait call, got none. All warnings: {:?}",
        result.warnings
    );

    assert_eq!(
        swap_step_warnings[0].kind,
        CostWarningKind::UnresolvedTraitCall {
            target_variable: "pool".to_string(),
            called_function: "swap".to_string(),
        }
    );
}

/// Test that providing trait implementations resolves the cost and produces
/// no warnings for the resolved calls.
#[test]
fn test_trait_resolution_with_implementations() {
    let epoch = StacksEpochId::Epoch33;
    let clarity_version = ClarityVersion::Clarity4;

    // The "pool" contract that implements the trait
    let pool_src = indoc! {r#"
        (define-data-var balance uint u1000)

        (define-public (get-balance (who principal))
            (ok (var-get balance))
        )
    "#};

    // The contract that calls via a trait
    let caller_src = indoc! {r#"
        (define-trait pool-trait (
            (get-balance (principal) (response uint uint))
        ))

        (define-public (check-balance (pool <pool-trait>) (who principal))
            (contract-call? pool get-balance who)
        )
    "#};

    let deployer = StandardPrincipalData::transient();
    let pool_id = QualifiedContractIdentifier::new(deployer.clone(), "pool".into());
    let caller_id = QualifiedContractIdentifier::new(deployer, "caller".into());

    // Set up environment with deployed contracts
    let mut memory_store = MemoryBackingStore::new();
    let mut db = memory_store.as_clarity_db();
    db.begin();
    db.set_clarity_epoch_version(epoch).unwrap();
    db.commit().unwrap();
    db.begin();
    db.set_tenure_height(1).unwrap();
    db.commit().unwrap();
    db.begin();
    db.setup_block_metadata(Some(1)).unwrap();
    db.commit().unwrap();

    let costs_contract_id = boot_code_id("costs", false);
    let costs_4_contract_id = boot_code_id("costs-4", false);
    let cost_voting_contract_id = boot_code_id("cost-voting", false);
    {
        let mut temp_env = OwnedEnvironment::new(db, epoch);
        temp_env
            .initialize_versioned_contract(
                costs_contract_id,
                clarity_version,
                BOOT_CODE_COSTS,
                None,
            )
            .expect("Failed to initialize costs contract");
        temp_env
            .initialize_versioned_contract(
                costs_4_contract_id,
                clarity_version,
                BOOT_CODE_COSTS_4,
                None,
            )
            .expect("Failed to initialize costs-4 contract");
        temp_env
            .initialize_versioned_contract(
                cost_voting_contract_id,
                clarity_version,
                &BOOT_CODE_COST_VOTING_TESTNET.to_string(),
                None,
            )
            .expect("Failed to initialize cost-voting contract");
        let (extracted_db, _) = temp_env.destruct().unwrap();
        db = extracted_db;
    }

    let mut owned_env = OwnedEnvironment::new_max_limit(db, epoch, false);

    // Deploy pool contract first, then caller
    owned_env
        .initialize_versioned_contract(pool_id.clone(), clarity_version, pool_src, None)
        .expect("Failed to deploy pool contract");
    owned_env
        .initialize_versioned_contract(caller_id.clone(), clarity_version, caller_src, None)
        .expect("Failed to deploy caller contract");

    // First: analyze WITHOUT trait implementations — should get warnings
    let caller_ast = ast::build_ast(&caller_id, caller_src, &mut (), clarity_version, epoch)
        .expect("Failed to build caller AST");
    owned_env.begin();
    let result_no_impls = {
        let contract_context = ContractContext::new(caller_id.clone(), clarity_version);
        let (mut env, invoke_ctx) = owned_env.get_exec_environment(None, None, &contract_context);
        static_cost_from_ast_with_source(
            &caller_ast,
            &clarity_version,
            epoch,
            Some(caller_src),
            &mut env,
            &invoke_ctx,
        )
        .expect("Failed to get static cost analysis")
    };
    let _ = owned_env.commit();

    assert!(
        !result_no_impls.warnings.is_empty(),
        "Expected warnings without trait implementations"
    );

    // Second: analyze WITH trait implementations — should resolve and no warnings
    let mut trait_impls: HashMap<String, Vec<QualifiedContractIdentifier>> = HashMap::new();
    trait_impls.insert("pool-trait".to_string(), vec![pool_id.clone()]);

    owned_env.begin();
    let result_with_impls = {
        let contract_context = ContractContext::new(caller_id.clone(), clarity_version);
        let (mut env, invoke_ctx) = owned_env.get_exec_environment(None, None, &contract_context);
        static_cost_from_ast_with_options(
            &caller_ast,
            &clarity_version,
            epoch,
            Some(caller_src),
            &trait_impls,
            &mut env,
            &invoke_ctx,
        )
        .expect("Failed to get static cost analysis with trait implementations")
    };
    let _ = owned_env.commit();

    assert!(
        result_with_impls.warnings.is_empty(),
        "Expected no warnings with trait implementations, got: {:?}",
        result_with_impls.warnings
    );

    // The resolved cost should be higher than the unresolved one (which had zero cost
    // for the trait call)
    let (cost_no_impls, _) = result_no_impls
        .costs
        .get("check-balance")
        .expect("check-balance not found");
    let (cost_with_impls, _) = result_with_impls
        .costs
        .get("check-balance")
        .expect("check-balance not found");

    println!("Cost without trait impl: {:?}", cost_no_impls);
    println!("Cost with trait impl: {:?}", cost_with_impls);

    assert!(
        cost_with_impls.max.runtime > cost_no_impls.max.runtime,
        "Resolved cost ({}) should be higher than unresolved cost ({}) due to callee function cost",
        cost_with_impls.max.runtime,
        cost_no_impls.max.runtime
    );

    assert!(
        cost_with_impls.max.read_count > cost_no_impls.max.read_count,
        "Resolved read_count ({}) should be higher than unresolved ({})",
        cost_with_impls.max.read_count,
        cost_no_impls.max.read_count
    );
}
