use std::collections::BTreeMap;

use crate::repl::session::Session;
use crate::repl::SessionSettings;

fn get_coverage_report(contract: &str, snippets: Vec<String>) -> String {
    let mut session = Session::new(SessionSettings::default());
    session.enable_coverage_hook();
    session.set_test_name("test_scenario".to_string());

    let _ = session.eval(contract.into(), false);
    for snippet in snippets {
        let _ = session.eval(snippet, false);
    }

    let (contract_id, contract) = session.contracts.pop_first().unwrap();
    let ast = contract.ast;

    let asts = BTreeMap::from([(contract_id.clone(), ast)]);
    let paths = BTreeMap::from([(contract_id.name.to_string(), "/contract-0.clar".into())]);

    session.collect_lcov_content(&asts, &paths)
}

fn get_expected_report(body: String) -> String {
    format!("TN:test_scenario\nSF:/contract-0.clar\n{body}\nend_of_record\n")
}

#[test]
fn line_is_executed() {
    let contract = "(define-read-only (add) (+ 1 2))";
    let snippet = "(contract-call? .contract-0 add)";
    let cov = get_coverage_report(contract, vec![snippet.into()]);

    let expect = get_expected_report(
        [
            "FN:1,add",
            "FNDA:1,add",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "BRF:0",
            "BRH:0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn line_is_executed_twice() {
    let contract = "(define-read-only (add) (+ 1 2))";
    // call it twice
    let snippet = "(contract-call? .contract-0 add) (contract-call? .contract-0 add)";
    let cov = get_coverage_report(contract, vec![snippet.into()]);

    let expect = get_expected_report(
        [
            "FN:1,add",
            "FNDA:1,add",
            "FNF:1",
            "FNH:1",
            "DA:1,2",
            "BRF:0",
            "BRH:0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn line_count_in_iterator() {
    let contract = [
        "(define-private (add-1 (n uint)) (+ n u1))",
        "(define-public (map-add-1)",
        "  (ok (map add-1 (list u2 u3)))",
        ")",
    ]
    .join("\n");
    let snippet = "(contract-call? .contract-0 map-add-1)";
    let cov = get_coverage_report(contract.as_str(), vec![snippet.into()]);

    let expect = get_expected_report(
        [
            "FN:1,add-1",
            "FN:2,map-add-1",
            "FNDA:1,add-1",
            "FNDA:1,map-add-1",
            "FNF:2",
            "FNH:2",
            "DA:1,2",
            "DA:2,1",
            "DA:3,1",
            "BRF:0",
            "BRH:0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

// each FNDA should have a corresponding DA
#[test]
fn function_hit_should_have_line_hit() {
    let contract = ["(define-read-only (t)", "  true", ")"].join("\n");

    let snippet = "(contract-call? .contract-0 t)";
    let cov = get_coverage_report(contract.as_str(), vec![snippet.into()]);

    let expect = get_expected_report(
        [
            "FN:1,t", "FNDA:1,t", "FNF:1", "FNH:1", "DA:1,1", "DA:2,1", "BRF:0", "BRH:0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn multiple_line_execution() {
    let contract = [
        "(define-read-only (add)",
        "  (begin",
        "    (+ (+ 1 1) (+ 1 2))",
        "    (+ 1 2 3)",
        "  )",
        ")",
    ]
    .join("\n");

    let snippet = "(contract-call? .contract-0 add)";
    let cov = get_coverage_report(contract.as_str(), vec![snippet.into()]);

    let expect = get_expected_report(
        [
            "FN:1,add",
            "FNDA:1,add",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "DA:2,1",
            "DA:3,1",
            "DA:4,1",
            "BRF:0",
            "BRH:0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn let_binding() {
    let contract = [
        "(define-public (add-print)",
        "  (let (",
        "    (c (+ 1 1))",
        "  )",
        "    (ok c)",
        "  )",
        ")",
    ]
    .join("\n");

    let snippet = "(contract-call? .contract-0 add-print)";
    let cov = get_coverage_report(contract.as_str(), vec![snippet.into()]);

    let expect = get_expected_report(
        [
            "FN:1,add-print",
            "FNDA:1,add-print",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "DA:2,1",
            "DA:3,1",
            "DA:5,1",
            "BRF:0",
            "BRH:0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn simple_if_branching() {
    let contract = [
        "(define-read-only (one-or-two (one bool))",
        "  (if one 1 2)",
        ")",
    ]
    .join("\n");

    let expect_base = [
        "FN:1,one-or-two",
        "FNDA:1,one-or-two",
        "FNF:1",
        "FNH:1",
        "DA:1,1",
        "DA:2,1",
        "BRF:2",
        "BRH:1",
    ];

    // left path
    let snippet = "(contract-call? .contract-0 one-or-two true)";
    let cov = get_coverage_report(contract.as_str(), vec![snippet.into()]);

    let expect = get_expected_report(
        [&expect_base[..], &["BRDA:2,8,0,1", "BRDA:2,8,1,0"]]
            .concat()
            .join("\n"),
    );
    assert_eq!(cov, expect);

    // right path
    let snippet = "(contract-call? .contract-0 one-or-two false)";
    let cov = get_coverage_report(contract.as_str(), vec![snippet.into()]);

    let expect = get_expected_report(
        [&expect_base[..], &["BRDA:2,8,0,0", "BRDA:2,8,1,1"]]
            .concat()
            .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn simple_if_branches_with_exprs() {
    let contract = [
        "(define-read-only (add-or-sub (add bool))",
        "  (if add (+ 1 1) (- 1 1))",
        ")",
    ]
    .join("\n");
    let snippet = "(contract-call? .contract-0 add-or-sub true)";
    let cov = get_coverage_report(contract.as_str(), vec![snippet.into()]);

    let expect = get_expected_report(
        [
            "FN:1,add-or-sub",
            "FNDA:1,add-or-sub",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "DA:2,1",
            "BRF:2",
            "BRH:1",
            "BRDA:2,8,0,1",
            "BRDA:2,8,1,0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn hit_all_if_branches() {
    let contract = [
        "(define-read-only (add-or-sub (add bool))",
        "  (if add (+ 1 1) (- 1 1))",
        ")",
    ]
    .join("\n");

    // hit left branch 3 times and right branch 2
    let snippets: Vec<String> = vec![
        "(contract-call? .contract-0 add-or-sub true)".into(),
        "(contract-call? .contract-0 add-or-sub true)".into(),
        "(contract-call? .contract-0 add-or-sub true)".into(),
        "(contract-call? .contract-0 add-or-sub false)".into(),
        "(contract-call? .contract-0 add-or-sub false)".into(),
    ];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        [
            "FN:1,add-or-sub",
            "FNDA:5,add-or-sub",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "DA:2,5",
            "BRF:2",
            "BRH:2",
            "BRDA:2,8,0,3",
            "BRDA:2,8,1,2",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn simple_asserts_branching() {
    let contract = [
        "(define-read-only (is-one (v int))",
        "  (ok (asserts! (is-eq v 1) (err u1)))",
        ")",
    ]
    .join("\n");

    // no hit on (err u1)
    let snippets: Vec<String> = vec!["(contract-call? .contract-0 is-one 1)".into()];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        [
            "FN:1,is-one",
            "FNDA:1,is-one",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "DA:2,1",
            "BRF:1",
            "BRH:0",
            "BRDA:2,10,0,0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);

    // hit on (err u1)
    let snippets: Vec<String> = vec!["(contract-call? .contract-0 is-one 2)".into()];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        [
            "FN:1,is-one",
            "FNDA:1,is-one",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "DA:2,1",
            "BRF:1",
            "BRH:1",
            "BRDA:2,10,0,1",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn branch_if_plus_and() {
    let contract = [
        "(define-read-only (unecessary-ifs (v int))",
        "  (if (and (> v 0) (> v 1) (> v 2) (> v 3))",
        "    (ok \"greater\")",
        "    (ok \"lower\")",
        "  )",
        ")",
    ]
    .join("\n");
    // calling with `2`, so that evualuation should stop at (> v 2) (which is false)
    let snippet = "(contract-call? .contract-0 unecessary-ifs 2)";
    let cov = get_coverage_report(contract.as_str(), vec![snippet.into()]);

    let expect = get_expected_report(
        vec![
            "FN:1,unecessary-ifs",
            "FNDA:1,unecessary-ifs",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "DA:2,1",
            "DA:3,0", // left if path
            "DA:4,1", // right if path
            "BRF:6",
            "BRH:4",
            "BRDA:2,10,0,1",
            "BRDA:2,10,1,1",
            "BRDA:2,10,2,1",
            "BRDA:2,10,3,0", // (> v 3) not hit
            "BRDA:3,8,0,0",  // left if path not hit
            "BRDA:4,8,1,1",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn branch_if_plus_or() {
    let contract = [
        "(define-read-only (unecessary-ors (v int))",
        "  (if (or (is-eq v 0) (is-eq v 1) (is-eq v 2) (is-eq v 3))",
        "    (ok \"match\")",
        "    (ok \"no match\")",
        "  )",
        ")",
    ]
    .join("\n");
    // calling with 1, so that evualuation should stop at (is-eq v 1)
    let snippet = "(contract-call? .contract-0 unecessary-ors 1)";
    let cov = get_coverage_report(contract.as_str(), vec![snippet.into()]);

    let expect = get_expected_report(
        vec![
            "FN:1,unecessary-ors",
            "FNDA:1,unecessary-ors",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "DA:2,1",
            "DA:3,1", // left if path
            "DA:4,0", // right if path
            "BRF:6",
            "BRH:3",
            "BRDA:2,10,0,1",
            "BRDA:2,10,1,1", // stop
            "BRDA:2,10,2,0",
            "BRDA:2,10,3,0",
            "BRDA:3,8,0,1",
            "BRDA:4,8,1,0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn match_opt_oneline() {
    let contract = [
        "(define-public (match-opt (opt? (optional int)))",
        "  (match opt? opt (ok opt) (err u1))",
        ")",
    ]
    .join("\n");

    let expect_base = [
        "FN:1,match-opt",
        "FNDA:1,match-opt",
        "FNF:1",
        "FNH:1",
        "DA:1,1",
        "DA:2,1",
        "BRF:2",
        "BRH:1",
    ];

    // left path
    let snippets: Vec<String> = vec!["(contract-call? .contract-0 match-opt (some 1))".into()];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        [&expect_base[..], &["BRDA:2,10,0,1", "BRDA:2,10,1,0"]]
            .concat()
            .join("\n"),
    );
    assert_eq!(cov, expect);

    // right path
    let snippets: Vec<String> = vec!["(contract-call? .contract-0 match-opt none)".into()];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        [&expect_base[..], &["BRDA:2,10,0,0", "BRDA:2,10,1,1"]]
            .concat()
            .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn match_opt_multiline() {
    let contract = [
        "(define-public (match-opt (opt? (optional int)))",
        "  (match opt?",
        "    opt",
        "    (ok opt)",
        "    (err u1)",
        "  )",
        ")",
    ]
    .join("\n");

    let expect_base = [
        "FN:1,match-opt",
        "FNDA:1,match-opt",
        "FNF:1",
        "FNH:1",
        "DA:1,1",
        "DA:2,1",
    ];

    // left path
    let snippets: Vec<String> = vec!["(contract-call? .contract-0 match-opt (some 1))".into()];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        [
            &expect_base[..],
            &[
                "DA:4,1",
                "DA:5,0",
                "BRF:2",
                "BRH:1",
                "BRDA:4,10,0,1",
                "BRDA:5,10,1,0",
            ],
        ]
        .concat()
        .join("\n"),
    );
    assert_eq!(cov, expect);

    // right path
    let snippets: Vec<String> = vec!["(contract-call? .contract-0 match-opt none)".into()];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        [
            &expect_base[..],
            &[
                "DA:4,0",
                "DA:5,1",
                "BRF:2",
                "BRH:1",
                "BRDA:4,10,0,0",
                "BRDA:5,10,1,1",
            ],
        ]
        .concat()
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn match_res_oneline() {
    // very similar to match opt
    // lighter test strategy, only test one liner and call both paths in same session
    let contract = [
        "(define-public (match-res (res (response int uint)))",
        "  (match res o (ok o) e (err e))",
        ")",
    ]
    .join("\n");

    // call left path twice and right path once
    let snippets: Vec<String> = vec![
        "(contract-call? .contract-0 match-res (ok 1))".into(),
        "(contract-call? .contract-0 match-res (ok 2))".into(),
        "(contract-call? .contract-0 match-res (err u1))".into(),
    ];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        [
            "FN:1,match-res",
            "FNDA:3,match-res",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "DA:2,3",
            "BRF:2",
            "BRH:2",
            "BRDA:2,11,0,2",
            "BRDA:2,11,1,1",
        ]
        .join("\n"),
    );

    assert_eq!(cov, expect);
}

#[test]
fn fold_iterator() {
    let contract = [
        "(define-private (inner-sum (a int) (b int)) (+ a b))",
        "(define-public (sum)",
        "  (ok",
        "    (fold",
        "      inner-sum",
        "      (list 0 1 2)",
        "      0",
        "    )",
        "  )",
        ")",
    ]
    .join("\n");

    let snippets: Vec<String> = vec!["(contract-call? .contract-0 sum)".into()];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        vec![
            "FN:1,inner-sum",
            "FN:2,sum",
            "FNDA:1,inner-sum",
            "FNDA:1,sum",
            "FNF:2",
            "FNH:2",
            "DA:1,3", // the list has 3 items
            "DA:2,1",
            "DA:3,1",
            "DA:4,1",
            "DA:5,1", // inner-sum func call
            "DA:6,1",
            "DA:7,1",
            "BRF:0",
            "BRH:0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn map_iterator() {
    let contract = [
        "(define-private (inner-square (n int)) (* n n))",
        "(define-public (square (ns (list 10 int)))",
        "  (ok (map",
        "    inner-square",
        "    ns",
        "  ))",
        ")",
    ]
    .join("\n");

    let snippets: Vec<String> = vec!["(contract-call? .contract-0 square (list 1 2 3))".into()];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        [
            "FN:1,inner-square",
            "FN:2,square",
            "FNDA:1,inner-square",
            "FNDA:1,square",
            "FNF:2",
            "FNH:2",
            "DA:1,3",
            "DA:2,1",
            "DA:3,1",
            "DA:4,1",
            "DA:5,1",
            "BRF:0",
            "BRH:0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn filter_iterator() {
    let contract = [
        "(define-private (is-positive (n int)) (>= n 0))",
        "(define-public (get-positive (ns (list 10 int)))",
        "  (ok (filter",
        "    is-positive",
        "    ns",
        "  ))",
        ")",
    ]
    .join("\n");

    let snippets: Vec<String> =
        vec!["(contract-call? .contract-0 get-positive (list -1 2 3))".into()];
    let cov = get_coverage_report(&contract, snippets);

    let expect = get_expected_report(
        [
            "FN:1,is-positive",
            "FN:2,get-positive",
            "FNDA:1,get-positive",
            "FNDA:1,is-positive",
            "FNF:2",
            "FNH:2",
            "DA:1,3",
            "DA:2,1",
            "DA:3,1",
            "DA:4,1",
            "DA:5,1",
            "BRF:0",
            "BRH:0",
        ]
        .join("\n"),
    );
    assert_eq!(cov, expect);
}

#[test]
fn multiple_test_files() {
    let mut session = Session::new(SessionSettings::default());
    session.enable_coverage_hook();

    let contract = "(define-read-only (add) (+ 1 2))";

    // insert 2 contracts
    // contract-0
    let _ = session.eval(contract.into(), false);
    // contract-1
    let _ = session.eval(contract.into(), false);

    // call contract-0 twice in test-1
    session.set_test_name("test-1".to_string());
    let snippet = "(contract-call? .contract-0 add)";
    let _ = session.eval(snippet.to_owned(), false);
    let snippet = "(contract-call? .contract-0 add)";
    let _ = session.eval(snippet.to_owned(), false);

    // call contract-0 once and contract-1 once in test-2
    session.set_test_name("test-2".to_string());
    let snippet = "(contract-call? .contract-0 add)";
    let _ = session.eval(snippet.to_owned(), false);
    let snippet = "(contract-call? .contract-1 add)";
    let _ = session.eval(snippet.to_owned(), false);

    let mut asts = BTreeMap::new();
    let mut paths = BTreeMap::new();
    for (i, (contract_id, contract)) in session.contracts.iter().enumerate() {
        asts.insert(contract_id.clone(), contract.ast.clone());
        paths.insert(contract_id.name.to_string(), format!("/contract-{i}.clar"));
    }

    let cov = session.collect_lcov_content(&asts, &paths);

    assert_eq!(
        [
            "TN:",
            "SF:/contract-0.clar",
            "FN:1,add",
            "FNF:1",
            "FNH:0",
            "BRF:0",
            "BRH:0",
            "end_of_record",
            "SF:/contract-1.clar",
            "FN:1,add",
            "FNF:1",
            "FNH:0",
            "BRF:0",
            "BRH:0",
            "end_of_record",
            "TN:test-1",
            "SF:/contract-0.clar",
            "FN:1,add",
            "FNDA:2,add",
            "FNF:1",
            "FNH:1",
            "DA:1,2",
            "BRF:0",
            "BRH:0",
            "end_of_record",
            "SF:/contract-1.clar",
            "FN:1,add",
            "FNF:1",
            "FNH:0",
            "BRF:0",
            "BRH:0",
            "end_of_record",
            "TN:test-2",
            "SF:/contract-0.clar",
            "FN:1,add",
            "FNDA:1,add",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "BRF:0",
            "BRH:0",
            "end_of_record",
            "SF:/contract-1.clar",
            "FN:1,add",
            "FNDA:1,add",
            "FNF:1",
            "FNH:1",
            "DA:1,1",
            "BRF:0",
            "BRH:0",
            "end_of_record",
            ""
        ]
        .join("\n"),
        cov
    );
}
