// Helper functions for ts-to-clar transpiler

/// Converts a string from camelCase or PascalCase to kebab-case.
pub fn to_kebab_case(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if c.is_uppercase() {
            if i != 0 {
                result.push('-');
            }
            for lower in c.to_lowercase() {
                result.push(lower);
            }
        } else {
            result.push(c);
        }
    }
    result
}

// #[cfg(test)]
// mod test {
//     use crate::{parse_ts, transpile};

//     #[track_caller]
//     fn simple_source_check(ts_source: &str, expected_clarity_output: &str) {
//         let result = transpile("test.clar.ts", ts_source);
//         assert_eq!(result, Ok(expected_clarity_output.to_string()));
//     }

//     #[test]
//     fn can_transpile() {
//         let file_name = "test.js";
//         let src = "function test() { return 42; }\n export default { readOnly: { test } };";

//         let _result = parse_ts(file_name, src);
//     }

//     #[test]
//     fn can_parse_data_var() {
//         simple_source_check(
//             "const count = new DataVar<Uint>(0);",
//             "(define-data-var count uint u0)\n",
//         );
//     }

//     #[test]
//     fn can_parse_data_with_basic_types() {
//         simple_source_check(
//             "const count = new DataVar<Int>(1);",
//             "(define-data-var count int 1)\n",
//         );

//         simple_source_check(
//             "const tokenName = new DataVar<StringAscii<32>>(\"sBTC\");",
//             "(define-data-var token-name (string-ascii 32) \"sBTC\")\n",
//         );

//         simple_source_check(
//             "const tokenName = new DataVar<StringUtf8<64>>(\"sBTC\");",
//             "(define-data-var token-name (string-utf8 64) u\"sBTC\")\n",
//         );

//         simple_source_check(
//             "const currentAggregatePubkey = new DataVar<ClBuffer<33>>(new Uint8Array([10, 1]));",
//             "(define-data-var current-aggregate-pubkey (buff 33) 0x0a01)\n",
//         );
//     }

//     #[test]
//     fn can_get_and_set_data_var() {
//         let ts_source = "const count = new DataVar<Uint>(0);\ncount.set(count.get() + 1);";
//         let expected = "(define-data-var count uint u0)\n(var-set count (+ (var-get count) u1))";
//         simple_source_check(ts_source, expected);
//     }

//     #[test]
//     fn can_infer_types() {
//         let ts_source = "const count = new DataVar<Uint>(1);\ncount.set(count.get() + 1);";
//         let expected = "(define-data-var count uint u1)\n(var-set count (+ (var-get count) u1))";
//         simple_source_check(ts_source, expected);

//         let ts_source = "const count = new DataVar<Int>(2);\ncount.set(count.get() + 1);";
//         let expected = "(define-data-var count int 2)\n(var-set count (+ (var-get count) 1))";
//         simple_source_check(ts_source, expected);
//     }

//     #[test]
//     fn handle_function() {
//         let ts_source = r#"const count = new DataVar<Uint>(0);

// function increment() {
//   count.set(count.get() + 1);
//   return ok(true);
// }"#;
//         let expected = r#"(define-data-var count uint u0)
// (define-private (increment)
//   (begin
//     (var-set count (+ (var-get count) u1))
//     (ok true)
//   )
// )
// "#;
//         simple_source_check(ts_source, expected);
//     }

//     #[test]
//     fn handle_function_args() {
//         // handle one arg
//         let ts_source = r#"const count = new DataVar<Uint>(0);

// function add(n: Uint) {
//   count.set(count.get() + n);
//   return ok(true);
// }"#;
//         let expected = r#"(define-data-var count uint u0)
// (define-private (add (n uint))
//   (begin
//     (var-set count (+ (var-get count) n))
//     (ok true)
//   )
// )
// "#;
//         simple_source_check(ts_source, expected);

//         // handle two args
//         let ts_source = r#"function add(a: Uint, b: Uint) {
//   return a + b;
// }"#;
//         let expected = r#"(define-private (add
//     (a uint)
//     (b uint)
//   )
//   (+ a b)
// )
// "#;
//         simple_source_check(ts_source, expected);
//     }
// }
