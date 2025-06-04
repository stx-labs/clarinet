mod helper;
mod parser;
// This module provides a transpiler that converts TypeScript into Clarity code.
// 1. It parses the TypeScript code into an AST using `swc`
// 2. Transform the AST into a Clarity AST
// traverses the AST to extract relevant information

use crate::parser::get_ir;

pub use self::helper::to_kebab_case;
use clarinet_format::formatter::{ClarityFormatter, Settings as FormatterSettings};
use clarity::vm::representations::PreSymbolicExpression;
use swc_common::sync::Lrc;
use swc_common::{FileName, SourceMap};
use swc_ecma_ast::{CallExpr, Expr, Function, Module};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};
use swc_ecma_visit::{Visit, VisitWith};

// this all file is a draft for a POC
// the new implementation will be split into multiple files (parser, transpiler, etc)

#[derive(Debug)]
pub struct AstVisitor {
    function_names: Vec<String>,
    function_calls: Vec<String>,
    data_vars: Vec<(String, String, String)>,
    var_get_calls: Vec<String>,
    var_set_calls: Vec<(String, String)>,
    __orig_src: Option<String>,
}

fn extract_bin_op(op: swc_ecma_ast::BinaryOp) -> &'static str {
    use swc_ecma_ast::BinaryOp::*;
    match op {
        Add => "+",
        Sub => "-",
        Mul => "*",
        Div => "/",
        _ => "unknown",
    }
}

impl Visit for AstVisitor {
    fn visit_var_decl(&mut self, var: &swc_ecma_ast::VarDecl) {
        for decl in &var.decls {
            if let Some(init) = &decl.init {
                if let Some(id) = &decl.name.as_ident() {
                    let var_name = id.sym.to_string();

                    let type_ann = id
                        .type_ann
                        .as_ref()
                        .map(|ann| {
                            if let swc_ecma_ast::TsType::TsTypeRef(type_ref) = &*ann.type_ann {
                                if let swc_ecma_ast::TsEntityName::Ident(type_ident) =
                                    &type_ref.type_name
                                {
                                    if type_ident.sym == "StringAscii"
                                        || type_ident.sym == "StringUtf8"
                                    {
                                        if let Some(type_params) = &type_ref.type_params {
                                            if let Some(param) = type_params.params.first() {
                                                if let swc_ecma_ast::TsType::TsLitType(lit_type) =
                                                    &**param
                                                {
                                                    if let swc_ecma_ast::TsLit::Number(num_lit) =
                                                        &lit_type.lit
                                                    {
                                                        return format!(
                                                            "({} {})",
                                                            helper::to_kebab_case(&type_ident.sym),
                                                            num_lit.value as i64
                                                        );
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    type_ident.sym.to_string()
                                } else {
                                    "unknown".to_string()
                                }
                            } else {
                                "unknown".to_string()
                            }
                        })
                        .unwrap_or_else(|| "unknown".to_string());

                    if let Expr::New(new_expr) = &**init {
                        if let Some(ident) = new_expr.callee.as_ident() {
                            if ident.sym == "DataVar" {
                                let type_arg = new_expr
                                    .type_args
                                    .as_ref()
                                    .and_then(|type_args| type_args.params.first())
                                    .map(|first| {
                                        match &**first {
                                            swc_ecma_ast::TsType::TsTypeRef(type_ref) => {
                                                if let swc_ecma_ast::TsEntityName::Ident(type_ident) = &type_ref.type_name {
                                                    // Handle ClBuffer<N> generics
                                                    if type_ident.sym == "ClBuffer" {
                                                        if let Some(type_params) = &type_ref.type_params {
                                                            if let Some(param) = type_params.params.first() {
                                                                if let swc_ecma_ast::TsType::TsLitType(lit_type) = &**param {
                                                                    if let swc_ecma_ast::TsLit::Number(num_lit) = &lit_type.lit {
                                                                        return format!("(buff {})", num_lit.value as i64);
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                    // Handle StringAscii<N> and StringUtf8<N> generics
                                                    if type_ident.sym == "StringAscii" || type_ident.sym == "StringUtf8" {
                                                        if let Some(type_params) = &type_ref.type_params {
                                                            if let Some(param) = type_params.params.first() {
                                                                if let swc_ecma_ast::TsType::TsLitType(lit_type) = &**param {
                                                                    if let swc_ecma_ast::TsLit::Number(num_lit) = &lit_type.lit {
                                                                        return format!("({} {})", helper::to_kebab_case(&type_ident.sym),  num_lit.value as i64);
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                    type_ident.sym.to_string()
                                                } else {
                                                    "unknown".to_string()
                                                }
                                            }
                                            _ => "unknown".to_string(),
                                        }
                                    })
                                    .unwrap_or_else(|| type_ann.clone());
                                // Get initial value
                                let initial_value = if let Some(arg) =
                                    new_expr.args.as_ref().and_then(|args| args.first())
                                {
                                    match &*arg.expr {
                                        Expr::Lit(swc_ecma_ast::Lit::Num(num)) => {
                                            if type_arg == "Int" {
                                                format!("{}", num.value as i64)
                                            } else if type_arg == "Uint" {
                                                format!("u{}", num.value as u64)
                                            } else {
                                                panic!("Unknown type for number")
                                            }
                                        }
                                        Expr::Lit(swc_ecma_ast::Lit::Str(s)) => s.value.to_string(),
                                        Expr::New(new_inner) => {
                                            // Handle new Uint8Array([...]) for ClBuffer
                                            if let Some(inner_ident) = new_inner.callee.as_ident() {
                                                if inner_ident.sym == "Uint8Array"
                                                    && type_arg.starts_with("(buff ")
                                                {
                                                    new_inner
                                                        .args
                                                        .as_ref()
                                                        .and_then(|args| args.first())
                                                        .and_then(|array_arg| {
                                                            if let Expr::Array(arr) =
                                                                &*array_arg.expr
                                                            {
                                                                Some(ts_uint_to_clar_buff(arr))
                                                            } else {
                                                                None
                                                            }
                                                        })
                                                        .unwrap_or_else(|| "unknown".to_string())
                                                } else {
                                                    "unknown".to_string()
                                                }
                                            } else {
                                                "unknown".to_string()
                                            }
                                        }
                                        _ => "unknown".to_string(),
                                    }
                                } else {
                                    "unknown".to_string()
                                };
                                self.data_vars.push((var_name, type_arg, initial_value));
                            }
                        }
                    }
                }
            }
        }
        var.visit_children_with(self);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if let Expr::Call(call_expr) = expr {
            if let Some(ident) = call_expr.callee.as_expr().and_then(|expr| expr.as_ident()) {
                match ident.sym.as_ref() {
                    "get" => {
                        if let Some(var_ident) =
                            call_expr.args.first().and_then(|arg| arg.expr.as_ident())
                        {
                            self.var_get_calls.push(var_ident.sym.to_string());
                        }
                    }
                    "set" => {
                        if let Some(var_ident) =
                            call_expr.args.first().and_then(|arg| arg.expr.as_ident())
                        {
                            if let Some(value_expr) = call_expr.args.get(1).map(|arg| &*arg.expr) {
                                // Use expr_to_clarity for any kind of expression
                                let value = expr_to_clarity(value_expr, &self.data_vars, None)
                                    .unwrap_or_else(|| "unknown".to_string());
                                self.var_set_calls.push((var_ident.sym.to_string(), value));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        expr.visit_children_with(self);
    }

    fn visit_module_item(&mut self, item: &swc_ecma_ast::ModuleItem) {
        if let swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Decl(swc_ecma_ast::Decl::Var(
            var_decl,
        ))) = item
        {
            if var_decl.kind == swc_ecma_ast::VarDeclKind::Const {
                for decl in &var_decl.decls {
                    if let Some(init) = &decl.init {
                        if let Expr::New(new_expr) = &**init {
                            if let Some(ident) = new_expr.callee.as_ident() {
                                if ident.sym == "DataVar" {
                                    // DataVar const found; handled in visit_var_decl
                                }
                            }
                        }
                    }
                }
            }
        }
        item.visit_children_with(self);
    }

    fn visit_fn_decl(&mut self, node: &swc_ecma_ast::FnDecl) {
        self.function_names.push(node.ident.sym.to_string());
        node.visit_children_with(self);
    }

    fn visit_function(&mut self, function: &Function) {
        function.visit_children_with(self);
    }

    fn visit_call_expr(&mut self, call_expr: &CallExpr) {
        if let Some(ident) = call_expr.callee.as_expr().and_then(|expr| expr.as_ident()) {
            self.function_calls.push(ident.sym.to_string());
        }

        call_expr.visit_children_with(self);
    }
}

fn expr_to_clarity(
    expr: &Expr,
    data_vars: &[(String, String, String)],
    var_type_ctx: Option<&str>,
) -> Option<String> {
    match expr {
        Expr::Call(call) => {
            // Handle method calls: e.g., count.get(), count.set(...)
            if let swc_ecma_ast::Callee::Expr(callee_expr) = &call.callee {
                if let Expr::Member(member) = &**callee_expr {
                    // Only support simple ident.property
                    if let (Expr::Ident(obj), swc_ecma_ast::MemberProp::Ident(prop)) =
                        (&*member.obj, &member.prop)
                    {
                        let var_name = obj.sym.to_string();
                        let prop_name = prop.sym.to_string();
                        if let Some((_, type_name, _)) =
                            data_vars.iter().find(|(n, _, _)| *n == var_name)
                        {
                            match prop_name.as_str() {
                                "get" => {
                                    return Some(format!(
                                        "(var-get {})",
                                        helper::to_kebab_case(&var_name)
                                    ));
                                }
                                "set" => {
                                    if let Some(arg) = call.args.first() {
                                        if let Some(arg_str) =
                                            expr_to_clarity(&arg.expr, data_vars, Some(type_name))
                                        {
                                            return Some(format!(
                                                "(var-set {} {})",
                                                helper::to_kebab_case(&var_name),
                                                arg_str
                                            ));
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                } else if let Expr::Ident(ident) = &**callee_expr {
                    // Handle ok(true) and similar
                    if ident.sym == "ok" {
                        if let Some(arg) = call.args.first() {
                            let arg_str = expr_to_clarity(&arg.expr, data_vars, None)
                                .unwrap_or_else(|| "true".to_string());
                            return Some(format!("(ok {})", arg_str));
                        } else {
                            return Some("(ok true)".to_string());
                        }
                    }
                }
            }
            None
        }
        Expr::Bin(bin) => {
            let op = self::extract_bin_op(bin.op);
            let left = expr_to_clarity(&bin.left, data_vars, var_type_ctx)?;
            let right = expr_to_clarity(&bin.right, data_vars, var_type_ctx)?;
            Some(format!("({} {} {})", op, left, right))
        }
        Expr::Lit(swc_ecma_ast::Lit::Num(num)) => match var_type_ctx {
            Some(t) if t.eq_ignore_ascii_case("int") => Some(format!("{}", num.value as i64)),
            _ => Some(format!("u{}", num.value as u64)),
        },
        Expr::Ident(ident) => {
            // If the identifier matches a data var, use (var-get ...), else just the name
            if data_vars
                .iter()
                .any(|(n, _, _)| n == &ident.sym.to_string())
            {
                Some(format!("(var-get {})", helper::to_kebab_case(&ident.sym)))
            } else {
                Some(helper::to_kebab_case(&ident.sym))
            }
        }
        _ => None,
    }
}

fn stmt_to_clarity(
    stmt: &swc_ecma_ast::Stmt,
    data_vars: &[(String, String, String)],
) -> Option<String> {
    match stmt {
        swc_ecma_ast::Stmt::Expr(expr_stmt) => expr_to_clarity(&expr_stmt.expr, data_vars, None),
        swc_ecma_ast::Stmt::Return(ret_stmt) => ret_stmt
            .arg
            .as_ref()
            .and_then(|arg| expr_to_clarity(arg, data_vars, None)),
        _ => None,
    }
}

fn ts_type_to_clarity_type(
    type_name: &str,
    type_params: Option<&swc_ecma_ast::TsTypeParamInstantiation>,
) -> String {
    match type_name {
        "Uint" => "uint".to_string(),
        "Int" => "int".to_string(),
        "StringAscii" | "StringUtf8" => {
            if let Some(params) = type_params {
                if let Some(param) = params.params.first() {
                    if let swc_ecma_ast::TsType::TsLitType(lit_type) = &**param {
                        if let swc_ecma_ast::TsLit::Number(num_lit) = &lit_type.lit {
                            return format!(
                                "({} {})",
                                type_name.to_lowercase(),
                                num_lit.value as i64
                            );
                        }
                    }
                }
            }
            format!("({} N)", type_name.to_lowercase())
        }
        _ => type_name.to_lowercase(),
    }
}

fn fn_to_clarity(
    fn_decl: &swc_ecma_ast::FnDecl,
    data_vars: &[(String, String, String)],
) -> Option<String> {
    let name = helper::to_kebab_case(&fn_decl.ident.sym);
    // Extract function parameters and map types
    let params: Vec<String> = fn_decl
        .function
        .params
        .iter()
        .filter_map(|param| {
            if let swc_ecma_ast::Pat::Ident(binding_ident) = &param.pat {
                let param_name = helper::to_kebab_case(&binding_ident.id.sym);
                let type_info =
                    binding_ident
                        .type_ann
                        .as_ref()
                        .and_then(|ann| match &*ann.type_ann {
                            swc_ecma_ast::TsType::TsTypeRef(type_ref) => {
                                match &type_ref.type_name {
                                    swc_ecma_ast::TsEntityName::Ident(type_ident) => Some((
                                        type_ident.sym.as_ref(),
                                        type_ref.type_params.as_ref(),
                                    )),
                                    _ => None,
                                }
                            }
                            _ => None,
                        });
                let clarity_type = ts_type_to_clarity_type(
                    type_info.map_or("unknown", |(name, _)| name),
                    type_info.and_then(|(_, params)| params).map(|v| &**v),
                );
                Some(format!("({} {})", param_name, clarity_type))
            } else {
                None
            }
        })
        .collect();

    let params_str = if !params.is_empty() {
        format!(" {}", params.join(" "))
    } else {
        String::new()
    };
    let mut body_lines = Vec::new();
    if let Some(body) = &fn_decl.function.body {
        for stmt in &body.stmts {
            if let Some(line) = stmt_to_clarity(stmt, data_vars) {
                body_lines.push(line);
            }
        }
    }
    let body = if body_lines.len() > 1 {
        let indented: Vec<String> = body_lines.iter().map(|l| format!("    {}", l)).collect();
        format!("(begin\n{}\n  )", indented.join("\n"))
    } else if body_lines.len() == 1 {
        body_lines[0].to_string()
    } else {
        "(begin)".to_string()
    };
    Some(format!(
        "(define-private ({}{})\n  {}\n)",
        name, params_str, body
    ))
}

fn ast_to_clarity(
    visitor: AstVisitor,
) -> Result<Vec<PreSymbolicExpression>, swc_ecma_parser::error::Error> {
    let mut clarity_lines = Vec::new();
    for (name, typ, value) in &visitor.data_vars {
        let typ_lower = typ.to_lowercase();
        let clarity_type = typ_lower.as_str();
        let clarity_var_name = helper::to_kebab_case(name);
        let clarity_value = if clarity_type.starts_with("(string-ascii ") {
            format!("\"{}\"", value)
        } else if clarity_type.starts_with("(string-utf8 ") {
            format!("u\"{}\"", value)
        } else {
            value.clone()
        };
        let clarity_code = format!(
            "(define-data-var {} {} {})",
            clarity_var_name, clarity_type, clarity_value
        );
        clarity_lines.push(clarity_code);
    }

    if let Some(orig_src) = visitor.__orig_src.as_ref() {
        let source_map: Lrc<SourceMap> = Default::default();
        let source_file =
            source_map.new_source_file(FileName::Custom("tmp.ts".into()).into(), orig_src.clone());
        let lexer = Lexer::new(
            Syntax::Typescript(Default::default()),
            Default::default(),
            StringInput::from(&*source_file),
            None,
        );
        let mut parser = Parser::new_from(lexer);
        if let Ok(module) = parser.parse_module() {
            for item in &module.body {
                match item {
                    swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Expr(expr_stmt)) => {
                        if let Some(clar) =
                            expr_to_clarity(&expr_stmt.expr, &visitor.data_vars, None)
                        {
                            clarity_lines.push(clar);
                        }
                    }
                    swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Decl(
                        swc_ecma_ast::Decl::Fn(fn_decl),
                    )) => {
                        if let Some(clar) = fn_to_clarity(fn_decl, &visitor.data_vars) {
                            clarity_lines.push(clar);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    let all_code = clarity_lines.join("\n");
    let exprs = clarity::vm::ast::parser::v2::parse(&all_code).unwrap();
    Ok(exprs)
}

fn parse_ts(file_name: &str, src: &str) -> Result<Module, swc_ecma_parser::error::Error> {
    let cm: Lrc<SourceMap> = Default::default();

    let fm = cm.new_source_file(FileName::Custom(file_name.into()).into(), src.into());
    let lexer = Lexer::new(
        Syntax::Typescript(Default::default()),
        Default::default(),
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);

    parser.parse_module()
}

fn visit_ts_ast(
    module: Module,
    orig_src: Option<String>,
) -> Result<AstVisitor, swc_ecma_parser::error::Error> {
    let mut visitor = AstVisitor {
        function_names: Vec::new(),
        function_calls: Vec::new(),
        data_vars: Vec::new(),
        var_get_calls: Vec::new(),
        var_set_calls: Vec::new(),
        __orig_src: orig_src,
    };

    module.visit_with(&mut visitor);
    Ok(visitor)
}

fn ts_uint_to_clar_buff(arr: &swc_ecma_ast::ArrayLit) -> String {
    let hex_string: String = arr
        .elems
        .iter()
        .filter_map(|el| {
            el.as_ref().and_then(|el| {
                if let Expr::Lit(swc_ecma_ast::Lit::Num(num)) = &*el.expr {
                    Some(format!("{:02x}", num.value as u8))
                } else {
                    None
                }
            })
        })
        .collect();
    format!("0x{}", hex_string)
}

pub fn transpile(file_name: &str, src: &str) -> Result<String, swc_ecma_parser::error::Error> {
    let _ir = get_ir(file_name, src.to_string());
    let module = parse_ts(file_name, src)?;
    let visitor = visit_ts_ast(module, Some(src.to_string()))?;
    let pses = ast_to_clarity(visitor)?;
    let formatter = ClarityFormatter::new(FormatterSettings::default());

    let clarity_code = formatter.format_ast(&pses);
    Ok(clarity_code)
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
