// parser.rs parses the TypeScript code and creates the intermediate representation (IR) for the TypeScript to Clar conversion.
// It extracts the top-level declarations and their types.
//  - define-constant
//  - define-data-var
//  - define-data-map
//  - define-read-only
//  - define-public
//  - define-private

use anyhow::{anyhow, Result};
use swc_common::{sync::Lrc, FileName, SourceMap};
use swc_ecma_ast::{
    BlockStmt, Expr, Module, NewExpr, TsEntityName, TsType, TsTypeParamInstantiation, TsTypeRef,
    VarDeclKind, VarDeclarator,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};
use swc_ecma_visit::{Visit, VisitWith};

use clarity::vm::callables::DefineType;
use clarity::vm::types::{SequenceSubtype, TypeSignature};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IRConstant {
    pub name: String,
    pub typ: TypeSignature,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IRDataVar {
    pub name: String,
    pub typ: TypeSignature,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IRDataMap {
    pub name: String,
    pub key_typ: TypeSignature,
    pub value_typ: TypeSignature,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRFunction {
    pub name: String,
    pub define_type: DefineType,
    pub params: Vec<(String, TypeSignature)>,
    pub return_type: TypeSignature,
    pub body: BlockStmt,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IR {
    pub source: String,
    pub constants: Vec<IRConstant>,
    pub data_vars: Vec<IRDataVar>,
    pub data_maps: Vec<IRDataMap>,
    pub functions: Vec<IRFunction>,
    pub top_level_exprs: Vec<Expr>,
}

fn parse_ts(file_name: &str, src: String) -> Result<Module> {
    let cm: Lrc<SourceMap> = Default::default();
    let fm = cm.new_source_file(FileName::Custom(file_name.into()).into(), src);
    let lexer = Lexer::new(
        Syntax::Typescript(Default::default()),
        Default::default(),
        StringInput::from(&*fm),
        None,
    );
    let mut parser = Parser::new_from(lexer);
    parser.parse_module().map_err(|e| anyhow!("{:?}", e))
}

fn get_ascii_type(n: u32) -> TypeSignature {
    use clarity::vm::types::{BufferLength, StringSubtype::ASCII};
    TypeSignature::SequenceType(SequenceSubtype::StringType(ASCII(
        BufferLength::try_from(n).unwrap(),
    )))
}

fn get_utf8_type(n: u32) -> TypeSignature {
    use clarity::vm::types::{StringSubtype::UTF8, StringUTF8Length};
    TypeSignature::SequenceType(SequenceSubtype::StringType(UTF8(
        StringUTF8Length::try_from(n).unwrap(),
    )))
}

fn extract_numeric_type_param(type_params: Option<&TsTypeParamInstantiation>) -> Result<u32> {
    let param = type_params
        .and_then(|params| params.params.first())
        .ok_or_else(|| anyhow!("Missing type parameter"))?;

    if let TsType::TsLitType(lit_type) = param.as_ref() {
        if let swc_ecma_ast::TsLit::Number(num_lit) = &lit_type.lit {
            return Ok(num_lit.value as u32);
        }
    }
    Err(anyhow!("Expected numeric literal type parameter"))
}

fn extract_type(
    type_ident: &str,
    type_params: Option<&TsTypeParamInstantiation>,
) -> Result<TypeSignature> {
    match type_ident {
        "Uint" => Ok(TypeSignature::UIntType),
        "Int" => Ok(TypeSignature::IntType),
        "Bool" => Ok(TypeSignature::BoolType),
        "StringAscii" => extract_numeric_type_param(type_params).map(get_ascii_type),
        "StringUtf8" => extract_numeric_type_param(type_params).map(get_utf8_type),
        _ => Err(anyhow!("Unknown type: {}", type_ident)),
    }
}

fn arg_type_to_signature(ts_type: &TsType) -> Result<TypeSignature> {
    if let TsType::TsTypeRef(TsTypeRef {
        type_name: TsEntityName::Ident(type_ident),
        type_params,
        ..
    }) = ts_type
    {
        extract_type(type_ident.sym.as_str(), type_params.as_deref())
    } else {
        Err(anyhow!("Expected TsTypeRef with Ident type name"))
    }
}

fn extract_var_name(decl: &VarDeclarator) -> Result<String> {
    decl.name
        .as_ident()
        .map(|id| id.sym.to_string())
        .ok_or_else(|| anyhow!("Expected identifier for variable name"))
}

fn extract_var_expr(new_expr: &NewExpr) -> Option<Expr> {
    new_expr.args.as_ref()?.first().map(|arg| *arg.expr.clone())
}

fn parse_function_params(params: &[swc_ecma_ast::Param]) -> Result<Vec<(String, TypeSignature)>> {
    params
        .iter()
        .map(|param| {
            if let swc_ecma_ast::Pat::Ident(ident) = &param.pat {
                let param_name = ident.id.sym.to_string();
                let param_type = ident
                    .type_ann
                    .as_ref()
                    .ok_or_else(|| anyhow!("Missing type annotation for param '{}'.", param_name))
                    .and_then(|type_ann_box| {
                        arg_type_to_signature(&type_ann_box.type_ann)
                            .map_err(|e| anyhow!("Invalid param type for '{}': {}", param_name, e))
                    })?;
                Ok((param_name, param_type))
            } else {
                Err(anyhow!("Expected identifier for parameter."))
            }
        })
        .collect()
}

impl Visit for IR {
    fn visit_var_decl(&mut self, var_decl: &swc_ecma_ast::VarDecl) {
        if var_decl.kind == VarDeclKind::Const {
            for decl in &var_decl.decls {
                let Some(init) = &decl.init else { continue };
                let Expr::New(new_expr) = &**init else {
                    continue;
                };
                let Some(callee_ident) = new_expr.callee.as_ident() else {
                    continue;
                };
                let name = match extract_var_name(decl) {
                    Ok(n) => n,
                    Err(_) => continue,
                };
                match callee_ident.sym.as_str() {
                    "Constant" => {
                        let type_args = new_expr.type_args.as_deref().unwrap();
                        let typ = arg_type_to_signature(type_args.params.first().unwrap());
                        let expr = extract_var_expr(new_expr).unwrap_or_default();
                        if let Ok(typ) = typ {
                            self.constants.push(IRConstant { name, typ, expr });
                        }
                    }
                    "DataVar" => {
                        let type_args = new_expr.type_args.as_deref().unwrap();
                        let typ = arg_type_to_signature(type_args.params.first().unwrap());
                        let expr = extract_var_expr(new_expr).unwrap_or_default();
                        if let Ok(typ) = typ {
                            self.data_vars.push(IRDataVar { name, typ, expr });
                        }
                    }
                    "DataMap" => {
                        let type_args = new_expr.type_args.as_deref().unwrap();
                        let key_typ = arg_type_to_signature(type_args.params.first().unwrap())
                            .expect("Expected key type");
                        let value_typ = arg_type_to_signature(type_args.params.get(1).unwrap())
                            .expect("Expected value type");
                        self.data_maps.push(IRDataMap {
                            name,
                            key_typ,
                            value_typ,
                        });
                    }
                    _ => {}
                }
            }
        }
        var_decl.visit_children_with(self);
    }

    fn visit_fn_decl(&mut self, fn_decl: &swc_ecma_ast::FnDecl) {
        if fn_decl.function.is_async || fn_decl.function.is_generator {
            return;
        }
        let name = fn_decl.ident.sym.to_string();

        let params = match parse_function_params(&fn_decl.function.params) {
            Ok(p) => p,
            Err(_) => return,
        };

        let return_type = if let Some(type_ann_box) = &fn_decl.function.return_type {
            match arg_type_to_signature(&type_ann_box.type_ann) {
                Ok(t) => t,
                Err(_) => return,
            }
        } else {
            TypeSignature::BoolType
        };

        let Some(body) = fn_decl.function.body.clone() else {
            return;
        };

        self.functions.push(IRFunction {
            name,
            define_type: DefineType::Private,
            params,
            return_type,
            body,
        });
    }

    fn visit_module_item(&mut self, item: &swc_ecma_ast::ModuleItem) {
        if let swc_ecma_ast::ModuleItem::Stmt(swc_ecma_ast::Stmt::Expr(expr_stmt)) = item {
            self.top_level_exprs.push(*expr_stmt.expr.clone());
        }
        item.visit_children_with(self);
    }
}

pub fn get_ir(file_name: &str, source: String) -> IR {
    let module = parse_ts(file_name, source.clone()).expect("Failed to parse TypeScript");
    let mut ir = IR {
        source,
        constants: Vec::new(),
        data_vars: Vec::new(),
        data_maps: Vec::new(),
        functions: Vec::new(),
        top_level_exprs: Vec::new(),
    };
    module.visit_with(&mut ir);
    ir
}

#[cfg(test)]
mod test {
    use super::{get_ir, IRConstant, IRDataVar};
    use crate::parser::{get_ascii_type, get_utf8_type, IRDataMap, IR};
    use clarity::vm::types::TypeSignature::*;
    use indoc::indoc;
    use swc_atoms::Atom;
    use swc_common::{Span, Spanned};
    use swc_ecma_ast::{Expr, Lit, Number, Str};

    #[track_caller]
    fn get_tmp_ir(src: &str) -> IR {
        get_ir("tmp.clar.ts", src.to_string())
    }

    fn expr_number(value: f64, span: Span) -> Expr {
        Expr::Lit(Lit::Num(Number {
            span,
            value,
            raw: Some(Atom::from(value.to_string())),
        }))
    }

    fn expr_string(value: &str, span: Span) -> Expr {
        Expr::Lit(Lit::Str(Str {
            span,
            value: value.into(),
            raw: Some(Atom::from(format!("\"{}\"", value))),
        }))
    }

    fn expr_bool(value: bool, span: Span) -> Expr {
        use swc_ecma_ast::{Bool, Expr, Lit};
        Expr::Lit(Lit::Bool(Bool { span, value }))
    }

    #[test]
    fn test_constant_ir() {
        let src = indoc!(
            r#"const OWNER_ROLE = new Constant<Uint>(1);
            const COST = new Constant<Int>(10);
            const HELLO = new Constant<StringAscii<32>>("World");"#
        );
        let constants = get_tmp_ir(src).constants;
        let expected = IRConstant {
            name: "OWNER_ROLE".to_string(),
            typ: UIntType,
            expr: expr_number(1.0, constants[0].expr.span()),
        };
        assert_eq!(constants[0], expected);
        let expected = IRConstant {
            name: "COST".to_string(),
            typ: IntType,
            expr: expr_number(10.0, constants[1].expr.span()),
        };
        assert_eq!(constants[1], expected);
        let expected = IRConstant {
            name: "HELLO".to_string(),
            typ: get_ascii_type(32),
            expr: expr_string("World", constants[2].expr.span()),
        };
        assert_eq!(constants[2], expected);
    }

    #[test]
    fn test_data_var_ir() {
        let src = indoc!(
            "const count = new DataVar<Int>(42);
            const ucount = new DataVar<Uint>(1);
            const msg = new DataVar<StringAscii<16>>(\"hello\");
            const umsg = new DataVar<StringUtf8<64>>(\"world\");"
        );
        let vars = get_tmp_ir(src).data_vars;
        let expected_int = IRDataVar {
            name: "count".to_string(),
            typ: IntType,
            expr: expr_number(42.0, vars[0].expr.span()),
        };
        let expected_uint = IRDataVar {
            name: "ucount".to_string(),
            typ: UIntType,
            expr: expr_number(1.0, vars[1].expr.span()),
        };
        let expected_ascii = IRDataVar {
            name: "msg".to_string(),
            typ: get_ascii_type(16),
            expr: expr_string("hello", vars[2].expr.span()),
        };

        let expected_utf8 = IRDataVar {
            name: "umsg".to_string(),
            typ: get_utf8_type(64),
            expr: expr_string("world", vars[3].expr.span()),
        };
        assert_eq!(vars[0], expected_int);
        assert_eq!(vars[1], expected_uint);
        assert_eq!(vars[2], expected_ascii);
        assert_eq!(vars[3], expected_utf8);
    }

    #[test]
    fn test_var_can_be_expression() {
        let src = "const value = new DataVar<Uint>(1 + 2);";
        let expected = IRDataVar {
            name: "value".to_string(),
            typ: UIntType,
            expr: Expr::from("1 + 2"),
        };
        let ir = get_tmp_ir(src).data_vars[0].clone();
        assert_eq!(ir.name, expected.name);
        assert_eq!(ir.typ, expected.typ);
        ir.expr.expect_bin();
    }

    #[test]
    fn test_data_var_bool_ir() {
        let src = "const isActive = new DataVar<Bool>(true);";
        let ir = get_tmp_ir(src).data_vars[0].clone();
        let expected = IRDataVar {
            name: "isActive".to_string(),
            typ: BoolType,
            expr: expr_bool(true, ir.expr.span()),
        };
        assert_eq!(ir, expected);
    }

    #[test]
    fn test_data_map_ir() {
        let src = "const statuses = new DataMap<Uint, Bool>();";
        let expected = IRDataMap {
            name: "statuses".to_string(),
            key_typ: UIntType,
            value_typ: BoolType,
        };
        assert_eq!(get_tmp_ir(src).data_maps[0], expected);
    }

    #[test]
    fn test_multiple_var_types() {
        let src = indoc!(
            "const a = new Constant<Uint>(12);
            const b = new DataVar<Uint>(100);
            const c = new DataMap<StringAscii<2>, StringUtf8<4>>();"
        );
        let ir = get_tmp_ir(src);
        assert_eq!(
            ir.constants[0],
            IRConstant {
                name: "a".to_string(),
                typ: UIntType,
                expr: expr_number(12.0, ir.constants[0].expr.span()),
            },
        );
        assert_eq!(
            ir.data_vars[0],
            IRDataVar {
                name: "b".to_string(),
                typ: UIntType,
                expr: expr_number(100.0, ir.data_vars[0].expr.span()),
            },
        );
        assert_eq!(
            ir.data_maps[0],
            IRDataMap {
                name: "c".to_string(),
                key_typ: get_ascii_type(2),
                value_typ: get_utf8_type(4),
            },
        );
    }

    #[test]
    fn test_top_level_expressions() {
        let src = "const n = new DataVar<Int>(0); n.set(1);";
        let ir = get_tmp_ir(src);
        assert_eq!(ir.top_level_exprs.len(), 1);
    }

    #[test]
    fn test_basic_function_ir() {
        let src = "function add(a: Int, b: Int): Int { return a + b }";
        let ir = get_tmp_ir(src);
        assert_eq!(ir.functions.len(), 1);

        let func = &ir.functions[0];
        let expected_params = vec![("a".to_string(), IntType), ("b".to_string(), IntType)];
        assert_eq!(func.name, "add");
        assert_eq!(func.params, expected_params);
        assert_eq!(func.return_type, IntType);
    }

    #[test]
    fn test_function_update_var() {
        let src = indoc! {"
            const n = new DataVar<Int>(0);
            function setN(newValue: Int) {
                return n.set(newValue);
            }
        "};

        let ir = get_tmp_ir(src);
        assert_eq!(ir.functions.len(), 1);
        let func = &ir.functions[0];
        assert_eq!(func.name, "setN");
        let expected_params = vec![("newValue".to_string(), IntType)];
        assert_eq!(func.params, expected_params);
        assert_eq!(func.return_type, BoolType);
    }
}
