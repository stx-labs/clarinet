// parser.rs parses the TypeScript code and creates the intermediate representation (IR) for the TypeScript to Clar conversion.
// It extracts the top-level declarations and their types.
//  - define-constant
//  - define-data-var
//  - define-data-map
//  - define-read-only
//  - define-public
//  - define-private

use anyhow::Result;
use swc_common::{sync::Lrc, FileName, SourceMap};
use swc_ecma_ast::{
    Expr, Module, NewExpr, TsEntityName, TsType, TsTypeParamInstantiation, TsTypeRef, VarDeclKind,
    VarDeclarator,
};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax};
use swc_ecma_visit::{Visit, VisitWith};

use clarity::vm::callables::DefineType;
use clarity::vm::types::{SequenceSubtype, TypeSignature};

/// Represents a constant in the IR.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IRConstant {
    pub name: String,
    pub typ: TypeSignature,
    pub expr: Expr,
}

/// Represents a data variable in the IR.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IRDataVar {
    pub name: String,
    pub typ: TypeSignature,
    pub expr: Expr,
}

/// Represents a data map in the IR.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IRDataMap {
    pub name: String,
    pub key_typ: TypeSignature,
    pub value_typ: TypeSignature,
}

/// Represents a function in the IR.
#[derive(Debug, PartialEq, Clone)]
pub struct IRFunction {
    pub name: String,
    pub define_type: DefineType,
    pub params: Vec<(String, TypeSignature)>,
    pub return_type: TypeSignature,
    pub body: Expr,
}

/// The main IR structure.
#[derive(Debug, PartialEq, Clone)]
pub struct IR {
    pub source: String,
    pub constants: Vec<IRConstant>,
    pub data_vars: Vec<IRDataVar>,
    pub data_maps: Vec<IRDataMap>,
    pub functions: Vec<IRFunction>,
}

/// Parse TypeScript source into a SWC AST module.
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
    parser
        .parse_module()
        .map_err(|e| anyhow::anyhow!("{:?}", e))
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
        .ok_or_else(|| anyhow::anyhow!("Missing type parameter"))?;

    if let TsType::TsLitType(lit_type) = &**param {
        if let swc_ecma_ast::TsLit::Number(num_lit) = &lit_type.lit {
            return Ok(num_lit.value as u32);
        }
    }
    Err(anyhow::anyhow!("Expected numeric literal type parameter"))
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
        _ => Err(anyhow::anyhow!("Unknown type: {}", type_ident)),
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
        Err(anyhow::anyhow!("Expected TsTypeRef with Ident type name"))
    }
}

fn extract_var_name(decl: &VarDeclarator) -> Result<String> {
    decl.name
        .as_ident()
        .map(|id| id.sym.to_string())
        .ok_or_else(|| anyhow::anyhow!("Expected identifier for variable name"))
}

fn extract_var_expr(new_expr: &NewExpr) -> Option<Expr> {
    new_expr.args.as_ref()?.first().map(|arg| *arg.expr.clone())
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
                    "Const" => {
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
}

/// Build the IR from a TypeScript source file.
pub fn get_ir(file_name: &str, source: String) -> IR {
    let module = parse_ts(file_name, source.clone()).expect("Failed to parse TypeScript");
    let mut ir = IR {
        source,
        constants: Vec::new(),
        data_vars: Vec::new(),
        data_maps: Vec::new(),
        functions: Vec::new(),
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
            r#"const OWNER_ROLE = new Const<Uint>(1);
            const COST = new Const<Int>(10);
            const HELLO = new Const<StringAscii<32>>("World");"#
        );
        let constants = get_tmp_ir(src).constants;
        let expected = IRConstant {
            name: "OWNER_ROLE".to_string(),
            typ: UIntType,
            expr: expr_number(1.0, constants[0].expr.span()),
        };
        assert_eq!(expected, constants[0]);
        let expected = IRConstant {
            name: "COST".to_string(),
            typ: IntType,
            expr: expr_number(10.0, constants[1].expr.span()),
        };
        assert_eq!(expected, constants[1]);
        let expected = IRConstant {
            name: "HELLO".to_string(),
            typ: get_ascii_type(32),
            expr: expr_string("World", constants[2].expr.span()),
        };
        assert_eq!(expected, constants[2]);
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
        assert_eq!(expected_int, vars[0]);
        assert_eq!(expected_uint, vars[1]);
        assert_eq!(expected_ascii, vars[2]);
        assert_eq!(expected_utf8, vars[3]);
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
        assert_eq!(expected.name, ir.name);
        assert_eq!(expected.typ, ir.typ);
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
        assert_eq!(expected, ir);
    }

    #[test]
    fn test_data_map_ir() {
        let src = "const statuses = new DataMap<Uint, Bool>();";
        let expected = IRDataMap {
            name: "statuses".to_string(),
            key_typ: UIntType,
            value_typ: BoolType,
        };
        assert_eq!(expected, get_tmp_ir(src).data_maps[0]);
    }

    #[test]
    fn test_multiple_var_types() {
        let src = indoc!(
            "const a = new Const<Uint>(12);
            const b = new DataVar<Uint>(100);
            const c = new DataMap<StringAscii<2>, StringUtf8<4>>();"
        );
        let ir = get_tmp_ir(src);
        assert_eq!(
            IRConstant {
                name: "a".to_string(),
                typ: UIntType,
                expr: expr_number(12.0, ir.constants[0].expr.span()),
            },
            ir.constants[0],
        );
        assert_eq!(
            IRDataVar {
                name: "b".to_string(),
                typ: UIntType,
                expr: expr_number(100.0, ir.data_vars[0].expr.span()),
            },
            ir.data_vars[0],
        );
        assert_eq!(
            IRDataMap {
                name: "c".to_string(),
                key_typ: get_ascii_type(2),
                value_typ: get_utf8_type(4),
            },
            ir.data_maps[0],
        );
    }
}
