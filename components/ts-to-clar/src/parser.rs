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
    Expr, Lit, Module, NewExpr, TsEntityName, TsType, TsTypeParamInstantiation, TsTypeRef,
    VarDeclKind, VarDeclarator,
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
    pub value: String,
}

/// Represents a data variable in the IR.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IRDataVar {
    pub name: String,
    pub typ: TypeSignature,
    pub value: String,
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

/// Extract a numeric type parameter from a type parameter instantiation.
fn extract_numeric_type_param(type_params: Option<&TsTypeParamInstantiation>) -> Option<u32> {
    type_params
        .and_then(|params| params.params.first())
        .and_then(|param| match &**param {
            TsType::TsLitType(lit_type) => match &lit_type.lit {
                swc_ecma_ast::TsLit::Number(num_lit) => Some(num_lit.value as u32),
                _ => None,
            },
            _ => None,
        })
}

/// Extract a TypeSignature from a type identifier and its parameters.
fn extract_type(
    type_ident: &str,
    type_params: Option<&TsTypeParamInstantiation>,
) -> Option<TypeSignature> {
    match type_ident {
        "Uint" => Some(TypeSignature::UIntType),
        "Int" => Some(TypeSignature::IntType),
        "Bool" => Some(TypeSignature::BoolType),
        "StringAscii" => extract_numeric_type_param(type_params).map(get_ascii_type),
        "StringUtf8" => extract_numeric_type_param(type_params).map(get_utf8_type),
        _ => None,
    }
}

/// Convert a TsType to a TypeSignature, if possible.
fn arg_type_to_signature(ts_type: &TsType) -> Option<TypeSignature> {
    if let TsType::TsTypeRef(TsTypeRef {
        type_name: TsEntityName::Ident(type_ident),
        type_params,
        ..
    }) = ts_type
    {
        extract_type(type_ident.sym.as_str(), type_params.as_deref())
    } else {
        None
    }
}

/// Extract the name from a variable declarator.
fn extract_var_name(decl: &VarDeclarator) -> Option<String> {
    decl.name.as_ident().map(|id| id.sym.to_string())
}

/// Extract the value from a new expression argument.
fn extract_new_expr_value(new_expr: &NewExpr) -> Option<String> {
    new_expr
        .args
        .as_ref()?
        .first()
        .map(|first_arg| match &*first_arg.expr {
            Expr::Lit(Lit::Num(num)) => format!("{}", num.value as i64),
            Expr::Lit(Lit::Str(s)) => s.value.to_string(),
            Expr::Lit(Lit::Bool(b)) => b.value.to_string(),
            _ => String::new(),
        })
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
                    Some(n) => n,
                    None => continue,
                };
                let value = extract_new_expr_value(new_expr).unwrap_or_default();
                match callee_ident.sym.as_str() {
                    "Const" => {
                        let type_args = new_expr.type_args.as_deref().unwrap();
                        let typ = arg_type_to_signature(type_args.params.first().unwrap());
                        if let Some(typ) = typ {
                            self.constants.push(IRConstant { name, typ, value });
                        }
                    }
                    "DataVar" => {
                        let type_args = new_expr.type_args.as_deref().unwrap();
                        let typ = arg_type_to_signature(type_args.params.first().unwrap());
                        if let Some(typ) = typ {
                            self.data_vars.push(IRDataVar { name, typ, value });
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

    #[track_caller]
    fn get_tmp_ir(src: &str) -> IR {
        get_ir("tmp.clar.ts", src.to_string())
    }

    #[test]
    fn test_constant_ir() {
        let src = indoc!(
            "
            const OWNER_ROLE = new Const<Uint>(1);
            const COST = new Const<Int>(10);
            const HELLO = new Const<StringAscii<32>>(\"World\");
        "
        );
        let constants = get_tmp_ir(src).constants;
        let expected = IRConstant {
            name: "OWNER_ROLE".to_string(),
            typ: UIntType,
            value: "1".to_string(),
        };
        assert_eq!(constants[0], expected);
        let expected = IRConstant {
            name: "COST".to_string(),
            typ: IntType,
            value: "10".to_string(),
        };
        assert_eq!(constants[1], expected);
        let expected = IRConstant {
            name: "HELLO".to_string(),
            typ: get_ascii_type(32),
            value: "World".to_string(),
        };
        assert_eq!(constants[2], expected);
    }

    #[test]
    fn test_data_var_ir() {
        let src = indoc!(
            "
            const count = new DataVar<Int>(42);
            const ucount = new DataVar<Uint>(1);
            const msg = new DataVar<StringAscii<16>>(\"hello\");
            const umsg = new DataVar<StringUtf8<64>>(\"world\");
        "
        );
        let ir = get_tmp_ir(src);
        let expected_int = IRDataVar {
            name: "count".to_string(),
            typ: IntType,
            value: "42".to_string(),
        };
        let expected_uint = IRDataVar {
            name: "ucount".to_string(),
            typ: UIntType,
            value: "1".to_string(),
        };
        let expected_ascii = IRDataVar {
            name: "msg".to_string(),
            typ: get_ascii_type(16),
            value: "hello".to_string(),
        };
        let expected_utf8 = IRDataVar {
            name: "umsg".to_string(),
            typ: get_utf8_type(64),
            value: "world".to_string(),
        };
        assert_eq!(ir.data_vars[0], expected_int);
        assert_eq!(ir.data_vars[1], expected_uint);
        assert_eq!(ir.data_vars[2], expected_ascii);
        assert_eq!(ir.data_vars[3], expected_utf8);
    }

    #[test]
    fn test_data_var_bool_ir() {
        let src = "const isActive = new DataVar<Bool>(true);";
        let expected = IRDataVar {
            name: "isActive".to_string(),
            typ: BoolType,
            value: "true".to_string(),
        };
        assert_eq!(get_tmp_ir(src).data_vars[0], expected);
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
            "
            const a = new Const<Uint>(12);
            const b = new DataVar<Uint>(100);
            const c = new DataMap<StringAscii<2>, StringUtf8<4>>();
        "
        );
        let ir = get_tmp_ir(src);
        assert_eq!(
            ir.constants[0],
            IRConstant {
                name: "a".to_string(),
                typ: UIntType,
                value: "12".to_string(),
            }
        );
        assert_eq!(
            ir.data_vars[0],
            IRDataVar {
                name: "b".to_string(),
                typ: UIntType,
                value: "100".to_string(),
            }
        );
        assert_eq!(
            ir.data_maps[0],
            IRDataMap {
                name: "c".to_string(),
                key_typ: get_ascii_type(2),
                value_typ: get_utf8_type(4),
            }
        );
    }
}
