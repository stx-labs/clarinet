// parser.rs parses the TypeScript code and creates the intermediate representation (IR) for the TypeScript to Clar conversion.
// It extracts the top-level declarations and their types.
//  - define-constant
//  - define-data-var
//  - define-data-map
//  - define-read-only
//  - define-public
//  - define-private

use anyhow::{anyhow, Result};
use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::{
    Expression, Function, Program, PropertyKey, Statement, TSLiteral, TSSignature, TSType,
    TSTypeParameterInstantiation, VariableDeclaration, VariableDeclarator,
};

use oxc_parser::Parser;
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;

use oxc_traverse::{traverse_mut_with_ctx, ReusableTraverseCtx, Traverse, TraverseCtx};

use clarity::vm::callables::DefineType;
use clarity::vm::types::{SequenceSubtype, TupleTypeSignature, TypeSignature};
use clarity::vm::ClarityName;

pub struct IRConstant<'a> {
    pub name: String,
    pub r#type: TypeSignature,
    pub expr: Expression<'a>,
}

pub struct IRDataVar<'a> {
    pub name: String,
    pub r#type: TypeSignature,
    pub expr: Expression<'a>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct IRDataMap {
    pub name: String,
    pub key_type: TypeSignature,
    pub value_type: TypeSignature,
}

pub struct IRFunction<'a> {
    pub name: String,
    pub define_type: DefineType,
    pub params: Vec<(String, TypeSignature)>,
    pub return_type: TypeSignature,
    pub body: oxc_allocator::Vec<'a, Statement<'a>>,
}

pub struct IR<'a> {
    allocator: &'a Allocator,
    pub constants: Vec<IRConstant<'a>>,
    pub data_vars: Vec<IRDataVar<'a>>,
    pub data_maps: Vec<IRDataMap>,
    pub functions: Vec<IRFunction<'a>>,
    pub top_level_exprs: Vec<Expression<'a>>,
}

fn parse_ts<'a>(allocator: &'a Allocator, file_name: &str, src: &'a str) -> Result<Program<'a>> {
    let source_type = SourceType::from_path(file_name).unwrap_or_default();
    let parser_return = Parser::new(allocator, src, source_type).parse();

    if !parser_return.errors.is_empty() {
        return Err(anyhow!("Parser errors: {:?}", parser_return.errors));
    }

    Ok(parser_return.program)
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

fn extract_numeric_type_param(type_params: Option<&TSTypeParameterInstantiation>) -> Result<u32> {
    let param = type_params
        .and_then(|params| params.params.first())
        .ok_or_else(|| anyhow!("Missing type parameter"))?;

    if let TSType::TSLiteralType(boxed_type) = param {
        if let TSLiteral::NumericLiteral(num_lit) = &boxed_type.literal {
            return Ok(num_lit.value as u32);
        }
    }
    Err(anyhow!("Expected numeric literal type parameter"))
}

fn extract_type(
    type_ident: &str,
    type_params: Option<&TSTypeParameterInstantiation>,
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

fn ts_to_clar_type(ts_type: &TSType) -> Result<TypeSignature> {
    match ts_type {
        TSType::TSTypeReference(boxed_ref) => {
            let type_name = boxed_ref.type_name.get_identifier_reference();
            extract_type(type_name.name.as_str(), boxed_ref.type_arguments.as_deref())
        }
        TSType::TSTypeLiteral(boxed_lit) => {
            let members = boxed_lit
                .members
                .iter()
                .map(|member| match member {
                    TSSignature::TSPropertySignature(prop_signature) => {
                        let key = &prop_signature.key;
                        let type_annotation = &prop_signature.type_annotation;
                        if let Some(type_annotation) = type_annotation {
                            match key {
                                PropertyKey::StaticIdentifier(ident) => {
                                    let name = ClarityName::from(ident.name.as_str());
                                    let member_type =
                                        ts_to_clar_type(&type_annotation.type_annotation)?;
                                    Ok((name, member_type))
                                }
                                _ => Err(anyhow!("Expected identifier for property key")),
                            }
                        } else {
                            Err(anyhow!("Missing type annotation"))
                        }
                    }
                    _ => Err(anyhow!("Unexpected type for member: {:?}", member)),
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(TypeSignature::TupleType(TupleTypeSignature::try_from(
                members,
            )?))
        }
        _ => Err(anyhow!("Unexpected type: {:?}", ts_type)),
    }
}

fn extract_var_expr<'a>(
    new_expr: &'a oxc_ast::ast::NewExpression<'a>,
) -> Option<&'a Expression<'a>> {
    let first_arg = new_expr.arguments.first();
    first_arg.map(|arg| arg.to_expression())
}

fn parse_function_params(
    params: &[oxc_ast::ast::FormalParameter],
) -> Result<Vec<(String, TypeSignature)>> {
    params
        .iter()
        .map(|param| {
            if let Some(ident) = param.pattern.get_binding_identifier() {
                let param_name = ident.name.to_string();

                let param_type = param
                    .pattern
                    .type_annotation
                    .as_ref()
                    .ok_or_else(|| anyhow!("Missing type annotation for param '{}'.", param_name))
                    .and_then(|type_ann| {
                        ts_to_clar_type(&type_ann.type_annotation)
                            .map_err(|e| anyhow!("Invalid param type for '{}': {}", param_name, e))
                    })?;
                Ok((param_name, param_type))
            } else {
                Err(anyhow!("Expected identifier for parameter."))
            }
        })
        .collect()
}

impl<'a> Traverse<'a> for IR<'a> {
    fn enter_variable_declaration(
        &mut self,
        node: &mut VariableDeclaration<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        if node.kind == oxc_ast::ast::VariableDeclarationKind::Const {
            for decl in &node.declarations {
                let VariableDeclarator { id, init, .. } = decl;
                let Some(init) = init else { continue };
                let Expression::NewExpression(new_expr) = init else {
                    continue;
                };
                let Some(callee_ident) = new_expr.callee.get_identifier_reference() else {
                    continue;
                };
                let name = match id.get_binding_identifier() {
                    Some(n) => n.name.to_string(),
                    None => continue,
                };
                match callee_ident.name.as_str() {
                    "Constant" => {
                        let type_args = new_expr.type_arguments.as_ref().unwrap();
                        let r#type = ts_to_clar_type(&type_args.params[0]);
                        match r#type {
                            Ok(r#type) => {
                                let expr = extract_var_expr(new_expr).unwrap();
                                self.constants.push(IRConstant {
                                    name,
                                    r#type,
                                    // todo: explore if we can avoid cloning the expression
                                    expr: expr.clone_in(self.allocator),
                                });
                            }
                            Err(e) => {
                                println!("Error: {}", e);
                            }
                        }
                    }
                    "DataVar" => {
                        let type_args = new_expr.type_arguments.as_ref().unwrap();
                        let r#type = ts_to_clar_type(&type_args.params[0]);
                        match r#type {
                            Ok(r#type) => {
                                let expr = extract_var_expr(new_expr).unwrap();
                                self.data_vars.push(IRDataVar {
                                    name,
                                    r#type,
                                    // todo: explore if we can avoid cloning the expression
                                    expr: expr.clone_in(self.allocator),
                                });
                            }
                            Err(e) => {
                                println!("Error: {}", e);
                            }
                        }
                    }
                    "DataMap" => {
                        let params = &new_expr.type_arguments.as_ref().unwrap().params;
                        let key_type = ts_to_clar_type(&params[0]).expect("Invalid key type");
                        let value_type = ts_to_clar_type(&params[1]).expect("Invalid value type");
                        self.data_maps.push(IRDataMap {
                            name,
                            key_type,
                            value_type,
                        });
                    }
                    _ => {}
                }
            }
        }
    }

    fn enter_function(&mut self, node: &mut Function<'a>, _ctx: &mut TraverseCtx<'a>) {
        if node.generator || node.r#async {
            return;
        }
        let name = node.id.as_ref().unwrap().name.to_string();
        let params = parse_function_params(&node.params.items);
        let return_type = if let Some(type_ann) = &node.return_type {
            match ts_to_clar_type(&type_ann.type_annotation) {
                Ok(t) => t,
                Err(_) => return,
            }
        } else {
            TypeSignature::BoolType
        };
        let Some(body) = node.body.as_ref() else {
            return;
        };
        let Ok(params) = params else {
            return;
        };

        self.functions.push(IRFunction {
            name,
            define_type: DefineType::Private,
            params,
            return_type,
            body: body.statements.clone_in(self.allocator),
        });
    }

    fn enter_statement(&mut self, node: &mut Statement<'a>, _ctx: &mut TraverseCtx<'a>) {
        if let Statement::ExpressionStatement(expr_stmt) = node {
            let expr = expr_stmt.expression.clone_in(self.allocator);
            self.top_level_exprs.push(expr);
        }
    }
}

pub fn get_ir<'a>(allocator: &'a Allocator, file_name: &str, source: &'a str) -> IR<'a> {
    let mut module = parse_ts(allocator, file_name, source).expect("Failed to parse TypeScript");

    let mut ir = IR {
        allocator,
        constants: Vec::new(),
        data_vars: Vec::new(),
        data_maps: Vec::new(),
        functions: Vec::new(),
        top_level_exprs: Vec::new(),
    };
    let scoping = SemanticBuilder::new()
        .build(&module)
        .semantic
        .into_scoping();
    traverse_mut_with_ctx(
        &mut ir,
        &mut module,
        &mut ReusableTraverseCtx::new(scoping, allocator),
    );

    ir
}

#[cfg(test)]
mod test {
    use crate::parser::{
        get_ascii_type, get_ir, get_utf8_type, IRConstant, IRDataMap, IRDataVar, IR,
    };

    use clarity::vm::{
        callables::DefineType,
        types::{
            TupleTypeSignature,
            TypeSignature::{self, *},
        },
        ClarityName,
    };
    use indoc::indoc;
    use oxc_allocator::{Allocator, Box, FromIn};
    use oxc_ast::{
        ast::{
            BinaryOperator, Expression, NumberBase, ObjectPropertyKind, PropertyKey, PropertyKind,
            Statement,
        },
        AstBuilder,
    };
    use oxc_span::{Atom, Span};

    #[track_caller]
    fn get_tmp_ir<'a>(allocator: &'a Allocator, src: &'a str) -> IR<'a> {
        get_ir(allocator, "tmp.clar.ts", src)
    }

    fn expr_number<'a>(allocator: &'a Allocator, value: f64) -> Expression<'a> {
        AstBuilder::new(allocator).expression_numeric_literal(
            Span::empty(0),
            value,
            Some(Atom::from_in(value.to_string(), allocator)),
            NumberBase::Decimal,
        )
    }

    fn expr_string<'a>(allocator: &'a Allocator, value: &'a str) -> Expression<'a> {
        AstBuilder::new(allocator).expression_string_literal(
            Span::empty(0),
            value,
            Some(Atom::from_in(value.to_string(), allocator)),
        )
    }

    fn expr_bool<'a>(allocator: &'a Allocator, value: bool) -> Expression<'a> {
        AstBuilder::new(allocator).expression_boolean_literal(Span::empty(0), value)
    }

    fn expr_binary<'a>(
        allocator: &'a Allocator,
        left: Expression<'a>,
        right: Expression<'a>,
        operator: BinaryOperator,
    ) -> Expression<'a> {
        let expr =
            AstBuilder::new(allocator).binary_expression(Span::empty(0), left, operator, right);
        Expression::BinaryExpression(Box::new_in(expr, allocator))
    }

    fn simple_object_property<'a>(
        builder: &'a AstBuilder<'a>,
        key: &'a str,
        value: Expression<'a>,
    ) -> ObjectPropertyKind<'a> {
        let k = builder.property_key_static_identifier(Span::default(), key);
        builder.object_property_kind_object_property(
            Span::default(),
            PropertyKind::Init,
            k,
            value,
            false,
            false,
            false,
        )
    }

    #[track_caller]
    fn assert_expr_eq(actual: &Expression, expected: &Expression) {
        use Expression::*;
        match (&actual, &expected) {
            (NumericLiteral(actual_num), NumericLiteral(expected_num)) => {
                assert_eq!(actual_num.value, expected_num.value);
            }
            (StringLiteral(actual_str), StringLiteral(expected_str)) => {
                assert_eq!(actual_str.value, expected_str.value);
            }
            (BooleanLiteral(actual_bool), BooleanLiteral(expected_bool)) => {
                assert_eq!(actual_bool.value, expected_bool.value);
            }
            (BinaryExpression(actual_bin), BinaryExpression(expected_bin)) => {
                assert_eq!(actual_bin.operator, expected_bin.operator);
                assert_expr_eq(&actual_bin.left, &expected_bin.left);
                assert_expr_eq(&actual_bin.right, &expected_bin.right);
            }
            (ObjectExpression(actual_obj), ObjectExpression(expected_obj)) => {
                assert_eq!(actual_obj.properties.len(), expected_obj.properties.len());
                for (actual_prop, expected_prop) in actual_obj
                    .properties
                    .iter()
                    .zip(expected_obj.properties.iter())
                {
                    match (actual_prop, expected_prop) {
                        (
                            ObjectPropertyKind::ObjectProperty(actual_prop),
                            ObjectPropertyKind::ObjectProperty(expected_prop),
                        ) => {
                            match (&actual_prop.key, &expected_prop.key) {
                                (
                                    PropertyKey::StaticIdentifier(actual_key),
                                    PropertyKey::StaticIdentifier(expected_key),
                                ) => {
                                    assert_eq!(actual_key.name, expected_key.name);
                                }
                                _ => panic!("Expected matching expression types"),
                            }
                            assert_expr_eq(&actual_prop.value, &expected_prop.value);
                        }
                        _ => panic!("Expected matching expression types"),
                    }
                }
            }
            _ => panic!("Expected matching expression types"),
        }
    }

    fn assert_constant_eq(actual: &IRConstant, expected: &IRConstant) {
        assert_eq!(actual.name, expected.name);
        pretty_assertions::assert_eq!(actual.r#type, expected.r#type);
        assert_expr_eq(&actual.expr, &expected.expr);
    }

    fn assert_data_var_eq(actual: &IRDataVar, expected: &IRDataVar) {
        assert_eq!(actual.name, expected.name);
        pretty_assertions::assert_eq!(actual.r#type, expected.r#type);
        assert_expr_eq(&actual.expr, &expected.expr);
    }

    #[test]
    fn test_constant_ir() {
        let allocator = Allocator::default();
        let src = indoc!(
            r#"const OWNER_ROLE = new Constant<Uint>(1);
            const COST = new Constant<Int>(10);
            const HELLO = new Constant<StringAscii<32>>("World");"#
        );
        let constants = get_tmp_ir(&allocator, src).constants;
        let expected = IRConstant {
            name: "OWNER_ROLE".to_string(),
            r#type: UIntType,
            expr: expr_number(&allocator, 1.0),
        };
        assert_constant_eq(&constants[0], &expected);

        let expected = IRConstant {
            name: "COST".to_string(),
            r#type: IntType,
            expr: expr_number(&allocator, 10.0),
        };
        assert_constant_eq(&constants[1], &expected);
        let expected = IRConstant {
            name: "HELLO".to_string(),
            r#type: get_ascii_type(32),
            expr: expr_string(&allocator, "World"),
        };
        assert_constant_eq(&constants[2], &expected);
    }

    #[test]
    fn test_data_var_ir() {
        let allocator = Allocator::default();
        let src = indoc!(
            "const count = new DataVar<Int>(42);
            const ucount = new DataVar<Uint>(1);
            const msg = new DataVar<StringAscii<16>>(\"hello\");
            const umsg = new DataVar<StringUtf8<64>>(\"world\");"
        );
        let vars = get_tmp_ir(&allocator, src).data_vars;
        let expected_int = IRDataVar {
            name: "count".to_string(),
            r#type: IntType,
            expr: expr_number(&allocator, 42.0),
        };
        assert_data_var_eq(&vars[0], &expected_int);
        let expected_uint = IRDataVar {
            name: "ucount".to_string(),
            r#type: UIntType,
            expr: expr_number(&allocator, 1.0),
        };
        assert_data_var_eq(&vars[1], &expected_uint);
        let expected_ascii = IRDataVar {
            name: "msg".to_string(),
            r#type: get_ascii_type(16),
            expr: expr_string(&allocator, "hello"),
        };
        assert_data_var_eq(&vars[2], &expected_ascii);
        let expected_utf8 = IRDataVar {
            name: "umsg".to_string(),
            r#type: get_utf8_type(64),
            expr: expr_string(&allocator, "world"),
        };
        assert_data_var_eq(&vars[3], &expected_utf8);
    }

    #[test]
    fn test_var_can_be_expression() {
        let src = "const value = new DataVar<Uint>(1 + 2);";
        let allocator = Allocator::default();
        let expected = IRDataVar {
            name: "value".to_string(),
            r#type: UIntType,
            expr: expr_binary(
                &allocator,
                expr_number(&allocator, 1.0),
                expr_number(&allocator, 2.0),
                BinaryOperator::Addition,
            ),
        };
        let ir = get_tmp_ir(&allocator, src);
        assert_data_var_eq(&ir.data_vars[0], &expected);
    }

    #[test]
    fn test_data_var_bool_ir() {
        let src = "const isActive = new DataVar<Bool>(true);";
        let allocator = Allocator::default();
        let ir = &get_tmp_ir(&allocator, src).data_vars[0];
        let expected = IRDataVar {
            name: "isActive".to_string(),
            r#type: BoolType,
            expr: expr_bool(&allocator, true),
        };
        assert_data_var_eq(ir, &expected);
    }

    #[test]
    fn test_data_map_ir() {
        let src = "const statuses = new DataMap<Uint, Bool>();";
        let expected = IRDataMap {
            name: "statuses".to_string(),
            key_type: UIntType,
            value_type: BoolType,
        };
        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, src);
        assert_eq!(ir.data_maps[0], expected);
    }

    #[test]
    fn test_multiple_var_types() {
        let src = indoc!(
            "const a = new Constant<Uint>(12);
            const b = new DataVar<Uint>(100);
            const c = new DataMap<StringAscii<2>, StringUtf8<4>>();"
        );
        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, src);
        let expected_constant = IRConstant {
            name: "a".to_string(),
            r#type: UIntType,
            expr: expr_number(&allocator, 12.0),
        };
        let expected_data_var = IRDataVar {
            name: "b".to_string(),
            r#type: UIntType,
            expr: expr_number(&allocator, 100.0),
        };
        let expected_data_map = IRDataMap {
            name: "c".to_string(),
            key_type: get_ascii_type(2),
            value_type: get_utf8_type(4),
        };
        assert_constant_eq(&ir.constants[0], &expected_constant);
        assert_data_var_eq(&ir.data_vars[0], &expected_data_var);
        assert_eq!(&ir.data_maps[0], &expected_data_map);
    }

    #[test]
    fn test_tuple_type_data_var_ir() {
        let src = "const state = new DataVar<{ active: Bool, ok: Uint }>({ active: true, ok: 1 });";
        let allocator = Allocator::default();
        assert_eq!(get_tmp_ir(&allocator, src).data_vars.len(), 1);
        let ir = &get_tmp_ir(&allocator, src).data_vars[0];
        let tuple_type = TypeSignature::TupleType(
            TupleTypeSignature::try_from(vec![
                (ClarityName::from("active"), TypeSignature::BoolType),
                (ClarityName::from("ok"), TypeSignature::UIntType),
            ])
            .unwrap(),
        );
        let b = AstBuilder::new(&allocator);
        let span = Span::default();
        let val = b.expression_boolean_literal(span, true);
        let prop_active = simple_object_property(&b, "active", val);
        let val = b.expression_numeric_literal(span, 1.0, Some(b.atom("1")), NumberBase::Decimal);
        let prop_ok = simple_object_property(&b, "ok", val);

        let expected = IRDataVar {
            name: "state".to_string(),
            r#type: tuple_type,
            expr: b.expression_object(span, b.vec_from_iter(vec![prop_active, prop_ok])),
        };
        assert_data_var_eq(ir, &expected);
    }

    #[test]
    fn test_tuple_type_constant_ir() {
        let src = "const state = new Constant<{ active: Bool }>({ active: true });";
        let allocator = Allocator::default();
        assert_eq!(get_tmp_ir(&allocator, src).constants.len(), 1);
        let ir = &get_tmp_ir(&allocator, src).constants[0];

        let tuple_type = TypeSignature::TupleType(
            TupleTypeSignature::try_from(vec![(
                ClarityName::from("active"),
                TypeSignature::BoolType,
            )])
            .unwrap(),
        );

        let b = AstBuilder::new(&allocator);
        let span = Span::default();
        let val = b.expression_boolean_literal(span, true);
        let prop_active = simple_object_property(&b, "active", val);

        let expected = IRConstant {
            name: "state".to_string(),
            r#type: tuple_type,
            expr: b.expression_object(span, b.vec_from_iter(vec![prop_active])),
        };
        assert_constant_eq(ir, &expected);
    }

    #[test]
    fn test_tuple_type_data_map() {
        use clarity::vm::types::TypeSignature::*;
        let src = "const state = new DataMap<{ ok: Uint }, { active: Bool }>();";
        let allocator = Allocator::default();
        assert_eq!(get_tmp_ir(&allocator, src).data_maps.len(), 1);
        let ir = &get_tmp_ir(&allocator, src).data_maps[0];

        let key_tuple_type = TupleType(
            TupleTypeSignature::try_from(vec![(ClarityName::from("ok"), UIntType)]).unwrap(),
        );
        let value_tuple_type = TupleType(
            TupleTypeSignature::try_from(vec![(ClarityName::from("active"), BoolType)]).unwrap(),
        );

        let expected = IRDataMap {
            name: "state".to_string(),
            key_type: key_tuple_type,
            value_type: value_tuple_type,
        };

        assert_eq!(ir, &expected);
    }

    #[test]
    fn test_top_level_expressions() {
        let src = "const n = new DataVar<Int>(0); n.set(1);";
        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, src);
        assert_eq!(ir.top_level_exprs.len(), 1);
    }

    #[test]
    fn test_basic_function_ir() {
        let src = "function add(a: Int, b: Int): Int { return a + b }";
        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, src);
        assert_eq!(ir.functions.len(), 1);

        let func = &ir.functions[0];
        let expected_params = vec![("a".to_string(), IntType), ("b".to_string(), IntType)];
        assert_eq!(func.name, "add");
        assert_eq!(func.params, expected_params);
        assert_eq!(func.return_type, IntType);
        assert_eq!(func.define_type, DefineType::Private);
        assert_eq!(func.body.len(), 1);
        matches!(func.body[0], Statement::ReturnStatement(_));
    }

    #[test]
    fn test_function_update_var() {
        let src = indoc! {"
            const n = new DataVar<Int>(0);
            function setN(newValue: Int) {
                return n.set(newValue);
            }
        "};

        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, src);
        assert_eq!(ir.functions.len(), 1);
        let func = &ir.functions[0];
        assert_eq!(func.name, "setN");
        let expected_params = vec![("newValue".to_string(), IntType)];
        assert_eq!(func.params, expected_params);
        assert_eq!(func.return_type, BoolType);
    }
}
