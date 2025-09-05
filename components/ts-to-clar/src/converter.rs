// converter.rs converts the TS intermediate representation (IR) to Clarity PreSymbolicExpressions (PSEs)

use std::vec;

use clarity::vm::representations::{PreSymbolicExpression, PreSymbolicExpressionType, Span};
use clarity::vm::types::{
    PrincipalData, SequenceSubtype, StringSubtype, TypeSignature as ClarityTypeSignature,
};
use clarity::vm::{ClarityName, Value as ClarityValue};
use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::{Expression as OxcExpression, ObjectPropertyKind};
use oxc_ast::AstBuilder;

use crate::parser::{IRConstant, IRDataMap, IRDataVar, IRFunction, IR};
use crate::{expression_converter, to_kebab_case};

fn type_signature_to_pse(
    type_signature: &ClarityTypeSignature,
) -> Result<PreSymbolicExpression, anyhow::Error> {
    Ok(match type_signature {
        ClarityTypeSignature::UIntType => PreSymbolicExpression::atom(ClarityName::from("uint")),
        ClarityTypeSignature::IntType => PreSymbolicExpression::atom(ClarityName::from("int")),
        ClarityTypeSignature::BoolType => PreSymbolicExpression::atom(ClarityName::from("bool")),
        ClarityTypeSignature::PrincipalType => {
            PreSymbolicExpression::atom(ClarityName::from("principal"))
        }
        ClarityTypeSignature::SequenceType(seq_subtype) => match seq_subtype {
            SequenceSubtype::StringType(string_subtype) => match string_subtype {
                StringSubtype::ASCII(len) => PreSymbolicExpression::list(vec![
                    PreSymbolicExpression::atom(ClarityName::from("string-ascii")),
                    PreSymbolicExpression::atom_value(ClarityValue::Int(u32::from(len).into())),
                ]),
                StringSubtype::UTF8(len) => PreSymbolicExpression::list(vec![
                    PreSymbolicExpression::atom(ClarityName::from("string-utf8")),
                    PreSymbolicExpression::atom_value(ClarityValue::Int(u32::from(len).into())),
                ]),
            },
            _ => return Err(anyhow::anyhow!("Unsupported sequence type")),
        },
        ClarityTypeSignature::TupleType(tuple_type) => {
            let data_map = tuple_type
                .get_type_map()
                .iter()
                .map(|(key, signature)| {
                    let name_expr =
                        PreSymbolicExpression::atom(ClarityName::from(key.to_string().as_str()));
                    let type_expr = type_signature_to_pse(signature)?;
                    Ok(vec![name_expr, type_expr])
                })
                .collect::<Result<Vec<_>, anyhow::Error>>()?
                .into_iter()
                .flatten()
                .collect();

            PreSymbolicExpression::tuple(data_map)
        }
        _ => return Err(anyhow::anyhow!("Unsupported type signature")),
    })
}

fn convert_expression_with_type(
    allocator: &Allocator,
    ir: &IR,
    expr: &OxcExpression,
    r#type: &ClarityTypeSignature,
) -> Result<PreSymbolicExpression, anyhow::Error> {
    if expr.is_call_expression() || expr.is_call_like_expression() || expr.is_binaryish() {
        // to convert top level expressions that are not inside a function body,
        // wrap them in a temporary function and call convert_function_body
        let builder = AstBuilder::new(allocator);
        let function =
            builder.statement_expression(oxc_span::Span::default(), expr.clone_in(allocator));

        let temp_ir_function = IRFunction {
            name: "temp".to_string(),
            parameters: vec![],
            return_type: Some(r#type.clone()),
            body: oxc_allocator::Vec::from_array_in([function], allocator),
        };

        return expression_converter::convert_function_body(allocator, ir, &temp_ir_function);
    }

    Ok(match r#type {
        ClarityTypeSignature::UIntType => match &expr {
            OxcExpression::NumericLiteral(num) => {
                PreSymbolicExpression::atom_value(ClarityValue::UInt(num.value as u128))
            }
            _ => return Err(anyhow::anyhow!("Invalid expression for UInt")),
        },
        ClarityTypeSignature::IntType => match &expr {
            OxcExpression::NumericLiteral(num) => {
                PreSymbolicExpression::atom_value(ClarityValue::Int(num.value as i128))
            }
            _ => return Err(anyhow::anyhow!("Invalid expression for Int")),
        },
        ClarityTypeSignature::BoolType => match &expr {
            OxcExpression::BooleanLiteral(bool) => {
                PreSymbolicExpression::atom(ClarityName::from(bool.value.to_string().as_str()))
            }
            _ => return Err(anyhow::anyhow!("Invalid expression for Bool")),
        },
        ClarityTypeSignature::PrincipalType => match &expr {
            OxcExpression::StringLiteral(str) => PreSymbolicExpression::atom_value(
                ClarityValue::Principal(PrincipalData::parse(str.value.as_str())?),
            ),
            OxcExpression::Identifier(ident) => {
                if ident.name == "txSender" {
                    PreSymbolicExpression::atom(ClarityName::from("tx-sender"))
                } else {
                    return Err(anyhow::anyhow!(
                        "Invalid identifier for Principal: {}",
                        ident.name
                    ));
                }
            }
            _ => return Err(anyhow::anyhow!("Invalid expression for Principal")),
        },
        ClarityTypeSignature::SequenceType(SequenceSubtype::StringType(StringSubtype::ASCII(
            _len,
        ))) => match &expr {
            OxcExpression::StringLiteral(str) => PreSymbolicExpression::atom_value(
                ClarityValue::string_ascii_from_bytes(str.value.to_string().into_bytes()).unwrap(),
            ),
            _ => return Err(anyhow::anyhow!("Invalid expression for ASCII")),
        },
        ClarityTypeSignature::SequenceType(SequenceSubtype::StringType(StringSubtype::UTF8(
            _len,
        ))) => match &expr {
            OxcExpression::StringLiteral(str) => PreSymbolicExpression::atom_value(
                ClarityValue::string_utf8_from_bytes(str.value.to_string().into_bytes()).unwrap(),
            ),
            _ => return Err(anyhow::anyhow!("Invalid expression for UTF8")),
        },
        ClarityTypeSignature::TupleType(tuple_type) => match &expr {
            OxcExpression::ObjectExpression(obj) => {
                let data_map: Vec<PreSymbolicExpression> = obj
                    .properties
                    .iter()
                    .map(|property| match property {
                        ObjectPropertyKind::ObjectProperty(property) => {
                            let key = property.key.static_name().unwrap();
                            let key_name = ClarityName::from(key.to_string().as_str());
                            let prop_type = tuple_type.get_type_map().get(&key_name).unwrap();
                            let value = convert_expression_with_type(
                                allocator,
                                ir,
                                &property.value,
                                &prop_type.clone(),
                            )?;
                            Ok(vec![PreSymbolicExpression::atom(key_name), value])
                        }
                        ObjectPropertyKind::SpreadProperty(_property) => {
                            Err(anyhow::anyhow!("Todo: spread property for Tuple"))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .flatten()
                    .collect();

                PreSymbolicExpression::tuple(data_map)
            }

            _ => return Err(anyhow::anyhow!("Unsupported expression for Tuple")),
        },
        ClarityTypeSignature::ResponseType(_boxed_types) => match &expr {
            _ => return Err(anyhow::anyhow!("Invalid expression for Response type")),
        },
        _ => {
            return Err(anyhow::anyhow!(format!(
                "Unsupported type for variable with {:?} type",
                r#type
            )))
        }
    })
}

fn convert_constant(
    allocator: &Allocator,
    ir: &IR,
    constant: &IRConstant,
) -> Result<PreSymbolicExpression, anyhow::Error> {
    Ok(PreSymbolicExpression::list(vec![
        PreSymbolicExpression::atom(ClarityName::from("define-constant")),
        PreSymbolicExpression::atom(ClarityName::from(constant.name.as_str())),
        convert_expression_with_type(allocator, ir, &constant.expr, &constant.r#type)?,
    ]))
}

fn convert_data_var(
    allocator: &Allocator,
    ir: &IR,
    data_var: &IRDataVar,
) -> Result<PreSymbolicExpression, anyhow::Error> {
    Ok(PreSymbolicExpression {
        id: 0,
        pre_expr: PreSymbolicExpressionType::List(vec![
            PreSymbolicExpression::atom(ClarityName::from("define-data-var")),
            PreSymbolicExpression::atom(ClarityName::from(data_var.name.as_str())),
            type_signature_to_pse(&data_var.r#type)?,
            convert_expression_with_type(allocator, ir, &data_var.expr, &data_var.r#type)?,
        ]),
        span: Span::zero(),
    })
}

fn convert_data_map(data_map: &IRDataMap) -> Result<PreSymbolicExpression, anyhow::Error> {
    Ok(PreSymbolicExpression::list(vec![
        PreSymbolicExpression::atom(ClarityName::from("define-map")),
        PreSymbolicExpression::atom(ClarityName::from(data_map.name.as_str())),
        type_signature_to_pse(&data_map.key_type)?,
        type_signature_to_pse(&data_map.value_type)?,
    ]))
}

fn convert_function(
    allocator: &Allocator,
    ir: &IR,
    function: &IRFunction,
) -> Result<PreSymbolicExpression, anyhow::Error> {
    let parameters: Vec<Vec<PreSymbolicExpression>> = function
        .parameters
        .iter()
        .map(|(name, r#type)| {
            let name = PreSymbolicExpression::atom(ClarityName::from(to_kebab_case(name).as_str()));
            let r#type = type_signature_to_pse(r#type)?;
            Ok(vec![name, r#type])
        })
        .collect::<Result<Vec<_>, anyhow::Error>>()?
        .into_iter()
        .collect();

    let define_type = if ir.read_only_functions.contains(&function.name) {
        "define-read-only"
    } else if ir.public_functions.contains(&function.name) {
        "define-public"
    } else {
        "define-private"
    };

    let function_name = to_kebab_case(function.name.as_str());
    let name_expr = PreSymbolicExpression::atom(ClarityName::from(function_name.as_str()));
    let name_and_parameters = if parameters.is_empty() {
        PreSymbolicExpression::list(vec![name_expr])
    } else {
        let params: Vec<_> = parameters
            .iter()
            .map(|p| PreSymbolicExpression::list(p.clone()))
            .collect();
        let mut name_and_params = vec![name_expr];
        name_and_params.extend(params);
        PreSymbolicExpression::list(name_and_params)
    };

    Ok(PreSymbolicExpression::list(vec![
        PreSymbolicExpression::atom(ClarityName::from(define_type)),
        name_and_parameters,
        expression_converter::convert_function_body(allocator, ir, function)?,
    ]))
}

pub fn convert(
    allocator: &Allocator,
    ir: &IR,
) -> Result<Vec<PreSymbolicExpression>, anyhow::Error> {
    let mut pses = vec![];

    pses.extend(
        ir.constants
            .iter()
            .map(|c| convert_constant(allocator, ir, c))
            .collect::<Result<Vec<_>, _>>()?,
    );

    pses.extend(
        ir.data_vars
            .iter()
            .map(|v| convert_data_var(allocator, ir, v))
            .collect::<Result<Vec<_>, _>>()?,
    );

    pses.extend(
        ir.data_maps
            .iter()
            .map(convert_data_map)
            .collect::<Result<Vec<_>, _>>()?,
    );

    pses.extend(
        ir.functions
            .iter()
            .map(|function| convert_function(allocator, &ir, function))
            .collect::<Result<Vec<_>, _>>()?,
    );

    Ok(pses)
}

#[cfg(test)]
mod test {
    use clarity::vm::representations::{PreSymbolicExpression, PreSymbolicExpressionType, Span};
    use clarity::vm::{ClarityName, Value as ClarityValue};
    use indoc::indoc;
    use oxc_allocator::Allocator;

    use super::*;
    use crate::parser::get_ir;

    fn get_tmp_ir<'a>(allocator: &'a Allocator, ts_source: &'a str) -> IR<'a> {
        get_ir(allocator, "tmp.clar.ts", ts_source)
    }

    fn set_pse_span_to_0(pse: &mut [PreSymbolicExpression]) {
        for expr in pse {
            expr.span = Span::zero();
            match &mut expr.pre_expr {
                PreSymbolicExpressionType::List(list) => set_pse_span_to_0(list),
                PreSymbolicExpressionType::Tuple(tuple) => set_pse_span_to_0(tuple),
                _ => {}
            }
        }
    }

    #[track_caller]
    fn assert_pses_eq(ts_source: &str, expected_clar_source: &str) {
        let mut expected_pse = clarity::vm::ast::parser::v2::parse(expected_clar_source).unwrap();
        set_pse_span_to_0(&mut expected_pse);

        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, ts_source);
        let actual_pse = convert(&allocator, &ir).expect("Failed to convert IR to PSE");

        pretty_assertions::assert_eq!(actual_pse, expected_pse);
    }

    fn ascii_value(value: &str) -> ClarityValue {
        ClarityValue::string_ascii_from_bytes(value.to_string().into_bytes()).unwrap()
    }

    // These first two tests build the expected PSEs manually making it easier
    // to debug the conversion process and show the intent.
    // The following tests rely on the assert_pses_eq function which dynamically
    // builds the expected PSEs from the Clarity source code.

    #[test]
    fn test_convert_constant() {
        let ts_src = "const OWNER_ROLE: Uint = 1;";
        let expected_pse = PreSymbolicExpression::list(vec![
            PreSymbolicExpression::atom(ClarityName::from("define-constant")),
            PreSymbolicExpression::atom(ClarityName::from("OWNER_ROLE")),
            PreSymbolicExpression::atom_value(ClarityValue::UInt(1)),
        ]);

        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, ts_src);
        let pses = convert(&allocator, &ir).unwrap();
        assert_eq!(pses, vec![expected_pse]);
    }

    #[test]
    fn test_convert_data_var() {
        let ts_src = "const count = new DataVar<Uint>(0);";
        let expected_pse = PreSymbolicExpression::list(vec![
            PreSymbolicExpression::atom(ClarityName::from("define-data-var")),
            PreSymbolicExpression::atom(ClarityName::from("count")),
            PreSymbolicExpression::atom(ClarityName::from("uint")),
            PreSymbolicExpression::atom_value(ClarityValue::UInt(0)),
        ]);

        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, ts_src);
        let pses = convert(&allocator, &ir).unwrap();
        assert_eq!(pses, vec![expected_pse]);

        let ts_src = r#"const msg = new DataVar<StringAscii<16>>("hello");"#;

        let expected_pse = PreSymbolicExpression::list(vec![
            PreSymbolicExpression::atom(ClarityName::from("define-data-var")),
            PreSymbolicExpression::atom(ClarityName::from("msg")),
            PreSymbolicExpression::list(vec![
                PreSymbolicExpression::atom(ClarityName::from("string-ascii")),
                PreSymbolicExpression::atom_value(ClarityValue::Int(16)),
            ]),
            PreSymbolicExpression::atom_value(ascii_value("hello")),
        ]);
        let ir = get_tmp_ir(&allocator, ts_src);
        let pses = convert(&allocator, &ir).unwrap();
        pretty_assertions::assert_eq!(pses, vec![expected_pse]);
    }

    // The following tests use the assert_pses_eq function to dynamically
    // build the expected PSEs from the Clarity source code.

    #[test]
    fn test_principal_data_var() {
        let ts_src =
            "const owner = new DataVar<Principal>(\"ST3PF13W7Z0RRM42A8VZRVFQ75SV1K26RXEP8YGKJ\");";
        assert_pses_eq(
            ts_src,
            r#"(define-data-var owner principal 'ST3PF13W7Z0RRM42A8VZRVFQ75SV1K26RXEP8YGKJ)"#,
        );

        let ts_src = "const owner = new DataVar<Principal>(txSender);";
        assert_pses_eq(ts_src, r#"(define-data-var owner principal tx-sender)"#);
    }

    #[test]
    fn test_constant_with_expression() {
        let ts_src = "const U2: Uint = 1 + 1;";
        assert_pses_eq(ts_src, r#"(define-constant U2 (+ u1 u1))"#);
    }

    #[test]
    fn test_constant_with_err_expression() {
        let ts_src = "const ERR_INTERNAL: ClError<never, Int> = err(5000);";
        assert_pses_eq(ts_src, r#"(define-constant ERR_INTERNAL (err 5000))"#);
        let ts_src = "const ERR_INTERNAL: ClError<never, Uint> = err(5001);";
        assert_pses_eq(ts_src, r#"(define-constant ERR_INTERNAL (err u5001))"#);
    }

    #[test]
    fn test_convert_tuple_type() {
        let ts_src = "const state = new DataVar<{ ok: Int }>({ ok: 1 });";
        assert_pses_eq(ts_src, r#"(define-data-var state { ok: int } { ok: 1 })"#);
        let ts_src = "const state = new DataVar<{ ok: Bool }>({ ok: true });";
        assert_pses_eq(
            ts_src,
            r#"(define-data-var state { ok: bool } { ok: true })"#,
        );
    }

    #[test]
    fn test_convert_tuple_type_preserves_order() {
        // let ts_src = "const state = new DataVar<{ zz: Bool, aa: Int }>({ zz: true, aa: 2 });";

        // TODO: explore tuple type sig conversation.
        // Because TupleTypeSignature.type_map is a BTreeMap, the order of the properties
        // is sorted. We would prefer it to preserve the order of the properties.

        // assert_pses_eq(
        //     ts_src,
        //     r#"(define-data-var state { zz: bool, aa: int } { zz: true, aa: 2 })"#,
        // );

        let ts_src = "const state = new DataVar<{ aa: Int, zz: Int }>({ aa: 2, zz: 1 });";
        assert_pses_eq(
            ts_src,
            r#"(define-data-var state { aa: int, zz: int } { aa: 2, zz: 1 })"#,
        );
    }

    #[test]
    fn test_convert_data_map() {
        let ts_src = "const msgs = new DataMap<Uint, StringAscii<16>>();";
        assert_pses_eq(ts_src, r#"(define-map msgs uint (string-ascii 16))"#);
    }

    #[test]
    fn test_convert_data_map_with_tuple_type() {
        let ts_src = "const state = new DataMap<{ ok: Uint }, { active: Bool }>();";
        assert_pses_eq(
            ts_src,
            r#"(define-map state { ok: uint } { active: bool })"#,
        );
    }

    #[test]
    fn test_function_with_no_parameters() {
        let ts_src = "function printtrue() { return print(true); }";
        assert_pses_eq(ts_src, r#"(define-private (printtrue) (print true))"#);
    }

    #[test]
    fn test_function_with_one_parameter() {
        let ts_src = "function printarg(arg: Uint) { return print(arg); }";
        assert_pses_eq(
            ts_src,
            r#"(define-private (printarg (arg uint)) (print arg))"#,
        );
        let ts_src = "function printarg(arg: StringAscii<16>) { return print(arg); }";
        assert_pses_eq(
            ts_src,
            r#"(define-private (printarg (arg (string-ascii 16))) (print arg))"#,
        );
    }

    #[test]
    fn test_function_with_parameters() {
        let ts_src = "function add(a: Uint, b: Uint) { return a + b; }";
        assert_pses_eq(ts_src, "(define-private (add (a uint) (b uint)) (+ a b))");
    }

    #[test]
    fn test_read_only_functions() {
        let ts_src = indoc! {
            r#"function myfunc() { return true; }
            export default { readOnly: { myfunc } } satisfies Contract
            "#
        };
        assert_pses_eq(ts_src, r#"(define-read-only (myfunc) true)"#);
    }

    #[test]
    fn test_public_functions() {
        let ts_src = indoc! {
            r#"function myfunc() { return ok(true); }
            export default { public: { myfunc } } satisfies Contract
            "#
        };
        assert_pses_eq(ts_src, r#"(define-public (myfunc) (ok true))"#);
    }

    #[test]
    fn test_function_arg_casing() {
        let ts_src = indoc! {
            r#"const addr = new DataVar<Principal>(txSender);
            function updateAddr(newAddr: Principal) { return ok(addr.set(newAddr)); }"#
        };
        assert_pses_eq(
            ts_src,
            indoc! {
                r#"(define-data-var addr principal tx-sender)
                (define-private (update-addr (new-addr principal)) (ok (var-set addr new-addr)))"#
            },
        );
    }
}
