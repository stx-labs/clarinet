// converter.rs converts the TS intermediate representation (IR) to Clarity PreSymbolicExpressions (PSEs)

use clarity::vm::{
    representations::{PreSymbolicExpression, PreSymbolicExpressionType, Span},
    types::{SequenceSubtype, StringSubtype, TypeSignature as ClarityTypeSignature},
    ClarityName, Value as ClarityValue,
};
use oxc_ast::ast::{Expression as OxcExpression, ObjectPropertyKind};

use crate::parser::{IRConstant, IRDataMap, IRDataVar, IR};

fn type_signature_to_pse(
    type_signature: &ClarityTypeSignature,
) -> Result<PreSymbolicExpression, anyhow::Error> {
    Ok(match type_signature {
        ClarityTypeSignature::UIntType => PreSymbolicExpression::atom(ClarityName::from("uint")),
        ClarityTypeSignature::IntType => PreSymbolicExpression::atom(ClarityName::from("int")),
        ClarityTypeSignature::BoolType => PreSymbolicExpression::atom(ClarityName::from("bool")),
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
            let data_map: Vec<PreSymbolicExpression> = tuple_type
                .get_type_map()
                .iter()
                .flat_map(|(key, signature)| {
                    let name_expr =
                        PreSymbolicExpression::atom(ClarityName::from(key.to_string().as_str()));
                    let type_expr = type_signature_to_pse(signature).unwrap();
                    vec![name_expr, type_expr]
                })
                .collect();
            PreSymbolicExpression::tuple(data_map)
        }
        _ => return Err(anyhow::anyhow!("Unsupported type signature")),
    })
}

fn convert_expression_with_type(
    expr: &OxcExpression,
    r#type: &ClarityTypeSignature,
) -> Result<PreSymbolicExpression, anyhow::Error> {
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
                let data_map: Vec<Vec<PreSymbolicExpression>> = obj
                    .properties
                    .iter()
                    .map(|property| match property {
                        ObjectPropertyKind::ObjectProperty(property) => {
                            let key = property.key.static_name().unwrap();
                            let key_name = ClarityName::from(key.to_string().as_str());
                            let prop_type = tuple_type.get_type_map().get(&key_name).unwrap();
                            let value =
                                convert_expression_with_type(&property.value, &prop_type.clone())?;
                            Ok(vec![PreSymbolicExpression::atom(key_name), value])
                        }
                        ObjectPropertyKind::SpreadProperty(_property) => {
                            Err(anyhow::anyhow!("Todo: spread property for Tuple"))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                // Flatten in a second step for simpler error handling in the map above
                // Todo: explore using .flat_map() directly above
                let flattened_data_map: Vec<PreSymbolicExpression> =
                    data_map.iter().flatten().cloned().collect();

                PreSymbolicExpression::tuple(flattened_data_map)
            }

            _ => return Err(anyhow::anyhow!("Unsupported expression for Tuple")),
        },
        _ => return Err(anyhow::anyhow!("Unsupported type for constant")),
    })
}

fn convert_constant(constant: &IRConstant) -> Result<PreSymbolicExpression, anyhow::Error> {
    let value = convert_expression_with_type(&constant.expr, &constant.r#type)?;

    Ok(PreSymbolicExpression::list(vec![
        PreSymbolicExpression::atom(ClarityName::from("define-const")),
        PreSymbolicExpression::atom(ClarityName::from(constant.name.as_str())),
        value,
    ]))
}

fn convert_data_var(data_var: &IRDataVar) -> Result<PreSymbolicExpression, anyhow::Error> {
    let value = convert_expression_with_type(&data_var.expr, &data_var.r#type)?;

    Ok(PreSymbolicExpression {
        id: 0,
        pre_expr: PreSymbolicExpressionType::List(vec![
            PreSymbolicExpression::atom(ClarityName::from("define-data-var")),
            PreSymbolicExpression::atom(ClarityName::from(data_var.name.as_str())),
            type_signature_to_pse(&data_var.r#type)?,
            value,
        ]),
        span: Span::zero(),
    })
}

fn convert_data_map(data_map: &IRDataMap) -> Result<PreSymbolicExpression, anyhow::Error> {
    Ok(PreSymbolicExpression::list(vec![
        PreSymbolicExpression::atom(ClarityName::from("define-data-map")),
        PreSymbolicExpression::atom(ClarityName::from(data_map.name.as_str())),
        type_signature_to_pse(&data_map.key_type)?,
        type_signature_to_pse(&data_map.value_type)?,
    ]))
}

pub fn convert(ir: IR) -> Result<Vec<PreSymbolicExpression>, anyhow::Error> {
    let mut pses = vec![];

    let constants = ir
        .constants
        .iter()
        .map(convert_constant)
        .collect::<Result<Vec<_>, _>>()?;
    pses.extend(constants);

    let data_vars = ir
        .data_vars
        .iter()
        .map(convert_data_var)
        .collect::<Result<Vec<_>, _>>()?;
    pses.extend(data_vars);

    let data_maps = ir
        .data_maps
        .iter()
        .map(convert_data_map)
        .collect::<Result<Vec<_>, _>>()?;
    pses.extend(data_maps);

    Ok(pses)
}

#[cfg(test)]
mod test {
    use clarity::vm::{
        representations::{PreSymbolicExpression, PreSymbolicExpressionType, Span},
        ClarityName, Value as ClarityValue,
    };
    use oxc_allocator::Allocator;

    use crate::parser::get_ir;

    use super::*;

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

    fn assert_pses_eq(ts_source: &str, expected_clar_source: &str) {
        let mut expected_pse = clarity::vm::ast::parser::v2::parse(expected_clar_source).unwrap();
        set_pse_span_to_0(&mut expected_pse);

        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, ts_source);
        let actual_pse = convert(ir).expect("Failed to convert IR to PSE");

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
        let ts_src = "const OWNER_ROLE = new Constant<Uint>(1);";
        let expected_pse = PreSymbolicExpression::list(vec![
            PreSymbolicExpression::atom(ClarityName::from("define-const")),
            PreSymbolicExpression::atom(ClarityName::from("OWNER_ROLE")),
            PreSymbolicExpression::atom_value(ClarityValue::UInt(1)),
        ]);

        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, ts_src);
        let pses = convert(ir).unwrap();
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
        let pses = convert(ir).unwrap();
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
        let pses = convert(ir).unwrap();
        pretty_assertions::assert_eq!(pses, vec![expected_pse]);
    }

    // The following tests use the assert_pses_eq function to dynamically
    // build the expected PSEs from the Clarity source code.

    #[test]
    fn test_convert_tuple_type() {
        let ts_src = "const state = new DataVar<{ ok: Int }>({ ok: 1 });";
        assert_pses_eq(ts_src, r#"(define-data-var state { ok: int } { ok: 1 })"#);
        let ts_src = "const state = new DataVar<{ ok: Bool }>({ ok: true });";
        assert_pses_eq(
            ts_src,
            r#"(define-data-var state { ok: bool } { ok: true })"#,
        );
        // let ts_src =
        //     "const state = new DataVar<{ ok: Int, active: Bool }>({ ok: 1, active: true });";
        // assert_pses_eq(
        //     ts_src,
        //     r#"(define-data-var state { ok: int, active: bool } { ok: 1, active: true })"#,
        // );
    }

    #[test]
    fn test_convert_data_map() {
        let ts_src = "const msgs = new DataMap<Uint, StringAscii<16>>();";
        assert_pses_eq(ts_src, r#"(define-data-map msgs uint (string-ascii 16))"#);
    }

    #[test]
    fn test_convert_data_map_with_tuple_type() {
        let ts_src = "const state = new DataMap<{ ok: Uint }, { active: Bool }>();";
        assert_pses_eq(
            ts_src,
            r#"(define-data-map state { ok: uint } { active: bool })"#,
        );
    }
}
