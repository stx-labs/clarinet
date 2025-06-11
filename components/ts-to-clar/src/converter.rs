// converter.rs converts the TS intermediate representation (IR) to Clarity PreSymbolicExpressions (PSEs)

use clarity::vm::{
    representations::{
        PreSymbolicExpression,
        PreSymbolicExpressionType::{self, Atom, AtomValue, List},
        Span,
    },
    types::{SequenceSubtype, StringSubtype, TypeSignature as ClarityTypeSignature},
    ClarityName, Value as ClarityValue,
};

use crate::parser::{IRConstant, IRDataMap, IRDataVar, IR};

fn build_default_pse(pre_expr: PreSymbolicExpressionType) -> PreSymbolicExpression {
    PreSymbolicExpression {
        id: 0,
        pre_expr,
        span: Span::zero(),
    }
}

fn type_signature_to_pse(
    type_signature: &ClarityTypeSignature,
) -> Result<PreSymbolicExpression, anyhow::Error> {
    let pse_type = match type_signature {
        ClarityTypeSignature::UIntType => Atom(ClarityName::from("uint")),
        ClarityTypeSignature::IntType => Atom(ClarityName::from("int")),
        ClarityTypeSignature::SequenceType(seq_subtype) => match seq_subtype {
            SequenceSubtype::StringType(string_subtype) => match string_subtype {
                StringSubtype::ASCII(len) => List(vec![
                    build_default_pse(Atom(ClarityName::from("string-ascii"))),
                    build_default_pse(AtomValue(ClarityValue::Int(u32::from(len).into()))),
                ]),
                StringSubtype::UTF8(len) => List(vec![
                    build_default_pse(Atom(ClarityName::from("string-utf8"))),
                    build_default_pse(AtomValue(ClarityValue::Int(u32::from(len).into()))),
                ]),
            },
            _ => return Err(anyhow::anyhow!("Unsupported sequence type")),
        },
        _ => return Err(anyhow::anyhow!("Unsupported type signature")),
    };

    Ok(build_default_pse(pse_type))
}

fn convert_constant(constant: &IRConstant) -> Result<PreSymbolicExpression, anyhow::Error> {
    let value = match &constant.expr {
        oxc_ast::ast::Expression::NumericLiteral(num) => match constant.r#type {
            ClarityTypeSignature::UIntType => ClarityValue::UInt(num.value as u128),
            ClarityTypeSignature::IntType => ClarityValue::Int(num.value as i128),
            _ => return Err(anyhow::anyhow!("Unsupported numeric type for constant")),
        },
        _ => return Err(anyhow::anyhow!("Unsupported expression type for constant")),
    };

    Ok(PreSymbolicExpression {
        id: 0,
        pre_expr: List(vec![
            build_default_pse(Atom(ClarityName::from("define-const"))),
            build_default_pse(Atom(ClarityName::from(constant.name.as_str()))),
            build_default_pse(AtomValue(value)),
        ]),
        span: Span::zero(),
    })
}

fn convert_data_var(data_var: &IRDataVar) -> Result<PreSymbolicExpression, anyhow::Error> {
    let value = match &data_var.expr {
        oxc_ast::ast::Expression::NumericLiteral(num) => match data_var.r#type {
            ClarityTypeSignature::UIntType => ClarityValue::UInt(num.value as u128),
            ClarityTypeSignature::IntType => ClarityValue::Int(num.value as i128),
            _ => return Err(anyhow::anyhow!("Unsupported numeric type for data var")),
        },
        oxc_ast::ast::Expression::StringLiteral(str) => {
            ClarityValue::string_ascii_from_bytes(str.value.to_string().into_bytes()).unwrap()
        }
        _ => return Err(anyhow::anyhow!("Unsupported expression type for data var")),
    };

    Ok(PreSymbolicExpression {
        id: 0,
        pre_expr: List(vec![
            build_default_pse(Atom(ClarityName::from("define-data-var"))),
            build_default_pse(Atom(ClarityName::from(data_var.name.as_str()))),
            type_signature_to_pse(&data_var.r#type)?,
            build_default_pse(AtomValue(value)),
        ]),
        span: Span::zero(),
    })
}

fn convert_data_map(data_map: &IRDataMap) -> Result<PreSymbolicExpression, anyhow::Error> {
    Ok(PreSymbolicExpression {
        id: 0,
        pre_expr: List(vec![
            build_default_pse(Atom(ClarityName::from("define-data-map"))),
            build_default_pse(Atom(ClarityName::from(data_map.name.as_str()))),
            type_signature_to_pse(&data_map.key_type)?,
            type_signature_to_pse(&data_map.value_type)?,
        ]),
        span: Span::zero(),
    })
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
        representations::{PreSymbolicExpression, Span},
        ClarityName, Value as ClarityValue,
    };
    use oxc_allocator::Allocator;

    use crate::{converter::build_default_pse, parser::get_ir};

    use super::*;

    fn get_tmp_ir<'a>(allocator: &'a Allocator, ts_source: &'a str) -> IR<'a> {
        get_ir(allocator, "tmp.clar.ts", ts_source)
    }

    fn set_pse_span_to_0(pse: &mut [PreSymbolicExpression]) {
        for expr in pse {
            expr.span = Span::zero();
            if let PreSymbolicExpressionType::List(list) = &mut expr.pre_expr {
                set_pse_span_to_0(list);
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
        let expected_pse = build_default_pse(List(vec![
            build_default_pse(Atom(ClarityName::from("define-const"))),
            build_default_pse(Atom(ClarityName::from("OWNER_ROLE"))),
            build_default_pse(AtomValue(ClarityValue::UInt(1))),
        ]));

        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, ts_src);
        let pses = convert(ir).unwrap();
        assert_eq!(pses, vec![expected_pse]);
    }

    #[test]
    fn test_convert_data_var() {
        let ts_src = "const count = new DataVar<Uint>(0);";
        let expected_pse = build_default_pse(List(vec![
            build_default_pse(Atom(ClarityName::from("define-data-var"))),
            build_default_pse(Atom(ClarityName::from("count"))),
            build_default_pse(Atom(ClarityName::from("uint"))),
            build_default_pse(AtomValue(ClarityValue::UInt(0))),
        ]));

        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, ts_src);
        let pses = convert(ir).unwrap();
        assert_eq!(pses, vec![expected_pse]);

        let ts_src = r#"const msg = new DataVar<StringAscii<16>>("hello");"#;
        assert_pses_eq(ts_src, r#"(define-data-var msg (string-ascii 16) "hello")"#);

        let expected_pse = build_default_pse(List(vec![
            build_default_pse(Atom(ClarityName::from("define-data-var"))),
            build_default_pse(Atom(ClarityName::from("msg"))),
            build_default_pse(List(vec![
                build_default_pse(Atom(ClarityName::from("string-ascii"))),
                build_default_pse(AtomValue(ClarityValue::Int(16))),
            ])),
            build_default_pse(AtomValue(ascii_value("hello"))),
        ]));
        let ir = get_tmp_ir(&allocator, ts_src);
        let pses = convert(ir).unwrap();
        pretty_assertions::assert_eq!(pses, vec![expected_pse]);
    }

    // The following tests use the assert_pses_eq function to dynamically
    // build the expected PSEs from the Clarity source code.

    #[test]
    fn test_convert_data_map() {
        let ts_src = "const msgs = new DataMap<Uint, StringAscii<16>>();";
        assert_pses_eq(ts_src, r#"(define-data-map msgs uint (string-ascii 16))"#);
    }
}
