// converter.rs converts the TS intermediate representation (IR) to Clarity PreSymbolicExpressions (PSEs)

use clarity::vm::{
    representations::{
        PreSymbolicExpression,
        PreSymbolicExpressionType::{self, *},
        Span,
    },
    types::TypeSignature as ClarityTypeSignature,
    ClarityName, Value as ClarityValue,
};

use crate::parser::{IRConstant, IRDataVar, IR};

fn build_default_pse(pre_expr: PreSymbolicExpressionType) -> PreSymbolicExpression {
    PreSymbolicExpression {
        pre_expr,
        id: 0,
        span: Span::zero(),
    }
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
        pre_expr: List(vec![
            build_default_pse(Atom(ClarityName::from("define-const"))),
            build_default_pse(Atom(ClarityName::from(constant.name.as_str()))),
            build_default_pse(AtomValue(value)),
        ]),
        id: 0,
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
        _ => return Err(anyhow::anyhow!("Unsupported expression type for data var")),
    };

    Ok(PreSymbolicExpression {
        pre_expr: List(vec![
            build_default_pse(Atom(ClarityName::from("define-data-var"))),
            build_default_pse(Atom(ClarityName::from(data_var.name.as_str()))),
            build_default_pse(Atom(ClarityName::from(
                data_var.r#type.to_string().as_str(),
            ))),
            build_default_pse(AtomValue(value)),
        ]),
        id: 0,
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

    Ok(pses)
}

#[cfg(test)]
mod test {
    use clarity::vm::{
        representations::{
            PreSymbolicExpression,
            PreSymbolicExpressionType::{Atom, AtomValue},
            Span,
        },
        ClarityName, Value as ClarityValue,
    };
    use oxc_allocator::Allocator;

    use crate::parser::get_ir;

    use super::*;

    fn get_tmp_ir<'a>(allocator: &'a Allocator, ts_source: &'a str) -> IR<'a> {
        get_ir(allocator, "tmp.clar.ts", ts_source)
    }

    // fn assert_pses_eq(ts_source: &str, expected_clar_source: &str) {
    //     let expected_pse = clarity::vm::ast::parser::v2::parse(expected_clar_source).unwrap();
    //     println!("expected_pse: {:#?}", expected_pse);
    //     let allocator = Allocator::default();
    //     let ir = get_tmp_ir(&allocator, ts_source);
    //     let pses = convert(ir).unwrap();
    //     assert_eq!(pses, expected_pse);
    // }

    #[test]
    fn test_convert_constant() {
        let ts_src = "const OWNER_ROLE = new Constant<Uint>(1);";
        let expected_pse = PreSymbolicExpression {
            id: 0,
            span: Span::zero(),
            pre_expr: PreSymbolicExpressionType::List(vec![
                build_default_pse(Atom(ClarityName::from("define-const"))),
                build_default_pse(Atom(ClarityName::from("OWNER_ROLE"))),
                build_default_pse(AtomValue(ClarityValue::UInt(1))),
            ]),
        };

        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, ts_src);
        let pses = convert(ir).unwrap();
        assert_eq!(pses, vec![expected_pse]);
    }

    #[test]
    fn test_convert_data_var() {
        let ts_src = "const count = new DataVar<Uint>(0);";
        // assert_pses_eq(ts_src, "(define-data-var count uint u0)");
        // (define-data-var count uint u1)
        let expected_pse = PreSymbolicExpression {
            id: 0,
            span: Span::zero(),
            pre_expr: PreSymbolicExpressionType::List(vec![
                build_default_pse(Atom(ClarityName::from("define-data-var"))),
                build_default_pse(Atom(ClarityName::from("count"))),
                build_default_pse(Atom(ClarityName::from("uint"))),
                build_default_pse(AtomValue(ClarityValue::UInt(0))),
            ]),
        };

        let allocator = Allocator::default();
        let ir = get_tmp_ir(&allocator, ts_src);
        let pses = convert(ir).unwrap();
        assert_eq!(pses, vec![expected_pse]);
    }
}
