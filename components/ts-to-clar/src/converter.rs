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
        swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Num(num)) => match constant.typ {
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
        swc_ecma_ast::Expr::Lit(swc_ecma_ast::Lit::Num(num)) => match data_var.typ {
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
            build_default_pse(Atom(ClarityName::from(data_var.typ.to_string().as_str()))),
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

    use crate::parser::get_ir;

    use super::*;

    fn get_tmp_ir(ts_source: &str) -> IR {
        get_ir("tmp.clar.ts", ts_source.to_string())
    }

    // fn assert_pses_eq(ts_source: &str, expected_clar_source: &str) {
    //     let expected_pse = clarity::vm::ast::parser::v2::parse(expected_clar_source).unwrap();
    //     println!("expected_pse: {:#?}", expected_pse);
    //     let ir = get_tmp_ir(ts_source);
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

        let ir = get_tmp_ir(ts_src);
        let pses = convert(ir).unwrap();
        assert_eq!(pses, vec![expected_pse]);
    }

    #[test]
    fn test_convert_data_var() {
        let ts_src = "const count = new DataVar<Uint>(0);";
        // let epse = assert_pses_eq(ts_src, "(define-data-var count uint u1)");
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

        let ir = get_tmp_ir(ts_src);
        let pses = convert(ir).unwrap();
        assert_eq!(pses, vec![expected_pse]);
    }
}
