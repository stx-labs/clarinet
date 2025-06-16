use std::cell::Cell;

use clarity::vm::{
    representations::PreSymbolicExpression, types::TypeSignature, ClarityName,
    Value as ClarityValue,
};
use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::{Expression, Program, Statement};
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;
use oxc_traverse::{traverse_mut_with_ctx, ReusableTraverseCtx, Traverse, TraverseCtx};

use crate::parser::IRFunction;

struct StatementConverter<'a> {
    function: &'a IRFunction<'a>,
    expressions: Vec<PreSymbolicExpression>,
}

impl<'a> StatementConverter<'a> {
    fn new(function: &'a IRFunction<'a>) -> Self {
        Self {
            function,
            expressions: Vec::new(),
        }
    }

    fn get_parameter_type(&self, param_name: &str) -> Option<&TypeSignature> {
        self.function
            .parameters
            .iter()
            .find_map(|(name, type_sig)| {
                if name == param_name {
                    Some(type_sig)
                } else {
                    None
                }
            })
    }

    fn convert_numeric_literal(
        &self,
        value: i128,
        context_type: Option<&TypeSignature>,
    ) -> PreSymbolicExpression {
        match context_type {
            Some(TypeSignature::UIntType) => {
                PreSymbolicExpression::atom_value(ClarityValue::UInt(value as u128))
            }
            _ => PreSymbolicExpression::atom_value(ClarityValue::Int(value)),
        }
    }

    fn find_chain_context_type<'b>(
        &'b self,
        expr: &'b Expression<'a>,
        operator: &str,
    ) -> Option<&'a TypeSignature>
    where
        'b: 'a,
    {
        match expr {
            Expression::BinaryExpression(bin) => {
                let bin_op = match bin.operator {
                    oxc_ast::ast::BinaryOperator::Addition => "+",
                    oxc_ast::ast::BinaryOperator::Subtraction => "-",
                    oxc_ast::ast::BinaryOperator::Multiplication => "*",
                    oxc_ast::ast::BinaryOperator::Division => "/",
                    _ => return None,
                };
                if bin_op == operator {
                    let left = self.find_chain_context_type(&bin.left, operator);
                    if left.is_some() {
                        left
                    } else {
                        self.find_chain_context_type(&bin.right, operator)
                    }
                } else {
                    None
                }
            }
            Expression::Identifier(ident) => self.get_parameter_type(ident.name.as_str()),
            _ => None,
        }
    }

    fn collect_operands(
        &self,
        expr: &Expression<'a>,
        operator: &str,
        operands: &mut Vec<PreSymbolicExpression>,
        context_type: Option<&'a TypeSignature>,
    ) {
        match expr {
            Expression::BinaryExpression(bin) => {
                let bin_op = match bin.operator {
                    oxc_ast::ast::BinaryOperator::Addition => "+",
                    oxc_ast::ast::BinaryOperator::Subtraction => "-",
                    oxc_ast::ast::BinaryOperator::Multiplication => "*",
                    oxc_ast::ast::BinaryOperator::Division => "/",
                    _ => return,
                };
                if bin_op == operator {
                    self.collect_operands(&bin.left, operator, operands, context_type);
                    self.collect_operands(&bin.right, operator, operands, context_type);
                } else {
                    operands.push(self.convert_expression(expr, context_type));
                }
            }
            _ => operands.push(self.convert_expression(expr, context_type)),
        }
    }

    fn convert_expression(
        &self,
        expr: &Expression<'a>,
        context_type: Option<&TypeSignature>,
    ) -> PreSymbolicExpression {
        match expr {
            Expression::Identifier(ident) => {
                PreSymbolicExpression::atom(ClarityName::from(ident.name.as_str()))
            }
            Expression::NumericLiteral(num_lit) => {
                self.convert_numeric_literal(num_lit.value as i128, context_type)
            }
            Expression::CallExpression(call_expr) => {
                // TODO: callee should be an authorized function name
                // `clarity::functions::NativeFunctions::ALL_NAMES` can be used
                let callee = call_expr.callee.get_identifier_reference().unwrap();
                let callee_atom =
                    PreSymbolicExpression::atom(ClarityName::from(callee.name.as_str()));
                let arg = call_expr.arguments.first().unwrap().to_expression();

                match &arg {
                    Expression::BooleanLiteral(bool_lit) => PreSymbolicExpression::list(vec![
                        callee_atom,
                        PreSymbolicExpression::atom(ClarityName::from(
                            bool_lit.value.to_string().as_str(),
                        )),
                    ]),
                    Expression::Identifier(ident) => PreSymbolicExpression::list(vec![
                        callee_atom,
                        PreSymbolicExpression::atom(ClarityName::from(ident.name.as_str())),
                    ]),
                    _ => panic!("Only boolean literals and identifiers are supported for now"),
                }
            }
            Expression::BinaryExpression(bin_expr) => {
                use oxc_ast::ast::BinaryOperator;
                let operator = match bin_expr.operator {
                    BinaryOperator::Addition => "+",
                    BinaryOperator::Subtraction => "-",
                    BinaryOperator::Multiplication => "*",
                    BinaryOperator::Division => "/",
                    BinaryOperator::Remainder => "mod",
                    BinaryOperator::LessThan => "<",
                    BinaryOperator::GreaterThan => ">",
                    BinaryOperator::LessEqualThan => "<=",
                    BinaryOperator::GreaterEqualThan => ">=",
                    BinaryOperator::Equality => "is-eq",
                    BinaryOperator::StrictEquality => "is-eq",
                    BinaryOperator::BitwiseAnd => "bit-and",
                    BinaryOperator::BitwiseOR => "bit-or",
                    BinaryOperator::BitwiseXOR => "bit-xor",
                    BinaryOperator::ShiftLeft => "bit-shift-left",
                    BinaryOperator::ShiftRight => "bit-shift-right",
                    BinaryOperator::Inequality => todo!(),
                    BinaryOperator::StrictInequality => todo!(),
                    BinaryOperator::Exponential => todo!(),
                    BinaryOperator::ShiftRightZeroFill => todo!(),
                    BinaryOperator::In => todo!(),
                    BinaryOperator::Instanceof => todo!(),
                };

                // Handle variadic operators (+, -, *, /)
                let is_variadic = matches!(
                    bin_expr.operator,
                    BinaryOperator::Addition
                        | BinaryOperator::Subtraction
                        | BinaryOperator::Multiplication
                        | BinaryOperator::Division
                );

                if is_variadic {
                    let mut operands = Vec::new();
                    let chain_context_type = self.find_chain_context_type(expr, operator);
                    self.collect_operands(
                        &bin_expr.left,
                        operator,
                        &mut operands,
                        chain_context_type,
                    );
                    self.collect_operands(
                        &bin_expr.right,
                        operator,
                        &mut operands,
                        chain_context_type,
                    );
                    let mut list = vec![PreSymbolicExpression::atom(ClarityName::from(operator))];
                    list.extend(operands);
                    PreSymbolicExpression::list(list)
                } else {
                    PreSymbolicExpression::list(vec![
                        PreSymbolicExpression::atom(ClarityName::from(operator)),
                        self.convert_expression(&bin_expr.left, context_type),
                        self.convert_expression(&bin_expr.right, context_type),
                    ])
                }
            }
            _ => panic!("Only function calls and binary expressions are supported for now"),
        }
    }
}

impl<'a> Traverse<'a> for StatementConverter<'a> {
    fn enter_statement(&mut self, node: &mut Statement<'a>, _ctx: &mut TraverseCtx<'a>) {
        match node {
            Statement::ExpressionStatement(expr_stmt) => {
                self.expressions
                    .push(self.convert_expression(&expr_stmt.expression, None));
            }
            Statement::ReturnStatement(ret_stmt) => {
                if let Some(expr) = &ret_stmt.argument {
                    self.expressions.push(self.convert_expression(expr, None));
                }
            }
            _ => {}
        }
    }

    fn enter_expression(&mut self, node: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {}
}

pub fn convert<'a>(
    allocator: &'a Allocator,
    function: &IRFunction<'a>,
) -> Result<PreSymbolicExpression, anyhow::Error> {
    let mut program = Program {
        span: oxc_span::Span::default(),
        source_type: SourceType::ts(),
        source_text: "",
        comments: oxc_allocator::Vec::new_in(allocator),
        hashbang: None,
        directives: oxc_allocator::Vec::new_in(allocator),
        body: function.body.clone_in(allocator),
        scope_id: Cell::new(None),
    };

    let scoping = SemanticBuilder::new()
        .build(&program)
        .semantic
        .into_scoping();

    let mut converter = StatementConverter::new(function);
    traverse_mut_with_ctx(
        &mut converter,
        &mut program,
        &mut ReusableTraverseCtx::new(scoping, allocator),
    );

    if converter.expressions.is_empty() {
        return Err(anyhow::anyhow!("No expressions found"));
    }

    if converter.expressions.len() == 1 {
        Ok(converter.expressions[0].clone())
    } else {
        let mut begin_exprs = vec![PreSymbolicExpression::atom(ClarityName::from("begin"))];
        begin_exprs.extend(converter.expressions);
        Ok(PreSymbolicExpression::list(begin_exprs))
    }
}

#[cfg(test)]
mod test {
    use clarity::vm::representations::{PreSymbolicExpressionType, Span};
    use oxc_allocator::Allocator;

    use crate::parser::get_ir;

    use super::*;

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

    fn get_expected_pse(expected_clar_source: &str) -> PreSymbolicExpression {
        let mut expected_pse = clarity::vm::ast::parser::v2::parse(expected_clar_source).unwrap();
        set_pse_span_to_0(&mut expected_pse);
        expected_pse.into_iter().next().unwrap()
    }

    fn assert_pses_eq(ts_src: &str, expected_clar_source: &str) {
        let allocator = Allocator::default();
        let expr = get_ir(&allocator, "tmp.clar.ts", ts_src);
        let result = convert(&allocator, &expr.functions[0]).unwrap();
        let expected_pse = get_expected_pse(expected_clar_source);
        pretty_assertions::assert_eq!(result, expected_pse);
    }

    #[test]
    fn test_expression_call() {
        let ts_src = "function printtrue() { return print(true); }";
        assert_pses_eq(ts_src, "(print true)");
    }

    #[test]
    fn test_expression_multiple_statements() {
        let ts_src = "function printtrue() { print(true); return print(true); }";
        assert_pses_eq(ts_src, "(begin (print true) (print true))");
    }

    #[test]
    fn test_expression_return_uint() {
        let ts_src = "function printarg(arg: Uint) { return print(arg); }";
        assert_pses_eq(ts_src, "(print arg)");
    }

    #[test]
    fn test_expression_return_ok() {
        let ts_src = "function okarg(arg: Uint) { return ok(arg); }";
        assert_pses_eq(ts_src, "(ok arg)");
    }

    #[test]
    fn test_operator() {
        let ts_src = "function add(a: Uint, b: Uint) { return a + b; }";
        assert_pses_eq(ts_src, "(+ a b)");

        let ts_src = "function sub(a: Uint, b: Uint) { return a - b; }";
        assert_pses_eq(ts_src, "(- a b)");

        let ts_src = "function add1and1() { return 1 + 1; }";
        assert_pses_eq(ts_src, "(+ 1 1)");
    }

    #[test]
    fn test_type_casting() {
        let ts_src = "function add1(a: Int) { return a + 1; }";
        assert_pses_eq(ts_src, "(+ a 1)");

        let ts_src = "function add1(a: Uint) { return a + 1; }";
        assert_pses_eq(ts_src, "(+ a u1)");

        let ts_src = "function add1(a: Int) { return 1 + a; }";
        assert_pses_eq(ts_src, "(+ 1 a)");

        let ts_src = "function add1(a: Uint) { return 1 + a; }";
        assert_pses_eq(ts_src, "(+ u1 a)");
    }

    #[test]
    fn test_operator_chaining() {
        let ts_src = "function add3(a: Uint) { return a + 1 + 2; }";
        assert_pses_eq(ts_src, "(+ a u1 u2)");

        let ts_src = "function add3(a: Uint) { return 1 + a + 2; }";
        assert_pses_eq(ts_src, "(+ u1 a u2)");

        let ts_src = "function add3(a: Uint) { return 1 + 2 + a; }";
        assert_pses_eq(ts_src, "(+ u1 u2 a)");

        let ts_src = "function mul2(a: Int) { return a * 1 * 2; }";
        assert_pses_eq(ts_src, "(* a 1 2)");

        let ts_src = "function mul2(a: Int) { return 1 * a * 2; }";
        assert_pses_eq(ts_src, "(* 1 a 2)");

        let ts_src = "function mul2(a: Int) { return 1 * 2 * a; }";
        assert_pses_eq(ts_src, "(* 1 2 a)");
    }

    // #[test]
    // fn test_ok_operator() {
    //     let ts_src = "function okarg(arg: Uint) { return ok(arg + 1); }";
    //     assert_pses_eq(ts_src, "(ok (+ arg u1))");
    // }
}
