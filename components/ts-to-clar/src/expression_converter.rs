use std::cell::Cell;

use clarity::vm::{
    representations::{PreSymbolicExpression, PreSymbolicExpressionType},
    types::TypeSignature,
    ClarityName, Value as ClarityValue,
};
use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::{self, Expression};
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;
use oxc_traverse::{traverse_mut, Ancestor, Traverse, TraverseCtx};

use crate::{
    parser::{IRFunction, IR},
    to_kebab_case,
    types::ts_to_clar_type,
};

struct StatementConverter<'a> {
    ir: &'a IR<'a>,
    function: &'a IRFunction<'a>,
    expressions: Vec<PreSymbolicExpression>,
    lists_stack: Vec<PreSymbolicExpression>,
    current_context_type: Option<TypeSignature>,
    current_context_type_stack: Vec<Option<TypeSignature>>,
    current_bindings: Vec<(String, Option<TypeSignature>)>,
}

fn atom(name: &str) -> PreSymbolicExpression {
    PreSymbolicExpression::atom(ClarityName::from(name))
}

fn get_clarity_binary_operator(operator: &ast::BinaryOperator) -> &str {
    use ast::BinaryOperator::*;
    match operator {
        Addition => "+",
        Subtraction => "-",
        Multiplication => "*",
        Division => "/",
        Remainder => "mod",
        Exponential => "pow",
        LessThan => "<",
        GreaterThan => ">",
        LessEqualThan => "<=",
        GreaterEqualThan => ">=",
        Equality => "is-eq",
        StrictEquality => "is-eq",
        BitwiseAnd => "bit-and",
        BitwiseOR => "bit-or",
        BitwiseXOR => "bit-xor",
        ShiftLeft => "bit-shift-left",
        ShiftRight => "bit-shift-right",
        Inequality => todo!(),
        StrictInequality => todo!(),
        ShiftRightZeroFill => todo!(),
        In => todo!(),
        Instanceof => todo!(),
    }
}

impl<'a> StatementConverter<'a> {
    fn new(ir: &'a IR, function: &'a IRFunction<'a>) -> Self {
        Self {
            ir,
            function,
            expressions: Vec::new(),
            lists_stack: vec![],
            current_context_type: None,
            current_context_type_stack: vec![],
            current_bindings: vec![],
        }
    }

    fn add_binding(
        &mut self,
        variable_declarator: &ast::VariableDeclarator<'a>,
    ) -> Option<TypeSignature> {
        let type_annotation = if let ast::BindingPattern {
            type_annotation: Some(boxed_type_annotation),
            ..
        } = &variable_declarator.id
        {
            Some(ts_to_clar_type(&boxed_type_annotation.type_annotation).unwrap())
        } else {
            // type annotation is not always needed
            // but this current approach probably isn't robust enough
            None
        };

        let binding_name = if let ast::BindingPattern {
            kind: ast::BindingPatternKind::BindingIdentifier(ident),
            ..
        } = &variable_declarator.id
        {
            ident.name.as_str()
        } else {
            return None;
        };

        self.current_bindings
            .push((binding_name.to_string(), type_annotation.clone()));

        type_annotation
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

    fn infer_binary_expression_type(
        &self,
        operator: &str,
        left_type: Option<&TypeSignature>,
        right_type: Option<&TypeSignature>,
    ) -> Option<TypeSignature> {
        match operator {
            "+" | "-" | "*" | "/" | "mod" => {
                if left_type == Some(&TypeSignature::UIntType)
                    || right_type == Some(&TypeSignature::UIntType)
                {
                    Some(TypeSignature::UIntType)
                } else if left_type == Some(&TypeSignature::IntType)
                    || right_type == Some(&TypeSignature::IntType)
                {
                    Some(TypeSignature::IntType)
                } else {
                    // todo: remove default once we have a more robust type inference system
                    Some(TypeSignature::IntType)
                }
            }
            "<" | ">" | "<=" | ">=" | "is-eq" => Some(TypeSignature::BoolType),
            "bit-and" | "bit-or" | "bit-xor" | "bit-shift-left" | "bit-shift-right" => {
                left_type.cloned().or_else(|| right_type.cloned())
            }
            _ => None,
        }
    }

    fn get_expression_type(&self, expr: &Expression<'a>) -> Option<TypeSignature> {
        match expr {
            Expression::Identifier(ident) => {
                if let Some(param_type) = self.get_parameter_type(ident.name.as_str()) {
                    return Some(param_type.clone());
                }

                if let Some((_, binding_type)) = self
                    .current_bindings
                    .iter()
                    .find(|(name, _)| name == ident.name.as_str())
                {
                    return binding_type.clone();
                }

                None
            }
            Expression::NumericLiteral(_) => {
                // todo: remove default once we have a more robust type inference system
                Some(TypeSignature::IntType)
            }
            Expression::CallExpression(_call_expr) => {
                // todo: for function calls, we could potentially infer the return type
                // for now,return None and let context determine
                None
            }
            Expression::BinaryExpression(bin_expr) => {
                // Recursively determine types of operands
                let left_type = self.get_expression_type(&bin_expr.left);
                let right_type = self.get_expression_type(&bin_expr.right);

                let operator = get_clarity_binary_operator(&bin_expr.operator);

                self.infer_binary_expression_type(operator, left_type.as_ref(), right_type.as_ref())
            }
            _ => None,
        }
    }

    fn ingest_last_stack_item(&mut self) {
        if self.lists_stack.len() == 1 {
            self.expressions.push(self.lists_stack.pop().unwrap());
        } else if self.lists_stack.len() > 1 {
            let last_stack_item = self.lists_stack.pop().unwrap();

            if let Some(last_pre_expr) = self.lists_stack.last_mut() {
                if let PreSymbolicExpressionType::List(list) = &mut last_pre_expr.pre_expr {
                    list.push(last_stack_item.clone());
                }
            }
        }
    }
}

impl<'a> Traverse<'a> for StatementConverter<'a> {
    fn enter_program(&mut self, _node: &mut ast::Program<'a>, _ctx: &mut TraverseCtx<'a>) {
        // println!("enter_program: {:#?}", _node);
    }

    fn exit_program(&mut self, _node: &mut ast::Program<'a>, _ctx: &mut TraverseCtx<'a>) {
        // ingesting remaining items in the stack such as the one in the let binding
        while !self.lists_stack.is_empty() {
            self.ingest_last_stack_item();
        }
    }

    fn enter_variable_declaration(
        &mut self,
        _node: &mut ast::VariableDeclaration<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        // two lists are pushed here, the first one will is the let expression list
        let let_expr = PreSymbolicExpression::list(vec![atom("let")]);
        self.lists_stack.push(let_expr);
        self.lists_stack.push(PreSymbolicExpression::list(vec![]));
    }

    fn exit_variable_declaration(
        &mut self,
        _node: &mut ast::VariableDeclaration<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.ingest_last_stack_item();
    }

    fn enter_variable_declarator(
        &mut self,
        variable_declarator: &mut ast::VariableDeclarator<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        if let Some(type_annotation) = self.add_binding(variable_declarator) {
            self.current_context_type = Some(type_annotation);
        }

        self.lists_stack.push(PreSymbolicExpression::list(vec![]));
    }

    fn exit_variable_declarator(
        &mut self,
        _node: &mut ast::VariableDeclarator<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.current_context_type = None;
        self.ingest_last_stack_item();
    }

    fn enter_call_expression(
        &mut self,
        call_expr: &mut ast::CallExpression<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        if let Expression::StaticMemberExpression(member_expr) = &call_expr.callee {
            if let Expression::Identifier(ident) = &member_expr.object {
                let data_var = self
                    .ir
                    .data_vars
                    .iter()
                    .find(|data_var| data_var.name == ident.name.as_str());
                if let Some(data_var) = data_var {
                    self.current_context_type = Some(data_var.r#type.clone());
                    if member_expr.property.name == "get" {
                        self.lists_stack
                            .push(PreSymbolicExpression::list(vec![atom("var-get")]));
                    } else if member_expr.property.name == "set" {
                        self.lists_stack
                            .push(PreSymbolicExpression::list(vec![atom("var-set")]));
                    }
                }

                if self
                    .ir
                    .std_namespace_import
                    .as_ref()
                    .is_some_and(|n| n == ident.name.as_str())
                {
                    self.lists_stack.push(PreSymbolicExpression::list(vec![atom(
                        member_expr.property.name.as_str(),
                    )]));
                    return;
                }
            }
            return;
        }

        // let callee = match &call_expr.callee {
        //     Expression::Identifier(ident) => atom(ident.name.as_str()),
        //     _ => todo!(),
        // };
        // self.lists_stack
        //     .push(PreSymbolicExpression::list(vec![callee]));

        self.lists_stack.push(PreSymbolicExpression::list(vec![]));
    }

    fn exit_call_expression(
        &mut self,
        _call_expr: &mut ast::CallExpression<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.ingest_last_stack_item();
    }

    fn enter_binding_identifier(
        &mut self,
        node: &mut ast::BindingIdentifier<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.lists_stack
            .push(atom(to_kebab_case(node.name.as_str()).as_str()));
    }

    fn exit_binding_identifier(
        &mut self,
        _node: &mut ast::BindingIdentifier<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.ingest_last_stack_item();
    }

    fn enter_static_member_expression(
        &mut self,
        member_expr: &mut ast::StaticMemberExpression<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        if let Expression::Identifier(ident) = &member_expr.object {
            if self
                .ir
                .std_namespace_import
                .as_ref()
                .is_some_and(|n| n == ident.name.as_str())
            {
                return;
            }

            self.lists_stack.push(atom(ident.name.as_str()));
        }
    }

    fn exit_static_member_expression(
        &mut self,
        member_expr: &mut ast::StaticMemberExpression<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        if let Expression::Identifier(ident) = &member_expr.object {
            if self
                .ir
                .std_namespace_import
                .as_ref()
                .is_some_and(|n| n == ident.name.as_str())
            {
                return;
            }

            self.ingest_last_stack_item();
        }
    }

    fn enter_identifier_reference(
        &mut self,
        ident: &mut ast::IdentifierReference<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        // If the identifier is a TypeReference, just ignore it
        // We might need a more robust approach for this in the future and
        // handle IdentifierReference in their parent enter/exit methods
        if matches!(ctx.parent(), Ancestor::TSTypeReferenceTypeName(_)) {
            return;
        }

        let ident_name = ident.name.as_str();
        let matching_function = self.ir.functions.iter().any(|f| f.name == ident_name);
        if matching_function {
            self.lists_stack
                .push(atom(to_kebab_case(ident_name).as_str()));
            return;
        }

        let matching_data_var = self
            .current_bindings
            .iter()
            .any(|(name, _)| name == ident_name);
        if matching_data_var {
            self.lists_stack
                .push(atom(to_kebab_case(ident_name).as_str()));
            return;
        }

        if let Some((_, name)) = self
            .ir
            .std_specific_imports
            .iter()
            .find(|(_, name)| name == ident_name)
        {
            // todo: get type_signature of the std func
            self.lists_stack.push(atom(name.as_str()));
            return;
        }

        if self
            .ir
            .std_namespace_import
            .as_ref()
            .is_some_and(|n| n == ident_name)
        {
            return;
        }

        // todo: handle keyword, bool, etc, panic otherwise
        self.lists_stack.push(atom(ident.name.as_str()));
    }

    fn exit_identifier_reference(
        &mut self,
        ident: &mut ast::IdentifierReference<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        if self
            .ir
            .std_namespace_import
            .as_ref()
            .is_some_and(|n| n == ident.name.as_str())
        {
            return;
        }

        if matches!(ctx.parent(), Ancestor::TSTypeReferenceTypeName(_)) {
            return;
        }
        self.ingest_last_stack_item();
    }

    fn enter_binary_expression(
        &mut self,
        bin_expr: &mut ast::BinaryExpression<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.lists_stack.push(PreSymbolicExpression::list(vec![]));
        let operator = get_clarity_binary_operator(&bin_expr.operator);

        // Use the new type inference system to determine the result type
        let left_type = self.get_expression_type(&bin_expr.left);
        let right_type = self.get_expression_type(&bin_expr.right);

        self.current_context_type =
            self.infer_binary_expression_type(operator, left_type.as_ref(), right_type.as_ref());

        if matches!(operator, "is-eq" | "<" | ">" | "<=" | ">=") {
            self.current_context_type_stack
                .push(self.current_context_type.clone());
            if let Some(ref ltype) = left_type {
                self.current_context_type = Some(ltype.clone());
            }
        } else {
            self.current_context_type_stack
                .push(self.current_context_type.clone());
        }

        self.lists_stack.push(atom(operator));
        self.ingest_last_stack_item();
    }

    fn exit_binary_expression(
        &mut self,
        _bin_expr: &mut ast::BinaryExpression<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.ingest_last_stack_item();
        self.current_context_type = self.current_context_type_stack.pop().unwrap_or(None);
    }

    fn enter_conditional_expression(
        &mut self,
        node: &mut ast::ConditionalExpression<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        if matches!(node.test, Expression::BinaryExpression(_)) {
            self.lists_stack
                .push(PreSymbolicExpression::list(vec![atom("if")]));
        }
    }

    fn exit_conditional_expression(
        &mut self,
        _node: &mut ast::ConditionalExpression<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.ingest_last_stack_item();
    }

    fn enter_numeric_literal(
        &mut self,
        node: &mut ast::NumericLiteral<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.lists_stack.push(match &self.current_context_type {
            Some(TypeSignature::UIntType) => {
                PreSymbolicExpression::atom_value(ClarityValue::UInt(node.value as u128))
            }
            Some(TypeSignature::IntType) => {
                PreSymbolicExpression::atom_value(ClarityValue::Int(node.value as i128))
            }
            _ => {
                // todo: should not default but panic instead
                PreSymbolicExpression::atom_value(ClarityValue::Int(node.value as i128))
            }
        })
    }

    fn exit_numeric_literal(
        &mut self,
        _node: &mut ast::NumericLiteral<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.ingest_last_stack_item();
    }

    fn enter_string_literal(
        &mut self,
        node: &mut ast::StringLiteral<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.lists_stack.push(PreSymbolicExpression::atom_value(
            ClarityValue::string_ascii_from_bytes(node.value.as_bytes().to_vec()).unwrap(),
        ));
    }

    fn exit_string_literal(
        &mut self,
        _node: &mut ast::StringLiteral<'a>,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.ingest_last_stack_item();
    }

    fn enter_boolean_literal(
        &mut self,
        node: &mut ast::BooleanLiteral,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.lists_stack.push(atom(node.value.to_string().as_str()));
    }

    fn exit_boolean_literal(
        &mut self,
        _node: &mut ast::BooleanLiteral,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.ingest_last_stack_item();
    }
}

/// Convert a TypeScript function to a Clarity expression
pub fn convert_function_body<'a>(
    allocator: &'a Allocator,
    ir: &IR,
    function: &IRFunction<'a>,
) -> Result<PreSymbolicExpression, anyhow::Error> {
    let mut program = ast::Program {
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

    let mut converter = StatementConverter::new(ir, function);
    traverse_mut(&mut converter, allocator, &mut program, scoping);

    if converter.expressions.is_empty() {
        return Err(anyhow::anyhow!("No expressions found"));
    }

    if converter.expressions.len() == 1 {
        Ok(converter.expressions[0].clone())
    } else {
        let mut begin_exprs = vec![atom("begin")];
        begin_exprs.extend(converter.expressions);
        Ok(PreSymbolicExpression::list(begin_exprs))
    }
}

#[cfg(test)]
mod test {
    use clarity::vm::representations::{PreSymbolicExpressionType, Span};
    use indoc::{formatdoc, indoc};
    use oxc_allocator::Allocator;

    use crate::{clarity_std::STD_PKG_NAME, parser::get_ir};

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

    /// asserts the function body of the last function provided in the ts_src
    #[track_caller]
    fn assert_last_function_body_eq(ts_src: &str, expected_clar_source: &str) {
        let expected_pse = get_expected_pse(expected_clar_source);

        let import = format!(r#"import * as c from "{STD_PKG_NAME}";"#);
        let ts_src = format!("{import}\n{ts_src}");

        let allocator = Allocator::default();
        let ir = get_ir(&allocator, "tmp.clar.ts", &ts_src);
        let result = convert_function_body(&allocator, &ir, ir.functions.last().unwrap()).unwrap();
        pretty_assertions::assert_eq!(result, expected_pse);
    }

    #[test]
    fn test_return_bool() {
        let ts_src = "function return_true() { return true; }";
        assert_last_function_body_eq(ts_src, "true");
    }

    #[test]
    fn test_expression_call() {
        let ts_src = formatdoc! { r#"import {{ print }} from "{STD_PKG_NAME}";
            function printtrue() {{ return print(true); }}"#
        };
        assert_last_function_body_eq(&ts_src, "(print true)");
    }

    #[test]
    fn test_expression_multiple_statements() {
        let ts_src = "function printtrue() { c.print(true); return c.print(true); }";
        assert_last_function_body_eq(ts_src, "(begin (print true) (print true))");
    }

    #[test]
    fn test_expression_return_uint() {
        let ts_src = "function printarg(arg: Uint) { return print(arg); }";
        assert_last_function_body_eq(ts_src, "(print arg)");
    }

    #[test]
    fn test_expression_return_ok() {
        let ts_src = "function okarg(arg: Uint) { return ok(arg); }";
        assert_last_function_body_eq(ts_src, "(ok arg)");
    }

    #[test]
    fn test_operator() {
        let ts_src = "function add(a: Uint, b: Uint) { return a + b; }";
        assert_last_function_body_eq(ts_src, "(+ a b)");

        let ts_src = "function sub(a: Uint, b: Uint) { return a - b; }";
        assert_last_function_body_eq(ts_src, "(- a b)");

        let ts_src = "function add1and1() { return 1 + 1; }";
        assert_last_function_body_eq(ts_src, "(+ 1 1)");

        let ts_src = "function add1and2() { return 2 ** 3; }";
        assert_last_function_body_eq(ts_src, "(pow 2 3)");

        let ts_src = "function add1and2() { return 2 % 3; }";
        assert_last_function_body_eq(ts_src, "(mod 2 3)");
    }

    #[test]
    fn test_ternary_operator() {
        let ts_src = indoc!(
            r#"function evenOrOdd(n: Int) {
                return n % 2 === 0 ? 'even' : 'odd';
            }
            "#
        );
        let expected_clar_src = indoc!(r#"(if (is-eq (mod n 2) 0) "even" "odd")"#);
        assert_last_function_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_type_inference() {
        let ts_src = "function add1(a: Int) { return a + 1; }";
        assert_last_function_body_eq(ts_src, "(+ a 1)");

        let ts_src = "function add1(a: Uint) { return a + 1; }";
        assert_last_function_body_eq(ts_src, "(+ a u1)");

        let ts_src = "function add1(a: Int) { return 1 + a; }";
        assert_last_function_body_eq(ts_src, "(+ 1 a)");

        let ts_src = "function add1(a: Uint) { return 1 + a; }";
        assert_last_function_body_eq(ts_src, "(+ u1 a)");
    }

    #[test]
    fn test_ternary_operator_with_type_inference() {
        let ts_src = indoc!(
            r#"function evenOrOdd(n: Uint) {
                return n % 2 === 0 ? 'even' : 'odd';
            }
            "#
        );
        let expected_clar_src = indoc!(r#"(if (is-eq (mod n u2) u0) "even" "odd")"#);
        assert_last_function_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_data_var_type_inference() {
        let ts_src = indoc!(
            r#"const count = new DataVar<Uint>(0);
            function increment() {
                return count.set(count.get() + 1);
            }
            "#
        );
        let expected_clar_src = indoc!(r#"(var-set count (+ (var-get count) u1))"#);
        assert_last_function_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_operator_chaining() {
        // todo: fix variadic operators
        // see a previous implementation for this here:
        // https://github.com/hirosystems/clarinet/blob/6f9a320a425fceaf47c5b5c9867ec7a08bac27d9/components/ts-to-clar/src/expression_converter.rs#L31-L97
        // it wasn't kept because it was a recursive implementation instead of using the traverse pattern
        let ts_src = "function add3(a: Int) { return a + 1 + 2; }";
        assert_last_function_body_eq(ts_src, "(+ (+ a 1) 2)");

        let ts_src = "function add3(a: Uint) { return a + 1 + 2; }";
        assert_last_function_body_eq(ts_src, "(+ (+ a u1) u2)");

        // let ts_src = "function add3(a: Uint) { return 1 + a + 2; }";
        // assert_last_function_body_eq(ts_src, "(+ u1 a u2)");

        //     let ts_src = "function add3(a: Uint) { return 1 + 2 + a; }";
        //     assert_pses_eq(ts_src, "(+ u1 u2 a)");

        //     let ts_src = "function mul2(a: Int) { return a * 1 * 2; }";
        //     assert_pses_eq(ts_src, "(* a 1 2)");

        //     let ts_src = "function mul2(a: Int) { return 1 * a * 2; }";
        //     assert_pses_eq(ts_src, "(* 1 a 2)");

        //     let ts_src = "function mul2(a: Int) { return 1 * 2 * a; }";
        //     assert_pses_eq(ts_src, "(* 1 2 a)");
    }

    #[test]
    fn test_ok_operator() {
        let ts_src = "function okarg(arg: Uint) { return ok(arg + 1); }";
        assert_last_function_body_eq(ts_src, "(ok (+ arg u1))");
    }

    #[test]
    fn test_data_var_get() {
        let ts_src = indoc!(
            r#"const count = new DataVar<Uint>(0);
            function getCount() {
                return count.get();
            }
            "#
        );
        let expected_clar_src = indoc!(r#"(var-get count)"#);
        assert_last_function_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_data_var_set() {
        let ts_src = indoc!(
            r#"const count = new DataVar<Int>(0);
            function increment() {
                return count.set(count.get() + 1);
            }
            "#
        );
        let expected_clar_src = indoc!(r#"(var-set count (+ (var-get count) 1))"#);
        assert_last_function_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_call_custom_function() {
        let ts_src = indoc!(
            r#"function printBool(n: Bool) { return print(n); }
            function printTrue() { return printBool(true); }
            "#
        );
        let expected_clar_src = indoc!(r#"(print-bool true)"#);
        assert_last_function_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_variable_binding() {
        let ts_src = indoc!(
            r#"function printCount() {
                const myCount = 1;
                print(myCount);
                return true;
            }
            "#
        );
        let expected_clar_src = indoc!(
            r#"(let ((my-count 1))
              (print my-count)
              true
            )"#
        );
        assert_last_function_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_variable_binding_type_casting() {
        let ts_src = indoc!(r#"function printCount() { const myCount: Uint = 1; return true; }"#);
        let expected_clar_src = "(let ((my-count u1)) true)";
        assert_last_function_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_multiple_variable_bindings() {
        let ts_src =
            indoc!(r#"function printCount() { const myCount1 = 1, myCount2 = 2; return true; }"#);
        let expected_clar_src = "(let ((my-count1 1) (my-count2 2)) true)";
        assert_last_function_body_eq(ts_src, expected_clar_src);
    }
}
