use std::cell::Cell;
use std::marker::PhantomData;

use clarity::vm::representations::{PreSymbolicExpression, PreSymbolicExpressionType};
use clarity::vm::types::TypeSignature;
use clarity::vm::{ClarityName, Value as ClarityValue};
use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::{self, Argument, Expression};
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;
use oxc_traverse::{traverse_mut, Ancestor, Traverse};

use crate::clarity_std::{Parameter, FUNCTIONS, KEYWORDS_TYPES};
use crate::parser::{IRFunction, IR};
use crate::to_kebab_case;
use crate::types::{extract_type, ts_to_clar_type};

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
        let current_bindings = function
            .parameters
            .iter()
            .map(|(name, r#type)| (name.to_string(), Some(r#type.clone())))
            .collect();

        let current_context_type = function.return_type.clone();

        Self {
            ir,
            function,
            expressions: Vec::new(),
            lists_stack: vec![],
            current_context_type,
            current_context_type_stack: vec![],
            current_bindings,
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
            // Try to infer type from initializer expression
            if let Some(init_expr) = &variable_declarator.init {
                self.infer_type_from_expression(init_expr)
            } else {
                None
            }
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

    fn infer_type_from_expression(&self, expr: &Expression<'a>) -> Option<TypeSignature> {
        match expr {
            // Handle method call chains like: counts.get(txSender).defaultTo(0)
            Expression::CallExpression(call_expr) => {
                if let Expression::StaticMemberExpression(member_expr) = &call_expr.callee {
                    if member_expr.property.name.as_str() == "defaultTo" {
                        if let Some(root_type) = self.find_root_data_map_type(&member_expr.object) {
                            return Some(root_type);
                        }
                    }
                    // Handle data map get calls like: counts.get(txSender)
                    else if member_expr.property.name.as_str() == "get" {
                        if let Some(root_type) = self.find_root_data_map_type(&member_expr.object) {
                            // For data map get, the type is Optional<T> where T is the value type
                            // But for type inference purposes, we return T as the inner type
                            return Some(root_type);
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn find_root_data_map_type(&self, expr: &Expression<'a>) -> Option<TypeSignature> {
        match expr {
            Expression::StaticMemberExpression(member_expr) => {
                self.find_root_data_map_type(&member_expr.object)
            }
            Expression::CallExpression(call_expr) => {
                self.find_root_data_map_type(&call_expr.callee)
            }
            Expression::Identifier(ident) => {
                let var_name = ident.name.as_str();
                self.ir.data_maps.iter().find_map(|data_map| {
                    if data_map.name == var_name {
                        Some(data_map.value_type.clone())
                    } else {
                        None
                    }
                })
            }
            _ => None,
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

                if let Some((_, type_signature)) = KEYWORDS_TYPES.get(ident.name.as_str()) {
                    return Some(type_signature.clone());
                }

                None
            }
            Expression::NumericLiteral(_) => self.current_context_type.clone(),
            Expression::CallExpression(_call_expr) => {
                // todo: for function calls, we could potentially infer the return type
                // for now, return None and let context determine
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

    fn handle_std_function_call(
        &mut self,
        func_name: &str,
        arguments: &oxc_allocator::Vec<'a, Argument<'a>>,
        ctx: &mut TraverseCtx<'a>,
    ) -> bool {
        if let Some(clar_func) = FUNCTIONS.get(func_name) {
            self.lists_stack
                .push(PreSymbolicExpression::list(vec![atom(clar_func.name)]));

            match func_name {
                "getStacksBlockInfo" | "getBurnBlockInfo" => {
                    if let Some((_, Parameter::Identifiers(allowed_params))) = clar_func
                        .parameters
                        .iter()
                        .find(|(name, _)| name == &"prop-name")
                    {
                        if let Some(Argument::StringLiteral(str)) = arguments.first() {
                            if allowed_params.contains(&str.value.as_str()) {
                                self.lists_stack
                                    .push(atom(str.value.as_str().to_lowercase().as_str()));
                                ctx.state.skip_next_string_argument = true;
                                self.ingest_last_stack_item();
                            }
                        }
                    }
                }
                _ => (),
            }
            return true;
        }
        false
    }

    fn ingest_last_stack_item(&mut self) {
        if self.lists_stack.len() == 1 {
            self.expressions.push(self.lists_stack.pop().unwrap());
        } else if self.lists_stack.len() > 1 {
            let last_stack_item = self.lists_stack.pop().unwrap();

            if let Some(last_pre_expr) = self.lists_stack.last_mut() {
                match &mut last_pre_expr.pre_expr {
                    PreSymbolicExpressionType::List(list) => {
                        list.push(last_stack_item.clone());
                    }
                    PreSymbolicExpressionType::Tuple(tuple) => {
                        tuple.push(last_stack_item.clone());
                    }
                    _ => {
                        // For other types, we can't ingest
                    }
                }
            }
        }
    }
}

struct ConverterState<'a> {
    call_expression_to_ingest: u32,
    skip_next_string_argument: bool,
    object_property_depth: u32,
    array_depth: u32,
    data: PhantomData<&'a ()>,
}
type TraverseCtx<'a> = oxc_traverse::TraverseCtx<'a, ConverterState<'a>>;

impl<'a> Traverse<'a, ConverterState<'a>> for StatementConverter<'a> {
    fn enter_program(&mut self, _node: &mut ast::Program<'a>, _ctx: &mut TraverseCtx<'a>) {
        // println!("enter_program: {:#?}", _node);
    }

    fn exit_program(&mut self, _node: &mut ast::Program<'a>, _ctx: &mut TraverseCtx<'a>) {
        // ingesting remaining items in the stack such as the one in the let binding
        while !self.lists_stack.is_empty() {
            self.ingest_last_stack_item();
        }
    }

    fn enter_expression(
        &mut self,
        node: &mut Expression<'a>,
        ctx: &mut oxc_traverse::TraverseCtx<'a, ConverterState<'a>>,
    ) {
        // add debugging here if needed
    }

    fn enter_ts_as_expression(
        &mut self,
        node: &mut ast::TSAsExpression<'a>,
        _ctx: &mut oxc_traverse::TraverseCtx<'a, ConverterState<'a>>,
    ) {
        if let ast::TSType::TSTypeReference(type_ref) = &node.type_annotation {
            if let ast::TSTypeName::IdentifierReference(ident) = &type_ref.type_name {
                let type_signature = extract_type(&ident.name, None).ok();
                if let Some(type_sig) = type_signature {
                    self.current_context_type = Some(type_sig);
                }
            }
        }
    }

    fn exit_ts_as_expression(
        &mut self,
        _node: &mut ast::TSAsExpression<'a>,
        _ctx: &mut oxc_traverse::TraverseCtx<'a, ConverterState<'a>>,
    ) {
        self.current_context_type = None;
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

        // The declarator list should contain [variable_name, variable_value]
        // We need to convert this into a binding pair (variable_name variable_value)
        // and add that binding pair to the binding list

        if self.lists_stack.len() >= 2 {
            let declarator_list = self.lists_stack.pop().unwrap();

            // Convert the declarator list into a binding pair
            match declarator_list.pre_expr {
                PreSymbolicExpressionType::List(declarator_items) => {
                    if declarator_items.len() == 2 {
                        // Create binding pair (variable_name variable_value)
                        let binding_pair = PreSymbolicExpression::list(declarator_items);

                        // Add the binding pair to the binding list
                        if let Some(binding_list) = self.lists_stack.last_mut() {
                            if let PreSymbolicExpressionType::List(bindings) =
                                &mut binding_list.pre_expr
                            {
                                bindings.push(binding_pair);
                            }
                        }
                    } else {
                        // Fallback: use the old behavior
                        let restored_list = PreSymbolicExpression {
                            pre_expr: PreSymbolicExpressionType::List(declarator_items),
                            id: declarator_list.id,
                            span: declarator_list.span,
                        };
                        self.lists_stack.push(restored_list);
                        self.ingest_last_stack_item();
                    }
                }
                _ => {
                    // Fallback: use the old behavior
                    self.lists_stack.push(declarator_list);
                    self.ingest_last_stack_item();
                }
            }
        }
    }

    fn enter_call_expression(
        &mut self,
        call_expr: &mut ast::CallExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        match &call_expr.callee {
            Expression::Identifier(ident) => {
                let ident_name = ident.name.as_str();
                if self
                    .ir
                    .std_specific_imports
                    .iter()
                    .any(|(_, name)| name == ident_name)
                {
                    if self.handle_std_function_call(ident_name, &call_expr.arguments, ctx) {
                        ctx.state.call_expression_to_ingest += 1;
                        return;
                    } else {
                        println!("Unknown std function: {}", ident_name);
                        return;
                    }
                }
            }
            Expression::StaticMemberExpression(member_expr) => {
                if member_expr.property.name.as_str() == "defaultTo" {
                    self.lists_stack
                        .push(PreSymbolicExpression::list(vec![atom("default-to")]));

                    if let Some(inner_type) = self.get_expression_type(&member_expr.object) {
                        self.current_context_type = Some(inner_type);
                    } else if let Some(inner_type) =
                        self.find_root_data_map_type(&member_expr.object)
                    {
                        self.current_context_type = Some(inner_type);
                    }

                    self.current_context_type_stack
                        .push(self.current_context_type.clone());
                    return;
                }

                let Expression::Identifier(ident) = &member_expr.object else {
                    return;
                };
                let ident_name = ident.name.as_str();

                // Handle data variable access
                if let Some(data_var) = self
                    .ir
                    .data_vars
                    .iter()
                    .find(|data_var| data_var.name == ident_name)
                {
                    self.current_context_type = Some(data_var.r#type.clone());
                    let atom_name = match member_expr.property.name.as_str() {
                        "get" => "var-get",
                        "set" => "var-set",
                        _ => return,
                    };
                    self.lists_stack
                        .push(PreSymbolicExpression::list(vec![atom(atom_name)]));
                    ctx.state.call_expression_to_ingest += 1;
                    return;
                }

                // Handle data map access
                if let Some(data_map) = self
                    .ir
                    .data_maps
                    .iter()
                    .find(|data_map| data_map.name == ident_name)
                {
                    self.current_context_type = Some(data_map.value_type.clone());
                    let atom_name = match member_expr.property.name.as_str() {
                        "get" => "map-get?",
                        "insert" => "map-insert",
                        "set" => "map-set",
                        "delete" => "map-delete",
                        _ => return,
                    };
                    self.lists_stack
                        .push(PreSymbolicExpression::list(vec![atom(atom_name)]));
                    ctx.state.call_expression_to_ingest += 1;
                    return;
                }

                // Handle std namespace calls
                if self
                    .ir
                    .std_namespace_import
                    .as_ref()
                    .is_some_and(|n| n == ident_name)
                {
                    if self.handle_std_function_call(
                        &member_expr.property.name,
                        &call_expr.arguments,
                        ctx,
                    ) {
                        ctx.state.call_expression_to_ingest += 1;
                        return;
                    } else {
                        // @todo: throw / handle error
                        println!("Unknown std function: {}", member_expr.property.name);
                    }
                    return;
                }
            }
            _ => {}
        }

        // todo: handle (currnently) global functions like ok() or err()
        // should probably be part of std
        ctx.state.call_expression_to_ingest += 1;
        self.lists_stack.push(PreSymbolicExpression::list(vec![]));
    }

    fn exit_call_expression(
        &mut self,
        call_expr: &mut ast::CallExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        if let Expression::StaticMemberExpression(member_expr) = &call_expr.callee {
            if member_expr.property.name.as_str() == "defaultTo" {
                // For defaultTo, we need to reorder arguments: (default-to default_value optional_expr)
                if let Some(current_list) = self.lists_stack.last_mut() {
                    if let PreSymbolicExpressionType::List(list) = &mut current_list.pre_expr {
                        if list.len() == 3 {
                            list.swap(1, 2);
                        }
                    }
                }
                self.ingest_last_stack_item();
                return;
            }
        }

        if ctx.state.call_expression_to_ingest > 0 {
            // Don't ingest immediately if we're inside an object property
            // Let the object property handler take care of it
            if ctx.state.object_property_depth == 0 {
                self.ingest_last_stack_item();
            }
            ctx.state.call_expression_to_ingest -= 1;
        }
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

    fn enter_array_expression(
        &mut self,
        _node: &mut ast::ArrayExpression<'a>,
        ctx: &mut oxc_traverse::TraverseCtx<'a, ConverterState<'a>>,
    ) {
        ctx.state.array_depth += 1;
        self.lists_stack
            .push(PreSymbolicExpression::list(vec![atom("list")]));
    }

    fn enter_array_expression_element(
        &mut self,
        _node: &mut ast::ArrayExpressionElement<'a>,
        _ctx: &mut oxc_traverse::TraverseCtx<'a, ConverterState<'a>>,
    ) {
    }

    fn exit_array_expression(
        &mut self,
        _node: &mut ast::ArrayExpression<'a>,
        ctx: &mut oxc_traverse::TraverseCtx<'a, ConverterState<'a>>,
    ) {
        ctx.state.array_depth -= 1;
        // Always ingest arrays, but if we're inside an object property,
        // it will be handled differently by the object property logic
        if ctx.state.object_property_depth == 0 || ctx.state.array_depth > 0 {
            self.ingest_last_stack_item();
        }
    }

    fn enter_object_expression(
        &mut self,
        node: &mut ast::ObjectExpression<'a>,
        _ctx: &mut oxc_traverse::TraverseCtx<'a, ConverterState<'a>>,
    ) {
        self.lists_stack.push(PreSymbolicExpression::tuple(vec![]));
    }

    fn exit_object_expression(
        &mut self,
        _node: &mut ast::ObjectExpression<'a>,
        ctx: &mut oxc_traverse::TraverseCtx<'a, ConverterState<'a>>,
    ) {
        // Only ingest if we're not inside an object property context
        // If we are inside an object property, the exit_object_property handler will take care of ingestion
        if ctx.state.object_property_depth == 0 {
            self.ingest_last_stack_item();
        }
    }

    fn enter_object_property(
        &mut self,
        node: &mut ast::ObjectProperty<'a>,
        ctx: &mut oxc_traverse::TraverseCtx<'a, ConverterState<'a>>,
    ) {
        ctx.state.object_property_depth += 1;

        // Handle the property key (name)
        if let ast::PropertyKey::StaticIdentifier(identifier) = &node.key {
            let key_name = to_kebab_case(identifier.name.as_str());
            self.lists_stack.push(atom(&key_name));
        }
    }

    fn exit_object_property(
        &mut self,
        _node: &mut ast::ObjectProperty<'a>,
        ctx: &mut oxc_traverse::TraverseCtx<'a, ConverterState<'a>>,
    ) {
        ctx.state.object_property_depth -= 1;

        // Check if there's an unprocessed array on the stack that needs to be ingested first
        // Arrays inside object properties don't get auto-ingested, so we need to handle them here
        // Only do this for complex cases where we have nested structures
        if self.lists_stack.len() >= 4 {
            // More conservative - only for deeply nested cases
            // Get the top item without removing it
            if let Some(top_item) = self.lists_stack.last() {
                // Check if it's a list (potential array)
                if let PreSymbolicExpressionType::List(list) = &top_item.pre_expr {
                    // Check if it starts with "list" atom (unprocessed array)
                    if let Some(first) = list.first() {
                        if let PreSymbolicExpressionType::Atom(clarity_name) = &first.pre_expr {
                            // Create a ClarityName for "list" to compare
                            let list_clarity_name = ClarityName::from("list");
                            if clarity_name == &list_clarity_name {
                                // Check if we have nested arrays by looking at the array contents
                                let has_nested_arrays = list.iter().skip(1).any(|item| {
                                    matches!(&item.pre_expr, PreSymbolicExpressionType::List(inner_list)
                                        if inner_list.first().is_some_and(|first| {
                                            matches!(&first.pre_expr, PreSymbolicExpressionType::Atom(name)
                                                if name == &ClarityName::from("list"))
                                        }))
                                });

                                // Only ingest for nested arrays, not simple arrays
                                if has_nested_arrays {
                                    self.ingest_last_stack_item();
                                }
                            }
                        }
                    }
                }
            }
        }

        // Stack order: tuple, key, value (top)
        // We need to ingest both key and value into the tuple in the right order
        // Pop the value first and store it
        let value = self.lists_stack.pop().unwrap();
        // Pop the key and store it
        let key = self.lists_stack.pop().unwrap();

        // Push them back in the correct order: key first, then value
        if let Some(last_pre_expr) = self.lists_stack.last_mut() {
            if let PreSymbolicExpressionType::Tuple(tuple) = &mut last_pre_expr.pre_expr {
                tuple.push(key);
                tuple.push(value);
            }
        }
    }

    fn enter_static_member_expression(
        &mut self,
        member_expr: &mut ast::StaticMemberExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        // Check if this is property access (not a method call)
        let is_method_call = matches!(ctx.parent(), Ancestor::CallExpressionCallee(_));

        if !is_method_call {
            // This is property access, start a clarity get expression with property name first
            let property_name = to_kebab_case(member_expr.property.name.as_str());
            self.lists_stack.push(PreSymbolicExpression::list(vec![
                atom("get"),
                atom(&property_name),
            ]));
            return;
        }

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
        ctx: &mut TraverseCtx<'a>,
    ) {
        // @todo: find a better, generic way to know if in `exit_*` if an expression need to be ingested
        // could be through some sort of expression ids stack
        let is_method_call = matches!(ctx.parent(), Ancestor::CallExpressionCallee(_));

        if !is_method_call {
            self.ingest_last_stack_item();
            return;
        }

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

        // imports are handled in enter_call_expression
        if self
            .ir
            .std_specific_imports
            .iter()
            .any(|(_, name)| name == ident_name)
        {
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

        // native keywords
        if let Some((clarity_name, _)) = KEYWORDS_TYPES.get(ident_name) {
            self.lists_stack.push(atom(clarity_name));
            return;
        }

        // function call
        let matching_function = self.ir.functions.iter().any(|f| f.name == ident_name);
        if matching_function {
            self.lists_stack
                .push(atom(to_kebab_case(ident_name).as_str()));
            return;
        }

        // data-var reference
        let matching_data_var = self
            .current_bindings
            .iter()
            .any(|(name, _)| name == ident_name);
        if matching_data_var {
            self.lists_stack
                .push(atom(to_kebab_case(ident_name).as_str()));
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

        if self
            .ir
            .std_specific_imports
            .iter()
            .any(|(_, name)| name == ident.name.as_str())
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
        let expected_type = match &self.current_context_type {
            Some(r#type) => match r#type {
                TypeSignature::IntType | TypeSignature::UIntType => r#type.clone(),
                TypeSignature::ResponseType(boxed_type) => {
                    let (ok_type, err_type) = boxed_type.as_ref();
                    if ok_type == &TypeSignature::NoType {
                        err_type.clone()
                    } else {
                        ok_type.clone()
                    }
                }
                _ => {
                    // Default to IntType if no context type is set
                    // todo: when type inference is more robust, maybe panic
                    TypeSignature::IntType
                }
            },
            None => {
                // Default to IntType if no context type is set
                // todo: when type inference is more robust, maybe panic
                TypeSignature::IntType
            }
        };
        self.lists_stack.push(match expected_type {
            TypeSignature::UIntType => {
                PreSymbolicExpression::atom_value(ClarityValue::UInt(node.value as u128))
            }
            TypeSignature::IntType => {
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
        ctx: &mut TraverseCtx<'a>,
    ) {
        // Ingest if:
        // - Not inside any object property (object_property_depth == 0), OR
        // - Inside an array (array_depth > 0), even if inside object property
        if ctx.state.object_property_depth == 0 || ctx.state.array_depth > 0 {
            self.ingest_last_stack_item();
        }
    }

    fn enter_string_literal(
        &mut self,
        node: &mut ast::StringLiteral<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        if ctx.state.skip_next_string_argument {
            return;
        }
        self.lists_stack.push(PreSymbolicExpression::atom_value(
            ClarityValue::string_ascii_from_bytes(node.value.as_bytes().to_vec()).unwrap(),
        ));
    }

    fn exit_string_literal(
        &mut self,
        _node: &mut ast::StringLiteral<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        if ctx.state.skip_next_string_argument {
            ctx.state.skip_next_string_argument = false;
            return;
        }
        // Ingest if:
        // - Not inside any object property (object_property_depth == 0), OR
        // - Inside an array (array_depth > 0), even if inside object property
        if ctx.state.object_property_depth == 0 || ctx.state.array_depth > 0 {
            self.ingest_last_stack_item();
        }
    }

    fn enter_boolean_literal(
        &mut self,
        node: &mut ast::BooleanLiteral,
        _ctx: &mut TraverseCtx<'a>,
    ) {
        self.lists_stack.push(atom(node.value.to_string().as_str()));
    }

    fn exit_boolean_literal(&mut self, _node: &mut ast::BooleanLiteral, ctx: &mut TraverseCtx<'a>) {
        // Ingest if:
        // - Not inside any object property (object_property_depth == 0), OR
        // - Inside an array (array_depth > 0), even if inside object property
        if ctx.state.object_property_depth == 0 || ctx.state.array_depth > 0 {
            self.ingest_last_stack_item();
        }
    }

    fn enter_if_statement(&mut self, node: &mut ast::IfStatement<'a>, _ctx: &mut TraverseCtx<'a>) {
        let is_early_return = match &node.consequent {
            ast::Statement::ReturnStatement(_) => true,
            ast::Statement::BlockStatement(block) => {
                block.body.len() == 1
                    && matches!(block.body.first(), Some(ast::Statement::ReturnStatement(_)))
            }
            _ => false,
        };

        if is_early_return {
            // `if` with early return is treated as an `asserts!`
            self.lists_stack
                .push(PreSymbolicExpression::list(vec![atom("asserts!")]));
        }
    }

    fn exit_if_statement(&mut self, node: &mut ast::IfStatement<'a>, _ctx: &mut TraverseCtx<'a>) {
        // Check if this was an assert pattern (early return)
        let is_early_return = match &node.consequent {
            ast::Statement::ReturnStatement(_) => true,
            ast::Statement::BlockStatement(block) => {
                block.body.len() == 1
                    && matches!(block.body.first(), Some(ast::Statement::ReturnStatement(_)))
            }
            _ => false,
        };

        if is_early_return {
            // a `if` with early return is treated as an `asserts!`
            // the condition (1st item) neeed to wraped in (not ...)
            // the last item is the `asserts!`
            self.lists_stack.last_mut().map(|item| {
                if let PreSymbolicExpressionType::List(list) = &mut item.pre_expr {
                    // the first item is the condition
                    let condition = list.get_mut(1).cloned().unwrap_or(atom("true"));
                    let not_condition = PreSymbolicExpression::list(vec![atom("not"), condition]);
                    list[1] = not_condition;
                }
            });
            //
            self.ingest_last_stack_item();
        }
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
    let state = ConverterState {
        call_expression_to_ingest: 0,
        skip_next_string_argument: false,
        object_property_depth: 0,
        array_depth: 0,
        data: PhantomData,
    };
    traverse_mut(&mut converter, allocator, &mut program, scoping, state);

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

    use super::*;
    use crate::clarity_std::STD_PKG_NAME;
    use crate::parser::get_ir;

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

    fn get_expected_pse(expected_clar_src: &str) -> PreSymbolicExpression {
        let mut expected_pse = clarity::vm::ast::parser::v2::parse(expected_clar_src).unwrap();
        set_pse_span_to_0(&mut expected_pse);
        expected_pse.into_iter().next().unwrap()
    }

    /// asserts the function body of the last function provided in the ts_src
    #[track_caller]
    fn assert_body_eq(ts_src: &str, expected_clar_src: &str) {
        let expected_pse = get_expected_pse(expected_clar_src);

        let allocator = Allocator::default();
        let ir = get_ir(&allocator, "tmp.clar.ts", ts_src);
        let result = convert_function_body(&allocator, &ir, ir.functions.last().unwrap()).unwrap();
        pretty_assertions::assert_eq!(result, expected_pse);
    }

    /// include the std lib impact as `c`
    #[track_caller]
    fn assert_body_eq_with_std(ts_src: &str, expected_clar_src: &str) {
        let import = format!(r#"import * as c from "{STD_PKG_NAME}";"#);
        let ts_src = format!("{import}\n{ts_src}");

        assert_body_eq(&ts_src, expected_clar_src);
    }

    #[test]
    fn test_return_bool() {
        let ts_src = "function return_true() { return true; }";
        assert_body_eq(ts_src, "true");
    }

    #[test]
    fn test_expression_call() {
        let ts_src = formatdoc! { r#"import {{ print }} from "{STD_PKG_NAME}";
            function printtrue() {{ return print(true); }}"#
        };
        assert_body_eq(&ts_src, "(print true)");
    }

    #[test]
    fn test_expression_multiple_statements() {
        let ts_src = "function printtrue() { c.print(true); return c.print(true); }";
        assert_body_eq_with_std(ts_src, "(begin (print true) (print true))");
    }

    #[test]
    fn test_expression_return_uint() {
        let ts_src = formatdoc! { r#"import {{ print }} from "{STD_PKG_NAME}";
            function printarg(arg: Uint) {{ return print(arg); }};"#
        };
        assert_body_eq_with_std(&ts_src, "(print arg)");
    }

    #[test]
    fn test_expression_return_ok() {
        let ts_src = "function okarg(arg: Uint) { return ok(arg); }";
        assert_body_eq(ts_src, "(ok arg)");
    }

    #[test]
    fn test_operator() {
        let ts_src = "function add(a: Uint, b: Uint) { return a + b; }";
        assert_body_eq(ts_src, "(+ a b)");

        let ts_src = "function sub(a: Uint, b: Uint) { return a - b; }";
        assert_body_eq(ts_src, "(- a b)");

        let ts_src = "function add1and1() { return 1 + 1; }";
        assert_body_eq(ts_src, "(+ 1 1)");

        let ts_src = "function add1and2() { return 2 ** 3; }";
        assert_body_eq(ts_src, "(pow 2 3)");

        let ts_src = "function add1and2() { return 2 % 3; }";
        assert_body_eq(ts_src, "(mod 2 3)");
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
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_type_inference() {
        let ts_src = "function add1(a: Int) { return a + 1; }";
        assert_body_eq(ts_src, "(+ a 1)");

        let ts_src = "function add1(a: Uint) { return a + 1; }";
        assert_body_eq(ts_src, "(+ a u1)");

        let ts_src = "function add1(a: Int) { return 1 + a; }";
        assert_body_eq(ts_src, "(+ 1 a)");

        let ts_src = "function add1(a: Uint) { return 1 + a; }";
        assert_body_eq(ts_src, "(+ u1 a)");
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
        assert_body_eq(ts_src, expected_clar_src);
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
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_operator_chaining() {
        // todo: fix variadic operators
        // see a previous implementation for this here:
        // https://github.com/hirosystems/clarinet/blob/6f9a320a425fceaf47c5b5c9867ec7a08bac27d9/components/ts-to-clar/src/expression_converter.rs#L31-L97
        // it wasn't kept because it was a recursive implementation instead of using the traverse pattern
        let ts_src = "function add3(a: Int) { return a + 1 + 2; }";
        assert_body_eq(ts_src, "(+ (+ a 1) 2)");

        let ts_src = "function add3(a: Uint) { return a + 1 + 2; }";
        assert_body_eq(ts_src, "(+ (+ a u1) u2)");

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
        assert_body_eq(ts_src, "(ok (+ arg u1))");
    }

    #[test]
    fn test_err_operator() {
        let ts_src = "function err1() { return err(1); }";
        assert_body_eq(ts_src, "(err 1)");
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
        assert_body_eq(ts_src, expected_clar_src);
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
        assert_body_eq(ts_src, expected_clar_src);

        let ts_src = indoc!(
            r#"const count = new DataVar<Int>(0);
            function set1() {
                return count.set(1);
            }
            "#
        );
        let expected_clar_src = indoc! {
            r#"(var-set count 1)"#
        };
        assert_body_eq(ts_src, expected_clar_src);

        let ts_src = indoc!(
            r#"const count = new DataVar<Int>(0);
            function increment() {
                count.set(count.get() + 1);
                return ok("alright");
            }
            "#
        );
        let expected_clar_src = indoc! {
            r#"(begin
                (var-set count (+ (var-get count) 1))
                (ok "alright")
            )"#
        };
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_as_type_inference() {
        let ts_src = "function returnU2() { return 2 as Uint; }";
        let expected_clar_src = "u2";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_call_custom_function() {
        let ts_src = indoc!(
            r#"function printBool(n: Bool) { return print(n); }
            function printTrue() { return printBool(true); }
            "#
        );
        let expected_clar_src = indoc!(r#"(print-bool true)"#);
        assert_body_eq_with_std(ts_src, expected_clar_src);
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
        assert_body_eq_with_std(ts_src, expected_clar_src);
    }

    #[test]
    fn test_variable_binding_type_casting() {
        let ts_src = "function casting() { const myCount: Uint = 1; return true; }";
        let expected_clar_src = "(let ((my-count u1)) true)";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_multiple_variable_bindings() {
        let ts_src = "function printCount() { const myCount1 = 1, myCount2 = 2; return true; }";
        let expected_clar_src = "(let ((my-count1 1) (my-count2 2)) true)";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_function_arg_casing() {
        let ts_src = indoc!(
            r#"const addr = new DataVar<Principal>(txSender);
            function updateAddr(newAddr: Principal) { return ok(addr.set(newAddr)); }"#
        );
        let expected_clar_src = "(ok (var-set addr new-addr))";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_native_keywords_support() {
        let ts_src = "function okTxSender() { return ok(txSender); }";
        let expected_clar_src = "(ok tx-sender)";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_native_keywords_type_inference() {
        let ts_src = "function previousBlockHeight() { return stacksBlockHeight - 1; }";
        let expected_clar_src = "(- stacks-block-height u1)";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_native_functions_support() {
        let ts_src = "function myToInt(n: Uint) { return c.toInt(n); }";
        let expected_clar_src = "(to-int n)";
        assert_body_eq_with_std(ts_src, expected_clar_src);

        // test without the namespace import
        let ts_src = formatdoc! {
            r#" import {{ toInt }} from "{STD_PKG_NAME}";
            function myToInt(n: Uint) {{ return toInt(n); }}"#
        };
        let expected_clar_src = "(to-int n)";
        assert_body_eq(&ts_src, expected_clar_src);
    }

    #[test]
    fn test_get_stacks_block_info() {
        let ts_src = indoc! {
            r#"function getTime() {
                return c.getStacksBlockInfo("time", stacksBlockHeight);
            }"#
        };
        let expected_clar_src = "(get-stacks-block-info? time stacks-block-height)";
        assert_body_eq_with_std(ts_src, expected_clar_src);
    }

    #[test]
    fn test_access_tuple_prop() {
        let ts_src = indoc!(
            r#"const count = new DataVar<{ currentValue: Uint }>({ currentValue: 0 });
            function getCount() { return count.get().currentValue; }"#
        );
        let expected_clar_src = indoc!(r#"(get current-value (var-get count))"#);
        assert_body_eq(ts_src, expected_clar_src);

        let ts_src = indoc!(
            r#"const count = new DataVar<{ currentValue: Uint }>({ currentValue: 0 });
                function getCount() {
                    const data = count.get();
                    return data.currentValue;
                };"#
        );
        let expected_clar_src =
            indoc!(r#"(let ((data (var-get count))) (get current-value data))"#);
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_handle_basic_tuple_expr() {
        let ts_src = indoc! {
            r#"function dataTrue() {
                return {
                    keyBool: true,
                    keyInt: 1,
                    keyString: "value"
                };
            }"#
        };
        let expected_clar_src = r#"{ key-bool: true, key-int: 1, key-string: "value" }"#;
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_handle_nested_tuple() {
        let ts_src = indoc! {
            r#"function dataTrue() {
                return {
                    keyTuple: { keyBool: true }
                };
            }"#
        };
        let expected_clar_src = r#"{ key-tuple: { key-bool: true } }"#;
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_simple_list() {
        let ts_src = "function returnList() { return [1, 2, 3]; }";
        let expected_clar_src = "(list 1 2 3)";
        assert_body_eq(ts_src, expected_clar_src);

        let ts_src = "function returnList() { return [[true, false], [true, false]]; }";
        let expected_clar_src = "(list (list true false) (list true false))";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_simple_object_with_array() {
        let ts_src = r#"function test() { return { list1: [1, 2, 3] }; }"#;
        let expected_clar_src = r#"{ list1: (list 1 2 3) }"#;
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_variable_binding_with_array() {
        let ts_src = r#"function test() { const data = [1, 2, 3]; return data; }"#;
        let expected_clar_src = r#"(let ((data (list 1 2 3))) data)"#;
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_simple_object_variable() {
        let ts_src = r#"function test() { const data = { key1: 1, key2: 2 }; return data; }"#;
        let expected_clar_src = r#"(let ((data { key1: 1, key2: 2 })) data)"#;
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_object_with_single_array() {
        let ts_src = r#"function test() { const data = { list1: [1, 2] }; return data; }"#;
        let expected_clar_src = r#"(let ((data { list1: (list 1 2) })) data)"#;
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_get_var_in_tuple() {
        let ts_src = indoc! {
            r#"const count = new DataVar<Uint>(0);
            function getCount() {
                return { data: count.get() };
            }"#
        };
        let expected_clar_src = "{ data: (var-get count) }";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_object_with_nested_list() {
        let ts_src = indoc! {
            r#"function returnList() {
                return { list: [[true, false]] };
            }"#
        };
        let expected_clar_src = "{ list: (list (list true false)) }";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_data_map_get() {
        let ts_src = indoc! {
            r#"const counts = new DataMap<Principal, Uint>();
            function getMyCount() {
                const count = counts.get(txSender);
                return count;
            }"#
        };
        let expected_clar_src = "(let ((count (map-get? counts tx-sender))) count)";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_optional_default_to() {
        let ts_src = indoc! {
            r#"const counts = new DataMap<Principal, Uint>();
            function getMyCount() {
                return counts.get(txSender).defaultTo(0);
            }"#
        };
        let expected_clar_src = "(default-to u0 (map-get? counts tx-sender))";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_optional_default_to_on_let_binding() {
        let ts_src = indoc! {
            r#"const counts = new DataMap<Principal, Uint>();
            function getMyCount() {
                const count = counts.get(txSender);
                return count.defaultTo(0);
            }"#
        };
        let expected_clar_src = indoc! {
            r#"(let ((count (map-get? counts tx-sender)))
                (default-to u0 count)
            )"#
        };
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_optional_default_to_type_inference() {
        let ts_src = indoc! {
            r#"const counts = new DataMap<Principal, Uint>();
            function getMyCountPlus1() {
                return counts.get(txSender).defaultTo(0) + 1;
            }"#
        };
        let expected_clar_src = "(+ (default-to u0 (map-get? counts tx-sender)) u1)";
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_variable_type_inference() {
        let ts_src = indoc! {
            r#"const counts = new DataMap<Principal, Uint>();
            function getMyCountPlus1() {
                const currentCount = counts.get(txSender).defaultTo(0);
                return currentCount + 1;
            }"#
        };
        let expected_clar_src = indoc! {
            r#"(let ((current-count (default-to u0 (map-get? counts tx-sender))))
                (+ current-count u1)
            )"#
        };
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_if_with_early_return() {
        let ts_src = indoc! {
            r#"function evenOrOdd(n: Uint) {
                if (n % 2 == 0) return "even";
                return "odd";
            }"#
        };
        let expected_clar_src = indoc! {
            r#"(begin
                (asserts! (not (is-eq (mod n u2) u0)) "even")
                "odd"
            )"#
        };
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_assert_with_error_code() {
        let ts_src = indoc! {
            r#"function validateAmount(amount: Uint) {
                if (amount <= 0) return err(400 as Uint);
                return ok(amount);
            }"#
        };
        let expected_clar_src = indoc! {
            r#"(begin
                (asserts! (not (<= amount u0)) (err u400))
                (ok amount)
            )"#
        };
        assert_body_eq(ts_src, expected_clar_src);
    }

    #[test]
    fn test_assert_simple_condition() {
        let ts_src = indoc! {
            r#"function requireAuth(authorized: Bool) {
                if (!authorized) return err(403 as Uint);
                return ok(true);
            }"#
        };
        let expected_clar_src = indoc! {
            r#"(begin
                (asserts! (not authorized) (err u403))
                (ok true)
            )"#
        };
        assert_body_eq(ts_src, expected_clar_src);
    }
}
