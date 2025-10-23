use std::collections::HashMap;

use clarity_repl::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};
use clarity_repl::clarity::functions::define::DefineFunctions;
use clarity_repl::clarity::vm::types::{
    QualifiedContractIdentifier, StandardPrincipalData, TraitIdentifier,
};
use clarity_repl::clarity::{ClarityName, SymbolicExpression};
use lsp_types::Range;

use super::helpers::span_to_range;
#[cfg(target_arch = "wasm32")]
#[allow(unused_imports)]
use crate::utils::log;

#[derive(Clone, Debug, PartialEq)]
pub enum DefinitionLocation {
    Internal(Range),
    External(QualifiedContractIdentifier, ClarityName),
}

// `global` holds all of the top-level user-defined keywords that are available in the global scope
// `local` holds the locally user-defined keywords: function parameters, let and match bindings
// when a user-defined keyword is used in the code, its position and definition location are stored in `tokens`
#[derive(Clone, Debug, Default)]
pub struct Definitions {
    pub tokens: HashMap<(u32, u32), DefinitionLocation>,
    global: HashMap<ClarityName, Range>,
    local: HashMap<u64, HashMap<ClarityName, Range>>,
    deployer: Option<StandardPrincipalData>,
}

impl Definitions {
    pub fn new(deployer: Option<StandardPrincipalData>) -> Self {
        Self {
            deployer,
            ..Default::default()
        }
    }

    pub fn run(&mut self, expressions: &[SymbolicExpression]) {
        traverse(self, expressions);
    }

    fn set_function_parameters_scope(&mut self, expr: &SymbolicExpression) -> Option<()> {
        let mut local_scope = HashMap::new();
        let (_, binding_exprs) = expr.match_list()?.get(1)?.match_list()?.split_first()?;
        for binding in binding_exprs {
            if let Some(name) = binding
                .match_list()
                .and_then(|l| l.split_first())
                .and_then(|(name, _)| name.match_atom())
            {
                local_scope.insert(name.to_owned(), span_to_range(&binding.span));
            }
        }
        self.local.insert(expr.id, local_scope);
        Some(())
    }

    // helper method to retrieve definitions of global keyword used in methods such as
    // (var-get <global-keyword>) (map-insert <global-keyword> ...) (nft-burn <global-keyword> ...)
    fn set_definition_for_arg_at_index(
        &mut self,
        expr: &SymbolicExpression,
        token: &ClarityName,
        index: usize,
    ) -> Option<()> {
        let range = self.global.get(token)?;
        let keyword = expr.match_list()?.get(index)?;
        self.tokens.insert(
            (keyword.span.start_line, keyword.span.start_column),
            DefinitionLocation::Internal(*range),
        );
        Some(())
    }

    /// Return `QualifiedContractIdentifier` with transient principal (`S1G2081040G2081040G2081040G208105NK8PE5`) replaced
    fn clone_and_replace_transient(
        &self,
        identifier: &QualifiedContractIdentifier,
    ) -> QualifiedContractIdentifier {
        if identifier.issuer == StandardPrincipalData::transient() {
            if let Some(deployer) = &self.deployer {
                let contract = &format!("{deployer}.{}", identifier.name);
                return QualifiedContractIdentifier::parse(contract)
                    .expect("Failed to set contract name");
            }
        }
        identifier.to_owned()
    }
}

impl<'a> ASTVisitor<'a> for Definitions {
    fn traverse_expr(&mut self, expr: &'a SymbolicExpression) -> bool {
        use clarity_repl::clarity::vm::representations::SymbolicExpressionType::*;
        match &expr.expr {
            AtomValue(value) => self.visit_atom_value(expr, value),
            Atom(name) => self.visit_atom(expr, name),
            List(exprs) => {
                let result = self.traverse_list(expr, exprs);
                // clear local scope after traversing it
                self.local.remove(&expr.id);
                result
            }
            LiteralValue(value) => self.visit_literal_value(expr, value),
            Field(field) => self.visit_field(expr, field),
            TraitReference(name, trait_def) => self.visit_trait_reference(expr, name, trait_def),
        }
    }

    fn visit_atom(&mut self, expr: &'a SymbolicExpression, atom: &'a ClarityName) -> bool {
        // iterate on local scopes to find if the variable is declared in one of them
        // the order does not matter because variable shadowing is not allowed
        for scope in self.local.values() {
            if let Some(range) = scope.get(atom) {
                self.tokens.insert(
                    (expr.span.start_line, expr.span.start_column),
                    DefinitionLocation::Internal(*range),
                );
                return true;
            }
        }

        if let Some(range) = self.global.get(atom) {
            self.tokens.insert(
                (expr.span.start_line, expr.span.start_column),
                DefinitionLocation::Internal(*range),
            );
        }
        true
    }

    fn visit_use_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        trait_identifier: &TraitIdentifier,
    ) -> bool {
        let Some(keyword) = expr.match_list().and_then(|expr| expr.get(1)) else {
            return true;
        };
        let contract_identifier =
            self.clone_and_replace_transient(&trait_identifier.contract_identifier);

        self.tokens.insert(
            (keyword.span.start_line, keyword.span.start_column),
            DefinitionLocation::External(contract_identifier, trait_identifier.name.clone()),
        );
        true
    }

    fn visit_impl_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        trait_identifier: &TraitIdentifier,
    ) -> bool {
        let Some(keyword) = expr.match_list().and_then(|expr| expr.get(1)) else {
            return true;
        };
        let contract_identifier =
            self.clone_and_replace_transient(&trait_identifier.contract_identifier);

        self.tokens.insert(
            (keyword.span.start_line, keyword.span.start_column),
            DefinitionLocation::External(contract_identifier, trait_identifier.name.clone()),
        );
        true
    }

    fn visit_var_set(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _value: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, name, 1);
        true
    }

    fn visit_var_get(&mut self, expr: &'a SymbolicExpression, name: &'a ClarityName) -> bool {
        self.set_definition_for_arg_at_index(expr, name, 1);
        true
    }

    fn visit_map_insert(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        _value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, name, 1);
        true
    }

    fn visit_map_get(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, name, 1);
        true
    }

    fn visit_map_set(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        _value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, name, 1);
        true
    }

    fn visit_map_delete(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, name, 1);
        true
    }

    fn visit_call_user_defined(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _args: &'a [SymbolicExpression],
    ) -> bool {
        if let Some(range) = self.global.get(name) {
            self.tokens.insert(
                (expr.span.start_line, expr.span.start_column + 1),
                DefinitionLocation::Internal(*range),
            );
        }
        true
    }

    fn visit_ft_mint(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _amount: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, token, 1);
        true
    }

    fn visit_ft_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _amount: &'a SymbolicExpression,
        _sender: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, token, 1);
        true
    }

    fn visit_ft_get_balance(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _owner: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, token, 1);
        true
    }

    fn visit_ft_get_supply(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, token, 1);
        true
    }

    fn visit_ft_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _amount: &'a SymbolicExpression,
        _sender: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, token, 1);
        true
    }

    fn visit_nft_burn(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _identifier: &'a SymbolicExpression,
        _sender: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, token, 1);
        true
    }

    fn visit_nft_get_owner(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _identifier: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, token, 1);
        true
    }

    fn visit_nft_mint(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _identifier: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, token, 1);
        true
    }

    fn visit_nft_transfer(
        &mut self,
        expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _identifier: &'a SymbolicExpression,
        _sender: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, token, 1);
        true
    }

    fn visit_map(
        &mut self,
        expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        _sequences: &'a [SymbolicExpression],
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, func, 1);
        true
    }

    fn visit_filter(
        &mut self,
        expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        _sequence: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, func, 1);
        true
    }

    fn visit_fold(
        &mut self,
        expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        _sequence: &'a SymbolicExpression,
        _initial: &'a SymbolicExpression,
    ) -> bool {
        self.set_definition_for_arg_at_index(expr, func, 1);
        true
    }

    fn visit_static_contract_call(
        &mut self,
        expr: &'a SymbolicExpression,
        identifier: &'a QualifiedContractIdentifier,
        function_name: &'a ClarityName,
        _args: &'a [SymbolicExpression],
    ) -> bool {
        if let Some(list) = expr.match_list() {
            if let Some(SymbolicExpression { span, .. }) = list.get(2) {
                let identifier = self.clone_and_replace_transient(identifier);

                self.tokens.insert(
                    (span.start_line, span.start_column),
                    DefinitionLocation::External(identifier, function_name.to_owned()),
                );
            };
        };

        true
    }

    fn traverse_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.set_function_parameters_scope(expr);
        self.traverse_expr(body) && self.visit_define_private(expr, name, parameters, body)
    }

    fn visit_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _parameters: Option<Vec<clarity_repl::analysis::ast_visitor::TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        self.global.insert(name.clone(), span_to_range(&expr.span));
        true
    }

    fn traverse_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.set_function_parameters_scope(expr);
        self.traverse_expr(body) && self.visit_define_read_only(expr, name, parameters, body)
    }

    fn visit_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _parameters: Option<Vec<clarity_repl::analysis::ast_visitor::TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        self.global.insert(name.clone(), span_to_range(&expr.span));
        true
    }

    fn traverse_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.set_function_parameters_scope(expr);
        self.traverse_expr(body) && self.visit_define_public(expr, name, parameters, body)
    }

    fn visit_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _parameters: Option<Vec<clarity_repl::analysis::ast_visitor::TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        self.global.insert(name.clone(), span_to_range(&expr.span));
        true
    }

    fn visit_define_constant(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _value: &'a SymbolicExpression,
    ) -> bool {
        self.global.insert(name.clone(), span_to_range(&expr.span));
        true
    }

    fn visit_define_data_var(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _data_type: &'a SymbolicExpression,
        _initial: &'a SymbolicExpression,
    ) -> bool {
        self.global.insert(name.clone(), span_to_range(&expr.span));
        true
    }

    fn visit_define_map(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key_type: &'a SymbolicExpression,
        _value_type: &'a SymbolicExpression,
    ) -> bool {
        self.global.insert(name.clone(), span_to_range(&expr.span));
        true
    }

    fn visit_define_ft(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _supply: Option<&'a SymbolicExpression>,
    ) -> bool {
        self.global.insert(name.clone(), span_to_range(&expr.span));
        true
    }

    fn visit_define_nft(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _nft_type: &'a SymbolicExpression,
    ) -> bool {
        self.global.insert(name.clone(), span_to_range(&expr.span));
        true
    }

    fn traverse_let(
        &mut self,
        expr: &'a SymbolicExpression,
        bindings: &HashMap<&'a ClarityName, &'a SymbolicExpression>,
        body: &'a [SymbolicExpression],
    ) -> bool {
        let local_scope = || -> Option<HashMap<ClarityName, Range>> {
            let mut result = HashMap::new();

            let binding_exprs = expr.match_list()?.get(1)?.match_list()?;
            for binding in binding_exprs {
                if let Some(name) = binding
                    .match_list()
                    .and_then(|l| l.split_first())
                    .and_then(|(name, _)| name.match_atom())
                {
                    result.insert(name.to_owned(), span_to_range(&binding.span));
                }
            }
            Some(result)
        };
        if let Some(local_scope) = local_scope() {
            self.local.insert(expr.id, local_scope);
        }

        for binding in bindings.values() {
            if !self.traverse_expr(binding) {
                return false;
            }
        }

        for expr in body {
            if !self.traverse_expr(expr) {
                return false;
            }
        }
        self.visit_let(expr, bindings, body)
    }

    fn traverse_match_option(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
        some_name: &'a ClarityName,
        some_branch: &'a SymbolicExpression,
        none_branch: &'a SymbolicExpression,
    ) -> bool {
        self.local.insert(
            expr.id,
            HashMap::from([(some_name.clone(), span_to_range(&input.span))]),
        );
        self.traverse_expr(input)
            && self.traverse_expr(some_branch)
            && self.traverse_expr(none_branch)
            && self.visit_match_option(expr, input, some_name, some_branch, none_branch)
    }

    fn traverse_match_response(
        &mut self,
        expr: &'a SymbolicExpression,
        input: &'a SymbolicExpression,
        ok_name: &'a ClarityName,
        ok_branch: &'a SymbolicExpression,
        err_name: &'a ClarityName,
        err_branch: &'a SymbolicExpression,
    ) -> bool {
        self.local.insert(
            expr.id,
            HashMap::from([
                (ok_name.clone(), span_to_range(&input.span)),
                (err_name.clone(), span_to_range(&input.span)),
            ]),
        );
        self.traverse_expr(input)
            && self.traverse_expr(ok_branch)
            && self.traverse_expr(err_branch)
            && self.visit_match_response(expr, input, ok_name, ok_branch, err_name, err_branch)
    }
}

pub fn get_definitions(
    expressions: &[SymbolicExpression],
    issuer: Option<StandardPrincipalData>,
) -> HashMap<(u32, u32), DefinitionLocation> {
    let mut definitions_visitor = Definitions::new(issuer);
    definitions_visitor.run(expressions);
    definitions_visitor.tokens
}

pub fn get_public_function_definitions(
    definitions: &mut HashMap<ClarityName, Range>,
    expressions: &[SymbolicExpression],
) {
    for expression in expressions {
        if let Some((function_name, args)) = expression
            .match_list()
            .and_then(|l| l.split_first())
            .and_then(|(function_name, args)| Some((function_name.match_atom()?, args)))
        {
            if let Some(DefineFunctions::PublicFunction | DefineFunctions::ReadOnlyFunction) =
                DefineFunctions::lookup_by_name(function_name)
            {
                if let Some(function_name) = args
                    .split_first()
                    .and_then(|(args_list, _)| args_list.match_list()?.split_first())
                    .and_then(|(function_name, _)| function_name.match_atom())
                {
                    definitions.insert(function_name.to_owned(), span_to_range(&expression.span));
                }
            }
        }
    }
}

pub fn get_trait_definitions(
    definitions: &mut HashMap<ClarityName, Range>,
    expressions: &[SymbolicExpression],
) {
    for expression in expressions {
        expression
            .match_list()
            .and_then(|list| list.split_first())
            .and_then(|(function_name, args)| Some((function_name.match_atom()?, args)))
            .filter(|(function_name, _)| {
                matches!(
                    DefineFunctions::lookup_by_name(function_name),
                    Some(DefineFunctions::Trait)
                )
            })
            .and_then(|(_, args)| args.split_first()) // Throw away `define-trait`
            .and_then(|(trait_name, _)| trait_name.match_atom())
            .inspect(|&trait_name| {
                definitions.insert(trait_name.to_owned(), span_to_range(&expression.span));
            });
    }
}

pub fn get_public_function_and_trait_definitions(
    definitions: &mut HashMap<ClarityName, Range>,
    expressions: &[SymbolicExpression],
) {
    get_public_function_definitions(definitions, expressions);
    get_trait_definitions(definitions, expressions);
}

#[cfg(test)]
mod definitions_visitor_tests {
    use std::collections::HashMap;

    use clarity_repl::clarity::ast::build_ast;
    use clarity_repl::clarity::vm::types::{QualifiedContractIdentifier, StandardPrincipalData};
    use clarity_repl::clarity::{ClarityVersion, StacksEpochId, SymbolicExpression};
    use lsp_types::{Position, Range};

    use super::{DefinitionLocation, Definitions};

    fn get_ast(source: &str) -> Vec<SymbolicExpression> {
        let contract_ast = build_ast(
            &QualifiedContractIdentifier::transient(),
            source,
            &mut (),
            ClarityVersion::Clarity1,
            StacksEpochId::Epoch21,
        )
        .unwrap();
        contract_ast.expressions
    }

    fn get_tokens(sources: &str) -> HashMap<(u32, u32), DefinitionLocation> {
        let ast = get_ast(sources);
        let mut definitions_visitor = Definitions::new(Some(StandardPrincipalData::transient()));
        definitions_visitor.run(&ast);
        definitions_visitor.tokens
    }

    fn new_range(start_line: u32, start_column: u32, end_line: u32, end_column: u32) -> Range {
        Range::new(
            Position::new(start_line, start_column),
            Position::new(end_line, end_column),
        )
    }

    #[test]
    fn find_define_private_bindings() {
        let tokens = get_tokens("(define-private (func (arg1 int)) (ok arg1))");
        assert_eq!(tokens.keys().len(), 1);
        assert_eq!(tokens.keys().next(), Some(&(1, 39)));
        assert_eq!(
            tokens.values().next(),
            Some(&DefinitionLocation::Internal(new_range(0, 22, 0, 32)))
        );
    }

    #[test]
    fn find_define_read_only_bindings() {
        let tokens = get_tokens("(define-read-only (func (arg1 int)) (ok arg1))");
        assert_eq!(tokens.keys().len(), 1);
        assert_eq!(tokens.keys().next(), Some(&(1, 41)));
        assert_eq!(
            tokens.values().next(),
            Some(&DefinitionLocation::Internal(new_range(0, 24, 0, 34)))
        );
    }

    #[test]
    fn find_define_public_bindings() {
        let tokens = get_tokens("(define-public (func (arg1 int)) (ok arg1))");
        assert_eq!(tokens.keys().len(), 1);
        assert_eq!(tokens.keys().next(), Some(&(1, 38)));
        assert_eq!(
            tokens.values().next(),
            Some(&DefinitionLocation::Internal(new_range(0, 21, 0, 31)))
        );
    }

    #[test]
    fn find_let_bindings() {
        let tokens = get_tokens("(let ((val1 u1)) (ok val1))");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens.keys().next(), Some(&(1, 22)));
        assert_eq!(
            tokens.values().next(),
            Some(&DefinitionLocation::Internal(new_range(0, 6, 0, 15)))
        );
    }

    #[test]
    fn find_data_var_definition() {
        let tokens = get_tokens(
            [
                "(define-data-var var1 int 1)",
                "(var-get var1)",
                "(var-set var1 2)",
            ]
            .join("\n")
            .as_str(),
        );

        let expected_range = new_range(0, 0, 0, 28);
        assert_eq!(tokens.len(), 2);
        assert_eq!(
            tokens.get(&(2, 10)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
        assert_eq!(
            tokens.get(&(3, 10)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
    }

    #[test]
    fn find_map_definition() {
        let tokens = get_tokens(
            [
                "(define-map owners int principal)",
                "(map-insert owners 1 tx-sender)",
                "(map-get? owners 1)",
                "(map-set owners 1 tx-sender)",
                "(map-delete owners 1)",
            ]
            .join("\n")
            .as_str(),
        );

        let expected_range = new_range(0, 0, 0, 33);
        assert_eq!(tokens.len(), 4);
        assert_eq!(
            tokens.get(&(2, 13)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
        assert_eq!(
            tokens.get(&(3, 11)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
        assert_eq!(
            tokens.get(&(4, 10)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
        assert_eq!(
            tokens.get(&(5, 13)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
    }

    #[test]
    fn find_ft_definition() {
        let tokens = get_tokens(
            [
                "(define-fungible-token ft u1)",
                "(ft-mint? ft u1 tx-sender)",
                "(ft-burn? ft u1 tx-sender)",
                "(ft-get-balance ft tx-sender)",
                "(ft-get-supply ft)",
                "(ft-transfer? ft u1 tx-sender tx-sender)",
            ]
            .join("\n")
            .as_str(),
        );

        let expected_range = new_range(0, 0, 0, 29);
        assert_eq!(tokens.len(), 5);
        assert_eq!(
            tokens.get(&(2, 11)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
        assert_eq!(
            tokens.get(&(3, 11)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
        assert_eq!(
            tokens.get(&(4, 17)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
        assert_eq!(
            tokens.get(&(5, 16)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
        assert_eq!(
            tokens.get(&(6, 15)),
            Some(&DefinitionLocation::Internal(expected_range))
        );
    }

    #[test]
    fn find_definition_in_tuple() {
        let tokens = get_tokens("(define-public (ok-tuple (arg1 int)) (ok { value: arg1 }))");

        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens.get(&(1, 51)),
            Some(&DefinitionLocation::Internal(new_range(0, 25, 0, 35)))
        );
    }

    #[test]
    fn find_definition_in_map() {
        let tokens =
            get_tokens("(define-private (double (n int)) (* n 2)) (map double (list 1 2))");

        assert_eq!(tokens.len(), 2);
        assert_eq!(
            tokens.get(&(1, 48)),
            Some(&DefinitionLocation::Internal(new_range(0, 0, 0, 41)))
        );
    }

    #[test]
    fn find_definition_in_filter() {
        let tokens =
            get_tokens("(define-private (is-even (n int)) (is-eq (* (/ n 2) 2) n)) (filter is-even (list 0 1 2 3 4 5))");

        assert_eq!(tokens.len(), 3);
        assert_eq!(
            tokens.get(&(1, 68)),
            Some(&DefinitionLocation::Internal(new_range(0, 0, 0, 58)))
        );
    }

    #[test]
    fn find_definition_in_fold() {
        let tokens =
            get_tokens("(define-private (sum (a int) (b int)) (+ a b)) (fold sum (list 1 2) 0)");

        assert_eq!(tokens.len(), 3);
        assert_eq!(
            tokens.get(&(1, 54)),
            Some(&DefinitionLocation::Internal(new_range(0, 0, 0, 46)))
        );
    }
}
