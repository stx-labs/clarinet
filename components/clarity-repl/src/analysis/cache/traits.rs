//! Builds an ordered map of all trait imports (`use-trait`) in a contract

use clarity::vm::analysis::ContractAnalysis;
use clarity::vm::{ClarityVersion, SymbolicExpression, SymbolicExpressionType};
use clarity_types::types::TraitIdentifier;
use clarity_types::ClarityName;
use indexmap::IndexMap;

use crate::analysis::annotation::{get_index_of_span, Annotation};
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};

pub struct TraitData<'a> {
    pub expr: &'a SymbolicExpression,
    /// Has this trait appeared in a function signature inside `define-trait`?
    pub declare_trait: bool,
    /// Has this trait appeared in the arg list of a public function?
    pub public_fn: bool,
    /// Has this trait appeared in the arg list of a read-only function?
    pub read_only_fn: bool,
    /// Has this trait appeared in the arg list of a private function?
    pub private_fn: bool,
    /// Annotation comment, if present
    pub annotation: Option<usize>,
}

impl<'a> TraitData<'a> {
    pub fn new(expr: &'a SymbolicExpression, annotation: Option<usize>) -> Self {
        Self {
            expr,
            declare_trait: false,
            public_fn: false,
            read_only_fn: false,
            private_fn: false,
            annotation,
        }
    }

    pub fn is_used(&self) -> bool {
        self.declare_trait || self.public_fn || self.read_only_fn
    }

    pub fn is_private_fn_only(&self) -> bool {
        !self.is_used() && self.private_fn
    }
}

pub type TraitMap<'a> = IndexMap<&'a ClarityName, TraitData<'a>>;

pub struct TraitMapBuilder<'a> {
    clarity_version: ClarityVersion,
    annotations: &'a Vec<Annotation>,
    map: TraitMap<'a>,
}

impl<'a> TraitMapBuilder<'a> {
    pub fn build(
        clarity_version: ClarityVersion,
        contract_analysis: &'a ContractAnalysis,
        annotations: &'a Vec<Annotation>,
    ) -> TraitMap<'a> {
        let mut builder = Self {
            clarity_version,
            annotations,
            map: IndexMap::new(),
        };
        traverse(&mut builder, &contract_analysis.expressions);
        builder.map
    }

    /// Search a symbolic expression for a trait reference and run `func` on its `TraitData`
    fn process_symbolic_expr(&mut self, expr: &SymbolicExpression, func: fn(&mut TraitData) -> ()) {
        use SymbolicExpressionType::*;

        match &expr.expr {
            TraitReference(name, _) => {
                self.map.get_mut(name).map(func);
            }
            List(exprs) => {
                for expr in exprs {
                    self.process_symbolic_expr(expr, func);
                }
            }
            Field(_) | AtomValue(_) | Atom(_) | LiteralValue(_) => {}
        }
    }

    /// Search parameter list for trait references and run `func` on their `TraitData`
    fn process_param_list(&mut self, params: &[TypedVar], func: fn(&mut TraitData) -> ()) {
        for param in params {
            self.process_symbolic_expr(param.type_expr, func);
        }
    }
}

impl<'a> ASTVisitor<'a> for TraitMapBuilder<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_use_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _trait_identifier: &TraitIdentifier,
    ) -> bool {
        let annotation = get_index_of_span(self.annotations, &expr.span);
        self.map.insert(name, TraitData::new(expr, annotation));
        true
    }

    /// Override so that we only traverse the params, not the entire function
    fn traverse_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.visit_define_read_only(expr, name, parameters, body)
    }

    fn visit_define_read_only(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        if let Some(params) = parameters {
            self.process_param_list(&params, |usage| usage.read_only_fn = true);
        }
        true
    }

    fn traverse_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.visit_define_public(expr, name, parameters, body)
    }

    fn visit_define_public(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        if let Some(params) = parameters {
            self.process_param_list(&params, |usage| usage.public_fn = true);
        }
        true
    }

    fn traverse_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.visit_define_private(expr, name, parameters, body)
    }

    fn visit_define_private(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        if let Some(params) = parameters {
            self.process_param_list(&params, |usage| usage.private_fn = true);
        }
        true
    }

    fn visit_define_trait(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        functions: &'a [SymbolicExpression],
    ) -> bool {
        // Insert the defined trait itself (for case_trait lint)
        if !self.map.contains_key(name) {
            let annotation = get_index_of_span(self.annotations, &expr.span);
            let mut data = TraitData::new(expr, annotation);
            // A defined trait is inherently "used" — it's being declared, not imported
            data.declare_trait = true;
            self.map.insert(name, data);
        }

        // Also check for trait references within the function signatures (for unused_trait lint)
        for func_expr in functions {
            self.process_symbolic_expr(func_expr, |usage| usage.declare_trait = true);
        }
        true
    }

    /// Skip, not relevant
    fn traverse_define_constant(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _value: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_nft(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _nft_type: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_ft(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _supply: Option<&'a SymbolicExpression>,
    ) -> bool {
        true
    }

    fn traverse_define_map(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _key_type: &'a SymbolicExpression,
        _value_type: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_define_data_var(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        _data_type: &'a SymbolicExpression,
        _initial: &'a SymbolicExpression,
    ) -> bool {
        true
    }

    fn traverse_impl_trait(
        &mut self,
        _expr: &'a SymbolicExpression,
        _trait_identifier: &TraitIdentifier,
    ) -> bool {
        true
    }
}
