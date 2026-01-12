//! Lint to find unused bindings from `let` statements or function args
//!
//! A diagnostic is generated if:
//!  - A function argument is not referenced
//!  - A `let` binding is not referenced

use std::collections::HashMap;
use std::hash::Hash;

use clarity::vm::analysis::ContractAnalysis;
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;

use crate::analysis::annotation::{get_index_of_span, Annotation};
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindingType {
    FunctionArg,
    LetBinding,
}

/// Unique identifier for each binding, including span to prevent ambiguity
#[derive(PartialEq, Eq, Hash)]
pub struct Binding<'a> {
    pub kind: BindingType,
    pub name: &'a ClarityName,
    pub span: Span,
}

/// Data associated with a local variable
pub struct BindingData<'a> {
    pub expr: &'a SymbolicExpression,
    /// Has this variable been referenced?
    pub used: bool,
    /// Annotation comment, if present
    pub annotation: Option<usize>,
}

impl<'a> BindingData<'a> {
    fn new(expr: &'a SymbolicExpression, annotation: Option<usize>) -> Self {
        Self {
            expr,
            annotation,
            used: false,
        }
    }
}

pub type BindingMap<'a> = HashMap<Binding<'a>, BindingData<'a>>;

pub struct BindingMapBuilder<'a> {
    clarity_version: ClarityVersion,
    annotations: &'a Vec<Annotation>,
    /// Names of all `let` bindings and function args currently in scope
    active_bindings: HashMap<&'a ClarityName, BindingData<'a>>,
    /// Variables which went out of scope without being referenced
    bindings: BindingMap<'a>,
}

impl<'a> BindingMapBuilder<'a> {
    pub fn build(
        clarity_version: ClarityVersion,
        contract_analysis: &'a ContractAnalysis,
        annotations: &'a Vec<Annotation>,
    ) -> BindingMap<'a> {
        let mut builder = Self {
            clarity_version,
            annotations,
            active_bindings: HashMap::new(),
            bindings: HashMap::new(),
        };
        // Traverse the entire AST
        traverse(&mut builder, &contract_analysis.expressions);

        builder.bindings
    }

    /// Add function parameters to current scope (`active_bindings`)
    fn add_to_scope(&mut self, params: &[TypedVar<'a>]) {
        for param in params {
            let annotation = get_index_of_span(self.annotations, &param.decl_span);
            let data = BindingData::new(param.type_expr, annotation);
            self.active_bindings.insert(param.name, data);
        }
    }

    /// Remove function parameters to current scope (`active_bindings`)
    fn remove_from_scope(&mut self, params: &[TypedVar<'a>]) {
        for param in params {
            if let Some(data) = self.active_bindings.remove(param.name) {
                let binding = Binding {
                    kind: BindingType::FunctionArg,
                    name: param.name,
                    span: param.decl_span.clone(),
                };
                self.bindings.insert(binding, data);
            }
        }
    }

    fn traverse_define_fn(
        &mut self,
        _expr: &'a SymbolicExpression,
        _name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        if let Some(params) = parameters {
            self.add_to_scope(&params);
            let res = self.traverse_expr(body);
            self.remove_from_scope(&params);
            res
        } else {
            self.traverse_expr(body)
        }
    }
}

impl<'a> ASTVisitor<'a> for BindingMapBuilder<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn traverse_let(
        &mut self,
        expr: &'a SymbolicExpression,
        bindings: &HashMap<&'a ClarityName, &'a SymbolicExpression>,
        body: &'a [SymbolicExpression],
    ) -> bool {
        // Traverse bindings
        for (name, expr) in bindings {
            if !self.traverse_expr(expr) {
                return false;
            }
            // Binding becomes active AFTER traversing it's `expr` and BEFORE traversing the next binding's `expr`
            let annotation = get_index_of_span(self.annotations, &expr.span);
            self.active_bindings
                .insert(name, BindingData::new(expr, annotation));
        }

        // Traverse `let` body
        for expr in body {
            if !self.traverse_expr(expr) {
                return false;
            }
        }

        let res = self.visit_let(expr, bindings, body);

        // Leaving scope, remove current `let` bindings and save those that were not used
        for name in bindings.keys() {
            if let Some(data) = self.active_bindings.remove(name) {
                let binding = Binding {
                    kind: BindingType::LetBinding,
                    name,
                    span: data.expr.span.clone(),
                };
                self.bindings.insert(binding, data);
            }
        }
        res
    }

    fn traverse_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_define_fn(expr, name, parameters, body)
    }

    fn traverse_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_define_fn(expr, name, parameters, body)
    }

    fn traverse_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        parameters: Option<Vec<TypedVar<'a>>>,
        body: &'a SymbolicExpression,
    ) -> bool {
        self.traverse_define_fn(expr, name, parameters, body)
    }

    fn visit_atom(&mut self, _expr: &'a SymbolicExpression, atom: &'a ClarityName) -> bool {
        if let Some(var) = self.active_bindings.get_mut(atom) {
            var.used = true;
        };
        true
    }
}
