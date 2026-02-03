//! Builds `HashMap` of all bindings (`let` bindings and function args) in a contract

use clarity::vm::analysis::ContractAnalysis;
use clarity::vm::representations::Span;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;
use hashbrown::HashMap;

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
            if !self.traverse_expr(body) {
                return false;
            }
            self.remove_from_scope(&params);
            true
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
        // Add `let` bindings to current scope and save them
        // NOTE: We receive the bindings as a `HashMap`, which means may not be in the order declared
        //       So we have to add all bindings to the current scope before we process any expressions
        //       This means the linter may consider a binding valid before it actually is
        //       This isn't a problem now, since interpretation will fail if you try to use a binding before it's declared,
        //       or try to re-declare an existing binding in a nested scope
        for (name, expr) in bindings {
            let annotation = get_index_of_span(self.annotations, &expr.span);
            self.active_bindings
                .insert(name, BindingData::new(expr, annotation));
        }

        // Traverse expressions in bindings
        for expr in bindings.values() {
            if !self.traverse_expr(expr) {
                return false;
            }
        }

        // Traverse `let` body
        for expr in body {
            if !self.traverse_expr(expr) {
                return false;
            }
        }

        if !self.visit_let(expr, bindings, body) {
            return false;
        }

        // Remove `let` bindings from current scope and save them
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
        true
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
