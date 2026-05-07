//! Builds maps of all function definitions in a contract
//!
//! Functions are stored in `HashMap`s (not `IndexMap`s) because the AST is
//! topologically sorted, not in source order. Lints that need source order
//! should sort diagnostics by span.

use std::collections::HashMap;

use clarity::vm::analysis::ContractAnalysis;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;

use crate::analysis::annotation::{get_index_of_span, Annotation};
use crate::analysis::ast_visitor::{traverse, ASTVisitor, TypedVar};

/// Data for a public or read-only function
pub struct FnData<'a> {
    pub expr: &'a SymbolicExpression,
    /// Annotation comment, if present
    pub annotation: Option<usize>,
}

/// Data for a private function, including call-site tracking
pub struct PrivateFnData<'a> {
    pub expr: &'a SymbolicExpression,
    /// Has this function been called?
    pub called: bool,
    /// Annotation comment, if present
    pub annotation: Option<usize>,
}

impl<'a> PrivateFnData<'a> {
    fn new(expr: &'a SymbolicExpression, annotation: Option<usize>) -> Self {
        Self {
            expr,
            called: false,
            annotation,
        }
    }
}

pub type FnMap<'a> = HashMap<&'a ClarityName, FnData<'a>>;
pub type PrivateFnMap<'a> = HashMap<&'a ClarityName, PrivateFnData<'a>>;

pub struct FnMaps<'a> {
    pub public: FnMap<'a>,
    pub read_only: FnMap<'a>,
    pub private: PrivateFnMap<'a>,
}

pub struct FnMapBuilder<'a> {
    clarity_version: ClarityVersion,
    annotations: &'a [Annotation],
    public: FnMap<'a>,
    read_only: FnMap<'a>,
    private: PrivateFnMap<'a>,
}

impl<'a> FnMapBuilder<'a> {
    pub fn build(
        clarity_version: ClarityVersion,
        contract_analysis: &'a ContractAnalysis,
        annotations: &'a [Annotation],
    ) -> FnMaps<'a> {
        let mut builder = Self {
            clarity_version,
            annotations,
            public: HashMap::new(),
            read_only: HashMap::new(),
            private: HashMap::new(),
        };
        traverse(&mut builder, &contract_analysis.expressions);
        FnMaps {
            public: builder.public,
            read_only: builder.read_only,
            private: builder.private,
        }
    }

    fn set_called(&mut self, func: &ClarityName) {
        if let Some(data) = self.private.get_mut(func) {
            data.called = true;
        }
    }
}

impl<'a> ASTVisitor<'a> for FnMapBuilder<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_public(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        let annotation = get_index_of_span(self.annotations, &expr.span);
        self.public.insert(name, FnData { expr, annotation });
        true
    }

    fn visit_define_read_only(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        let annotation = get_index_of_span(self.annotations, &expr.span);
        self.read_only.insert(name, FnData { expr, annotation });
        true
    }

    fn visit_define_private(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _parameters: Option<Vec<TypedVar<'a>>>,
        _body: &'a SymbolicExpression,
    ) -> bool {
        let annotation = get_index_of_span(self.annotations, &expr.span);
        self.private
            .insert(name, PrivateFnData::new(expr, annotation));
        true
    }

    fn visit_call_user_defined(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _args: &'a [SymbolicExpression],
    ) -> bool {
        self.set_called(name);
        true
    }

    fn visit_filter(
        &mut self,
        _expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        _sequence: &'a SymbolicExpression,
    ) -> bool {
        self.set_called(func);
        true
    }

    fn visit_fold(
        &mut self,
        _expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        _sequence: &'a SymbolicExpression,
        _initial: &'a SymbolicExpression,
    ) -> bool {
        self.set_called(func);
        true
    }

    fn visit_map(
        &mut self,
        _expr: &'a SymbolicExpression,
        func: &'a ClarityName,
        _sequences: &'a [SymbolicExpression],
    ) -> bool {
        self.set_called(func);
        true
    }
}
