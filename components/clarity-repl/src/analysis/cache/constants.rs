//! Builds `HashMap` of all constants in a contract

use std::collections::HashMap;

use clarity::vm::analysis::ContractAnalysis;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;

use crate::analysis::annotation::{get_index_of_span, Annotation};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};

pub struct ConstantData<'a> {
    pub expr: &'a SymbolicExpression,
    /// Has constant been referenced?
    pub used: bool,
    /// Annotation comment, if present
    pub annotation: Option<usize>,
}

impl<'a> ConstantData<'a> {
    pub fn new(expr: &'a SymbolicExpression, annotation: Option<usize>) -> Self {
        Self {
            expr,
            annotation,
            used: false,
        }
    }
}

pub type ConstantMap<'a> = HashMap<&'a ClarityName, ConstantData<'a>>;

pub struct ConstantMapBuilder<'a> {
    clarity_version: ClarityVersion,
    annotations: &'a Vec<Annotation>,
    map: ConstantMap<'a>,
}

impl<'a> ConstantMapBuilder<'a> {
    pub fn build(
        clarity_version: ClarityVersion,
        contract_analysis: &'a ContractAnalysis,
        annotations: &'a Vec<Annotation>,
    ) -> ConstantMap<'a> {
        let mut builder = Self {
            clarity_version,
            annotations,
            map: HashMap::new(),
        };
        // Traverse the entire AST
        traverse(&mut builder, &contract_analysis.expressions);

        builder.map
    }
}

impl<'a> ASTVisitor<'a> for ConstantMapBuilder<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_constant(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _value: &'a SymbolicExpression,
    ) -> bool {
        let annotation = get_index_of_span(self.annotations, &expr.span);
        let const_data = ConstantData::new(expr, annotation);
        self.map.insert(name, const_data);

        true
    }

    fn visit_atom(&mut self, _expr: &'a SymbolicExpression, atom: &'a ClarityName) -> bool {
        if let Some(data) = self.map.get_mut(atom) {
            data.used = true;
        }
        true
    }
}
