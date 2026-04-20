//! Builds an ordered map of all data variables in a contract

use clarity::vm::analysis::ContractAnalysis;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;
use indexmap::IndexMap;

use crate::analysis::annotation::{get_index_of_span, Annotation};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};

pub struct DataVarData<'a> {
    pub expr: &'a SymbolicExpression,
    /// Has this variable been written to?
    pub written_to: bool,
    /// Has this variable been read from?
    pub read_from: bool,
    /// Annotation comment, if present
    pub annotation: Option<usize>,
}

impl<'a> DataVarData<'a> {
    pub fn new(expr: &'a SymbolicExpression, annotation: Option<usize>) -> Self {
        Self {
            expr,
            written_to: false,
            read_from: false,
            annotation,
        }
    }
}

pub type DataVarMap<'a> = IndexMap<&'a ClarityName, DataVarData<'a>>;

pub struct DataVarMapBuilder<'a> {
    clarity_version: ClarityVersion,
    annotations: &'a [Annotation],
    map: DataVarMap<'a>,
}

impl<'a> DataVarMapBuilder<'a> {
    pub fn build(
        clarity_version: ClarityVersion,
        contract_analysis: &'a ContractAnalysis,
        annotations: &'a [Annotation],
    ) -> DataVarMap<'a> {
        let mut builder = Self {
            clarity_version,
            annotations,
            map: IndexMap::new(),
        };
        traverse(&mut builder, &contract_analysis.expressions);
        builder.map
    }
}

impl<'a> ASTVisitor<'a> for DataVarMapBuilder<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_data_var(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _data_type: &'a SymbolicExpression,
        _initial: &'a SymbolicExpression,
    ) -> bool {
        let annotation = get_index_of_span(self.annotations, &expr.span);
        self.map.insert(name, DataVarData::new(expr, annotation));
        true
    }

    fn visit_var_set(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _value: &'a SymbolicExpression,
    ) -> bool {
        if let Some(data) = self.map.get_mut(name) {
            data.written_to = true;
        }
        true
    }

    fn visit_var_get(&mut self, _expr: &'a SymbolicExpression, name: &'a ClarityName) -> bool {
        if let Some(data) = self.map.get_mut(name) {
            data.read_from = true;
        }
        true
    }
}
