//! Builds an ordered map of all map definitions in a contract

use std::collections::HashMap;

use clarity::vm::analysis::ContractAnalysis;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;
use indexmap::IndexMap;

use crate::analysis::annotation::{get_index_of_span, Annotation};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};

pub struct MapData<'a> {
    pub expr: &'a SymbolicExpression,
    /// Has `map-insert` been called on map?
    pub map_insert: bool,
    /// Has `map-delete` been called on map?
    pub map_delete: bool,
    /// Has `map-set` been called on map?
    pub map_set: bool,
    /// Has `map-get?` been called on map?
    pub map_get: bool,
    /// Annotation comment, if present
    pub annotation: Option<usize>,
}

impl<'a> MapData<'a> {
    pub fn new(expr: &'a SymbolicExpression, annotation: Option<usize>) -> Self {
        Self {
            expr,
            map_insert: false,
            map_delete: false,
            map_set: false,
            map_get: false,
            annotation,
        }
    }

    /// Decide if a map is "used"
    ///
    /// This means whether or not it can affect contract execution, which is still possible even if `map-get?` is never called.
    /// For example, it might track unique hashes using `map-insert` and fail if the same hash is used twice.
    /// So the conditions we will use to determine if a map is used:
    ///  - `map-insert` is called
    ///  - `map-set` and either `map-delete` or `map-get?` are called
    pub fn is_used(&self) -> bool {
        self.map_insert || (self.map_set && (self.map_get || self.map_delete))
    }
}

pub type MapDefinitionMap<'a> = IndexMap<&'a ClarityName, MapData<'a>>;

pub struct MapDefinitionMapBuilder<'a> {
    clarity_version: ClarityVersion,
    annotations: &'a Vec<Annotation>,
    map: MapDefinitionMap<'a>,
}

impl<'a> MapDefinitionMapBuilder<'a> {
    pub fn build(
        clarity_version: ClarityVersion,
        contract_analysis: &'a ContractAnalysis,
        annotations: &'a Vec<Annotation>,
    ) -> MapDefinitionMap<'a> {
        let mut builder = Self {
            clarity_version,
            annotations,
            map: IndexMap::new(),
        };
        traverse(&mut builder, &contract_analysis.expressions);
        builder.map
    }
}

impl<'a> ASTVisitor<'a> for MapDefinitionMapBuilder<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_map(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key_type: &'a SymbolicExpression,
        _value_type: &'a SymbolicExpression,
    ) -> bool {
        let annotation = get_index_of_span(self.annotations, &expr.span);
        self.map.insert(name, MapData::new(expr, annotation));
        true
    }

    fn visit_map_insert(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        _value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        if let Some(data) = self.map.get_mut(name) {
            data.map_insert = true;
        }
        true
    }

    fn visit_map_delete(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        if let Some(data) = self.map.get_mut(name) {
            data.map_delete = true;
        }
        true
    }

    fn visit_map_set(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
        _value: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        if let Some(data) = self.map.get_mut(name) {
            data.map_set = true;
        }
        true
    }

    fn visit_map_get(
        &mut self,
        _expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _key: &HashMap<Option<&'a ClarityName>, &'a SymbolicExpression>,
    ) -> bool {
        if let Some(data) = self.map.get_mut(name) {
            data.map_get = true;
        }
        true
    }
}
