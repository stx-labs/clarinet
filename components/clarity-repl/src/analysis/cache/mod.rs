//! Data structures used in multiple lints/analysis passes

use clarity::vm::analysis::ContractAnalysis;

pub mod bindings;
pub mod constants;
pub mod data_vars;
pub mod functions;
pub mod maps;

use bindings::{BindingMap, BindingMapBuilder};
use constants::{ConstantMap, ConstantMapBuilder};
use data_vars::{DataVarMap, DataVarMapBuilder};
use functions::{FnMap, FnMapBuilder, FnMaps};
use maps::{MapDefinitionMap, MapDefinitionMapBuilder};

use crate::analysis::annotation::Annotation;

/// Container struct for all cached itemss
/// All fields are lazy-initialized, and only created if used in at least one pass
pub struct AnalysisCache<'a> {
    pub contract_analysis: &'a ContractAnalysis,
    pub annotations: &'a Vec<Annotation>,

    constants: Option<ConstantMap<'a>>,
    bindings: Option<BindingMap<'a>>,
    data_vars: Option<DataVarMap<'a>>,
    functions: Option<FnMaps<'a>>,
    maps: Option<MapDefinitionMap<'a>>,
}

impl<'a> AnalysisCache<'a> {
    pub fn new(contract_analysis: &'a ContractAnalysis, annotations: &'a Vec<Annotation>) -> Self {
        Self {
            contract_analysis,
            annotations,
            constants: None,
            bindings: None,
            data_vars: None,
            functions: None,
            maps: None,
        }
    }

    pub fn get_constants(&mut self) -> &ConstantMap<'a> {
        self.constants.get_or_insert(ConstantMapBuilder::build(
            self.contract_analysis.clarity_version,
            self.contract_analysis,
            self.annotations,
        ))
    }

    pub fn get_bindings(&mut self) -> &BindingMap<'a> {
        self.bindings.get_or_insert(BindingMapBuilder::build(
            self.contract_analysis.clarity_version,
            self.contract_analysis,
            self.annotations,
        ))
    }

    pub fn get_data_vars(&mut self) -> &DataVarMap<'a> {
        self.data_vars.get_or_insert(DataVarMapBuilder::build(
            self.contract_analysis.clarity_version,
            self.contract_analysis,
            self.annotations,
        ))
    }

    fn get_functions(&mut self) -> &FnMaps<'a> {
        self.functions.get_or_insert(FnMapBuilder::build(
            self.contract_analysis.clarity_version,
            self.contract_analysis,
            self.annotations,
        ))
    }

    pub fn get_public_fns(&mut self) -> &FnMap<'a> {
        &self.get_functions().public
    }

    pub fn get_read_only_fns(&mut self) -> &FnMap<'a> {
        &self.get_functions().read_only
    }

    pub fn get_private_fns(&mut self) -> &FnMap<'a> {
        &self.get_functions().private
    }

    pub fn get_maps(&mut self) -> &MapDefinitionMap<'a> {
        self.maps.get_or_insert(MapDefinitionMapBuilder::build(
            self.contract_analysis.clarity_version,
            self.contract_analysis,
            self.annotations,
        ))
    }
}
