//! Data structures used in multiple lints/analysis passes

use clarity::vm::analysis::ContractAnalysis;

pub mod bindings;
pub mod constants;
pub mod data_vars;
pub mod functions;
pub mod maps;
pub mod tokens;
pub mod traits;

use bindings::{BindingMap, BindingMapBuilder};
use constants::{ConstantMap, ConstantMapBuilder};
use data_vars::{DataVarMap, DataVarMapBuilder};
use functions::{FnMap, FnMapBuilder, FnMaps, PrivateFnMap};
use maps::{MapDefinitionMap, MapDefinitionMapBuilder};
use tokens::{TokenMap, TokenMapBuilder, TokenMaps};
use traits::{DeclaredTraitMap, ImportedTraitMap, TraitMapBuilder, TraitMaps};

use crate::analysis::annotation::Annotation;

/// Container struct for all cached items
/// All fields are lazy-initialized, and only created if used in at least one pass
pub struct AnalysisCache<'a> {
    pub contract_analysis: &'a ContractAnalysis,
    pub annotations: &'a Vec<Annotation>,

    constants: Option<ConstantMap<'a>>,
    bindings: Option<BindingMap<'a>>,
    data_vars: Option<DataVarMap<'a>>,
    functions: Option<FnMaps<'a>>,
    maps: Option<MapDefinitionMap<'a>>,
    tokens: Option<TokenMaps<'a>>,
    traits: Option<TraitMaps<'a>>,
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
            tokens: None,
            traits: None,
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

    pub fn get_private_fns(&mut self) -> &PrivateFnMap<'a> {
        &self.get_functions().private
    }

    pub fn get_maps(&mut self) -> &MapDefinitionMap<'a> {
        self.maps.get_or_insert(MapDefinitionMapBuilder::build(
            self.contract_analysis.clarity_version,
            self.contract_analysis,
            self.annotations,
        ))
    }

    fn get_tokens(&mut self) -> &TokenMaps<'a> {
        self.tokens.get_or_insert(TokenMapBuilder::build(
            self.contract_analysis.clarity_version,
            self.contract_analysis,
            self.annotations,
        ))
    }

    pub fn get_fts(&mut self) -> &TokenMap<'a> {
        &self.get_tokens().fts
    }

    pub fn get_nfts(&mut self) -> &TokenMap<'a> {
        &self.get_tokens().nfts
    }

    fn get_traits(&mut self) -> &TraitMaps<'a> {
        self.traits.get_or_insert(TraitMapBuilder::build(
            self.contract_analysis.clarity_version,
            self.contract_analysis,
            self.annotations,
        ))
    }

    pub fn get_declared_traits(&mut self) -> &DeclaredTraitMap<'a> {
        &self.get_traits().declared
    }

    pub fn get_imported_traits(&mut self) -> &ImportedTraitMap<'a> {
        &self.get_traits().imported
    }
}
