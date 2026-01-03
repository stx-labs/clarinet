//! Data structures used in multiple lints/analysis passes

use clarity::vm::analysis::ContractAnalysis;

pub mod constants;

use constants::Constants;

use crate::analysis::annotation::Annotation;

/// Container struct for all cached itemss
/// All fields are lazy-initialized, and only created if used in at least one pass
pub struct AnalysisCache<'a> {
    pub contract_analysis: &'a ContractAnalysis,
    pub annotations: &'a Vec<Annotation>,

    constants: Option<Constants<'a>>,
}

impl<'a> AnalysisCache<'a> {
    pub fn new(contract_analysis: &'a ContractAnalysis, annotations: &'a Vec<Annotation>) -> Self {
        Self {
            contract_analysis,
            annotations,
            constants: None,
        }
    }

    pub fn get_constant_data(&mut self) -> &Constants<'a> {
        self.constants.get_or_insert(Constants::new(
            self.contract_analysis.clarity_version,
            self.contract_analysis,
            self.annotations,
        ))
    }
}
