use std::fmt;

#[derive(Debug, thiserror::Error)]
pub enum StaticCostError {
    /// The contract's Clarity source could not be parsed into an AST.
    #[error("failed to parse contract AST: {0}")]
    AstParse(String),

    /// The contract source was not found in the database.
    #[error("contract source not found: {0}")]
    ContractNotFound(String),

    /// The contract could not be loaded from the database.
    #[error("failed to load contract: {0}")]
    ContractLoad(String),

    /// The AST has an unexpected structure (should not occur for valid Clarity).
    #[error("malformed AST: {0}")]
    MalformedAst(&'static str),

    /// A type signature could not be parsed or inferred.
    #[error("failed to parse type: {0}")]
    TypeParse(String),

    /// A cost function evaluation failed.
    #[error("cost calculation failed: {0}")]
    CostCalculation(String),
}

/// A warning emitted during static cost analysis when the analysis is
/// incomplete — e.g. a trait-based `contract-call?` whose target cost
/// cannot be resolved statically.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CostWarning {
    pub function_name: String,
    pub kind: CostWarningKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CostWarningKind {
    /// A `contract-call?` targets a trait parameter whose implementing
    /// contract is unknown, so the cost of the called function is not
    /// included in the estimate.
    UnresolvedTraitCall {
        /// The parameter variable name used as the contract target (e.g. `token-contract`).
        target_variable: String,
        /// The function being called on the trait (e.g. `get-active-bin-id`).
        called_function: String,
    },
}

impl fmt::Display for CostWarning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            CostWarningKind::UnresolvedTraitCall {
                target_variable,
                called_function,
            } => write!(
                f,
                "{}: contract-call? to trait parameter `{}` function `{}` \
                 has unresolved cost (target contract unknown)",
                self.function_name, target_variable, called_function,
            ),
        }
    }
}
