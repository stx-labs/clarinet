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
