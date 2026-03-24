//! Builds an ordered map of all FT and NFT definitions in a contract

use clarity::vm::analysis::ContractAnalysis;
use clarity::vm::{ClarityVersion, SymbolicExpression};
use clarity_types::ClarityName;
use indexmap::IndexMap;

use crate::analysis::annotation::{get_index_of_span, Annotation};
use crate::analysis::ast_visitor::{traverse, ASTVisitor};

pub struct TokenData<'a> {
    pub expr: &'a SymbolicExpression,
    /// Has a mint function been called on this token?
    pub minted: bool,
    /// Annotation comment, if present
    pub annotation: Option<usize>,
}

impl<'a> TokenData<'a> {
    pub fn new(expr: &'a SymbolicExpression, annotation: Option<usize>) -> Self {
        Self {
            expr,
            minted: false,
            annotation,
        }
    }
}

pub type TokenMap<'a> = IndexMap<&'a ClarityName, TokenData<'a>>;

pub struct TokenMaps<'a> {
    pub fts: TokenMap<'a>,
    pub nfts: TokenMap<'a>,
}

pub struct TokenMapBuilder<'a> {
    clarity_version: ClarityVersion,
    annotations: &'a Vec<Annotation>,
    pub fts: TokenMap<'a>,
    pub nfts: TokenMap<'a>,
}

impl<'a> TokenMapBuilder<'a> {
    pub fn build(
        clarity_version: ClarityVersion,
        contract_analysis: &'a ContractAnalysis,
        annotations: &'a Vec<Annotation>,
    ) -> TokenMaps<'a> {
        let mut builder = Self {
            clarity_version,
            annotations,
            fts: IndexMap::new(),
            nfts: IndexMap::new(),
        };
        traverse(&mut builder, &contract_analysis.expressions);
        TokenMaps {
            fts: builder.fts,
            nfts: builder.nfts,
        }
    }
}

impl<'a> ASTVisitor<'a> for TokenMapBuilder<'a> {
    fn get_clarity_version(&self) -> &ClarityVersion {
        &self.clarity_version
    }

    fn visit_define_ft(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _supply: Option<&'a SymbolicExpression>,
    ) -> bool {
        let annotation = get_index_of_span(self.annotations, &expr.span);
        self.fts.insert(name, TokenData::new(expr, annotation));
        true
    }

    fn visit_define_nft(
        &mut self,
        expr: &'a SymbolicExpression,
        name: &'a ClarityName,
        _nft_type: &'a SymbolicExpression,
    ) -> bool {
        let annotation = get_index_of_span(self.annotations, &expr.span);
        self.nfts.insert(name, TokenData::new(expr, annotation));
        true
    }

    fn visit_ft_mint(
        &mut self,
        _expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _amount: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        if let Some(data) = self.fts.get_mut(token) {
            data.minted = true;
        }
        true
    }

    fn visit_nft_mint(
        &mut self,
        _expr: &'a SymbolicExpression,
        token: &'a ClarityName,
        _identifier: &'a SymbolicExpression,
        _recipient: &'a SymbolicExpression,
    ) -> bool {
        if let Some(data) = self.nfts.get_mut(token) {
            data.minted = true;
        }
        true
    }
}
