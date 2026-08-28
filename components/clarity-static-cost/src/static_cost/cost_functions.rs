use clarity::vm::costs::cost_functions::ClarityCostFunction;
use clarity::vm::costs::costs_1::Costs1;
use clarity::vm::costs::costs_2::Costs2;
use clarity::vm::costs::costs_3::Costs3;
use clarity::vm::costs::costs_4::Costs4;
use clarity::vm::costs::costs_5::Costs5;
use clarity::vm::costs::ExecutionCost;
use clarity::vm::errors::VmExecutionError;
use clarity::vm::functions::NativeFunctions;
use stacks_common::types::StacksEpochId;

/// The cost-function table the VM prices an epoch with.
///
/// Selection is separate from evaluation so a test can check it against
/// `LimitedCostTracker::default_cost_contract_for_epoch`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CostModel {
    Costs1,
    Costs2,
    Costs3,
    Costs4,
    Costs5,
}

impl CostModel {
    /// Mirrors `LimitedCostTracker::default_cost_contract_for_epoch`; keep the
    /// arms grouped the way it groups them.
    fn for_epoch(epoch: StacksEpochId) -> Self {
        match epoch {
            StacksEpochId::Epoch10 => unreachable!("epoch 1.0 is not supported"),
            StacksEpochId::Epoch20 => CostModel::Costs1,
            StacksEpochId::Epoch2_05 => CostModel::Costs2,
            StacksEpochId::Epoch21
            | StacksEpochId::Epoch22
            | StacksEpochId::Epoch23
            | StacksEpochId::Epoch24
            | StacksEpochId::Epoch25
            | StacksEpochId::Epoch30
            | StacksEpochId::Epoch31
            | StacksEpochId::Epoch32 => CostModel::Costs3,
            StacksEpochId::Epoch33 | StacksEpochId::Epoch34 => CostModel::Costs4,
            StacksEpochId::Epoch40 | StacksEpochId::Epoch41 => CostModel::Costs5,
        }
    }

    fn eval(self, f: &ClarityCostFunction, n: u64) -> Result<ExecutionCost, VmExecutionError> {
        match self {
            CostModel::Costs1 => f.eval::<Costs1>(n),
            CostModel::Costs2 => f.eval::<Costs2>(n),
            CostModel::Costs3 => f.eval::<Costs3>(n),
            CostModel::Costs4 => f.eval::<Costs4>(n),
            CostModel::Costs5 => f.eval::<Costs5>(n),
        }
    }

    /// The boot contract the VM loads for this table.
    #[cfg(test)]
    fn boot_contract_name(self) -> &'static str {
        use clarity::vm::costs::{
            COSTS_1_NAME, COSTS_2_NAME, COSTS_3_NAME, COSTS_4_NAME, COSTS_5_NAME,
        };

        match self {
            CostModel::Costs1 => COSTS_1_NAME,
            CostModel::Costs2 => COSTS_2_NAME,
            CostModel::Costs3 => COSTS_3_NAME,
            CostModel::Costs4 => COSTS_4_NAME,
            CostModel::Costs5 => COSTS_5_NAME,
        }
    }
}

/// Extension trait for ClarityCostFunction to evaluate costs for a specific epoch
pub trait ClarityCostFunctionExt {
    fn eval_for_epoch(
        &self,
        n: u64,
        epoch: StacksEpochId,
    ) -> Result<ExecutionCost, VmExecutionError>;
}

impl ClarityCostFunctionExt for ClarityCostFunction {
    fn eval_for_epoch(
        &self,
        n: u64,
        epoch: StacksEpochId,
    ) -> Result<ExecutionCost, VmExecutionError> {
        CostModel::for_epoch(epoch).eval(self, n)
    }
}

pub fn from_native_function(native_function: NativeFunctions) -> ClarityCostFunction {
    match native_function {
        NativeFunctions::Let => ClarityCostFunction::Let,
        NativeFunctions::If => ClarityCostFunction::If,
        NativeFunctions::TupleCons => ClarityCostFunction::TupleCons,
        NativeFunctions::Add => ClarityCostFunction::Add,
        NativeFunctions::Subtract => ClarityCostFunction::Sub,
        NativeFunctions::Multiply => ClarityCostFunction::Mul,
        NativeFunctions::Divide => ClarityCostFunction::Div,
        NativeFunctions::CmpGeq => ClarityCostFunction::Geq,
        NativeFunctions::CmpLeq => ClarityCostFunction::Leq,
        NativeFunctions::CmpLess => ClarityCostFunction::Le,
        NativeFunctions::CmpGreater => ClarityCostFunction::Ge,
        NativeFunctions::ToInt => ClarityCostFunction::IntCast,
        NativeFunctions::ToUInt => ClarityCostFunction::IntCast,
        NativeFunctions::Modulo => ClarityCostFunction::Mod,
        NativeFunctions::Power => ClarityCostFunction::Pow,
        NativeFunctions::Sqrti => ClarityCostFunction::Sqrti,
        NativeFunctions::Log2 => ClarityCostFunction::Log2,
        NativeFunctions::BitwiseXor => ClarityCostFunction::Xor,
        NativeFunctions::And => ClarityCostFunction::And,
        NativeFunctions::Or => ClarityCostFunction::Or,
        NativeFunctions::Not => ClarityCostFunction::Not,
        NativeFunctions::Equals => ClarityCostFunction::Eq,
        NativeFunctions::Map => ClarityCostFunction::Map,
        NativeFunctions::Fold => ClarityCostFunction::Fold,
        NativeFunctions::Append => ClarityCostFunction::Append,
        NativeFunctions::Concat => ClarityCostFunction::Concat,
        NativeFunctions::AsMaxLen => ClarityCostFunction::AsMaxLen,
        NativeFunctions::Len => ClarityCostFunction::Len,
        NativeFunctions::ElementAt => ClarityCostFunction::ElementAt,
        NativeFunctions::ElementAtAlias => ClarityCostFunction::ElementAt,
        NativeFunctions::IndexOf => ClarityCostFunction::IndexOf,
        NativeFunctions::IndexOfAlias => ClarityCostFunction::IndexOf,
        NativeFunctions::BuffToIntLe => ClarityCostFunction::BuffToIntLe,
        NativeFunctions::BuffToUIntLe => ClarityCostFunction::BuffToUIntLe,
        NativeFunctions::BuffToIntBe => ClarityCostFunction::BuffToIntBe,
        NativeFunctions::BuffToUIntBe => ClarityCostFunction::BuffToUIntBe,
        NativeFunctions::IsStandard => ClarityCostFunction::IsStandard,
        NativeFunctions::PrincipalDestruct => ClarityCostFunction::PrincipalDestruct,
        NativeFunctions::PrincipalConstruct => ClarityCostFunction::PrincipalConstruct,
        NativeFunctions::StringToInt => ClarityCostFunction::StringToInt,
        NativeFunctions::StringToUInt => ClarityCostFunction::StringToUInt,
        NativeFunctions::IntToAscii => ClarityCostFunction::IntToAscii,
        NativeFunctions::IntToUtf8 => ClarityCostFunction::IntToUtf8,
        NativeFunctions::GetBurnBlockInfo => ClarityCostFunction::GetBurnBlockInfo,
        NativeFunctions::StxGetAccount => ClarityCostFunction::StxGetAccount,
        NativeFunctions::Slice => ClarityCostFunction::Slice,
        NativeFunctions::ToConsensusBuff => ClarityCostFunction::ToConsensusBuff,
        NativeFunctions::FromConsensusBuff => ClarityCostFunction::FromConsensusBuff,
        NativeFunctions::StxTransferMemo => ClarityCostFunction::StxTransferMemo,
        NativeFunctions::ReplaceAt => ClarityCostFunction::ReplaceAt,
        NativeFunctions::AsContract => ClarityCostFunction::AsContract,
        NativeFunctions::AsContractSafe => ClarityCostFunction::AsContractSafe,
        NativeFunctions::Secp256r1Verify => ClarityCostFunction::Secp256r1verify,
        NativeFunctions::ListCons => ClarityCostFunction::ListCons,
        NativeFunctions::FetchVar => ClarityCostFunction::FetchVar,
        NativeFunctions::SetVar => ClarityCostFunction::SetVar,
        NativeFunctions::FetchEntry => ClarityCostFunction::FetchEntry,
        NativeFunctions::SetEntry => ClarityCostFunction::SetEntry,
        NativeFunctions::InsertEntry => ClarityCostFunction::SetEntry,
        NativeFunctions::DeleteEntry => ClarityCostFunction::SetEntry,
        NativeFunctions::TupleGet => ClarityCostFunction::TupleGet,
        NativeFunctions::TupleMerge => ClarityCostFunction::TupleMerge,
        NativeFunctions::Begin => ClarityCostFunction::Begin,
        NativeFunctions::Hash160 => ClarityCostFunction::Hash160,
        NativeFunctions::Sha256 => ClarityCostFunction::Sha256,
        NativeFunctions::Sha512 => ClarityCostFunction::Sha512,
        NativeFunctions::Sha512Trunc256 => ClarityCostFunction::Sha512t256,
        NativeFunctions::Keccak256 => ClarityCostFunction::Keccak256,
        NativeFunctions::Secp256k1Recover => ClarityCostFunction::Secp256k1recover,
        NativeFunctions::Secp256k1Verify => ClarityCostFunction::Secp256k1verify,
        NativeFunctions::Print => ClarityCostFunction::Print,
        NativeFunctions::ContractCall => ClarityCostFunction::ContractCall,
        NativeFunctions::ContractOf => ClarityCostFunction::ContractOf,
        NativeFunctions::PrincipalOf => ClarityCostFunction::PrincipalOf,
        NativeFunctions::AtBlock => ClarityCostFunction::AtBlock,
        NativeFunctions::GetBlockInfo => ClarityCostFunction::BlockInfo,
        NativeFunctions::GetStacksBlockInfo => ClarityCostFunction::BlockInfo,
        NativeFunctions::GetTenureInfo => ClarityCostFunction::BlockInfo,
        NativeFunctions::ConsError => ClarityCostFunction::ErrCons,
        NativeFunctions::ConsOkay => ClarityCostFunction::OkCons,
        NativeFunctions::ConsSome => ClarityCostFunction::SomeCons,
        NativeFunctions::DefaultTo => ClarityCostFunction::DefaultTo,
        NativeFunctions::Asserts => ClarityCostFunction::Asserts,
        NativeFunctions::UnwrapRet => ClarityCostFunction::UnwrapRet,
        NativeFunctions::UnwrapErrRet => ClarityCostFunction::UnwrapErrOrRet,
        NativeFunctions::IsOkay => ClarityCostFunction::IsOkay,
        NativeFunctions::IsNone => ClarityCostFunction::IsNone,
        NativeFunctions::IsErr => ClarityCostFunction::IsErr,
        NativeFunctions::IsSome => ClarityCostFunction::IsSome,
        NativeFunctions::Unwrap => ClarityCostFunction::Unwrap,
        NativeFunctions::UnwrapErr => ClarityCostFunction::UnwrapErr,
        NativeFunctions::Match => ClarityCostFunction::Match,
        NativeFunctions::TryRet => ClarityCostFunction::TryRet,
        NativeFunctions::Filter => ClarityCostFunction::Filter,
        NativeFunctions::GetTokenBalance => ClarityCostFunction::FtBalance,
        NativeFunctions::GetAssetOwner => ClarityCostFunction::NftOwner,
        NativeFunctions::TransferToken => ClarityCostFunction::FtTransfer,
        NativeFunctions::TransferAsset => ClarityCostFunction::NftTransfer,
        NativeFunctions::MintAsset => ClarityCostFunction::NftMint,
        NativeFunctions::MintToken => ClarityCostFunction::FtMint,
        NativeFunctions::GetTokenSupply => ClarityCostFunction::FtSupply,
        NativeFunctions::BurnToken => ClarityCostFunction::FtBurn,
        NativeFunctions::BurnAsset => ClarityCostFunction::NftBurn,
        NativeFunctions::GetStxBalance => ClarityCostFunction::StxBalance,
        NativeFunctions::StxTransfer => ClarityCostFunction::StxTransfer,
        NativeFunctions::StxBurn => ClarityCostFunction::StxTransfer,
        NativeFunctions::BitwiseAnd => ClarityCostFunction::BitwiseAnd,
        NativeFunctions::BitwiseOr => ClarityCostFunction::BitwiseOr,
        NativeFunctions::BitwiseNot => ClarityCostFunction::BitwiseNot,
        NativeFunctions::BitwiseLShift => ClarityCostFunction::BitwiseLShift,
        NativeFunctions::BitwiseRShift => ClarityCostFunction::BitwiseRShift,
        NativeFunctions::BitwiseXor2 => ClarityCostFunction::Xor,
        NativeFunctions::ContractHash => ClarityCostFunction::ContractHash,
        NativeFunctions::ToAscii => ClarityCostFunction::ToAscii,
        NativeFunctions::RestrictAssets => ClarityCostFunction::RestrictAssets,
        NativeFunctions::AllowanceWithStx => ClarityCostFunction::Unimplemented,
        NativeFunctions::AllowanceWithFt => ClarityCostFunction::Unimplemented,
        NativeFunctions::AllowanceWithNft => ClarityCostFunction::Unimplemented,
        NativeFunctions::AllowanceWithStacking => ClarityCostFunction::Unimplemented,
        NativeFunctions::AllowanceAll => ClarityCostFunction::Unimplemented,
        NativeFunctions::AllowanceWithStaking => ClarityCostFunction::Unimplemented,
        NativeFunctions::AllowanceWithPox => ClarityCostFunction::Unimplemented,
        NativeFunctions::VerifyMerkleProof => ClarityCostFunction::VerifyMerkleProof,
        NativeFunctions::GetBitcoinTxOutput => ClarityCostFunction::GetBitcoinTxOutput,
        NativeFunctions::Ed25519Verify => ClarityCostFunction::Ed25519verify,
        NativeFunctions::Secp256k1Decompress => ClarityCostFunction::Secp256k1decompress,
    }
}

#[cfg(test)]
mod tests {
    use clarity::vm::costs::LimitedCostTracker;

    use super::*;

    /// Our mapping must match the VM's, and nothing else holds them together.
    ///
    /// Compares cost contracts, so it does not cover the mainnet/testnet split
    /// of `costs-2`, which this crate does not model.
    #[test]
    fn cost_model_matches_the_table_the_vm_prices_with() {
        for &epoch in StacksEpochId::ALL {
            // Epoch 1.0 predates Clarity and has no cost contract.
            if epoch == StacksEpochId::Epoch10 {
                continue;
            }

            let expected = LimitedCostTracker::default_cost_contract_for_epoch(epoch)
                .expect("every Clarity-bearing epoch has a default cost contract");

            assert_eq!(
                CostModel::for_epoch(epoch).boot_contract_name(),
                expected,
                "static cost analysis prices {epoch} with a different table than the VM"
            );
        }
    }

    #[test]
    fn epoch_4_prices_with_costs_5() {
        assert_eq!(
            CostModel::for_epoch(StacksEpochId::Epoch33),
            CostModel::Costs4
        );
        assert_eq!(
            CostModel::for_epoch(StacksEpochId::Epoch34),
            CostModel::Costs4
        );
        assert_eq!(
            CostModel::for_epoch(StacksEpochId::Epoch40),
            CostModel::Costs5
        );
    }

    /// The two tables differ, so switching epoch 4.0 is not a no-op.
    #[test]
    fn costs_4_and_costs_5_disagree() {
        let f = ClarityCostFunction::Add;
        assert_ne!(
            f.eval_for_epoch(10, StacksEpochId::Epoch34).unwrap(),
            f.eval_for_epoch(10, StacksEpochId::Epoch40).unwrap(),
        );
    }
}
