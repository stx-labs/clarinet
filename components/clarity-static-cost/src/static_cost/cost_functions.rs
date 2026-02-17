use clarity::vm::costs::cost_functions::ClarityCostFunction;
use clarity::vm::costs::costs_1::Costs1;
use clarity::vm::costs::costs_2::Costs2;
use clarity::vm::costs::costs_3::Costs3;
use clarity::vm::costs::costs_4::Costs4;
use clarity::vm::costs::ExecutionCost;
use clarity::vm::errors::VmExecutionError;
use clarity::vm::functions::NativeFunctions;
use stacks_common::types::StacksEpochId;

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
        match epoch {
            StacksEpochId::Epoch20 => self.eval::<Costs1>(n),
            StacksEpochId::Epoch2_05 => self.eval::<Costs2>(n),
            StacksEpochId::Epoch21
            | StacksEpochId::Epoch22
            | StacksEpochId::Epoch23
            | StacksEpochId::Epoch24
            | StacksEpochId::Epoch25
            | StacksEpochId::Epoch30
            | StacksEpochId::Epoch31
            | StacksEpochId::Epoch32 => self.eval::<Costs3>(n),
            StacksEpochId::Epoch33 => self.eval::<Costs4>(n),
            StacksEpochId::Epoch34 => self.eval::<Costs4>(n),
            StacksEpochId::Epoch10 => self.eval::<Costs1>(n),
        }
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
        NativeFunctions::StxBurn => ClarityCostFunction::Unimplemented,
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
    }
}
