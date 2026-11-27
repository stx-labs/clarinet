use clarity::types::StacksEpochId;
use clarity::vm::ClarityVersion;

pub const DEFAULT_EPOCH: StacksEpochId = StacksEpochId::Epoch40;
pub const MAINNET_DEFAULT_EPOCH: StacksEpochId = DEFAULT_EPOCH;
pub const TESTNET_DEFAULT_EPOCH: StacksEpochId = StacksEpochId::Epoch40;

pub const DEFAULT_CLARITY_VERSION: ClarityVersion = ClarityVersion::Clarity6;
