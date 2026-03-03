use clarity::types::StacksEpochId;
use clarity::vm::ClarityVersion;
use indoc::indoc;

use crate::repl::{
    ClarityCodeSource, ClarityContract, ContractDeployer, Epoch, DEFAULT_CLARITY_VERSION,
    DEFAULT_EPOCH,
};

impl ClarityContract {
    pub fn fixture() -> Self {
        #[rustfmt::skip]
        let snippet = indoc!("
            (define-data-var x uint u0)
            (define-read-only (get-x)
              (var-get x))
            (define-public (incr)
              (let ((new-x (+ (var-get x) u1)))
                (var-set x new-x)
                (ok new-x)))
        ");

        Self {
            code_source: ClarityCodeSource::ContractInMemory(snippet.to_owned()),
            name: "contract".into(),
            deployer: ContractDeployer::DefaultDeployer,
            clarity_version: DEFAULT_CLARITY_VERSION,
            epoch: Epoch::Specific(DEFAULT_EPOCH),
            is_requirement: false,
        }
    }
}

pub struct ClarityContractBuilder {
    contract: ClarityContract,
}

impl Default for ClarityContractBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ClarityContractBuilder {
    pub fn new() -> Self {
        Self {
            contract: ClarityContract::fixture(),
        }
    }

    pub fn code_source(mut self, code_source: String) -> Self {
        self.contract.code_source = ClarityCodeSource::ContractInMemory(code_source);
        self
    }

    pub fn name(mut self, name: &str) -> Self {
        name.clone_into(&mut self.contract.name);
        self
    }

    pub fn deployer(mut self, address: &str) -> Self {
        self.contract.deployer = ContractDeployer::Address(address.to_owned());
        self
    }

    pub fn clarity_version(mut self, clarity_version: ClarityVersion) -> Self {
        self.contract.clarity_version = clarity_version;
        self
    }

    pub fn epoch(mut self, epoch: StacksEpochId) -> Self {
        self.contract.epoch = Epoch::Specific(epoch);
        self
    }

    pub fn build(self) -> ClarityContract {
        let default_version = ClarityVersion::default_for_epoch(self.contract.epoch.resolve());
        let clarity_version = self.contract.clarity_version;

        if clarity_version > default_version {
            panic!(
                "invalid clarity version {} for epoch {}",
                clarity_version,
                self.contract.epoch.resolve()
            );
        }

        self.contract
    }
}
