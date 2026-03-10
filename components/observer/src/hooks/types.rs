use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct PoxConfig {
    pub first_burnchain_block_height: u64,
    pub prepare_phase_len: u64,
    pub reward_phase_len: u64,
    pub rewarded_addresses_per_block: usize,
}

impl PoxConfig {
    pub fn mainnet_default() -> PoxConfig {
        PoxConfig {
            first_burnchain_block_height: 666050,
            prepare_phase_len: 100,
            reward_phase_len: 2000,
            rewarded_addresses_per_block: 2,
        }
    }

    pub fn testnet_default() -> PoxConfig {
        PoxConfig {
            first_burnchain_block_height: 2000000,
            prepare_phase_len: 50,
            reward_phase_len: 1000,
            rewarded_addresses_per_block: 2,
        }
    }

    pub fn devnet_default() -> PoxConfig {
        Self::default()
    }

    pub fn get_pox_cycle_len(&self) -> u64 {
        self.prepare_phase_len + self.reward_phase_len
    }

    pub fn get_pox_cycle_id(&self, block_height: u64) -> u64 {
        (block_height.saturating_sub(self.first_burnchain_block_height)) / self.get_pox_cycle_len()
    }

    pub fn get_pos_in_pox_cycle(&self, block_height: u64) -> u64 {
        (block_height.saturating_sub(self.first_burnchain_block_height)) % self.get_pox_cycle_len()
    }

    pub fn get_burn_address(&self) -> &str {
        match self.first_burnchain_block_height {
            666050 => "1111111111111111111114oLvT2",
            2000000 => "burn-address-regtest",
            _ => "burn-address",
        }
    }
}

impl Default for PoxConfig {
    fn default() -> PoxConfig {
        PoxConfig {
            first_burnchain_block_height: 100,
            prepare_phase_len: 5,
            reward_phase_len: 15,
            rewarded_addresses_per_block: 2,
        }
    }
}
