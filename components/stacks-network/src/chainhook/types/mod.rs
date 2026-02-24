pub mod bitcoin;
mod contract_interface;
mod events;
mod processors;
mod rosetta;
mod signers;

pub use contract_interface::*;
pub use events::*;
pub use processors::*;
pub use rosetta::*;
pub use signers::*;

pub const DEFAULT_STACKS_NODE_RPC: &str = "http://localhost:20443";

#[derive(Clone, Debug)]
pub enum Chain {
    Bitcoin,
    Stacks,
}
