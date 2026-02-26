use crate::chainhook::types::{StacksBlockData, StacksMicroblockData};

pub mod accounts;
#[allow(non_snake_case, unreachable_code)]
pub mod bitcoin_blocks;
pub mod bitcoin_shapes;
pub mod microblocks;
#[allow(non_snake_case, unreachable_code)]
pub mod stacks_blocks;
pub mod stacks_events;
pub mod stacks_shapes;
pub mod transactions;

pub enum BlockEvent {
    Block(Box<StacksBlockData>),
    Microblock(StacksMicroblockData),
}

impl BlockEvent {
    pub fn expect_block(self) -> StacksBlockData {
        match self {
            BlockEvent::Block(block_data) => *block_data,
            _ => panic!("expected block"),
        }
    }
}
