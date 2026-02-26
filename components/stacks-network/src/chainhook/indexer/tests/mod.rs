pub mod helpers;
use std::thread::sleep;
use std::time::Duration;

use self::helpers::BlockEvent;
use super::fork_scratch_pad::ForkScratchPad;
use super::StacksBlockPool;
use crate::chainhook::types::{
    BitcoinBlockData, BlockchainEvent, StacksBlockData, StacksChainEvent,
};
use crate::chainhook::utils::{AbstractBlock, Context};

pub type StacksChainEventExpectation = Box<dyn Fn(Option<StacksChainEvent>)>;

pub fn process_stacks_blocks_and_check_expectations(
    (steps, block_pool_seed): (
        Vec<(BlockEvent, StacksChainEventExpectation)>,
        Option<Vec<StacksBlockData>>,
    ),
) {
    let logger = hiro_system_kit::log::setup_logger();
    let _guard = hiro_system_kit::log::setup_global_logger(logger.clone());
    let ctx = Context::empty();
    let mut blocks_processor = StacksBlockPool::new();

    if let Some(block_pool_seed) = block_pool_seed {
        blocks_processor.seed_block_pool(block_pool_seed, &ctx);
    }

    for (block_event, check_chain_event_expectations) in steps.into_iter() {
        sleep(Duration::new(0, 200_000_000));
        match block_event {
            BlockEvent::Block(block) => {
                let chain_event = blocks_processor.process_block(*block, &ctx).unwrap();
                check_chain_event_expectations(chain_event);
            }
            BlockEvent::Microblock(microblock) => {
                let chain_event = blocks_processor
                    .process_microblocks(vec![microblock], &ctx)
                    .unwrap();
                check_chain_event_expectations(chain_event);
            }
        }
    }
}

pub type BlockchainEventExpectation = Box<dyn Fn(Option<BlockchainEvent>)>;

pub fn process_bitcoin_blocks_and_check_expectations(
    steps: Vec<(BitcoinBlockData, BlockchainEventExpectation)>,
) {
    let mut blocks_processor = ForkScratchPad::new();
    for (block, check_chain_event_expectations) in steps.into_iter() {
        let chain_event = blocks_processor
            .process_header(block.get_header(), &Context::empty())
            .unwrap();
        check_chain_event_expectations(chain_event);
    }
}
