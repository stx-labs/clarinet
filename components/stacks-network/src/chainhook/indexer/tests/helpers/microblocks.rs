use super::BlockEvent;
use crate::chainhook::types::{
    BlockIdentifier, StacksMicroblockData, StacksMicroblockMetadata, StacksTransactionData,
};

pub fn generate_test_microblock(
    fork_id: u8,
    microblock_height: u64,
    transactions: Vec<StacksTransactionData>,
    anchor: BlockEvent,
    parent_microblock: Option<BlockEvent>,
) -> BlockEvent {
    let mut hash = vec![
        fork_id, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    let anchor = match anchor {
        BlockEvent::Block(anchor) => anchor,
        _ => unreachable!(),
    };
    let parent_block_identifier = if microblock_height == 0 {
        anchor.block_identifier.clone()
    } else {
        match parent_microblock {
            Some(BlockEvent::Microblock(parent_microblock)) => {
                assert_eq!(
                    parent_microblock.block_identifier.index,
                    microblock_height - 1
                );
                parent_microblock.block_identifier.clone()
            }
            _ => {
                let mut parent_hash = vec![
                    fork_id, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                ];
                parent_hash.append(&mut (microblock_height - 1).to_be_bytes().to_vec());

                BlockIdentifier {
                    index: microblock_height - 1,
                    hash: hex::encode(&parent_hash[..]),
                }
            }
        }
    };
    hash.append(&mut microblock_height.to_be_bytes().to_vec());
    BlockEvent::Microblock(StacksMicroblockData {
        block_identifier: BlockIdentifier {
            index: microblock_height,
            hash: hex::encode(&hash[..]),
        },
        parent_block_identifier,
        timestamp: 0,
        transactions,
        metadata: StacksMicroblockMetadata {
            anchor_block_identifier: anchor.block_identifier,
        },
    })
}

pub fn a1(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(1, 0, vec![], anchor, parent_microblock)
}

pub fn a2(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(2, 0, vec![], anchor, parent_microblock)
}

pub fn b1(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(1, 1, vec![], anchor, parent_microblock)
}

pub fn b2(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(2, 1, vec![], anchor, parent_microblock)
}

pub fn c1(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(1, 2, vec![], anchor, parent_microblock)
}

pub fn c2(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(2, 2, vec![], anchor, parent_microblock)
}

pub fn d1(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(1, 3, vec![], anchor, parent_microblock)
}

pub fn d2(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(2, 3, vec![], anchor, parent_microblock)
}

pub fn e1(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(1, 4, vec![], anchor, parent_microblock)
}

pub fn e2(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(2, 4, vec![], anchor, parent_microblock)
}

pub fn b3(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(3, 1, vec![], anchor, parent_microblock)
}

pub fn c3(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(3, 2, vec![], anchor, parent_microblock)
}

pub fn d3(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(3, 3, vec![], anchor, parent_microblock)
}

pub fn e3(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(3, 4, vec![], anchor, parent_microblock)
}

pub fn f1(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(1, 5, vec![], anchor, parent_microblock)
}

pub fn f2(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(2, 5, vec![], anchor, parent_microblock)
}

pub fn f3(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(3, 5, vec![], anchor, parent_microblock)
}

pub fn g1(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(1, 6, vec![], anchor, parent_microblock)
}

pub fn g2(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(2, 6, vec![], anchor, parent_microblock)
}

pub fn g3(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(3, 6, vec![], anchor, parent_microblock)
}

pub fn h1(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(1, 7, vec![], anchor, parent_microblock)
}

pub fn h3(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(3, 7, vec![], anchor, parent_microblock)
}

pub fn i1(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(1, 8, vec![], anchor, parent_microblock)
}

pub fn i3(anchor: BlockEvent, parent_microblock: Option<BlockEvent>) -> BlockEvent {
    generate_test_microblock(3, 8, vec![], anchor, parent_microblock)
}
