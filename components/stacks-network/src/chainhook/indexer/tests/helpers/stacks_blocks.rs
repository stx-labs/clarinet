use crate::chainhook::types::{
    BlockIdentifier, StacksBlockData, StacksBlockMetadata, StacksBlockMetadataRewardSet,
    StacksBlockMetadataRewardSetSigner, StacksTransactionData,
};

use super::BlockEvent;

pub fn generate_test_stacks_block(
    fork_id: u8,
    block_height: u64,
    transactions: Vec<StacksTransactionData>,
    parent: Option<BlockEvent>,
) -> BlockEvent {
    let mut hash = vec![
        fork_id, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

    let parent_height = match block_height {
        0 => 0,
        _ => block_height - 1,
    };

    let (parent_block_identifier, confirm_microblock_identifier) = match parent {
        Some(BlockEvent::Block(parent)) => {
            assert_eq!(parent.block_identifier.index, parent_height);
            (parent.block_identifier.clone(), None)
        }
        Some(BlockEvent::Microblock(microblock_parent)) => {
            assert_eq!(
                microblock_parent.metadata.anchor_block_identifier.index,
                parent_height,
            );
            (
                microblock_parent.metadata.anchor_block_identifier.clone(),
                Some(microblock_parent.block_identifier.clone()),
            )
        }
        _ => {
            let mut parent_hash = if parent_height == 1 {
                vec![
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                ]
            } else {
                vec![
                    fork_id, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                ]
            };
            parent_hash.append(&mut parent_height.to_be_bytes().to_vec());
            (
                BlockIdentifier {
                    index: parent_height,
                    hash: hex::encode(&parent_hash[..]),
                },
                None,
            )
        }
    };
    hash.append(&mut block_height.to_be_bytes().to_vec());
    BlockEvent::Block(Box::new(StacksBlockData {
        block_identifier: BlockIdentifier {
            index: block_height,
            hash: hex::encode(&hash[..]),
        },
        parent_block_identifier,
        timestamp: 0,
        transactions,
        metadata: StacksBlockMetadata {
            bitcoin_anchor_block_identifier: BlockIdentifier {
                index: parent_height,
                hash: String::new(),
            },
            pox_cycle_index: 1,
            pox_cycle_position: block_height.try_into().unwrap(),
            pox_cycle_length: 100,
            confirm_microblock_identifier,
            stacks_block_hash: String::new(),
            block_time: Some(12345),
            tenure_height: Some(1122),
            signer_bitvec: Some("1010101010101".to_owned()),
            signer_signature: Some(vec!["1234".to_owned(), "2345".to_owned()]),
            signer_public_keys: Some(vec!["12".to_owned(), "23".to_owned()]),
            cycle_number: Some(1),
            reward_set: Some(StacksBlockMetadataRewardSet {
                pox_ustx_threshold: "50000".to_owned(),
                rewarded_addresses: vec![],
                signers: Some(vec![
                    StacksBlockMetadataRewardSetSigner {
                        signing_key: "0123".to_owned(),
                        weight: 123,
                        stacked_amt: "555555".to_owned(),
                    },
                    StacksBlockMetadataRewardSetSigner {
                        signing_key: "2345".to_owned(),
                        weight: 234,
                        stacked_amt: "6677777".to_owned(),
                    },
                ]),
            }),
        },
    }))
}

pub fn A1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(0, 1, vec![], parent)
}
pub fn A2(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 1, vec![], parent)
}

pub fn B1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 2, vec![], parent)
}

pub fn B2(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(2, 2, vec![], parent)
}

pub fn C1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 3, vec![], parent)
}

pub fn C2(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(2, 3, vec![], parent)
}

pub fn D1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 4, vec![], parent)
}

pub fn D2(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(2, 4, vec![], parent)
}

pub fn E1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 5, vec![], parent)
}

pub fn E2(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(2, 5, vec![], parent)
}

pub fn B3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 2, vec![], parent)
}

pub fn C3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 3, vec![], parent)
}

pub fn D3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 4, vec![], parent)
}

pub fn E3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 5, vec![], parent)
}

pub fn F1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 6, vec![], parent)
}

pub fn F2(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(2, 6, vec![], parent)
}

pub fn F3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 6, vec![], parent)
}

pub fn G1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 7, vec![], parent)
}

pub fn G2(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(2, 7, vec![], parent)
}

pub fn G3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 7, vec![], parent)
}

pub fn H1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 8, vec![], parent)
}

pub fn H2(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(2, 8, vec![], parent)
}

pub fn H3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 8, vec![], parent)
}

pub fn I1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 9, vec![], parent)
}

pub fn I2(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(2, 9, vec![], parent)
}

pub fn I3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 9, vec![], parent)
}

pub fn J1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 10, vec![], parent)
}

pub fn J2(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(2, 10, vec![], parent)
}

pub fn J3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 10, vec![], parent)
}

pub fn K1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 11, vec![], parent)
}

pub fn K3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 11, vec![], parent)
}

pub fn L1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 12, vec![], parent)
}

pub fn L3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 12, vec![], parent)
}

pub fn M1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 13, vec![], parent)
}

pub fn M3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 13, vec![], parent)
}

pub fn N1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 14, vec![], parent)
}

pub fn N3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 14, vec![], parent)
}

pub fn O1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 15, vec![], parent)
}

pub fn O3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 15, vec![], parent)
}

pub fn P1(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(1, 16, vec![], parent)
}

pub fn P3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 16, vec![], parent)
}

pub fn Q3(parent: Option<BlockEvent>) -> BlockEvent {
    generate_test_stacks_block(3, 17, vec![], parent)
}
