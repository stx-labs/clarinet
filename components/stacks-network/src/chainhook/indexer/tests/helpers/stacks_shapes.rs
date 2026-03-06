use hiro_system_kit::slog;

use super::super::StacksChainEventExpectation;
use super::{microblocks, stacks_blocks, BlockEvent};
use crate::chainhook::types::{StacksBlockData, StacksChainEvent};
use crate::chainhook::utils::Context;

pub fn expect_no_chain_update() -> StacksChainEventExpectation {
    Box::new(move |chain_event_to_check: Option<StacksChainEvent>| {
        assert!(
            chain_event_to_check.is_none(),
            "expected no Chain update, got {:?}",
            chain_event_to_check
        );
    })
}

pub fn expect_chain_updated_with_block(
    expected_block: BlockEvent,
    confirmed_blocks: Vec<BlockEvent>,
) -> StacksChainEventExpectation {
    expect_chain_updated_with_blocks(vec![expected_block], confirmed_blocks)
}

pub fn expect_chain_updated_with_microblock(
    expected_microblock: BlockEvent,
) -> StacksChainEventExpectation {
    expect_chain_updated_with_microblocks(vec![expected_microblock])
}

pub fn expect_chain_updated_with_microblocks(
    expected_microblocks: Vec<BlockEvent>,
) -> StacksChainEventExpectation {
    let ctx = Context::empty();
    Box::new(move |chain_event_to_check: Option<StacksChainEvent>| {
        assert!(
            match chain_event_to_check {
                Some(StacksChainEvent::ChainUpdatedWithMicroblocks(ref event)) => {
                    assert_eq!(expected_microblocks.len(), event.new_microblocks.len());
                    for (expected_microblock, new_microblock) in
                        expected_microblocks.iter().zip(&event.new_microblocks)
                    {
                        let expected_microblock = match expected_microblock {
                            BlockEvent::Microblock(expected_microblock) => expected_microblock,
                            _ => unreachable!(),
                        };
                        ctx.try_log(|logger| {
                            slog::debug!(
                                logger,
                                "Checking {} and {}",
                                expected_microblock.block_identifier,
                                new_microblock.block_identifier
                            )
                        });
                        assert!(
                            new_microblock
                                .block_identifier
                                .eq(&expected_microblock.block_identifier),
                            "{} ≠ {}",
                            new_microblock.block_identifier,
                            expected_microblock.block_identifier
                        );
                    }
                    true
                }
                _ => false,
            },
            "expected ChainUpdatedWithMicroblocks, got {:?}",
            chain_event_to_check
        );
    })
}

pub fn expect_chain_updated_with_blocks(
    expected_blocks: Vec<BlockEvent>,
    confirmed_blocks: Vec<BlockEvent>,
) -> StacksChainEventExpectation {
    let ctx = Context::empty();
    Box::new(move |chain_event_to_check: Option<StacksChainEvent>| {
        assert!(
            match chain_event_to_check {
                Some(StacksChainEvent::ChainUpdatedWithBlocks(ref event)) => {
                    assert_eq!(expected_blocks.len(), event.new_blocks.len());
                    for (expected_block, new_block) in expected_blocks.iter().zip(&event.new_blocks)
                    {
                        let expected_block = match expected_block {
                            BlockEvent::Block(expected_block) => expected_block,
                            _ => unreachable!(),
                        };
                        ctx.try_log(|logger| {
                            slog::debug!(
                                logger,
                                "Checking {} and {}",
                                expected_block.block_identifier,
                                new_block.block.block_identifier
                            )
                        });
                        assert!(
                            new_block
                                .block
                                .block_identifier
                                .eq(&expected_block.block_identifier),
                            "{} ≠ {}",
                            new_block.block.block_identifier,
                            expected_block.block_identifier
                        );
                    }
                    assert_eq!(confirmed_blocks.len(), event.confirmed_blocks.len());
                    for (expected_confirmed_block, confirmed_block) in
                        confirmed_blocks.iter().zip(&event.confirmed_blocks)
                    {
                        let expected_block = match expected_confirmed_block {
                            BlockEvent::Block(block) => block,
                            _ => unreachable!(),
                        };
                        ctx.try_log(|logger| {
                            slog::debug!(
                                logger,
                                "Checking {} and {}",
                                expected_block.block_identifier,
                                confirmed_block.block_identifier
                            )
                        });
                        assert!(
                            confirmed_block
                                .block_identifier
                                .eq(&expected_block.block_identifier),
                            "{} ≠ {}",
                            confirmed_block.block_identifier,
                            expected_block.block_identifier
                        );
                    }

                    true
                }
                _ => false,
            },
            "expected ChainUpdatedWithBlocks, got {:?}",
            chain_event_to_check
        );
    })
}

pub fn expect_chain_updated_with_block_and_microblock_updates(
    expected_block: BlockEvent,
    mut microblocks_to_rollback: Vec<BlockEvent>,
    microblocks_to_apply: Vec<BlockEvent>,
    _confirmed_blocks: Vec<BlockEvent>,
) -> StacksChainEventExpectation {
    microblocks_to_rollback.reverse();
    Box::new(move |chain_event_to_check: Option<StacksChainEvent>| {
        assert!(
            match chain_event_to_check {
                Some(StacksChainEvent::ChainUpdatedWithBlocks(ref event)) => {
                    assert_eq!(event.new_blocks.len(), 1);
                    assert_eq!(
                        microblocks_to_rollback.len(),
                        event.new_blocks[0].parent_microblocks_to_rollback.len()
                    );
                    assert_eq!(
                        microblocks_to_apply.len(),
                        event.new_blocks[0].parent_microblocks_to_apply.len()
                    );
                    let expected_block = match expected_block {
                        BlockEvent::Block(ref expected_block) => expected_block,
                        _ => unreachable!(),
                    };

                    assert!(
                        event.new_blocks[0]
                            .block
                            .block_identifier
                            .eq(&expected_block.block_identifier),
                        "{} ≠ {}",
                        event.new_blocks[0].block.block_identifier,
                        expected_block.block_identifier
                    );
                    let expected_microblock_id = event.new_blocks[0]
                        .block
                        .metadata
                        .confirm_microblock_identifier
                        .as_ref()
                        .unwrap();
                    let microblock_id = expected_block
                        .metadata
                        .confirm_microblock_identifier
                        .as_ref()
                        .unwrap();
                    assert!(
                        &expected_microblock_id.eq(microblock_id),
                        "{} ≠ {}",
                        expected_microblock_id,
                        microblock_id
                    );

                    for (expected, microblock) in microblocks_to_rollback
                        .iter()
                        .zip(&event.new_blocks[0].parent_microblocks_to_rollback)
                    {
                        let expected = match expected {
                            BlockEvent::Microblock(expected) => expected,
                            _ => unreachable!(),
                        };
                        assert!(
                            expected.block_identifier.eq(&microblock.block_identifier),
                            "{} ≠ {}",
                            expected.block_identifier,
                            microblock.block_identifier
                        );
                    }
                    for (expected, microblock) in microblocks_to_apply
                        .iter()
                        .zip(&event.new_blocks[0].parent_microblocks_to_apply)
                    {
                        let expected = match expected {
                            BlockEvent::Microblock(expected) => expected,
                            _ => unreachable!(),
                        };
                        assert!(
                            expected.block_identifier.eq(&microblock.block_identifier),
                            "{} ≠ {}",
                            expected.block_identifier,
                            microblock.block_identifier
                        );
                    }
                    true
                }
                _ => false,
            },
            "expected ChainUpdatedWithBlocks, got {:?}",
            chain_event_to_check
        );
    })
}

pub fn expect_chain_updated_with_block_reorg_and_microblock_updates(
    previous_block: BlockEvent,
    new_block: BlockEvent,
    microblocks_to_rollback: Vec<BlockEvent>,
    microblocks_to_apply: Vec<BlockEvent>,
    _confirmed_blocks: Vec<BlockEvent>,
) -> StacksChainEventExpectation {
    Box::new(move |chain_event_to_check: Option<StacksChainEvent>| {
        assert!(
            match chain_event_to_check {
                Some(StacksChainEvent::ChainUpdatedWithReorg(ref event)) => {
                    assert_eq!(event.blocks_to_apply.len(), 1);
                    assert_eq!(event.blocks_to_rollback.len(), 1);
                    assert_eq!(
                        microblocks_to_rollback.len(),
                        event.blocks_to_rollback[0]
                            .parent_microblocks_to_rollback
                            .len()
                    );
                    assert_eq!(
                        microblocks_to_apply.len(),
                        event.blocks_to_apply[0].parent_microblocks_to_apply.len()
                    );
                    let previous_block = match previous_block {
                        BlockEvent::Block(ref previous_block) => previous_block,
                        _ => unreachable!(),
                    };
                    let new_block = match new_block {
                        BlockEvent::Block(ref new_block) => new_block,
                        _ => unreachable!(),
                    };
                    assert!(
                        event.blocks_to_apply[0]
                            .block
                            .block_identifier
                            .eq(&new_block.block_identifier),
                        "{} ≠ {}",
                        event.blocks_to_apply[0].block.block_identifier,
                        new_block.block_identifier
                    );
                    assert!(
                        event.blocks_to_rollback[0]
                            .block
                            .block_identifier
                            .eq(&previous_block.block_identifier),
                        "{} ≠ {}",
                        event.blocks_to_rollback[0].block.block_identifier,
                        previous_block.block_identifier
                    );

                    let expected_microblock_id = event.blocks_to_apply[0]
                        .block
                        .metadata
                        .confirm_microblock_identifier
                        .as_ref()
                        .unwrap();
                    let microblock_id = new_block
                        .metadata
                        .confirm_microblock_identifier
                        .as_ref()
                        .unwrap();
                    assert!(
                        &expected_microblock_id.eq(microblock_id),
                        "{} ≠ {}",
                        expected_microblock_id,
                        microblock_id
                    );

                    let expected_microblock_id = event.blocks_to_rollback[0]
                        .block
                        .metadata
                        .confirm_microblock_identifier
                        .as_ref()
                        .unwrap();
                    let microblock_id = previous_block
                        .metadata
                        .confirm_microblock_identifier
                        .as_ref()
                        .unwrap();
                    assert!(
                        &expected_microblock_id.eq(microblock_id),
                        "{} ≠ {}",
                        expected_microblock_id,
                        microblock_id
                    );

                    for (expected, microblock) in microblocks_to_rollback
                        .iter()
                        .zip(&event.blocks_to_rollback[0].parent_microblocks_to_rollback)
                    {
                        let expected = match expected {
                            BlockEvent::Microblock(expected) => expected,
                            _ => unreachable!(),
                        };
                        assert!(
                            expected.block_identifier.eq(&microblock.block_identifier),
                            "{} ≠ {}",
                            expected.block_identifier,
                            microblock.block_identifier
                        );
                    }
                    for (expected, microblock) in microblocks_to_apply
                        .iter()
                        .zip(&event.blocks_to_apply[0].parent_microblocks_to_apply)
                    {
                        let expected = match expected {
                            BlockEvent::Microblock(expected) => expected,
                            _ => unreachable!(),
                        };
                        assert!(
                            expected.block_identifier.eq(&microblock.block_identifier),
                            "{} ≠ {}",
                            expected.block_identifier,
                            microblock.block_identifier
                        );
                    }
                    true
                }
                _ => false,
            },
            "expected ChainUpdatedWithBlocks, got {:?}",
            chain_event_to_check
        );
    })
}

pub fn expect_chain_updated_with_block_reorg(
    mut blocks_to_rollback: Vec<BlockEvent>,
    blocks_to_apply: Vec<BlockEvent>,
    _confirmed_blocks: Vec<BlockEvent>,
) -> StacksChainEventExpectation {
    blocks_to_rollback.reverse();
    Box::new(move |chain_event_to_check: Option<StacksChainEvent>| {
        assert!(
            match chain_event_to_check {
                Some(StacksChainEvent::ChainUpdatedWithReorg(ref event)) => {
                    assert_eq!(blocks_to_rollback.len(), event.blocks_to_rollback.len());
                    assert_eq!(blocks_to_apply.len(), event.blocks_to_apply.len());
                    for (expected, block_update) in
                        blocks_to_rollback.iter().zip(&event.blocks_to_rollback)
                    {
                        let expected = match expected {
                            BlockEvent::Block(expected) => expected,
                            _ => unreachable!(),
                        };
                        assert!(
                            expected
                                .block_identifier
                                .eq(&block_update.block.block_identifier),
                            "{} ≠ {}",
                            expected.block_identifier,
                            block_update.block.block_identifier
                        );
                    }
                    for (expected, block_update) in
                        blocks_to_apply.iter().zip(&event.blocks_to_apply)
                    {
                        let expected = match expected {
                            BlockEvent::Block(expected) => expected,
                            _ => unreachable!(),
                        };
                        assert!(
                            expected
                                .block_identifier
                                .eq(&block_update.block.block_identifier),
                            "{} ≠ {}",
                            expected.block_identifier,
                            block_update.block.block_identifier
                        );
                    }
                    true
                }
                _ => false,
            },
            "expected ChainUpdatedWithReorg, got {:?}",
            chain_event_to_check
        );
    })
}

pub fn expect_chain_updated_with_microblock_reorg(
    mut microblocks_to_rollback: Vec<BlockEvent>,
    microblocks_to_apply: Vec<BlockEvent>,
) -> StacksChainEventExpectation {
    microblocks_to_rollback.reverse();
    Box::new(move |chain_event_to_check: Option<StacksChainEvent>| {
        assert!(
            match chain_event_to_check {
                Some(StacksChainEvent::ChainUpdatedWithMicroblocksReorg(ref event)) => {
                    assert_eq!(
                        microblocks_to_rollback.len(),
                        event.microblocks_to_rollback.len()
                    );
                    assert_eq!(microblocks_to_apply.len(), event.microblocks_to_apply.len());
                    for (expected, microblock) in microblocks_to_rollback
                        .iter()
                        .zip(&event.microblocks_to_rollback)
                    {
                        let expected = match expected {
                            BlockEvent::Microblock(expected) => expected,
                            _ => unreachable!(),
                        };
                        assert!(
                            expected.block_identifier.eq(&microblock.block_identifier),
                            "{} ≠ {}",
                            expected.block_identifier,
                            microblock.block_identifier
                        );
                    }
                    for (expected, microblock) in
                        microblocks_to_apply.iter().zip(&event.microblocks_to_apply)
                    {
                        let expected = match expected {
                            BlockEvent::Microblock(expected) => expected,
                            _ => unreachable!(),
                        };
                        assert!(
                            expected.block_identifier.eq(&microblock.block_identifier),
                            "{} ≠ {}",
                            expected.block_identifier,
                            microblock.block_identifier
                        );
                    }
                    true
                }
                _ => false,
            },
            "expected ChainUpdatedWithMicroblocksReorg, got {:?}",
            chain_event_to_check
        );
    })
}

// Test vectors:
// 001 to 020: Stacks anchored blocks received in order
// 021 to 040: Stacks anchored blocks received out of order
// 041 to 060: Stacks anchored blocks received in order + microblocks received in order
// 061 to 080: Stacks anchored blocks received in order + microblocks received out of order
// 081 to 100: Stacks anchored blocks received out of order + microblocks received out of order

/// Vector 001: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)
///
pub fn get_vector_001() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
    ]
}

/// Vector 002: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(4)
///        \  B2(3)  -  C2(5)
///
pub fn get_vector_002() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None)],
                vec![stacks_blocks::B2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None)],
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
    ]
}

/// Vector 003: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_003() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
    ]
}

/// Vector 004: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_004() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 005: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_005() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
    ]
}

/// Vector 006: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_006() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
    ]
}

/// Vector 007: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(9) - H1(10) - I1(11) - J1(12)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_007() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![]),
        ),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![]),
        ),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(
                stacks_blocks::J1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
            ),
        ),
    ]
}

/// Vector 008: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(9)  -  H1(10) - I1(11)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_008() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![]),
        ),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![]),
        ),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(
                stacks_blocks::J1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
            ),
        ),
    ]
}

/// Vector 009: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(9)  -  H1(10)  -  I1(11)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_009() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![]),
        ),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![]),
        ),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(
                stacks_blocks::J1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
            ),
        ),
    ]
}

/// Vector 010: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(9)  -  H1(10) -  I1(11)
///        \  B2(4)  -  C2(5)  -  D2(12)
///
pub fn get_vector_010() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![]),
        ),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![]),
        ),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(
                stacks_blocks::J1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
            ),
        ),
    ]
}

/// Vector 011: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)  -  H1(11)  -  I1(12)
///       \                               \ E3(9)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_011() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (stacks_blocks::E3(None), expect_no_chain_update()),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![]),
        ),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![]),
        ),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(
                stacks_blocks::J1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
            ),
        ),
    ]
}

/// Vector 012: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)  -  H1(12)  -  I1(13)
///       \                               \ E3(9)  -  F3(11)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_012() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (stacks_blocks::E3(None), expect_no_chain_update()),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![]),
        ),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![]),
        ),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(stacks_blocks::J1(None), vec![]),
        ),
        (
            stacks_blocks::K1(None),
            expect_chain_updated_with_block(
                stacks_blocks::K1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                ],
            ),
        ),
    ]
}

/// Vector 013: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)  -  H1(12)  -  I1(14)
///       \                               \ E3(9)  -  F3(11) -  G3(13)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_013() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (stacks_blocks::E3(None), expect_no_chain_update()),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![]),
        ),
        (stacks_blocks::G3(None), expect_no_chain_update()),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![]),
        ),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(
                stacks_blocks::J1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
            ),
        ),
    ]
}

/// Vector 014: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)  -  H1(12)  -  I1(14)
///       \                               \ E3(9)  -  F3(11) -  G3(13)  -  H3(15)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_014() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (stacks_blocks::E3(None), expect_no_chain_update()),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![]),
        ),
        (stacks_blocks::G3(None), expect_no_chain_update()),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![]),
        ),
        (stacks_blocks::H3(None), expect_no_chain_update()),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(
                stacks_blocks::J1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
            ),
        ),
    ]
}

/// Vector 015: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)  -  H1(12)  -  I1(14)
///       \                               \ E3(9)  -  F3(11) -  G3(13)  -  H3(15)  -  I3(16)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_015() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::E3(Some(stacks_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![]),
        ),
        (stacks_blocks::G3(None), expect_no_chain_update()),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![]),
        ),
        (stacks_blocks::H3(None), expect_no_chain_update()),
        (
            stacks_blocks::I3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                    stacks_blocks::H1(None),
                    stacks_blocks::I1(None),
                ],
                vec![
                    stacks_blocks::E3(Some(stacks_blocks::D1(None))),
                    stacks_blocks::F3(None),
                    stacks_blocks::G3(None),
                    stacks_blocks::H3(None),
                    stacks_blocks::I3(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::J3(None),
            expect_chain_updated_with_block(stacks_blocks::J3(None), vec![]),
        ),
        (
            stacks_blocks::K3(None),
            expect_chain_updated_with_block(stacks_blocks::K3(None), vec![]),
        ),
        (
            stacks_blocks::L3(None),
            expect_chain_updated_with_block(stacks_blocks::L3(None), vec![]),
        ),
        (
            stacks_blocks::M3(None),
            expect_chain_updated_with_block(stacks_blocks::M3(None), vec![]),
        ),
        (
            stacks_blocks::N3(None),
            expect_chain_updated_with_block(stacks_blocks::N3(None), vec![]),
        ),
        (
            stacks_blocks::O3(None),
            expect_chain_updated_with_block(stacks_blocks::O3(None), vec![]),
        ),
        (
            stacks_blocks::P3(None),
            expect_chain_updated_with_block(
                stacks_blocks::P3(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E3(None),
                    stacks_blocks::F3(None),
                    stacks_blocks::G3(None),
                    stacks_blocks::H3(None),
                    stacks_blocks::I3(None),
                    stacks_blocks::J3(None),
                ],
            ),
        ),
    ]
}

/// Vector 016: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)  -  H1(12)  -  I1(14)
///       \                               \ E3(9)  -  F3(11) -  G3(13)  -  H3(15)  -  I3(16) - J3(18) - K3(19) - L3(20) - M3(21) - N3(22) - O3(23) - P3(24)
///        \  B2(4)  -  C2(5)  -  D2(17)
///
pub fn get_vector_016() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::E3(Some(stacks_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![]),
        ),
        (stacks_blocks::G3(None), expect_no_chain_update()),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![]),
        ),
        (stacks_blocks::H3(None), expect_no_chain_update()),
        (
            stacks_blocks::I3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                    stacks_blocks::H1(None),
                    stacks_blocks::I1(None),
                ],
                vec![
                    stacks_blocks::E3(Some(stacks_blocks::D1(None))),
                    stacks_blocks::F3(None),
                    stacks_blocks::G3(None),
                    stacks_blocks::H3(None),
                    stacks_blocks::I3(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (
            stacks_blocks::J3(None),
            expect_chain_updated_with_block(stacks_blocks::J3(None), vec![]),
        ),
        (
            stacks_blocks::K3(None),
            expect_chain_updated_with_block(stacks_blocks::K3(None), vec![]),
        ),
        (
            stacks_blocks::L3(None),
            expect_chain_updated_with_block(stacks_blocks::L3(None), vec![]),
        ),
        (
            stacks_blocks::M3(None),
            expect_chain_updated_with_block(stacks_blocks::M3(None), vec![]),
        ),
        (
            stacks_blocks::N3(None),
            expect_chain_updated_with_block(stacks_blocks::N3(None), vec![]),
        ),
        (
            stacks_blocks::O3(None),
            expect_chain_updated_with_block(stacks_blocks::O3(None), vec![]),
        ),
        (
            stacks_blocks::P3(None),
            expect_chain_updated_with_block(
                stacks_blocks::P3(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E3(None),
                    stacks_blocks::F3(None),
                    stacks_blocks::G3(None),
                    stacks_blocks::H3(None),
                    stacks_blocks::I3(None),
                    stacks_blocks::J3(None),
                ],
            ),
        ),
    ]
}

/// Vector 017: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)
///       \                               \ E3(9)  -  F3(11) -  G3(12)
///        \  B2(4)  -  C2(5)  -  D2(13) -  E2(14)  - F2(15)  - G2(16)
///
pub fn get_vector_017() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::E3(Some(stacks_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![]),
        ),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (
            stacks_blocks::G3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                ],
                vec![
                    stacks_blocks::E3(Some(stacks_blocks::D1(None))),
                    stacks_blocks::F3(None),
                    stacks_blocks::G3(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (stacks_blocks::E2(None), expect_no_chain_update()),
        (stacks_blocks::F2(None), expect_no_chain_update()),
        (stacks_blocks::G2(None), expect_no_chain_update()),
    ]
}

/// Vector 018: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)
///       \                               \ E3(9)  -  F3(10)
///        \  B2(4)  -  C2(5)  -  D2(11) -  E2(12) -  F2(13)  - G2(14)
///
pub fn get_vector_018() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::E3(Some(stacks_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            stacks_blocks::F3(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::E1(None), stacks_blocks::F1(None)],
                vec![
                    stacks_blocks::E3(Some(stacks_blocks::D1(None))),
                    stacks_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (stacks_blocks::E2(None), expect_no_chain_update()),
        (stacks_blocks::F2(None), expect_no_chain_update()),
        (
            stacks_blocks::G2(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E3(Some(stacks_blocks::D1(None))),
                    stacks_blocks::F3(None),
                ],
                vec![
                    stacks_blocks::B2(None),
                    stacks_blocks::C2(None),
                    stacks_blocks::D2(None),
                    stacks_blocks::E2(None),
                    stacks_blocks::F2(None),
                    stacks_blocks::G2(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 019: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  - G1(15)
///       \                               \ E3(9)  -  F3(10)
///        \  B2(4)  -  C2(5)  -  D2(11) -  E2(12) -  F2(13) - G2(14)
///
pub fn get_vector_019() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::E3(Some(stacks_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            stacks_blocks::F3(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::E1(None), stacks_blocks::F1(None)],
                vec![
                    stacks_blocks::E3(Some(stacks_blocks::D1(None))),
                    stacks_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (stacks_blocks::E2(None), expect_no_chain_update()),
        (stacks_blocks::F2(None), expect_no_chain_update()),
        (
            stacks_blocks::G2(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E3(Some(stacks_blocks::D1(None))),
                    stacks_blocks::F3(None),
                ],
                vec![
                    stacks_blocks::B2(None),
                    stacks_blocks::C2(None),
                    stacks_blocks::D2(None),
                    stacks_blocks::E2(None),
                    stacks_blocks::F2(None),
                    stacks_blocks::G2(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::G1(None), expect_no_chain_update()),
    ]
}

/// Vector 020: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  - G1(15)
///       \                               \ E3(9)  -  F3(10) - G3(16)
///        \  B2(4)  -  C2(5)  -  D2(11) -  E2(12) -  F2(13) - G2(14)
///
pub fn get_vector_020() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::B2(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::E3(Some(stacks_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            stacks_blocks::F3(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::E1(None), stacks_blocks::F1(None)],
                vec![
                    stacks_blocks::E3(Some(stacks_blocks::D1(None))),
                    stacks_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (stacks_blocks::E2(None), expect_no_chain_update()),
        (stacks_blocks::F2(None), expect_no_chain_update()),
        (
            stacks_blocks::G2(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E3(Some(stacks_blocks::D1(None))),
                    stacks_blocks::F3(None),
                ],
                vec![
                    stacks_blocks::B2(None),
                    stacks_blocks::C2(None),
                    stacks_blocks::D2(None),
                    stacks_blocks::E2(None),
                    stacks_blocks::F2(None),
                    stacks_blocks::G2(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (
            stacks_blocks::G3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::B2(None),
                    stacks_blocks::C2(None),
                    stacks_blocks::D2(None),
                    stacks_blocks::E2(None),
                    stacks_blocks::F2(None),
                    stacks_blocks::G2(None),
                ],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E3(Some(stacks_blocks::D1(None))),
                    stacks_blocks::F3(None),
                    stacks_blocks::G3(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 021: Generate the following blocks
///
/// A1(1)  -  B1(3)  -  C1(2)
///
pub fn get_vector_021() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::C1(None), expect_no_chain_update()),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_blocks(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![],
            ),
        ),
    ]
}

/// Vector 022: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)
///        \  B2(5)  -  C2(4)
///
pub fn get_vector_022() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
    ]
}

/// Vector 023: Generate the following blocks
///
/// A1(1)  -  B1(5)  -  C1(3)
///        \  B2(2)  -  C2(4)
///
pub fn get_vector_023() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block(stacks_blocks::B2(None), vec![]),
        ),
        (stacks_blocks::C1(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block(stacks_blocks::C2(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![],
            ),
        ),
    ]
}

/// Vector 024: Generate the following blocks
///
/// A1(1)  -  B1(5)  -  C1(4)  -  D1(6)
///        \  B2(2)  -  C2(3)
///
pub fn get_vector_024() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block(stacks_blocks::B2(None), vec![]),
        ),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block(stacks_blocks::C2(None), vec![]),
        ),
        (stacks_blocks::C1(None), expect_no_chain_update()),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block(stacks_blocks::D1(None), vec![]),
        ),
    ]
}

/// Vector 025: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(4)  -  D1(5)  -  E1(6)
///        \  B2(3)  -  C2(7)
///
pub fn get_vector_025() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None)],
                vec![stacks_blocks::B2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None)],
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block(stacks_blocks::D1(None), vec![]),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (stacks_blocks::C2(None), expect_no_chain_update()),
    ]
}

/// Vector 026: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(8)  -  E1(7)  -  F1(6)
///        \  B2(5)  -  C2(4)
///
pub fn get_vector_026() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (stacks_blocks::F1(None), expect_no_chain_update()),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 027: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(4)  -  D1(9)  -  E1(8)  -  F1(7)  -  G1(6)
///        \  B2(5)  -  C2(3)
///
pub fn get_vector_027() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (stacks_blocks::F1(None), expect_no_chain_update()),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 028: Generate the following blocks
///
/// A1(1)  -  B1(8)  -  C1(10)  -  D1(3)  -  E1(6)  -  F1(2)  -  G1(5)  -  H1(4)
///        \  B2(7)  -  C2(9)
///
pub fn get_vector_028() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::F1(None), expect_no_chain_update()),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (stacks_blocks::H1(None), expect_no_chain_update()),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block(stacks_blocks::B2(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None)],
                vec![stacks_blocks::B1(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                    stacks_blocks::H1(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 029: Generate the following blocks
///
/// A1(1)  -  B1(7)  -  C1(6)  -  D1(9)  -  E1(10)  -  F1(2)  -  G1(3)  -  H1(4)  -  I1(11) - J1(12) - K1(13)
///        \  B2(8)  -  C2(5)
///
pub fn get_vector_029() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::F1(None), expect_no_chain_update()),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (stacks_blocks::H1(None), expect_no_chain_update()),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (stacks_blocks::C1(None), expect_no_chain_update()),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_blocks(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_blocks(
                vec![
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                    stacks_blocks::H1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::I1(None)], vec![]),
        ),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(
                stacks_blocks::J1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
            ),
        ),
        (
            stacks_blocks::K1(None),
            expect_chain_updated_with_block(stacks_blocks::K1(None), vec![stacks_blocks::E1(None)]),
        ),
        (
            stacks_blocks::L1(None),
            expect_chain_updated_with_block(stacks_blocks::L1(None), vec![stacks_blocks::F1(None)]),
        ),
    ]
}

/// Vector 030: Generate the following blocks
///
/// A1(1)  -  B1(9)  -  C1(8)  -  D1(7)  -  E1(6)  -  F1(5)  -  G1(4)  -  H1(3)  -  I1(2)
///        \  B2(11)  -  C2(10)
///
pub fn get_vector_030() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::I1(None), expect_no_chain_update()),
        (stacks_blocks::H1(None), expect_no_chain_update()),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (stacks_blocks::F1(None), expect_no_chain_update()),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (stacks_blocks::C1(None), expect_no_chain_update()),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_blocks(
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                    stacks_blocks::H1(None),
                    stacks_blocks::I1(None),
                ],
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                ],
            ),
        ),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (stacks_blocks::B2(None), expect_no_chain_update()),
    ]
}

/// Vector 031: Generate the following blocks
///
/// A1(1)  -  B1(8)  -  C1(7)  -  D1(6)  -  E1(4)  -  F1(9)  -  G1(11)  -  H1(12)  -  I1(10) -  J1(11) -  K1(12)
///       \                               \ E3(2)
///        \  B2(3)  -  C2(5)
///
pub fn get_vector_031() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::E3(None), expect_no_chain_update()),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::B2(None)], vec![]),
        ),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::C2(None)], vec![]),
        ),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (stacks_blocks::C1(None), expect_no_chain_update()),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::F1(None)], vec![]),
        ),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (stacks_blocks::I1(None), expect_no_chain_update()),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::G1(None)], vec![]),
        ),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_blocks(
                vec![stacks_blocks::H1(None), stacks_blocks::I1(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(
                stacks_blocks::J1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
            ),
        ),
        (
            stacks_blocks::K1(None),
            expect_chain_updated_with_block(stacks_blocks::K1(None), vec![stacks_blocks::E1(None)]),
        ),
    ]
}

/// Vector 032: Generate the following blocks
///
/// A1(1)  -  B1(3)  -  C1(5)  -  D1(2)  -  E1(8)  -  F1(10)  -  G1(13)  -  H1(12)  -  I1(11)
///       \                     \ D3(7)  -  E3(9)
///        \  B2(4)  -  C2(6)
///
pub fn get_vector_032() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::B1(None)], vec![]),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None)],
                vec![stacks_blocks::B2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (
            stacks_blocks::D3(Some(stacks_blocks::C1(None))),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::D1(None)],
                vec![stacks_blocks::D3(Some(stacks_blocks::C1(None)))],
                vec![],
            ),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::D3(Some(stacks_blocks::C1(None)))],
                vec![stacks_blocks::D1(None), stacks_blocks::E1(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::E3(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::D1(None), stacks_blocks::E1(None)],
                vec![
                    stacks_blocks::D3(Some(stacks_blocks::C1(None))),
                    stacks_blocks::E3(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::D3(Some(stacks_blocks::C1(None))),
                    stacks_blocks::E3(None),
                ],
                vec![
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::I1(None), expect_no_chain_update()),
        (stacks_blocks::H1(None), expect_no_chain_update()),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_blocks(
                vec![
                    stacks_blocks::G1(None),
                    stacks_blocks::H1(None),
                    stacks_blocks::I1(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 033: Generate the following blocks
///
/// A1(1)  -  B1(12)  -  C1(13)  -  D1(14) -  E1(9)  -  F1(6)  -  G1(5)  -  H1(4)  -  I1(2)
///       \                       \ D3(10) -  E3(7)  -  F3(3)
///        \  B2(11)  -  C2(8)
///
pub fn get_vector_033() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::I1(None), expect_no_chain_update()),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (stacks_blocks::H1(None), expect_no_chain_update()),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (stacks_blocks::F1(None), expect_no_chain_update()),
        (stacks_blocks::E3(None), expect_no_chain_update()),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (
            stacks_blocks::D3(Some(stacks_blocks::C1(None))),
            expect_no_chain_update(),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_blocks(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![],
            ),
        ),
        (stacks_blocks::B1(None), expect_no_chain_update()),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D3(None),
                    stacks_blocks::E3(None),
                    stacks_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::D3(Some(stacks_blocks::C1(None))),
                    stacks_blocks::E3(None),
                    stacks_blocks::F3(None),
                ],
                vec![
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                    stacks_blocks::H1(None),
                    stacks_blocks::I1(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 034: Generate the following blocks
///
/// A1(1)  -  B1(12)  -  C1(14)  -  D1(7)  -  E1(2)  -  F1(4)  -  G1(6)  -  H1(9)  -  I1(13)
///       \            \ C3(5)   -  D3(3)  -  E3(8)  -  F3(15)
///        \  B2(10)  -  C2(11)
///
pub fn get_vector_034() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (stacks_blocks::D3(None), expect_no_chain_update()),
        (stacks_blocks::F1(None), expect_no_chain_update()),
        (
            stacks_blocks::C3(Some(stacks_blocks::B1(None))),
            expect_no_chain_update(),
        ),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (stacks_blocks::E3(None), expect_no_chain_update()),
        (stacks_blocks::H1(None), expect_no_chain_update()),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::B2(None)], vec![]),
        ),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::C2(None)], vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C3(Some(stacks_blocks::B1(None))),
                    stacks_blocks::D3(None),
                    stacks_blocks::E3(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::I1(None), expect_no_chain_update()),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::C3(Some(stacks_blocks::B1(None))),
                    stacks_blocks::D3(None),
                    stacks_blocks::E3(None),
                ],
                vec![
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                    stacks_blocks::H1(None),
                    stacks_blocks::I1(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::F3(None), expect_no_chain_update()),
    ]
}

/// Vector 035: Generate the following blocks
///
/// A1(1)  -  B1(5)  -  C1(4)  -  D1(8)  -  E1(10)  -  F1(13)  -  G1(12)  -  H1(15)  -  I1(14)
///       \           \ C3(6)  -  D3(7)  -  E3(11)  -  F3(9)   -  G3(16)
///        \  B2(2)  -  C2(3)
///
pub fn get_vector_035() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::B2(None)], vec![]),
        ),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::C2(None)], vec![]),
        ),
        (stacks_blocks::C1(None), expect_no_chain_update()),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::C3(Some(stacks_blocks::B1(None))),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::C1(None)],
                vec![stacks_blocks::C3(Some(stacks_blocks::B1(None)))],
                vec![],
            ),
        ),
        (
            stacks_blocks::D3(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::D3(None)], vec![]),
        ),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::C3(Some(stacks_blocks::B1(None))),
                    stacks_blocks::D3(None),
                ],
                vec![
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::E3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                ],
                vec![
                    stacks_blocks::C3(Some(stacks_blocks::B1(None))),
                    stacks_blocks::D3(None),
                    stacks_blocks::E3(None),
                    stacks_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::C3(Some(stacks_blocks::B1(None))),
                    stacks_blocks::D3(None),
                    stacks_blocks::E3(None),
                    stacks_blocks::F3(None),
                ],
                vec![
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                ],
                vec![stacks_blocks::A1(None)],
            ),
        ),
        (stacks_blocks::I1(None), expect_no_chain_update()),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_blocks(
                vec![stacks_blocks::H1(None), stacks_blocks::I1(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(stacks_blocks::J1(None), vec![]),
        ),
        (
            stacks_blocks::K1(None),
            expect_chain_updated_with_block(stacks_blocks::K1(None), vec![]),
        ),
        (
            stacks_blocks::L1(None),
            expect_chain_updated_with_block(stacks_blocks::L1(None), vec![]),
        ),
        (
            stacks_blocks::M1(None),
            expect_chain_updated_with_block(
                stacks_blocks::M1(None),
                vec![
                    stacks_blocks::A1(None),
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                    stacks_blocks::G1(None),
                ],
            ),
        ),
    ]
}

/// Vector 036: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(4) - D1(9) -  E1(16)  -  F1(6)  -  G1(15)
///       \          \  C3(6) - D3(7) -  E3(17)  -  F3(11) -  G3(12)
///        \  B2(3)  -  C2(8) - D2(5) -  E2(14)  -  F2(13) -  G2(10)
///
pub fn get_vector_036() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::B1(None)], vec![]),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None)],
                vec![stacks_blocks::B2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None)],
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![],
            ),
        ),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (
            stacks_blocks::C3(Some(stacks_blocks::B1(None))),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::C1(None)],
                vec![stacks_blocks::C3(Some(stacks_blocks::B1(None)))],
                vec![],
            ),
        ),
        (
            stacks_blocks::D3(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::D3(None)], vec![]),
        ),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (stacks_blocks::G2(None), expect_no_chain_update()),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (stacks_blocks::G3(None), expect_no_chain_update()),
        (stacks_blocks::F2(None), expect_no_chain_update()),
        (
            stacks_blocks::E2(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C3(Some(stacks_blocks::B1(None))),
                    stacks_blocks::D3(None),
                ],
                vec![
                    stacks_blocks::B2(None),
                    stacks_blocks::C2(None),
                    stacks_blocks::D2(None),
                    stacks_blocks::E2(None),
                    stacks_blocks::F2(None),
                    stacks_blocks::G2(None),
                ],
                vec![stacks_blocks::A1(None)],
            ),
        ),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (
            stacks_blocks::E3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::B2(None),
                    stacks_blocks::C2(None),
                    stacks_blocks::D2(None),
                    stacks_blocks::E2(None),
                    stacks_blocks::F2(None),
                    stacks_blocks::G2(None),
                ],
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C3(Some(stacks_blocks::B1(None))),
                    stacks_blocks::D3(None),
                    stacks_blocks::E3(None),
                    stacks_blocks::F3(None),
                    stacks_blocks::G3(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 037: Generate the following blocks
///
/// A1(1)  -  B1(2) - C1(4) - D1(9)  - E1(16) - F1(6)  -  G1(15)
///        \  B3(6) - C3(7) - D3(17) - E3(11) - F3(12)
///        \  B2(3) - C2(8) - D2(5)  - E2(14) - F2(13) -  G2(10)
///
pub fn get_vector_037() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_blocks(vec![stacks_blocks::B1(None)], vec![]),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None)],
                vec![stacks_blocks::B2(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B2(None)],
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![],
            ),
        ),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (stacks_blocks::B3(None), expect_no_chain_update()),
        (
            stacks_blocks::C3(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                vec![stacks_blocks::B3(None), stacks_blocks::C3(None)],
                vec![],
            ),
        ),
        (
            stacks_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![stacks_blocks::B3(None), stacks_blocks::C3(None)],
                vec![
                    stacks_blocks::B2(None),
                    stacks_blocks::C2(None),
                    stacks_blocks::D2(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (stacks_blocks::G2(None), expect_no_chain_update()),
        (stacks_blocks::E3(None), expect_no_chain_update()),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (stacks_blocks::F2(None), expect_no_chain_update()),
        (
            stacks_blocks::E2(None),
            expect_chain_updated_with_blocks(
                vec![
                    stacks_blocks::E2(None),
                    stacks_blocks::F2(None),
                    stacks_blocks::G2(None),
                ],
                vec![],
            ),
        ),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (stacks_blocks::D3(None), expect_no_chain_update()),
    ]
}

/// Vector 038: Generate the following blocks
///
/// A1(1)  -  B1(16) - C1(6)  - D1(5)  - E1(4) -  F1(3)
///        \  B3(17) - C3(10) - D3(9)  - E3(8)  - F3(7)
///        \  B2(18) - C2(15) - D2(14) - E2(13) - F2(12) - G2(11)
///
pub fn get_vector_038() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::F1(None), expect_no_chain_update()),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (stacks_blocks::C1(None), expect_no_chain_update()),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (stacks_blocks::E3(None), expect_no_chain_update()),
        (stacks_blocks::D3(None), expect_no_chain_update()),
        (stacks_blocks::C3(None), expect_no_chain_update()),
        (stacks_blocks::G2(None), expect_no_chain_update()),
        (stacks_blocks::F2(None), expect_no_chain_update()),
        (stacks_blocks::E2(None), expect_no_chain_update()),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_blocks(
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::B3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::B1(None),
                    stacks_blocks::C1(None),
                    stacks_blocks::D1(None),
                    stacks_blocks::E1(None),
                    stacks_blocks::F1(None),
                ],
                vec![
                    stacks_blocks::B3(None),
                    stacks_blocks::C3(None),
                    stacks_blocks::D3(None),
                    stacks_blocks::E3(None),
                    stacks_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    stacks_blocks::B3(None),
                    stacks_blocks::C3(None),
                    stacks_blocks::D3(None),
                    stacks_blocks::E3(None),
                    stacks_blocks::F3(None),
                ],
                vec![
                    stacks_blocks::B2(None),
                    stacks_blocks::C2(None),
                    stacks_blocks::D2(None),
                    stacks_blocks::E2(None),
                    stacks_blocks::F2(None),
                    stacks_blocks::G2(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 039: Generate the following blocks
///
/// A1(1)  -  B1(15)  -  C1(8)  -  D1(7)  -  E1(6)   - F1(3) - G1(2)
///       \                               \  E3(10)  - F3(9)
///        \  B2(14)  -  C2(13)  -  D2(12) -  E2(11) - F2(5) - G2(4)
///
pub fn get_vector_039() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (stacks_blocks::F1(None), expect_no_chain_update()),
        (stacks_blocks::G2(None), expect_no_chain_update()),
        (stacks_blocks::F2(None), expect_no_chain_update()),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (stacks_blocks::C1(None), expect_no_chain_update()),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (stacks_blocks::E3(None), expect_no_chain_update()),
        (stacks_blocks::E2(None), expect_no_chain_update()),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_blocks(
                vec![
                    stacks_blocks::B2(None),
                    stacks_blocks::C2(None),
                    stacks_blocks::D2(None),
                    stacks_blocks::E2(None),
                    stacks_blocks::F2(None),
                    stacks_blocks::G2(None),
                ],
                vec![stacks_blocks::A1(None)],
            ),
        ),
    ]
}

/// Vector 040: Generate the following blocks
///
/// A1(1)  -  B1(16)  -  C1(6)  -  D1(5)  -  E1(4)  - F1(3) -  G1(2)
///       \                               \  E3(9)  - F3(8) -  G3(7)
///        \  B2(15)  -  C2(14)  -  D2(13) - E2(12) - F2(11) - G2(10)
///
pub fn get_vector_040() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (stacks_blocks::G1(None), expect_no_chain_update()),
        (stacks_blocks::F1(None), expect_no_chain_update()),
        (stacks_blocks::E1(None), expect_no_chain_update()),
        (stacks_blocks::D1(None), expect_no_chain_update()),
        (stacks_blocks::C1(None), expect_no_chain_update()),
        (stacks_blocks::G3(None), expect_no_chain_update()),
        (stacks_blocks::F3(None), expect_no_chain_update()),
        (
            stacks_blocks::E3(Some(stacks_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (stacks_blocks::G2(None), expect_no_chain_update()),
        (stacks_blocks::F2(None), expect_no_chain_update()),
        (stacks_blocks::E2(None), expect_no_chain_update()),
        (stacks_blocks::D2(None), expect_no_chain_update()),
        (stacks_blocks::C2(None), expect_no_chain_update()),
        (
            stacks_blocks::B2(None),
            expect_chain_updated_with_blocks(
                vec![
                    stacks_blocks::B2(None),
                    stacks_blocks::C2(None),
                    stacks_blocks::D2(None),
                    stacks_blocks::E2(None),
                    stacks_blocks::F2(None),
                    stacks_blocks::G2(None),
                ],
                vec![stacks_blocks::A1(None)],
            ),
        ),
    ]
}

/// Vector 041: Generate the following blocks
///
/// A1(1) - B1(2) - C1(3) -  D1(5) - E1(8) - F1(9)
///               \ C2(4)  - D2(6) - E2(7) - F2(10) - G2(11)
///
pub fn get_vector_041() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![]
}

/// Vector 042: Generate the following blocks
///
/// A1(1) -  B1(2) - [a1](3) - [b1](4) - [c1](5) -  C1(6)
///
pub fn get_vector_042() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            microblocks::a1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::a1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::b1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::b1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::c1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::c1(stacks_blocks::B1(None), None)),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
    ]
}

/// Vector 043: Generate the following blocks
///
/// A1(1) -  B1(2) - [a1](3) - [b1](4) - [c1](5) - [d1](6) - [e1](7) -  C1(8)
///
pub fn get_vector_043() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            microblocks::a1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::a1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::b1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::b1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::c1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::c1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::d1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::d1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::e1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::e1(stacks_blocks::B1(None), None)),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
    ]
}

/// Vector 044: Generate the following blocks
///
/// A1(1) -  B1(2) - [a1](3) - [b1](4)
///                \ [a2](4) - [b2](5)
///
pub fn get_vector_044() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            microblocks::a1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::a1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::b1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::b1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::a2(stacks_blocks::B1(None), None),
            expect_no_chain_update(),
        ),
        (
            microblocks::b2(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
            ),
        ),
    ]
}

/// Vector 045: Generate the following blocks
///
/// A1(1) -  B1(2) - [a1](3) - [b1](4) - [c1](6)
///                \ [a2](4) - [b2](5)
///
pub fn get_vector_045() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            microblocks::a1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::a1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::b1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::b1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::a2(stacks_blocks::B1(None), None),
            expect_no_chain_update(),
        ),
        (
            microblocks::b2(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
            ),
        ),
        (
            microblocks::c1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                    microblocks::c1(stacks_blocks::B1(None), None),
                ],
            ),
        ),
    ]
}

/// Vector 046: Generate the following blocks
///
/// A1(1) -  B1(2) - [a1](3) - [b1](4) - [c1](6) - [d1](7)
///                \ [a2](4) - [b2](5)
///
pub fn get_vector_046() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            microblocks::a1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::a1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::b1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::b1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::a2(stacks_blocks::B1(None), None),
            expect_no_chain_update(),
        ),
        (
            microblocks::b2(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
            ),
        ),
        (
            microblocks::c1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                    microblocks::c1(stacks_blocks::B1(None), None),
                ],
            ),
        ),
        (
            microblocks::d1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::d1(stacks_blocks::B1(None), None)),
        ),
    ]
}

/// Vector 047: Generate the following blocks
///
/// A1(1) -  B1(2) - [a1](3) - [b1](4) - [c1](7) - [d1](8) - C1(9)
///                \ [a2](5) - [b2](6)
///
pub fn get_vector_047() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            microblocks::a1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::a1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::b1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::b1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::a2(stacks_blocks::B1(None), None),
            expect_no_chain_update(),
        ),
        (
            microblocks::b2(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
            ),
        ),
        (
            microblocks::c1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                    microblocks::c1(stacks_blocks::B1(None), None),
                ],
            ),
        ),
        (
            microblocks::d1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::d1(stacks_blocks::B1(None), None)),
        ),
        (
            stacks_blocks::C1(Some(microblocks::d1(stacks_blocks::B1(None), None))),
            expect_chain_updated_with_block(
                stacks_blocks::C1(Some(microblocks::d1(stacks_blocks::B1(None), None))),
                vec![],
            ),
        ),
    ]
}

/// Vector 048: Generate the following blocks
///
/// A1(1) -  B1(2) - [a1](3) - [b1](4) - [c1](6) - [d1](7)
///                \ [a2](4) - [b2](5)                    - C1(8)
///
pub fn get_vector_048() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            microblocks::a1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::a1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::b1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::b1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::a2(stacks_blocks::B1(None), None),
            expect_no_chain_update(),
        ),
        (
            microblocks::b2(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
            ),
        ),
        (
            microblocks::c1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                    microblocks::c1(stacks_blocks::B1(None), None),
                ],
            ),
        ),
        (
            microblocks::d1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::d1(stacks_blocks::B1(None), None)),
        ),
        (
            stacks_blocks::C1(Some(microblocks::b2(stacks_blocks::B1(None), None))),
            expect_chain_updated_with_block_and_microblock_updates(
                stacks_blocks::C1(Some(microblocks::b2(stacks_blocks::B1(None), None))),
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                    microblocks::c1(stacks_blocks::B1(None), None),
                    microblocks::d1(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 049: Generate the following blocks
///
/// A1(1) -  B1(2) - [a1](3) - [b1](4) - [c1](6) - [d1](7)  - C1(10)
///                \ [a2](4) - [b2](5)                      - C2(9)
///
pub fn get_vector_049() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            microblocks::a1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::a1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::b1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::b1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::a2(stacks_blocks::B1(None), None),
            expect_no_chain_update(),
        ),
        (
            microblocks::b2(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
            ),
        ),
        (
            microblocks::c1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock_reorg(
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                    microblocks::c1(stacks_blocks::B1(None), None),
                ],
            ),
        ),
        (
            microblocks::d1(stacks_blocks::B1(None), None),
            expect_chain_updated_with_microblock(microblocks::d1(stacks_blocks::B1(None), None)),
        ),
        (
            microblocks::c2(stacks_blocks::B1(None), None),
            expect_no_chain_update(),
        ),
        (
            stacks_blocks::C2(Some(microblocks::b2(stacks_blocks::B1(None), None))),
            expect_chain_updated_with_block_and_microblock_updates(
                stacks_blocks::C2(Some(microblocks::b2(stacks_blocks::B1(None), None))),
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                    microblocks::c1(stacks_blocks::B1(None), None),
                    microblocks::d1(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
                vec![],
            ),
        ),
        (
            stacks_blocks::C1(Some(microblocks::d1(stacks_blocks::B1(None), None))),
            expect_chain_updated_with_block_reorg_and_microblock_updates(
                stacks_blocks::C2(Some(microblocks::b2(stacks_blocks::B1(None), None))),
                stacks_blocks::C1(Some(microblocks::d1(stacks_blocks::B1(None), None))),
                vec![
                    microblocks::a2(stacks_blocks::B1(None), None),
                    microblocks::b2(stacks_blocks::B1(None), None),
                ],
                vec![
                    microblocks::a1(stacks_blocks::B1(None), None),
                    microblocks::b1(stacks_blocks::B1(None), None),
                    microblocks::c1(stacks_blocks::B1(None), None),
                    microblocks::d1(stacks_blocks::B1(None), None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 050: Generate the following blocks
///
/// A1(1) -  B1(2) - [a1](3) - [b1](4) - [c1](6) - [d1](7)  - C1(10)
///                \ [a2](4) - [b2](5)                      - C2(9)  - D2(12)
///                                    \ [c2](8)            - C3(11)
///
pub fn get_vector_050() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    let mut base = get_vector_049();
    base.append(&mut vec![(
        stacks_blocks::C3(Some(microblocks::c2(stacks_blocks::B1(None), None))),
        expect_chain_updated_with_block_reorg_and_microblock_updates(
            stacks_blocks::C1(Some(microblocks::d1(stacks_blocks::B1(None), None))),
            stacks_blocks::C3(Some(microblocks::c2(stacks_blocks::B1(None), None))),
            vec![
                microblocks::a1(stacks_blocks::B1(None), None),
                microblocks::b1(stacks_blocks::B1(None), None),
                microblocks::c1(stacks_blocks::B1(None), None),
                microblocks::d1(stacks_blocks::B1(None), None),
            ],
            vec![
                microblocks::a2(stacks_blocks::B1(None), None),
                microblocks::b2(stacks_blocks::B1(None), None),
                microblocks::c2(stacks_blocks::B1(None), None),
            ],
            vec![],
        ),
    )]);
    base
}

/// Vector 051: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(1)  -  E1(2)  -  F1(3)  -  G1(1)  -  H1(2)  -  I1(3)  -  J1(1)  -  K1(2)  -  L1(3)  -  M1(1)  -  N1(2)  -  O1(3)  -  P1(1)
///
pub fn get_vector_051() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            stacks_blocks::A1(None),
            expect_chain_updated_with_block(stacks_blocks::A1(None), vec![]),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
        (
            stacks_blocks::C1(None),
            expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
        ),
        (
            stacks_blocks::D1(None),
            expect_chain_updated_with_block(stacks_blocks::D1(None), vec![]),
        ),
        (
            stacks_blocks::E1(None),
            expect_chain_updated_with_block(stacks_blocks::E1(None), vec![]),
        ),
        (
            stacks_blocks::F1(None),
            expect_chain_updated_with_block(stacks_blocks::F1(None), vec![]),
        ),
        (
            stacks_blocks::G1(None),
            expect_chain_updated_with_block(stacks_blocks::G1(None), vec![stacks_blocks::A1(None)]),
        ),
        (
            stacks_blocks::H1(None),
            expect_chain_updated_with_block(stacks_blocks::H1(None), vec![stacks_blocks::B1(None)]),
        ),
        (
            stacks_blocks::I1(None),
            expect_chain_updated_with_block(stacks_blocks::I1(None), vec![stacks_blocks::C1(None)]),
        ),
        (
            stacks_blocks::J1(None),
            expect_chain_updated_with_block(stacks_blocks::J1(None), vec![stacks_blocks::D1(None)]),
        ),
        (
            stacks_blocks::K1(None),
            expect_chain_updated_with_block(stacks_blocks::K1(None), vec![stacks_blocks::E1(None)]),
        ),
        (
            stacks_blocks::L1(None),
            expect_chain_updated_with_block(stacks_blocks::L1(None), vec![stacks_blocks::F1(None)]),
        ),
        (
            stacks_blocks::M1(None),
            expect_chain_updated_with_block(stacks_blocks::M1(None), vec![stacks_blocks::G1(None)]),
        ),
        (
            stacks_blocks::N1(None),
            expect_chain_updated_with_block(stacks_blocks::N1(None), vec![stacks_blocks::H1(None)]),
        ),
        (
            stacks_blocks::O1(None),
            expect_chain_updated_with_block(stacks_blocks::O1(None), vec![stacks_blocks::I1(None)]),
        ),
        (
            stacks_blocks::P1(None),
            expect_chain_updated_with_block(stacks_blocks::P1(None), vec![stacks_blocks::J1(None)]),
        ),
    ]
}

/// Vector 052: Generate the following blocks
///
///  [a1](1) - [b1](2) - B1(3)
///
///
pub fn get_vector_052() -> Vec<(BlockEvent, StacksChainEventExpectation)> {
    vec![
        (
            microblocks::a1(stacks_blocks::A1(None), None),
            expect_no_chain_update(),
        ),
        (
            microblocks::b1(stacks_blocks::A1(None), None),
            expect_no_chain_update(),
        ),
        (
            stacks_blocks::B1(None),
            expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
        ),
    ]
}

/// Vector 053: Generate the following blocks
///
///  A1(0) - B1(1) - C1(3)
///        \ B2(2)
///
///
pub fn get_vector_053() -> (
    Vec<(BlockEvent, StacksChainEventExpectation)>,
    Option<Vec<StacksBlockData>>,
) {
    (
        vec![
            (
                stacks_blocks::B1(None),
                expect_chain_updated_with_block(stacks_blocks::B1(None), vec![]),
            ),
            (
                stacks_blocks::B2(None),
                expect_chain_updated_with_block_reorg(
                    vec![stacks_blocks::B1(None)],
                    vec![stacks_blocks::B2(None)],
                    vec![],
                ),
            ),
            (
                stacks_blocks::C1(None),
                expect_chain_updated_with_block_reorg(
                    vec![stacks_blocks::B2(None)],
                    vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                    vec![],
                ),
            ),
        ],
        Some(vec![get_block_from_block_event(stacks_blocks::A1(None))]),
    )
}
/// Vector 054: Generate the following blocks
///
///  A1(0) - B1(0) - C1(1) - D1(4)
///        \ B2(2) - C2(3)
///
///
pub fn get_vector_054() -> (
    Vec<(BlockEvent, StacksChainEventExpectation)>,
    Option<Vec<StacksBlockData>>,
) {
    (
        vec![
            (
                stacks_blocks::C1(None),
                expect_chain_updated_with_block(stacks_blocks::C1(None), vec![]),
            ),
            (stacks_blocks::B2(None), expect_no_chain_update()),
            (
                stacks_blocks::C2(None),
                expect_chain_updated_with_block_reorg(
                    vec![stacks_blocks::B1(None), stacks_blocks::C1(None)],
                    vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                    vec![],
                ),
            ),
            (
                stacks_blocks::D1(None),
                expect_chain_updated_with_block_reorg(
                    vec![stacks_blocks::B2(None), stacks_blocks::C2(None)],
                    vec![
                        stacks_blocks::B1(None),
                        stacks_blocks::C1(None),
                        stacks_blocks::D1(None),
                    ],
                    vec![],
                ),
            ),
        ],
        Some(vec![
            get_block_from_block_event(stacks_blocks::A1(None)),
            get_block_from_block_event(stacks_blocks::B1(None)),
        ]),
    )
}

/// Vector 055: Generate the following blocks
///
///  A1(0) - B1(0)
///        \ B2(0) - C2(0) - D2(0) - E2(1) - F2(2) - G2(3) - H2(4) - I2(7) - J2(11)
///                        \ D3(5) - E3(6) - F3(8) - G3(9) - H3(10) - I3(12) - J3(13) - K3(14) - L3(15) - M3(16) - N3(17) - O3(18) - P3(19) - Q3(20)
///
pub fn get_vector_055() -> (
    Vec<(BlockEvent, StacksChainEventExpectation)>,
    Option<Vec<StacksBlockData>>,
) {
    (
        vec![
            (
                stacks_blocks::E2(None),
                expect_chain_updated_with_block(stacks_blocks::E2(None), vec![]),
            ),
            (
                stacks_blocks::F2(None),
                expect_chain_updated_with_block(stacks_blocks::F2(None), vec![]),
            ),
            (
                stacks_blocks::G2(None),
                expect_chain_updated_with_block(stacks_blocks::G2(None), vec![]),
            ),
            (
                stacks_blocks::H2(None),
                expect_chain_updated_with_block(stacks_blocks::H2(None), vec![]),
            ),
            (
                stacks_blocks::D3(Some(stacks_blocks::C2(None))),
                expect_no_chain_update(),
            ),
            (stacks_blocks::E3(None), expect_no_chain_update()),
            (
                stacks_blocks::I2(None),
                expect_chain_updated_with_block(stacks_blocks::I2(None), vec![]),
            ),
            (stacks_blocks::F3(None), expect_no_chain_update()),
            (stacks_blocks::G3(None), expect_no_chain_update()),
            (stacks_blocks::H3(None), expect_no_chain_update()),
            (
                stacks_blocks::J2(None),
                expect_chain_updated_with_block(stacks_blocks::J2(None), vec![]),
            ),
            (stacks_blocks::I3(None), expect_no_chain_update()),
            (
                stacks_blocks::J3(None),
                expect_chain_updated_with_block_reorg(
                    vec![
                        stacks_blocks::D2(None),
                        stacks_blocks::E2(None),
                        stacks_blocks::F2(None),
                        stacks_blocks::G2(None),
                        stacks_blocks::H2(None),
                        stacks_blocks::I2(None),
                        stacks_blocks::J2(None),
                    ],
                    vec![
                        stacks_blocks::D3(None),
                        stacks_blocks::E3(None),
                        stacks_blocks::F3(None),
                        stacks_blocks::G3(None),
                        stacks_blocks::H3(None),
                        stacks_blocks::I3(None),
                        stacks_blocks::J3(None),
                    ],
                    vec![],
                ),
            ),
            (
                stacks_blocks::K3(None),
                expect_chain_updated_with_block(stacks_blocks::K3(None), vec![]),
            ),
            (
                stacks_blocks::L3(None),
                expect_chain_updated_with_block(stacks_blocks::L3(None), vec![]),
            ),
            (
                stacks_blocks::M3(None),
                expect_chain_updated_with_block(stacks_blocks::M3(None), vec![]),
            ),
            (
                stacks_blocks::N3(None),
                expect_chain_updated_with_block(stacks_blocks::N3(None), vec![]),
            ),
            (
                stacks_blocks::O3(None),
                expect_chain_updated_with_block(stacks_blocks::O3(None), vec![]),
            ),
            (
                stacks_blocks::P3(None),
                expect_chain_updated_with_block(stacks_blocks::P3(None), vec![]),
            ),
            (
                stacks_blocks::Q3(None),
                expect_chain_updated_with_block(
                    stacks_blocks::Q3(None),
                    vec![
                        stacks_blocks::A1(None),
                        stacks_blocks::B2(None),
                        stacks_blocks::C2(None),
                        stacks_blocks::D3(None),
                        stacks_blocks::E3(None),
                        stacks_blocks::F3(None),
                        stacks_blocks::G3(None),
                        stacks_blocks::H3(None),
                        stacks_blocks::I3(None),
                        stacks_blocks::J3(None),
                        stacks_blocks::K3(None),
                    ],
                ),
            ),
        ],
        Some(vec![
            get_block_from_block_event(stacks_blocks::A1(None)),
            get_block_from_block_event(stacks_blocks::B1(None)),
            get_block_from_block_event(stacks_blocks::B2(None)),
            get_block_from_block_event(stacks_blocks::C2(None)),
            get_block_from_block_event(stacks_blocks::D2(None)),
        ]),
    )
}

fn get_block_from_block_event(block_event: BlockEvent) -> StacksBlockData {
    match block_event {
        BlockEvent::Block(block) => *block,
        _ => unreachable!(),
    }
}
