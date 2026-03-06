use hiro_system_kit::slog;

use super::super::BlockchainEventExpectation;
use super::bitcoin_blocks;
use crate::chainhook::types::{BitcoinBlockData, BlockchainEvent};
use crate::chainhook::utils::Context;

pub fn expect_no_chain_update() -> BlockchainEventExpectation {
    Box::new(move |chain_event_to_check: Option<BlockchainEvent>| {
        assert!(
            chain_event_to_check.is_none(),
            "expected no Chain update, got {:?}",
            chain_event_to_check
        );
    })
}

pub fn expect_chain_updated_with_block(
    expected_block: BitcoinBlockData,
    confirmed_blocks: Vec<BitcoinBlockData>,
) -> BlockchainEventExpectation {
    expect_chain_updated_with_blocks(vec![expected_block], confirmed_blocks)
}

pub fn expect_chain_updated_with_blocks(
    expected_blocks: Vec<BitcoinBlockData>,
    confirmed_blocks: Vec<BitcoinBlockData>,
) -> BlockchainEventExpectation {
    let ctx = Context::empty();
    Box::new(move |chain_event_to_check: Option<BlockchainEvent>| {
        assert!(
            match chain_event_to_check {
                Some(BlockchainEvent::BlockchainUpdatedWithHeaders(ref event)) => {
                    assert_eq!(expected_blocks.len(), event.new_headers.len());
                    for (expected_block, new_block) in
                        expected_blocks.iter().zip(&event.new_headers)
                    {
                        ctx.try_log(|logger| {
                            slog::debug!(
                                logger,
                                "Checking {} and {}",
                                expected_block.block_identifier,
                                new_block.block_identifier
                            )
                        });
                        assert!(
                            new_block
                                .block_identifier
                                .eq(&expected_block.block_identifier),
                            "{} ≠ {}",
                            new_block.block_identifier,
                            expected_block.block_identifier
                        );
                    }
                    assert_eq!(confirmed_blocks.len(), event.confirmed_headers.len());
                    for (expected_block, confirmed_block) in
                        confirmed_blocks.iter().zip(&event.confirmed_headers)
                    {
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

pub fn expect_chain_updated_with_block_reorg(
    mut blocks_to_rollback: Vec<BitcoinBlockData>,
    blocks_to_apply: Vec<BitcoinBlockData>,
    _confirmed_blocks: Vec<BitcoinBlockData>,
) -> BlockchainEventExpectation {
    blocks_to_rollback.reverse();
    Box::new(move |chain_event_to_check: Option<BlockchainEvent>| {
        assert!(
            match chain_event_to_check {
                Some(BlockchainEvent::BlockchainUpdatedWithReorg(ref event)) => {
                    assert_eq!(blocks_to_rollback.len(), event.headers_to_rollback.len());
                    assert_eq!(blocks_to_apply.len(), event.headers_to_apply.len());
                    for (expected, block_update) in
                        blocks_to_rollback.iter().zip(&event.headers_to_rollback)
                    {
                        assert!(
                            expected.block_identifier.eq(&block_update.block_identifier),
                            "{} ≠ {}",
                            expected.block_identifier,
                            block_update.block_identifier
                        );
                    }
                    for (expected, block_update) in
                        blocks_to_apply.iter().zip(&event.headers_to_apply)
                    {
                        assert!(
                            expected.block_identifier.eq(&block_update.block_identifier),
                            "{} ≠ {}",
                            expected.block_identifier,
                            block_update.block_identifier
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

// Test vectors:
// 001 to 020: Bitcoin anchored blocks received in order
// 021 to 040: Bitcoin anchored blocks received out of order

/// Vector 001: Generate the following blocks
///
/// A1(1)  -  B1(2)  -  C1(3)
///
pub fn get_vector_001() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
    ]
}

/// Vector 002: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(4)
///        \  B2(3)  -  C2(5)
///
pub fn get_vector_002() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None)],
                vec![bitcoin_blocks::B2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None)],
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
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
pub fn get_vector_003() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
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
pub fn get_vector_004() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
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
pub fn get_vector_005() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
    ]
}

/// Vector 006: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_006() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
    ]
}

/// Vector 007: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(9)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_007() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
    ]
}

/// Vector 008: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(9)  -  H1(10)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_008() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::H1(None),
                vec![bitcoin_blocks::B1(None)],
            ),
        ),
    ]
}

/// Vector 009: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(9)  -  H1(10)  -  I1(11)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_009() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::H1(None),
                vec![bitcoin_blocks::B1(None)],
            ),
        ),
        (
            bitcoin_blocks::I1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::I1(None),
                vec![bitcoin_blocks::C1(None)],
            ),
        ),
    ]
}

/// Vector 010: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(9)  -  H1(10) -  I1(11)
///        \  B2(4)  -  C2(5)  -  D2(12)
///
pub fn get_vector_010() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::H1(None),
                vec![bitcoin_blocks::B1(None)],
            ),
        ),
        (
            bitcoin_blocks::I1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::I1(None),
                vec![bitcoin_blocks::C1(None)],
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
pub fn get_vector_011() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::H1(None),
                vec![bitcoin_blocks::B1(None)],
            ),
        ),
        (
            bitcoin_blocks::I1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::I1(None),
                vec![bitcoin_blocks::C1(None)],
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
pub fn get_vector_012() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::H1(None),
                vec![bitcoin_blocks::B1(None)],
            ),
        ),
        (
            bitcoin_blocks::I1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::I1(None),
                vec![bitcoin_blocks::C1(None)],
            ),
        ),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
    ]
}

/// Vector 013: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)  -  H1(12)  -  I1(14)
///       \                               \ E3(9)  -  F3(11) -  G3(13)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_013() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::H1(None),
                vec![bitcoin_blocks::B1(None)],
            ),
        ),
        (bitcoin_blocks::G3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::I1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::I1(None),
                vec![bitcoin_blocks::C1(None)],
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
pub fn get_vector_014() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::H1(None),
                vec![bitcoin_blocks::B1(None)],
            ),
        ),
        (bitcoin_blocks::G3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::I1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::I1(None),
                vec![bitcoin_blocks::C1(None)],
            ),
        ),
        (bitcoin_blocks::H3(None), expect_no_chain_update()),
    ]
}

/// Vector 015: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)  -  H1(12)  -  I1(14)
///       \                               \ E3(9)  -  F3(11) -  G3(13)  -  H3(15)  -  I3(16)
///        \  B2(4)  -  C2(5)
///
pub fn get_vector_015() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (
            bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::H1(None),
                vec![bitcoin_blocks::B1(None)],
            ),
        ),
        (bitcoin_blocks::G3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::I1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::I1(None),
                vec![bitcoin_blocks::C1(None)],
            ),
        ),
        (bitcoin_blocks::H3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::I3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                    bitcoin_blocks::G1(None),
                    bitcoin_blocks::H1(None),
                    bitcoin_blocks::I1(None),
                ],
                vec![
                    bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
                    bitcoin_blocks::F3(None),
                    bitcoin_blocks::G3(None),
                    bitcoin_blocks::H3(None),
                    bitcoin_blocks::I3(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 016: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)  -  H1(12)  -  I1(14)
///       \                               \ E3(9)  -  F3(11) -  G3(13)  -  H3(15)  -  I3(16)
///        \  B2(4)  -  C2(5)  -  D2(17)
///
pub fn get_vector_016() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (
            bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::H1(None),
                vec![bitcoin_blocks::B1(None)],
            ),
        ),
        (bitcoin_blocks::G3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::I1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::I1(None),
                vec![bitcoin_blocks::C1(None)],
            ),
        ),
        (bitcoin_blocks::H3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::I3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                    bitcoin_blocks::G1(None),
                    bitcoin_blocks::H1(None),
                    bitcoin_blocks::I1(None),
                ],
                vec![
                    bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
                    bitcoin_blocks::F3(None),
                    bitcoin_blocks::G3(None),
                    bitcoin_blocks::H3(None),
                    bitcoin_blocks::I3(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
    ]
}

/// Vector 017: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  -  G1(10)
///       \                               \ E3(9)  -  F3(11) -  G3(12)
///        \  B2(4)  -  C2(5)  -  D2(13) -  E2(14)  - F2(15)  - G2(16)
///
pub fn get_vector_017() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (
            bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_block(
                bitcoin_blocks::G1(None),
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::G3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                    bitcoin_blocks::G1(None),
                ],
                vec![
                    bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
                    bitcoin_blocks::F3(None),
                    bitcoin_blocks::G3(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
        (bitcoin_blocks::E2(None), expect_no_chain_update()),
        (bitcoin_blocks::F2(None), expect_no_chain_update()),
        (bitcoin_blocks::G2(None), expect_no_chain_update()),
    ]
}

/// Vector 018: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)
///       \                               \ E3(9)  -  F3(10)
///        \  B2(4)  -  C2(5)  -  D2(11) -  E2(12) -  F2(13)  - G2(14)
///
pub fn get_vector_018() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (
            bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            bitcoin_blocks::F3(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::E1(None), bitcoin_blocks::F1(None)],
                vec![
                    bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
                    bitcoin_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
        (bitcoin_blocks::E2(None), expect_no_chain_update()),
        (bitcoin_blocks::F2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::G2(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
                    bitcoin_blocks::F3(None),
                ],
                vec![
                    bitcoin_blocks::B2(None),
                    bitcoin_blocks::C2(None),
                    bitcoin_blocks::D2(None),
                    bitcoin_blocks::E2(None),
                    bitcoin_blocks::F2(None),
                    bitcoin_blocks::G2(None),
                ],
                vec![bitcoin_blocks::A1(None)],
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
pub fn get_vector_019() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (
            bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            bitcoin_blocks::F3(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::E1(None), bitcoin_blocks::F1(None)],
                vec![
                    bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
                    bitcoin_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
        (bitcoin_blocks::E2(None), expect_no_chain_update()),
        (bitcoin_blocks::F2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::G2(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
                    bitcoin_blocks::F3(None),
                ],
                vec![
                    bitcoin_blocks::B2(None),
                    bitcoin_blocks::C2(None),
                    bitcoin_blocks::D2(None),
                    bitcoin_blocks::E2(None),
                    bitcoin_blocks::F2(None),
                    bitcoin_blocks::G2(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
    ]
}

/// Vector 020: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(6)  -  E1(7)  -  F1(8)  - G1(15)
///       \                               \ E3(9)  -  F3(10) - G3(16)
///        \  B2(4)  -  C2(5)  -  D2(11) -  E2(12) -  F2(13) - G2(14)
///
pub fn get_vector_020() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block(bitcoin_blocks::F1(None), vec![]),
        ),
        (
            bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (
            bitcoin_blocks::F3(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::E1(None), bitcoin_blocks::F1(None)],
                vec![
                    bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
                    bitcoin_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
        (bitcoin_blocks::E2(None), expect_no_chain_update()),
        (bitcoin_blocks::F2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::G2(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
                    bitcoin_blocks::F3(None),
                ],
                vec![
                    bitcoin_blocks::B2(None),
                    bitcoin_blocks::C2(None),
                    bitcoin_blocks::D2(None),
                    bitcoin_blocks::E2(None),
                    bitcoin_blocks::F2(None),
                    bitcoin_blocks::G2(None),
                ],
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::G3(None), expect_no_chain_update()),
    ]
}

/// Vector 021: Generate the following blocks
///
/// A1(1)  -  B1(3)  -  C1(2)
///
pub fn get_vector_021() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::C1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_blocks(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
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
pub fn get_vector_022() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
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
pub fn get_vector_023() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block(bitcoin_blocks::B2(None), vec![]),
        ),
        (bitcoin_blocks::C1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block(bitcoin_blocks::C2(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
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
pub fn get_vector_024() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block(bitcoin_blocks::B2(None), vec![]),
        ),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block(bitcoin_blocks::C2(None), vec![]),
        ),
        (bitcoin_blocks::C1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block(bitcoin_blocks::D1(None), vec![]),
        ),
    ]
}

/// Vector 025: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(4)  -  D1(5)  -  E1(6)
///        \  B2(3)  -  C2(7)
///
pub fn get_vector_025() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None)],
                vec![bitcoin_blocks::B2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None)],
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block(bitcoin_blocks::D1(None), vec![]),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block(bitcoin_blocks::E1(None), vec![]),
        ),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
    ]
}

/// Vector 026: Generate the following blocks
///  
/// A1(1)  -  B1(2)  -  C1(3)  -  D1(8)  -  E1(7)  -  F1(6)
///        \  B2(5)  -  C2(4)
///
pub fn get_vector_026() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (bitcoin_blocks::F1(None), expect_no_chain_update()),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
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
pub fn get_vector_027() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block(bitcoin_blocks::B1(None), vec![]),
        ),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block(bitcoin_blocks::C1(None), vec![]),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::F1(None), expect_no_chain_update()),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                    bitcoin_blocks::G1(None),
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
pub fn get_vector_028() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::F1(None), expect_no_chain_update()),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (bitcoin_blocks::H1(None), expect_no_chain_update()),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block(bitcoin_blocks::B2(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None)],
                vec![bitcoin_blocks::B1(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                    bitcoin_blocks::G1(None),
                    bitcoin_blocks::H1(None),
                ],
                vec![],
            ),
        ),
    ]
}

/// Vector 029: Generate the following blocks
///  
/// A1(1)  -  B1(7)  -  C1(6)  -  D1(9)  -  E1(10)  -  F1(2)  -  G1(3)  -  H1(4)  -  I1(11)
///        \  B2(8)  -  C2(5)
///
pub fn get_vector_029() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::F1(None), expect_no_chain_update()),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::H1(None), expect_no_chain_update()),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (bitcoin_blocks::C1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_blocks(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_blocks(
                vec![
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                    bitcoin_blocks::G1(None),
                    bitcoin_blocks::H1(None),
                ],
                vec![bitcoin_blocks::A1(None), bitcoin_blocks::B1(None)],
            ),
        ),
        (
            bitcoin_blocks::I1(None),
            expect_chain_updated_with_blocks(
                vec![bitcoin_blocks::I1(None)],
                vec![bitcoin_blocks::C1(None)],
            ),
        ),
    ]
}

/// Vector 030: Generate the following blocks
///  
/// A1(1)  -  B1(9)  -  C1(8)  -  D1(7)  -  E1(6)  -  F1(5)  -  G1(4)  -  H1(3)  -  I1(2)
///        \  B2(11)  -  C2(10)
///
pub fn get_vector_030() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::I1(None), expect_no_chain_update()),
        (bitcoin_blocks::H1(None), expect_no_chain_update()),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::F1(None), expect_no_chain_update()),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (bitcoin_blocks::C1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_blocks(
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                    bitcoin_blocks::G1(None),
                    bitcoin_blocks::H1(None),
                    bitcoin_blocks::I1(None),
                ],
                vec![
                    bitcoin_blocks::A1(None),
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                ],
            ),
        ),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (bitcoin_blocks::B2(None), expect_no_chain_update()),
    ]
}

/// Vector 031: Generate the following blocks
///  
/// A1(1)  -  B1(8)  -  C1(7)  -  D1(6)  -  E1(4)  -  F1(9)  -  G1(11)  -  H1(12)  -  I1(10)
///       \                               \ E3(2)
///        \  B2(3)  -  C2(5)
///
pub fn get_vector_031() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::B2(None)], vec![]),
        ),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::C2(None)], vec![]),
        ),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (bitcoin_blocks::C1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::F1(None)], vec![]),
        ),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (bitcoin_blocks::I1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_blocks(
                vec![bitcoin_blocks::G1(None)],
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_blocks(
                vec![bitcoin_blocks::H1(None), bitcoin_blocks::I1(None)],
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
            ),
        ),
    ]
}

/// Vector 032: Generate the following blocks
///  
/// A1(1)  -  B1(3)  -  C1(5)  -  D1(2)  -  E1(8)  -  F1(10)  -  G1(13)  -  H1(12)  -  I1(11)
///       \                     \ D3(7)  -  E3(9)
///        \  B2(4)  -  C2(6)
///
pub fn get_vector_032() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::B1(None)], vec![]),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None)],
                vec![bitcoin_blocks::B2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::D3(Some(bitcoin_blocks::C1(None))),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::D1(None)],
                vec![bitcoin_blocks::D3(Some(bitcoin_blocks::C1(None)))],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::D3(Some(bitcoin_blocks::C1(None)))],
                vec![bitcoin_blocks::D1(None), bitcoin_blocks::E1(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E3(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::D1(None), bitcoin_blocks::E1(None)],
                vec![
                    bitcoin_blocks::D3(Some(bitcoin_blocks::C1(None))),
                    bitcoin_blocks::E3(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::D3(Some(bitcoin_blocks::C1(None))),
                    bitcoin_blocks::E3(None),
                ],
                vec![
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::I1(None), expect_no_chain_update()),
        (bitcoin_blocks::H1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::G1(None),
            expect_chain_updated_with_blocks(
                vec![
                    bitcoin_blocks::G1(None),
                    bitcoin_blocks::H1(None),
                    bitcoin_blocks::I1(None),
                ],
                vec![
                    bitcoin_blocks::A1(None),
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                ],
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
pub fn get_vector_033() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::I1(None), expect_no_chain_update()),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (bitcoin_blocks::H1(None), expect_no_chain_update()),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::F1(None), expect_no_chain_update()),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::D3(Some(bitcoin_blocks::C1(None))),
            expect_no_chain_update(),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_blocks(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![],
            ),
        ),
        (bitcoin_blocks::B1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D3(None),
                    bitcoin_blocks::E3(None),
                    bitcoin_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D1(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::D3(Some(bitcoin_blocks::C1(None))),
                    bitcoin_blocks::E3(None),
                    bitcoin_blocks::F3(None),
                ],
                vec![
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                    bitcoin_blocks::G1(None),
                    bitcoin_blocks::H1(None),
                    bitcoin_blocks::I1(None),
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
pub fn get_vector_034() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (bitcoin_blocks::D3(None), expect_no_chain_update()),
        (bitcoin_blocks::F1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None))),
            expect_no_chain_update(),
        ),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
        (bitcoin_blocks::H1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::B2(None)], vec![]),
        ),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::C2(None)], vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None))),
                    bitcoin_blocks::D3(None),
                    bitcoin_blocks::E3(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::I1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None))),
                    bitcoin_blocks::D3(None),
                    bitcoin_blocks::E3(None),
                ],
                vec![
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                    bitcoin_blocks::G1(None),
                    bitcoin_blocks::H1(None),
                    bitcoin_blocks::I1(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
    ]
}

/// Vector 035: Generate the following blocks
///  
/// A1(1)  -  B1(5)  -  C1(4)  -  D1(8)  -  E1(10)  -  F1(13)  -  G1(12)  -  H1(15)  -  I1(14)
///       \           \ C3(6)  -  D3(7)  -  E3(11)  -  F3(9)   -  G3(16)
///        \  B2(2)  -  C2(3)
///
pub fn get_vector_035() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::B2(None)], vec![]),
        ),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::C2(None)], vec![]),
        ),
        (bitcoin_blocks::C1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None), bitcoin_blocks::C2(None)],
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None))),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None)))],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D3(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::D3(None)], vec![]),
        ),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::E1(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None))),
                    bitcoin_blocks::D3(None),
                ],
                vec![
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::E3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                ],
                vec![
                    bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None))),
                    bitcoin_blocks::D3(None),
                    bitcoin_blocks::E3(None),
                    bitcoin_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::F1(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None))),
                    bitcoin_blocks::D3(None),
                    bitcoin_blocks::E3(None),
                    bitcoin_blocks::F3(None),
                ],
                vec![
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                    bitcoin_blocks::G1(None),
                ],
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::I1(None), expect_no_chain_update()),
        (
            bitcoin_blocks::H1(None),
            expect_chain_updated_with_blocks(
                vec![bitcoin_blocks::H1(None), bitcoin_blocks::I1(None)],
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
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
pub fn get_vector_036() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::B1(None)], vec![]),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None)],
                vec![bitcoin_blocks::B2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None)],
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![],
            ),
        ),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None))),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None)))],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::D3(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::D3(None)], vec![]),
        ),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (bitcoin_blocks::G2(None), expect_no_chain_update()),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (bitcoin_blocks::G3(None), expect_no_chain_update()),
        (bitcoin_blocks::F2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::E2(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C3(Some(bitcoin_blocks::B1(None))),
                    bitcoin_blocks::D3(None),
                ],
                vec![
                    bitcoin_blocks::B2(None),
                    bitcoin_blocks::C2(None),
                    bitcoin_blocks::D2(None),
                    bitcoin_blocks::E2(None),
                    bitcoin_blocks::F2(None),
                    bitcoin_blocks::G2(None),
                ],
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
    ]
}

/// Vector 037: Generate the following blocks
///  
/// A1(1)  -  B1(2) - C1(4) - D1(9)  - E1(16) - F1(6)  -  G1(15)
///        \  B3(6) - C3(7) - D3(17) - E3(11) - F3(12)
///        \  B2(3) - C2(8) - D2(5)  - E2(14) - F2(13) -  G2(10)
///
pub fn get_vector_037() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_blocks(vec![bitcoin_blocks::B1(None)], vec![]),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None)],
                vec![bitcoin_blocks::B2(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::C1(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B2(None)],
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![],
            ),
        ),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
        (bitcoin_blocks::B3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::C3(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B1(None), bitcoin_blocks::C1(None)],
                vec![bitcoin_blocks::B3(None), bitcoin_blocks::C3(None)],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::C2(None),
            expect_chain_updated_with_block_reorg(
                vec![bitcoin_blocks::B3(None), bitcoin_blocks::C3(None)],
                vec![
                    bitcoin_blocks::B2(None),
                    bitcoin_blocks::C2(None),
                    bitcoin_blocks::D2(None),
                ],
                vec![],
            ),
        ),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (bitcoin_blocks::G2(None), expect_no_chain_update()),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (bitcoin_blocks::F2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::E2(None),
            expect_chain_updated_with_blocks(
                vec![
                    bitcoin_blocks::E2(None),
                    bitcoin_blocks::F2(None),
                    bitcoin_blocks::G2(None),
                ],
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (bitcoin_blocks::D3(None), expect_no_chain_update()),
    ]
}

/// Vector 038: Generate the following blocks
///  
/// A1(1)  -  B1(16) - C1(6)  - D1(5)  - E1(4) -  F1(3)
///        \  B3(17) - C3(10) - D3(9)  - E3(8)  - F3(7)
///        \  B2(18) - C2(15) - D2(14) - E2(13) - F2(12) - G2(11)
///
pub fn get_vector_038() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::F1(None), expect_no_chain_update()),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (bitcoin_blocks::C1(None), expect_no_chain_update()),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
        (bitcoin_blocks::D3(None), expect_no_chain_update()),
        (bitcoin_blocks::C3(None), expect_no_chain_update()),
        (bitcoin_blocks::G2(None), expect_no_chain_update()),
        (bitcoin_blocks::F2(None), expect_no_chain_update()),
        (bitcoin_blocks::E2(None), expect_no_chain_update()),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B1(None),
            expect_chain_updated_with_blocks(
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::B3(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::B1(None),
                    bitcoin_blocks::C1(None),
                    bitcoin_blocks::D1(None),
                    bitcoin_blocks::E1(None),
                    bitcoin_blocks::F1(None),
                ],
                vec![
                    bitcoin_blocks::B3(None),
                    bitcoin_blocks::C3(None),
                    bitcoin_blocks::D3(None),
                    bitcoin_blocks::E3(None),
                    bitcoin_blocks::F3(None),
                ],
                vec![],
            ),
        ),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_block_reorg(
                vec![
                    bitcoin_blocks::B3(None),
                    bitcoin_blocks::C3(None),
                    bitcoin_blocks::D3(None),
                    bitcoin_blocks::E3(None),
                    bitcoin_blocks::F3(None),
                ],
                vec![
                    bitcoin_blocks::B2(None),
                    bitcoin_blocks::C2(None),
                    bitcoin_blocks::D2(None),
                    bitcoin_blocks::E2(None),
                    bitcoin_blocks::F2(None),
                    bitcoin_blocks::G2(None),
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
pub fn get_vector_039() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::F1(None), expect_no_chain_update()),
        (bitcoin_blocks::G2(None), expect_no_chain_update()),
        (bitcoin_blocks::F2(None), expect_no_chain_update()),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (bitcoin_blocks::C1(None), expect_no_chain_update()),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (bitcoin_blocks::E3(None), expect_no_chain_update()),
        (bitcoin_blocks::E2(None), expect_no_chain_update()),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_blocks(
                vec![
                    bitcoin_blocks::B2(None),
                    bitcoin_blocks::C2(None),
                    bitcoin_blocks::D2(None),
                    bitcoin_blocks::E2(None),
                    bitcoin_blocks::F2(None),
                    bitcoin_blocks::G2(None),
                ],
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::B1(None), expect_no_chain_update()),
    ]
}

/// Vector 040: Generate the following blocks
///  
/// A1(1)  -  B1(16)  -  C1(6)  -  D1(5)  -  E1(4)  - F1(3) -  G1(2)
///       \                               \  E3(9)  - F3(8) -  G3(7)
///        \  B2(15)  -  C2(14)  -  D2(13) - E2(12) - F2(11) - G2(10)
///
pub fn get_vector_040() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![
        (
            bitcoin_blocks::A1(None),
            expect_chain_updated_with_block(bitcoin_blocks::A1(None), vec![]),
        ),
        (bitcoin_blocks::G1(None), expect_no_chain_update()),
        (bitcoin_blocks::F1(None), expect_no_chain_update()),
        (bitcoin_blocks::E1(None), expect_no_chain_update()),
        (bitcoin_blocks::D1(None), expect_no_chain_update()),
        (bitcoin_blocks::C1(None), expect_no_chain_update()),
        (bitcoin_blocks::G3(None), expect_no_chain_update()),
        (bitcoin_blocks::F3(None), expect_no_chain_update()),
        (
            bitcoin_blocks::E3(Some(bitcoin_blocks::D1(None))),
            expect_no_chain_update(),
        ),
        (bitcoin_blocks::G2(None), expect_no_chain_update()),
        (bitcoin_blocks::F2(None), expect_no_chain_update()),
        (bitcoin_blocks::E2(None), expect_no_chain_update()),
        (bitcoin_blocks::D2(None), expect_no_chain_update()),
        (bitcoin_blocks::C2(None), expect_no_chain_update()),
        (
            bitcoin_blocks::B2(None),
            expect_chain_updated_with_blocks(
                vec![
                    bitcoin_blocks::B2(None),
                    bitcoin_blocks::C2(None),
                    bitcoin_blocks::D2(None),
                    bitcoin_blocks::E2(None),
                    bitcoin_blocks::F2(None),
                    bitcoin_blocks::G2(None),
                ],
                vec![bitcoin_blocks::A1(None)],
            ),
        ),
        (bitcoin_blocks::B1(None), expect_no_chain_update()),
    ]
}

/// Vector 041: Generate the following blocks
///  
/// A1(1) - B1(2) - C1(3) -  D1(5) - E1(8) - F1(9)
///               \ C2(4)  - D2(6) - E2(7) - F2(10) - G2(11)
///
pub fn get_vector_041() -> Vec<(BitcoinBlockData, BlockchainEventExpectation)> {
    vec![]
}
