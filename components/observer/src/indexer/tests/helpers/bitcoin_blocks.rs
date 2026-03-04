use crate::types::{
    BitcoinBlockData, BitcoinBlockMetadata, BitcoinTransactionData, BlockIdentifier,
};

pub fn generate_test_bitcoin_block(
    fork_id: u8,
    block_height: u64,
    transactions: Vec<BitcoinTransactionData>,
    parent: Option<BitcoinBlockData>,
) -> BitcoinBlockData {
    let mut hash = vec![
        fork_id, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

    let parent_height = match block_height {
        0 => 0,
        _ => block_height - 1,
    };

    let parent_block_identifier = match parent {
        Some(parent) => {
            assert_eq!(parent.block_identifier.index, parent_height);
            parent.block_identifier.clone()
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
            BlockIdentifier {
                index: parent_height,
                hash: format!("0x{}", hex::encode(&parent_hash[..])),
            }
        }
    };
    hash.append(&mut block_height.to_be_bytes().to_vec());
    BitcoinBlockData {
        block_identifier: BlockIdentifier {
            index: block_height,
            hash: format!("0x{}", hex::encode(&hash[..])),
        },
        parent_block_identifier,
        timestamp: 0,
        transactions,
        metadata: BitcoinBlockMetadata {
            network: crate::types::BitcoinNetwork::Regtest,
        },
    }
}

pub fn A1(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(0, 1, vec![], parent)
}

pub fn B1(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(1, 2, vec![], parent)
}

pub fn B2(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(2, 2, vec![], parent)
}

pub fn C1(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(1, 3, vec![], parent)
}

pub fn C2(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(2, 3, vec![], parent)
}

pub fn D1(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(1, 4, vec![], parent)
}

pub fn D2(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(2, 4, vec![], parent)
}

pub fn E1(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(1, 5, vec![], parent)
}

pub fn E2(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(2, 5, vec![], parent)
}

pub fn B3(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(3, 2, vec![], parent)
}

pub fn C3(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(3, 3, vec![], parent)
}

pub fn D3(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(3, 4, vec![], parent)
}

pub fn E3(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(3, 5, vec![], parent)
}

pub fn F1(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(1, 6, vec![], parent)
}

pub fn F2(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(2, 6, vec![], parent)
}

pub fn F3(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(3, 6, vec![], parent)
}

pub fn G1(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(1, 7, vec![], parent)
}

pub fn G2(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(2, 7, vec![], parent)
}

pub fn G3(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(3, 7, vec![], parent)
}

pub fn H1(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(1, 8, vec![], parent)
}

pub fn H3(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(3, 8, vec![], parent)
}

pub fn I1(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(1, 9, vec![], parent)
}

pub fn I3(parent: Option<BitcoinBlockData>) -> BitcoinBlockData {
    generate_test_bitcoin_block(3, 9, vec![], parent)
}
