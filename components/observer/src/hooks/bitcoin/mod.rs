use super::types::PoxConfig;
use crate::types::BitcoinNetwork;

pub fn get_stacks_canonical_magic_bytes(network: &BitcoinNetwork) -> [u8; 2] {
    match network {
        BitcoinNetwork::Mainnet => *b"X2",
        BitcoinNetwork::Testnet => *b"T2",
        BitcoinNetwork::Regtest => *b"id",
        BitcoinNetwork::Signet => unreachable!(),
    }
}

pub fn get_canonical_pox_config(network: &BitcoinNetwork) -> PoxConfig {
    match network {
        BitcoinNetwork::Mainnet => PoxConfig::mainnet_default(),
        BitcoinNetwork::Testnet => PoxConfig::testnet_default(),
        BitcoinNetwork::Regtest => PoxConfig::devnet_default(),
        BitcoinNetwork::Signet => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
#[repr(u8)]
pub enum StacksOpcodes {
    BlockCommit = b'[',
    KeyRegister = b'^',
    StackStx = b'x',
    PreStx = b'p',
    TransferStx = b'$',
}

impl TryFrom<u8> for StacksOpcodes {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == StacksOpcodes::BlockCommit as u8 => Ok(StacksOpcodes::BlockCommit),
            x if x == StacksOpcodes::KeyRegister as u8 => Ok(StacksOpcodes::KeyRegister),
            x if x == StacksOpcodes::StackStx as u8 => Ok(StacksOpcodes::StackStx),
            x if x == StacksOpcodes::PreStx as u8 => Ok(StacksOpcodes::PreStx),
            x if x == StacksOpcodes::TransferStx as u8 => Ok(StacksOpcodes::TransferStx),
            _ => Err(()),
        }
    }
}
