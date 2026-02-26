use serde::{Deserialize, Serialize};

use super::TransactionIdentifier;

/// A transaction input, which defines old coins to be consumed
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Serialize, Deserialize)]
pub struct TxIn {
    /// The reference to the previous output that is being used an an input.
    pub previous_output: OutPoint,
    /// The script which pushes values on the stack which will cause
    /// the referenced output's script to be accepted.
    pub script_sig: String,
    /// The sequence number, which suggests to miners which of two
    /// conflicting transactions should be preferred, or 0xFFFFFFFF
    /// to ignore this feature. This is generally never used since
    /// the miner behaviour cannot be enforced.
    pub sequence: u32,
    /// Witness data: an array of byte-arrays.
    /// Note that this field is *not* (de)serialized with the rest of the TxIn in
    /// Encodable/Decodable, as it is (de)serialized at the end of the full
    /// Transaction. It *is* (de)serialized with the rest of the TxIn in other
    /// (de)serialization routines.
    pub witness: Vec<String>,
}

/// A transaction output, which defines new coins to be created from old ones.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Serialize, Deserialize)]
pub struct TxOut {
    /// The value of the output, in satoshis.
    pub value: u64,
    /// The script which must be satisfied for the output to be spent.
    pub script_pubkey: String,
}

/// A reference to a transaction output.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct OutPoint {
    /// The referenced transaction's txid.
    pub txid: TransactionIdentifier,
    /// The index of the referenced output in its transaction's vout.
    pub vout: u32,
    /// The value of the referenced.
    pub value: u64,
    /// The script which must be satisfied for the output to be spent.
    pub block_height: u64,
}

impl TxOut {
    pub fn get_script_pubkey_bytes(&self) -> Vec<u8> {
        hex::decode(self.get_script_pubkey_hex()).expect("not provided for coinbase txs")
    }

    pub fn get_script_pubkey_hex(&self) -> &str {
        &self.script_pubkey[2..]
    }
}

/// The Witness is the data used to unlock bitcoins since the [segwit upgrade](https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki)
///
/// Can be logically seen as an array of byte-arrays `Vec<Vec<u8>>` and indeed you can convert from
/// it [`Witness::from_vec`] and convert into it [`Witness::to_vec`].
///
/// For serialization and deserialization performance it is stored internally as a single `Vec`,
/// saving some allocations.
///
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Serialize, Deserialize)]
pub struct Witness {
    /// contains the witness `Vec<Vec<u8>>` serialization without the initial varint indicating the
    /// number of elements (which is stored in `witness_elements`)
    content: Vec<u8>,

    /// Number of elements in the witness.
    /// It is stored separately (instead of as VarInt in the initial part of content) so that method
    /// like [`Witness::push`] doesn't have case requiring to shift the entire array
    witness_elements: usize,

    /// If `witness_elements > 0` it's a valid index pointing to the last witness element in `content`
    /// (Including the varint specifying the length of the element)
    last: usize,

    /// If `witness_elements > 1` it's a valid index pointing to the second-to-last witness element in `content`
    /// (Including the varint specifying the length of the element)
    second_to_last: usize,
}
