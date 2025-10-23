use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::io::{Read, Write};
use std::ops::{Deref, DerefMut};
use std::str::FromStr;

pub use clarity::codec::StacksMessageCodec;
use clarity::codec::{
    read_next, read_next_exact, write_next, Error as CodecError, MAX_MESSAGE_LEN,
};
use clarity::types::chainstate::{
    BlockHeaderHash, BurnchainHeaderHash, ConsensusHash, StacksAddress, StacksBlockId,
    StacksPublicKey, StacksWorkScore, TrieHash,
};
use clarity::types::{PrivateKey, StacksEpochId};
use clarity::vm::types::{
    PrincipalData, QualifiedContractIdentifier, StandardPrincipalData, TupleData, Value,
};
use clarity::vm::{ClarityName, ClarityVersion, ContractName};
use clarity::{
    impl_array_hexstring_fmt, impl_array_newtype, impl_byte_array_message_codec,
    impl_byte_array_serde,
};
use serde::{Deserialize, Serialize};
use stacks_common::address::{
    AddressHashMode, C32_ADDRESS_VERSION_MAINNET_MULTISIG, C32_ADDRESS_VERSION_MAINNET_SINGLESIG,
    C32_ADDRESS_VERSION_TESTNET_MULTISIG, C32_ADDRESS_VERSION_TESTNET_SINGLESIG,
};
use stacks_common::util::hash::{Hash160, Sha256Sum, Sha512Trunc256Sum};
use stacks_common::util::retry::BoundReader;
use stacks_common::util::secp256k1::{
    MessageSignature, Secp256k1PrivateKey, Secp256k1PublicKey, MESSAGE_SIGNATURE_ENCODED_SIZE,
};
use stacks_common::util::vrf::VRFProof;

use crate::impl_byte_array_newtype;

pub const MAX_BLOCK_LEN: u32 = 2 * 1024 * 1024;
pub const MAX_TRANSACTION_LEN: u32 = MAX_BLOCK_LEN;

/// Define a "u8" enum
///  gives you a try_from(u8) -> Option<Self> function
#[macro_export]
macro_rules! define_u8_enum {
    ($(#[$outer:meta])*
     $Name:ident {
         $(
             $(#[$inner:meta])*
             $Variant:ident = $Val:literal),+
     }) =>
    {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
        #[repr(u8)]
        $(#[$outer])*
        pub enum $Name {
            $(  $(#[$inner])*
                $Variant = $Val),*,
        }
        impl $Name {
            /// All members of the enum
            pub const ALL: &'static [$Name] = &[$($Name::$Variant),*];

            /// Return the u8 representation of the variant
            pub fn to_u8(&self) -> u8 {
                match self {
                    $(
                        $Name::$Variant => $Val,
                    )*
                }
            }

            /// Returns Some and the variant if `v` is a u8 corresponding to a variant in this enum.
            /// Returns None otherwise
            pub fn from_u8(v: u8) -> Option<Self> {
                match v {
                    $(
                        v if v == $Name::$Variant as u8 => Some($Name::$Variant),
                    )*
                    _ => None
                }
            }
        }
    }
}

#[macro_export]
macro_rules! impl_byte_array_message_codec {
    ($thing:ident, $len:expr) => {
        impl clarity::codec::StacksMessageCodec for $thing {
            fn consensus_serialize<W: std::io::Write>(
                &self,
                fd: &mut W,
            ) -> Result<(), clarity::codec::Error> {
                fd.write_all(self.as_bytes())
                    .map_err(clarity::codec::Error::WriteError)
            }
            fn consensus_deserialize<R: std::io::Read>(
                fd: &mut R,
            ) -> Result<$thing, clarity::codec::Error> {
                let mut buf = [0u8; ($len as usize)];
                fd.read_exact(&mut buf)
                    .map_err(clarity::codec::Error::ReadError)?;
                let ret = $thing::from_bytes(&buf).expect("BUG: buffer is not the right size");
                Ok(ret)
            }
        }
    };
}

/// Borrowed from Andrew Poelstra's rust-bitcoin
#[macro_export]
macro_rules! impl_array_newtype {
    ($thing:ident, $ty:ty, $len:expr) => {
        impl $thing {
            #[inline]
            #[allow(dead_code)]
            /// Converts the object to a raw pointer
            pub fn as_ptr(&self) -> *const $ty {
                let &$thing(ref dat) = self;
                dat.as_ptr()
            }

            #[inline]
            #[allow(dead_code)]
            /// Converts the object to a mutable raw pointer
            pub fn as_mut_ptr(&mut self) -> *mut $ty {
                let &mut $thing(ref mut dat) = self;
                dat.as_mut_ptr()
            }

            #[inline]
            #[allow(dead_code)]
            /// Returns the length of the object as an array
            pub fn len(&self) -> usize {
                $len
            }

            #[inline]
            #[allow(dead_code)]
            /// Returns whether the object, as an array, is empty. Always false.
            pub fn is_empty(&self) -> bool {
                false
            }

            #[inline]
            #[allow(dead_code)]
            /// Returns the underlying bytes.
            pub fn as_bytes(&self) -> &[$ty; $len] {
                &self.0
            }

            #[inline]
            #[allow(dead_code)]
            /// Returns the underlying bytes.
            pub fn to_bytes(&self) -> [$ty; $len] {
                self.0.clone()
            }

            #[inline]
            #[allow(dead_code)]
            /// Returns the underlying bytes.
            pub fn into_bytes(self) -> [$ty; $len] {
                self.0
            }
        }

        impl<'a> From<&'a [$ty]> for $thing {
            fn from(data: &'a [$ty]) -> $thing {
                assert_eq!(data.len(), $len);
                let mut ret = [0; $len];
                ret.copy_from_slice(&data[..]);
                $thing(ret)
            }
        }

        impl ::std::ops::Index<usize> for $thing {
            type Output = $ty;

            #[inline]
            fn index(&self, index: usize) -> &$ty {
                let &$thing(ref dat) = self;
                &dat[index]
            }
        }

        impl_index_newtype!($thing, $ty);

        impl PartialEq for $thing {
            #[inline]
            fn eq(&self, other: &$thing) -> bool {
                &self[..] == &other[..]
            }
        }

        impl Eq for $thing {}

        impl PartialOrd for $thing {
            #[inline]
            fn partial_cmp(&self, other: &$thing) -> Option<::std::cmp::Ordering> {
                Some(self.cmp(&other))
            }
        }

        impl Ord for $thing {
            #[inline]
            fn cmp(&self, other: &$thing) -> ::std::cmp::Ordering {
                // manually implement comparison to get little-endian ordering
                // (we need this for our numeric types; non-numeric ones shouldn't
                // be ordered anyway except to put them in BTrees or whatever, and
                // they don't care how we order as long as we're consisistent).
                for i in 0..$len {
                    if self[$len - 1 - i] < other[$len - 1 - i] {
                        return ::std::cmp::Ordering::Less;
                    }
                    if self[$len - 1 - i] > other[$len - 1 - i] {
                        return ::std::cmp::Ordering::Greater;
                    }
                }
                ::std::cmp::Ordering::Equal
            }
        }

        // #[cfg_attr(allow(expl_impl_clone_on_copy))] // we don't define the `struct`, we have to explicitly impl
        impl Clone for $thing {
            #[inline]
            fn clone(&self) -> $thing {
                *self
            }
        }

        impl Copy for $thing {}

        impl ::std::hash::Hash for $thing {
            #[inline]
            fn hash<H>(&self, state: &mut H)
            where
                H: ::std::hash::Hasher,
            {
                (&self[..]).hash(state);
            }

            fn hash_slice<H>(data: &[$thing], state: &mut H)
            where
                H: ::std::hash::Hasher,
            {
                for d in data.iter() {
                    (&d[..]).hash(state);
                }
            }
        }
    };
}

#[macro_export]
macro_rules! impl_index_newtype {
    ($thing:ident, $ty:ty) => {
        impl ::std::ops::Index<::std::ops::Range<usize>> for $thing {
            type Output = [$ty];

            #[inline]
            fn index(&self, index: ::std::ops::Range<usize>) -> &[$ty] {
                &self.0[index]
            }
        }

        impl ::std::ops::Index<::std::ops::RangeTo<usize>> for $thing {
            type Output = [$ty];

            #[inline]
            fn index(&self, index: ::std::ops::RangeTo<usize>) -> &[$ty] {
                &self.0[index]
            }
        }

        impl ::std::ops::Index<::std::ops::RangeFrom<usize>> for $thing {
            type Output = [$ty];

            #[inline]
            fn index(&self, index: ::std::ops::RangeFrom<usize>) -> &[$ty] {
                &self.0[index]
            }
        }

        impl ::std::ops::Index<::std::ops::RangeFull> for $thing {
            type Output = [$ty];

            #[inline]
            fn index(&self, _: ::std::ops::RangeFull) -> &[$ty] {
                &self.0[..]
            }
        }
    };
}

/// How a transaction may be appended to the Stacks blockchain
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum TransactionAnchorMode {
    OnChainOnly = 1,  // must be included in a StacksBlock
    OffChainOnly = 2, // must be included in a StacksMicroBlock
    Any = 3,          // either
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum TransactionAuthFlags {
    // types of auth
    AuthStandard = 0x04,
    AuthSponsored = 0x05,
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
/// This data structure represents a list of booleans
/// as a bitvector.
///
/// The generic argument `MAX_SIZE` specifies the maximum number of
/// elements that the bit vector can hold. It is not the _actual_ size
/// of the bitvec: if there are only 8 entries, the bitvector will
/// just have a single byte, even if the MAX_SIZE is u16::MAX. This
/// type parameter ensures that constructors and deserialization routines
/// error if input data is too long.
pub struct BitVec<const MAX_SIZE: u16> {
    data: Vec<u8>,
    len: u16,
}

impl<const MAX_SIZE: u16> StacksMessageCodec for BitVec<MAX_SIZE> {
    fn consensus_serialize<W: std::io::Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.len)?;
        write_next(fd, &self.data)
    }

    fn consensus_deserialize<R: std::io::Read>(fd: &mut R) -> Result<Self, CodecError> {
        let len = read_next(fd)?;
        if len == 0 {
            return Err(CodecError::DeserializeError(
                "BitVec lengths must be positive".to_string(),
            ));
        }
        if len > MAX_SIZE {
            return Err(CodecError::DeserializeError(format!(
                "BitVec length exceeded maximum. Max size = {MAX_SIZE}, len = {len}"
            )));
        }

        let data = read_next_exact(fd, Self::data_len(len).into())?;
        Ok(BitVec { data, len })
    }
}

impl<const MAX_SIZE: u16> BitVec<MAX_SIZE> {
    /// Return the number of bytes needed to store `len` bits.
    fn data_len(len: u16) -> u16 {
        len / 8 + if len.is_multiple_of(8) { 0 } else { 1 }
    }
}

/// Transaction signatures are validated by calculating the public key from the signature, and
/// verifying that all public keys hash to the signing account's hash.  To do so, we must preserve
/// enough information in the auth structure to recover each public key's bytes.
///
/// An auth field can be a public key or a signature.  In both cases, the public key (either given
/// in-the-raw or embedded in a signature) may be encoded as compressed or uncompressed.
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum TransactionAuthFieldID {
    // types of auth fields
    PublicKeyCompressed = 0x00,
    PublicKeyUncompressed = 0x01,
    SignatureCompressed = 0x02,
    SignatureUncompressed = 0x03,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum TransactionPublicKeyEncoding {
    // ways we can encode a public key
    Compressed = 0x00,
    Uncompressed = 0x01,
}

impl TransactionPublicKeyEncoding {
    pub fn from_u8(n: u8) -> Option<TransactionPublicKeyEncoding> {
        match n {
            x if x == TransactionPublicKeyEncoding::Compressed as u8 => {
                Some(TransactionPublicKeyEncoding::Compressed)
            }
            x if x == TransactionPublicKeyEncoding::Uncompressed as u8 => {
                Some(TransactionPublicKeyEncoding::Uncompressed)
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TransactionAuthField {
    PublicKey(StacksPublicKey),
    Signature(TransactionPublicKeyEncoding, MessageSignature),
}

impl TransactionAuthField {
    pub fn is_public_key(&self) -> bool {
        matches!(*self, TransactionAuthField::PublicKey(_))
    }

    pub fn is_signature(&self) -> bool {
        matches!(*self, TransactionAuthField::Signature(_, _))
    }

    pub fn as_public_key(&self) -> Option<Secp256k1PublicKey> {
        match *self {
            #[allow(clippy::clone_on_copy)]
            TransactionAuthField::PublicKey(ref pubk) => Some(pubk.clone()),
            _ => None,
        }
    }

    pub fn as_signature(&self) -> Option<(TransactionPublicKeyEncoding, MessageSignature)> {
        match *self {
            TransactionAuthField::Signature(ref key_fmt, ref sig) => Some((*key_fmt, sig.clone())),
            _ => None,
        }
    }

    // TODO: enforce u8; 32
    pub fn get_public_key(&self, sighash_bytes: &[u8]) -> Result<Secp256k1PublicKey, CodecError> {
        match *self {
            // wasm does not compile with *pubk instead of pubk.clone()
            #[allow(clippy::clone_on_copy)]
            TransactionAuthField::PublicKey(ref pubk) => Ok(pubk.clone()),
            TransactionAuthField::Signature(ref key_fmt, ref sig) => {
                let mut pubk = Secp256k1PublicKey::recover_to_pubkey(sighash_bytes, sig)
                    .map_err(|e| CodecError::SigningError(e.to_string()))?;
                pubk.set_compressed(*key_fmt == TransactionPublicKeyEncoding::Compressed);
                Ok(pubk)
            }
        }
    }
}

// tag address hash modes as "singlesig" or "multisig" so we can't accidentally construct an
// invalid spending condition
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SinglesigHashMode {
    P2PKH = 0x00,
    P2WPKH = 0x02,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum MultisigHashMode {
    P2SH = 0x01,
    P2WSH = 0x03,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum OrderIndependentMultisigHashMode {
    P2SH = 0x05,
    P2WSH = 0x07,
}

impl SinglesigHashMode {
    pub fn to_address_hash_mode(&self) -> AddressHashMode {
        match *self {
            SinglesigHashMode::P2PKH => AddressHashMode::SerializeP2PKH,
            SinglesigHashMode::P2WPKH => AddressHashMode::SerializeP2WPKH,
        }
    }

    pub fn from_address_hash_mode(hm: AddressHashMode) -> Option<SinglesigHashMode> {
        match hm {
            AddressHashMode::SerializeP2PKH => Some(SinglesigHashMode::P2PKH),
            AddressHashMode::SerializeP2WPKH => Some(SinglesigHashMode::P2WPKH),
            _ => None,
        }
    }

    pub fn from_u8(n: u8) -> Option<SinglesigHashMode> {
        match n {
            x if x == SinglesigHashMode::P2PKH as u8 => Some(SinglesigHashMode::P2PKH),
            x if x == SinglesigHashMode::P2WPKH as u8 => Some(SinglesigHashMode::P2WPKH),
            _ => None,
        }
    }
}

impl MultisigHashMode {
    pub fn to_address_hash_mode(&self) -> AddressHashMode {
        match *self {
            MultisigHashMode::P2SH => AddressHashMode::SerializeP2SH,
            MultisigHashMode::P2WSH => AddressHashMode::SerializeP2WSH,
        }
    }

    pub fn from_address_hash_mode(hm: AddressHashMode) -> Option<MultisigHashMode> {
        match hm {
            AddressHashMode::SerializeP2SH => Some(MultisigHashMode::P2SH),
            AddressHashMode::SerializeP2WSH => Some(MultisigHashMode::P2WSH),
            _ => None,
        }
    }

    pub fn from_u8(n: u8) -> Option<MultisigHashMode> {
        match n {
            x if x == MultisigHashMode::P2SH as u8 => Some(MultisigHashMode::P2SH),
            x if x == MultisigHashMode::P2WSH as u8 => Some(MultisigHashMode::P2WSH),
            _ => None,
        }
    }
}

impl OrderIndependentMultisigHashMode {
    pub fn to_address_hash_mode(&self) -> AddressHashMode {
        match *self {
            OrderIndependentMultisigHashMode::P2SH => AddressHashMode::SerializeP2SH,
            OrderIndependentMultisigHashMode::P2WSH => AddressHashMode::SerializeP2WSH,
        }
    }

    pub fn from_address_hash_mode(hm: AddressHashMode) -> Option<OrderIndependentMultisigHashMode> {
        match hm {
            AddressHashMode::SerializeP2SH => Some(OrderIndependentMultisigHashMode::P2SH),
            AddressHashMode::SerializeP2WSH => Some(OrderIndependentMultisigHashMode::P2WSH),
            _ => None,
        }
    }

    pub fn from_u8(n: u8) -> Option<OrderIndependentMultisigHashMode> {
        match n {
            x if x == OrderIndependentMultisigHashMode::P2SH as u8 => {
                Some(OrderIndependentMultisigHashMode::P2SH)
            }
            x if x == OrderIndependentMultisigHashMode::P2WSH as u8 => {
                Some(OrderIndependentMultisigHashMode::P2WSH)
            }
            _ => None,
        }
    }
}

/// A structure that encodes enough state to authenticate
/// a transaction's execution against a Stacks address.
/// public_keys + signatures_required determines the Principal.
/// nonce is the "check number" for the Principal.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MultisigSpendingCondition {
    pub hash_mode: MultisigHashMode,
    pub signer: Hash160,
    pub nonce: u64,  // nth authorization from this account
    pub tx_fee: u64, // microSTX/compute rate offered by this account
    pub fields: Vec<TransactionAuthField>,
    pub signatures_required: u16,
}

impl MultisigSpendingCondition {
    pub fn push_signature(
        &mut self,
        key_encoding: TransactionPublicKeyEncoding,
        signature: MessageSignature,
    ) {
        self.fields
            .push(TransactionAuthField::Signature(key_encoding, signature));
    }

    pub fn push_public_key(&mut self, public_key: Secp256k1PublicKey) {
        self.fields
            .push(TransactionAuthField::PublicKey(public_key));
    }

    pub fn pop_auth_field(&mut self) -> Option<TransactionAuthField> {
        self.fields.pop()
    }

    pub fn address_mainnet(&self) -> StacksAddress {
        StacksAddress::new(C32_ADDRESS_VERSION_MAINNET_MULTISIG, self.signer.clone()).unwrap()
    }

    pub fn address_testnet(&self) -> StacksAddress {
        StacksAddress::new(C32_ADDRESS_VERSION_TESTNET_MULTISIG, self.signer.clone()).unwrap()
    }

    /// Authenticate a spending condition against an initial sighash.
    /// In doing so, recover all public keys and verify that they hash to the signer
    /// via the given hash mode.
    pub fn verify(
        &self,
        initial_sighash: &Txid,
        cond_code: &TransactionAuthFlags,
    ) -> Result<Txid, CodecError> {
        let mut pubkeys = vec![];
        let mut cur_sighash = *initial_sighash;
        let mut num_sigs: u16 = 0;
        let mut have_uncompressed = false;
        for field in self.fields.iter() {
            let pubkey = match field {
                TransactionAuthField::PublicKey(ref pubkey) => {
                    if !pubkey.compressed() {
                        have_uncompressed = true;
                    }
                    #[allow(clippy::clone_on_copy)]
                    pubkey.clone()
                }
                TransactionAuthField::Signature(ref pubkey_encoding, ref sigbuf) => {
                    if *pubkey_encoding == TransactionPublicKeyEncoding::Uncompressed {
                        have_uncompressed = true;
                    }

                    let (pubkey, next_sighash) = TransactionSpendingCondition::next_verification(
                        &cur_sighash,
                        cond_code,
                        self.tx_fee,
                        self.nonce,
                        pubkey_encoding,
                        sigbuf,
                    )?;
                    cur_sighash = next_sighash;
                    num_sigs = num_sigs
                        .checked_add(1)
                        .ok_or(CodecError::SigningError("Too many signatures".to_string()))?;
                    pubkey
                }
            };
            pubkeys.push(pubkey);
        }

        if num_sigs != self.signatures_required {
            return Err(CodecError::SigningError(
                "Incorrect number of signatures".to_string(),
            ));
        }

        if have_uncompressed && self.hash_mode == MultisigHashMode::P2WSH {
            return Err(CodecError::SigningError(
                "Uncompressed keys are not allowed in this hash mode".to_string(),
            ));
        }

        let addr_bytes = match StacksAddress::from_public_keys(
            0,
            &self.hash_mode.to_address_hash_mode(),
            self.signatures_required as usize,
            &pubkeys,
        ) {
            Some(a) => a.bytes().clone(),
            None => {
                return Err(CodecError::SigningError(
                    "Failed to generate address from public keys".to_string(),
                ));
            }
        };

        if addr_bytes != self.signer {
            return Err(CodecError::SigningError(format!(
                "Signer hash does not equal hash of public key(s): {} != {}",
                addr_bytes, self.signer
            )));
        }

        Ok(cur_sighash)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SinglesigSpendingCondition {
    pub hash_mode: SinglesigHashMode,
    pub signer: Hash160,
    pub nonce: u64,  // nth authorization from this account
    pub tx_fee: u64, // microSTX/compute rate offerred by this account
    pub key_encoding: TransactionPublicKeyEncoding,
    pub signature: MessageSignature,
}

impl SinglesigSpendingCondition {
    pub fn set_signature(&mut self, signature: MessageSignature) {
        self.signature = signature;
    }

    pub fn pop_signature(&mut self) -> Option<TransactionAuthField> {
        if self.signature == MessageSignature::empty() {
            return None;
        }

        let ret = self.signature.clone();
        self.signature = MessageSignature::empty();

        Some(TransactionAuthField::Signature(self.key_encoding, ret))
    }

    pub fn address_mainnet(&self) -> StacksAddress {
        let version = match self.hash_mode {
            SinglesigHashMode::P2PKH => C32_ADDRESS_VERSION_MAINNET_SINGLESIG,
            SinglesigHashMode::P2WPKH => C32_ADDRESS_VERSION_MAINNET_MULTISIG,
        };
        StacksAddress::new(version, self.signer.clone()).unwrap()
    }

    pub fn address_testnet(&self) -> StacksAddress {
        let version = match self.hash_mode {
            SinglesigHashMode::P2PKH => C32_ADDRESS_VERSION_TESTNET_SINGLESIG,
            SinglesigHashMode::P2WPKH => C32_ADDRESS_VERSION_TESTNET_MULTISIG,
        };
        StacksAddress::new(version, self.signer.clone()).unwrap()
    }

    /// Authenticate a spending condition against an initial sighash.
    /// In doing so, recover all public keys and verify that they hash to the signer
    /// via the given hash mode.
    /// Returns the final sighash
    pub fn verify(
        &self,
        initial_sighash: &Txid,
        cond_code: &TransactionAuthFlags,
    ) -> Result<Txid, CodecError> {
        let (pubkey, next_sighash) = TransactionSpendingCondition::next_verification(
            initial_sighash,
            cond_code,
            self.tx_fee,
            self.nonce,
            &self.key_encoding,
            &self.signature,
        )?;
        let addr_bytes = match StacksAddress::from_public_keys(
            0,
            &self.hash_mode.to_address_hash_mode(),
            1,
            &vec![pubkey],
        ) {
            Some(a) => a.bytes().clone(),
            None => {
                return Err(CodecError::SigningError(
                    "Failed to generate address from public key".to_string(),
                ));
            }
        };

        if addr_bytes != self.signer {
            return Err(CodecError::SigningError(format!(
                "Signer hash does not equal hash of public key(s): {} != {}",
                &addr_bytes, &self.signer
            )));
        }

        Ok(next_sighash)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OrderIndependentMultisigSpendingCondition {
    pub hash_mode: OrderIndependentMultisigHashMode,
    pub signer: Hash160,
    pub nonce: u64,  // nth authorization from this account
    pub tx_fee: u64, // microSTX/compute rate offered by this account
    pub fields: Vec<TransactionAuthField>,
    pub signatures_required: u16,
}

impl OrderIndependentMultisigSpendingCondition {
    pub fn push_signature(
        &mut self,
        key_encoding: TransactionPublicKeyEncoding,
        signature: MessageSignature,
    ) {
        self.fields
            .push(TransactionAuthField::Signature(key_encoding, signature));
    }

    pub fn push_public_key(&mut self, public_key: StacksPublicKey) {
        self.fields
            .push(TransactionAuthField::PublicKey(public_key));
    }

    pub fn pop_auth_field(&mut self) -> Option<TransactionAuthField> {
        self.fields.pop()
    }

    pub fn address_mainnet(&self) -> StacksAddress {
        StacksAddress::new(C32_ADDRESS_VERSION_MAINNET_MULTISIG, self.signer.clone()).unwrap()
    }

    pub fn address_testnet(&self) -> StacksAddress {
        StacksAddress::new(C32_ADDRESS_VERSION_TESTNET_MULTISIG, self.signer.clone()).unwrap()
    }

    /// Authenticate a spending condition against an initial sighash.
    /// In doing so, recover all public keys and verify that they hash to the signer
    /// via the given hash mode.
    pub fn verify(
        &self,
        initial_sighash: &Txid,
        cond_code: &TransactionAuthFlags,
    ) -> Result<Txid, CodecError> {
        let mut pubkeys = vec![];
        let mut num_sigs: u16 = 0;
        let mut have_uncompressed = false;
        for field in self.fields.iter() {
            let pubkey = match field {
                TransactionAuthField::PublicKey(ref pubkey) => {
                    if !pubkey.compressed() {
                        have_uncompressed = true;
                    }
                    pubkey.clone()
                }
                TransactionAuthField::Signature(ref pubkey_encoding, ref sigbuf) => {
                    if *pubkey_encoding == TransactionPublicKeyEncoding::Uncompressed {
                        have_uncompressed = true;
                    }

                    let (pubkey, _next_sighash) = TransactionSpendingCondition::next_verification(
                        initial_sighash,
                        cond_code,
                        self.tx_fee,
                        self.nonce,
                        pubkey_encoding,
                        sigbuf,
                    )?;
                    num_sigs = num_sigs
                        .checked_add(1)
                        .ok_or(CodecError::SigningError("Too many signatures".to_string()))?;
                    pubkey
                }
            };
            pubkeys.push(pubkey);
        }

        if num_sigs < self.signatures_required {
            return Err(CodecError::SigningError(format!(
                "Not enough signatures. Got {num_sigs}, expected at least {req}",
                req = self.signatures_required
            )));
        }

        if have_uncompressed && self.hash_mode == OrderIndependentMultisigHashMode::P2WSH {
            return Err(CodecError::SigningError(
                "Uncompressed keys are not allowed in this hash mode".to_string(),
            ));
        }

        let addr_bytes = match StacksAddress::from_public_keys(
            0,
            &self.hash_mode.to_address_hash_mode(),
            self.signatures_required as usize,
            &pubkeys,
        ) {
            Some(a) => a.bytes().clone(),
            None => {
                return Err(CodecError::SigningError(
                    "Failed to generate address from public keys".to_string(),
                ));
            }
        };

        if addr_bytes != self.signer {
            return Err(CodecError::SigningError(format!(
                "Signer hash does not equal hash of public key(s): {} != {}",
                addr_bytes, self.signer
            )));
        }

        Ok(*initial_sighash)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TransactionSpendingCondition {
    Singlesig(SinglesigSpendingCondition),
    Multisig(MultisigSpendingCondition),
    OrderIndependentMultisig(OrderIndependentMultisigSpendingCondition),
}

impl TransactionSpendingCondition {
    pub fn new_singlesig_p2pkh(pubkey: StacksPublicKey) -> Option<TransactionSpendingCondition> {
        let key_encoding = if pubkey.compressed() {
            TransactionPublicKeyEncoding::Compressed
        } else {
            TransactionPublicKeyEncoding::Uncompressed
        };
        let signer_addr =
            StacksAddress::from_public_keys(0, &AddressHashMode::SerializeP2PKH, 1, &vec![pubkey])?;

        Some(TransactionSpendingCondition::Singlesig(
            SinglesigSpendingCondition {
                signer: signer_addr.bytes().clone(),
                nonce: 0,
                tx_fee: 0,
                hash_mode: SinglesigHashMode::P2PKH,
                key_encoding,
                signature: MessageSignature::empty(),
            },
        ))
    }

    pub fn new_singlesig_p2wpkh(pubkey: StacksPublicKey) -> Option<TransactionSpendingCondition> {
        let signer_addr = StacksAddress::from_public_keys(
            0,
            &AddressHashMode::SerializeP2WPKH,
            1,
            &vec![pubkey],
        )?;

        Some(TransactionSpendingCondition::Singlesig(
            SinglesigSpendingCondition {
                signer: signer_addr.bytes().clone(),
                nonce: 0,
                tx_fee: 0,
                hash_mode: SinglesigHashMode::P2WPKH,
                key_encoding: TransactionPublicKeyEncoding::Compressed,
                signature: MessageSignature::empty(),
            },
        ))
    }

    pub fn new_multisig_p2sh(
        num_sigs: u16,
        pubkeys: Vec<StacksPublicKey>,
    ) -> Option<TransactionSpendingCondition> {
        let signer_addr = StacksAddress::from_public_keys(
            0,
            &AddressHashMode::SerializeP2SH,
            usize::from(num_sigs),
            &pubkeys,
        )?;

        Some(TransactionSpendingCondition::Multisig(
            MultisigSpendingCondition {
                signer: signer_addr.bytes().clone(),
                nonce: 0,
                tx_fee: 0,
                hash_mode: MultisigHashMode::P2SH,
                fields: vec![],
                signatures_required: num_sigs,
            },
        ))
    }

    pub fn new_multisig_order_independent_p2sh(
        num_sigs: u16,
        pubkeys: Vec<StacksPublicKey>,
    ) -> Option<TransactionSpendingCondition> {
        let signer_addr = StacksAddress::from_public_keys(
            0,
            &AddressHashMode::SerializeP2SH,
            usize::from(num_sigs),
            &pubkeys,
        )?;

        Some(TransactionSpendingCondition::OrderIndependentMultisig(
            OrderIndependentMultisigSpendingCondition {
                signer: signer_addr.bytes().clone(),
                nonce: 0,
                tx_fee: 0,
                hash_mode: OrderIndependentMultisigHashMode::P2SH,
                fields: vec![],
                signatures_required: num_sigs,
            },
        ))
    }

    pub fn new_multisig_order_independent_p2wsh(
        num_sigs: u16,
        pubkeys: Vec<StacksPublicKey>,
    ) -> Option<TransactionSpendingCondition> {
        let signer_addr = StacksAddress::from_public_keys(
            0,
            &AddressHashMode::SerializeP2WSH,
            usize::from(num_sigs),
            &pubkeys,
        )?;

        Some(TransactionSpendingCondition::OrderIndependentMultisig(
            OrderIndependentMultisigSpendingCondition {
                signer: signer_addr.bytes().clone(),
                nonce: 0,
                tx_fee: 0,
                hash_mode: OrderIndependentMultisigHashMode::P2WSH,
                fields: vec![],
                signatures_required: num_sigs,
            },
        ))
    }

    pub fn new_multisig_p2wsh(
        num_sigs: u16,
        pubkeys: Vec<StacksPublicKey>,
    ) -> Option<TransactionSpendingCondition> {
        let signer_addr = StacksAddress::from_public_keys(
            0,
            &AddressHashMode::SerializeP2WSH,
            usize::from(num_sigs),
            &pubkeys,
        )?;

        Some(TransactionSpendingCondition::Multisig(
            MultisigSpendingCondition {
                signer: signer_addr.bytes().clone(),
                nonce: 0,
                tx_fee: 0,
                hash_mode: MultisigHashMode::P2WSH,
                fields: vec![],
                signatures_required: num_sigs,
            },
        ))
    }

    /// When committing to the fact that a transaction is sponsored, the origin doesn't know
    /// anything else.  Instead, it commits to this sentinel value as its sponsor.
    /// It is intractable to calculate a private key that could generate this.
    pub fn new_initial_sighash() -> TransactionSpendingCondition {
        TransactionSpendingCondition::Singlesig(SinglesigSpendingCondition {
            signer: Hash160([0u8; 20]),
            nonce: 0,
            tx_fee: 0,
            hash_mode: SinglesigHashMode::P2PKH,
            key_encoding: TransactionPublicKeyEncoding::Compressed,
            signature: MessageSignature::empty(),
        })
    }

    pub fn num_signatures(&self) -> u16 {
        match *self {
            TransactionSpendingCondition::Singlesig(ref data) => {
                if data.signature != MessageSignature::empty() {
                    1
                } else {
                    0
                }
            }
            TransactionSpendingCondition::Multisig(ref data) => {
                let mut num_sigs: u16 = 0;
                for field in data.fields.iter() {
                    if field.is_signature() {
                        num_sigs = num_sigs
                            .checked_add(1)
                            .expect("Unreasonable amount of signatures"); // something is seriously wrong if this fails
                    }
                }
                num_sigs
            }
            TransactionSpendingCondition::OrderIndependentMultisig(ref data) => {
                let mut num_sigs: u16 = 0;
                for field in data.fields.iter() {
                    if field.is_signature() {
                        num_sigs = num_sigs
                            .checked_add(1)
                            .expect("Unreasonable amount of signatures"); // something is seriously wrong if this fails
                    }
                }
                num_sigs
            }
        }
    }

    pub fn signatures_required(&self) -> u16 {
        match *self {
            TransactionSpendingCondition::Singlesig(_) => 1,
            TransactionSpendingCondition::Multisig(ref multisig_data) => {
                multisig_data.signatures_required
            }
            TransactionSpendingCondition::OrderIndependentMultisig(ref multisig_data) => {
                multisig_data.signatures_required
            }
        }
    }

    pub fn nonce(&self) -> u64 {
        match *self {
            TransactionSpendingCondition::Singlesig(ref data) => data.nonce,
            TransactionSpendingCondition::Multisig(ref data) => data.nonce,
            TransactionSpendingCondition::OrderIndependentMultisig(ref data) => data.nonce,
        }
    }

    pub fn tx_fee(&self) -> u64 {
        match *self {
            TransactionSpendingCondition::Singlesig(ref data) => data.tx_fee,
            TransactionSpendingCondition::Multisig(ref data) => data.tx_fee,
            TransactionSpendingCondition::OrderIndependentMultisig(ref data) => data.tx_fee,
        }
    }

    pub fn set_nonce(&mut self, n: u64) {
        match *self {
            TransactionSpendingCondition::Singlesig(ref mut singlesig_data) => {
                singlesig_data.nonce = n;
            }
            TransactionSpendingCondition::Multisig(ref mut multisig_data) => {
                multisig_data.nonce = n;
            }
            TransactionSpendingCondition::OrderIndependentMultisig(ref mut multisig_data) => {
                multisig_data.nonce = n;
            }
        }
    }

    pub fn set_tx_fee(&mut self, tx_fee: u64) {
        match *self {
            TransactionSpendingCondition::Singlesig(ref mut singlesig_data) => {
                singlesig_data.tx_fee = tx_fee;
            }
            TransactionSpendingCondition::Multisig(ref mut multisig_data) => {
                multisig_data.tx_fee = tx_fee;
            }
            TransactionSpendingCondition::OrderIndependentMultisig(ref mut multisig_data) => {
                multisig_data.tx_fee = tx_fee;
            }
        }
    }

    pub fn get_tx_fee(&self) -> u64 {
        match *self {
            TransactionSpendingCondition::Singlesig(ref singlesig_data) => singlesig_data.tx_fee,
            TransactionSpendingCondition::Multisig(ref multisig_data) => multisig_data.tx_fee,
            TransactionSpendingCondition::OrderIndependentMultisig(ref multisig_data) => {
                multisig_data.tx_fee
            }
        }
    }

    /// Get the mainnet account address of the spending condition
    pub fn address_mainnet(&self) -> StacksAddress {
        match *self {
            TransactionSpendingCondition::Singlesig(ref data) => data.address_mainnet(),
            TransactionSpendingCondition::Multisig(ref data) => data.address_mainnet(),
            TransactionSpendingCondition::OrderIndependentMultisig(ref data) => {
                data.address_mainnet()
            }
        }
    }

    /// Get the mainnet account address of the spending condition
    pub fn address_testnet(&self) -> StacksAddress {
        match *self {
            TransactionSpendingCondition::Singlesig(ref data) => data.address_testnet(),
            TransactionSpendingCondition::Multisig(ref data) => data.address_testnet(),
            TransactionSpendingCondition::OrderIndependentMultisig(ref data) => {
                data.address_testnet()
            }
        }
    }

    /// Get the address for an account, given the network flag
    pub fn get_address(&self, mainnet: bool) -> StacksAddress {
        if mainnet {
            self.address_mainnet()
        } else {
            self.address_testnet()
        }
    }

    /// Clear fee rate, nonces, signatures, and public keys
    pub fn clear(&mut self) {
        match *self {
            TransactionSpendingCondition::Singlesig(ref mut singlesig_data) => {
                singlesig_data.tx_fee = 0;
                singlesig_data.nonce = 0;
                singlesig_data.signature = MessageSignature::empty();
            }
            TransactionSpendingCondition::Multisig(ref mut multisig_data) => {
                multisig_data.tx_fee = 0;
                multisig_data.nonce = 0;
                multisig_data.fields.clear();
            }
            TransactionSpendingCondition::OrderIndependentMultisig(ref mut multisig_data) => {
                multisig_data.tx_fee = 0;
                multisig_data.nonce = 0;
                multisig_data.fields.clear();
            }
        }
    }

    pub fn make_sighash_presign(
        cur_sighash: &Txid,
        cond_code: &TransactionAuthFlags,
        tx_fee: u64,
        nonce: u64,
    ) -> Txid {
        // new hash combines the previous hash and all the new data this signature will add.  This
        // includes:
        // * the previous hash
        // * the auth flag
        // * the fee rate (big-endian 8-byte number)
        // * nonce (big-endian 8-byte number)
        let new_tx_hash_bits_len = 32 + 1 + 8 + 8;
        let mut new_tx_hash_bits = Vec::with_capacity(new_tx_hash_bits_len as usize);

        new_tx_hash_bits.extend_from_slice(cur_sighash.as_bytes());
        new_tx_hash_bits.extend_from_slice(&[*cond_code as u8]);
        new_tx_hash_bits.extend_from_slice(&tx_fee.to_be_bytes());
        new_tx_hash_bits.extend_from_slice(&nonce.to_be_bytes());

        assert!(new_tx_hash_bits.len() == new_tx_hash_bits_len as usize);

        Txid::from_sighash_bytes(&new_tx_hash_bits)
    }

    pub fn make_sighash_postsign(
        cur_sighash: &Txid,
        pubkey: &StacksPublicKey,
        sig: &MessageSignature,
    ) -> Txid {
        // new hash combines the previous hash and all the new data this signature will add.  This
        // includes:
        // * the public key compression flag
        // * the signature
        let new_tx_hash_bits_len = 32 + 1 + MESSAGE_SIGNATURE_ENCODED_SIZE;
        let mut new_tx_hash_bits = Vec::with_capacity(new_tx_hash_bits_len as usize);
        let pubkey_encoding = if pubkey.compressed() {
            TransactionPublicKeyEncoding::Compressed
        } else {
            TransactionPublicKeyEncoding::Uncompressed
        };

        new_tx_hash_bits.extend_from_slice(cur_sighash.as_bytes());
        new_tx_hash_bits.extend_from_slice(&[pubkey_encoding as u8]);
        new_tx_hash_bits.extend_from_slice(sig.as_bytes());

        assert!(new_tx_hash_bits.len() == new_tx_hash_bits_len as usize);

        Txid::from_sighash_bytes(&new_tx_hash_bits)
    }

    /// Linear-complexity signing algorithm -- we sign a rolling hash over all data committed to by
    /// the previous signer (instead of naively re-serializing the transaction each time), as well
    /// as over new data provided by this key (excluding its own public key or signature, which
    /// are authenticated by the spending condition's key hash).
    /// Calculates and returns the next signature and sighash, which the subsequent private key
    /// must sign.
    pub fn next_signature(
        cur_sighash: &Txid,
        cond_code: &TransactionAuthFlags,
        tx_fee: u64,
        nonce: u64,
        privk: &Secp256k1PrivateKey,
    ) -> Result<(MessageSignature, Txid), CodecError> {
        let sighash_presign = TransactionSpendingCondition::make_sighash_presign(
            cur_sighash,
            cond_code,
            tx_fee,
            nonce,
        );

        // sign the current hash
        let sig = privk
            .sign(sighash_presign.as_bytes())
            .map_err(|se| CodecError::SigningError(se.to_string()))?;

        let pubk = Secp256k1PublicKey::from_private(privk);
        let next_sighash =
            TransactionSpendingCondition::make_sighash_postsign(&sighash_presign, &pubk, &sig);

        Ok((sig, next_sighash))
    }

    /// Linear-complexity verifying algorithm -- we verify a rolling hash over all data committed
    /// to by order of signers (instead of re-serializing the transaction each time).
    /// Calculates the next sighash and public key, which the next verifier must verify.
    /// Used by StacksTransaction::verify*
    pub fn next_verification(
        cur_sighash: &Txid,
        cond_code: &TransactionAuthFlags,
        tx_fee: u64,
        nonce: u64,
        key_encoding: &TransactionPublicKeyEncoding,
        sig: &MessageSignature,
    ) -> Result<(StacksPublicKey, Txid), CodecError> {
        let sighash_presign = TransactionSpendingCondition::make_sighash_presign(
            cur_sighash,
            cond_code,
            tx_fee,
            nonce,
        );

        // verify the current signature
        let mut pubk = StacksPublicKey::recover_to_pubkey(sighash_presign.as_bytes(), sig)
            .map_err(|ve| CodecError::SigningError(ve.to_string()))?;

        match key_encoding {
            TransactionPublicKeyEncoding::Compressed => pubk.set_compressed(true),
            TransactionPublicKeyEncoding::Uncompressed => pubk.set_compressed(false),
        };

        // what's the next sighash going to be?
        let next_sighash =
            TransactionSpendingCondition::make_sighash_postsign(&sighash_presign, &pubk, sig);
        Ok((pubk, next_sighash))
    }

    /// Verify all signatures
    pub fn verify(
        &self,
        initial_sighash: &Txid,
        cond_code: &TransactionAuthFlags,
    ) -> Result<Txid, CodecError> {
        match *self {
            TransactionSpendingCondition::Singlesig(ref data) => {
                data.verify(initial_sighash, cond_code)
            }
            TransactionSpendingCondition::Multisig(ref data) => {
                data.verify(initial_sighash, cond_code)
            }
            TransactionSpendingCondition::OrderIndependentMultisig(ref data) => {
                data.verify(initial_sighash, cond_code)
            }
        }
    }
}

/// Types of transaction authorizations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TransactionAuth {
    Standard(TransactionSpendingCondition),
    Sponsored(TransactionSpendingCondition, TransactionSpendingCondition), // the second account pays on behalf of the first account
}

impl TransactionAuth {
    pub fn from_p2pkh(privk: &Secp256k1PrivateKey) -> Option<TransactionAuth> {
        TransactionSpendingCondition::new_singlesig_p2pkh(StacksPublicKey::from_private(privk))
            .map(TransactionAuth::Standard)
    }

    pub fn from_p2sh(privks: &[Secp256k1PrivateKey], num_sigs: u16) -> Option<TransactionAuth> {
        let mut pubks = vec![];
        for privk in privks.iter() {
            pubks.push(StacksPublicKey::from_private(privk));
        }

        TransactionSpendingCondition::new_multisig_p2sh(num_sigs, pubks)
            .map(TransactionAuth::Standard)
    }

    pub fn from_order_independent_p2sh(
        privks: &[Secp256k1PrivateKey],
        num_sigs: u16,
    ) -> Option<TransactionAuth> {
        let pubks = privks.iter().map(StacksPublicKey::from_private).collect();

        TransactionSpendingCondition::new_multisig_order_independent_p2sh(num_sigs, pubks)
            .map(TransactionAuth::Standard)
    }

    pub fn from_order_independent_p2wsh(
        privks: &[Secp256k1PrivateKey],
        num_sigs: u16,
    ) -> Option<TransactionAuth> {
        let pubks = privks.iter().map(StacksPublicKey::from_private).collect();

        TransactionSpendingCondition::new_multisig_order_independent_p2wsh(num_sigs, pubks)
            .map(TransactionAuth::Standard)
    }

    pub fn from_p2wpkh(privk: &Secp256k1PrivateKey) -> Option<TransactionAuth> {
        TransactionSpendingCondition::new_singlesig_p2wpkh(StacksPublicKey::from_private(privk))
            .map(TransactionAuth::Standard)
    }

    pub fn from_p2wsh(privks: &[Secp256k1PrivateKey], num_sigs: u16) -> Option<TransactionAuth> {
        let mut pubks = vec![];
        for privk in privks.iter() {
            pubks.push(StacksPublicKey::from_private(privk));
        }

        TransactionSpendingCondition::new_multisig_p2wsh(num_sigs, pubks)
            .map(TransactionAuth::Standard)
    }

    /// merge two standard auths into a sponsored auth.
    /// build them with the above helper methods
    pub fn into_sponsored(self, sponsor_auth: TransactionAuth) -> Option<TransactionAuth> {
        match (self, sponsor_auth) {
            (TransactionAuth::Standard(sc), TransactionAuth::Standard(sp)) => {
                Some(TransactionAuth::Sponsored(sc, sp))
            }
            (_, _) => None,
        }
    }

    /// Directly set the sponsor spending condition
    pub fn set_sponsor(
        &mut self,
        sponsor_spending_cond: TransactionSpendingCondition,
    ) -> Result<(), CodecError> {
        match *self {
            TransactionAuth::Sponsored(_, ref mut ssc) => {
                *ssc = sponsor_spending_cond;
                Ok(())
            }
            _ => Err(CodecError::GenericError(
                "IncompatibleSpendingConditionError".into(),
            )),
        }
    }

    pub fn is_standard(&self) -> bool {
        matches!(*self, TransactionAuth::Standard(_))
    }

    pub fn is_sponsored(&self) -> bool {
        matches!(*self, TransactionAuth::Sponsored(_, _))
    }

    /// When beginning to sign a sponsored transaction, the origin account will not commit to any
    /// information about the sponsor (only that it is sponsored).  It does so by using sentinel
    /// sponsored account information.
    pub fn into_initial_sighash_auth(self) -> TransactionAuth {
        match self {
            TransactionAuth::Standard(mut origin) => {
                origin.clear();
                TransactionAuth::Standard(origin)
            }
            TransactionAuth::Sponsored(mut origin, _) => {
                origin.clear();
                TransactionAuth::Sponsored(
                    origin,
                    TransactionSpendingCondition::new_initial_sighash(),
                )
            }
        }
    }

    pub fn origin(&self) -> &TransactionSpendingCondition {
        match *self {
            TransactionAuth::Standard(ref s) => s,
            TransactionAuth::Sponsored(ref s, _) => s,
        }
    }

    pub fn get_origin_nonce(&self) -> u64 {
        self.origin().nonce()
    }

    pub fn set_origin_nonce(&mut self, n: u64) {
        match *self {
            TransactionAuth::Standard(ref mut s) => s.set_nonce(n),
            TransactionAuth::Sponsored(ref mut s, _) => s.set_nonce(n),
        }
    }

    pub fn sponsor(&self) -> Option<&TransactionSpendingCondition> {
        match *self {
            TransactionAuth::Standard(_) => None,
            TransactionAuth::Sponsored(_, ref s) => Some(s),
        }
    }

    pub fn get_sponsor_nonce(&self) -> Option<u64> {
        self.sponsor().map(|s| s.nonce())
    }

    pub fn set_sponsor_nonce(&mut self, n: u64) -> Result<(), CodecError> {
        match *self {
            TransactionAuth::Standard(_) => Err(CodecError::GenericError(
                "IncompatibleSpendingConditionError".into(),
            )),
            TransactionAuth::Sponsored(_, ref mut s) => {
                s.set_nonce(n);
                Ok(())
            }
        }
    }

    pub fn set_tx_fee(&mut self, tx_fee: u64) {
        match *self {
            TransactionAuth::Standard(ref mut s) => s.set_tx_fee(tx_fee),
            TransactionAuth::Sponsored(_, ref mut s) => s.set_tx_fee(tx_fee),
        }
    }

    pub fn get_tx_fee(&self) -> u64 {
        match *self {
            TransactionAuth::Standard(ref s) => s.get_tx_fee(),
            TransactionAuth::Sponsored(_, ref s) => s.get_tx_fee(),
        }
    }

    pub fn verify_origin(&self, initial_sighash: &Txid) -> Result<Txid, CodecError> {
        match *self {
            TransactionAuth::Standard(ref origin_condition) => {
                origin_condition.verify(initial_sighash, &TransactionAuthFlags::AuthStandard)
            }
            TransactionAuth::Sponsored(ref origin_condition, _) => {
                origin_condition.verify(initial_sighash, &TransactionAuthFlags::AuthStandard)
            }
        }
    }

    pub fn verify(&self, initial_sighash: &Txid) -> Result<(), CodecError> {
        let origin_sighash = self.verify_origin(initial_sighash)?;
        match *self {
            TransactionAuth::Standard(_) => Ok(()),
            TransactionAuth::Sponsored(_, ref sponsor_condition) => sponsor_condition
                .verify(&origin_sighash, &TransactionAuthFlags::AuthSponsored)
                .map(|_sigh| ()),
        }
    }

    /// Clear out all transaction auth fields, nonces, and fee rates from the spending condition(s).
    pub fn clear(&mut self) {
        match *self {
            TransactionAuth::Standard(ref mut origin_condition) => {
                origin_condition.clear();
            }
            TransactionAuth::Sponsored(ref mut origin_condition, ref mut sponsor_condition) => {
                origin_condition.clear();
                sponsor_condition.clear();
            }
        }
    }

    /// Checks if this TransactionAuth is supported in the passed epoch
    /// OrderIndependent multisig is not supported before epoch 3.0
    pub fn is_supported_in_epoch(&self, epoch_id: StacksEpochId) -> bool {
        match &self {
            TransactionAuth::Sponsored(ref origin, ref sponsor) => {
                let origin_supported = match origin {
                    TransactionSpendingCondition::OrderIndependentMultisig(..) => {
                        epoch_id >= StacksEpochId::Epoch30
                    }
                    _ => true,
                };
                let sponsor_supported = match sponsor {
                    TransactionSpendingCondition::OrderIndependentMultisig(..) => {
                        epoch_id >= StacksEpochId::Epoch30
                    }
                    _ => true,
                };
                origin_supported && sponsor_supported
            }
            TransactionAuth::Standard(ref origin) => match origin {
                TransactionSpendingCondition::OrderIndependentMultisig(..) => {
                    epoch_id >= StacksEpochId::Epoch30
                }
                _ => true,
            },
        }
    }
}

/// A transaction that calls into a smart contract
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TransactionContractCall {
    pub address: StacksAddress,
    pub contract_name: ContractName,
    pub function_name: ClarityName,
    pub function_args: Vec<Value>,
}

/// printable-ASCII-only string, but encodable.
/// Note that it cannot be longer than ARRAY_MAX_LEN (4.1 billion bytes)
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct StacksString(Vec<u8>);

impl fmt::Display for StacksString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // guaranteed to always succeed because the string is ASCII
        f.write_str(String::from_utf8_lossy(self).into_owned().as_str())
    }
}

impl fmt::Debug for StacksString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(String::from_utf8_lossy(self).into_owned().as_str())
    }
}

impl std::str::FromStr for StacksString {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !StacksString::is_valid_string(&String::from(s)) {
            return Err("Invalid string".to_string());
        }
        Ok(StacksString(s.as_bytes().to_vec()))
    }
}

impl Deref for StacksString {
    type Target = Vec<u8>;
    fn deref(&self) -> &Vec<u8> {
        &self.0
    }
}

impl DerefMut for StacksString {
    fn deref_mut(&mut self) -> &mut Vec<u8> {
        &mut self.0
    }
}

impl From<ClarityName> for StacksString {
    fn from(clarity_name: ClarityName) -> StacksString {
        // .unwrap() is safe since StacksString is less strict
        StacksString::from_str(&clarity_name).unwrap()
    }
}

impl From<ContractName> for StacksString {
    fn from(contract_name: ContractName) -> StacksString {
        // .unwrap() is safe since StacksString is less strict
        StacksString::from_str(&contract_name).unwrap()
    }
}

impl StacksString {
    /// Is the given string a valid Clarity string?
    pub fn is_valid_string(s: &String) -> bool {
        s.is_ascii() && StacksString::is_printable(s)
    }

    pub fn is_printable(s: &String) -> bool {
        if !s.is_ascii() {
            return false;
        }
        // all characters must be ASCII "printable" characters, excluding "delete".
        // This is 0x20 through 0x7e, inclusive, as well as '\t' and '\n'
        // TODO: DRY up with vm::representations
        for c in s.as_bytes().iter() {
            if (*c < 0x20 && *c != b'\t' && *c != b'\n') || (*c > 0x7e) {
                return false;
            }
        }
        true
    }

    pub fn is_clarity_variable(&self) -> bool {
        ClarityName::try_from(self.to_string()).is_ok()
    }

    pub fn from_string(s: &String) -> Option<StacksString> {
        if !StacksString::is_valid_string(s) {
            return None;
        }
        Some(StacksString(s.as_bytes().to_vec()))
    }
}

#[test]
fn test_display() {
    let stxstr = StacksString::from_string(&"hello".to_string()).unwrap();
    println!("log: {stxstr:#?}");
    println!("log: {:#?}", stxstr.to_string());
}

impl StacksMessageCodec for StacksString {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.0)
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<StacksString, CodecError> {
        let bytes: Vec<u8> = {
            let mut bound_read = BoundReader::from_reader(fd, MAX_MESSAGE_LEN as u64);
            read_next(&mut bound_read)
        }?;

        // must encode a valid string
        let s = String::from_utf8(bytes.clone()).map_err(|_e| {
            CodecError::DeserializeError(
                "Invalid Stacks string: could not build from utf8".to_string(),
            )
        })?;

        if !StacksString::is_valid_string(&s) {
            // non-printable ASCII or not ASCII
            return Err(CodecError::DeserializeError(
                "Invalid Stacks string: non-printable or non-ASCII string".to_string(),
            ));
        }

        Ok(StacksString(bytes))
    }
}

/// A transaction that instantiates a smart contract
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TransactionSmartContract {
    pub name: ContractName,
    pub code_body: StacksString,
}

/// Cause of change in mining tenure
/// Depending on cause, tenure can be ended or extended
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum TenureChangeCause {
    /// A valid winning block-commit
    BlockFound = 0,
    /// The next burnchain block is taking too long, so extend the runtime budget
    Extended = 1,
}

impl TryFrom<u8> for TenureChangeCause {
    type Error = ();

    fn try_from(num: u8) -> Result<Self, Self::Error> {
        match num {
            0 => Ok(Self::BlockFound),
            1 => Ok(Self::Extended),
            _ => Err(()),
        }
    }
}

impl StacksMessageCodec for TenureChangeCause {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        let byte = (*self) as u8;
        write_next(fd, &byte)
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<TenureChangeCause, CodecError> {
        let byte: u8 = read_next(fd)?;
        TenureChangeCause::try_from(byte).map_err(|_| {
            CodecError::DeserializeError(format!("Unrecognized TenureChangeCause byte {byte}"))
        })
    }
}

/// A transaction from Stackers to signal new mining tenure
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TenureChangePayload {
    /// Consensus hash of this tenure.  Corresponds to the sortition in which the miner of this
    /// block was chosen.  It may be the case that this miner's tenure gets _extended_ across
    /// subsequent sortitions; if this happens, then this `consensus_hash` value _remains the same_
    /// as the sortition in which the winning block-commit was mined.
    pub tenure_consensus_hash: ConsensusHash,
    /// Consensus hash of the previous tenure.  Corresponds to the sortition of the previous
    /// winning block-commit.
    pub prev_tenure_consensus_hash: ConsensusHash,
    /// Current consensus hash on the underlying burnchain.  Corresponds to the last-seen
    /// sortition.
    pub burn_view_consensus_hash: ConsensusHash,
    /// The StacksBlockId of the last block from the previous tenure
    pub previous_tenure_end: StacksBlockId,
    /// The number of blocks produced since the last sortition-linked tenure
    pub previous_tenure_blocks: u32,
    /// A flag to indicate the cause of this tenure change
    pub cause: TenureChangeCause,
    /// The ECDSA public key hash of the current tenure
    pub pubkey_hash: Hash160,
}

impl TenureChangePayload {
    pub fn extend(
        &self,
        burn_view_consensus_hash: ConsensusHash,
        last_tenure_block_id: StacksBlockId,
        num_blocks_so_far: u32,
    ) -> Self {
        TenureChangePayload {
            tenure_consensus_hash: self.tenure_consensus_hash.clone(),
            prev_tenure_consensus_hash: self.tenure_consensus_hash.clone(),
            burn_view_consensus_hash,
            previous_tenure_end: last_tenure_block_id,
            previous_tenure_blocks: num_blocks_so_far,
            cause: TenureChangeCause::Extended,
            pubkey_hash: self.pubkey_hash.clone(),
        }
    }
}

impl StacksMessageCodec for TenureChangePayload {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.tenure_consensus_hash)?;
        write_next(fd, &self.prev_tenure_consensus_hash)?;
        write_next(fd, &self.burn_view_consensus_hash)?;
        write_next(fd, &self.previous_tenure_end)?;
        write_next(fd, &self.previous_tenure_blocks)?;
        write_next(fd, &self.cause)?;
        write_next(fd, &self.pubkey_hash)
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        Ok(Self {
            tenure_consensus_hash: read_next(fd)?,
            prev_tenure_consensus_hash: read_next(fd)?,
            burn_view_consensus_hash: read_next(fd)?,
            previous_tenure_end: read_next(fd)?,
            previous_tenure_blocks: read_next(fd)?,
            cause: read_next(fd)?,
            pubkey_hash: read_next(fd)?,
        })
    }
}

/// A coinbase commits to 32 bytes of control-plane information
pub struct CoinbasePayload(pub [u8; 32]);
impl_byte_array_message_codec!(CoinbasePayload, 32);
impl_array_newtype!(CoinbasePayload, u8, 32);
impl_array_hexstring_fmt!(CoinbasePayload);
impl_byte_array_newtype!(CoinbasePayload, u8, 32);
impl_byte_array_serde!(CoinbasePayload);

pub struct TokenTransferMemo(pub [u8; 34]); // same length as it is in stacks v1
impl_byte_array_message_codec!(TokenTransferMemo, 34);
impl_array_newtype!(TokenTransferMemo, u8, 34);
impl_array_hexstring_fmt!(TokenTransferMemo);
impl_byte_array_newtype!(TokenTransferMemo, u8, 34);
impl_byte_array_serde!(TokenTransferMemo);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TransactionPayload {
    TokenTransfer(PrincipalData, u64, TokenTransferMemo),
    ContractCall(TransactionContractCall),
    SmartContract(TransactionSmartContract, Option<ClarityVersion>),
    // the previous epoch leader sent two microblocks with the same sequence, and this is proof
    PoisonMicroblock(StacksMicroblockHeader, StacksMicroblockHeader),
    Coinbase(CoinbasePayload, Option<PrincipalData>, Option<VRFProof>),
    TenureChange(TenureChangePayload),
}

impl TransactionPayload {
    pub fn name(&self) -> &'static str {
        match self {
            TransactionPayload::TokenTransfer(..) => "TokenTransfer",
            TransactionPayload::ContractCall(..) => "ContractCall",
            TransactionPayload::SmartContract(_, version_opt) => {
                if version_opt.is_some() {
                    "SmartContract(Versioned)"
                } else {
                    "SmartContract"
                }
            }
            TransactionPayload::PoisonMicroblock(..) => "PoisonMicroblock",
            TransactionPayload::Coinbase(_, _, vrf_opt) => {
                if vrf_opt.is_some() {
                    "Coinbase(Nakamoto)"
                } else {
                    "Coinbase"
                }
            }
            TransactionPayload::TenureChange(payload) => match payload.cause {
                TenureChangeCause::BlockFound => "TenureChange(BlockFound)",
                TenureChangeCause::Extended => "TenureChange(Extension)",
            },
        }
    }
}

define_u8_enum!(TransactionPayloadID {
    TokenTransfer = 0,
    SmartContract = 1,
    ContractCall = 2,
    PoisonMicroblock = 3,
    Coinbase = 4,
    // has an alt principal, but no VRF proof
    CoinbaseToAltRecipient = 5,
    VersionedSmartContract = 6,
    TenureChange = 7,
    // has a VRF proof, and may have an alt principal
    NakamotoCoinbase = 8
});

/// Encoding of an asset type identifier
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AssetInfo {
    pub contract_address: StacksAddress,
    pub contract_name: ContractName,
    pub asset_name: ClarityName,
}

/// numeric wire-format ID of an asset info type variant
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum AssetInfoID {
    STX = 0,
    FungibleAsset = 1,
    NonfungibleAsset = 2,
}

impl AssetInfoID {
    pub fn from_u8(b: u8) -> Option<AssetInfoID> {
        match b {
            0 => Some(AssetInfoID::STX),
            1 => Some(AssetInfoID::FungibleAsset),
            2 => Some(AssetInfoID::NonfungibleAsset),
            _ => None,
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum FungibleConditionCode {
    SentEq = 0x01,
    SentGt = 0x02,
    SentGe = 0x03,
    SentLt = 0x04,
    SentLe = 0x05,
}

impl FungibleConditionCode {
    pub fn from_u8(b: u8) -> Option<FungibleConditionCode> {
        match b {
            0x01 => Some(FungibleConditionCode::SentEq),
            0x02 => Some(FungibleConditionCode::SentGt),
            0x03 => Some(FungibleConditionCode::SentGe),
            0x04 => Some(FungibleConditionCode::SentLt),
            0x05 => Some(FungibleConditionCode::SentLe),
            _ => None,
        }
    }

    pub fn check(&self, amount_sent_condition: u128, amount_sent: u128) -> bool {
        match *self {
            FungibleConditionCode::SentEq => amount_sent == amount_sent_condition,
            FungibleConditionCode::SentGt => amount_sent > amount_sent_condition,
            FungibleConditionCode::SentGe => amount_sent >= amount_sent_condition,
            FungibleConditionCode::SentLt => amount_sent < amount_sent_condition,
            FungibleConditionCode::SentLe => amount_sent <= amount_sent_condition,
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum NonfungibleConditionCode {
    Sent = 0x10,
    NotSent = 0x11,
}

impl NonfungibleConditionCode {
    pub fn from_u8(b: u8) -> Option<NonfungibleConditionCode> {
        match b {
            0x10 => Some(NonfungibleConditionCode::Sent),
            0x11 => Some(NonfungibleConditionCode::NotSent),
            _ => None,
        }
    }

    pub fn was_sent(nft_sent_condition: &Value, nfts_sent: &[Value]) -> bool {
        for asset_sent in nfts_sent.iter() {
            if *asset_sent == *nft_sent_condition {
                // asset was sent, and is no longer owned by this principal
                return true;
            }
        }
        false
    }

    pub fn check(&self, nft_sent_condition: &Value, nfts_sent: &[Value]) -> bool {
        match *self {
            NonfungibleConditionCode::Sent => {
                NonfungibleConditionCode::was_sent(nft_sent_condition, nfts_sent)
            }
            NonfungibleConditionCode::NotSent => {
                !NonfungibleConditionCode::was_sent(nft_sent_condition, nfts_sent)
            }
        }
    }
}

/// Post-condition principal.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum PostConditionPrincipal {
    Origin,
    Standard(StacksAddress),
    Contract(StacksAddress, ContractName),
}

impl PostConditionPrincipal {
    pub fn to_principal_data(&self, origin_principal: &PrincipalData) -> PrincipalData {
        match *self {
            PostConditionPrincipal::Origin => origin_principal.clone(),
            PostConditionPrincipal::Standard(ref addr) => {
                PrincipalData::Standard(StandardPrincipalData::from(addr.clone()))
            }
            PostConditionPrincipal::Contract(ref addr, ref contract_name) => {
                PrincipalData::Contract(QualifiedContractIdentifier::new(
                    StandardPrincipalData::from(addr.clone()),
                    contract_name.clone(),
                ))
            }
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum PostConditionPrincipalID {
    Origin = 0x01,
    Standard = 0x02,
    Contract = 0x03,
}

/// Post-condition on a transaction
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TransactionPostCondition {
    STX(PostConditionPrincipal, FungibleConditionCode, u64),
    Fungible(
        PostConditionPrincipal,
        AssetInfo,
        FungibleConditionCode,
        u64,
    ),
    Nonfungible(
        PostConditionPrincipal,
        AssetInfo,
        Value,
        NonfungibleConditionCode,
    ),
}

/// Post-condition modes for unspecified assets
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum TransactionPostConditionMode {
    Allow = 0x01, // allow any other changes not specified
    Deny = 0x02,  // deny any other changes not specified
}

/// Stacks transaction versions
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Copy, Serialize, Deserialize)]
pub enum TransactionVersion {
    Mainnet = 0x00,
    Testnet = 0x80,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StacksTransaction {
    pub version: TransactionVersion,
    pub chain_id: u32,
    pub auth: TransactionAuth,
    pub anchor_mode: TransactionAnchorMode,
    pub post_condition_mode: TransactionPostConditionMode,
    pub post_conditions: Vec<TransactionPostCondition>,
    pub payload: TransactionPayload,
}

impl StacksTransaction {
    /// Create a new, unsigned transaction and an empty STX fee with no post-conditions.
    pub fn new(
        version: TransactionVersion,
        auth: TransactionAuth,
        payload: TransactionPayload,
    ) -> StacksTransaction {
        let anchor_mode = match payload {
            TransactionPayload::Coinbase(..) => TransactionAnchorMode::OnChainOnly,
            TransactionPayload::PoisonMicroblock(_, _) => TransactionAnchorMode::OnChainOnly,
            _ => TransactionAnchorMode::Any,
        };

        StacksTransaction {
            version,
            chain_id: 0,
            auth,
            anchor_mode,
            post_condition_mode: TransactionPostConditionMode::Deny,
            post_conditions: vec![],
            payload,
        }
    }

    /// Get fee rate
    pub fn get_tx_fee(&self) -> u64 {
        self.auth.get_tx_fee()
    }

    /// Set fee rate
    pub fn set_tx_fee(&mut self, tx_fee: u64) {
        self.auth.set_tx_fee(tx_fee);
    }

    /// Get origin nonce
    pub fn get_origin_nonce(&self) -> u64 {
        self.auth.get_origin_nonce()
    }

    /// get sponsor nonce
    pub fn get_sponsor_nonce(&self) -> Option<u64> {
        self.auth.get_sponsor_nonce()
    }

    /// set origin nonce
    pub fn set_origin_nonce(&mut self, n: u64) {
        self.auth.set_origin_nonce(n);
    }

    /// set sponsor nonce
    pub fn set_sponsor_nonce(&mut self, n: u64) -> Result<(), CodecError> {
        self.auth.set_sponsor_nonce(n)
    }

    /// Set anchor mode
    pub fn set_anchor_mode(&mut self, anchor_mode: TransactionAnchorMode) {
        self.anchor_mode = anchor_mode;
    }

    /// Set post-condition mode
    pub fn set_post_condition_mode(&mut self, postcond_mode: TransactionPostConditionMode) {
        self.post_condition_mode = postcond_mode;
    }

    /// Add a post-condition
    pub fn add_post_condition(&mut self, post_condition: TransactionPostCondition) {
        self.post_conditions.push(post_condition);
    }

    /// a txid of a stacks transaction is its sha512/256 hash
    pub fn txid(&self) -> Txid {
        let mut bytes = vec![];
        self.consensus_serialize(&mut bytes)
            .expect("BUG: failed to serialize to a vec");
        Txid::from_stacks_tx(&bytes)
    }

    /// Get a mutable reference to the internal auth structure
    pub fn borrow_auth(&mut self) -> &mut TransactionAuth {
        &mut self.auth
    }

    /// Get an immutable reference to the internal auth structure
    pub fn auth(&self) -> &TransactionAuth {
        &self.auth
    }

    /// begin signing the transaction.
    /// If this is a sponsored transaction, then the origin only commits to knowing that it is
    /// sponsored.  It does _not_ commit to the sponsored fields, so set them all to sentinel
    /// values.
    /// Return the initial sighash.
    fn sign_begin(&self) -> Txid {
        let mut tx = self.clone();
        tx.auth = tx.auth.into_initial_sighash_auth();
        tx.txid()
    }

    /// begin verifying a transaction.
    /// return the initial sighash
    fn verify_begin(&self) -> Txid {
        let mut tx = self.clone();
        tx.auth = tx.auth.into_initial_sighash_auth();
        tx.txid()
    }

    /// Sign a sighash and append the signature and public key to the given spending condition.
    /// Returns the next sighash
    fn sign_and_append(
        condition: &mut TransactionSpendingCondition,
        cur_sighash: &Txid,
        auth_flag: &TransactionAuthFlags,
        privk: &Secp256k1PrivateKey,
    ) -> Result<Txid, CodecError> {
        let (next_sig, next_sighash) = TransactionSpendingCondition::next_signature(
            cur_sighash,
            auth_flag,
            condition.tx_fee(),
            condition.nonce(),
            privk,
        )?;
        match condition {
            TransactionSpendingCondition::Singlesig(ref mut cond) => {
                cond.set_signature(next_sig);
                Ok(next_sighash)
            }
            TransactionSpendingCondition::Multisig(ref mut cond) => {
                cond.push_signature(
                    if privk.compress_public() {
                        TransactionPublicKeyEncoding::Compressed
                    } else {
                        TransactionPublicKeyEncoding::Uncompressed
                    },
                    next_sig,
                );
                Ok(next_sighash)
            }
            TransactionSpendingCondition::OrderIndependentMultisig(ref mut cond) => {
                cond.push_signature(
                    if privk.compress_public() {
                        TransactionPublicKeyEncoding::Compressed
                    } else {
                        TransactionPublicKeyEncoding::Uncompressed
                    },
                    next_sig,
                );
                Ok(*cur_sighash)
            }
        }
    }

    /// Pop the last auth field
    fn pop_auth_field(
        condition: &mut TransactionSpendingCondition,
    ) -> Option<TransactionAuthField> {
        match condition {
            TransactionSpendingCondition::Multisig(ref mut cond) => cond.pop_auth_field(),
            TransactionSpendingCondition::OrderIndependentMultisig(ref mut cond) => {
                cond.pop_auth_field()
            }
            TransactionSpendingCondition::Singlesig(ref mut cond) => cond.pop_signature(),
        }
    }

    /// Append a public key to a multisig condition
    fn append_pubkey(
        condition: &mut TransactionSpendingCondition,
        pubkey: &StacksPublicKey,
    ) -> Result<(), CodecError> {
        match condition {
            TransactionSpendingCondition::Multisig(ref mut cond) => {
                cond.push_public_key(pubkey.clone());
                Ok(())
            }
            TransactionSpendingCondition::OrderIndependentMultisig(ref mut cond) => {
                cond.push_public_key(pubkey.clone());
                Ok(())
            }
            _ => Err(CodecError::SigningError(
                "Not a multisig condition".to_string(),
            )),
        }
    }

    /// Append the next signature from the origin account authorization.
    /// Return the next sighash.
    pub fn sign_next_origin(
        &mut self,
        cur_sighash: &Txid,
        privk: &Secp256k1PrivateKey,
    ) -> Result<Txid, CodecError> {
        let next_sighash = match self.auth {
            TransactionAuth::Standard(ref mut origin_condition)
            | TransactionAuth::Sponsored(ref mut origin_condition, _) => {
                StacksTransaction::sign_and_append(
                    origin_condition,
                    cur_sighash,
                    &TransactionAuthFlags::AuthStandard,
                    privk,
                )?
            }
        };
        Ok(next_sighash)
    }

    /// Append the next public key to the origin account authorization.
    pub fn append_next_origin(&mut self, pubk: &StacksPublicKey) -> Result<(), CodecError> {
        match self.auth {
            TransactionAuth::Standard(ref mut origin_condition) => {
                StacksTransaction::append_pubkey(origin_condition, pubk)
            }
            TransactionAuth::Sponsored(ref mut origin_condition, _) => {
                StacksTransaction::append_pubkey(origin_condition, pubk)
            }
        }
    }

    /// Append the next signature from the sponsoring account.
    /// Return the next sighash
    pub fn sign_next_sponsor(
        &mut self,
        cur_sighash: &Txid,
        privk: &Secp256k1PrivateKey,
    ) -> Result<Txid, CodecError> {
        let next_sighash = match self.auth {
            TransactionAuth::Standard(_) => {
                // invalid
                return Err(CodecError::SigningError(
                    "Cannot sign standard authorization with a sponsoring private key".to_string(),
                ));
            }
            TransactionAuth::Sponsored(_, ref mut sponsor_condition) => {
                StacksTransaction::sign_and_append(
                    sponsor_condition,
                    cur_sighash,
                    &TransactionAuthFlags::AuthSponsored,
                    privk,
                )?
            }
        };
        Ok(next_sighash)
    }

    /// Append the next public key to the sponsor account authorization.
    pub fn append_next_sponsor(&mut self, pubk: &StacksPublicKey) -> Result<(), CodecError> {
        match self.auth {
            TransactionAuth::Standard(_) => Err(CodecError::SigningError(
                "Cannot append a public key to the sponsor of a standard auth condition"
                    .to_string(),
            )),
            TransactionAuth::Sponsored(_, ref mut sponsor_condition) => {
                StacksTransaction::append_pubkey(sponsor_condition, pubk)
            }
        }
    }

    /// Verify this transaction's signatures
    pub fn verify(&self) -> Result<(), CodecError> {
        self.auth.verify(&self.verify_begin())
    }

    /// Verify the transaction's origin signatures only.
    /// Used by sponsors to get the next sig-hash to sign.
    pub fn verify_origin(&self) -> Result<Txid, CodecError> {
        self.auth.verify_origin(&self.verify_begin())
    }

    /// Get the origin account's address
    pub fn origin_address(&self) -> StacksAddress {
        match (&self.version, &self.auth) {
            (&TransactionVersion::Mainnet, TransactionAuth::Standard(origin_condition)) => {
                origin_condition.address_mainnet()
            }
            (&TransactionVersion::Testnet, TransactionAuth::Standard(origin_condition)) => {
                origin_condition.address_testnet()
            }
            (
                &TransactionVersion::Mainnet,
                TransactionAuth::Sponsored(origin_condition, _unused),
            ) => origin_condition.address_mainnet(),
            (
                &TransactionVersion::Testnet,
                TransactionAuth::Sponsored(origin_condition, _unused),
            ) => origin_condition.address_testnet(),
        }
    }

    /// Get the sponsor account's address, if this transaction is sponsored
    pub fn sponsor_address(&self) -> Option<StacksAddress> {
        match (&self.version, &self.auth) {
            (&TransactionVersion::Mainnet, TransactionAuth::Standard(_unused)) => None,
            (&TransactionVersion::Testnet, TransactionAuth::Standard(_unused)) => None,
            (
                &TransactionVersion::Mainnet,
                TransactionAuth::Sponsored(_unused, sponsor_condition),
            ) => Some(sponsor_condition.address_mainnet()),
            (
                &TransactionVersion::Testnet,
                TransactionAuth::Sponsored(_unused, sponsor_condition),
            ) => Some(sponsor_condition.address_testnet()),
        }
    }

    /// Get a copy of the origin spending condition
    pub fn get_origin(&self) -> TransactionSpendingCondition {
        self.auth.origin().clone()
    }

    /// Get a copy of the sending condition that will pay the tx fee
    pub fn get_payer(&self) -> TransactionSpendingCondition {
        match self.auth.sponsor() {
            Some(tsc) => tsc.clone(),
            None => self.auth.origin().clone(),
        }
    }

    /// Is this a mainnet transaction?  false means 'testnet'
    pub fn is_mainnet(&self) -> bool {
        matches!(self.version, TransactionVersion::Mainnet)
    }

    pub fn tx_len(&self) -> u64 {
        let mut tx_bytes = vec![];
        self.consensus_serialize(&mut tx_bytes)
            .expect("BUG: Failed to serialize a transaction object");
        u64::try_from(tx_bytes.len()).expect("tx len exceeds 2^64 bytes")
    }

    pub fn consensus_deserialize_with_len<R: Read>(
        fd: &mut R,
    ) -> Result<(StacksTransaction, u64), CodecError> {
        let mut bound_read = BoundReader::from_reader(fd, MAX_TRANSACTION_LEN.into());
        let fd = &mut bound_read;

        let version_u8: u8 = read_next(fd)?;
        let chain_id: u32 = read_next(fd)?;
        let auth: TransactionAuth = read_next(fd)?;
        let anchor_mode_u8: u8 = read_next(fd)?;
        let post_condition_mode_u8: u8 = read_next(fd)?;
        let post_conditions: Vec<TransactionPostCondition> = read_next(fd)?;

        let payload: TransactionPayload = read_next(fd)?;

        let version = if (version_u8 & 0x80) == 0 {
            TransactionVersion::Mainnet
        } else {
            TransactionVersion::Testnet
        };

        let anchor_mode = match anchor_mode_u8 {
            x if x == TransactionAnchorMode::OffChainOnly as u8 => {
                TransactionAnchorMode::OffChainOnly
            }
            x if x == TransactionAnchorMode::OnChainOnly as u8 => {
                TransactionAnchorMode::OnChainOnly
            }
            x if x == TransactionAnchorMode::Any as u8 => TransactionAnchorMode::Any,
            _ => {
                return Err(CodecError::DeserializeError(format!(
                    "Failed to parse transaction: invalid anchor mode {anchor_mode_u8}"
                )));
            }
        };

        // if the payload is a proof of a poisoned microblock stream, or is a coinbase, then this _must_ be anchored.
        // Otherwise, if the offending leader is the next leader, they can just orphan their proof
        // of malfeasance.
        match payload {
            TransactionPayload::PoisonMicroblock(_, _) => {
                if anchor_mode != TransactionAnchorMode::OnChainOnly {
                    return Err(CodecError::DeserializeError(
                        "Failed to parse transaction: invalid anchor mode for PoisonMicroblock"
                            .to_string(),
                    ));
                }
            }
            TransactionPayload::Coinbase(..) => {
                if anchor_mode != TransactionAnchorMode::OnChainOnly {
                    return Err(CodecError::DeserializeError(
                        "Failed to parse transaction: invalid anchor mode for Coinbase".to_string(),
                    ));
                }
            }
            _ => {}
        }

        let post_condition_mode = match post_condition_mode_u8 {
            x if x == TransactionPostConditionMode::Allow as u8 => {
                TransactionPostConditionMode::Allow
            }
            x if x == TransactionPostConditionMode::Deny as u8 => {
                TransactionPostConditionMode::Deny
            }
            _ => {
                return Err(CodecError::DeserializeError(format!(
                    "Failed to parse transaction: invalid post-condition mode {post_condition_mode_u8}"
                )));
            }
        };
        let tx = StacksTransaction {
            version,
            chain_id,
            auth,
            anchor_mode,
            post_condition_mode,
            post_conditions,
            payload,
        };

        Ok((tx, fd.num_read()))
    }

    /// Try to convert to a coinbase payload
    pub fn try_as_coinbase(
        &self,
    ) -> Option<(&CoinbasePayload, Option<&PrincipalData>, Option<&VRFProof>)> {
        match &self.payload {
            TransactionPayload::Coinbase(payload, recipient_opt, vrf_proof_opt) => {
                Some((payload, recipient_opt.as_ref(), vrf_proof_opt.as_ref()))
            }
            _ => None,
        }
    }

    /// Try to convert to a tenure change payload
    pub fn try_as_tenure_change(&self) -> Option<&TenureChangePayload> {
        match &self.payload {
            TransactionPayload::TenureChange(tc_payload) => Some(tc_payload),
            _ => None,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Txid(pub [u8; 32]);
impl_array_newtype!(Txid, u8, 32);
impl_array_hexstring_fmt!(Txid);
impl_byte_array_newtype!(Txid, u8, 32);

impl Txid {
    /// A Stacks transaction ID is a sha512/256 hash (not a double-sha256 hash)
    pub fn from_stacks_tx(txdata: &[u8]) -> Txid {
        let h = Sha512Trunc256Sum::from_data(txdata);
        let mut bytes = [0u8; 32];
        bytes.copy_from_slice(h.as_bytes());
        Txid(bytes)
    }

    /// A sighash is calculated the same way as a txid
    pub fn from_sighash_bytes(txdata: &[u8]) -> Txid {
        Txid::from_stacks_tx(txdata)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StacksTransactionSigner {
    pub tx: StacksTransaction,
    pub sighash: Txid,
    origin_done: bool,
    check_oversign: bool,
    check_overlap: bool,
}

impl StacksTransactionSigner {
    pub fn new(tx: &StacksTransaction) -> StacksTransactionSigner {
        StacksTransactionSigner {
            tx: tx.clone(),
            sighash: tx.sign_begin(),
            origin_done: false,
            check_oversign: true,
            check_overlap: true,
        }
    }

    pub fn new_sponsor(
        tx: &StacksTransaction,
        spending_condition: TransactionSpendingCondition,
    ) -> Result<StacksTransactionSigner, CodecError> {
        if !tx.auth.is_sponsored() {
            return Err(CodecError::GenericError(
                "IncompatibleSpendingConditionError".into(),
            ));
        }
        let mut new_tx = tx.clone();
        new_tx.auth.set_sponsor(spending_condition)?;
        let origin_sighash = new_tx.verify_origin()?;

        Ok(StacksTransactionSigner {
            tx: new_tx,
            sighash: origin_sighash,
            origin_done: true,
            check_oversign: true,
            check_overlap: true,
        })
    }

    pub fn resume(&mut self, tx: &StacksTransaction) {
        self.tx = tx.clone()
    }

    pub fn disable_checks(&mut self) {
        self.check_oversign = false;
        self.check_overlap = false;
    }

    pub fn sign_origin(&mut self, privk: &Secp256k1PrivateKey) -> Result<(), CodecError> {
        if self.check_overlap && self.origin_done {
            // can't sign another origin private key since we started signing sponsors
            return Err(CodecError::SigningError(
                "Cannot sign origin after sponsor key".to_string(),
            ));
        }

        match self.tx.auth {
            TransactionAuth::Standard(ref origin_condition) => {
                if self.check_oversign
                    && origin_condition.num_signatures() >= origin_condition.signatures_required()
                {
                    return Err(CodecError::SigningError(
                        "Origin would have too many signatures".to_string(),
                    ));
                }
            }
            TransactionAuth::Sponsored(ref origin_condition, _) => {
                if self.check_oversign
                    && origin_condition.num_signatures() >= origin_condition.signatures_required()
                {
                    return Err(CodecError::SigningError(
                        "Origin would have too many signatures".to_string(),
                    ));
                }
            }
        }

        let next_sighash = self.tx.sign_next_origin(&self.sighash, privk)?;
        self.sighash = next_sighash;
        Ok(())
    }

    pub fn append_origin(&mut self, pubk: &Secp256k1PublicKey) -> Result<(), CodecError> {
        if self.check_overlap && self.origin_done {
            // can't append another origin key
            return Err(CodecError::SigningError(
                "Cannot append public key to origin after sponsor key".to_string(),
            ));
        }

        self.tx.append_next_origin(pubk)
    }

    pub fn sign_sponsor(&mut self, privk: &Secp256k1PrivateKey) -> Result<(), CodecError> {
        match self.tx.auth {
            TransactionAuth::Sponsored(_, ref sponsor_condition) => {
                if self.check_oversign
                    && sponsor_condition.num_signatures() >= sponsor_condition.signatures_required()
                {
                    return Err(CodecError::SigningError(
                        "Sponsor would have too many signatures".to_string(),
                    ));
                }
            }
            TransactionAuth::Standard(_) => todo!(),
        }

        let next_sighash = self.tx.sign_next_sponsor(&self.sighash, privk)?;
        self.sighash = next_sighash;
        self.origin_done = true;
        Ok(())
    }

    pub fn append_sponsor(&mut self, pubk: &Secp256k1PublicKey) -> Result<(), CodecError> {
        self.tx.append_next_sponsor(pubk)
    }

    pub fn pop_origin_auth_field(&mut self) -> Option<TransactionAuthField> {
        match self.tx.auth {
            TransactionAuth::Standard(ref mut origin_condition) => {
                StacksTransaction::pop_auth_field(origin_condition)
            }
            TransactionAuth::Sponsored(ref mut origin_condition, _) => {
                StacksTransaction::pop_auth_field(origin_condition)
            }
        }
    }

    pub fn pop_sponsor_auth_field(&mut self) -> Option<TransactionAuthField> {
        match self.tx.auth {
            TransactionAuth::Sponsored(_, ref mut sponsor_condition) => {
                StacksTransaction::pop_auth_field(sponsor_condition)
            }
            _ => None,
        }
    }

    pub fn complete(&self) -> bool {
        match self.tx.auth {
            TransactionAuth::Standard(ref origin_condition) => {
                origin_condition.num_signatures() >= origin_condition.signatures_required()
            }
            TransactionAuth::Sponsored(ref origin_condition, ref sponsored_condition) => {
                origin_condition.num_signatures() >= origin_condition.signatures_required()
                    && sponsored_condition.num_signatures()
                        >= sponsored_condition.signatures_required()
                    && (self.origin_done || !self.check_overlap)
            }
        }
    }

    pub fn get_tx_incomplete(&self) -> StacksTransaction {
        self.tx.clone()
    }

    pub fn get_tx(&self) -> Option<StacksTransaction> {
        if self.complete() {
            Some(self.tx.clone())
        } else {
            None
        }
    }
}

/// A block that contains blockchain-anchored data
/// (corresponding to a LeaderBlockCommitOp)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StacksBlock {
    pub header: StacksBlockHeader,
    pub txs: Vec<StacksTransaction>,
}

/// A microblock that contains non-blockchain-anchored data,
/// but is tied to an on-chain block
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StacksMicroblock {
    pub header: StacksMicroblockHeader,
    pub txs: Vec<StacksTransaction>,
}

/// The header for an on-chain-anchored Stacks block
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StacksBlockHeader {
    pub version: u8,
    pub total_work: StacksWorkScore, // NOTE: this is the work done on the chain tip this block builds on (i.e. take this from the parent)
    pub proof: VRFProof,
    pub parent_block: BlockHeaderHash, // NOTE: even though this is also present in the burn chain, we need this here for super-light clients that don't even have burn chain headers
    pub parent_microblock: BlockHeaderHash,
    pub parent_microblock_sequence: u16,
    pub tx_merkle_root: Sha512Trunc256Sum,
    pub state_index_root: TrieHash,
    pub microblock_pubkey_hash: Hash160, // we'll get the public key back from the first signature (note that this is the Hash160 of the _compressed_ public key)
}

/// Header structure for a microblock
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StacksMicroblockHeader {
    pub version: u8,
    pub sequence: u16,
    pub prev_block: BlockHeaderHash,
    pub tx_merkle_root: Sha512Trunc256Sum,
    pub signature: MessageSignature,
}

impl StacksMessageCodec for StacksMicroblockHeader {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        self.serialize(fd, false)
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<StacksMicroblockHeader, CodecError> {
        let version: u8 = read_next(fd)?;
        let sequence: u16 = read_next(fd)?;
        let prev_block: BlockHeaderHash = read_next(fd)?;
        let tx_merkle_root: Sha512Trunc256Sum = read_next(fd)?;
        let signature: MessageSignature = read_next(fd)?;

        // signature must be well-formed
        // let _ = signature
        //     .to_secp256k1_recoverable()
        //     .ok_or(CodecError::DeserializeError(
        //         "Failed to parse signature".to_string(),
        //     ))?;

        Ok(StacksMicroblockHeader {
            version,
            sequence,
            prev_block,
            tx_merkle_root,
            signature,
        })
    }
}

impl StacksMicroblockHeader {
    fn serialize<W: Write>(&self, fd: &mut W, empty_sig: bool) -> Result<(), CodecError> {
        write_next(fd, &self.version)?;
        write_next(fd, &self.sequence)?;
        write_next(fd, &self.prev_block)?;
        write_next(fd, &self.tx_merkle_root)?;
        if empty_sig {
            write_next(fd, &MessageSignature::empty())?;
        } else {
            write_next(fd, &self.signature)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NakamotoBlockHeader {
    pub version: u8,
    /// The total number of StacksBlock and NakamotoBlocks preceding
    /// this block in this block's history.
    pub chain_length: u64,
    /// Total amount of BTC spent producing the sortition that
    /// selected this block's miner.
    pub burn_spent: u64,
    /// The consensus hash of the burnchain block that selected this tenure.  The consensus hash
    /// uniquely identifies this tenure, including across all Bitcoin forks.
    pub consensus_hash: ConsensusHash,
    /// The index block hash of the immediate parent of this block.
    /// This is the hash of the parent block's hash and consensus hash.
    pub parent_block_id: StacksBlockId,
    /// The root of a SHA512/256 merkle tree over all this block's
    /// contained transactions
    pub tx_merkle_root: Sha512Trunc256Sum,
    /// The MARF trie root hash after this block has been processed
    pub state_index_root: TrieHash,
    /// A Unix time timestamp of when this block was mined, according to the miner.
    /// For the signers to consider a block valid, this timestamp must be:
    ///  * Greater than the timestamp of its parent block
    ///  * At most 15 seconds into the future
    pub timestamp: u64,
    /// Recoverable ECDSA signature from the tenure's miner.
    pub miner_signature: MessageSignature,
    /// The set of recoverable ECDSA signatures over
    /// the block header from the signer set active during the tenure.
    /// (ordered by reward set order)
    pub signer_signature: Vec<MessageSignature>,
    /// A bitvec which conveys whether reward addresses should be punished (by burning their PoX rewards)
    ///  or not in this block.
    ///
    /// The maximum number of entries in the bitvec is 4000.
    pub pox_treatment: BitVec<4000>,
}

impl StacksMessageCodec for NakamotoBlockHeader {
    fn consensus_serialize<W: std::io::Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.version)?;
        write_next(fd, &self.chain_length)?;
        write_next(fd, &self.burn_spent)?;
        write_next(fd, &self.consensus_hash)?;
        write_next(fd, &self.parent_block_id)?;
        write_next(fd, &self.tx_merkle_root)?;
        write_next(fd, &self.state_index_root)?;
        write_next(fd, &self.timestamp)?;
        write_next(fd, &self.miner_signature)?;
        write_next(fd, &self.signer_signature)?;
        write_next(fd, &self.pox_treatment)?;

        Ok(())
    }

    fn consensus_deserialize<R: std::io::Read>(fd: &mut R) -> Result<Self, CodecError> {
        Ok(NakamotoBlockHeader {
            version: read_next(fd)?,
            chain_length: read_next(fd)?,
            burn_spent: read_next(fd)?,
            consensus_hash: read_next(fd)?,
            parent_block_id: read_next(fd)?,
            tx_merkle_root: read_next(fd)?,
            state_index_root: read_next(fd)?,
            timestamp: read_next(fd)?,
            miner_signature: read_next(fd)?,
            signer_signature: read_next(fd)?,
            pox_treatment: read_next(fd)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NakamotoBlock {
    pub header: NakamotoBlockHeader,
    pub txs: Vec<StacksTransaction>,
}

impl StacksMessageCodec for NakamotoBlock {
    fn consensus_serialize<W: std::io::Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.header)?;
        write_next(fd, &self.txs)
    }

    fn consensus_deserialize<R: std::io::Read>(fd: &mut R) -> Result<Self, CodecError> {
        let (header, txs) = {
            let mut bound_read = BoundReader::from_reader(fd, u64::from(MAX_MESSAGE_LEN));
            let header: NakamotoBlockHeader = read_next(&mut bound_read)?;
            let txs: Vec<_> = read_next(&mut bound_read)?;
            (header, txs)
        };

        // // all transactions are unique
        // if !StacksBlock::validate_transactions_unique(&txs) {
        //     warn!("Invalid block: Found duplicate transaction";
        //         "consensus_hash" => %header.consensus_hash,
        //         "stacks_block_hash" => %header.block_hash(),
        //         "stacks_block_id" => %header.block_id()
        //     );
        //     return Err(CodecError::DeserializeError(
        //         "Invalid block: found duplicate transaction".to_string(),
        //     ));
        // }

        // // header and transactions must be consistent
        // let txid_vecs = txs.iter().map(|tx| tx.txid().as_bytes().to_vec()).collect();

        // let merkle_tree = MerkleTree::new(&txid_vecs);
        // let tx_merkle_root: Sha512Trunc256Sum = merkle_tree.root();

        // if tx_merkle_root != header.tx_merkle_root {
        //     warn!("Invalid block: Tx Merkle root mismatch";
        //         "consensus_hash" => %header.consensus_hash,
        //         "stacks_block_hash" => %header.block_hash(),
        //         "stacks_block_id" => %header.block_id()
        //     );
        //     return Err(CodecError::DeserializeError(
        //         "Invalid block: tx Merkle root mismatch".to_string(),
        //     ));
        // }

        Ok(NakamotoBlock { header, txs })
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
/// A vote across the signer set for a block
pub struct NakamotoBlockVote {
    pub signer_signature_hash: Sha512Trunc256Sum,
    pub rejected: bool,
}

impl StacksMessageCodec for NakamotoBlockVote {
    fn consensus_serialize<W: std::io::Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.signer_signature_hash)?;
        if self.rejected {
            write_next(fd, &1u8)?;
        }
        Ok(())
    }

    fn consensus_deserialize<R: std::io::Read>(fd: &mut R) -> Result<Self, CodecError> {
        let signer_signature_hash = read_next(fd)?;
        let rejected_byte: Option<u8> = read_next(fd).ok();
        let rejected = rejected_byte.is_some();
        Ok(Self {
            signer_signature_hash,
            rejected,
        })
    }
}

// values a miner uses to produce the next block
pub const MINER_BLOCK_CONSENSUS_HASH: ConsensusHash = ConsensusHash([1u8; 20]);
pub const MINER_BLOCK_HEADER_HASH: BlockHeaderHash = BlockHeaderHash([1u8; 32]);

#[derive(Debug, Clone, PartialEq)]
pub enum StacksBlockHeaderTypes {
    Epoch2(StacksBlockHeader),
    Nakamoto(NakamotoBlockHeader),
}

impl From<StacksBlockHeader> for StacksBlockHeaderTypes {
    fn from(value: StacksBlockHeader) -> Self {
        Self::Epoch2(value)
    }
}

impl From<NakamotoBlockHeader> for StacksBlockHeaderTypes {
    fn from(value: NakamotoBlockHeader) -> Self {
        Self::Nakamoto(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StacksHeaderInfo {
    /// Stacks block header
    pub anchored_header: StacksBlockHeaderTypes,
    /// Last microblock header (Stacks 2.x only; this is None in Stacks 3.x)
    pub microblock_tail: Option<StacksMicroblockHeader>,
    /// Height of this Stacks block
    pub stacks_block_height: u64,
    /// MARF root hash of the headers DB (not consensus critical)
    pub index_root: TrieHash,
    /// consensus hash of the burnchain block in which this miner was selected to produce this block
    pub consensus_hash: ConsensusHash,
    /// Hash of the burnchain block in which this miner was selected to produce this block
    pub burn_header_hash: BurnchainHeaderHash,
    /// Height of the burnchain block
    pub burn_header_height: u32,
    /// Timestamp of the burnchain block
    pub burn_header_timestamp: u64,
    /// Size of the block corresponding to `anchored_header` in bytes
    pub anchored_block_size: u64,
    /// The burnchain tip that is passed to Clarity while processing this block.
    /// This should always be `Some()` for Nakamoto blocks and `None` for 2.x blocks
    pub burn_view: Option<ConsensusHash>,
}

/// A record of a coin reward for a miner.  There will be at most two of these for a miner: one for
/// the coinbase + block-txs + confirmed-mblock-txs, and one for the produced-mblock-txs.  The
/// latter reward only stores the produced-mblock-txs, and is only ever stored if the microblocks
/// are ever confirmed.
#[derive(Debug, Clone, PartialEq)]
pub struct MinerReward {
    /// address of the miner that produced the block
    pub address: StacksAddress,
    /// address of the entity that receives the block reward.
    /// Ignored pre-2.1
    pub recipient: PrincipalData,
    /// block coinbase
    pub coinbase: u128,
    /// block transaction fees
    pub tx_fees_anchored: u128,
    /// microblock transaction fees from transactions *mined* by this miner
    pub tx_fees_streamed_produced: u128,
    /// microblock transaction fees from transactions *confirmed* by this miner
    pub tx_fees_streamed_confirmed: u128,
    /// virtual transaction index in the block where these rewards get applied.  the miner's reward
    /// is applied first (so vtxindex == 0) and user-burn supports would be applied after (so
    /// vtxindex > 0).
    pub vtxindex: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MinerRewardInfo {
    pub from_block_consensus_hash: ConsensusHash,
    pub from_stacks_block_hash: BlockHeaderHash,
    pub from_parent_block_consensus_hash: ConsensusHash,
    pub from_parent_stacks_block_hash: BlockHeaderHash,
}

// maximum amount of data a leader can send during its epoch (2MB)
pub const MAX_EPOCH_SIZE: u32 = 2 * 1024 * 1024;

// maximum microblock size is 64KB, but note that the current leader has a space budget of
// $MAX_EPOCH_SIZE bytes (so the average microblock size needs to be 4kb if there are 256 of them)
pub const MAX_MICROBLOCK_SIZE: u32 = 65536;

pub fn build_contract_call_transaction(
    contract_id: String,
    function_name: String,
    args: Vec<Value>,
    nonce: u64,
    fee: u64,
    sender_secret_key: &[u8],
) -> StacksTransaction {
    let contract_id =
        QualifiedContractIdentifier::parse(&contract_id).expect("Contract identifier invalid");

    let payload = TransactionContractCall {
        address: contract_id.issuer.into(),
        contract_name: contract_id.name,
        function_name: function_name.try_into().unwrap(),
        function_args: args,
    };

    let secret_key = Secp256k1PrivateKey::from_slice(sender_secret_key).unwrap();
    let mut public_key = Secp256k1PublicKey::from_private(&secret_key);
    public_key.set_compressed(true);

    let anchor_mode = TransactionAnchorMode::Any;
    let signer_addr =
        StacksAddress::from_public_keys(0, &AddressHashMode::SerializeP2PKH, 1, &vec![public_key])
            .unwrap();

    let spending_condition = TransactionSpendingCondition::Singlesig(SinglesigSpendingCondition {
        signer: signer_addr.bytes().clone(),
        nonce,
        tx_fee: fee,
        hash_mode: SinglesigHashMode::P2PKH,
        key_encoding: TransactionPublicKeyEncoding::Compressed,
        signature: MessageSignature::empty(),
    });

    let auth = TransactionAuth::Standard(spending_condition);
    let unsigned_tx = StacksTransaction {
        version: TransactionVersion::Testnet,
        chain_id: 0x80000000, // MAINNET=0x00000001
        auth,
        anchor_mode,
        post_condition_mode: TransactionPostConditionMode::Allow,
        post_conditions: vec![],
        payload: TransactionPayload::ContractCall(payload),
    };

    let mut unsigned_tx_bytes = vec![];
    unsigned_tx
        .consensus_serialize(&mut unsigned_tx_bytes)
        .expect("FATAL: invalid transaction");

    let mut tx_signer = StacksTransactionSigner::new(&unsigned_tx);
    tx_signer.sign_origin(&secret_key).unwrap();

    tx_signer.get_tx().unwrap()
}

impl StacksMessageCodec for TransactionContractCall {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.address)?;
        write_next(fd, &self.contract_name)?;
        write_next(fd, &self.function_name)?;
        write_next(fd, &self.function_args)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<TransactionContractCall, CodecError> {
        let address: StacksAddress = read_next(fd)?;
        let contract_name: ContractName = read_next(fd)?;
        let function_name: ClarityName = read_next(fd)?;
        let function_args: Vec<Value> = {
            let mut bound_read = BoundReader::from_reader(fd, MAX_TRANSACTION_LEN as u64);
            read_next(&mut bound_read)
        }?;

        // function name must be valid Clarity variable
        if !StacksString::from(function_name.clone()).is_clarity_variable() {
            return Err(CodecError::DeserializeError(
                "Failed to parse transaction: invalid function name -- not a Clarity variable"
                    .to_string(),
            ));
        }

        Ok(TransactionContractCall {
            address,
            contract_name,
            function_name,
            function_args,
        })
    }
}

impl StacksMessageCodec for TransactionSmartContract {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.name)?;
        write_next(fd, &self.code_body)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<TransactionSmartContract, CodecError> {
        let name: ContractName = read_next(fd)?;
        let code_body: StacksString = read_next(fd)?;
        Ok(TransactionSmartContract { name, code_body })
    }
}

fn clarity_version_consensus_serialize<W: Write>(
    version: &ClarityVersion,
    fd: &mut W,
) -> Result<(), CodecError> {
    match *version {
        ClarityVersion::Clarity1 => write_next(fd, &1u8)?,
        ClarityVersion::Clarity2 => write_next(fd, &2u8)?,
        ClarityVersion::Clarity3 => write_next(fd, &3u8)?,
        ClarityVersion::Clarity4 => write_next(fd, &4u8)?,
    }
    Ok(())
}

fn clarity_version_consensus_deserialize<R: Read>(
    fd: &mut R,
) -> Result<ClarityVersion, CodecError> {
    let version_byte: u8 = read_next(fd)?;
    match version_byte {
        1u8 => Ok(ClarityVersion::Clarity1),
        2u8 => Ok(ClarityVersion::Clarity2),
        3u8 => Ok(ClarityVersion::Clarity3),
        4u8 => Ok(ClarityVersion::Clarity4),
        _ => Err(CodecError::DeserializeError(format!(
            "Unrecognized ClarityVersion byte {}",
            &version_byte
        ))),
    }
}

impl StacksMessageCodec for TransactionPayload {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        match self {
            TransactionPayload::TokenTransfer(address, amount, memo) => {
                write_next(fd, &(TransactionPayloadID::TokenTransfer as u8))?;
                write_next(fd, address)?;
                write_next(fd, amount)?;
                write_next(fd, memo)?;
            }
            TransactionPayload::ContractCall(cc) => {
                write_next(fd, &(TransactionPayloadID::ContractCall as u8))?;
                cc.consensus_serialize(fd)?;
            }
            TransactionPayload::SmartContract(sc, version_opt) => {
                if let Some(version) = version_opt {
                    // caller requests a specific Clarity version
                    write_next(fd, &(TransactionPayloadID::VersionedSmartContract as u8))?;
                    clarity_version_consensus_serialize(version, fd)?;
                    sc.consensus_serialize(fd)?;
                } else {
                    // caller requests to use whatever the current clarity version is
                    write_next(fd, &(TransactionPayloadID::SmartContract as u8))?;
                    sc.consensus_serialize(fd)?;
                }
            }
            TransactionPayload::PoisonMicroblock(h1, h2) => {
                write_next(fd, &(TransactionPayloadID::PoisonMicroblock as u8))?;
                h1.consensus_serialize(fd)?;
                h2.consensus_serialize(fd)?;
            }
            TransactionPayload::Coinbase(buf, recipient_opt, vrf_opt) => {
                match (recipient_opt, vrf_opt) {
                    (None, None) => {
                        // stacks 2.05 and earlier only use this path
                        write_next(fd, &(TransactionPayloadID::Coinbase as u8))?;
                        write_next(fd, buf)?;
                    }
                    (Some(recipient), None) => {
                        write_next(fd, &(TransactionPayloadID::CoinbaseToAltRecipient as u8))?;
                        write_next(fd, buf)?;
                        write_next(fd, &Value::Principal(recipient.clone()))?;
                    }
                    (None, Some(vrf_proof)) => {
                        // nakamoto coinbase
                        // encode principal as (optional principal)
                        write_next(fd, &(TransactionPayloadID::NakamotoCoinbase as u8))?;
                        write_next(fd, buf)?;
                        write_next(fd, &Value::none())?;
                        write_next(fd, vrf_proof)?;
                    }
                    (Some(recipient), Some(vrf_proof)) => {
                        write_next(fd, &(TransactionPayloadID::NakamotoCoinbase as u8))?;
                        write_next(fd, buf)?;
                        write_next(
                            fd,
                            &Value::some(Value::Principal(recipient.clone())).expect(
                                "FATAL: failed to encode recipient principal as `optional`",
                            ),
                        )?;
                        write_next(fd, vrf_proof)?;
                    }
                }
            }
            TransactionPayload::TenureChange(tc) => {
                write_next(fd, &(TransactionPayloadID::TenureChange as u8))?;
                tc.consensus_serialize(fd)?;
            }
        }
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<TransactionPayload, CodecError> {
        let type_id_u8 = read_next(fd)?;
        let type_id = TransactionPayloadID::from_u8(type_id_u8).ok_or_else(|| {
            CodecError::DeserializeError(format!(
                "Failed to parse transaction -- unknown payload ID {type_id_u8}"
            ))
        })?;
        let payload = match type_id {
            TransactionPayloadID::TokenTransfer => {
                let principal = read_next(fd)?;
                let amount = read_next(fd)?;
                let memo = read_next(fd)?;
                TransactionPayload::TokenTransfer(principal, amount, memo)
            }
            TransactionPayloadID::ContractCall => {
                let payload: TransactionContractCall = read_next(fd)?;
                TransactionPayload::ContractCall(payload)
            }
            TransactionPayloadID::SmartContract => {
                let payload: TransactionSmartContract = read_next(fd)?;
                TransactionPayload::SmartContract(payload, None)
            }
            TransactionPayloadID::VersionedSmartContract => {
                let version = clarity_version_consensus_deserialize(fd)?;
                let payload: TransactionSmartContract = read_next(fd)?;
                TransactionPayload::SmartContract(payload, Some(version))
            }
            TransactionPayloadID::PoisonMicroblock => {
                let h1: StacksMicroblockHeader = read_next(fd)?;
                let h2: StacksMicroblockHeader = read_next(fd)?;

                // must differ in some field
                if h1 == h2 {
                    return Err(CodecError::DeserializeError(
                        "Failed to parse transaction -- microblock headers match".to_string(),
                    ));
                }

                // must have the same sequence number or same block parent
                if h1.sequence != h2.sequence && h1.prev_block != h2.prev_block {
                    return Err(CodecError::DeserializeError(
                        "Failed to parse transaction -- microblock headers do not identify a fork"
                            .to_string(),
                    ));
                }

                TransactionPayload::PoisonMicroblock(h1, h2)
            }
            TransactionPayloadID::Coinbase => {
                let payload: CoinbasePayload = read_next(fd)?;
                TransactionPayload::Coinbase(payload, None, None)
            }
            TransactionPayloadID::CoinbaseToAltRecipient => {
                let payload: CoinbasePayload = read_next(fd)?;
                let principal_value: Value = read_next(fd)?;
                let Value::Principal(recipient) = principal_value else {
                    return Err(CodecError::DeserializeError("Failed to parse coinbase transaction -- did not receive a recipient principal value".to_string()));
                };

                TransactionPayload::Coinbase(payload, Some(recipient), None)
            }
            // TODO: gate this!
            TransactionPayloadID::NakamotoCoinbase => {
                let payload: CoinbasePayload = read_next(fd)?;
                let principal_value_opt: Value = read_next(fd)?;
                let recipient_opt = if let Value::Optional(optional_data) = principal_value_opt {
                    if let Some(principal_value) = optional_data.data {
                        if let Value::Principal(recipient_principal) = *principal_value {
                            Some(recipient_principal)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    return Err(CodecError::DeserializeError("Failed to parse nakamoto coinbase transaction -- did not receive an optional recipient principal value".to_string()));
                };
                let vrf_proof: VRFProof = read_next(fd)?;
                TransactionPayload::Coinbase(payload, recipient_opt, Some(vrf_proof))
            }
            TransactionPayloadID::TenureChange => {
                let payload: TenureChangePayload = read_next(fd)?;
                TransactionPayload::TenureChange(payload)
            }
        };

        Ok(payload)
    }
}

impl StacksMessageCodec for AssetInfo {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.contract_address)?;
        write_next(fd, &self.contract_name)?;
        write_next(fd, &self.asset_name)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<AssetInfo, CodecError> {
        let contract_address: StacksAddress = read_next(fd)?;
        let contract_name: ContractName = read_next(fd)?;
        let asset_name: ClarityName = read_next(fd)?;
        Ok(AssetInfo {
            contract_address,
            contract_name,
            asset_name,
        })
    }
}

impl StacksMessageCodec for PostConditionPrincipal {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        match *self {
            PostConditionPrincipal::Origin => {
                write_next(fd, &(PostConditionPrincipalID::Origin as u8))?;
            }
            PostConditionPrincipal::Standard(ref address) => {
                write_next(fd, &(PostConditionPrincipalID::Standard as u8))?;
                write_next(fd, address)?;
            }
            PostConditionPrincipal::Contract(ref address, ref contract_name) => {
                write_next(fd, &(PostConditionPrincipalID::Contract as u8))?;
                write_next(fd, address)?;
                write_next(fd, contract_name)?;
            }
        }
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<PostConditionPrincipal, CodecError> {
        let principal_id: u8 = read_next(fd)?;
        let principal = match principal_id {
            x if x == PostConditionPrincipalID::Origin as u8 => PostConditionPrincipal::Origin,
            x if x == PostConditionPrincipalID::Standard as u8 => {
                let addr: StacksAddress = read_next(fd)?;
                PostConditionPrincipal::Standard(addr)
            }
            x if x == PostConditionPrincipalID::Contract as u8 => {
                let addr: StacksAddress = read_next(fd)?;
                let contract_name: ContractName = read_next(fd)?;
                PostConditionPrincipal::Contract(addr, contract_name)
            }
            _ => {
                return Err(CodecError::DeserializeError(format!(
                    "Failed to parse transaction: unknown post condition principal ID {principal_id}"
                )));
            }
        };
        Ok(principal)
    }
}

impl StacksMessageCodec for TransactionPostCondition {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        match *self {
            TransactionPostCondition::STX(ref principal, ref fungible_condition, ref amount) => {
                write_next(fd, &(AssetInfoID::STX as u8))?;
                write_next(fd, principal)?;
                write_next(fd, &(*fungible_condition as u8))?;
                write_next(fd, amount)?;
            }
            TransactionPostCondition::Fungible(
                ref principal,
                ref asset_info,
                ref fungible_condition,
                ref amount,
            ) => {
                write_next(fd, &(AssetInfoID::FungibleAsset as u8))?;
                write_next(fd, principal)?;
                write_next(fd, asset_info)?;
                write_next(fd, &(*fungible_condition as u8))?;
                write_next(fd, amount)?;
            }
            TransactionPostCondition::Nonfungible(
                ref principal,
                ref asset_info,
                ref asset_value,
                ref nonfungible_condition,
            ) => {
                write_next(fd, &(AssetInfoID::NonfungibleAsset as u8))?;
                write_next(fd, principal)?;
                write_next(fd, asset_info)?;
                write_next(fd, asset_value)?;
                write_next(fd, &(*nonfungible_condition as u8))?;
            }
        };
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<TransactionPostCondition, CodecError> {
        let asset_info_id: u8 = read_next(fd)?;
        let postcond = match asset_info_id {
            x if x == AssetInfoID::STX as u8 => {
                let principal: PostConditionPrincipal = read_next(fd)?;
                let condition_u8: u8 = read_next(fd)?;
                let amount: u64 = read_next(fd)?;

                let condition_code = FungibleConditionCode::from_u8(condition_u8).ok_or(
                    CodecError::DeserializeError(format!(
                    "Failed to parse transaction: Failed to parse STX fungible condition code {condition_u8}"
                )),
                )?;

                TransactionPostCondition::STX(principal, condition_code, amount)
            }
            x if x == AssetInfoID::FungibleAsset as u8 => {
                let principal: PostConditionPrincipal = read_next(fd)?;
                let asset: AssetInfo = read_next(fd)?;
                let condition_u8: u8 = read_next(fd)?;
                let amount: u64 = read_next(fd)?;

                let condition_code = FungibleConditionCode::from_u8(condition_u8).ok_or(
                    CodecError::DeserializeError(format!(
                    "Failed to parse transaction: Failed to parse FungibleAsset condition code {condition_u8}"
                )),
                )?;

                TransactionPostCondition::Fungible(principal, asset, condition_code, amount)
            }
            x if x == AssetInfoID::NonfungibleAsset as u8 => {
                let principal: PostConditionPrincipal = read_next(fd)?;
                let asset: AssetInfo = read_next(fd)?;
                let asset_value: Value = read_next(fd)?;
                let condition_u8: u8 = read_next(fd)?;

                let condition_code = NonfungibleConditionCode::from_u8(condition_u8)
                    .ok_or(CodecError::DeserializeError(format!(
                        "Failed to parse transaction: Failed to parse NonfungibleAsset condition code {condition_u8}"
                    )))?;

                TransactionPostCondition::Nonfungible(principal, asset, asset_value, condition_code)
            }
            _ => {
                return Err(CodecError::DeserializeError(format!(
                    "Failed to parse transaction: unknown asset info ID {asset_info_id}"
                )));
            }
        };

        Ok(postcond)
    }
}

impl StacksMessageCodec for TransactionAuth {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        match *self {
            TransactionAuth::Standard(ref origin_condition) => {
                write_next(fd, &(TransactionAuthFlags::AuthStandard as u8))?;
                write_next(fd, origin_condition)?;
            }
            TransactionAuth::Sponsored(ref origin_condition, ref sponsor_condition) => {
                write_next(fd, &(TransactionAuthFlags::AuthSponsored as u8))?;
                write_next(fd, origin_condition)?;
                write_next(fd, sponsor_condition)?;
            }
        }
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<TransactionAuth, CodecError> {
        let type_id: u8 = read_next(fd)?;
        let auth = match type_id {
            x if x == TransactionAuthFlags::AuthStandard as u8 => {
                let origin_auth: TransactionSpendingCondition = read_next(fd)?;
                TransactionAuth::Standard(origin_auth)
            }
            x if x == TransactionAuthFlags::AuthSponsored as u8 => {
                let origin_auth: TransactionSpendingCondition = read_next(fd)?;
                let sponsor_auth: TransactionSpendingCondition = read_next(fd)?;
                TransactionAuth::Sponsored(origin_auth, sponsor_auth)
            }
            _ => {
                return Err(CodecError::DeserializeError(format!(
                    "Failed to parse transaction authorization: unrecognized auth flags {type_id}"
                )));
            }
        };
        Ok(auth)
    }
}

impl StacksMessageCodec for TransactionSpendingCondition {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        match *self {
            TransactionSpendingCondition::Singlesig(ref data) => {
                data.consensus_serialize(fd)?;
            }
            TransactionSpendingCondition::Multisig(ref data) => {
                data.consensus_serialize(fd)?;
            }
            TransactionSpendingCondition::OrderIndependentMultisig(ref data) => {
                data.consensus_serialize(fd)?;
            }
        }
        Ok(())
    }

    fn consensus_deserialize<R: Read>(
        fd: &mut R,
    ) -> Result<TransactionSpendingCondition, CodecError> {
        // peek the hash mode byte
        let hash_mode_u8: u8 = read_next(fd)?;
        let peek_buf = [hash_mode_u8];
        let mut rrd = peek_buf.chain(fd);
        let cond = {
            if SinglesigHashMode::from_u8(hash_mode_u8).is_some() {
                let cond = SinglesigSpendingCondition::consensus_deserialize(&mut rrd)?;
                TransactionSpendingCondition::Singlesig(cond)
            } else if MultisigHashMode::from_u8(hash_mode_u8).is_some() {
                let cond = MultisigSpendingCondition::consensus_deserialize(&mut rrd)?;
                TransactionSpendingCondition::Multisig(cond)
            } else if OrderIndependentMultisigHashMode::from_u8(hash_mode_u8).is_some() {
                let cond =
                    OrderIndependentMultisigSpendingCondition::consensus_deserialize(&mut rrd)?;
                TransactionSpendingCondition::OrderIndependentMultisig(cond)
            } else {
                return Err(CodecError::DeserializeError(format!(
                    "Failed to parse spending condition: invalid hash mode {hash_mode_u8}"
                )));
            }
        };

        Ok(cond)
    }
}

impl StacksMessageCodec for SinglesigSpendingCondition {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &(self.hash_mode.clone() as u8))?;
        write_next(fd, &self.signer)?;
        write_next(fd, &self.nonce)?;
        write_next(fd, &self.tx_fee)?;
        write_next(fd, &(self.key_encoding as u8))?;
        write_next(fd, &self.signature)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(
        fd: &mut R,
    ) -> Result<SinglesigSpendingCondition, CodecError> {
        let hash_mode_u8: u8 = read_next(fd)?;
        let hash_mode = SinglesigHashMode::from_u8(hash_mode_u8).ok_or(
            CodecError::DeserializeError(format!(
                "Failed to parse singlesig spending condition: unknown hash mode {hash_mode_u8}"
            )),
        )?;

        let signer: Hash160 = read_next(fd)?;
        let nonce: u64 = read_next(fd)?;
        let tx_fee: u64 = read_next(fd)?;

        let key_encoding_u8: u8 = read_next(fd)?;
        let key_encoding = TransactionPublicKeyEncoding::from_u8(key_encoding_u8).ok_or(
            CodecError::DeserializeError(format!(
            "Failed to parse singlesig spending condition: unknown key encoding {key_encoding_u8}"
        )),
        )?;

        let signature: MessageSignature = read_next(fd)?;

        // sanity check -- must be compressed if we're using p2wpkh
        if hash_mode == SinglesigHashMode::P2WPKH
            && key_encoding != TransactionPublicKeyEncoding::Compressed
        {
            return Err(CodecError::DeserializeError("Failed to parse singlesig spending condition: incompatible hash mode and key encoding".to_string()));
        }

        Ok(SinglesigSpendingCondition {
            signer,
            nonce,
            tx_fee,
            hash_mode,
            key_encoding,
            signature,
        })
    }
}

impl StacksMessageCodec for MultisigSpendingCondition {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &(self.hash_mode.clone() as u8))?;
        write_next(fd, &self.signer)?;
        write_next(fd, &self.nonce)?;
        write_next(fd, &self.tx_fee)?;
        write_next(fd, &self.fields)?;
        write_next(fd, &self.signatures_required)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<MultisigSpendingCondition, CodecError> {
        let hash_mode_u8: u8 = read_next(fd)?;
        let hash_mode = MultisigHashMode::from_u8(hash_mode_u8).ok_or(
            CodecError::DeserializeError(format!(
                "Failed to parse multisig spending condition: unknown hash mode {hash_mode_u8}"
            )),
        )?;

        let signer: Hash160 = read_next(fd)?;
        let nonce: u64 = read_next(fd)?;
        let tx_fee: u64 = read_next(fd)?;
        let fields: Vec<TransactionAuthField> = {
            let mut bound_read = BoundReader::from_reader(fd, MAX_MESSAGE_LEN as u64);
            read_next(&mut bound_read)
        }?;

        let signatures_required: u16 = read_next(fd)?;

        // read and decode _exactly_ num_signatures signature buffers
        let mut num_sigs_given: u16 = 0;
        let mut have_uncompressed = false;
        for f in fields.iter() {
            match *f {
                TransactionAuthField::Signature(ref key_encoding, _) => {
                    num_sigs_given =
                        num_sigs_given
                            .checked_add(1)
                            .ok_or(CodecError::DeserializeError(
                                "Failed to parse multisig spending condition: too many signatures"
                                    .to_string(),
                            ))?;
                    if *key_encoding == TransactionPublicKeyEncoding::Uncompressed {
                        have_uncompressed = true;
                    }
                }
                TransactionAuthField::PublicKey(ref pubk) => {
                    if !pubk.compressed() {
                        have_uncompressed = true;
                    }
                }
            };
        }

        // must be given the right number of signatures
        if num_sigs_given != signatures_required {
            return Err(CodecError::DeserializeError(format!(
                "Failed to parse multisig spending condition: got {num_sigs_given} sigs, expected {signatures_required}"
            )));
        }

        // must all be compressed if we're using P2WSH
        if have_uncompressed && hash_mode == MultisigHashMode::P2WSH {
            return Err(CodecError::DeserializeError(
                "Failed to parse multisig spending condition: expected compressed keys only"
                    .to_string(),
            ));
        }

        Ok(MultisigSpendingCondition {
            signer,
            nonce,
            tx_fee,
            hash_mode,
            fields,
            signatures_required,
        })
    }
}

impl StacksMessageCodec for OrderIndependentMultisigSpendingCondition {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &(self.hash_mode.clone() as u8))?;
        write_next(fd, &self.signer)?;
        write_next(fd, &self.nonce)?;
        write_next(fd, &self.tx_fee)?;
        write_next(fd, &self.fields)?;
        write_next(fd, &self.signatures_required)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(
        fd: &mut R,
    ) -> Result<OrderIndependentMultisigSpendingCondition, CodecError> {
        let hash_mode_u8: u8 = read_next(fd)?;
        let hash_mode = OrderIndependentMultisigHashMode::from_u8(hash_mode_u8).ok_or(
            CodecError::DeserializeError(format!(
                "Failed to parse multisig spending condition: unknown hash mode {hash_mode_u8}"
            )),
        )?;

        let signer: Hash160 = read_next(fd)?;
        let nonce: u64 = read_next(fd)?;
        let tx_fee: u64 = read_next(fd)?;
        let fields: Vec<TransactionAuthField> = {
            let mut bound_read = BoundReader::from_reader(fd, MAX_MESSAGE_LEN as u64);
            read_next(&mut bound_read)
        }?;

        let signatures_required: u16 = read_next(fd)?;

        // read and decode _exactly_ num_signatures signature buffers
        let mut num_sigs_given: u16 = 0;
        let mut have_uncompressed = false;
        for f in fields.iter() {
            match *f {
                TransactionAuthField::Signature(ref key_encoding, _) => {
                    num_sigs_given =
                        num_sigs_given
                            .checked_add(1)
                            .ok_or(CodecError::DeserializeError(
                                "Failed to parse order independent multisig spending condition: too many signatures"
                                    .to_string(),
                            ))?;
                    if *key_encoding == TransactionPublicKeyEncoding::Uncompressed {
                        have_uncompressed = true;
                    }
                }
                TransactionAuthField::PublicKey(ref pubk) => {
                    if !pubk.compressed() {
                        have_uncompressed = true;
                    }
                }
            };
        }

        // must be given the right number of signatures
        if num_sigs_given < signatures_required {
            let msg = format!(
                "Failed to deserialize order independent multisig spending condition: got {num_sigs_given} sigs, expected at least {signatures_required}"
            );
            return Err(CodecError::DeserializeError(msg));
        }

        // must all be compressed if we're using P2WSH
        if have_uncompressed && hash_mode == OrderIndependentMultisigHashMode::P2WSH {
            let msg = "Failed to deserialize order independent multisig spending condition: expected compressed keys only".to_string();
            return Err(CodecError::DeserializeError(msg));
        }

        Ok(OrderIndependentMultisigSpendingCondition {
            signer,
            nonce,
            tx_fee,
            hash_mode,
            fields,
            signatures_required,
        })
    }
}

/// A container for public keys (compressed secp256k1 public keys)
pub struct StacksPublicKeyBuffer(pub [u8; 33]);
impl_array_newtype!(StacksPublicKeyBuffer, u8, 33);
impl_array_hexstring_fmt!(StacksPublicKeyBuffer);
impl_byte_array_newtype!(StacksPublicKeyBuffer, u8, 33);
impl_byte_array_message_codec!(StacksPublicKeyBuffer, 33);

impl StacksPublicKeyBuffer {
    pub fn from_public_key(pubkey: &Secp256k1PublicKey) -> StacksPublicKeyBuffer {
        let pubkey_bytes_vec = pubkey.to_bytes_compressed();
        let mut pubkey_bytes = [0u8; 33];
        pubkey_bytes.copy_from_slice(&pubkey_bytes_vec[..]);
        StacksPublicKeyBuffer(pubkey_bytes)
    }

    pub fn to_public_key(&self) -> Result<Secp256k1PublicKey, CodecError> {
        Secp256k1PublicKey::from_slice(&self.0).map_err(|_e_str| {
            CodecError::DeserializeError("Failed to decode Stacks public key".to_string())
        })
    }
}

impl StacksMessageCodec for TransactionAuthField {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        match *self {
            TransactionAuthField::PublicKey(ref pubk) => {
                let field_id = if pubk.compressed() {
                    TransactionAuthFieldID::PublicKeyCompressed
                } else {
                    TransactionAuthFieldID::PublicKeyUncompressed
                };

                let pubkey_buf = StacksPublicKeyBuffer::from_public_key(pubk);

                write_next(fd, &(field_id as u8))?;
                write_next(fd, &pubkey_buf)?;
            }
            TransactionAuthField::Signature(ref key_encoding, ref sig) => {
                let field_id = if *key_encoding == TransactionPublicKeyEncoding::Compressed {
                    TransactionAuthFieldID::SignatureCompressed
                } else {
                    TransactionAuthFieldID::SignatureUncompressed
                };

                write_next(fd, &(field_id as u8))?;
                write_next(fd, sig)?;
            }
        }
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<TransactionAuthField, CodecError> {
        let field_id: u8 = read_next(fd)?;
        let field = match field_id {
            x if x == TransactionAuthFieldID::PublicKeyCompressed as u8 => {
                let pubkey_buf: StacksPublicKeyBuffer = read_next(fd)?;
                let mut pubkey = pubkey_buf.to_public_key()?;
                pubkey.set_compressed(true);

                TransactionAuthField::PublicKey(pubkey)
            }
            x if x == TransactionAuthFieldID::PublicKeyUncompressed as u8 => {
                let pubkey_buf: StacksPublicKeyBuffer = read_next(fd)?;
                let mut pubkey = pubkey_buf.to_public_key()?;
                pubkey.set_compressed(false);

                TransactionAuthField::PublicKey(pubkey)
            }
            x if x == TransactionAuthFieldID::SignatureCompressed as u8 => {
                let sig: MessageSignature = read_next(fd)?;
                TransactionAuthField::Signature(TransactionPublicKeyEncoding::Compressed, sig)
            }
            x if x == TransactionAuthFieldID::SignatureUncompressed as u8 => {
                let sig: MessageSignature = read_next(fd)?;
                TransactionAuthField::Signature(TransactionPublicKeyEncoding::Uncompressed, sig)
            }
            _ => {
                return Err(CodecError::DeserializeError(format!(
                    "Failed to parse auth field: unknown auth field ID {field_id}"
                )));
            }
        };
        Ok(field)
    }
}

impl StacksMessageCodec for StacksTransaction {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &(self.version as u8))?;
        write_next(fd, &self.chain_id)?;
        write_next(fd, &self.auth)?;
        write_next(fd, &(self.anchor_mode as u8))?;
        write_next(fd, &(self.post_condition_mode as u8))?;
        write_next(fd, &self.post_conditions)?;
        write_next(fd, &self.payload)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<StacksTransaction, CodecError> {
        StacksTransaction::consensus_deserialize_with_len(fd).map(|(result, _)| result)
    }
}

define_u8_enum!(
/// Enum representing the SignerMessage type prefix
SignerMessageTypePrefix {
    /// Block Proposal message from miners
    BlockProposal = 0,
    /// Block Response message from signers
    BlockResponse = 1,
    /// Block Pushed message from miners
    BlockPushed = 2,
    /// Mock block proposal message from Epoch 2.5 miners
    MockProposal = 3,
    /// Mock block signature message from Epoch 2.5 signers
    MockSignature = 4,
    /// Mock block message from Epoch 2.5 miners
    MockBlock = 5
});

impl TryFrom<u8> for SignerMessageTypePrefix {
    type Error = CodecError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::from_u8(value).ok_or_else(|| {
            CodecError::DeserializeError(format!("Unknown signer message type prefix: {value}"))
        })
    }
}

impl From<&SignerMessage> for SignerMessageTypePrefix {
    fn from(message: &SignerMessage) -> Self {
        match message {
            SignerMessage::BlockProposal(_) => SignerMessageTypePrefix::BlockProposal,
            SignerMessage::BlockResponse(_) => SignerMessageTypePrefix::BlockResponse,
            SignerMessage::BlockPushed(_) => SignerMessageTypePrefix::BlockPushed,
            SignerMessage::MockProposal(_) => SignerMessageTypePrefix::MockProposal,
            SignerMessage::MockSignature(_) => SignerMessageTypePrefix::MockSignature,
            SignerMessage::MockBlock(_) => SignerMessageTypePrefix::MockBlock,
        }
    }
}

define_u8_enum!(
/// Enum representing the BlockResponse type prefix
BlockResponseTypePrefix {
    /// An accepted block response
    Accepted = 0,
    /// A rejected block response
    Rejected = 1
});

impl TryFrom<u8> for BlockResponseTypePrefix {
    type Error = CodecError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::from_u8(value).ok_or_else(|| {
            CodecError::DeserializeError(format!("Unknown block response type prefix: {value}"))
        })
    }
}

impl From<&BlockResponse> for BlockResponseTypePrefix {
    fn from(block_response: &BlockResponse) -> Self {
        match block_response {
            BlockResponse::Accepted(_) => BlockResponseTypePrefix::Accepted,
            BlockResponse::Rejected(_) => BlockResponseTypePrefix::Rejected,
        }
    }
}

// This enum is used to supply a `reason_code` for validation
//  rejection responses. This is serialized as an enum with string
//  type (in jsonschema terminology).
define_u8_enum![ValidateRejectCode {
    BadBlockHash = 0,
    BadTransaction = 1,
    InvalidBlock = 2,
    ChainstateError = 3,
    UnknownParent = 4,
    NonCanonicalTenure = 5,
    NoSuchTenure = 6
}];

impl TryFrom<u8> for ValidateRejectCode {
    type Error = CodecError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::from_u8(value)
            .ok_or_else(|| CodecError::DeserializeError(format!("Unknown type prefix: {value}")))
    }
}

define_u8_enum!(
/// Enum representing the reject code type prefix
RejectCodeTypePrefix {
    /// The block was rejected due to validation issues
    ValidationFailed = 0,
    /// The block was rejected due to connectivity issues with the signer
    ConnectivityIssues = 1,
    /// The block was rejected in a prior round
    RejectedInPriorRound = 2,
    /// The block was rejected due to no sortition view
    NoSortitionView = 3,
    /// The block was rejected due to a mismatch with expected sortition view
    SortitionViewMismatch = 4,
    /// The block was rejected due to a testing directive
    TestingDirective = 5
});

impl TryFrom<u8> for RejectCodeTypePrefix {
    type Error = CodecError;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::from_u8(value).ok_or_else(|| {
            CodecError::DeserializeError(format!("Unknown reject code type prefix: {value}"))
        })
    }
}

impl From<&RejectCode> for RejectCodeTypePrefix {
    fn from(reject_code: &RejectCode) -> Self {
        match reject_code {
            RejectCode::ValidationFailed(_) => RejectCodeTypePrefix::ValidationFailed,
            RejectCode::ConnectivityIssues => RejectCodeTypePrefix::ConnectivityIssues,
            RejectCode::RejectedInPriorRound => RejectCodeTypePrefix::RejectedInPriorRound,
            RejectCode::NoSortitionView => RejectCodeTypePrefix::NoSortitionView,
            RejectCode::SortitionViewMismatch => RejectCodeTypePrefix::SortitionViewMismatch,
            RejectCode::TestingDirective => RejectCodeTypePrefix::TestingDirective,
        }
    }
}

/// This enum is used to supply a `reason_code` for block rejections
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RejectCode {
    /// RPC endpoint Validation failed
    ValidationFailed(ValidateRejectCode),
    /// No Sortition View to verify against
    NoSortitionView,
    /// The block was rejected due to connectivity issues with the signer
    ConnectivityIssues,
    /// The block was rejected in a prior round
    RejectedInPriorRound,
    /// The block was rejected due to a mismatch with expected sortition view
    SortitionViewMismatch,
    /// The block was rejected due to a testing directive
    TestingDirective,
}

impl StacksMessageCodec for RejectCode {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &(RejectCodeTypePrefix::from(self) as u8))?;
        // Do not do a single match here as we may add other variants in the future and don't want to miss adding it
        match self {
            RejectCode::ValidationFailed(code) => write_next(fd, &(*code as u8))?,
            RejectCode::ConnectivityIssues
            | RejectCode::RejectedInPriorRound
            | RejectCode::NoSortitionView
            | RejectCode::SortitionViewMismatch
            | RejectCode::TestingDirective => {
                // No additional data to serialize / deserialize
            }
        };
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        let type_prefix_byte = read_next::<u8, _>(fd)?;
        let type_prefix = RejectCodeTypePrefix::try_from(type_prefix_byte)?;
        let code = match type_prefix {
            RejectCodeTypePrefix::ValidationFailed => RejectCode::ValidationFailed(
                ValidateRejectCode::try_from(read_next::<u8, _>(fd)?).map_err(|e| {
                    CodecError::DeserializeError(format!(
                        "Failed to decode validation reject code: {:?}",
                        &e
                    ))
                })?,
            ),
            RejectCodeTypePrefix::ConnectivityIssues => RejectCode::ConnectivityIssues,
            RejectCodeTypePrefix::RejectedInPriorRound => RejectCode::RejectedInPriorRound,
            RejectCodeTypePrefix::NoSortitionView => RejectCode::NoSortitionView,
            RejectCodeTypePrefix::SortitionViewMismatch => RejectCode::SortitionViewMismatch,
            RejectCodeTypePrefix::TestingDirective => RejectCode::TestingDirective,
        };
        Ok(code)
    }
}

/// A rejection response from a signer for a proposed block
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct BlockAccepted {
    /// The signer signature hash of the block that was accepted
    pub signer_signature_hash: Sha512Trunc256Sum,
    /// The signer's signature across the acceptance
    pub signature: MessageSignature,
    /// Signer message metadata
    pub metadata: SignerMessageMetadata,
}

impl StacksMessageCodec for BlockAccepted {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.signer_signature_hash)?;
        write_next(fd, &self.signature)?;
        write_next(fd, &self.metadata)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        let signer_signature_hash = read_next::<Sha512Trunc256Sum, _>(fd)?;
        let signature = read_next::<MessageSignature, _>(fd)?;
        let metadata = read_next::<SignerMessageMetadata, _>(fd)?;
        Ok(Self {
            signer_signature_hash,
            signature,
            metadata,
        })
    }
}

/// A rejection response from a signer for a proposed block
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct BlockRejection {
    /// The reason for the rejection
    pub reason: String,
    /// The reason code for the rejection
    pub reason_code: RejectCode,
    /// The signer signature hash of the block that was rejected
    pub signer_signature_hash: Sha512Trunc256Sum,
    /// The signer's signature across the rejection
    pub signature: MessageSignature,
    /// The chain id
    pub chain_id: u32,
    /// Signer message metadata
    pub metadata: SignerMessageMetadata,
}

impl StacksMessageCodec for BlockRejection {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.reason.as_bytes().to_vec())?;
        write_next(fd, &self.reason_code)?;
        write_next(fd, &self.signer_signature_hash)?;
        write_next(fd, &self.chain_id)?;
        write_next(fd, &self.signature)?;
        write_next(fd, &self.metadata)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        let reason_bytes = read_next::<Vec<u8>, _>(fd)?;
        let reason = String::from_utf8(reason_bytes).map_err(|e| {
            CodecError::DeserializeError(format!("Failed to decode reason string: {:?}", &e))
        })?;
        let reason_code = read_next::<RejectCode, _>(fd)?;
        let signer_signature_hash = read_next::<Sha512Trunc256Sum, _>(fd)?;
        let chain_id = read_next::<u32, _>(fd)?;
        let signature = read_next::<MessageSignature, _>(fd)?;
        let metadata = read_next::<SignerMessageMetadata, _>(fd)?;
        Ok(Self {
            reason,
            reason_code,
            signer_signature_hash,
            chain_id,
            signature,
            metadata,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
/// BlockProposal sent to signers
pub struct BlockProposal {
    /// The block itself
    pub block: NakamotoBlock,
    /// The burn height the block is mined during
    pub burn_height: u64,
    /// The reward cycle the block is mined during
    pub reward_cycle: u64,
}

impl StacksMessageCodec for BlockProposal {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        self.block.consensus_serialize(fd)?;
        self.burn_height.consensus_serialize(fd)?;
        self.reward_cycle.consensus_serialize(fd)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        let block = NakamotoBlock::consensus_deserialize(fd)?;
        let burn_height = u64::consensus_deserialize(fd)?;
        let reward_cycle = u64::consensus_deserialize(fd)?;
        Ok(BlockProposal {
            block,
            burn_height,
            reward_cycle,
        })
    }
}

/// The response that a signer sends back to observing miners
/// either accepting or rejecting a Nakamoto block with the corresponding reason
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum BlockResponse {
    /// The Nakamoto block was accepted and therefore signed
    Accepted(BlockAccepted),
    /// The Nakamoto block was rejected and therefore not signed
    Rejected(BlockRejection),
}

impl StacksMessageCodec for BlockResponse {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &(BlockResponseTypePrefix::from(self) as u8))?;
        match self {
            BlockResponse::Accepted(accepted) => {
                write_next(fd, accepted)?;
            }
            BlockResponse::Rejected(rejection) => {
                write_next(fd, rejection)?;
            }
        };
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        let type_prefix_byte = read_next::<u8, _>(fd)?;
        let type_prefix = BlockResponseTypePrefix::try_from(type_prefix_byte)?;
        let response = match type_prefix {
            BlockResponseTypePrefix::Accepted => {
                let accepted = read_next::<BlockAccepted, _>(fd)?;
                BlockResponse::Accepted(accepted)
            }
            BlockResponseTypePrefix::Rejected => {
                let rejection = read_next::<BlockRejection, _>(fd)?;
                BlockResponse::Rejected(rejection)
            }
        };
        Ok(response)
    }
}

/// Metadata for signer messages
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct SignerMessageMetadata {
    /// The signer's server version
    pub server_version: String,
}

/// To ensure backwards compatibility, when deserializing,
/// if no bytes are found, return empty metadata
impl StacksMessageCodec for SignerMessageMetadata {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.server_version.as_bytes().to_vec())?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        match read_next::<Vec<u8>, _>(fd) {
            Ok(server_version) => {
                let server_version = String::from_utf8(server_version).map_err(|e| {
                    CodecError::DeserializeError(format!(
                        "Failed to decode server version: {:?}",
                        &e
                    ))
                })?;
                Ok(Self { server_version })
            }
            Err(_) => {
                // For backwards compatibility, return empty metadata
                Ok(Self::empty())
            }
        }
    }
}

impl SignerMessageMetadata {
    /// Empty metadata
    pub fn empty() -> Self {
        Self {
            server_version: String::new(),
        }
    }
}

/// A mock signature for the stacks node to be used for mock signing.
/// This is only used by Epoch 2.5 signers to simulate the signing of a block for every sortition.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MockSignature {
    /// The signer's signature across the mock proposal
    pub signature: MessageSignature,
    /// The mock block proposal that was signed across
    pub mock_proposal: MockProposal,
    /// The signature metadata
    pub metadata: SignerMessageMetadata,
}

impl StacksMessageCodec for MockSignature {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.signature)?;
        self.mock_proposal.consensus_serialize(fd)?;
        self.metadata.consensus_serialize(fd)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        let signature = read_next::<MessageSignature, _>(fd)?;
        let mock_proposal = MockProposal::consensus_deserialize(fd)?;
        let metadata = SignerMessageMetadata::consensus_deserialize(fd)?;
        Ok(Self {
            signature,
            mock_proposal,
            metadata,
        })
    }
}

/// The signer relevant peer information from the stacks node
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PeerInfo {
    /// The burn block height
    pub burn_block_height: u64,
    /// The consensus hash of the stacks tip
    pub stacks_tip_consensus_hash: ConsensusHash,
    /// The stacks tip
    pub stacks_tip: BlockHeaderHash,
    /// The stacks tip height
    pub stacks_tip_height: u64,
    /// The pox consensus
    pub pox_consensus: ConsensusHash,
    /// The server version
    pub server_version: String,
    /// The network id
    pub network_id: u32,
}

impl StacksMessageCodec for PeerInfo {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        write_next(fd, &self.burn_block_height)?;
        write_next(fd, self.stacks_tip_consensus_hash.as_bytes())?;
        write_next(fd, &self.stacks_tip)?;
        write_next(fd, &self.stacks_tip_height)?;
        write_next(fd, &(self.server_version.len() as u8))?;
        fd.write_all(self.server_version.as_bytes())
            .map_err(CodecError::WriteError)?;
        write_next(fd, &self.pox_consensus)?;
        write_next(fd, &self.network_id)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        let burn_block_height = read_next::<u64, _>(fd)?;
        let stacks_tip_consensus_hash = read_next::<ConsensusHash, _>(fd)?;
        let stacks_tip = read_next::<BlockHeaderHash, _>(fd)?;
        let stacks_tip_height = read_next::<u64, _>(fd)?;
        let len_byte: u8 = read_next(fd)?;
        let mut bytes = vec![0u8; len_byte as usize];
        fd.read_exact(&mut bytes).map_err(CodecError::ReadError)?;
        // must encode a valid string
        let server_version = String::from_utf8(bytes).map_err(|_e| {
            CodecError::DeserializeError(
                "Failed to parse server version name: could not construct from utf8".to_string(),
            )
        })?;
        let pox_consensus = read_next::<ConsensusHash, _>(fd)?;
        let network_id = read_next(fd)?;
        Ok(Self {
            burn_block_height,
            stacks_tip_consensus_hash,
            stacks_tip,
            stacks_tip_height,
            server_version,
            pox_consensus,
            network_id,
        })
    }
}

/// A mock block proposal for Epoch 2.5 mock signing
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MockProposal {
    /// The view of the stacks node peer information at the time of the mock proposal
    pub peer_info: PeerInfo,
    /// The miner's signature across the peer info
    pub signature: MessageSignature,
}

// Helper function to generate domain for structured data hash
pub fn make_structured_data_domain(name: &str, version: &str, chain_id: u32) -> Value {
    Value::Tuple(
        TupleData::from_data(vec![
            (
                "name".into(),
                Value::string_ascii_from_bytes(name.into()).unwrap(),
            ),
            (
                "version".into(),
                Value::string_ascii_from_bytes(version.into()).unwrap(),
            ),
            ("chain-id".into(), Value::UInt(chain_id.into())),
        ])
        .unwrap(),
    )
}

/// Message prefix for signed structured data. "SIP018" in ascii
pub const STRUCTURED_DATA_PREFIX: [u8; 6] = [0x53, 0x49, 0x50, 0x30, 0x31, 0x38];

pub fn structured_data_hash(value: Value) -> Sha256Sum {
    let mut bytes = vec![];
    value.serialize_write(&mut bytes).unwrap();
    Sha256Sum::from_data(bytes.as_slice())
}

/// Generate a message hash for signing structured Clarity data.
/// Reference [SIP018](https://github.com/stacksgov/sips/blob/main/sips/sip-018/sip-018-signed-structured-data.md) for more information.
pub fn structured_data_message_hash(structured_data: Value, domain: Value) -> Sha256Sum {
    let message = [
        STRUCTURED_DATA_PREFIX.as_ref(),
        structured_data_hash(domain).as_bytes(),
        structured_data_hash(structured_data).as_bytes(),
    ]
    .concat();

    Sha256Sum::from_data(&message)
}

impl MockProposal {
    /// The signature hash for the mock proposal
    pub fn miner_signature_hash(&self) -> Sha256Sum {
        let domain_tuple =
            make_structured_data_domain("mock-miner", "1.0.0", self.peer_info.network_id);
        let data_tuple = Value::Tuple(
            TupleData::from_data(vec![
                (
                    "stacks-tip-consensus-hash".into(),
                    Value::buff_from((*self.peer_info.stacks_tip_consensus_hash.as_bytes()).into())
                        .unwrap(),
                ),
                (
                    "stacks-tip".into(),
                    Value::buff_from((*self.peer_info.stacks_tip.as_bytes()).into()).unwrap(),
                ),
                (
                    "stacks-tip-height".into(),
                    Value::UInt(self.peer_info.stacks_tip_height.into()),
                ),
                (
                    "server-version".into(),
                    Value::string_ascii_from_bytes(self.peer_info.server_version.clone().into())
                        .unwrap(),
                ),
                (
                    "pox-consensus".into(),
                    Value::buff_from((*self.peer_info.pox_consensus.as_bytes()).into()).unwrap(),
                ),
            ])
            .expect("Error creating signature hash"),
        );
        structured_data_message_hash(data_tuple, domain_tuple)
    }

    /// The signature hash including the miner's signature. Used by signers.
    pub fn signer_signature_hash(&self) -> Sha256Sum {
        let domain_tuple =
            make_structured_data_domain("mock-signer", "1.0.0", self.peer_info.network_id);
        let data_tuple = Value::Tuple(
            TupleData::from_data(vec![
                (
                    "miner-signature-hash".into(),
                    Value::buff_from((*self.miner_signature_hash().as_bytes()).into()).unwrap(),
                ),
                (
                    "miner-signature".into(),
                    Value::buff_from((*self.signature.as_bytes()).into()).unwrap(),
                ),
            ])
            .expect("Error creating signature hash"),
        );
        structured_data_message_hash(data_tuple, domain_tuple)
    }
}

impl StacksMessageCodec for MockProposal {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        self.peer_info.consensus_serialize(fd)?;
        write_next(fd, &self.signature)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        let peer_info = PeerInfo::consensus_deserialize(fd)?;
        let signature = read_next::<MessageSignature, _>(fd)?;
        Ok(Self {
            peer_info,
            signature,
        })
    }
}

/// The mock block data for epoch 2.5 miners to broadcast to simulate block signing
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MockBlock {
    /// The mock proposal that was signed across
    pub mock_proposal: MockProposal,
    /// The mock signatures that the miner received
    pub mock_signatures: Vec<MockSignature>,
}

impl StacksMessageCodec for MockBlock {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        self.mock_proposal.consensus_serialize(fd)?;
        write_next(fd, &self.mock_signatures)?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        let mock_proposal = MockProposal::consensus_deserialize(fd)?;
        let mock_signatures = read_next::<Vec<MockSignature>, _>(fd)?;
        Ok(Self {
            mock_proposal,
            mock_signatures,
        })
    }
}

/// The messages being sent through the stacker db contracts
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum SignerMessage {
    /// The block proposal from miners for signers to observe and sign
    BlockProposal(BlockProposal),
    /// The block response from signers for miners to observe
    BlockResponse(BlockResponse),
    /// A block pushed from miners to the signers set
    BlockPushed(NakamotoBlock),
    /// A mock signature from the epoch 2.5 signers
    MockSignature(MockSignature),
    /// A mock message from the epoch 2.5 miners
    MockProposal(MockProposal),
    /// A mock block from the epoch 2.5 miners
    MockBlock(MockBlock),
}

impl StacksMessageCodec for SignerMessage {
    fn consensus_serialize<W: Write>(&self, fd: &mut W) -> Result<(), CodecError> {
        SignerMessageTypePrefix::from(self)
            .to_u8()
            .consensus_serialize(fd)?;
        match self {
            SignerMessage::BlockProposal(block_proposal) => block_proposal.consensus_serialize(fd),
            SignerMessage::BlockResponse(block_response) => block_response.consensus_serialize(fd),
            SignerMessage::BlockPushed(block) => block.consensus_serialize(fd),
            SignerMessage::MockSignature(signature) => signature.consensus_serialize(fd),
            SignerMessage::MockProposal(message) => message.consensus_serialize(fd),
            SignerMessage::MockBlock(block) => block.consensus_serialize(fd),
        }?;
        Ok(())
    }

    fn consensus_deserialize<R: Read>(fd: &mut R) -> Result<Self, CodecError> {
        let type_prefix_byte = u8::consensus_deserialize(fd)?;
        let type_prefix = SignerMessageTypePrefix::try_from(type_prefix_byte)?;
        let message = match type_prefix {
            SignerMessageTypePrefix::BlockProposal => {
                let block_proposal = StacksMessageCodec::consensus_deserialize(fd)?;
                SignerMessage::BlockProposal(block_proposal)
            }
            SignerMessageTypePrefix::BlockResponse => {
                let block_response = StacksMessageCodec::consensus_deserialize(fd)?;
                SignerMessage::BlockResponse(block_response)
            }
            SignerMessageTypePrefix::BlockPushed => {
                let block = StacksMessageCodec::consensus_deserialize(fd)?;
                SignerMessage::BlockPushed(block)
            }
            SignerMessageTypePrefix::MockProposal => {
                let message = StacksMessageCodec::consensus_deserialize(fd)?;
                SignerMessage::MockProposal(message)
            }
            SignerMessageTypePrefix::MockSignature => {
                let signature = StacksMessageCodec::consensus_deserialize(fd)?;
                SignerMessage::MockSignature(signature)
            }
            SignerMessageTypePrefix::MockBlock => {
                let block = StacksMessageCodec::consensus_deserialize(fd)?;
                SignerMessage::MockBlock(block)
            }
        };
        Ok(message)
    }
}
