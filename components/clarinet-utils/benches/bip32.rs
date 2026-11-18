//! What `get_bip32_keys_from_mnemonic` costs in each of its three cache
//! states, and where the time goes when it has to derive.
//!
//! The derivation runs two stages back to back, once per wallet in
//! `settings/Devnet.toml`:
//!   1. `Mnemonic::to_seed` — PBKDF2-HMAC-SHA512, 2048 rounds
//!   2. `XPrv::derive_from_path` — one HMAC-SHA512 + one secp256k1 point
//!      multiplication per level (5 levels for `m/44'/5757'/0'/0/0`)
//!
//! The public key is free: the derived signing key already holds it.

use std::hint::black_box;
use std::str::FromStr;

use bip32::{DerivationPath, XPrv};
use bip39::Mnemonic;
use clarinet_utils::{
    get_bip32_keys_from_mnemonic, random_mnemonic, DerivedKeys, DEFAULT_DEPLOYER_MNEMONIC,
    DEFAULT_DERIVATION_PATH,
};

fn main() {
    divan::main();
}

/// A mnemonic no cache has seen: the full derivation.
#[divan::bench]
fn uncached(bencher: divan::Bencher) {
    bencher.with_inputs(random_mnemonic).bench_values(|phrase| {
        get_bip32_keys_from_mnemonic(&phrase, DEFAULT_DERIVATION_PATH).unwrap()
    });
}

/// A mnemonic Clarinet ships: resolved from the compile-time table.
#[divan::bench]
fn precomputed() -> DerivedKeys {
    get_bip32_keys_from_mnemonic(
        black_box(DEFAULT_DEPLOYER_MNEMONIC),
        black_box(DEFAULT_DERIVATION_PATH),
    )
    .unwrap()
}

/// A custom mnemonic on its second and later use: resolved from the memo.
///
/// Uses a 24-word phrase at a non-default path — it misses the table and lands
/// in the memo, and the key length is most of a memo hit, so a 12-word
/// `random_mnemonic()` would understate it against the `precomputed` row.
#[divan::bench]
fn memoized(bencher: divan::Bencher) {
    const OFF_TABLE_PATH: &str = "m/44'/5757'/0'/0/9";
    get_bip32_keys_from_mnemonic(DEFAULT_DEPLOYER_MNEMONIC, OFF_TABLE_PATH).unwrap();
    bencher.bench(|| {
        get_bip32_keys_from_mnemonic(
            black_box(DEFAULT_DEPLOYER_MNEMONIC),
            black_box(OFF_TABLE_PATH),
        )
        .unwrap()
    });
}

mod stages {
    use super::*;

    #[divan::bench]
    fn to_seed(bencher: divan::Bencher) {
        let mnemonic = Mnemonic::parse(DEFAULT_DEPLOYER_MNEMONIC).unwrap();
        bencher.bench(|| black_box(&mnemonic).to_seed(""));
    }

    #[divan::bench]
    fn derive_path(bencher: divan::Bencher) {
        let seed = Mnemonic::parse(DEFAULT_DEPLOYER_MNEMONIC)
            .unwrap()
            .to_seed("");
        let path = DerivationPath::from_str(DEFAULT_DERIVATION_PATH).unwrap();
        bencher.bench(|| XPrv::derive_from_path(black_box(&seed), black_box(&path)).unwrap());
    }

    /// Parsing the derivation path string itself — should be noise.
    #[divan::bench]
    fn derivation_path_parse() -> DerivationPath {
        DerivationPath::from_str(black_box(DEFAULT_DERIVATION_PATH)).unwrap()
    }
}
