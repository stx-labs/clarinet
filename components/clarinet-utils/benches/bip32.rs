//! Where the time inside `get_bip32_keys_from_mnemonic` actually goes.
//!
//! Three stages, run back to back for every wallet in `settings/Devnet.toml`:
//!   1. `Mnemonic::to_seed` — PBKDF2-HMAC-SHA512, 2048 rounds
//!   2. `XPrv::derive_from_path` — one HMAC-SHA512 + one secp256k1 point
//!      multiplication per level (5 levels for `m/44'/5757'/0'/0/0`)
//!   3. `PublicKey::from_secret_key` — one more point multiplication

use std::hint::black_box;
use std::str::FromStr;

use bip32::{DerivationPath, XPrv};
use bip39::{Language, Mnemonic};
use clarinet_utils::{get_bip32_keys_from_mnemonic, random_mnemonic};
use libsecp256k1::{PublicKey, SecretKey};

const PHRASE: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";
const DERIVATION: &str = "m/44'/5757'/0'/0/0";

fn main() {
    divan::main();
}

/// The three stages together, for a mnemonic no cache has seen.
#[divan::bench]
fn total_uncached(bencher: divan::Bencher) {
    bencher
        .with_inputs(|| random_mnemonic().to_string())
        .bench_values(|phrase| get_bip32_keys_from_mnemonic(&phrase, "", DERIVATION).unwrap());
}

/// The same call for a mnemonic Clarinet ships, which resolves from the
/// compile-time table instead.
#[divan::bench]
fn total_precomputed() -> (Vec<u8>, PublicKey) {
    get_bip32_keys_from_mnemonic(black_box(PHRASE), "", black_box(DERIVATION)).unwrap()
}

#[divan::bench]
fn stage1_to_seed(bencher: divan::Bencher) {
    let mnemonic = Mnemonic::parse_in(Language::English, PHRASE).unwrap();
    bencher.bench(|| black_box(&mnemonic).to_seed(""));
}

#[divan::bench]
fn stage2_derive_path(bencher: divan::Bencher) {
    let mnemonic = Mnemonic::parse_in(Language::English, PHRASE).unwrap();
    let mut seed = [0u8; 64];
    seed.copy_from_slice(&mnemonic.to_seed(""));
    let path = DerivationPath::from_str(DERIVATION).unwrap();
    bencher.bench(|| XPrv::derive_from_path(black_box(seed), black_box(&path)).unwrap());
}

#[divan::bench]
fn stage3_public_key(bencher: divan::Bencher) {
    let mnemonic = Mnemonic::parse_in(Language::English, PHRASE).unwrap();
    let mut seed = [0u8; 64];
    seed.copy_from_slice(&mnemonic.to_seed(""));
    let path = DerivationPath::from_str(DERIVATION).unwrap();
    let xprv = XPrv::derive_from_path(seed, &path).unwrap();
    let secret = SecretKey::parse_slice(&xprv.private_key().to_bytes()).unwrap();
    bencher.bench(|| PublicKey::from_secret_key(black_box(&secret)));
}

/// Parsing the derivation path string itself — should be noise.
#[divan::bench]
fn derivation_path_parse() -> DerivationPath {
    DerivationPath::from_str(black_box(DERIVATION)).unwrap()
}
