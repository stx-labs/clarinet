mod precomputed;

use std::collections::HashMap;
use std::str::FromStr;
use std::sync::{LazyLock, Mutex, MutexGuard, PoisonError};

use aes_gcm::aead::Aead;
use aes_gcm::{Aes256Gcm, Error as AesGcmError, KeyInit, Nonce};
use argon2::{Argon2, Error as Argon2Error};
use bip32::{DerivationPath, XPrv};
use bip39::{Error as MnemonicError, Mnemonic};
pub use precomputed::{
    DEFAULT_DEPLOYER_MNEMONIC, DEFAULT_DERIVATION_PATH, DEFAULT_DEVNET_ACCOUNTS,
    DEFAULT_FAUCET_MNEMONIC, DEFAULT_STACKER_MNEMONIC, DEFAULT_STACKS_MINER_MNEMONIC,
    DEFAULT_WALLET_1_MNEMONIC, DEFAULT_WALLET_2_MNEMONIC, DEFAULT_WALLET_3_MNEMONIC,
    DEFAULT_WALLET_4_MNEMONIC, DEFAULT_WALLET_5_MNEMONIC, DEFAULT_WALLET_6_MNEMONIC,
    DEFAULT_WALLET_7_MNEMONIC, DEFAULT_WALLET_8_MNEMONIC,
};
use strum::{Display, EnumString, VariantArray};

const AES_GCM_NONCE_SIZE: usize = 12;

/// Size of the random salt stored ahead of the nonce, for every strength but
/// `Basic`.
const SALT_SIZE: usize = 32;

const DEFAULT_SALT: &[u8] = b"clarinet_utils-derive_key_salt";

/// Declared weakest to strongest: [`Self::strongest_first`] relies on it.
#[derive(Debug, Clone, Copy, PartialEq, Default, Display, EnumString, VariantArray)]
#[strum(
    serialize_all = "lowercase",
    ascii_case_insensitive,
    parse_err_ty = String,
    parse_err_fn = unknown_strength
)]
pub enum MnemonicEncryptionStrength {
    Basic,
    #[default]
    Medium,
    High,
    Extreme,
}

fn unknown_strength(s: &str) -> String {
    format!("unknown encryption strength: {s}")
}

impl MnemonicEncryptionStrength {
    pub fn strongest_first() -> impl Iterator<Item = Self> {
        Self::VARIANTS.iter().rev().copied()
    }

    /// The account key in `settings/*.toml` that holds a mnemonic encrypted at
    /// this strength.
    pub const fn manifest_key(self) -> &'static str {
        match self {
            Self::Basic => "encrypted_mnemonic",
            Self::Medium => "encrypted_mnemonic_medium",
            Self::High => "encrypted_mnemonic_high",
            Self::Extreme => "encrypted_mnemonic_extreme",
        }
    }
}

fn parse_mnemonic(phrase: &str) -> Result<Mnemonic, String> {
    Mnemonic::parse(phrase).map_err(|e| e.to_string())
}

/// Validates a phrase and returns it in BIP39's canonical spelling.
pub fn mnemonic_from_phrase(phrase: &str) -> Result<String, String> {
    parse_mnemonic(phrase).map(|mnemonic| mnemonic.to_string())
}

pub fn random_mnemonic() -> String {
    // 16 bytes = 128 bits = 12 words
    Mnemonic::from_entropy(&rand::random::<[u8; 16]>())
        .unwrap()
        .to_string()
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DerivedKeys {
    pub secret_key: [u8; 32],
    /// Compressed SEC1
    pub public_key: [u8; 33],
}

/// `(phrase, derivation)`
type CacheKey = (String, String);

/// Most callers derive the same handful of wallets over and over: the LSP
/// rebuilds the whole `NetworkManifest` on every file save, and
/// `clarinet deployments apply` re-derives the signing key for every
/// transaction in the plan. The table in [`precomputed`] only covers the
/// mnemonics Clarinet ships; this covers everything else, including custom
/// wallets in a project's `Devnet.toml`.
///
/// One path does not benefit and must not be allowed to grow the map without
/// bound: an `[accounts.x]` table with no `mnemonic` gets a fresh
/// `random_mnemonic()` on every manifest load, so it can never be hit again.
/// [`CACHE_LIMIT`] keeps that from accumulating for the life of an LSP session.
///
/// This does keep derived secrets alive for the lifetime of the process. That
/// is already true of the plaintext mnemonics held in `NetworkManifest`, which
/// outlive every caller here.
static DERIVED_KEYS: LazyLock<Mutex<HashMap<CacheKey, DerivedKeys>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Bounds the map without being reachable by a real manifest. Sized well
/// above any plausible account count on purpose: if the cap were close to it,
/// a project declaring more wallets than the cap would evict and re-derive the
/// overflow on *every* load, permanently — trading a bounded ~300 KB for
/// hundreds of milliseconds. At this size an entry (~300 B) is only ever
/// evicted by the unrepeatable random phrases the cap exists for.
const CACHE_LIMIT: usize = 1024;

/// Insert, evicting an arbitrary entry first if the map is full.
///
/// Evicting rather than declining the insert matters: the phrases that fill
/// the map are the unrepeatable random ones, so refusing new entries would let
/// them permanently crowd out wallets that *are* looked up again. Which entry
/// goes is arbitrary, which is only tolerable because [`CACHE_LIMIT`] is far
/// above any real account count — a useful entry is picked with probability
/// `real_wallets / CACHE_LIMIT`, and evicting one costs a single re-derivation.
///
/// This is the sole insertion point and it evicts first, so the map is never
/// above the cap on entry and at most one entry is ever removed.
fn memoize(cache_key: CacheKey, keys: DerivedKeys) {
    let mut cache = derived_keys();
    if cache.len() >= CACHE_LIMIT {
        if let Some(evict) = cache.keys().next().cloned() {
            cache.remove(&evict);
        }
    }
    cache.insert(cache_key, keys);
}

/// A poisoned lock only means another thread panicked mid-lookup. This is a
/// cache, so recover the map rather than propagating the failure.
fn derived_keys() -> MutexGuard<'static, HashMap<CacheKey, DerivedKeys>> {
    DERIVED_KEYS.lock().unwrap_or_else(PoisonError::into_inner)
}

pub fn get_bip32_keys_from_mnemonic(phrase: &str, derivation: &str) -> Result<DerivedKeys, String> {
    // Checked first, so a table hit allocates nothing.
    if let Some(keys) = precomputed::lookup(phrase, derivation) {
        return Ok(keys);
    }

    let cache_key = (phrase.to_string(), derivation.to_string());
    if let Some(keys) = derived_keys().get(&cache_key) {
        return Ok(*keys);
    }

    // Deliberately not holding the guard across the derivation — it is ~0.8 ms,
    // and derivations of *different* wallets should not serialize behind each
    // other. Two threads racing on the same new wallet will therefore both
    // derive it and the second insert wins; that costs one extra derivation,
    // once, which is cheaper than the per-key coordination to avoid it.
    let keys = derive_bip32_keys(phrase, derivation)?;
    memoize(cache_key, keys);

    Ok(keys)
}

/// The derivation itself, with no caching. ~0.8 ms native / ~1.4 ms in the
/// wasm build, dominated by the 2048 PBKDF2-HMAC-SHA512 rounds of `to_seed`.
fn derive_bip32_keys(phrase: &str, derivation: &str) -> Result<DerivedKeys, String> {
    let seed = parse_mnemonic(phrase)?.to_seed("");
    let derivation_path = DerivationPath::from_str(derivation).map_err(|e| e.to_string())?;
    let xprv = XPrv::derive_from_path(&seed, &derivation_path).map_err(|e| e.to_string())?;
    Ok(DerivedKeys {
        secret_key: xprv.to_bytes(),
        public_key: xprv.public_key().to_bytes(),
    })
}

#[derive(Debug)]
pub enum MnemonicEncryptionError {
    AesGcm(AesGcmError),
    Argon2(Argon2Error),
    Bs58Decode(bs58::decode::Error),
    Mnemonic(MnemonicError),
    Utf8(std::str::Utf8Error),
    /// The ciphertext did not survive a base58 round trip
    DecodingMismatch,
    /// The buffer ends before the salt, or holds no ciphertext
    MissingData,
    /// The buffer ends before the AES nonce
    MissingNonce,
}

impl From<AesGcmError> for MnemonicEncryptionError {
    fn from(e: AesGcmError) -> Self {
        Self::AesGcm(e)
    }
}

impl From<Argon2Error> for MnemonicEncryptionError {
    fn from(e: Argon2Error) -> Self {
        Self::Argon2(e)
    }
}

impl From<bs58::decode::Error> for MnemonicEncryptionError {
    fn from(e: bs58::decode::Error) -> Self {
        Self::Bs58Decode(e)
    }
}

impl From<MnemonicError> for MnemonicEncryptionError {
    fn from(e: MnemonicError) -> Self {
        Self::Mnemonic(e)
    }
}

impl From<std::str::Utf8Error> for MnemonicEncryptionError {
    fn from(e: std::str::Utf8Error) -> Self {
        Self::Utf8(e)
    }
}

fn derive_key(
    password: &str,
    strength: MnemonicEncryptionStrength,
    salt: &[u8],
) -> Result<[u8; 32], MnemonicEncryptionError> {
    let (m_cost, t_cost, p_cost) = match strength {
        MnemonicEncryptionStrength::Basic => (19456, 2, 1),
        MnemonicEncryptionStrength::Medium => (262144, 6, 2),
        MnemonicEncryptionStrength::High => (1048576, 10, 2),
        MnemonicEncryptionStrength::Extreme => (2097152, 14, 4),
    };
    let params = argon2::Params::new(m_cost, t_cost, p_cost, Some(32))?;

    let mut key = [0u8; 32];
    Argon2::new(argon2::Algorithm::Argon2id, argon2::Version::V0x13, params).hash_password_into(
        password.as_bytes(),
        salt,
        &mut key,
    )?;

    Ok(key)
}

/// Returns `salt || nonce || ciphertext`, without the salt for `Basic`.
fn encrypt(
    data: &[u8],
    password: &str,
    strength: MnemonicEncryptionStrength,
) -> Result<Vec<u8>, MnemonicEncryptionError> {
    let mut bytes = Vec::new();

    let key = if strength == MnemonicEncryptionStrength::Basic {
        derive_key(password, strength, DEFAULT_SALT)?
    } else {
        let salt: [u8; SALT_SIZE] = rand::random();
        bytes.extend_from_slice(&salt);
        derive_key(password, strength, &salt)?
    };

    let nonce: [u8; AES_GCM_NONCE_SIZE] = rand::random();
    let ciphertext = Aes256Gcm::new((&key).into()).encrypt(&Nonce::from(nonce), data)?;

    bytes.extend_from_slice(&nonce);
    bytes.extend_from_slice(&ciphertext);

    Ok(bytes)
}

fn decrypt(
    data: &[u8],
    password: &str,
    strength: MnemonicEncryptionStrength,
) -> Result<Vec<u8>, MnemonicEncryptionError> {
    let (key, rest) = if strength == MnemonicEncryptionStrength::Basic {
        (derive_key(password, strength, DEFAULT_SALT)?, data)
    } else {
        let (salt, rest) = data
            .split_first_chunk::<SALT_SIZE>()
            .ok_or(MnemonicEncryptionError::MissingData)?;
        (derive_key(password, strength, salt)?, rest)
    };

    let (nonce, ciphertext) = rest
        .split_first_chunk::<AES_GCM_NONCE_SIZE>()
        .ok_or(MnemonicEncryptionError::MissingNonce)?;
    if ciphertext.is_empty() {
        return Err(MnemonicEncryptionError::MissingData);
    }

    Ok(Aes256Gcm::new((&key).into()).decrypt(&Nonce::from(*nonce), ciphertext)?)
}

pub fn encrypt_mnemonic_phrase(
    phrase: &str,
    password: &str,
    strength: MnemonicEncryptionStrength,
) -> Result<String, MnemonicEncryptionError> {
    Mnemonic::parse(phrase)?;
    let ciphertext = encrypt(phrase.as_bytes(), password, strength)?;
    let encrypted_mnemonic = bs58::encode(&ciphertext).into_string();
    let decoded_ciphertext = bs58::decode(&encrypted_mnemonic).into_vec()?;

    if ciphertext != decoded_ciphertext {
        return Err(MnemonicEncryptionError::DecodingMismatch);
    }

    Ok(encrypted_mnemonic)
}

pub fn decrypt_mnemonic_phrase(
    encrypted_mnemonic: &str,
    password: &str,
    strength: MnemonicEncryptionStrength,
) -> Result<String, MnemonicEncryptionError> {
    let cipher = bs58::decode(encrypted_mnemonic).into_vec()?;
    let plain = decrypt(&cipher, password, strength)?;
    let phrase = str::from_utf8(&plain)?;

    Ok(Mnemonic::parse(phrase)?.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::precomputed::is_precomputed;

    pub(crate) fn to_hex(bytes: &[u8]) -> String {
        bytes.iter().map(|b| format!("{b:02x}")).collect()
    }

    #[test]
    fn test_encryption_strength_parses_case_insensitively() {
        for &strength in MnemonicEncryptionStrength::VARIANTS {
            let name = strength.to_string();
            assert_eq!(name, name.to_lowercase());
            assert_eq!(name.parse(), Ok(strength));
            assert_eq!(name.to_uppercase().parse(), Ok(strength));
        }
        assert_eq!(
            "strongest".parse::<MnemonicEncryptionStrength>(),
            Err("unknown encryption strength: strongest".to_string())
        );
    }

    #[test]
    fn test_strongest_first_order() {
        use MnemonicEncryptionStrength::{Basic, Extreme, High, Medium};
        assert_eq!(
            MnemonicEncryptionStrength::strongest_first().collect::<Vec<_>>(),
            [Extreme, High, Medium, Basic]
        );
    }

    #[test]
    fn test_mnemonic_from_phrase_12() {
        let phrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
        assert_eq!(mnemonic_from_phrase(phrase).as_deref(), Ok(phrase));
    }

    #[test]
    fn test_mnemonic_from_phrase_24() {
        let phrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art";
        assert_eq!(mnemonic_from_phrase(phrase).as_deref(), Ok(phrase));
    }

    #[test]
    fn test_random_mnemonic_12() {
        assert_eq!(random_mnemonic().split_whitespace().count(), 12);
    }

    #[test]
    fn test_get_bip32_keys_from_mnemonic_12() {
        let phrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
        let derivation = "m/44'/0'/0'/0/0";
        let keys = get_bip32_keys_from_mnemonic(phrase, derivation).unwrap();
        assert_eq!(
            to_hex(&keys.secret_key),
            "e284129cc0922579a535bbf4d1a3b25773090d28c909bc0fed73b5e0222cc372"
        );
        assert_eq!(
            to_hex(&keys.public_key),
            "03aaeb52dd7494c361049de67cc680e83ebcbbbdbeb13637d92cd845f70308af5e"
        );
    }

    #[test]
    fn test_get_bip32_keys_from_mnemonic_24() {
        let phrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art";
        let derivation = "m/44'/0'/0'/0/0";
        let keys = get_bip32_keys_from_mnemonic(phrase, derivation).unwrap();
        assert_eq!(
            to_hex(&keys.secret_key),
            "cb4793fa23e98437e2d37655eaaa59cf03a0637ea890f85a85d91946e9214c64"
        );
        assert_eq!(
            to_hex(&keys.public_key),
            "0342d943b8dba93a4ce29b858479c67f1e4f1110eecbe1f83dc01b455eb8b123b3"
        );
    }

    /// A precomputed phrase must come back byte-identical to a live derivation.
    #[test]
    fn test_get_bip32_keys_uses_precomputed_table() {
        const PHRASE: &str = DEFAULT_DEPLOYER_MNEMONIC;
        const DERIVATION: &str = DEFAULT_DERIVATION_PATH;
        assert!(is_precomputed(PHRASE, DERIVATION));

        assert_eq!(
            get_bip32_keys_from_mnemonic(PHRASE, DERIVATION).unwrap(),
            derive_bip32_keys(PHRASE, DERIVATION).unwrap()
        );
    }

    /// A mnemonic outside the table must be memoized, and the memoized value
    /// must match a fresh derivation.
    #[test]
    fn test_get_bip32_keys_memoizes_unknown_mnemonics() {
        const PHRASE: &str = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
        const DERIVATION: &str = "m/44'/5757'/0'/0/3";
        assert!(!is_precomputed(PHRASE, DERIVATION));

        let first = get_bip32_keys_from_mnemonic(PHRASE, DERIVATION).unwrap();
        let second = get_bip32_keys_from_mnemonic(PHRASE, DERIVATION).unwrap();
        let fresh = derive_bip32_keys(PHRASE, DERIVATION).unwrap();

        assert_eq!(first, second);
        assert_eq!(first, fresh);
    }

    /// A full memo must keep working. The phrases that fill it are the
    /// unrepeatable random ones, so refusing new entries once full would let
    /// them crowd out wallets that are actually looked up again.
    ///
    /// Exercises `memoize` directly rather than deriving `CACHE_LIMIT` keys:
    /// filling the map through the public entry point would cost over a
    /// thousand PBKDF2 derivations, and eviction is a property of `memoize`.
    #[test]
    fn test_memo_still_caches_once_full() {
        const DERIVATION: &str = "m/44'/5757'/0'/0/7";
        let keys = derive_bip32_keys(DEFAULT_DEPLOYER_MNEMONIC, DERIVATION).unwrap();

        for i in 0..CACHE_LIMIT + 4 {
            memoize((format!("filler phrase {i}"), DERIVATION.to_string()), keys);
        }
        assert!(
            derived_keys().len() <= CACHE_LIMIT,
            "memo grew past its cap"
        );

        // A wallet first seen now must still land in the memo.
        let key = (
            "a phrase seen after the cap".to_string(),
            DERIVATION.to_string(),
        );
        memoize(key.clone(), keys);
        assert_eq!(
            derived_keys().get(&key),
            Some(&keys),
            "a wallet seen after the memo filled up was not cached"
        );
    }

    /// Neither cache may swallow the errors the callers rely on.
    #[test]
    fn test_get_bip32_keys_still_rejects_invalid_input() {
        const PHRASE: &str = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
        assert!(get_bip32_keys_from_mnemonic("not a mnemonic", DEFAULT_DERIVATION_PATH).is_err());
        assert!(get_bip32_keys_from_mnemonic(PHRASE, "not a path").is_err());
    }

    #[test]
    fn test_encrypt() {
        let password = "foo";
        let data = vec![42u8; 128];
        let strength = MnemonicEncryptionStrength::Basic;
        let encrypted = encrypt(&data, password, strength).expect("encrypt should have succeeded");
        let decrypted =
            decrypt(&encrypted, password, strength).expect("decrypt should have succeeded");

        assert_eq!(data, decrypted);

        // remove some bytes from encrypted and make sure it fails
        let mut buf = encrypted.clone();
        buf.truncate(AES_GCM_NONCE_SIZE - 1);
        assert!(matches!(
            decrypt(&buf, password, strength),
            Err(MnemonicEncryptionError::MissingNonce)
        ));

        let mut buf = encrypted.clone();
        buf.truncate(AES_GCM_NONCE_SIZE);
        assert!(matches!(
            decrypt(&buf, password, strength),
            Err(MnemonicEncryptionError::MissingData)
        ));

        let mut buf = encrypted;
        buf.truncate(AES_GCM_NONCE_SIZE + 1);
        assert!(matches!(
            decrypt(&buf, password, strength),
            Err(MnemonicEncryptionError::AesGcm(_))
        ));
    }

    #[test]
    fn test_encrypt_mnemonic() {
        const TEST_PHRASE: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";
        const TEST_PASSWORD: &str = "foo";
        for strength in [
            MnemonicEncryptionStrength::Basic,
            MnemonicEncryptionStrength::Medium,
        ] {
            let encrypted = encrypt_mnemonic_phrase(TEST_PHRASE, TEST_PASSWORD, strength)
                .expect("encrypt_mnemonic_phrase should succeed");
            let decrypted = decrypt_mnemonic_phrase(&encrypted, TEST_PASSWORD, strength)
                .expect("decrypt_mnemonic_phrase should succeed");

            assert_eq!(TEST_PHRASE, decrypted);

            for other in [
                MnemonicEncryptionStrength::Basic,
                MnemonicEncryptionStrength::Medium,
            ] {
                if other != strength {
                    assert!(decrypt_mnemonic_phrase(&encrypted, TEST_PASSWORD, other).is_err());
                }
            }

            let mut bad_bs58 = encrypted;
            bad_bs58.push('?');
            assert!(matches!(
                decrypt_mnemonic_phrase(&bad_bs58, TEST_PASSWORD, strength),
                Err(MnemonicEncryptionError::Bs58Decode(_))
            ));
        }
    }

    #[test]
    fn test_encrypt_mnemonic_invalid_phrase() {
        let bad_phrase = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout clawz";
        const TEST_PASSWORD: &str = "foo";
        assert!(matches!(
            encrypt_mnemonic_phrase(bad_phrase, TEST_PASSWORD, MnemonicEncryptionStrength::Basic),
            Err(MnemonicEncryptionError::Mnemonic(_))
        ));
    }
}
