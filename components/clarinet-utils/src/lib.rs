mod precomputed;

use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;
use std::sync::{LazyLock, Mutex, MutexGuard, PoisonError};

use aes_gcm::aead::Aead;
use aes_gcm::{Aes256Gcm, Error as AesGcmError, KeyInit, Nonce};
use argon2::{Argon2, Error as Argon2Error};
use bip32::{DerivationPath, XPrv};
use bip39::{Error as MnemonicError, Language, Mnemonic};
use libsecp256k1::{PublicKey, SecretKey};
pub use precomputed::{
    DEFAULT_DEPLOYER_MNEMONIC, DEFAULT_DERIVATION_PATH, DEFAULT_FAUCET_MNEMONIC,
    DEFAULT_STACKER_MNEMONIC, DEFAULT_STACKS_MINER_MNEMONIC, DEFAULT_WALLET_1_MNEMONIC,
    DEFAULT_WALLET_2_MNEMONIC, DEFAULT_WALLET_3_MNEMONIC, DEFAULT_WALLET_4_MNEMONIC,
    DEFAULT_WALLET_5_MNEMONIC, DEFAULT_WALLET_6_MNEMONIC, DEFAULT_WALLET_7_MNEMONIC,
    DEFAULT_WALLET_8_MNEMONIC,
};
use rand::RngCore;

/// Size of the AES-GCM nonce
pub const AES_GCM_NONCE_SIZE: usize = 12;

/// Size of the random salt used for Strong encryption
pub const SALT_SIZE: usize = 32;

const DEFAULT_SALT: &[u8] = b"clarinet_utils-derive_key_salt";

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum MnemonicEncryptionStrength {
    Basic,
    #[default]
    Medium,
    High,
    Extreme,
}

impl FromStr for MnemonicEncryptionStrength {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "basic" => Ok(Self::Basic),
            "medium" => Ok(Self::Medium),
            "high" => Ok(Self::High),
            "extreme" => Ok(Self::Extreme),
            _ => Err(format!("unknown encryption strength: {s}")),
        }
    }
}

impl fmt::Display for MnemonicEncryptionStrength {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Basic => write!(f, "basic"),
            Self::Medium => write!(f, "medium"),
            Self::High => write!(f, "high"),
            Self::Extreme => write!(f, "extreme"),
        }
    }
}

pub fn mnemonic_from_phrase(phrase: &str) -> Result<Mnemonic, String> {
    Mnemonic::parse_in(Language::English, phrase).map_err(|e| e.to_string())
}

pub fn random_mnemonic() -> Mnemonic {
    let mut entropy = [0u8; 16]; // 16 bytes = 128 bits = 12 words
    rand::rng().fill_bytes(&mut entropy);
    Mnemonic::from_entropy_in(Language::English, &entropy).unwrap()
}

pub(crate) type DerivedKeys = (Vec<u8>, PublicKey);

/// `(phrase, password, derivation)`.
type CacheKey = (String, String, String);

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

/// Comfortably above the ~13 wallets a devnet manifest declares, low enough
/// that the randomly generated phrases described above cannot pile up.
const CACHE_LIMIT: usize = 64;

/// Insert, evicting an arbitrary entry first if the map is full.
///
/// Evicting rather than declining the insert matters: the phrases that fill
/// the map are the unrepeatable random ones, so refusing new entries would let
/// them permanently crowd out wallets that *are* looked up again.
fn memoize(cache_key: CacheKey, keys: &DerivedKeys) {
    let mut cache = derived_keys();
    while cache.len() >= CACHE_LIMIT {
        let Some(evict) = cache.keys().next().cloned() else {
            break;
        };
        cache.remove(&evict);
    }
    cache.insert(cache_key, keys.clone());
}

/// A poisoned lock only means another thread panicked mid-lookup. This is a
/// cache, so recover the map rather than propagating the failure.
fn derived_keys() -> MutexGuard<'static, HashMap<CacheKey, DerivedKeys>> {
    DERIVED_KEYS.lock().unwrap_or_else(PoisonError::into_inner)
}

pub fn get_bip32_keys_from_mnemonic(
    phrase: &str,
    password: &str,
    derivation: &str,
) -> Result<DerivedKeys, String> {
    // A BIP39 passphrase changes the seed, so the table only applies without
    // one. Checking it first means a table hit allocates nothing.
    if password.is_empty() {
        if let Some(keys) = precomputed::lookup(phrase, derivation) {
            return Ok(keys);
        }
    }

    let cache_key = (
        phrase.to_string(),
        password.to_string(),
        derivation.to_string(),
    );

    if let Some(keys) = derived_keys().get(&cache_key) {
        return Ok(keys.clone());
    }

    // Deliberately not holding the guard across the derivation — it is ~0.8 ms,
    // and concurrent derivations of different wallets should not serialize.
    let keys = derive_bip32_keys(phrase, password, derivation)?;
    memoize(cache_key, &keys);

    Ok(keys)
}

/// The derivation itself, with no caching. ~0.8 ms native / ~1.4 ms in the
/// wasm build, dominated by the 2048 PBKDF2-HMAC-SHA512 rounds of `to_seed`.
fn derive_bip32_keys(
    phrase: &str,
    password: &str,
    derivation: &str,
) -> Result<DerivedKeys, String> {
    let mnemonic = mnemonic_from_phrase(phrase)?;
    let seed_vec = mnemonic.to_seed(password);
    if seed_vec.len() != 64 {
        return Err("Seed must be 64 bytes".to_string());
    }
    let mut seed = [0u8; 64];
    seed.copy_from_slice(&seed_vec);
    let derivation_path = DerivationPath::from_str(derivation).map_err(|e| e.to_string())?;
    let xprv = XPrv::derive_from_path(seed, &derivation_path).map_err(|e| e.to_string())?;
    let secret_bytes = xprv.private_key().to_bytes();
    let secret_key = SecretKey::parse_slice(&secret_bytes).unwrap();
    let public_key = PublicKey::from_secret_key(&secret_key);
    Ok((secret_bytes.to_vec(), public_key))
}

#[derive(Debug, Clone, PartialEq)]
pub enum EncryptionError {
    /// Wrapped aes_gcm::Error
    AesGcm(AesGcmError),
    /// Wrapped argon2::Error
    Argon2(Argon2Error),
    /// AES data was missing from the buffer
    MissingData,
    /// AES nonce was missing from the buffer
    MissingNonce,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MnemonicEncryptionError {
    /// Wrapped encryption error
    Encryption(EncryptionError),
    /// Wrapped bs58::decode::Error
    Bs58Decode(bs58::decode::Error),
    /// Decoding mismatch
    DecodingMismatch,
    /// Wrapped bip39::Error
    Mnemonic(MnemonicError),
    /// Wrapped std::str::Utf8Error
    Utf8(std::str::Utf8Error),
}

impl From<Argon2Error> for EncryptionError {
    fn from(e: Argon2Error) -> Self {
        Self::Argon2(e)
    }
}

impl From<AesGcmError> for EncryptionError {
    fn from(e: AesGcmError) -> Self {
        Self::AesGcm(e)
    }
}

impl From<EncryptionError> for MnemonicEncryptionError {
    fn from(e: EncryptionError) -> Self {
        Self::Encryption(e)
    }
}

impl From<MnemonicError> for MnemonicEncryptionError {
    fn from(e: MnemonicError) -> Self {
        Self::Mnemonic(e)
    }
}

impl From<bs58::decode::Error> for MnemonicEncryptionError {
    fn from(e: bs58::decode::Error) -> Self {
        Self::Bs58Decode(e)
    }
}

impl From<std::str::Utf8Error> for MnemonicEncryptionError {
    fn from(e: std::str::Utf8Error) -> Self {
        Self::Utf8(e)
    }
}

pub fn derive_key(
    password: &str,
    buf: &mut [u8],
    strength: MnemonicEncryptionStrength,
    salt: &[u8],
) -> Result<(), EncryptionError> {
    let argon2 = match strength {
        MnemonicEncryptionStrength::Basic => {
            let params = argon2::Params::new(19456, 2, 1, Some(32))?;
            Argon2::new(argon2::Algorithm::Argon2id, argon2::Version::V0x13, params)
        }
        MnemonicEncryptionStrength::Medium => {
            let params = argon2::Params::new(262144, 6, 2, Some(32))?;
            Argon2::new(argon2::Algorithm::Argon2id, argon2::Version::V0x13, params)
        }
        MnemonicEncryptionStrength::High => {
            let params = argon2::Params::new(1048576, 10, 2, Some(32))?;
            Argon2::new(argon2::Algorithm::Argon2id, argon2::Version::V0x13, params)
        }
        MnemonicEncryptionStrength::Extreme => {
            let params = argon2::Params::new(2097152, 14, 4, Some(32))?;
            Argon2::new(argon2::Algorithm::Argon2id, argon2::Version::V0x13, params)
        }
    };

    argon2.hash_password_into(password.as_bytes(), salt, buf)?;

    Ok(())
}

pub fn encrypt(
    data: &[u8],
    password: &str,
    strength: MnemonicEncryptionStrength,
) -> Result<Vec<u8>, EncryptionError> {
    let mut key = [0u8; 32];
    let mut rng = rand::rng();
    let mut nonce_bytes = [0u8; AES_GCM_NONCE_SIZE];

    let mut bytes = Vec::new();

    match strength {
        MnemonicEncryptionStrength::Basic => {
            derive_key(password, &mut key, strength, DEFAULT_SALT)?;
        }
        MnemonicEncryptionStrength::Medium
        | MnemonicEncryptionStrength::High
        | MnemonicEncryptionStrength::Extreme => {
            let mut salt = [0u8; SALT_SIZE];
            rng.fill_bytes(&mut salt);
            derive_key(password, &mut key, strength, &salt)?;
            bytes.extend_from_slice(&salt);
        }
    }

    rng.fill_bytes(&mut nonce_bytes);

    let nonce = Nonce::from(nonce_bytes);
    let cipher = Aes256Gcm::new((&key).into());
    let cipher_vec = cipher.encrypt(&nonce, data.to_vec().as_ref())?;

    bytes.extend_from_slice(&nonce_bytes);
    bytes.extend_from_slice(&cipher_vec);

    Ok(bytes)
}

pub fn decrypt(
    data: &[u8],
    password: &str,
    strength: MnemonicEncryptionStrength,
) -> Result<Vec<u8>, EncryptionError> {
    let mut key = [0u8; 32];

    let rest = match strength {
        MnemonicEncryptionStrength::Basic => {
            derive_key(password, &mut key, strength, DEFAULT_SALT)?;
            data
        }
        MnemonicEncryptionStrength::Medium
        | MnemonicEncryptionStrength::High
        | MnemonicEncryptionStrength::Extreme => {
            let Some(salt) = data.get(..SALT_SIZE) else {
                return Err(EncryptionError::MissingData);
            };
            derive_key(password, &mut key, strength, salt)?;
            &data[SALT_SIZE..]
        }
    };

    let Some(nonce_data) = rest.get(..AES_GCM_NONCE_SIZE) else {
        return Err(EncryptionError::MissingNonce);
    };
    let Some(cipher_data) = rest.get(AES_GCM_NONCE_SIZE..) else {
        return Err(EncryptionError::MissingData);
    };
    if cipher_data.is_empty() {
        return Err(EncryptionError::MissingData);
    }
    let nonce_array: [u8; AES_GCM_NONCE_SIZE] = nonce_data
        .try_into()
        .map_err(|_| EncryptionError::MissingNonce)?;
    let nonce = Nonce::from(nonce_array);
    let cipher = Aes256Gcm::new((&key).into());

    Ok(cipher.decrypt(&nonce, cipher_data)?)
}

pub fn encrypt_mnemonic_phrase(
    phrase: &str,
    password: &str,
    strength: MnemonicEncryptionStrength,
) -> Result<String, MnemonicEncryptionError> {
    let _ = Mnemonic::parse_in(Language::English, phrase)?;
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
) -> Result<Mnemonic, MnemonicEncryptionError> {
    let cipher = bs58::decode(encrypted_mnemonic).into_vec()?;
    let plain = decrypt(&cipher, password, strength)?;
    let phrase = str::from_utf8(&plain)?;
    let mnemonic = Mnemonic::parse_in(Language::English, phrase)?;

    Ok(mnemonic)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::precomputed::is_precomputed;

    #[test]
    fn test_mnemonic_from_phrase_12() {
        let phrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
        let mnemonic = mnemonic_from_phrase(phrase);
        assert!(mnemonic.is_ok());
        assert_eq!(mnemonic.unwrap().to_string(), phrase);
    }

    #[test]
    fn test_mnemonic_from_phrase_24() {
        let phrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art";
        let mnemonic = mnemonic_from_phrase(phrase);
        assert!(mnemonic.is_ok());
        assert_eq!(mnemonic.unwrap().to_string(), phrase);
    }

    #[test]
    fn test_random_mnemonic_12() {
        let mnemonic = random_mnemonic();
        let phrase = mnemonic.to_string();
        let words: Vec<&str> = phrase.split_whitespace().collect();
        assert_eq!(words.len(), 12);
    }

    #[test]
    fn test_get_bip32_keys_from_mnemonic_12() {
        let phrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
        let derivation = "m/44'/0'/0'/0/0";
        let result = get_bip32_keys_from_mnemonic(phrase, "", derivation);
        assert!(result.is_ok());
        let (secret, pubkey) = result.unwrap();
        assert_eq!(secret.len(), 32);
        assert_eq!(pubkey.serialize_compressed().len(), 33);
    }

    #[test]
    fn test_get_bip32_keys_from_mnemonic_24() {
        let phrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art";
        let derivation = "m/44'/0'/0'/0/0";
        let result = get_bip32_keys_from_mnemonic(phrase, "", derivation);
        assert!(result.is_ok());
        let (secret, pubkey) = result.unwrap();
        assert_eq!(secret.len(), 32);
        assert_eq!(pubkey.serialize_compressed().len(), 33);
    }

    /// A precomputed phrase must come back byte-identical to a live derivation.
    #[test]
    fn test_get_bip32_keys_uses_precomputed_table() {
        const PHRASE: &str = DEFAULT_DEPLOYER_MNEMONIC;
        const DERIVATION: &str = DEFAULT_DERIVATION_PATH;
        assert!(is_precomputed(PHRASE, DERIVATION));

        let (cached_secret, cached_public) =
            get_bip32_keys_from_mnemonic(PHRASE, "", DERIVATION).unwrap();
        let (derived_secret, derived_public) = derive_bip32_keys(PHRASE, "", DERIVATION).unwrap();

        assert_eq!(cached_secret, derived_secret);
        assert_eq!(cached_public, derived_public);
    }

    /// A BIP39 passphrase produces a different seed, so the table must not
    /// answer for it.
    #[test]
    fn test_get_bip32_keys_bypasses_table_when_password_is_set() {
        const PHRASE: &str = DEFAULT_DEPLOYER_MNEMONIC;
        const DERIVATION: &str = DEFAULT_DERIVATION_PATH;

        let (no_password, _) = get_bip32_keys_from_mnemonic(PHRASE, "", DERIVATION).unwrap();
        let (with_password, _) =
            get_bip32_keys_from_mnemonic(PHRASE, "hunter2", DERIVATION).unwrap();

        assert_ne!(no_password, with_password);
        assert_eq!(
            with_password,
            derive_bip32_keys(PHRASE, "hunter2", DERIVATION).unwrap().0
        );
    }

    /// A mnemonic outside the table must be memoized, and the memoized value
    /// must match a fresh derivation.
    #[test]
    fn test_get_bip32_keys_memoizes_unknown_mnemonics() {
        const PHRASE: &str = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
        const DERIVATION: &str = "m/44'/5757'/0'/0/3";
        assert!(!is_precomputed(PHRASE, DERIVATION));

        let first = get_bip32_keys_from_mnemonic(PHRASE, "", DERIVATION).unwrap();
        let second = get_bip32_keys_from_mnemonic(PHRASE, "", DERIVATION).unwrap();
        let fresh = derive_bip32_keys(PHRASE, "", DERIVATION).unwrap();

        assert_eq!(first, second);
        assert_eq!(first, fresh);
    }

    /// A full memo must keep working. The phrases that fill it are the
    /// unrepeatable random ones, so refusing new entries once full would let
    /// them crowd out wallets that are actually looked up again.
    #[test]
    fn test_memo_still_caches_once_full() {
        const DERIVATION: &str = "m/44'/5757'/0'/0/7";

        // Fill past the limit with phrases nothing will ask for again, the way
        // an `[accounts.x]` table with no `mnemonic` does on every load.
        for _ in 0..CACHE_LIMIT + 4 {
            let throwaway = random_mnemonic().to_string();
            get_bip32_keys_from_mnemonic(&throwaway, "", DERIVATION).unwrap();
        }
        assert!(
            derived_keys().len() <= CACHE_LIMIT,
            "memo grew past its cap"
        );

        // A wallet first seen now must still land in the memo.
        let phrase = random_mnemonic().to_string();
        let first = get_bip32_keys_from_mnemonic(&phrase, "", DERIVATION).unwrap();
        let key = (phrase.clone(), String::new(), DERIVATION.to_string());
        assert_eq!(
            derived_keys().get(&key),
            Some(&first),
            "a wallet seen after the memo filled up was not cached"
        );
    }

    /// Neither cache may swallow the errors the callers rely on.
    #[test]
    fn test_get_bip32_keys_still_rejects_invalid_input() {
        const PHRASE: &str = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
        assert!(
            get_bip32_keys_from_mnemonic("not a mnemonic", "", DEFAULT_DERIVATION_PATH).is_err()
        );
        assert!(get_bip32_keys_from_mnemonic(PHRASE, "", "not a path").is_err());
    }

    #[test]
    fn test_derive_key() {
        let mut short = [0u8; 1];
        let mut right = [0u8; 32];
        let password = "foo";

        let _ = derive_key(
            password,
            &mut short,
            MnemonicEncryptionStrength::Basic,
            DEFAULT_SALT,
        )
        .expect_err("Should have failed with 1-byte output buffer");

        derive_key(
            password,
            &mut right,
            MnemonicEncryptionStrength::Basic,
            DEFAULT_SALT,
        )
        .expect("Should have succeeded with 32-byte output buffer");
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
            Err(EncryptionError::MissingNonce)
        ));

        let mut buf = encrypted.clone();
        buf.truncate(AES_GCM_NONCE_SIZE);
        assert!(matches!(
            decrypt(&buf, password, strength),
            Err(EncryptionError::MissingData)
        ));

        let mut buf = encrypted.clone();
        buf.truncate(AES_GCM_NONCE_SIZE + 1);
        assert!(matches!(
            decrypt(&buf, password, strength),
            Err(EncryptionError::AesGcm(_))
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

            assert_eq!(TEST_PHRASE, decrypted.to_string());

            for other in [
                MnemonicEncryptionStrength::Basic,
                MnemonicEncryptionStrength::Medium,
            ] {
                if other != strength {
                    assert!(decrypt_mnemonic_phrase(&encrypted, TEST_PASSWORD, other).is_err());
                }
            }

            let mut bad_bs58 = encrypted.clone();
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
