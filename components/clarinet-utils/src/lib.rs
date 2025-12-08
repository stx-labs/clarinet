use std::str::FromStr;

use aes_gcm::aead::Aead;
use aes_gcm::{Aes256Gcm, Error as AesGcmError, KeyInit, Nonce};
use argon2::{Argon2, Error as Argon2Error};
use bip32::{DerivationPath, XPrv};
use bip39::{Error as MnemonicError, Language, Mnemonic};
use libsecp256k1::{PublicKey, SecretKey};
use rand::rngs::OsRng;
use rand::RngCore;

/// Size of the AES-GCM nonce
pub const AES_GCM_NONCE_SIZE: usize = 12;

pub fn mnemonic_from_phrase(phrase: &str) -> Result<Mnemonic, String> {
    Mnemonic::parse_in(Language::English, phrase).map_err(|e| e.to_string())
}

pub fn random_mnemonic() -> Mnemonic {
    let mut entropy = [0u8; 16]; // 16 bytes = 128 bits = 12 words
    rand::thread_rng().fill_bytes(&mut entropy);
    Mnemonic::from_entropy_in(Language::English, &entropy).unwrap()
}

pub fn get_bip32_keys_from_mnemonic(
    phrase: &str,
    password: &str,
    derivation: &str,
) -> Result<(Vec<u8>, PublicKey), String> {
    let mnemonic = Mnemonic::parse_in(Language::English, phrase).map_err(|e| e.to_string())?;
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

pub fn derive_key(password: &str, buf: &mut [u8]) -> Result<(), EncryptionError> {
    let salt = b"clarinet_utils-derive_key_salt";

    Argon2::default().hash_password_into(password.as_bytes(), salt, buf)?;

    Ok(())
}

pub fn encrypt(data: &[u8], password: &str) -> Result<Vec<u8>, EncryptionError> {
    let mut key = [0u8; 32];
    let mut rng = OsRng;
    let mut nonce_bytes = [0u8; AES_GCM_NONCE_SIZE];

    derive_key(password, &mut key)?;
    rng.fill_bytes(&mut nonce_bytes);

    let nonce = Nonce::from(nonce_bytes);
    let cipher = Aes256Gcm::new((&key).into());
    let cipher_vec = cipher.encrypt(&nonce, data.to_vec().as_ref())?;
    let mut bytes = Vec::new();

    bytes.extend_from_slice(&nonce_bytes);
    bytes.extend_from_slice(&cipher_vec);

    Ok(bytes)
}

pub fn decrypt(data: &[u8], password: &str) -> Result<Vec<u8>, EncryptionError> {
    let mut key = [0u8; 32];
    derive_key(password, &mut key)?;
    let Some(nonce_data) = data.get(..AES_GCM_NONCE_SIZE) else {
        return Err(EncryptionError::MissingNonce);
    };
    let Some(cipher_data) = data.get(AES_GCM_NONCE_SIZE..) else {
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
) -> Result<String, MnemonicEncryptionError> {
    let _ = Mnemonic::parse_in(Language::English, phrase)?;
    let ciphertext = encrypt(phrase.as_bytes(), password)?;
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
) -> Result<Mnemonic, MnemonicEncryptionError> {
    let cipher = bs58::decode(encrypted_mnemonic).into_vec()?;
    let plain = decrypt(&cipher, password)?;
    let phrase = str::from_utf8(&plain)?;
    let mnemonic = Mnemonic::parse_in(Language::English, phrase)?;

    Ok(mnemonic)
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn test_derive_key() {
        let mut short = [0u8; 1];
        let mut right = [0u8; 32];
        let password = "foo";

        let _ = derive_key(password, &mut short)
            .expect_err("Should have failed with 1-byte output buffer");

        let _ = derive_key(password, &mut right)
            .expect("Should have succeeded with 32-byte output buffer");
    }

    #[test]
    fn test_encrypt() {
        let password = "foo";
        let data = vec![42u8; 128];
        let encrypted = encrypt(&data, password).expect("encrypt should have succeeded");
        let decrypted = decrypt(&encrypted, password).expect("decrypt should have succeeded");

        assert_eq!(data, decrypted);

        // remove some bytes from encrypted and make sure it fails
        let mut buf = encrypted.clone();
        buf.truncate(AES_GCM_NONCE_SIZE - 1);
        assert!(matches!(
            decrypt(&buf, password),
            Err(EncryptionError::MissingNonce)
        ));

        let mut buf = encrypted.clone();
        buf.truncate(AES_GCM_NONCE_SIZE);
        assert!(matches!(
            decrypt(&buf, password),
            Err(EncryptionError::MissingData)
        ));

        let mut buf = encrypted.clone();
        buf.truncate(AES_GCM_NONCE_SIZE + 1);
        assert!(matches!(
            decrypt(&buf, password),
            Err(EncryptionError::AesGcm(_))
        ));
    }

    #[test]
    fn test_encrypt_mnemonic() {
        let phrase = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";
        let password = "foo";

        let encrypted = encrypt_mnemonic_phrase(phrase, password)
            .expect("encrypt_mnemonic_phrase should succeed");
        let decrypted = decrypt_mnemonic_phrase(&encrypted, password)
            .expect("decrypt_mnemonic_phrase should succeed");

        assert_eq!(phrase, decrypted.to_string());

        let bad_phrase = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout clawz";
        assert!(matches!(
            encrypt_mnemonic_phrase(bad_phrase, password),
            Err(MnemonicEncryptionError::Mnemonic(_))
        ));

        let mut bad_bs58 = encrypted.clone();
        bad_bs58.push('?');
        assert!(matches!(
            decrypt_mnemonic_phrase(&bad_bs58, password),
            Err(MnemonicEncryptionError::Bs58Decode(_))
        ));
    }
}
