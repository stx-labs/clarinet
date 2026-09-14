//! Compile-time table of derived keys for the mnemonics Clarinet ships.
//!
//! Deriving a wallet costs ~0.8 ms native / ~1.4 ms in the wasm build, almost
//! all of it the 2048 PBKDF2-HMAC-SHA512 rounds of `Mnemonic::to_seed`. A
//! default project has 10 accounts in `settings/Devnet.toml` and devnet adds
//! a miner and a stacker, so every session — and, in the LSP, every file save
//! — spends ~10 ms recomputing the same answer.
//!
//! These phrases are already public: `clarinet new` writes them into
//! `settings/Devnet.toml` along with the resulting secret keys, so baking the
//! derived keys in exposes nothing new.
//!
//! The phrases below are the single source of truth: `clarinet-files`
//! re-exports the devnet ones and `clarinet-cli` interpolates the account ones
//! into the `clarinet new` template, so a table entry cannot drift away from
//! the mnemonic it describes. What tests still have to guard is the *hex* —
//! see [`tests::entries_match_live_derivation`].
//!
//! To add an entry, put the phrase in with placeholder keys and run the
//! clarinet-utils tests; the failure prints what the values should be.

use libsecp256k1::PublicKey;

use crate::DerivedKeys;

/// Every mnemonic Clarinet ships is used at this path. A custom `derivation`
/// in a manifest misses the table and is derived normally.
///
/// Also re-exported by `clarinet-files` for consumers that already depend on
/// it; both paths resolve to this constant.
pub const DEFAULT_DERIVATION_PATH: &str = "m/44'/5757'/0'/0/0";

/// The accounts `clarinet new` writes into `settings/Devnet.toml`.
/// `clarinet-cli` interpolates these into the generated manifest.
pub const DEFAULT_DEPLOYER_MNEMONIC: &str = "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw";
pub const DEFAULT_WALLET_1_MNEMONIC: &str = "sell invite acquire kitten bamboo drastic jelly vivid peace spawn twice guilt pave pen trash pretty park cube fragile unaware remain midnight betray rebuild";
pub const DEFAULT_WALLET_2_MNEMONIC: &str = "hold excess usual excess ring elephant install account glad dry fragile donkey gaze humble truck breeze nation gasp vacuum limb head keep delay hospital";
pub const DEFAULT_WALLET_3_MNEMONIC: &str = "cycle puppy glare enroll cost improve round trend wrist mushroom scorpion tower claim oppose clever elephant dinosaur eight problem before frozen dune wagon high";
pub const DEFAULT_WALLET_4_MNEMONIC: &str = "board list obtain sugar hour worth raven scout denial thunder horse logic fury scorpion fold genuine phrase wealth news aim below celery when cabin";
pub const DEFAULT_WALLET_5_MNEMONIC: &str = "hurry aunt blame peanut heavy update captain human rice crime juice adult scale device promote vast project quiz unit note reform update climb purchase";
pub const DEFAULT_WALLET_6_MNEMONIC: &str = "area desk dutch sign gold cricket dawn toward giggle vibrant indoor bench warfare wagon number tiny universe sand talk dilemma pottery bone trap buddy";
pub const DEFAULT_WALLET_7_MNEMONIC: &str = "prevent gallery kind limb income control noise together echo rival record wedding sense uncover school version force bleak nuclear include danger skirt enact arrow";
pub const DEFAULT_WALLET_8_MNEMONIC: &str = "female adjust gallery certain visit token during great side clown fitness like hurt clip knife warm bench start reunion globe detail dream depend fortune";

/// Doubles as the `faucet` account of a generated project and as the devnet
/// faucet the chains coordinator spends from.
pub const DEFAULT_FAUCET_MNEMONIC: &str = "shadow private easily thought say logic fault paddle word top book during ignore notable orange flight clock image wealth health outside kitten belt reform";

/// Devnet infrastructure wallets. These never appear in a generated
/// `settings/Devnet.toml`; `clarinet-files` supplies them as defaults.
pub const DEFAULT_STACKS_MINER_MNEMONIC: &str = "fragile loan twenty basic net assault jazz absorb diet talk art shock innocent float punch travel gadget embrace caught blossom hockey surround initial reduce";
pub const DEFAULT_STACKER_MNEMONIC: &str = "empty lens any direct brother then drop fury rule pole win claim scissors list rescue horn rent inform relief jump sword weekend half legend";

/// The accounts a generated `settings/Devnet.toml` declares, in order.
///
/// `clarinet-cli` interpolates the phrases into its template by name — inline
/// format args need a plain identifier — and asserts against this list that it
/// emitted exactly these accounts. Going through the list rather than a local
/// copy is what makes an account added with an off-table phrase fail.
pub const DEFAULT_DEVNET_ACCOUNTS: [(&str, &str); 10] = [
    ("deployer", DEFAULT_DEPLOYER_MNEMONIC),
    ("wallet_1", DEFAULT_WALLET_1_MNEMONIC),
    ("wallet_2", DEFAULT_WALLET_2_MNEMONIC),
    ("wallet_3", DEFAULT_WALLET_3_MNEMONIC),
    ("wallet_4", DEFAULT_WALLET_4_MNEMONIC),
    ("wallet_5", DEFAULT_WALLET_5_MNEMONIC),
    ("wallet_6", DEFAULT_WALLET_6_MNEMONIC),
    ("wallet_7", DEFAULT_WALLET_7_MNEMONIC),
    ("wallet_8", DEFAULT_WALLET_8_MNEMONIC),
    ("faucet", DEFAULT_FAUCET_MNEMONIC),
];

/// Every phrase the workspace ships: the generated accounts plus the two
/// devnet infrastructure wallets, which never appear in a manifest.
#[cfg(test)]
fn all_shipped_mnemonics() -> impl Iterator<Item = &'static str> {
    DEFAULT_DEVNET_ACCOUNTS
        .iter()
        .map(|(_, mnemonic)| *mnemonic)
        .chain([DEFAULT_STACKS_MINER_MNEMONIC, DEFAULT_STACKER_MNEMONIC])
}

struct Entry {
    phrase: &'static str,
    secret_key: [u8; 32],
    /// Uncompressed SEC1. `PublicKey::parse` only has to check that the point
    /// is on the curve, where `parse_compressed` would have to recover `y`
    /// with a modular square root.
    public_key: [u8; 65],
}

const fn hex_nibble(c: u8) -> u8 {
    match c {
        b'0'..=b'9' => c - b'0',
        b'a'..=b'f' => c - b'a' + 10,
        _ => panic!("hex literal must be lowercase [0-9a-f]"),
    }
}

/// Decodes a hex literal at compile time, so the table stays readable (and
/// diffable against the `# secret_key:` comments in generated manifests)
/// without paying for decoding at runtime.
const fn hex<const N: usize>(s: &str) -> [u8; N] {
    let bytes = s.as_bytes();
    assert!(bytes.len() == 2 * N, "hex literal has the wrong length");
    let mut out = [0u8; N];
    let mut i = 0;
    while i < N {
        out[i] = (hex_nibble(bytes[2 * i]) << 4) | hex_nibble(bytes[2 * i + 1]);
        i += 1;
    }
    out
}

static ENTRIES: &[Entry] = &[
    Entry {
        phrase: DEFAULT_DEPLOYER_MNEMONIC,
        secret_key: hex("753b7cc01a1a2e86221266a154af739463fce51219d97e4f856cd7200c3bd2a6"),
        public_key: hex("0490a5cac7c33fda49f70bc1b0866fa0ba7a9440d9de647fecb8132ceb76a94dfa80c44836f7a07cf96a13d3c2c7833a833ff59a3533b8369daa4949da39b09901"),
    },
    Entry {
        phrase: DEFAULT_WALLET_1_MNEMONIC,
        secret_key: hex("7287ba251d44a4d3fd9276c88ce34c5c52a038955511cccaf77e61068649c178"),
        public_key: hex("04cd2cfdbd2ad9332828a7a13ef62cb999e063421c708e863a7ffed71fb61c88c94df6c2a9ebb183ffe28dcb6d43f36e926041fa56df1237ffa648cc0d22ea3737"),
    },
    Entry {
        phrase: DEFAULT_WALLET_2_MNEMONIC,
        secret_key: hex("530d9f61984c888536871c6573073bdfc0058896dc1adfe9a6a10dfacadc2091"),
        public_key: hex("041843d01fa0bb9a3495fd2caf92505a81055dbe1fd545880fd40c3a1c7fd9c40a89f441cf6ce7ed7e234e67d97613226993415af5aa0dd42f55807e7b128df9d8"),
    },
    Entry {
        phrase: DEFAULT_WALLET_3_MNEMONIC,
        secret_key: hex("d655b2523bcd65e34889725c73064feb17ceb796831c0e111ba1a552b0f31b39"),
        public_key: hex("04c4b5eacb71a27be633ed970dcbc41c00440364bc04ba38ae4683ac24e708bf334891b9ad5c5e9ee021e796102e40f7345e1b402ff69fb4f52e221b1dabad1968"),
    },
    Entry {
        phrase: DEFAULT_WALLET_4_MNEMONIC,
        secret_key: hex("f9d7206a47f14d2870c163ebab4bf3e70d18f5d14ce1031f3902fbbc894fe4c7"),
        public_key: hex("04b3e0a76b292b2c83fc0ac14ae6160d0438ebe94e14bbb5b7755153628886e08ea5b60ea54a7fa77b069f5bc87f281b92ff2583d5e865312043b995115ef64727"),
    },
    Entry {
        phrase: DEFAULT_WALLET_5_MNEMONIC,
        secret_key: hex("3eccc5dac8056590432db6a35d52b9896876a3d5cbdea53b72400bc9c2099fe8"),
        public_key: hex("049fa3c9b2c98acc46563707702ee633141b2c125b85649e11548a30233f97ed9f9a3153eb9bb1eda3a9931ea12216cf3c94ba54ffa426edbcd52e057f036f97dd"),
    },
    Entry {
        phrase: DEFAULT_WALLET_6_MNEMONIC,
        secret_key: hex("7036b29cb5e235e5fd9b09ae3e8eec4404e44906814d5d01cbca968a60ed4bfb"),
        public_key: hex("048efa20fa5706567008ebaf48f7ae891342eeb944d96392f719c505c89f84ed8d36c32d5d9f728bbae80a35c7acb50a7b038d0e5d45c2e4669c15469db53eca08"),
    },
    Entry {
        phrase: DEFAULT_WALLET_7_MNEMONIC,
        secret_key: hex("b463f0df6c05d2f156393eee73f8016c5372caa0e9e29a901bb7171d90dc4f14"),
        public_key: hex("043f19d77c842b675bd8c858e9ac8b0ca2efa566f17accf8ef9ceb5a992dc67836b95faf7956d718512b37bf67c0a6ee107206a2c980900142ce8b016110f90ffc"),
    },
    Entry {
        phrase: DEFAULT_WALLET_8_MNEMONIC,
        secret_key: hex("6a1a754ba863d7bab14adbbc3f8ebb090af9e871ace621d3e5ab634e1422885e"),
        public_key: hex("049fb154a570a1645af3dd43c3c668a979b59d21a46dd717fd799b13be3b2a0dc79bd248ed760f07f0aa0719b4601950c95e4e1a35ddd855a398b12d3ae31dbd34"),
    },
    Entry {
        phrase: DEFAULT_FAUCET_MNEMONIC,
        secret_key: hex("de433bdfa14ec43aa1098d5be594c8ffb20a31485ff9de2923b2689471c401b8"),
        public_key: hex("04add319140c528a8955d76d4afe32c4d3143fea57ea353a31ce793cffb77ef8615cc09053489331377e2c637385f00cd5a1cafa54e4a083908d08dfae4f088d72"),
    },
    Entry {
        phrase: DEFAULT_STACKS_MINER_MNEMONIC,
        secret_key: hex("3b68e410cc7f9b8bae76f2f2991b69ecd0627c95da22a904065dfb2a73d0585f"),
        public_key: hex("0439810ebf35e6f6c26062c99f3e183708d377720617c90a986859ec9c95d00be9e16af4fe2d857755f0dd691cecb3489a5c6e04889f0015ed0d39e8262fe36608"),
    },
    Entry {
        phrase: DEFAULT_STACKER_MNEMONIC,
        secret_key: hex("e93b8341c35983ae5b8f93b9530bd90281fd5e5e00a5094c2b7863ace78eac62"),
        public_key: hex("044b9b194720f870d99c4f629b84626e0cdf1198b10b4dca7c4851771d72d07f8578333fcbdeec9de504890796e32741f397bed30fa4bb7451cf0d4a457cdcb8c7"),
    },
];

fn find(phrase: &str, derivation: &str) -> Option<&'static Entry> {
    if derivation != DEFAULT_DERIVATION_PATH {
        return None;
    }
    ENTRIES.iter().find(|entry| entry.phrase == phrase)
}

/// The derived keys for a shipped mnemonic, or `None` if it isn't one.
pub(crate) fn lookup(phrase: &str, derivation: &str) -> Option<DerivedKeys> {
    let entry = find(phrase, derivation)?;
    let public_key = PublicKey::parse(&entry.public_key)
        .expect("precomputed public key is not a valid secp256k1 point");
    Some((entry.secret_key.to_vec(), public_key))
}

/// Whether this mnemonic and path resolve from the table instead of being
/// derived. Test-only on purpose: table membership exists for speed and may
/// gain or lose entries for speed, so it must not become product behaviour.
#[cfg(test)]
pub(crate) fn is_precomputed(phrase: &str, derivation: &str) -> bool {
    find(phrase, derivation).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{derive_bip32_keys, mnemonic_from_phrase};

    fn to_hex(bytes: &[u8]) -> String {
        bytes.iter().map(|b| format!("{b:02x}")).collect()
    }

    /// The table is only as good as its agreement with the real derivation.
    #[test]
    fn entries_match_live_derivation() {
        for entry in ENTRIES {
            let (secret_key, public_key) =
                derive_bip32_keys(entry.phrase, "", DEFAULT_DERIVATION_PATH)
                    .unwrap_or_else(|e| panic!("failed to derive {}: {e}", entry.phrase));

            assert_eq!(
                to_hex(&secret_key),
                to_hex(&entry.secret_key),
                "stale secret_key for \"{}\"",
                entry.phrase
            );
            assert_eq!(
                to_hex(&public_key.serialize()),
                to_hex(&entry.public_key),
                "stale public_key for \"{}\"",
                entry.phrase
            );
        }
    }

    /// Every phrase the workspace ships must have baked keys, or it silently
    /// costs a PBKDF2 derivation per session. Sharing the constants makes the
    /// phrases themselves impossible to drift; this covers the table.
    #[test]
    fn every_shipped_mnemonic_is_in_the_table() {
        assert_eq!(all_shipped_mnemonics().count(), ENTRIES.len());
        for mnemonic in all_shipped_mnemonics() {
            assert!(
                is_precomputed(mnemonic, DEFAULT_DERIVATION_PATH),
                "shipped mnemonic missing from the table: \"{mnemonic}\""
            );
        }
    }

    /// Callers look the table up with the *parsed* phrase — see
    /// `compute_addresses`, which passes `mnemonic_from_phrase(..).to_string()`.
    /// If a table phrase were not already in BIP39's canonical spelling, every
    /// lookup for it would miss and the entry would be dead weight.
    #[test]
    fn table_phrases_are_canonically_spelled() {
        for entry in ENTRIES {
            let parsed = mnemonic_from_phrase(entry.phrase)
                .unwrap_or_else(|e| panic!("invalid mnemonic in table: {e}"))
                .to_string();
            assert_eq!(parsed, entry.phrase, "table phrase is not canonical");
        }
    }

    #[test]
    fn lookup_is_scoped_to_the_default_derivation_path() {
        let phrase = ENTRIES[0].phrase;
        assert!(is_precomputed(phrase, DEFAULT_DERIVATION_PATH));
        assert!(!is_precomputed(phrase, "m/44'/5757'/0'/0/1"));
        assert!(!is_precomputed("not a mnemonic", DEFAULT_DERIVATION_PATH));
    }
}
