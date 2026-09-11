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
//! The table is kept honest by tests rather than by discipline:
//!   - [`tests::entries_match_live_derivation`] re-derives every entry
//!   - `clarinet-files` asserts the devnet miner/faucet/stacker defaults hit it
//!   - `clarinet-cli` asserts every account in a generated `Devnet.toml` hits it
//!
//! To add an entry, put the phrase in with placeholder keys and run the
//! clarinet-utils tests; the failure prints what the values should be.

use libsecp256k1::PublicKey;

/// Every mnemonic Clarinet ships is used at the default Stacks derivation
/// path. A custom `derivation` in a manifest misses the table and is derived
/// normally.
const DERIVATION_PATH: &str = "m/44'/5757'/0'/0/0";

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
    // deployer
    Entry {
        phrase: "twice kind fence tip hidden tilt action fragile skin nothing glory cousin green tomorrow spring wrist shed math olympic multiply hip blue scout claw",
        secret_key: hex("753b7cc01a1a2e86221266a154af739463fce51219d97e4f856cd7200c3bd2a6"),
        public_key: hex("0490a5cac7c33fda49f70bc1b0866fa0ba7a9440d9de647fecb8132ceb76a94dfa80c44836f7a07cf96a13d3c2c7833a833ff59a3533b8369daa4949da39b09901"),
    },
    // wallet_1
    Entry {
        phrase: "sell invite acquire kitten bamboo drastic jelly vivid peace spawn twice guilt pave pen trash pretty park cube fragile unaware remain midnight betray rebuild",
        secret_key: hex("7287ba251d44a4d3fd9276c88ce34c5c52a038955511cccaf77e61068649c178"),
        public_key: hex("04cd2cfdbd2ad9332828a7a13ef62cb999e063421c708e863a7ffed71fb61c88c94df6c2a9ebb183ffe28dcb6d43f36e926041fa56df1237ffa648cc0d22ea3737"),
    },
    // wallet_2
    Entry {
        phrase: "hold excess usual excess ring elephant install account glad dry fragile donkey gaze humble truck breeze nation gasp vacuum limb head keep delay hospital",
        secret_key: hex("530d9f61984c888536871c6573073bdfc0058896dc1adfe9a6a10dfacadc2091"),
        public_key: hex("041843d01fa0bb9a3495fd2caf92505a81055dbe1fd545880fd40c3a1c7fd9c40a89f441cf6ce7ed7e234e67d97613226993415af5aa0dd42f55807e7b128df9d8"),
    },
    // wallet_3
    Entry {
        phrase: "cycle puppy glare enroll cost improve round trend wrist mushroom scorpion tower claim oppose clever elephant dinosaur eight problem before frozen dune wagon high",
        secret_key: hex("d655b2523bcd65e34889725c73064feb17ceb796831c0e111ba1a552b0f31b39"),
        public_key: hex("04c4b5eacb71a27be633ed970dcbc41c00440364bc04ba38ae4683ac24e708bf334891b9ad5c5e9ee021e796102e40f7345e1b402ff69fb4f52e221b1dabad1968"),
    },
    // wallet_4
    Entry {
        phrase: "board list obtain sugar hour worth raven scout denial thunder horse logic fury scorpion fold genuine phrase wealth news aim below celery when cabin",
        secret_key: hex("f9d7206a47f14d2870c163ebab4bf3e70d18f5d14ce1031f3902fbbc894fe4c7"),
        public_key: hex("04b3e0a76b292b2c83fc0ac14ae6160d0438ebe94e14bbb5b7755153628886e08ea5b60ea54a7fa77b069f5bc87f281b92ff2583d5e865312043b995115ef64727"),
    },
    // wallet_5
    Entry {
        phrase: "hurry aunt blame peanut heavy update captain human rice crime juice adult scale device promote vast project quiz unit note reform update climb purchase",
        secret_key: hex("3eccc5dac8056590432db6a35d52b9896876a3d5cbdea53b72400bc9c2099fe8"),
        public_key: hex("049fa3c9b2c98acc46563707702ee633141b2c125b85649e11548a30233f97ed9f9a3153eb9bb1eda3a9931ea12216cf3c94ba54ffa426edbcd52e057f036f97dd"),
    },
    // wallet_6
    Entry {
        phrase: "area desk dutch sign gold cricket dawn toward giggle vibrant indoor bench warfare wagon number tiny universe sand talk dilemma pottery bone trap buddy",
        secret_key: hex("7036b29cb5e235e5fd9b09ae3e8eec4404e44906814d5d01cbca968a60ed4bfb"),
        public_key: hex("048efa20fa5706567008ebaf48f7ae891342eeb944d96392f719c505c89f84ed8d36c32d5d9f728bbae80a35c7acb50a7b038d0e5d45c2e4669c15469db53eca08"),
    },
    // wallet_7
    Entry {
        phrase: "prevent gallery kind limb income control noise together echo rival record wedding sense uncover school version force bleak nuclear include danger skirt enact arrow",
        secret_key: hex("b463f0df6c05d2f156393eee73f8016c5372caa0e9e29a901bb7171d90dc4f14"),
        public_key: hex("043f19d77c842b675bd8c858e9ac8b0ca2efa566f17accf8ef9ceb5a992dc67836b95faf7956d718512b37bf67c0a6ee107206a2c980900142ce8b016110f90ffc"),
    },
    // wallet_8
    Entry {
        phrase: "female adjust gallery certain visit token during great side clown fitness like hurt clip knife warm bench start reunion globe detail dream depend fortune",
        secret_key: hex("6a1a754ba863d7bab14adbbc3f8ebb090af9e871ace621d3e5ab634e1422885e"),
        public_key: hex("049fb154a570a1645af3dd43c3c668a979b59d21a46dd717fd799b13be3b2a0dc79bd248ed760f07f0aa0719b4601950c95e4e1a35ddd855a398b12d3ae31dbd34"),
    },
    // faucet — also `DEFAULT_FAUCET_MNEMONIC`
    Entry {
        phrase: "shadow private easily thought say logic fault paddle word top book during ignore notable orange flight clock image wealth health outside kitten belt reform",
        secret_key: hex("de433bdfa14ec43aa1098d5be594c8ffb20a31485ff9de2923b2689471c401b8"),
        public_key: hex("04add319140c528a8955d76d4afe32c4d3143fea57ea353a31ce793cffb77ef8615cc09053489331377e2c637385f00cd5a1cafa54e4a083908d08dfae4f088d72"),
    },
    // `DEFAULT_STACKS_MINER_MNEMONIC`
    Entry {
        phrase: "fragile loan twenty basic net assault jazz absorb diet talk art shock innocent float punch travel gadget embrace caught blossom hockey surround initial reduce",
        secret_key: hex("3b68e410cc7f9b8bae76f2f2991b69ecd0627c95da22a904065dfb2a73d0585f"),
        public_key: hex("0439810ebf35e6f6c26062c99f3e183708d377720617c90a986859ec9c95d00be9e16af4fe2d857755f0dd691cecb3489a5c6e04889f0015ed0d39e8262fe36608"),
    },
    // `DEFAULT_STACKER_MNEMONIC`
    Entry {
        phrase: "empty lens any direct brother then drop fury rule pole win claim scissors list rescue horn rent inform relief jump sword weekend half legend",
        secret_key: hex("e93b8341c35983ae5b8f93b9530bd90281fd5e5e00a5094c2b7863ace78eac62"),
        public_key: hex("044b9b194720f870d99c4f629b84626e0cdf1198b10b4dca7c4851771d72d07f8578333fcbdeec9de504890796e32741f397bed30fa4bb7451cf0d4a457cdcb8c7"),
    },
];

fn find(phrase: &str, derivation: &str) -> Option<&'static Entry> {
    if derivation != DERIVATION_PATH {
        return None;
    }
    ENTRIES.iter().find(|entry| entry.phrase == phrase)
}

/// The derived keys for a shipped mnemonic, or `None` if it isn't one.
pub(crate) fn lookup(phrase: &str, derivation: &str) -> Option<(Vec<u8>, PublicKey)> {
    let entry = find(phrase, derivation)?;
    let public_key = PublicKey::parse(&entry.public_key)
        .expect("precomputed public key is not a valid secp256k1 point");
    Some((entry.secret_key.to_vec(), public_key))
}

/// Whether this mnemonic and path resolve from the table instead of being
/// derived. Exposed so crates that own the shipped mnemonics can assert their
/// defaults are covered.
pub fn is_precomputed(phrase: &str, derivation: &str) -> bool {
    find(phrase, derivation).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::derive_bip32_keys;

    /// The table is only as good as its agreement with the real derivation.
    #[test]
    fn entries_match_live_derivation() {
        for entry in ENTRIES {
            let (secret_key, public_key) = derive_bip32_keys(entry.phrase, "", DERIVATION_PATH)
                .unwrap_or_else(|e| panic!("failed to derive {}: {e}", entry.phrase));

            let hex = |bytes: &[u8]| bytes.iter().map(|b| format!("{b:02x}")).collect::<String>();
            assert_eq!(
                hex(&secret_key),
                hex(&entry.secret_key),
                "stale secret_key for \"{}\"",
                entry.phrase
            );
            assert_eq!(
                hex(&public_key.serialize()),
                hex(&entry.public_key),
                "stale public_key for \"{}\"",
                entry.phrase
            );
        }
    }

    #[test]
    fn lookup_is_scoped_to_the_default_derivation_path() {
        let phrase = ENTRIES[0].phrase;
        assert!(is_precomputed(phrase, DERIVATION_PATH));
        assert!(!is_precomputed(phrase, "m/44'/5757'/0'/0/1"));
        assert!(!is_precomputed("not a mnemonic", DERIVATION_PATH));
    }
}
