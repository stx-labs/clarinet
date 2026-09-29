use clarinet_utils::get_bip32_keys_from_mnemonic;
use clarity::codec::StacksMessageCodec;
use clarity::types::chainstate::StacksAddress;
use clarity::util::secp256k1::{MessageSignature, Secp256k1PrivateKey, Secp256k1PublicKey};
use clarity::vm::types::{PrincipalData, QualifiedContractIdentifier};
use clarity::vm::{ClarityName, ClarityVersion, ContractName, Value as ClarityValue};
use libsecp256k1::PublicKey;
use stacks_codec::strings::StacksString;
use stacks_codec::transaction::{
    SinglesigHashMode, SinglesigSpendingCondition, StacksTransaction, TokenTransferMemo,
    TransactionAnchorMode, TransactionAuth, TransactionContractCall, TransactionPayload,
    TransactionPostConditionMode, TransactionPublicKeyEncoding, TransactionSmartContract,
    TransactionSpendingCondition, TransactionVersion,
};
use stacks_common::address::{
    AddressHashMode, C32_ADDRESS_VERSION_MAINNET_SINGLESIG, C32_ADDRESS_VERSION_TESTNET_SINGLESIG,
};

#[derive(Clone, Debug)]
pub struct Wallet {
    pub mnemonic: String,
    pub derivation: String,
    pub mainnet: bool,
}

impl Wallet {
    pub fn compute_stacks_address(&self) -> StacksAddress {
        let keypair = compute_keypair(self);
        compute_stacks_address(&keypair.public_key, self.mainnet)
    }
}

pub struct Keypair {
    pub secret_key: Secp256k1PrivateKey,
    pub public_key: PublicKey,
}

pub fn compute_stacks_address(public_key: &PublicKey, mainnet: bool) -> StacksAddress {
    let wrapped_public_key =
        Secp256k1PublicKey::from_slice(&public_key.serialize_compressed()).unwrap();

    StacksAddress::from_public_keys(
        match mainnet {
            true => C32_ADDRESS_VERSION_MAINNET_SINGLESIG,
            false => C32_ADDRESS_VERSION_TESTNET_SINGLESIG,
        },
        &AddressHashMode::SerializeP2PKH,
        1,
        &vec![wrapped_public_key],
    )
    .unwrap()
}

pub fn compute_keypair(wallet: &Wallet) -> Keypair {
    let (secret_bytes, public_key) =
        get_bip32_keys_from_mnemonic(&wallet.mnemonic, "", &wallet.derivation).unwrap();
    let wrapped_secret_key = Secp256k1PrivateKey::from_slice(&secret_bytes).unwrap();
    Keypair {
        secret_key: wrapped_secret_key,
        public_key,
    }
}

pub fn sign_transaction_payload(
    wallet: &Wallet,
    payload: TransactionPayload,
    nonce: u64,
    tx_fee: u64,
    anchor_mode: TransactionAnchorMode,
) -> Result<StacksTransaction, String> {
    let keypair = compute_keypair(wallet);
    let signer_addr = compute_stacks_address(&keypair.public_key, wallet.mainnet);

    let spending_condition = TransactionSpendingCondition::Singlesig(SinglesigSpendingCondition {
        signer: signer_addr.bytes().clone(),
        nonce,
        tx_fee,
        hash_mode: SinglesigHashMode::P2PKH,
        key_encoding: TransactionPublicKeyEncoding::Compressed,
        signature: MessageSignature::empty(),
    });

    let auth = TransactionAuth::Standard(spending_condition);
    let unsigned_tx = StacksTransaction {
        version: match wallet.mainnet {
            true => TransactionVersion::Mainnet,
            false => TransactionVersion::Testnet,
        },
        chain_id: match wallet.mainnet {
            true => 0x00000001,
            false => 0x80000000,
        },
        auth,
        anchor_mode,
        post_condition_mode: TransactionPostConditionMode::Allow,
        post_conditions: vec![],
        payload,
    };

    let mut unsigned_tx_bytes = vec![];
    unsigned_tx
        .consensus_serialize(&mut unsigned_tx_bytes)
        .expect("FATAL: invalid transaction");

    let mut signed_tx = unsigned_tx;
    let sighash = signed_tx.sign_begin();
    signed_tx
        .sign_next_origin(&sighash, &keypair.secret_key)
        .unwrap();
    Ok(signed_tx)
}

pub fn encode_contract_call(
    contract_id: &QualifiedContractIdentifier,
    function_name: ClarityName,
    function_args: Vec<ClarityValue>,
    wallet: &Wallet,
    nonce: u64,
    tx_fee: u64,
    anchor_mode: TransactionAnchorMode,
) -> Result<StacksTransaction, String> {
    let payload = TransactionContractCall {
        contract_name: contract_id.name.clone(),
        address: StacksAddress::from(contract_id.issuer.clone()),
        function_name,
        function_args,
    };
    sign_transaction_payload(
        wallet,
        TransactionPayload::ContractCall(payload),
        nonce,
        tx_fee,
        anchor_mode,
    )
}

pub fn encode_stx_transfer(
    recipient: PrincipalData,
    amount: u64,
    memo: [u8; 34],
    wallet: &Wallet,
    nonce: u64,
    tx_fee: u64,
    anchor_mode: TransactionAnchorMode,
) -> Result<StacksTransaction, String> {
    let payload = TransactionPayload::TokenTransfer(recipient, amount, TokenTransferMemo(memo));
    sign_transaction_payload(wallet, payload, nonce, tx_fee, anchor_mode)
}

pub fn encode_contract_publish(
    contract_name: &ContractName,
    source: &str,
    clarity_version: Option<ClarityVersion>,
    wallet: &Wallet,
    nonce: u64,
    tx_fee: u64,
    anchor_mode: TransactionAnchorMode,
) -> Result<StacksTransaction, String> {
    let payload = TransactionSmartContract {
        name: contract_name.clone(),
        code_body: StacksString::from_str(source).unwrap(),
    };
    sign_transaction_payload(
        wallet,
        TransactionPayload::SmartContract(payload, clarity_version),
        nonce,
        tx_fee,
        anchor_mode,
    )
}

pub fn build_contract_call_transaction(
    contract_id: String,
    function_name: String,
    args: Vec<ClarityValue>,
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

    let mut signed_tx = unsigned_tx;
    let sighash = signed_tx.sign_begin();
    signed_tx.sign_next_origin(&sighash, &secret_key).unwrap();

    signed_tx
}

pub fn build_contract_publish_transaction(
    contract_name: &str,
    source: &str,
    clarity_version: Option<ClarityVersion>,
    nonce: u64,
    fee: u64,
    sender_secret_key: &[u8],
) -> StacksTransaction {
    let payload = TransactionSmartContract {
        name: ContractName::try_from(contract_name.to_string()).unwrap(),
        code_body: StacksString::from_str(source).unwrap(),
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
        chain_id: 0x80000000,
        auth,
        anchor_mode,
        post_condition_mode: TransactionPostConditionMode::Allow,
        post_conditions: vec![],
        payload: TransactionPayload::SmartContract(payload, clarity_version),
    };

    let mut unsigned_tx_bytes = vec![];
    unsigned_tx
        .consensus_serialize(&mut unsigned_tx_bytes)
        .expect("FATAL: invalid transaction");

    let mut signed_tx = unsigned_tx;
    let sighash = signed_tx.sign_begin();
    signed_tx.sign_next_origin(&sighash, &secret_key).unwrap();

    signed_tx
}

#[cfg(test)]
mod tests {
    use clarity::types::PrivateKey;
    use clarity::util::hash::{bytes_to_hex, hex_bytes};
    use stacks_codec::transaction::TransactionAuthVerificationMode;

    use super::*;

    /// The deployer wallet `clarinet new` writes into `settings/Devnet.toml`.
    /// The expected values are the fixtures the template prints as comments,
    /// so a derivation or signing regression fails against addresses users
    /// can read in their own generated manifest.
    fn deployer_wallet() -> Wallet {
        Wallet {
            mnemonic: clarinet_utils::DEFAULT_DEPLOYER_MNEMONIC.to_string(),
            derivation: clarinet_utils::DEFAULT_DERIVATION_PATH.to_string(),
            mainnet: false,
        }
    }

    fn deployer_wallet_mainnet() -> Wallet {
        Wallet {
            mainnet: true,
            ..deployer_wallet()
        }
    }

    fn wallet_1() -> Wallet {
        Wallet {
            mnemonic: clarinet_utils::DEFAULT_WALLET_1_MNEMONIC.to_string(),
            derivation: clarinet_utils::DEFAULT_DERIVATION_PATH.to_string(),
            mainnet: false,
        }
    }

    /// The deployer secret key from the `clarinet new` template.
    fn deployer_secret_key() -> Vec<u8> {
        hex_bytes("753b7cc01a1a2e86221266a154af739463fce51219d97e4f856cd7200c3bd2a601").unwrap()
    }

    fn counter_call_payload() -> TransactionPayload {
        let contract_id =
            QualifiedContractIdentifier::parse("ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.counter")
                .unwrap();
        TransactionPayload::ContractCall(TransactionContractCall {
            address: StacksAddress::from(contract_id.issuer),
            contract_name: "counter".try_into().unwrap(),
            function_name: "increment".try_into().unwrap(),
            function_args: vec![ClarityValue::UInt(1)],
        })
    }

    #[test]
    fn deployer_address_matches_the_devnet_template() {
        assert_eq!(
            deployer_wallet().compute_stacks_address().to_string(),
            "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM"
        );
    }

    #[test]
    fn wallet_1_address_matches_the_devnet_template() {
        assert_eq!(
            wallet_1().compute_stacks_address().to_string(),
            "ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5"
        );
    }

    #[test]
    fn mainnet_version_byte_is_applied() {
        // Same key material as the devnet deployer, mainnet version byte.
        // Mirrors the mainnet deployer address asserted in clarinet-cli's
        // console tests.
        assert_eq!(
            deployer_wallet_mainnet()
                .compute_stacks_address()
                .to_string(),
            "SP1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRCBGD7R"
        );
    }

    #[test]
    fn keypair_matches_the_precomputed_deployer_keys() {
        let keypair = compute_keypair(&deployer_wallet());
        // `PrivateKey::to_bytes` returns the 32-byte scalar; the compression
        // flag lives on the public key side.
        let secret_bytes = keypair.secret_key.to_bytes();
        assert_eq!(
            bytes_to_hex(&secret_bytes),
            "753b7cc01a1a2e86221266a154af739463fce51219d97e4f856cd7200c3bd2a6"
        );
        assert_eq!(
            bytes_to_hex(&keypair.public_key.serialize_compressed()),
            "0390a5cac7c33fda49f70bc1b0866fa0ba7a9440d9de647fecb8132ceb76a94dfa"
        );
    }

    #[test]
    fn signed_contract_call_txid_is_stable() {
        let tx = sign_transaction_payload(
            &deployer_wallet(),
            counter_call_payload(),
            0,
            1_000,
            TransactionAnchorMode::Any,
        )
        .unwrap();
        assert_eq!(
            tx.txid().to_string(),
            "8bc7c85ac68ef7ae63e6353cf9e9955aa54a87c4d1e34fd7b1b6e3536544687c",
            "contract-call serialization or signing drifted"
        );
    }

    #[test]
    fn signed_transaction_fields_round_trip() {
        let tx = sign_transaction_payload(
            &deployer_wallet(),
            counter_call_payload(),
            7,
            2_500,
            TransactionAnchorMode::Any,
        )
        .unwrap();
        assert_eq!(tx.version, TransactionVersion::Testnet);
        assert_eq!(tx.chain_id, 0x80000000);
        let TransactionAuth::Standard(TransactionSpendingCondition::Singlesig(origin)) = &tx.auth
        else {
            panic!("expected singlesig standard auth");
        };
        assert_eq!(origin.nonce, 7);
        assert_eq!(origin.tx_fee, 2_500);
        assert_eq!(origin.hash_mode, SinglesigHashMode::P2PKH);
        assert_eq!(
            origin.key_encoding,
            TransactionPublicKeyEncoding::Compressed
        );
        assert!(
            tx.verify(TransactionAuthVerificationMode::EnforceLowS)
                .is_ok(),
            "signature must verify"
        );
    }

    #[test]
    fn stx_transfer_txid_is_stable() {
        let recipient =
            PrincipalData::parse_standard_principal("ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG")
                .unwrap()
                .into();
        let mut memo = [0u8; 34];
        memo[0..5].copy_from_slice(b"hello");
        let tx = encode_stx_transfer(
            recipient,
            1_000_000,
            memo,
            &wallet_1(),
            0,
            500,
            TransactionAnchorMode::Any,
        )
        .unwrap();
        assert_eq!(
            tx.txid().to_string(),
            "a5364f209406bce0205052fd2333836e61b7b277122b578e1baa58adbada8edd",
            "stx-transfer serialization drifted"
        );
    }

    #[test]
    fn contract_publish_txid_is_stable() {
        let contract_name: ContractName = "hello-world".try_into().unwrap();
        let tx = encode_contract_publish(
            &contract_name,
            "(define-public (say-hi) (ok u1))",
            Some(ClarityVersion::Clarity2),
            &deployer_wallet(),
            0,
            10_000,
            TransactionAnchorMode::Any,
        )
        .unwrap();
        assert_eq!(
            tx.txid().to_string(),
            "ec0de5590f5f3738086bc02e4fc837bcd611387e0f4bd87f69f453c24a810753",
            "contract-publish serialization drifted"
        );
    }

    #[test]
    fn build_contract_call_transaction_signs_with_raw_secret() {
        let tx = build_contract_call_transaction(
            "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.counter".to_string(),
            "increment".to_string(),
            vec![],
            3,
            1_000,
            &deployer_secret_key(),
        );
        assert_eq!(
            tx.txid().to_string(),
            "25b10176f57998de92fac6fc6bd4ba175014c56dcf4df1e561c5f60fca8a82b3",
            "raw-secret contract-call serialization drifted"
        );
    }

    #[test]
    fn build_contract_publish_transaction_signs_with_raw_secret() {
        let tx = build_contract_publish_transaction(
            "hello-world",
            "(define-public (say-hi) (ok u1))",
            Some(ClarityVersion::Clarity2),
            3,
            10_000,
            &deployer_secret_key(),
        );
        assert_eq!(
            tx.txid().to_string(),
            "93bb19e1517b3cee476def533f3e3cb8cc058351e50699f375b33faddf238a39",
            "raw-secret contract-publish serialization drifted"
        );
    }

    /// `bytes_to_hex` is what `mock_stacks_rpc` and `rpc_client` rely on for
    /// tx payloads; keep the txids asserted above consistent with the raw
    /// wire format.
    #[test]
    fn signed_contract_call_raw_encoding_is_stable() {
        let tx = sign_transaction_payload(
            &deployer_wallet(),
            counter_call_payload(),
            0,
            1_000,
            TransactionAnchorMode::Any,
        )
        .unwrap();
        let mut bytes = vec![];
        tx.consensus_serialize(&mut bytes)
            .expect("FATAL: invalid transaction");
        assert_eq!(
            bytes_to_hex(&bytes),
            "808000000004006d78de7b0625dfbfc16c3a8a5735f6dc3dc3f2ce000000000000000000000000000003e8000038781d4e42c05f5993ecbec3f93971afdc57b39b390df2f4403d588fb8a542402dc025a0e6d5d7a834bb774ff372284531e49116951323dfde3b043f0bc34751030100000000021a6d78de7b0625dfbfc16c3a8a5735f6dc3dc3f2ce07636f756e74657209696e6372656d656e74000000010100000000000000000000000000000001"
        );
    }
}
