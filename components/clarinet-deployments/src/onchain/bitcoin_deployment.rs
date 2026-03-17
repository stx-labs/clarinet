use std::str::FromStr;

use base58::FromBase58;
use bitcoin::absolute::LockTime;
use bitcoin::blockdata::opcodes;
use bitcoin::blockdata::script::Builder;
use bitcoin::consensus::encode;
use bitcoin::hashes::Hash;
use bitcoin::script::PushBytes;
use bitcoin::sighash::SighashCache;
use bitcoin::transaction::Version;
use bitcoin::{
    Amount, OutPoint, PubkeyHash, ScriptBuf, Sequence, Transaction, TxIn, TxOut, Witness,
};
use bitcoincore_rpc::bitcoin::secp256k1::{Message, PublicKey, Secp256k1, SecretKey};
use bitcoincore_rpc::bitcoin::Address;
use bitcoincore_rpc::{Client, RpcApi};
use bitcoincore_rpc_json::ListUnspentResultEntry;
use clarity::util::hash::bytes_to_hex;

use crate::types::BtcTransferSpecification;

pub fn build_transaction_spec(
    tx_spec: &BtcTransferSpecification,
    utxos: &mut Vec<ListUnspentResultEntry>,
) -> (Transaction, Vec<ListUnspentResultEntry>) {
    let mut transaction = Transaction {
        version: Version::ONE,
        lock_time: LockTime::ZERO,
        input: vec![],
        output: vec![],
    };

    // UTXOs selection
    let mut selected_utxos = Vec::new();
    let mut selected_utxos_indices = Vec::new();
    let mut cumulated_amount = 0;
    let typical_size = 600;
    let tx_fee = tx_spec.sats_per_byte * typical_size;
    let total_required = tx_spec.sats_amount + tx_fee;
    utxos.sort_by(|a, b| a.amount.cmp(&b.amount));
    for (i, utxo) in utxos.iter().enumerate() {
        cumulated_amount += utxo.amount.to_sat();
        selected_utxos_indices.push(i);
        if cumulated_amount >= total_required {
            break;
        }
    }
    if cumulated_amount < total_required {
        panic!("Unable to get enough UTXOs");
    }

    // Prepare transaction inputs
    selected_utxos_indices.reverse();
    for index in selected_utxos_indices {
        let utxo = utxos.remove(index);
        let input = TxIn {
            previous_output: OutPoint {
                txid: utxo.txid,
                vout: utxo.vout,
            },
            script_sig: ScriptBuf::default(),
            sequence: Sequence(0xFFFFFFFD), // allow RBF
            witness: Witness::new(),
        };
        transaction.input.push(input);
        selected_utxos.push(utxo);
    }

    let address = Address::from_str(&tx_spec.recipient).unwrap_or_else(|e| panic!("{e:?}"));

    let txout = TxOut {
        value: Amount::from_sat(tx_spec.sats_amount),
        script_pubkey: address.assume_checked_ref().script_pubkey(),
    };
    transaction.output.push(txout);

    // Prepare Sender change output
    let sender_pub_key_hash = tx_spec
        .expected_sender
        .from_base58()
        .expect("Unable to get bytes sender btc address");

    let pubkey_hash = PubkeyHash::from_slice(&sender_pub_key_hash[1..21]).expect("Invalid hash");

    let txout = TxOut {
        value: Amount::from_sat(cumulated_amount - tx_spec.sats_amount - tx_fee),
        script_pubkey: Builder::new()
            .push_opcode(opcodes::all::OP_DUP)
            .push_opcode(opcodes::all::OP_HASH160)
            .push_slice(pubkey_hash)
            .push_opcode(opcodes::all::OP_EQUALVERIFY)
            .push_opcode(opcodes::all::OP_CHECKSIG)
            .into_script(),
    };
    transaction.output.push(txout);

    (transaction, selected_utxos)
}

pub fn sign_transaction(
    transaction: &mut Transaction,
    utxos: Vec<ListUnspentResultEntry>,
    signer: &SecretKey,
) {
    for (i, utxo) in utxos.into_iter().enumerate() {
        let sig_hash_all = 0x01;
        let script_pub_key = ScriptBuf::from(utxo.script_pub_key.into_bytes());
        let sig_hash = SighashCache::new(transaction.clone())
            .legacy_signature_hash(i, &script_pub_key, sig_hash_all)
            .unwrap();

        let (sig_der, public_key) = {
            let sig_hash_bytes = sig_hash;
            let message =
                Message::from_digest_slice(&sig_hash_bytes[..]).expect("Unable to create Message");
            let secp = Secp256k1::new();
            let signature = secp.sign_ecdsa_recoverable(&message, signer);
            let public_key = PublicKey::from_secret_key(&secp, signer);
            let sig_der = signature.to_standard().serialize_der();
            (sig_der, public_key)
        };

        let sig_slice = [&*sig_der, &[sig_hash_all as u8][..]].concat();

        transaction.input[i].script_sig = Builder::new()
            .push_slice(<&PushBytes>::try_from(sig_slice.as_slice()).unwrap())
            .push_slice(public_key.serialize())
            .into_script();
    }
}

pub fn send_transaction_spec(
    bitcoin_rpc: &Client,
    bitcoin_wallet_rpc: &Client,
    tx_spec: &BtcTransferSpecification,
    signer: &SecretKey,
) -> Result<bitcoincore_rpc::bitcoin::Txid, String> {
    // In this v1, we're assuming that the bitcoin node is indexing sender's UTXOs.
    let sender_address =
        Address::from_str(&tx_spec.expected_sender).expect("Unable to parse address");

    let mut utxos = bitcoin_wallet_rpc
        .list_unspent(
            None,
            None,
            Some(&[sender_address.assume_checked_ref()]),
            None,
            None,
        )
        .expect("Unable to retrieve UTXOs");

    let (mut transaction, selected_utxos) = build_transaction_spec(tx_spec, &mut utxos);
    sign_transaction(&mut transaction, selected_utxos, signer);

    println!("-> Transaction\n{transaction:?}");

    let encoded_tx = encode::serialize(&transaction);

    println!("-> Transaction HEX\n{:?}", bytes_to_hex(&encoded_tx));

    let res = bitcoin_rpc.send_raw_transaction(&encoded_tx);

    Ok(res.unwrap())
}
