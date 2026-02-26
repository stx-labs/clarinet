use std::sync::LazyLock;

use test_case::test_case;

use super::*;
use crate::chainhook::hooks::types::{ChainhookSpecificationNetworkMap, HttpHook};
use crate::chainhook::types::BitcoinNetwork;

static INVALID_URL_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid 'http_post' data: url string must be a valid Url: relative URL without a base".into()
});
static INVALID_HTTP_HEADER_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid 'http_post' data: auth header must be a valid header value: failed to parse header value".into()
});
static INVALID_SPEC_NETWORK_MAP_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid Bitcoin predicate 'test' for network regtest: invalid 'then_that' value: invalid 'http_post' data: url string must be a valid Url: relative URL without a base\ninvalid Bitcoin predicate 'test' for network regtest: invalid 'then_that' value: invalid 'http_post' data: auth header must be a valid header value: failed to parse header value\ninvalid Bitcoin predicate 'test' for network regtest: invalid 'if_this' value: invalid predicate for scope 'txid': txid must be a 32 byte (64 character) hexadecimal string prefixed with '0x'\ninvalid Bitcoin predicate 'test' for network testnet: invalid 'then_that' value: invalid 'http_post' data: url string must be a valid Url: relative URL without a base\ninvalid Bitcoin predicate 'test' for network testnet: invalid 'then_that' value: invalid 'http_post' data: auth header must be a valid header value: failed to parse header value\ninvalid Bitcoin predicate 'test' for network testnet: invalid 'if_this' value: invalid predicate for scope 'txid': txid must be a 32 byte (64 character) hexadecimal string prefixed with '0x'\ninvalid Bitcoin predicate 'test' for network signet: invalid 'then_that' value: invalid 'http_post' data: url string must be a valid Url: relative URL without a base\ninvalid Bitcoin predicate 'test' for network signet: invalid 'then_that' value: invalid 'http_post' data: auth header must be a valid header value: failed to parse header value\ninvalid Bitcoin predicate 'test' for network signet: invalid 'if_this' value: invalid predicate for scope 'txid': txid must be a 32 byte (64 character) hexadecimal string prefixed with '0x'\ninvalid Bitcoin predicate 'test' for network mainnet: invalid 'then_that' value: invalid 'http_post' data: url string must be a valid Url: relative URL without a base\ninvalid Bitcoin predicate 'test' for network mainnet: invalid 'then_that' value: invalid 'http_post' data: auth header must be a valid header value: failed to parse header value\ninvalid Bitcoin predicate 'test' for network mainnet: invalid 'if_this' value: invalid predicate for scope 'txid': txid must be a 32 byte (64 character) hexadecimal string prefixed with '0x'".into()
});

static INVALID_TXID_PREDICATE: LazyLock<BitcoinPredicateType> =
    LazyLock::new(|| BitcoinPredicateType::Txid(ExactMatchingRule::Equals("test".into())));
static INVALID_HOOK_ACTION: LazyLock<HookAction> = LazyLock::new(|| {
    HookAction::HttpPost(HttpHook {
        url: "".into(),
        authorization_header: "\n".into(),
    })
});
static ALL_INVALID_SPEC: LazyLock<BitcoinChainhookSpecification> = LazyLock::new(|| {
    BitcoinChainhookSpecification::new(INVALID_TXID_PREDICATE.clone(), INVALID_HOOK_ACTION.clone())
});
static ALL_INVALID_SPEC_NETWORK_MAP: LazyLock<ChainhookSpecificationNetworkMap> =
    LazyLock::new(|| {
        ChainhookSpecificationNetworkMap::Bitcoin(BitcoinChainhookSpecificationNetworkMap {
            uuid: "test".into(),
            owner_uuid: None,
            name: "test".into(),
            version: 1,
            networks: BTreeMap::from([
                (BitcoinNetwork::Regtest, ALL_INVALID_SPEC.clone()),
                (BitcoinNetwork::Signet, ALL_INVALID_SPEC.clone()),
                (BitcoinNetwork::Mainnet, ALL_INVALID_SPEC.clone()),
                (BitcoinNetwork::Testnet, ALL_INVALID_SPEC.clone()),
            ]),
        })
    });

#[test_case(&*INVALID_HOOK_ACTION, Some(vec![(*INVALID_URL_ERR).clone(), (*INVALID_HTTP_HEADER_ERR).clone()]); "invalid http_post action"
)]
fn it_validates_hook_actions(action: &HookAction, expected_err: Option<Vec<String>>) {
    if let Err(e) = action.validate() {
        if let Some(expected) = expected_err {
            assert_eq!(e, expected);
        } else {
            panic!("Unexpected error in predicate validation: {:?}", action);
        }
    } else if expected_err.is_some() {
        panic!(
            "Missing expected error for predicate validation: {:?}",
            action
        );
    }
}

#[test_case(&*ALL_INVALID_SPEC_NETWORK_MAP, (*INVALID_SPEC_NETWORK_MAP_ERR).clone())]
fn it_validates_bitcoin_chainhook_specs(
    predicate: &ChainhookSpecificationNetworkMap,
    expected_err: String,
) {
    if let Err(e) = predicate.validate() {
        assert_eq!(e, expected_err);
    } else {
        panic!(
            "Missing expected error for predicate validation: {:?}",
            predicate
        );
    }
}
