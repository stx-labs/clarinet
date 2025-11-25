use std::collections::BTreeMap;
use std::sync::LazyLock;

use chainhook_types::StacksNetwork;
use test_case::test_case;

use crate::chainhooks::stacks::{
    StacksChainhookSpecification, StacksChainhookSpecificationNetworkMap,
    StacksContractCallBasedPredicate, StacksContractDeploymentPredicate, StacksPredicate,
    StacksPrintEventBasedPredicate,
};
use crate::chainhooks::types::{HttpHook, *};

static TXID_NO_PREFIX: LazyLock<String> =
    LazyLock::new(|| "1234567890123456789012345678901234567890123456789012345678901234".into());
static TXID_NOT_HEX: LazyLock<String> =
    LazyLock::new(|| "0xw234567890123456789012345678901234567890123456789012345678901234".into());
static TXID_SHORT: LazyLock<String> =
    LazyLock::new(|| "0x234567890123456789012345678901234567890123456789012345678901234".into());
static TXID_LONG: LazyLock<String> =
    LazyLock::new(|| "0x11234567890123456789012345678901234567890123456789012345678901234".into());
static TXID_VALID: LazyLock<String> =
    LazyLock::new(|| "0x1234567890123456789012345678901234567890123456789012345678901234".into());
static STACKS_ADDRESS_INVALID: LazyLock<String> =
    LazyLock::new(|| "SQ1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM".into());
static STACKS_ADDRESS_VALID_MAINNET: LazyLock<String> =
    LazyLock::new(|| "SP000000000000000000002Q6VF78".into());
static STACKS_ADDRESS_VALID_TESTNET: LazyLock<String> =
    LazyLock::new(|| "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM".into());
static STACKS_ADDRESS_VALID_MULTISIG: LazyLock<String> =
    LazyLock::new(|| "SN2QE43MMXFDMAT3TPRGQ38BQ50VSRMBRQ6B16W5J".into());
static CONTRACT_ID_INVALID_ADDRESS: LazyLock<String> =
    LazyLock::new(|| "SQ1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.contract-name".into());
static CONTRACT_ID_NO_PERIOD: LazyLock<String> =
    LazyLock::new(|| "SQ1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGMcontract-name".into());
static CONTRACT_ID_INVALID_NAME: LazyLock<String> =
    LazyLock::new(|| "SQ1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.!&*!".into());
static CONTRACT_ID_VALID: LazyLock<String> =
    LazyLock::new(|| "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.contract-name".into());
static INVALID_METHOD: LazyLock<String> = LazyLock::new(|| "!@*&!*".into());
static INVALID_REGEX: LazyLock<String> = LazyLock::new(|| "[\\]".into());
static VALID_REGEX: LazyLock<String> = LazyLock::new(|| "anything".into());

static TXID_PREDICATE_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid predicate for scope 'txid': txid must be a 32 byte (64 character) hexadecimal string prefixed with '0x'".into()
});
static INVALID_SPEC_NETWORK_MAP_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid Stacks predicate 'test' for network simnet: invalid 'then_that' value: invalid 'http_post' data: url string must be a valid Url: relative URL without a base\ninvalid Stacks predicate 'test' for network simnet: invalid 'then_that' value: invalid 'http_post' data: auth header must be a valid header value: failed to parse header value\ninvalid Stacks predicate 'test' for network simnet: invalid 'if_this' value: invalid predicate for scope 'print_event': invalid contract identifier: TypeParseFailure(\"Invalid principal literal: base58ck checksum 0x147e6835 does not match expected 0x9b3dfe6a\")\ninvalid Stacks predicate 'test' for network simnet: invalid 'if_this' value: invalid predicate for scope 'print_event': invalid regex: regex parse error:\n    [\\]\n    ^\nerror: unclosed character class\ninvalid Stacks predicate 'test' for network devnet: invalid 'then_that' value: invalid 'http_post' data: url string must be a valid Url: relative URL without a base\ninvalid Stacks predicate 'test' for network devnet: invalid 'then_that' value: invalid 'http_post' data: auth header must be a valid header value: failed to parse header value\ninvalid Stacks predicate 'test' for network devnet: invalid 'if_this' value: invalid predicate for scope 'print_event': invalid contract identifier: TypeParseFailure(\"Invalid principal literal: base58ck checksum 0x147e6835 does not match expected 0x9b3dfe6a\")\ninvalid Stacks predicate 'test' for network devnet: invalid 'if_this' value: invalid predicate for scope 'print_event': invalid regex: regex parse error:\n    [\\]\n    ^\nerror: unclosed character class\ninvalid Stacks predicate 'test' for network testnet: invalid 'then_that' value: invalid 'http_post' data: url string must be a valid Url: relative URL without a base\ninvalid Stacks predicate 'test' for network testnet: invalid 'then_that' value: invalid 'http_post' data: auth header must be a valid header value: failed to parse header value\ninvalid Stacks predicate 'test' for network testnet: invalid 'if_this' value: invalid predicate for scope 'print_event': invalid contract identifier: TypeParseFailure(\"Invalid principal literal: base58ck checksum 0x147e6835 does not match expected 0x9b3dfe6a\")\ninvalid Stacks predicate 'test' for network testnet: invalid 'if_this' value: invalid predicate for scope 'print_event': invalid regex: regex parse error:\n    [\\]\n    ^\nerror: unclosed character class\ninvalid Stacks predicate 'test' for network mainnet: invalid 'then_that' value: invalid 'http_post' data: url string must be a valid Url: relative URL without a base\ninvalid Stacks predicate 'test' for network mainnet: invalid 'then_that' value: invalid 'http_post' data: auth header must be a valid header value: failed to parse header value\ninvalid Stacks predicate 'test' for network mainnet: invalid 'if_this' value: invalid predicate for scope 'print_event': invalid contract identifier: TypeParseFailure(\"Invalid principal literal: base58ck checksum 0x147e6835 does not match expected 0x9b3dfe6a\")\ninvalid Stacks predicate 'test' for network mainnet: invalid 'if_this' value: invalid predicate for scope 'print_event': invalid regex: regex parse error:\n    [\\]\n    ^\nerror: unclosed character class".into()
});
static CONTRACT_DEPLOYER_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid predicate for scope 'contract_deployment': contract deployer must be a valid Stacks address: TypeParseFailure(\"Invalid principal literal: base58ck checksum 0x147e6835 does not match expected 0x9b3dfe6a\")".into()
});
static CONTRACT_ID_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid predicate for scope 'contract_call': invalid contract identifier: TypeParseFailure(\"Invalid principal literal: base58ck checksum 0x147e6835 does not match expected 0x9b3dfe6a\")".into()
});
static CONTRACT_ID_NO_PERIOD_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid predicate for scope 'contract_call': invalid contract identifier: TypeParseFailure(\"Invalid principal literal: expected a `.` in a qualified contract name\")".into()
});
static CONTRACT_METHOD_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid predicate for scope 'contract_call': invalid contract method: BadNameValue(\"ClarityName\", \"!@*&!*\")".into()
});
static PRINT_EVENT_ID_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid predicate for scope 'print_event': invalid contract identifier: TypeParseFailure(\"Invalid principal literal: base58ck checksum 0x147e6835 does not match expected 0x9b3dfe6a\")".into()
});
static INVALID_REGEX_ERR: LazyLock<String> = LazyLock::new(|| {
    "invalid predicate for scope 'print_event': invalid regex: regex parse error:\n    [\\]\n    ^\nerror: unclosed character class".into()
});

static INVALID_PREDICATE: LazyLock<StacksPredicate> = LazyLock::new(|| {
    StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::MatchesRegex {
        contract_identifier: CONTRACT_ID_INVALID_ADDRESS.clone(),
        regex: INVALID_REGEX.clone(),
    })
});
static INVALID_HOOK_ACTION: LazyLock<HookAction> = LazyLock::new(|| {
    HookAction::HttpPost(HttpHook {
        url: "".into(),
        authorization_header: "\n".into(),
    })
});
static ALL_INVALID_SPEC: LazyLock<StacksChainhookSpecification> = LazyLock::new(|| {
    StacksChainhookSpecification::new(INVALID_PREDICATE.clone(), INVALID_HOOK_ACTION.clone())
});
static ALL_INVALID_SPEC_NETWORK_MAP: LazyLock<ChainhookSpecificationNetworkMap> =
    LazyLock::new(|| {
        ChainhookSpecificationNetworkMap::Stacks(StacksChainhookSpecificationNetworkMap {
            uuid: "test".into(),
            owner_uuid: None,
            name: "test".into(),
            version: 1,
            networks: BTreeMap::from([
                (StacksNetwork::Simnet, ALL_INVALID_SPEC.clone()),
                (StacksNetwork::Devnet, ALL_INVALID_SPEC.clone()),
                (StacksNetwork::Testnet, ALL_INVALID_SPEC.clone()),
                (StacksNetwork::Mainnet, ALL_INVALID_SPEC.clone()),
            ]),
        })
    });

// StacksPredicate::BlockHeight
#[test_case(
    &StacksPredicate::BlockHeight(BlockIdentifierIndexRule::LowerThan(0)),
    Some(vec!["invalid predicate for scope 'block_height': 'lower_than' filter must be greater than 0".to_string()]);
    "invalid lower than"
)]
#[test_case(&StacksPredicate::BlockHeight(BlockIdentifierIndexRule::LowerThan(1)), None; "valid lower than")]
#[test_case(
    &StacksPredicate::BlockHeight(BlockIdentifierIndexRule::Between(10, 5)),
    Some(vec!["invalid predicate for scope 'block_height': 'between' filter must have left-hand-side valud greater than right-hand-side value".to_string()]);
    "invalid between"
)]
#[test_case(&StacksPredicate::BlockHeight(BlockIdentifierIndexRule::Between(5, 10)), None; "valid between")]
// StacksPredicate::ContractDeployment
#[test_case(
    &StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::Deployer((*STACKS_ADDRESS_INVALID).clone())),
    Some(vec![(*CONTRACT_DEPLOYER_ERR).clone()]);
    "deployer bad prefix"
)]
#[test_case(
    &StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::Deployer((*STACKS_ADDRESS_VALID_MAINNET).clone())),
    None;
    "deployer valid mainnet"
)]
#[test_case(
    &StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::Deployer((*STACKS_ADDRESS_VALID_TESTNET).clone())),
    None;
    "deployer valid testnet"
)]
#[test_case(
    &StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::Deployer((*STACKS_ADDRESS_VALID_MULTISIG).clone())),
    None;
    "deployer valid multisig"
)]
#[test_case(
    &StacksPredicate::ContractDeployment(StacksContractDeploymentPredicate::Deployer("*".to_string())),
    None;
    "deployer valid wildcard"
)]
// StacksPredicate::ContractCall
#[test_case(
    &StacksPredicate::ContractCall(StacksContractCallBasedPredicate { contract_identifier: (*CONTRACT_ID_INVALID_ADDRESS).clone(), method: (*INVALID_METHOD).clone()}),
    Some(vec![(*CONTRACT_ID_ERR).clone(), (*CONTRACT_METHOD_ERR).clone()]);
    "invalid id with invalid method"
)]
#[test_case(
    &StacksPredicate::ContractCall(StacksContractCallBasedPredicate { contract_identifier: (*CONTRACT_ID_VALID).clone(), method: (*INVALID_METHOD).clone()}),
    Some(vec![(*CONTRACT_METHOD_ERR).clone()]);
    "valid id with invalid method"
)]
#[test_case(
    &StacksPredicate::ContractCall(StacksContractCallBasedPredicate { contract_identifier: (*CONTRACT_ID_NO_PERIOD).clone(), method: "contract-name".to_string()}),
    Some(vec![(*CONTRACT_ID_NO_PERIOD_ERR).clone()]);
    "id no period"
)]
#[test_case(
    &StacksPredicate::ContractCall(StacksContractCallBasedPredicate { contract_identifier: (*CONTRACT_ID_INVALID_NAME).clone(), method: "contract-name".to_string()}),
    Some(vec![(*CONTRACT_ID_ERR).clone()]);
    "id invalid contract name"
)]
#[test_case(
    &StacksPredicate::ContractCall(StacksContractCallBasedPredicate { contract_identifier: (*CONTRACT_ID_VALID).clone(), method: "contract-name".to_string()}),
    None;
    "id valid"
)]
// StacksPredicate::PrintEvent
#[test_case(
    &StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::Contains { contract_identifier: (*CONTRACT_ID_INVALID_ADDRESS).clone(), contains: "string".to_string() }),
    Some(vec![(*PRINT_EVENT_ID_ERR).clone()]);
    "contains invalid id"
)]
#[test_case(
    &StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::Contains { contract_identifier: (*CONTRACT_ID_VALID).clone(), contains: "string".to_string() }),
    None;
    "contains valid"
)]
#[test_case(
    &StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::Contains { contract_identifier: "*".to_string(), contains: "string".to_string() }),
    None;
    "allows wildcard contract id"
)]
#[test_case(
    &StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::MatchesRegex { contract_identifier: (*CONTRACT_ID_INVALID_ADDRESS).clone(), regex: (*VALID_REGEX).clone() }),
    Some(vec![(*PRINT_EVENT_ID_ERR).clone()]);
    "regex invalid id"
)]
#[test_case(
    &StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::MatchesRegex { contract_identifier: (*CONTRACT_ID_VALID).clone(), regex:  (*INVALID_REGEX).clone() }),
    Some(vec![(*INVALID_REGEX_ERR).clone()]);
    "regex invalid regex"
)]
#[test_case(
    &StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::MatchesRegex { contract_identifier: (*CONTRACT_ID_INVALID_ADDRESS).clone(), regex:  (*INVALID_REGEX).clone() }),
    Some(vec![(*PRINT_EVENT_ID_ERR).clone(), (*INVALID_REGEX_ERR).clone()]);
    "regex invalid both"
)]
#[test_case(
    &StacksPredicate::PrintEvent(StacksPrintEventBasedPredicate::MatchesRegex { contract_identifier: (*CONTRACT_ID_VALID).clone(), regex:  (*VALID_REGEX).clone() }),
    None;
    "regex valid"
)]
// StacksPredicate::Txid
#[test_case(
    &StacksPredicate::Txid(ExactMatchingRule::Equals((*TXID_NO_PREFIX).clone())),
    Some(vec![(*TXID_PREDICATE_ERR).clone()]); "txid without 0x"
)]
#[test_case(
    &StacksPredicate::Txid(ExactMatchingRule::Equals((*TXID_NOT_HEX).clone())),
    Some(vec![(*TXID_PREDICATE_ERR).clone()]); "txid not hex"
)]
#[test_case(
    &StacksPredicate::Txid(ExactMatchingRule::Equals((*TXID_SHORT).clone())),
    Some(vec![(*TXID_PREDICATE_ERR).clone()]); "txid too short"
)]
#[test_case(
    &StacksPredicate::Txid(ExactMatchingRule::Equals((*TXID_LONG).clone())),
    Some(vec![(*TXID_PREDICATE_ERR).clone()]); "txid too long"
)]
#[test_case(
    &StacksPredicate::Txid(ExactMatchingRule::Equals((*TXID_VALID).clone())),
    None; "txid just right"
)]
fn it_validates_stacks_predicates(predicate: &StacksPredicate, expected_err: Option<Vec<String>>) {
    if let Err(e) = predicate.validate() {
        if let Some(expected) = expected_err {
            assert_eq!(e, expected);
        } else {
            panic!("Unexpected error in predicate validation: {:?}", predicate);
        }
    } else if expected_err.is_some() {
        panic!(
            "Missing expected error for predicate validation: {:?}",
            predicate
        );
    }
}

#[test_case(&*ALL_INVALID_SPEC_NETWORK_MAP, (*INVALID_SPEC_NETWORK_MAP_ERR).clone())]
fn it_validates_stacks_chainhook_specs(
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
