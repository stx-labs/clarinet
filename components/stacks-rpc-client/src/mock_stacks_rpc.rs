use clarity::util::hash::bytes_to_hex;
use clarity::vm::Value;
use mockito::{Mock, ServerGuard};
use serde_json::json;

use crate::rpc_client::NodeInfo;

pub struct MockStacksRpc {
    pub url: String,
    client: ServerGuard,
}

impl Default for MockStacksRpc {
    fn default() -> Self {
        Self::new()
    }
}

impl MockStacksRpc {
    pub fn new() -> Self {
        let client = mockito::Server::new();
        let url = client.url();
        Self { client, url }
    }

    pub fn get_info_mock(&mut self, info: NodeInfo) -> Mock {
        self.client
            .mock("GET", "/v2/info")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(json!(info).to_string())
            .create()
    }

    pub fn get_nonce_mock(&mut self, address: &str, nonce: u64) -> Mock {
        self.client.mock(
            "GET",
            format!("/v2/accounts/{address}").as_str(),
        )
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(r#"{{"balance":"10000000","nonce":{nonce}, "nonce_proof":"0x123", "balance_proof":"0x123"}}"#))
            .create()
    }

    pub fn get_contract_source_mock(&mut self, deployer: &str, contract_name: &str) -> Mock {
        self.client
            .mock(
                "GET",
                format!("/v2/contracts/source/{deployer}/{contract_name}").as_str(),
            )
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(r#"{"source":"(define-read-only (noop) (ok true))","publish_height":1}"#)
            .create()
    }

    pub fn sbtc_balance_mock(&mut self, deployer: &str, balance: u128) -> Mock {
        let result = Value::okay(Value::UInt(balance)).unwrap();
        let encoded = bytes_to_hex(&result.serialize_to_vec().unwrap());
        self.client
            .mock(
                "POST",
                format!("/v2/contracts/call-read/{deployer}/sbtc-token/get-balance").as_str(),
            )
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(r#"{{"okay":true,"result":"0x{encoded}"}}"#))
            .create()
    }

    pub fn contract_source_not_found_mock(&mut self, deployer: &str, contract_name: &str) -> Mock {
        self.client
            .mock(
                "GET",
                format!("/v2/contracts/source/{deployer}/{contract_name}").as_str(),
            )
            .with_status(404)
            .with_body("No contract source data found")
            .create()
    }

    pub fn get_burn_block_mock(&mut self, burn_block_height: u64) -> Mock {
        self.client.mock("GET", format!("/extended/v2/burn-blocks/{burn_block_height}").as_str())
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(r#"{{"burn_block_time":1234567890,"burn_block_hash":"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef","burn_block_height":{burn_block_height}}}"#))
            .create()
    }

    /// Accepts a broadcast only when its body carries `amount` as a serialized
    /// Clarity uint, so a test can pin the value a contract call was given.
    pub fn tx_carrying_amount_mock(&mut self, amount: u128, tx_id: &str) -> Mock {
        let needle = Value::UInt(amount).serialize_to_vec().unwrap();
        self.client
            .mock("POST", "/v2/transactions")
            .match_request(move |request| {
                let body = request.body().unwrap();
                body.windows(needle.len()).any(|window| window == needle)
            })
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(r#""{tx_id}""#))
            .create()
    }

    pub fn get_tx_mock(&mut self, tx_id: &str) -> Mock {
        self.client
            .mock("POST", "/v2/transactions")
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(r#""{tx_id}""#))
            .create()
    }
}
