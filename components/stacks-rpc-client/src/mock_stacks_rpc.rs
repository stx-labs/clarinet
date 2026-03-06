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

    pub fn get_burn_block_mock(&mut self, burn_block_height: u64) -> Mock {
        self.client.mock("GET", format!("/extended/v2/burn-blocks/{burn_block_height}").as_str())
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(format!(r#"{{"burn_block_time":1234567890,"burn_block_hash":"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef","burn_block_height":{burn_block_height}}}"#))
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
