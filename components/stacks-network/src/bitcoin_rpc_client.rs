use std::sync::mpsc::Sender;
use std::time::Duration;

use crate::event::DevnetEvent;

pub(crate) struct BitcoinRpcClient {
    client: reqwest::Client,
    url: String,
    username: String,
    password: String,
    host: String,
}

impl BitcoinRpcClient {
    pub(crate) fn new(url: String, username: String, password: String) -> Self {
        let host = url.strip_prefix("http://").unwrap_or(&url).to_string();
        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(30))
            .build()
            .expect("Unable to build http client");
        Self {
            client,
            url,
            username,
            password,
            host,
        }
    }

    pub(crate) fn request(&self) -> reqwest::RequestBuilder {
        self.client
            .post(&self.url)
            .timeout(Duration::from_secs(3))
            .basic_auth(&self.username, Some(&self.password))
            .header("Content-Type", "application/json")
            .header("Host", &self.host)
    }

    pub(crate) async fn call(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<reqwest::Response, reqwest::Error> {
        self.request()
            .json(&serde_json::json!({
                "jsonrpc": "1.0",
                "id": "stacks-network",
                "method": method,
                "params": params,
            }))
            .send()
            .await
    }

    pub(crate) async fn call_with_retry(
        &self,
        method: &str,
        params: serde_json::Value,
        max_errors: u32,
        event_tx: &Sender<DevnetEvent>,
    ) -> Result<(), String> {
        let mut error_count = 0;
        loop {
            let Err(e) = self.call(method, params.clone()).await else {
                return Ok(());
            };

            error_count += 1;
            if error_count > max_errors {
                return Err(e.to_string());
            }
            if error_count > 1 {
                let _ = event_tx.send(DevnetEvent::error(e.to_string()));
            }
            tokio::time::sleep(Duration::from_millis(500)).await;
            let _ = event_tx.send(DevnetEvent::info("Waiting for bitcoin-node".to_string()));
        }
    }
}
