use std::collections::HashMap;
use std::sync::LazyLock;
use std::time::Duration;

use reqwest::blocking::Client;
use serde::de::DeserializeOwned;

use super::retry::{self, Response, TransportError};

static API_KEY: LazyLock<Option<String>> = LazyLock::new(|| std::env::var("HIRO_API_KEY").ok());

fn collect_headers(headers: &reqwest::header::HeaderMap) -> HashMap<String, String> {
    headers
        .iter()
        .filter_map(|(k, v)| {
            v.to_str()
                .ok()
                .map(|s| (k.as_str().to_string(), s.to_string()))
        })
        .collect()
}

pub fn http_request<T: DeserializeOwned>(url: &str) -> Result<T, String> {
    let client = Client::new();

    let transport = || -> Result<Response, TransportError> {
        let mut request = client
            .get(url)
            .header("x-hiro-product", "clarinet-cli")
            .header("Accept", "application/json");

        if let Some(api_key) = API_KEY.as_ref() {
            request = request.header("x-api-key", api_key);
        }

        let response = request
            .send()
            .map_err(|e| TransportError::Network(e.to_string()))?;

        let status = response.status();
        let status_text = status.canonical_reason().unwrap_or("").to_string();
        let headers = collect_headers(response.headers());
        let body = response
            .text()
            .unwrap_or_else(|_| "Unable to read response body".to_string());

        Ok(Response {
            status: status.as_u16(),
            status_text,
            headers,
            body,
        })
    };

    let sleep = |ms: u32| std::thread::sleep(Duration::from_millis(ms as u64));

    retry::run_with_retry(transport, sleep)
}
