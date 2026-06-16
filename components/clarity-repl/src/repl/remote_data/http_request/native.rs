use std::collections::HashMap;
use std::sync::LazyLock;
use std::time::Duration;

use reqwest::blocking::Client;
use serde::de::DeserializeOwned;

use super::retry::{self, Response};

// Matches FETCH_TIMEOUT_MS on the Node-WASM side so the two paths can't
// silently diverge on hang behaviour.
const REQUEST_TIMEOUT: Duration = Duration::from_secs(30);

// Reused across calls so the connection pool and TLS sessions can be kept warm.
static CLIENT: LazyLock<Client> = LazyLock::new(|| {
    Client::builder()
        .timeout(REQUEST_TIMEOUT)
        .build()
        .expect("failed to build HTTP client")
});

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
    let transport = || -> Result<Response, String> {
        let mut request = CLIENT
            .get(url)
            .header("x-hiro-product", "clarinet-cli")
            .header("Accept", "application/json");

        // Read per-request so callers can set HIRO_API_KEY after first use
        // (matches the Node-WASM glue's behaviour).
        if let Ok(api_key) = std::env::var("HIRO_API_KEY") {
            request = request.header("x-api-key", api_key);
        }

        let response = request.send().map_err(|e| e.to_string())?;

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
