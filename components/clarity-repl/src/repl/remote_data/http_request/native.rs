use std::sync::LazyLock;
use std::time::Duration;

use reqwest::blocking::Client;
use reqwest::header::HeaderMap;
use reqwest::StatusCode;
use serde::de::DeserializeOwned;

static API_KEY: LazyLock<Option<String>> = LazyLock::new(|| std::env::var("HIRO_API_KEY").ok());

const MAX_RETRY_ATTEMPTS: u32 = 3;

fn get_uint_header_value(headers: &HeaderMap, header_name: &str) -> Option<u32> {
    headers
        .get(header_name)
        .and_then(|h| h.to_str().ok())
        .and_then(|s| s.parse().ok())
}

fn handle_response<T: DeserializeOwned>(
    response: reqwest::blocking::Response,
) -> Result<T, String> {
    let status = response.status();

    if status.is_success() {
        return response.json::<T>().map_err(|e| e.to_string());
    }

    let msg = response
        .text()
        .unwrap_or("Unable to read response body".to_string());
    Err(format!("http error - status: {status} - message: {msg}"))
}

fn is_rate_limited(headers: &HeaderMap) -> (bool, Option<u32>) {
    let remaining = get_uint_header_value(headers, "ratelimit-remaining");
    // make sure the retry_after is at most 60 seconds
    let retry_after = get_uint_header_value(headers, "retry-after").map(|v| v.min(60));
    (matches!(remaining, Some(0)), retry_after)
}

pub fn http_request<T: DeserializeOwned>(url: &str) -> Result<T, String> {
    let client = Client::new();

    let mut attempts = 0;
    loop {
        let mut request = client
            .get(url)
            .header("x-hiro-product", "clarinet-cli")
            .header("Accept", "application/json");

        if let Some(api_key) = API_KEY.as_ref() {
            request = request.header("x-api-key", api_key);
        }

        let response = request.send().map_err(|e| e.to_string())?;
        let status = response.status();

        if status.is_success() {
            return response.json::<T>().map_err(|e| e.to_string());
        }

        // Retry on server errors (5xx) and rate limits (429)
        let is_retryable = status.is_server_error() || status == StatusCode::TOO_MANY_REQUESTS;
        if !is_retryable {
            return handle_response(response);
        }

        attempts += 1;
        if attempts >= MAX_RETRY_ATTEMPTS {
            return handle_response(response);
        }

        if status == StatusCode::TOO_MANY_REQUESTS {
            let headers = response.headers().clone();
            let (is_rate_limited, retry_after) = is_rate_limited(&headers);
            if !is_rate_limited {
                return handle_response(response);
            }
            let retry_delay = retry_after.unwrap_or(1);
            uprint!("Rate limited, retrying after {retry_delay} seconds...\n");
            std::thread::sleep(Duration::from_secs(retry_delay as u64));
        } else {
            uprint!("Server error ({status}), retrying in 3 seconds...\n");
            std::thread::sleep(Duration::from_secs(3));
        }
    }
}
