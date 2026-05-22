use std::collections::HashMap;

use serde::de::DeserializeOwned;

pub struct Response {
    pub status: u16,
    pub status_text: String,
    pub headers: HashMap<String, String>,
    pub body: String,
}

pub enum TransportError {
    Network(String),
}

const MAX_RETRY_ATTEMPTS: u32 = 3;

fn get_header_ci<'a>(headers: &'a HashMap<String, String>, name: &str) -> Option<&'a str> {
    headers
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case(name))
        .map(|(_, v)| v.as_str())
}

fn get_uint_header(headers: &HashMap<String, String>, name: &str) -> Option<u32> {
    get_header_ci(headers, name).and_then(|v| v.parse().ok())
}

fn is_rate_limited(headers: &HashMap<String, String>) -> (bool, Option<u32>) {
    let remaining = get_uint_header(headers, "ratelimit-remaining");
    // cap retry_after at 60 seconds
    let retry_after = get_uint_header(headers, "retry-after").map(|v| v.min(60));
    (matches!(remaining, Some(0)), retry_after)
}

fn format_http_error(response: &Response) -> String {
    let status_line = if response.status_text.is_empty() {
        response.status.to_string()
    } else {
        format!("{} {}", response.status, response.status_text)
    };
    let msg = if response.body.is_empty() {
        "Unable to read response body".to_string()
    } else {
        response.body.clone()
    };
    format!("http error - status: {status_line} - message: {msg}")
}

/// Shared retry loop. `transport` performs a single HTTP GET. `sleep` blocks for the
/// given number of milliseconds (must work in the current thread, including wasm).
///
/// Retry policy (mirrors the original native.rs behaviour):
/// - Up to 3 attempts total.
/// - 5xx → sleep 3s and retry.
/// - 429 → respect retry-after header (capped at 60s); fall back to 1s only when
///   ratelimit-remaining == 0, otherwise treat as non-retryable.
/// - Everything else non-2xx → non-retryable, formatted error returned.
pub fn run_with_retry<T, F, S>(transport: F, mut sleep: S) -> Result<T, String>
where
    T: DeserializeOwned,
    F: Fn() -> Result<Response, TransportError>,
    S: FnMut(u32),
{
    let mut attempts: u32 = 0;
    loop {
        let response = match transport() {
            Ok(r) => r,
            Err(TransportError::Network(e)) => return Err(e),
        };
        let status = response.status;

        if (200..300).contains(&status) {
            return serde_json::from_str(&response.body).map_err(|e| e.to_string());
        }

        let is_retryable = (500..600).contains(&status) || status == 429;
        if !is_retryable {
            return Err(format_http_error(&response));
        }

        attempts += 1;
        if attempts >= MAX_RETRY_ATTEMPTS {
            return Err(format_http_error(&response));
        }

        if status == 429 {
            let (rate_limited, retry_after) = is_rate_limited(&response.headers);
            if !rate_limited {
                return Err(format_http_error(&response));
            }
            let retry_delay = retry_after.unwrap_or(1);
            uprint!("Rate limited, retrying after {retry_delay} seconds...\n");
            sleep(retry_delay.saturating_mul(1000));
        } else {
            let status_line = if response.status_text.is_empty() {
                status.to_string()
            } else {
                format!("{status} {}", response.status_text)
            };
            uprint!("Server error ({status_line}), retrying in 3 seconds...\n");
            sleep(3000);
        }
    }
}
