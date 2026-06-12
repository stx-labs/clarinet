use std::collections::HashMap;

use serde::Deserialize;
use serde::de::DeserializeOwned;

#[derive(Deserialize)]
pub struct Response {
    pub status: u16,
    #[serde(rename = "statusText", default)]
    pub status_text: String,
    #[serde(default)]
    pub headers: HashMap<String, String>,
    #[serde(default)]
    pub body: String,
}

const MAX_RETRY_ATTEMPTS: u32 = 3;

fn get_uint_header_ci(headers: &HashMap<String, String>, name: &str) -> Option<u32> {
    headers
        .iter()
        .find(|(k, _)| k.eq_ignore_ascii_case(name))
        .and_then(|(_, v)| v.parse().ok())
}

fn status_line(status: u16, status_text: &str) -> String {
    if status_text.is_empty() {
        status.to_string()
    } else {
        format!("{status} {status_text}")
    }
}

fn format_http_error(response: &Response) -> String {
    let line = status_line(response.status, &response.status_text);
    let msg = if response.body.is_empty() {
        "Unable to read response body"
    } else {
        &response.body
    };
    format!("http error - status: {line} - message: {msg}")
}

fn truncate_body(body: &str) -> String {
    let cut = body
        .char_indices()
        .nth(512)
        .map(|(i, _)| i)
        .unwrap_or(body.len());
    if cut < body.len() {
        format!("{}…", &body[..cut])
    } else {
        body.to_string()
    }
}

/// Shared retry loop. `transport` performs a single HTTP GET; transport errors
/// (DNS, connection refused, per-attempt timeout, malformed response) are
/// returned as-is and not retried — the caller's first attempt is the only
/// attempt. `sleep` blocks for the given number of milliseconds (must work in
/// the current thread, including wasm).
///
/// Retry policy:
/// - Up to 3 attempts total for retryable HTTP statuses.
/// - 5xx → sleep 3s and retry.
/// - 429 → only retry when ratelimit-remaining == 0; sleep retry-after seconds
///   (capped at 60), defaulting to 1s if the header is absent.
/// - Everything else non-2xx → non-retryable, formatted error returned.
pub fn run_with_retry<T, F, S>(transport: F, mut sleep: S) -> Result<T, String>
where
    T: DeserializeOwned,
    F: Fn() -> Result<Response, String>,
    S: FnMut(u32),
{
    let mut attempts: u32 = 0;
    loop {
        let response = transport()?;
        let status = response.status;

        if (200..300).contains(&status) {
            return serde_json::from_str(&response.body).map_err(|e| {
                let preview = truncate_body(&response.body);
                format!("failed to parse JSON response: {e} — body: {preview}")
            });
        }

        let is_retryable = (500..600).contains(&status) || status == 429;
        if !is_retryable {
            return Err(format_http_error(&response));
        }

        attempts += 1;
        if attempts >= MAX_RETRY_ATTEMPTS {
            return Err(format_http_error(&response));
        }

        let delay_ms = if status == 429 {
            let remaining = get_uint_header_ci(&response.headers, "ratelimit-remaining");
            if !matches!(remaining, Some(0)) {
                return Err(format_http_error(&response));
            }
            let retry_after = get_uint_header_ci(&response.headers, "retry-after")
                .map(|v| v.min(60))
                .unwrap_or(1);
            uprint!("Rate limited, retrying after {retry_after} seconds...\n");
            retry_after.saturating_mul(1000)
        } else {
            let line = status_line(status, &response.status_text);
            uprint!("Server error ({line}), retrying in 3 seconds...\n");
            3000
        };
        sleep(delay_ms);
    }
}
