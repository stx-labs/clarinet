use std::collections::HashMap;

use serde::de::DeserializeOwned;
use serde::Deserialize;

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

#[cfg(test)]
mod tests {
    use std::cell::{Cell, RefCell};

    use super::*;

    fn response(status: u16, headers: &[(&str, &str)], body: &str) -> Response {
        Response {
            status,
            status_text: String::new(),
            headers: headers
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
            body: body.to_string(),
        }
    }

    /// Feeds `run_with_retry` a scripted sequence of transport results and
    /// records how many attempts were made and which sleeps were requested.
    fn run_scripted(
        script: Vec<Result<Response, String>>,
    ) -> (Result<serde_json::Value, String>, usize, Vec<u32>) {
        let script = RefCell::new(script);
        let calls = Cell::new(0);
        let sleeps = RefCell::new(Vec::new());
        let result = run_with_retry(
            || {
                calls.set(calls.get() + 1);
                script.borrow_mut().remove(0)
            },
            |ms| sleeps.borrow_mut().push(ms),
        );
        (result, calls.get(), sleeps.into_inner())
    }

    #[test]
    fn success_returns_parsed_json() {
        let (result, calls, sleeps) = run_scripted(vec![Ok(response(200, &[], r#"{"ok":true}"#))]);
        assert_eq!(result.unwrap(), serde_json::json!({"ok": true}));
        assert_eq!(calls, 1);
        assert!(sleeps.is_empty());
    }

    #[test]
    fn server_error_retries_up_to_three_attempts() {
        let (result, calls, sleeps) = run_scripted(vec![
            Ok(response(500, &[], "boom")),
            Ok(response(502, &[], "boom")),
            Ok(response(503, &[], "boom")),
        ]);
        assert_eq!(
            result.unwrap_err(),
            "http error - status: 503 - message: boom"
        );
        assert_eq!(calls, 3);
        assert_eq!(sleeps, vec![3000, 3000]);
    }

    #[test]
    fn recovers_after_transient_server_error() {
        let (result, calls, sleeps) = run_scripted(vec![
            Ok(response(500, &[], "boom")),
            Ok(response(200, &[], r#"{"ok":true}"#)),
        ]);
        assert!(result.is_ok());
        assert_eq!(calls, 2);
        assert_eq!(sleeps, vec![3000]);
    }

    #[test]
    fn rate_limit_honors_retry_after() {
        let (result, calls, sleeps) = run_scripted(vec![
            Ok(response(
                429,
                &[("ratelimit-remaining", "0"), ("retry-after", "2")],
                "",
            )),
            Ok(response(200, &[], r#"{"ok":true}"#)),
        ]);
        assert!(result.is_ok());
        assert_eq!(calls, 2);
        assert_eq!(sleeps, vec![2000]);
    }

    #[test]
    fn rate_limit_retry_after_is_capped_at_60s() {
        let (_, _, sleeps) = run_scripted(vec![
            Ok(response(
                429,
                &[("ratelimit-remaining", "0"), ("retry-after", "120")],
                "",
            )),
            Ok(response(200, &[], r#"{"ok":true}"#)),
        ]);
        assert_eq!(sleeps, vec![60_000]);
    }

    #[test]
    fn rate_limit_defaults_to_1s_without_retry_after() {
        let (_, _, sleeps) = run_scripted(vec![
            Ok(response(429, &[("ratelimit-remaining", "0")], "")),
            Ok(response(200, &[], r#"{"ok":true}"#)),
        ]);
        assert_eq!(sleeps, vec![1000]);
    }

    #[test]
    fn rate_limit_headers_are_matched_case_insensitively() {
        let (result, calls, _) = run_scripted(vec![
            Ok(response(
                429,
                &[("RateLimit-Remaining", "0"), ("Retry-After", "1")],
                "",
            )),
            Ok(response(200, &[], r#"{"ok":true}"#)),
        ]);
        assert!(result.is_ok());
        assert_eq!(calls, 2);
    }

    #[test]
    fn rate_limit_without_remaining_zero_is_not_retried() {
        let (result, calls, sleeps) = run_scripted(vec![Ok(response(429, &[], "slow down"))]);
        assert_eq!(
            result.unwrap_err(),
            "http error - status: 429 - message: slow down"
        );
        assert_eq!(calls, 1);
        assert!(sleeps.is_empty());
    }

    #[test]
    fn non_retryable_status_returns_formatted_error() {
        let mut response = response(404, &[], "nope");
        response.status_text = "Not Found".to_string();
        let (result, calls, _) = run_scripted(vec![Ok(response)]);
        assert_eq!(
            result.unwrap_err(),
            "http error - status: 404 Not Found - message: nope"
        );
        assert_eq!(calls, 1);
    }

    #[test]
    fn transport_error_propagates_without_retry() {
        let (result, calls, sleeps) = run_scripted(vec![Err("connection refused".to_string())]);
        assert_eq!(result.unwrap_err(), "connection refused");
        assert_eq!(calls, 1);
        assert!(sleeps.is_empty());
    }

    #[test]
    fn json_parse_failure_truncates_body_preview() {
        let body = "é".repeat(600);
        let (result, _, _) = run_scripted(vec![Ok(response(200, &[], &body))]);
        let err = result.unwrap_err();
        assert!(err.starts_with("failed to parse JSON response:"));
        // 512 chars + ellipsis, cut on a char boundary despite multi-byte input
        assert!(err.ends_with(&format!("{}…", "é".repeat(512))));
    }
}
