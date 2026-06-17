use serde::de::DeserializeOwned;
use wasm_bindgen::prelude::*;

use super::retry::{self, Response};

#[wasm_bindgen(module = "/src/repl/remote_data/http_request/sync_http.cjs")]
extern "C" {
    #[wasm_bindgen(js_name = syncHttpRequest, catch)]
    fn js_sync_http_request(url: &str, headers_json: &str) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(js_name = syncSleep)]
    fn js_sync_sleep(ms: u32);
}

fn js_error_to_string(err: JsValue) -> String {
    if let Some(s) = err.as_string() {
        return s;
    }
    if let Some(msg) = js_sys::Reflect::get(&err, &JsValue::from_str("message"))
        .ok()
        .and_then(|v| v.as_string())
    {
        return msg;
    }
    format!("{err:?}")
}

pub fn http_request<T: DeserializeOwned>(url: &str) -> Result<T, String> {
    let transport = || -> Result<Response, String> {
        // Rust-side headers; the JS glue additionally injects HIRO_API_KEY as x-api-key.
        let headers_json = r#"{"Accept":"application/json","x-hiro-product":"clarinet-sdk"}"#;

        // The JS glue throws on transport failure (the body_len < 0 path in
        // sync_http.cjs), so anything returned here is a real HTTP response.
        let response_value = js_sync_http_request(url, headers_json).map_err(js_error_to_string)?;

        serde_wasm_bindgen::from_value::<Response>(response_value)
            .map_err(|e| format!("malformed worker response: {e}"))
    };

    let sleep = |ms: u32| js_sync_sleep(ms);

    retry::run_with_retry(transport, sleep)
}
