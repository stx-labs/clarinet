use std::collections::HashMap;

use js_sys::Reflect;
use serde::de::DeserializeOwned;
use wasm_bindgen::prelude::*;

use super::retry::{self, Response, TransportError};

#[wasm_bindgen(module = "/src/repl/remote_data/http_request/sync_http.cjs")]
extern "C" {
    #[wasm_bindgen(js_name = syncHttpRequest, catch)]
    fn js_sync_http_request(url: &str, headers_json: &str) -> Result<JsValue, JsValue>;

    #[wasm_bindgen(js_name = syncSleep)]
    fn js_sync_sleep(ms: u32);
}

fn js_string_field(obj: &JsValue, key: &str) -> Option<String> {
    Reflect::get(obj, &JsValue::from_str(key))
        .ok()
        .and_then(|v| v.as_string())
}

fn js_number_field(obj: &JsValue, key: &str) -> Option<f64> {
    Reflect::get(obj, &JsValue::from_str(key))
        .ok()
        .and_then(|v| v.as_f64())
}

fn js_headers_to_map(obj: &JsValue, key: &str) -> HashMap<String, String> {
    let headers_value = match Reflect::get(obj, &JsValue::from_str(key)) {
        Ok(v) if !v.is_undefined() && !v.is_null() => v,
        _ => return HashMap::new(),
    };
    let headers_obj: &js_sys::Object = headers_value.unchecked_ref();
    let keys = js_sys::Object::keys(headers_obj);
    let mut map = HashMap::with_capacity(keys.length() as usize);
    for k in keys.iter() {
        let key_str = match k.as_string() {
            Some(s) => s,
            None => continue,
        };
        let value = match Reflect::get(&headers_value, &k) {
            Ok(v) => v,
            Err(_) => continue,
        };
        if let Some(value_str) = value.as_string() {
            map.insert(key_str, value_str);
        }
    }
    map
}

fn js_error_to_string(err: JsValue) -> String {
    if let Some(s) = err.as_string() {
        return s;
    }
    if let Some(s) = js_string_field(&err, "message") {
        return s;
    }
    format!("{err:?}")
}

pub fn http_request<T: DeserializeOwned>(url: &str) -> Result<T, String> {
    let transport = || -> Result<Response, TransportError> {
        // Rust-side headers; the JS glue additionally injects HIRO_API_KEY as x-api-key.
        let headers_json =
            r#"{"Accept":"application/json","x-hiro-product":"clarinet-sdk"}"#.to_string();

        let response_obj = js_sync_http_request(url, &headers_json)
            .map_err(|e| TransportError::Network(js_error_to_string(e)))?;

        let status = js_number_field(&response_obj, "status")
            .map(|n| n as i32)
            .unwrap_or(0);
        if status < 0 {
            let body = js_string_field(&response_obj, "body").unwrap_or_default();
            return Err(TransportError::Network(body));
        }
        let status = status as u16;
        let status_text = js_string_field(&response_obj, "statusText").unwrap_or_default();
        let headers = js_headers_to_map(&response_obj, "headers");
        let body = js_string_field(&response_obj, "body").unwrap_or_default();

        Ok(Response {
            status,
            status_text,
            headers,
            body,
        })
    };

    let sleep = |ms: u32| js_sync_sleep(ms);

    retry::run_with_retry(transport, sleep)
}
