use hashbrown::HashMap;
use js_sys::{Function as JsFunction, Promise};
use serde::{Deserialize, Serialize};
use serde_wasm_bindgen::{from_value as decode_from_js, to_value as encode_to_js};
use wasm_bindgen::JsValue;
use wasm_bindgen_futures::JsFuture;

use super::{FileAccessor, FileAccessorResult};

#[derive(Serialize, Deserialize)]
struct WFSRequest {
    pub path: String,
}

#[derive(Serialize, Deserialize)]
struct WFSRequestMany {
    pub paths: Vec<String>,
}

#[derive(Serialize, Deserialize)]
struct WFSWriteRequest<'a> {
    pub path: String,
    pub content: &'a [u8],
}

pub struct WASMFileSystemAccessor {
    client_request: JsFunction,
}

impl WASMFileSystemAccessor {
    pub fn new(client_request: JsFunction) -> WASMFileSystemAccessor {
        WASMFileSystemAccessor { client_request }
    }

    fn get_request_promise<T: Serialize>(
        &self,
        action: String,
        data: &T,
    ) -> FileAccessorResult<JsValue> {
        let request_promise = self.client_request.call2(
            &JsValue::NULL,
            &JsValue::from(action),
            &encode_to_js(data).unwrap(),
        );

        Box::pin(async move {
            match request_promise {
                Ok(promise) => match JsFuture::from(Promise::resolve(&promise)).await {
                    Ok(js_data) => Ok(js_data),
                    Err(err) => Err(format!("error: {:?}", &err)),
                },
                Err(err) => Err(format!("error: {:?}", &err)),
            }
        })
    }
}

impl FileAccessor for WASMFileSystemAccessor {
    fn file_exists(&self, path: String) -> FileAccessorResult<bool> {
        let file_exists_request =
            self.get_request_promise("vfs/exists".into(), &WFSRequest { path });

        Box::pin(async move {
            file_exists_request
                .await
                .and_then(|r| decode_from_js(r).map_err(|err| err.to_string()))
        })
    }

    fn read_file(&self, path: String) -> FileAccessorResult<String> {
        let read_file_promise =
            self.get_request_promise("vfs/readFile".into(), &WFSRequest { path });

        Box::pin(async move {
            read_file_promise
                .await
                .and_then(|r| decode_from_js(r).map_err(|err| err.to_string()))
        })
    }

    fn read_files(
        &self,
        contracts_paths: Vec<String>,
    ) -> FileAccessorResult<HashMap<String, String>> {
        let read_contract_promise = self.get_request_promise(
            "vfs/readFiles".into(),
            &WFSRequestMany {
                paths: contracts_paths,
            },
        );

        Box::pin(async move {
            read_contract_promise
                .await
                .and_then(|r| decode_from_js(r).map_err(|err| err.to_string()))
        })
    }

    fn write_file(&self, location: String, content: &[u8]) -> FileAccessorResult<()> {
        let write_file_promise = self.get_request_promise(
            "vfs/writeFile".into(),
            &WFSWriteRequest {
                path: location,
                content,
            },
        );

        Box::pin(async move { write_file_promise.await.map(|_| ()) })
    }
}
