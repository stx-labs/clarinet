// TODO: the browser (`web`) path has its own fetch-based implementation and
// does not share this retry policy yet — the native/Node parity guarantee
// doesn't extend to it.
#[cfg(not(all(target_arch = "wasm32", feature = "web")))]
mod retry;

#[cfg(not(target_arch = "wasm32"))]
mod native;
#[cfg(not(target_arch = "wasm32"))]
pub use native::http_request;

#[cfg(all(target_arch = "wasm32", feature = "web"))]
mod browser;
#[cfg(all(target_arch = "wasm32", feature = "web"))]
pub use browser::http_request;

#[cfg(all(target_arch = "wasm32", not(feature = "web")))]
mod node;
#[cfg(all(target_arch = "wasm32", not(feature = "web")))]
pub use node::http_request;
