use tokio::runtime::{Builder, Runtime};

/// The runtime Clarinet's blocking entry points drive their futures on: a
/// single-threaded scheduler with the io and time drivers enabled.
///
/// Meant to be owned by the thread that blocks on it — either the main thread
/// or a dedicated `thread_named` worker — and built once per thread.
pub fn create_basic_runtime() -> Runtime {
    Builder::new_current_thread()
        .enable_all()
        .max_blocking_threads(32)
        .build()
        .expect("failed to build tokio runtime")
}
