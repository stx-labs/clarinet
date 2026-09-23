use tokio::runtime::{Builder, Runtime};

/// Only makes progress while `block_on` polls it, so build one per blocking
/// thread: the main thread or a dedicated `thread_named` worker.
pub fn create_basic_runtime() -> Runtime {
    Builder::new_current_thread()
        .enable_all()
        .max_blocking_threads(32)
        .build()
        .expect("failed to build tokio runtime")
}
