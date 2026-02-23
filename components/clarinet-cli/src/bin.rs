#[macro_use]
extern crate hiro_system_kit;

mod deployments;
mod devnet;
mod frontend;
mod generate;
mod lsp;

use frontend::cli;
#[cfg(not(any(target_env = "msvc", target_os = "macos")))]
use tikv_jemallocator::Jemalloc;

/// Enable jemalloc as the global allocator
/// Disable for...
///  - MSVC compiler: Not supported by `jemalloc`
///  - MacOS: System allocator already based on jemalloc
#[cfg(not(any(target_env = "msvc", target_os = "macos")))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

pub fn main() {
    cli::main();
}
