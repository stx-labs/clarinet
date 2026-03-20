use clarinet_lib::frontend::cli;
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
