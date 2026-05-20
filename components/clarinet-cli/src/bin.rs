use clarinet_lib::frontend::cli;
#[cfg(not(any(
    target_env = "msvc",
    target_os = "macos",
    all(target_arch = "aarch64", target_env = "musl")
)))]
use tikv_jemallocator::Jemalloc;

/// Enable jemalloc as the global allocator
/// Disable for...
///  - MSVC compiler: Not supported by `jemalloc`
///  - MacOS: System allocator already based on jemalloc
///  - aarch64-musl: `jemalloc` upstream has no atomics implementation for this
///    target combo, alls back to musl's system allocator.
#[cfg(not(any(
    target_env = "msvc",
    target_os = "macos",
    all(target_arch = "aarch64", target_env = "musl")
)))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

pub fn main() {
    cli::main();
}
