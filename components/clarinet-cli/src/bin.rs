extern crate serde;

#[macro_use]
extern crate serde_json;

#[macro_use]
extern crate hiro_system_kit;

mod deployments;
mod devnet;
mod frontend;
mod generate;
mod lsp;

use frontend::cli;
#[cfg(not(target_env = "msvc"))]
use tikv_jemallocator::Jemalloc;

/// Enable jemalloc as the global allocator (except in Windows)
#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

pub fn main() {
    cli::main();
}
