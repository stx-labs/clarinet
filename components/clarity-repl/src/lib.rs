#[macro_use]
extern crate hiro_system_kit;

#[macro_use]
mod uprint;

pub mod analysis;

pub mod repl;
pub mod utils;

#[cfg(test)]
pub mod test_fixtures;

#[cfg(not(target_arch = "wasm32"))]
pub mod frontend;
