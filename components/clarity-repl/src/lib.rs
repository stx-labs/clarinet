#[macro_use]
extern crate hiro_system_kit;

// `pub` only so the exported macros can reach `_print`/`_eprint` via `$crate::uprint::…`.
#[doc(hidden)]
#[macro_use]
pub mod uprint;

pub mod analysis;

pub mod repl;
pub mod utils;

#[cfg(test)]
pub mod test_fixtures;

#[cfg(not(target_arch = "wasm32"))]
pub mod frontend;
