//! Universal print macros.
//!
//! The platform split lives in `_print`/`_eprint` rather than in the macro expansion
//! (as `std`'s `println!` does with `io::_print`), so `web-sys` stays an implementation
//! detail of this crate and callers need no wasm-only dependency of their own.

use std::fmt::Arguments;

pub fn _print(args: Arguments<'_>) {
    #[cfg(not(target_arch = "wasm32"))]
    println!("{args}");
    #[cfg(target_arch = "wasm32")]
    web_sys::console::log_1(&args.to_string().into());
}

pub fn _eprint(args: Arguments<'_>) {
    #[cfg(not(target_arch = "wasm32"))]
    eprintln!("{args}");
    #[cfg(target_arch = "wasm32")]
    web_sys::console::error_1(&args.to_string().into());
}

/// Print a line to stdout (native) or `console.log` (wasm32).
#[macro_export]
macro_rules! uprint {
    ( $( $t:tt )* ) => {
        $crate::uprint::_print(format_args!( $( $t )* ))
    };
}

/// Print a line to stderr (native) or `console.error` (wasm32).
#[macro_export]
macro_rules! ueprint {
    ( $( $t:tt )* ) => {
        $crate::uprint::_eprint(format_args!( $( $t )* ))
    };
}
