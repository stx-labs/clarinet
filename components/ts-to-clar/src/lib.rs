mod converter;
mod expression_converter;
mod helper;
mod parser;
mod types;

// This module provides a transpiler that converts TypeScript into Clarity code.
// 1. It parses the TypeScript code into an Intermediate Representation (IR) using SWC
// 2. Transform the IR into a Clarity AST (PSEs)
// 3. Format the PSEs into Clarity code using Clarinet Format

use clarinet_format::formatter::{self, ClarityFormatter};
use oxc_allocator::Allocator;

use crate::converter::convert;
use crate::parser::get_ir;

pub use self::helper::to_kebab_case;

pub fn transpile(file_name: &str, src: &str) -> Result<String, anyhow::Error> {
    let allocator = Allocator::default();
    let ir = get_ir(&allocator, file_name, src);
    let pses = convert(&allocator, ir)?;
    let formatter = ClarityFormatter::new(formatter::Settings::default());
    Ok(formatter.format_ast(&pses))
}

#[cfg(test)]
mod test {
    use indoc::indoc;

    use super::*;

    #[test]
    fn test_transpile_toplevel_data() {
        let src = indoc! {
            "const OWNER_ROLE = new Constant<Uint>(1);
            const count = new DataVar<Uint>(0);
            const msgs = new DataMap<Uint, StringAscii<16>>();
        "};
        let clarity_code = transpile("test.clar.ts", src).unwrap();

        assert_eq!(
            clarity_code,
            "(define-const OWNER_ROLE u1)\n(define-data-var count uint u0)\n(define-data-map msgs uint (string-ascii 16))"
        );
    }
}
