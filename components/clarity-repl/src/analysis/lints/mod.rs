mod case_const;
mod error_const;
mod noop;
mod panic;
mod unnecessary_as_max_len;
mod unnecessary_public;
mod unused_binding;
mod unused_const;
mod unused_data_var;
mod unused_map;
mod unused_private_fn;
mod unused_token;
mod unused_trait;

pub use case_const::CaseConst;
pub use error_const::ErrorConst;
pub use noop::NoopChecker;
pub use panic::PanicChecker;
pub use unnecessary_as_max_len::UnnecessaryAsMaxLen;
pub use unnecessary_public::UnnecessaryPublic;
pub use unused_binding::UnusedBinding;
pub use unused_const::UnusedConst;
pub use unused_data_var::UnusedDataVar;
pub use unused_map::UnusedMap;
pub use unused_private_fn::UnusedPrivateFn;
pub use unused_token::UnusedToken;
pub use unused_trait::UnusedTrait;

#[cfg(test)]
mod tests {
    use clarity_types::diagnostic::Level;
    use indoc::indoc;

    use super::{CaseConst, UnusedConst};
    use crate::analysis::linter::{Lint};
    use crate::repl::session::Session;
    use crate::repl::SessionSettings;

    #[test]
    fn allow_with_annotation_at_error_level() {
        #[rustfmt::skip]
        let snippet = indoc!("
            ;; #[allow(unused_const, case_const)]
            (define-constant this_is_regular_snake_case u100)
        ").to_string();

        let mut settings = SessionSettings::default();
        settings.repl_settings.analysis.disable_all_lints();
        settings
            .repl_settings
            .analysis
            .set_lint_level(UnusedConst::get_name(),Level::Error);
        settings
            .repl_settings
            .analysis
            .set_lint_level(CaseConst::get_name(), Level::Error);

        let (_, result) = Session::new_without_boot_contracts(settings)
            .formatted_interpretation(snippet, Some("checker".to_string()), false, None)
            .expect("Invalid code snippet");

        assert_eq!(result.diagnostics.len(), 0);
    }
}
