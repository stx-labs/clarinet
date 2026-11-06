mod common;

use clarinet_format::formatter::Settings;
use common::check_entry_points;

#[test]
fn incomplete_special_forms_preserve_source() {
    for source in [
        "(let)",
        "(match)",
        "(define-map)",
        "(restrict-assets?)",
        "(restrict-assets? tx-sender)",
        "(let not-bindings u1)",
        "(restrict-assets? tx-sender u1 (ok true))",
        "(ok (let))",
        "(some (match))",
        "(some (restrict-assets? tx-sender))",
        "(some (let not-bindings (match)))",
    ] {
        check_entry_points(source, Settings::default(), |mode, formatted, _| {
            assert_eq!(formatted.trim_end(), source, "{mode}: {source}");
        });
    }
}
