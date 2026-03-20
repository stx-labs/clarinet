use clarity::vm::diagnostic::{Diagnostic, Level};

use crate::analysis::linter::LintName;

/// Format the level string, including the lint name if provided.
///
/// With a lint name: `warning[unused_const]:`
/// Without: `warning:`
fn level_to_string(level: &Level, lint_name: Option<&LintName>) -> String {
    let level_str = match level {
        Level::Note => blue!("note"),
        Level::Warning => yellow!("warning"),
        Level::Error => red!("error"),
    };
    match lint_name {
        Some(name) => {
            use colored::Colorize;
            let styled_name = name.to_string().purple().bold();
            format!("{level_str}[{styled_name}]:")
        }
        None => format!("{level_str}:"),
    }
}

// Generate the formatted output for this diagnostic, given the source code.
// TODO: Preferably a filename would be saved in the Span, but for now, pass a name here.
pub fn output_diagnostic(
    diagnostic: &Diagnostic,
    name: &str,
    lines: &[String],
    lint_name: Option<&LintName>,
) -> Vec<String> {
    let mut output = Vec::new();

    if !diagnostic.spans.is_empty() {
        output.push(format!(
            "{}:{}:{}: {} {}",
            name, // diagnostic.spans[0].filename,
            diagnostic.spans[0].start_line,
            diagnostic.spans[0].start_column,
            level_to_string(&diagnostic.level, lint_name),
            diagnostic.message,
        ));
    } else {
        output.push(format!(
            "{} {}",
            level_to_string(&diagnostic.level, lint_name),
            diagnostic.message,
        ));
    }
    output.append(&mut output_code(diagnostic, lines));
    output
}

pub fn output_code(diagnostic: &Diagnostic, lines: &[String]) -> Vec<String> {
    let mut output = Vec::new();
    if diagnostic.spans.is_empty() {
        return output;
    }
    let span = &diagnostic.spans[0];
    let first_line = span.start_line.saturating_sub(1) as usize;

    output.push(lines[first_line].clone());
    let mut pointer = format!(
        "{: <1$}^",
        "",
        (span.start_column.saturating_sub(1)) as usize
    );
    if span.start_line == span.end_line {
        pointer = format!(
            "{}{:~<2$}",
            pointer,
            "",
            (span.end_column - span.start_column) as usize
        );
    }
    pointer = pointer.to_string();
    output.push(pointer);

    for span in diagnostic.spans.iter().skip(1) {
        let first_line = span.start_line.saturating_sub(1) as usize;
        let last_line = span.end_line.saturating_sub(1) as usize;

        output.push(lines[first_line].clone());
        let mut pointer = format!(
            "{: <1$}^",
            "",
            (span.start_column.saturating_sub(1)) as usize
        );
        if span.start_line == span.end_line {
            pointer = format!(
                "{}{:~<2$}",
                pointer,
                "",
                (span.end_column - span.start_column) as usize
            );
        } else {
            #[allow(clippy::needless_range_loop)]
            for line_num in (first_line + 1)..last_line {
                output.push(lines[line_num].clone());
            }
        }
        output.push(pointer);
    }
    output
}
