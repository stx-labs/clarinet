use clarity::vm::representations::PreSymbolicExpression;

/// Extract the original source text using its span
/// This is useful for strings where we don't want to touch the formatting
pub fn extract_expr_source(expr: &PreSymbolicExpression, source: &str) -> String {
    let span = &expr.span;
    let start_line = span.start_line as usize;

    if let Some(line) = source.lines().nth(start_line - 1) {
        let start_col = (span.start_column as usize).saturating_sub(1);
        let end_col = span.end_column as usize;

        if start_col < line.len() && end_col <= line.len() {
            return line[start_col..end_col].to_string();
        }
    }

    String::new()
}

pub fn ignored_exprs(exprs: &[PreSymbolicExpression], source: &str) -> String {
    let start = exprs.first().unwrap().span();
    let end = exprs.last().unwrap().span();

    let start_line = start.start_line as usize;
    let end_line = end.end_line as usize;

    let mut result = String::new();
    let mut is_first = true;

    for (idx, line) in source
        .lines()
        .skip(start_line - 1)
        .take(end_line - start_line + 1)
        .enumerate()
    {
        if !is_first {
            result.push('\n');
        }

        if idx == 0 {
            // First line (the one with the opening parenthesis)
            if let Some(paren_pos) = line.find('(') {
                result.push_str(&line[paren_pos..]);
            }
        } else if idx == end_line - start_line {
            // Last line - up to and including end column
            let end_column = end.end_column as usize;
            if end_column <= line.len() {
                result.push_str(&line[..end_column]);
            } else {
                result.push_str(line);
            }
        } else {
            // Middle lines - complete line
            result.push_str(line);
        }

        is_first = false;
    }

    result
}
