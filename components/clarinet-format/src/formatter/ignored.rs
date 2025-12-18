use clarity::vm::representations::PreSymbolicExpression;

fn u32_to_usize(val: u32) -> usize {
    usize::try_from(val).unwrap()
}

/// spans use 1-based index, this const is largely only for this comment
const COLUMN_TO_INDEX_OFFSET: usize = 1;

/// Extract the original source text using its span
pub fn extract_expr_source(expr: &PreSymbolicExpression, source: &str) -> String {
    let span = &expr.span;
    extract_source_range(
        source,
        span.start_line,
        span.start_column,
        span.end_line,
        span.end_column,
    )
}

pub fn extract_source_range(
    source: &str,
    start_line: u32,
    start_column: u32,
    end_line: u32,
    end_column: u32,
) -> String {
    let start_line = u32_to_usize(start_line);
    let end_line = u32_to_usize(end_line);
    let start_column = u32_to_usize(start_column);
    let end_column = u32_to_usize(end_column);

    let lines: Vec<&str> = source.lines().collect();
    let mut result = String::new();

    for line_idx in start_line..=end_line {
        if line_idx > lines.len() {
            break;
        }

        let line = lines[line_idx - COLUMN_TO_INDEX_OFFSET];
        let is_first_line = line_idx == start_line;
        let is_last_line = line_idx == end_line;

        if is_first_line && is_last_line {
            let start_col = start_column.saturating_sub(COLUMN_TO_INDEX_OFFSET);
            let end_col = end_column.min(line.len());
            if start_col < end_col {
                result.push_str(&line[start_col..end_col]);
            }
        } else if is_first_line {
            let start_col = start_column.saturating_sub(COLUMN_TO_INDEX_OFFSET);
            if start_col < line.len() {
                result.push_str(&line[start_col..]);
            }
            if !is_last_line {
                result.push('\n');
            }
        } else if is_last_line {
            let end_col = end_column.min(line.len());
            if end_col > 0 {
                result.push_str(&line[..end_col]);
            }
        } else {
            // Middle lines
            result.push_str(line);
            result.push('\n');
        }
    }

    result
}

pub fn ignored_exprs(exprs: &[PreSymbolicExpression], source: &str) -> String {
    let start = exprs.first().unwrap().span();
    let end = exprs.last().unwrap().span();
    extract_source_range(
        source,
        start.start_line,
        start.start_column,
        end.end_line,
        end.end_column,
    )
}
