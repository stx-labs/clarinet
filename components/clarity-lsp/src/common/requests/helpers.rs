use std::cmp::Ordering;

use clarity::vm::{SymbolicExpression, SymbolicExpressionType};
use clarity_types::representations::Span;
use clarity_types::ClarityName;
use ls_types::{Position, Range};

/// The byte offset in `line` of an LSP column. Columns count UTF-16 code
/// units, so after a non-ASCII character they no longer match bytes and
/// slicing by the column panics. Past the end it's the line's length.
pub fn column_to_byte(line: &str, column: u32) -> usize {
    let mut units = 0;
    line.char_indices()
        .find(|(_, c)| {
            let reached = units >= column as usize;
            units += c.len_utf16();
            reached
        })
        .map_or(line.len(), |(byte, _)| byte)
}

pub fn span_to_range(span: &Span) -> Range {
    if span == &Span::zero() {
        return Range::default();
    }

    {
        Range::new(
            Position::new(span.start_line - 1, span.start_column - 1),
            Position::new(span.end_line - 1, span.end_column),
        )
    }
}

// end_offset is used to include the end position of a keyword, for go to definition in particular
pub fn is_position_within_span(position: &Position, span: &Span, end_offset: u32) -> bool {
    if position.line < span.start_line || position.line > span.end_line {
        return false;
    }
    if position.line == span.start_line && position.character < span.start_column {
        return false;
    }
    if position.line == span.end_line && position.character > span.end_column + end_offset {
        return false;
    }

    true
}

pub fn get_expression_name_at_position(
    position: &Position,
    expressions: &[SymbolicExpression],
) -> Option<ClarityName> {
    for expr in expressions {
        if is_position_within_span(position, &expr.span, 0) {
            match &expr.expr {
                SymbolicExpressionType::Atom(function_name) => {
                    return Some(function_name.to_owned())
                }
                SymbolicExpressionType::List(expressions) => {
                    return get_expression_name_at_position(position, expressions)
                }
                _ => {}
            }
        }
    }
    None
}

pub fn get_function_at_position(
    position: &Position,
    expressions: &[SymbolicExpression],
) -> Option<(ClarityName, Option<u32>)> {
    for expr in expressions {
        if is_position_within_span(position, &expr.span, 0) {
            if let Some(expressions) = expr.match_list() {
                return get_function_at_position(position, expressions);
            }
        }
    }

    let mut position_in_parameters: i32 = -1;
    for parameter in expressions {
        match position.line.cmp(&parameter.span.end_line) {
            Ordering::Equal if position.character > parameter.span.end_column + 1 => {
                position_in_parameters += 1
            }
            Ordering::Greater => position_in_parameters += 1,
            _ => {}
        }
    }

    let (function_name, _) = expressions.split_first()?;

    Some((
        function_name.match_atom()?.to_owned(),
        position_in_parameters.try_into().ok(),
    ))
}

pub fn get_atom_or_field_start_at_position(
    position: &Position,
    expressions: &[SymbolicExpression],
) -> Option<(u32, u32)> {
    for expr in expressions {
        if is_position_within_span(position, &expr.span, 1) {
            match &expr.expr {
                SymbolicExpressionType::Atom(_) | SymbolicExpressionType::Field(_) => {
                    return Some((expr.span.start_line, expr.span.start_column))
                }
                SymbolicExpressionType::List(expressions) => {
                    return get_atom_or_field_start_at_position(position, expressions)
                }
                _ => {}
            }
        }
    }
    None
}

#[cfg(test)]
mod column_to_byte_tests {
    use super::column_to_byte;

    #[test]
    fn counts_utf16_units() {
        let line = ";; café 😀 x";
        assert_eq!(column_to_byte(line, 3), 3);
        // "é" is one unit and two bytes
        assert_eq!(column_to_byte(line, 7), 8);
        // "😀" is two units and four bytes
        assert_eq!(column_to_byte(line, 10), 13);
        assert_eq!(column_to_byte(line, 99), line.len());
    }
}
