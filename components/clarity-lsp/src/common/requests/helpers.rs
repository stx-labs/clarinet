use std::cmp::Ordering;

use clarity_repl::clarity::representations::Span;
use clarity_repl::clarity::{ClarityName, SymbolicExpression, SymbolicExpressionType};
use ls_types::{Position, Range};

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

// end_offset is usded to include the end position of a keyword, for go to definition in particular
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
            Ordering::Equal => {
                if position.character > parameter.span.end_column + 1 {
                    position_in_parameters += 1
                }
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
