use clarity::vm::representations::PreSymbolicExpression;

use super::is_comment;

/// Syntax positions exclude comments; the gaps retain every comment in source order.
pub(super) struct Sequence<'a> {
    pub items: Vec<Item<'a>>,
    pub end_comments: &'a [PreSymbolicExpression],
}

pub(super) struct Item<'a> {
    pub expression: &'a PreSymbolicExpression,
    pub comments: &'a [PreSymbolicExpression],
}

impl<'a> From<&'a [PreSymbolicExpression]> for Sequence<'a> {
    fn from(mut expressions: &'a [PreSymbolicExpression]) -> Self {
        let mut items = Vec::new();
        while let Some(position) = expressions.iter().position(|expr| !is_comment(expr)) {
            let (comments, rest) = expressions.split_at(position);
            items.push(Item {
                expression: &rest[0],
                comments,
            });
            expressions = &rest[1..];
        }
        Self {
            items,
            end_comments: expressions,
        }
    }
}
