use clarity_repl::clarity::SymbolicExpression;
use ls_types::Position;

use super::api_ref::API_REF;
use super::helpers::get_expression_name_at_position;

pub fn get_expression_documentation(
    position: &Position,
    expressions: &[SymbolicExpression],
) -> Option<String> {
    let expression_name = get_expression_name_at_position(position, expressions)?;

    API_REF
        .get(&expression_name.to_string())
        .map(|(documentation, _)| documentation.to_owned())
}
