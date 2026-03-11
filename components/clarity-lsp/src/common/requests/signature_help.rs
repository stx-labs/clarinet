use clarity_repl::clarity::docs::FunctionAPI;
use ls_types::{ParameterInformation, ParameterLabel, Position, SignatureInformation};

use super::api_ref::API_REF;
use super::helpers::get_function_at_position;
use crate::state::ActiveContractData;

pub fn get_signatures(
    contract: &ActiveContractData,
    position: &Position,
) -> Option<Vec<SignatureInformation>> {
    let (function_name, mut active_parameter) =
        get_function_at_position(position, contract.expressions.as_ref()?)?;

    if [
        "define-read-only",
        "define-public",
        "define-private",
        "define-trait,",
        "let",
        "begin",
        "tuple",
    ]
    .contains(&function_name.as_str())
    {
        // showing signature help for define-<function>, define-trait, let, and begin adds to much noise
        // it doesn't make sense for the tuple {} notation
        return None;
    }

    let (_, reference) = API_REF.get(&function_name.to_string())?;
    let FunctionAPI {
        signature,
        output_type,
        ..
    } = (*reference).as_ref()?;

    let signatures = signature
        .split(" |")
        .map(|mut signature| {
            signature = signature.trim();
            let mut signature_without_parenthesis = signature.chars();
            signature_without_parenthesis.next();
            signature_without_parenthesis.next_back();
            let signature_without_parenthesis = signature_without_parenthesis.as_str();
            let parameters = signature_without_parenthesis
                .split(' ')
                .collect::<Vec<&str>>();
            let (_, parameters) = parameters.split_first().expect("invalid signature format");

            if active_parameter.unwrap_or_default() >= parameters.len().try_into().unwrap() {
                if let Some(variadic_index) = parameters.iter().position(|p| p.contains("...")) {
                    active_parameter = Some(variadic_index.try_into().unwrap());
                }
            }
            let label = if output_type.eq("Not Applicable") {
                String::from(signature)
            } else {
                format!("{} -> {}", &signature, &output_type)
            };

            SignatureInformation {
                active_parameter,
                documentation: None,
                label,
                parameters: Some(
                    parameters
                        .iter()
                        .map(|param| ParameterInformation {
                            documentation: None,
                            label: ParameterLabel::Simple(param.to_string()),
                        })
                        .collect::<Vec<ParameterInformation>>(),
                ),
            }
        })
        .collect::<Vec<SignatureInformation>>();

    Some(signatures)
}

#[cfg(test)]
mod definitions_visitor_tests {
    use clarity_repl::clarity::functions::NativeFunctions;
    use clarity_repl::clarity::ClarityVersion::Clarity2;
    use clarity_repl::clarity::StacksEpochId::Epoch21;
    use ls_types::ParameterLabel::Simple;
    use ls_types::{ParameterInformation, Position, SignatureInformation};

    use super::get_signatures;
    use crate::state::ActiveContractData;

    fn get_source_signature(
        source: String,
        position: &Position,
    ) -> Option<Vec<ls_types::SignatureInformation>> {
        let contract = &ActiveContractData::new(Clarity2, Epoch21, None, source);
        get_signatures(contract, position)
    }

    #[test]
    fn get_simple_signature() {
        let signatures = get_source_signature(
            "(var-set counter )".to_owned(),
            &Position {
                line: 1,
                character: 18,
            },
        );

        assert!(signatures.is_some());
        let signatures = signatures.unwrap();
        assert_eq!(signatures.len(), 1);
        assert_eq!(
            signatures.first().unwrap(),
            &SignatureInformation {
                label: "(var-set var-name expr1) -> bool".to_string(),
                documentation: None,
                parameters: Some(
                    [
                        ParameterInformation {
                            label: Simple("var-name".to_string()),
                            documentation: None,
                        },
                        ParameterInformation {
                            label: Simple("expr1".to_string()),
                            documentation: None,
                        },
                    ]
                    .to_vec(),
                ),
                active_parameter: Some(1),
            }
        );
    }

    #[test]
    fn ensure_all_native_function_have_valid_signature() {
        for method in NativeFunctions::ALL_NAMES {
            if [
                "define-read-only",
                "define-public",
                "define-readonly",
                "define-trait,",
                "let",
                "begin",
                "tuple",
            ]
            .contains(method)
            {
                continue;
            }

            let src = format!("({method} )");
            let signatures = get_source_signature(
                src,
                &Position {
                    line: 1,
                    character: 2,
                },
            );
            assert!(signatures.is_some());
            match *method {
                "match" => {
                    assert_eq!(signatures.unwrap().len(), 2)
                }
                _ => {
                    assert_eq!(signatures.unwrap().len(), 1)
                }
            }
        }
    }
}
