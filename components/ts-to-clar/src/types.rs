use anyhow::{anyhow, Result};
use clarity::vm::types::{SequenceSubtype, TupleTypeSignature, TypeSignature};
use clarity::vm::ClarityName;
use oxc_ast::ast;

pub fn get_ascii_type(n: u32) -> TypeSignature {
    use clarity::vm::types::{BufferLength, StringSubtype::ASCII};
    TypeSignature::SequenceType(SequenceSubtype::StringType(ASCII(
        BufferLength::try_from(n).unwrap(),
    )))
}

pub fn get_utf8_type(n: u32) -> TypeSignature {
    use clarity::vm::types::{StringSubtype::UTF8, StringUTF8Length};
    TypeSignature::SequenceType(SequenceSubtype::StringType(UTF8(
        StringUTF8Length::try_from(n).unwrap(),
    )))
}

fn extract_numeric_type_param(
    type_params: Option<&ast::TSTypeParameterInstantiation>,
) -> Result<u32> {
    let param = type_params
        .and_then(|params| params.params.first())
        .ok_or_else(|| anyhow!("Missing type parameter"))?;

    if let ast::TSType::TSLiteralType(boxed_type) = param {
        if let ast::TSLiteral::NumericLiteral(num_lit) = &boxed_type.literal {
            return Ok(num_lit.value as u32);
        }
    }
    Err(anyhow!("Expected numeric literal type parameter"))
}

fn extract_type(
    type_ident: &str,
    type_params: Option<&ast::TSTypeParameterInstantiation>,
) -> Result<TypeSignature> {
    match type_ident {
        "Uint" => Ok(TypeSignature::UIntType),
        "Int" => Ok(TypeSignature::IntType),
        "Bool" => Ok(TypeSignature::BoolType),
        "StringAscii" => extract_numeric_type_param(type_params).map(get_ascii_type),
        "StringUtf8" => extract_numeric_type_param(type_params).map(get_utf8_type),
        _ => Err(anyhow!("Unknown type: {}", type_ident)),
    }
}

pub fn ts_to_clar_type(ts_type: &ast::TSType) -> Result<TypeSignature> {
    match ts_type {
        ast::TSType::TSTypeReference(boxed_ref) => {
            let type_name = boxed_ref.type_name.get_identifier_reference();
            extract_type(type_name.name.as_str(), boxed_ref.type_arguments.as_deref())
        }
        ast::TSType::TSTypeLiteral(boxed_lit) => {
            let members = boxed_lit
                .members
                .iter()
                .map(|member| match member {
                    ast::TSSignature::TSPropertySignature(prop_signature) => {
                        let key = &prop_signature.key;
                        let type_annotation = &prop_signature.type_annotation;
                        if let Some(type_annotation) = type_annotation {
                            match key {
                                ast::PropertyKey::StaticIdentifier(ident) => {
                                    let name = ClarityName::from(ident.name.as_str());
                                    let member_type =
                                        ts_to_clar_type(&type_annotation.type_annotation)?;
                                    Ok((name, member_type))
                                }
                                _ => Err(anyhow!("Expected identifier for property key")),
                            }
                        } else {
                            Err(anyhow!("Missing type annotation"))
                        }
                    }
                    _ => Err(anyhow!("Unexpected type for member: {:?}", member)),
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(TypeSignature::TupleType(TupleTypeSignature::try_from(
                members,
            )?))
        }
        _ => Err(anyhow!("Unexpected type: {:?}", ts_type)),
    }
}
