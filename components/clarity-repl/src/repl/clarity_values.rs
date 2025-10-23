use std::fmt::Write;

use clarity::codec::StacksMessageCodec;
use clarity::util::hash;
use clarity::vm::Value;
use clarity_types::types::{CharType, SequenceData};

pub fn to_raw_value(value: &Value) -> String {
    let hex = value
        .serialize_to_hex()
        .unwrap_or_else(|_e| panic!("failed to parse clarity value: {}", value));
    format!("0x{hex}")
}

pub fn uint8_to_string(value: &[u8]) -> String {
    value_to_string(&uint8_to_value(value))
}

pub fn uint8_to_value(mut value: &[u8]) -> Value {
    Value::consensus_deserialize(&mut value)
        .unwrap_or_else(|e| panic!("failed to parse clarity value: {}", e))
}

pub fn value_to_string(value: &Value) -> String {
    match value {
        Value::Principal(principal_data) => format!("'{principal_data}"),
        Value::Tuple(tup_data) => {
            let mut data = String::new();
            for (name, value) in &tup_data.data_map {
                write!(&mut data, "{}: {}, ", name, value_to_string(value)).unwrap();
            }
            format!("{{ {} }}", data.trim_end_matches(", "))
        }
        Value::Optional(opt_data) => match &opt_data.data {
            Some(x) => format!("(some {})", value_to_string(x)),
            None => "none".to_string(),
        },
        Value::Response(res_data) => {
            let committed = if res_data.committed { "ok" } else { "err" };
            format!("({} {})", committed, value_to_string(&res_data.data))
        }
        Value::Sequence(SequenceData::String(CharType::ASCII(ascii_data))) => {
            format!("\"{}\"", String::from_utf8_lossy(&ascii_data.data))
        }
        Value::Sequence(SequenceData::String(CharType::UTF8(utf8_data))) => {
            let result = utf8_data
                .data
                .iter()
                .map(|c| {
                    if c.len() > 1 {
                        format!("\\u{{{}}}", hash::to_hex(&c[..]))
                    } else {
                        (c[0] as char).to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join("");
            format!("u\"{result}\"")
        }
        Value::Sequence(SequenceData::List(list_data)) => {
            let data = list_data
                .data
                .iter()
                .map(value_to_string)
                .collect::<Vec<_>>()
                .join(" ");
            format!("(list {data})")
        }
        _ => value.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;

    use clarity::vm::{ClarityName, Value};
    use clarity_types::types::{
        ASCIIData, CharType, ListData, ListTypeData, OptionalData, PrincipalData,
        QualifiedContractIdentifier, ResponseData, SequenceData, SequencedValue,
        StandardPrincipalData, TupleData, TypeSignature, UTF8Data, NONE,
    };

    use super::value_to_string;

    #[test]
    fn test_value_to_string() {
        let mut s = value_to_string(&Value::Int(42));
        assert_eq!(s, "42");

        s = value_to_string(&Value::UInt(12345678909876));
        assert_eq!(s, "u12345678909876");

        s = value_to_string(&Value::Bool(true));
        assert_eq!(s, "true");

        s = value_to_string(&Value::Principal(PrincipalData::Standard(
            StandardPrincipalData::transient(),
        )));
        assert_eq!(s, "'S1G2081040G2081040G2081040G208105NK8PE5");

        s = value_to_string(&Value::Principal(PrincipalData::Contract(
            QualifiedContractIdentifier::transient(),
        )));
        assert_eq!(s, "'S1G2081040G2081040G2081040G208105NK8PE5.__transient");

        s = value_to_string(&Value::buff_from(vec![1, 2, 3]).unwrap());
        assert_eq!(s, "0x010203");

        s = value_to_string(&Value::buff_from(vec![1, 2, 3]).unwrap());
        assert_eq!(s, "0x010203");

        s = value_to_string(&Value::Tuple(
            TupleData::from_data(vec![(
                ClarityName::try_from("foo".to_string()).unwrap(),
                Value::Bool(true),
            )])
            .unwrap(),
        ));
        assert_eq!(s, "{ foo: true }");

        s = value_to_string(&Value::Optional(OptionalData {
            data: Some(Box::new(Value::UInt(42))),
        }));
        assert_eq!(s, "(some u42)");

        s = value_to_string(&NONE);
        assert_eq!(s, "none");

        s = value_to_string(&Value::Response(ResponseData {
            committed: true,
            data: Box::new(Value::Int(-321)),
        }));
        assert_eq!(s, "(ok -321)");

        s = value_to_string(&Value::Response(ResponseData {
            committed: false,
            data: Box::new(Value::Sequence(SequenceData::String(CharType::ASCII(
                ASCIIData {
                    data: "'foo'".as_bytes().to_vec(),
                },
            )))),
        }));
        assert_eq!(s, "(err \"'foo'\")");

        s = value_to_string(&Value::Sequence(SequenceData::String(CharType::ASCII(
            ASCIIData {
                data: "Hello, \"world\"\n".as_bytes().to_vec(),
            },
        ))));
        assert_eq!(s, "\"Hello, \"world\"\n\"");

        s = value_to_string(&UTF8Data::to_value(&"Hello, 'world'\n".as_bytes().to_vec()).unwrap());
        assert_eq!(s, "u\"Hello, 'world'\n\"");

        s = value_to_string(&Value::Sequence(SequenceData::List(ListData {
            data: vec![Value::Int(-321)],
            type_signature: ListTypeData::new_list(TypeSignature::IntType, 2).unwrap(),
        })));
        assert_eq!(s, "(list -321)");
    }
}
