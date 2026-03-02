use std::io::Cursor;

use clarity::codec::StacksMessageCodec;
use clarity::vm::types::Value as ClarityValue;

pub fn try_decode_clarity_value(hex_value: &str) -> Option<ClarityValue> {
    let hex_value = hex_value.strip_prefix("0x")?;
    let value_bytes = hex::decode(hex_value).ok()?;
    ClarityValue::consensus_deserialize(&mut Cursor::new(&value_bytes)).ok()
}
