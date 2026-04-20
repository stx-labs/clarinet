/// Convert a Clarity kebab-case name to camelCase.
/// e.g. "get-counter" -> "getCounter", "simple-nft" -> "simpleNft"
pub fn clarity_to_camel(name: &str) -> String {
    let mut result = String::with_capacity(name.len());
    let mut capitalize_next = false;

    for ch in name.chars() {
        if ch == '-' {
            capitalize_next = true;
        } else if capitalize_next {
            result.extend(ch.to_uppercase());
            capitalize_next = false;
        } else {
            result.push(ch);
        }
    }

    result
}
