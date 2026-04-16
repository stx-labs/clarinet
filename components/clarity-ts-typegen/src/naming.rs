/// Convert a Clarity kebab-case name to camelCase.
/// e.g. "get-counter" -> "getCounter", "err-unauthorized" -> "errUnauthorized"
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

/// Convert a Clarity kebab-case name to PascalCase.
/// e.g. "get-counter" -> "GetCounter", "counter" -> "Counter"
pub fn clarity_to_pascal(name: &str) -> String {
    let mut result = String::with_capacity(name.len());
    let mut capitalize_next = true;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_camel_case() {
        assert_eq!(clarity_to_camel("get-counter"), "getCounter");
        assert_eq!(clarity_to_camel("counter"), "counter");
        assert_eq!(clarity_to_camel("err-unauthorized"), "errUnauthorized");
        assert_eq!(clarity_to_camel("a-b-c"), "aBC");
        assert_eq!(clarity_to_camel("token-uri"), "tokenUri");
        assert_eq!(clarity_to_camel("my_var"), "my_var");
        assert_eq!(clarity_to_camel("x"), "x");
    }

    #[test]
    fn test_pascal_case() {
        assert_eq!(clarity_to_pascal("get-counter"), "GetCounter");
        assert_eq!(clarity_to_pascal("counter"), "Counter");
        assert_eq!(clarity_to_pascal("err-unauthorized"), "ErrUnauthorized");
        assert_eq!(clarity_to_pascal("a-b-c"), "ABC");
        assert_eq!(clarity_to_pascal("token-uri"), "TokenUri");
        assert_eq!(clarity_to_pascal("my_var"), "My_var");
        assert_eq!(clarity_to_pascal("x"), "X");
    }
}
