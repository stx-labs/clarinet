use clarity::vm::representations::Span;
use clarity::vm::ClarityName;
use regex::Regex;
use strum::EnumString;

#[derive(Debug)]
pub enum AnnotationKind {
    Allow(WarningKind),
    Filter(Vec<ClarityName>),
    FilterAll,
}

impl std::str::FromStr for AnnotationKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"([[:word:]]+)(\(([^)]+)\))?").unwrap();
        if let Some(captures) = re.captures(s) {
            let (base, value) = if captures.get(1).is_some() && captures.get(3).is_some() {
                (&captures[1], &captures[3])
            } else {
                (&captures[1], "")
            };
            match base {
                "allow" => match value.parse() {
                    Ok(value) => Ok(AnnotationKind::Allow(value)),
                    Err(_) => Err("missing value for 'allow' annotation".to_string()),
                },
                "filter" => {
                    if value == "*" {
                        Ok(AnnotationKind::FilterAll)
                    } else {
                        let params: Vec<ClarityName> = value
                            .split(',')
                            .filter(|s| !s.is_empty())
                            .map(|s| ClarityName::from(s.trim()))
                            .collect();
                        if params.is_empty() {
                            Err("missing value for 'filter' annotation".to_string())
                        } else {
                            Ok(AnnotationKind::Filter(params))
                        }
                    }
                }
                _ => Err("unrecognized annotation".to_string()),
            }
        } else {
            Err("malformed annotation".to_string())
        }
    }
}

#[derive(Debug, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum WarningKind {
    UncheckedData,
    UncheckedParams,
    Noop,
    UnusedConst,
    UnusedDataVar,
    UnusedMap,
    UnusedPrivateFn,
    UnusedToken,
}

#[derive(Debug)]
pub struct Annotation {
    pub kind: AnnotationKind,
    pub span: Span,
}

/// Returns the index in `annotations` for `span`
/// Assumes `annotations` is sorted by `span.start_line`
pub fn get_index_of_span(annotations: &[Annotation], span: &Span) -> Option<usize> {
    for (i, annotation) in annotations.iter().enumerate() {
        if annotation.span.start_line == (span.start_line - 1) {
            return Some(i);
        } else if annotation.span.start_line >= span.start_line {
            // The annotations are ordered by span, so if we have passed
            // the target line, return.
            break;
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_unchecked_data() {
        match "unchecked_data".parse::<WarningKind>() {
            Ok(WarningKind::UncheckedData) => (),
            _ => panic!("failed to parse warning kind correctly"),
        };
    }

    #[test]
    fn parse_warning_error() {
        match "invalid_string".parse::<WarningKind>() {
            Err(_) => (),
            _ => panic!("failed to return error for bad string"),
        };
    }

    #[test]
    fn parse_allow_unchecked_data() {
        match "allow(unchecked_data)".parse::<AnnotationKind>() {
            Ok(AnnotationKind::Allow(WarningKind::UncheckedData)) => (),
            _ => panic!("failed to parse annotation kind correctly"),
        };
    }

    #[test]
    fn parse_annotation_kind_error() {
        match "invalid_string".parse::<AnnotationKind>() {
            Err(_) => (),
            _ => panic!("failed to return error for bad string"),
        };
    }

    #[test]
    fn parse_annotation_kind_error2() {
        match "invalid(string)".parse::<AnnotationKind>() {
            Err(_) => (),
            _ => panic!("failed to return error for bad string"),
        };
    }

    #[test]
    fn parse_annotation_kind_empty() {
        match "".parse::<AnnotationKind>() {
            Err(_) => (),
            _ => panic!("failed to return error for bad string"),
        };
    }

    #[test]
    fn parse_filter() {
        match "filter(foo,bar)".parse::<AnnotationKind>() {
            Ok(AnnotationKind::Filter(params)) => {
                assert!(
                    params.len() == 2 && params[0].as_str() == "foo" && params[1].as_str() == "bar",
                    "failed to parse 'filter' parameters correctly"
                )
            }
            _ => panic!("failed to parse 'filter' correctly"),
        };
    }

    #[test]
    fn parse_filter_all() {
        match "filter(*)".parse::<AnnotationKind>() {
            Ok(AnnotationKind::FilterAll) => (),
            _ => panic!("failed to parse 'filter(*)' correctly"),
        };
    }

    #[test]
    fn parse_filter_empty() {
        match "filter".parse::<AnnotationKind>() {
            Err(_) => (),
            _ => panic!("failed to return error for 'filter' with no parameters"),
        };
    }
}
