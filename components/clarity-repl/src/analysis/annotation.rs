use clarity::vm::representations::Span;
use strum::EnumString;

use crate::utils::Environment;

#[derive(Debug)]
pub enum AnnotationKind {
    Allow(Vec<WarningKind>),
    Env(Environment),
    /// `#[filter(...)]`, used by the removed check-checker. Still accepted
    /// because deployed contracts carry it.
    Filter,
}

impl std::str::FromStr for AnnotationKind {
    type Err = String;

    /// Parse an annotation, e.g. `#[env(simnet)]` or `#[allow(unused_const)]`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s
            .trim()
            .strip_prefix("#[")
            .and_then(|s| s.strip_suffix(']'))
            .ok_or_else(|| "malformed annotation: expected #[...]".to_string())?
            .trim();
        let (name, value) = match s.find('(') {
            Some(open) => {
                let name = s[..open].trim();
                let rest = &s[open + 1..];
                let value = rest
                    .strip_suffix(')')
                    .ok_or_else(|| "malformed annotation: missing closing ')'".to_string())?;
                (name, Some(value.trim()))
            }
            None => (s, None),
        };

        match name {
            "allow" => {
                let params: Vec<WarningKind> = value
                    .unwrap_or("")
                    .split(',')
                    .filter(|s| !s.is_empty())
                    .map(|s| {
                        s.trim().parse().map_err(|_| {
                            format!("unknown warning kind '{s}' in 'allow' annotation")
                        })
                    })
                    .collect::<Result<_, _>>()?;
                if params.is_empty() {
                    Err("missing value for 'allow' annotation".to_string())
                } else {
                    Ok(AnnotationKind::Allow(params))
                }
            }
            "env" => {
                let value =
                    value.ok_or_else(|| "missing value for 'env' annotation".to_string())?;
                let env: Environment = value
                    .parse()
                    .map_err(|_| format!("bad environment {value} for 'env' annotation"))?;
                if env == Environment::OnChain {
                    return Err(
                        "'onchain' is not a valid environment for 'env' annotation".to_string()
                    );
                }
                Ok(AnnotationKind::Env(env))
            }
            "filter" => Ok(AnnotationKind::Filter),
            _ => Err("unrecognized annotation".to_string()),
        }
    }
}

#[derive(Debug, EnumString, PartialEq, Eq, Hash)]
#[strum(serialize_all = "snake_case")]
pub enum WarningKind {
    // Used by the removed check-checker. Still accepted because deployed
    // contracts carry them.
    UncheckedData,
    UncheckedParams,
    // Linter. Keep sorted alphabetically
    AtBlock,
    CallChecker,
    CaseBinding,
    CaseConst,
    CaseDataVar,
    CaseFn,
    CaseMap,
    CaseToken,
    CaseTrait,
    ErrorConst,
    FlattenVariadic,
    Noop,
    Panic,
    RenamedBuiltin,
    UnnecessaryAsMaxLen,
    UnnecessaryPublic,
    UnnecessaryTuple,
    UnusedBinding,
    UnusedConst,
    UnusedDataVar,
    UnusedMap,
    UnusedPrivateFn,
    UnusedToken,
    UnusedTrait,
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
        match "#[allow(unchecked_data)]".parse::<AnnotationKind>() {
            Ok(AnnotationKind::Allow(params)) => {
                assert_eq!(params.len(), 1);
                assert!(matches!(params[0], WarningKind::UncheckedData));
            }
            _ => panic!("failed to parse annotation kind correctly"),
        };
    }

    #[test]
    fn parse_allow_multiple() {
        match "#[allow(unused_const, case_const)]".parse::<AnnotationKind>() {
            Ok(AnnotationKind::Allow(params)) => {
                assert_eq!(params.len(), 2);
                assert!(params.contains(&WarningKind::UnusedConst));
                assert!(params.contains(&WarningKind::CaseConst));
            }
            _ => panic!("failed to parse multiple allow annotation correctly"),
        };
    }

    #[test]
    fn parse_allow_rejects_unknown_warning_kind() {
        // If the user includes an unknown warning kind alongside a valid one,
        // parsing should fail rather than silently dropping the unknown name and
        // accepting only the recognized parts.
        let result = "#[allow(unused_const, not_a_real_warning)]".parse::<AnnotationKind>();
        assert!(
            result.is_err(),
            "expected Err for unknown warning kind, got {result:?} \
             (the unknown kind 'not_a_real_warning' was silently dropped)"
        );
    }

    #[test]
    fn parse_annotation_kind_error() {
        match "#[invalid_string]".parse::<AnnotationKind>() {
            Err(_) => (),
            _ => panic!("failed to return error for bad string"),
        };
    }

    #[test]
    fn parse_annotation_kind_error2() {
        match "#[invalid(string)]".parse::<AnnotationKind>() {
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
    fn parse_annotation_missing_brackets() {
        match "allow(unchecked_data)".parse::<AnnotationKind>() {
            Err(_) => (),
            _ => panic!("failed to return error for annotation without #[...]"),
        };
    }

    #[test]
    fn parse_legacy_filter() {
        for annotation in ["#[filter(foo,bar)]", "#[filter(*)]", "#[filter]"] {
            assert!(
                matches!(annotation.parse(), Ok(AnnotationKind::Filter)),
                "failed to parse '{annotation}'"
            );
        }
    }
}
