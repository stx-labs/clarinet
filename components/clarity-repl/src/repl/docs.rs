use std::collections::BTreeMap;
use std::fmt;
use std::str::FromStr;
use std::sync::LazyLock;

use clarinet_defaults::DEFAULT_CLARITY_VERSION;
use clarity::vm::docs::{
    make_api_reference, make_define_reference, make_keyword_reference, FunctionAPI,
};
use clarity::vm::functions::define::DefineFunctions;
use clarity::vm::functions::NativeFunctions;
use clarity::vm::variables::NativeVariables;
use clarity::vm::ClarityVersion;

use super::{clarity_version_from_u8, clarity_version_to_u8};

/// The Clarity versions an index lists.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ClarityVersions {
    Only(ClarityVersion),
    All,
}

impl Default for ClarityVersions {
    fn default() -> Self {
        Self::Only(DEFAULT_CLARITY_VERSION)
    }
}

impl FromStr for ClarityVersions {
    type Err = String;

    /// Accepts a version number (`6`), a version name (`clarity6`) or `all`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("all") {
            return Ok(Self::All);
        }
        s.parse()
            .ok()
            .and_then(clarity_version_from_u8)
            .or_else(|| s.parse().ok())
            .map(Self::Only)
            .ok_or_else(|| {
                let versions: Vec<_> = ClarityVersion::ALL
                    .iter()
                    .map(|&version| clarity_version_to_u8(version).to_string())
                    .collect();
                format!(
                    "invalid Clarity version '{s}', expected one of {} or 'all'",
                    versions.join(", ")
                )
            })
    }
}

impl fmt::Display for ClarityVersions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Only(version) => write!(f, "{version}"),
            Self::All => write!(f, "all Clarity versions"),
        }
    }
}

impl ClarityVersions {
    fn include(self, min: ClarityVersion, max: Option<ClarityVersion>) -> bool {
        match self {
            Self::Only(version) => min <= version && max.is_none_or(|max| version <= max),
            Self::All => true,
        }
    }
}

struct Entry {
    doc: String,
    min_version: ClarityVersion,
    max_version: Option<ClarityVersion>,
}

fn normalize_description(description: &str) -> String {
    description.replace('\n', " ")
}

fn availability(min_version: ClarityVersion, max_version: Option<ClarityVersion>) -> String {
    match max_version {
        Some(max_version) => format!("Available since {min_version} through {max_version}"),
        None => format!("Available since {min_version}"),
    }
}

fn format_api_doc(function_api: FunctionAPI) -> (String, Entry) {
    let description = normalize_description(&function_api.description);
    let availability = availability(function_api.min_version, function_api.max_version);
    let doc = format!(
        "Usage\n{}\n\nClarity version\n{availability}\n\nDescription\n{description}\n\nExamples\n{}",
        function_api.signature, function_api.example
    );
    let entry = Entry {
        doc,
        min_version: function_api.min_version,
        max_version: function_api.max_version,
    };
    (function_api.name, entry)
}

fn build_api_reference() -> BTreeMap<String, Entry> {
    NativeFunctions::ALL
        .iter()
        .map(|func| format_api_doc(make_api_reference(func)))
        .chain(
            DefineFunctions::ALL
                .iter()
                .map(|func| format_api_doc(make_define_reference(func))),
        )
        .collect()
}

fn clarity_keywords() -> BTreeMap<String, Entry> {
    NativeVariables::ALL
        .iter()
        .filter_map(make_keyword_reference)
        .map(|key| {
            let description = normalize_description(key.description);
            let availability = availability(key.min_version, key.max_version);
            let doc = format!(
                "Clarity version\n{availability}\n\nDescription\n{description}\n\nExamples\n{}",
                key.example
            );
            let entry = Entry {
                doc,
                min_version: key.min_version,
                max_version: key.max_version,
            };
            (key.name.to_string(), entry)
        })
        .collect()
}

static CLARITY_STD_REF: LazyLock<BTreeMap<String, Entry>> = LazyLock::new(build_api_reference);

static CLARITY_KEYWORDS_REF: LazyLock<BTreeMap<String, Entry>> = LazyLock::new(clarity_keywords);

fn index(
    reference: &'static BTreeMap<String, Entry>,
    versions: ClarityVersions,
) -> impl Iterator<Item = &'static str> {
    reference
        .iter()
        .filter(move |(_, entry)| versions.include(entry.min_version, entry.max_version))
        .map(|(name, _)| name.as_str())
}

pub fn functions_index(versions: ClarityVersions) -> impl Iterator<Item = &'static str> {
    index(&CLARITY_STD_REF, versions)
}

pub fn keywords_index(versions: ClarityVersions) -> impl Iterator<Item = &'static str> {
    index(&CLARITY_KEYWORDS_REF, versions)
}

/// Operators are keyed with an alias, like `+ (add)`, so they also match on their symbol alone.
pub fn describe(name: &str) -> Option<&'static str> {
    CLARITY_STD_REF
        .get(name)
        .or_else(|| CLARITY_KEYWORDS_REF.get(name))
        .or_else(|| {
            CLARITY_STD_REF
                .iter()
                .find(|(key, _)| {
                    key.split_once(' ')
                        .is_some_and(|(symbol, _)| symbol == name)
                })
                .map(|(_, entry)| entry)
        })
        .map(|entry| entry.doc.as_str())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn describe_finds_functions_and_keywords() {
        assert!(describe("map").unwrap().starts_with("Usage\n(map "));
        assert!(describe("tx-sender")
            .unwrap()
            .starts_with("Clarity version\nAvailable since Clarity 1\n\nDescription\n"));
        assert_eq!(describe("not-a-clarity-name"), None);
    }

    #[test]
    fn describe_finds_operators_by_symbol() {
        assert_eq!(describe("+"), describe("+ (add)"));
        assert!(describe("<").unwrap().starts_with("Usage\n(< "));
        assert!(describe("<=").unwrap().starts_with("Usage\n(<= "));
    }

    #[test]
    fn describe_shows_availability() {
        assert!(describe("block-height")
            .unwrap()
            .starts_with("Clarity version\nAvailable since Clarity 1 through Clarity 2\n"));
        assert!(describe("as-contract?")
            .unwrap()
            .contains("\n\nClarity version\nAvailable since Clarity 4\n\nDescription\n"));
    }

    #[test]
    fn describe_ignores_versions() {
        assert!(describe("block-height").is_some());
        assert!(describe("stacks-block-height").is_some());
    }

    #[test]
    fn indexes_are_sorted_and_describable() {
        assert!(functions_index(ClarityVersions::All).is_sorted());
        assert!(keywords_index(ClarityVersions::All).is_sorted());
        assert!(functions_index(ClarityVersions::All)
            .chain(keywords_index(ClarityVersions::All))
            .all(|name| describe(name).is_some()));
    }

    #[test]
    fn indexes_filter_by_version() {
        let keywords = |versions| keywords_index(versions).collect::<Vec<_>>();
        let functions = |versions| functions_index(versions).collect::<Vec<_>>();
        let clarity1 = ClarityVersions::Only(ClarityVersion::Clarity1);

        assert!(keywords(clarity1).contains(&"block-height"));
        assert!(!keywords(clarity1).contains(&"stacks-block-height"));
        assert!(!keywords(ClarityVersions::default()).contains(&"block-height"));
        assert!(keywords(ClarityVersions::default()).contains(&"stacks-block-height"));
        assert!(keywords(ClarityVersions::All).contains(&"block-height"));

        assert!(functions(clarity1).contains(&"define-public"));
        assert!(!functions(clarity1).contains(&"as-contract?"));
        assert!(functions(ClarityVersions::default()).contains(&"as-contract?"));
    }

    #[test]
    fn parses_versions() {
        let parse = |s: &str| s.parse::<ClarityVersions>();
        assert_eq!(parse("all"), Ok(ClarityVersions::All));
        assert_eq!(
            parse("3"),
            Ok(ClarityVersions::Only(ClarityVersion::Clarity3))
        );
        assert_eq!(
            parse("clarity2"),
            Ok(ClarityVersions::Only(ClarityVersion::Clarity2))
        );
        assert!(parse("0").unwrap_err().contains("expected one of 1, 2"));
        assert!(parse("latest").is_err());
    }
}
