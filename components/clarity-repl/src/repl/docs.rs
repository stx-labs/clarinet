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

impl Entry {
    /// Keywords have no `usage`.
    fn new(
        usage: Option<&str>,
        description: &str,
        example: &str,
        min_version: ClarityVersion,
        max_version: Option<ClarityVersion>,
    ) -> Self {
        let availability = match max_version {
            Some(max_version) => format!("Available since {min_version} through {max_version}"),
            None => format!("Available since {min_version}"),
        };
        let sections = [
            usage.map(|usage| ("Usage", clarity_block(usage))),
            Some(("Clarity version", availability)),
            Some(("Description", unwrap_lines(description))),
            Some(("Examples", clarity_block(example))),
        ];
        let doc = sections
            .into_iter()
            .flatten()
            .map(|(title, body)| format!("## {title}\n{body}"))
            .collect::<Vec<_>>()
            .join("\n\n");
        Self {
            doc,
            min_version,
            max_version,
        }
    }
}

/// Joins stacks-core's hard-wrapped lines, keeping paragraphs and list items.
fn unwrap_lines(text: &str) -> String {
    let lines: Vec<&str> = text.lines().collect();
    lines
        .split(|line| line.trim().is_empty())
        .filter(|block| !block.is_empty())
        .map(unwrap_block)
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// List items keep their indent relative to the block's outermost item, so nested lists stay nested.
fn unwrap_block(lines: &[&str]) -> String {
    let indent = |line: &str| line.len() - line.trim_start().len();
    let is_list_item = |line: &str| {
        let line = line.trim_start();
        line.starts_with("* ") || line.starts_with("- ")
    };
    let base = lines
        .iter()
        .filter(|line| is_list_item(line))
        .map(|line| indent(line))
        .min()
        .unwrap_or(0);
    let mut out = String::new();
    for line in lines {
        if is_list_item(line) {
            if !out.is_empty() {
                out.push('\n');
            }
            out.push_str(&" ".repeat(indent(line) - base));
        } else if !out.is_empty() {
            out.push(' ');
        }
        out.push_str(line.trim());
    }
    out
}

fn clarity_block(code: &str) -> String {
    format!("```clarity\n{}\n```", code.trim())
}

fn function_entry(api: FunctionAPI) -> (String, Entry) {
    let entry = Entry::new(
        Some(&api.signature),
        &api.description,
        &api.example,
        api.min_version,
        api.max_version,
    );
    (api.name, entry)
}

fn build_api_reference() -> BTreeMap<String, Entry> {
    NativeFunctions::ALL
        .iter()
        .map(|func| function_entry(make_api_reference(func)))
        .chain(
            DefineFunctions::ALL
                .iter()
                .map(|func| function_entry(make_define_reference(func))),
        )
        .collect()
}

fn clarity_keywords() -> BTreeMap<String, Entry> {
    NativeVariables::ALL
        .iter()
        .filter_map(make_keyword_reference)
        .map(|key| {
            let entry = Entry::new(
                None,
                key.description,
                key.example,
                key.min_version,
                key.max_version,
            );
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

/// The entry for `name` under a `# name` heading.
pub fn describe(name: &str) -> Option<String> {
    lookup(name).map(|doc| format!("# {name}\n{doc}"))
}

/// Operators are keyed with an alias, like `+ (add)`, so they also match on their symbol alone.
fn lookup(name: &str) -> Option<&'static str> {
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
    fn lookup_finds_functions_and_keywords() {
        assert!(lookup("map")
            .unwrap()
            .starts_with("## Usage\n```clarity\n(map "));
        assert!(lookup("tx-sender")
            .unwrap()
            .starts_with("## Clarity version\nAvailable since Clarity 1\n\n## Description\n"));
        assert_eq!(lookup("not-a-clarity-name"), None);
    }

    #[test]
    fn lookup_finds_operators_by_symbol() {
        assert_eq!(lookup("+"), lookup("+ (add)"));
        assert!(lookup("<")
            .unwrap()
            .starts_with("## Usage\n```clarity\n(< "));
        assert!(lookup("<=")
            .unwrap()
            .starts_with("## Usage\n```clarity\n(<= "));
    }

    #[test]
    fn lookup_shows_availability() {
        assert!(lookup("block-height")
            .unwrap()
            .starts_with("## Clarity version\nAvailable since Clarity 1 through Clarity 2\n"));
        assert!(lookup("as-contract?")
            .unwrap()
            .contains("\n\n## Clarity version\nAvailable since Clarity 4\n\n## Description\n"));
    }

    #[test]
    fn lookup_fences_code() {
        assert!(lookup("tx-sender")
            .unwrap()
            .ends_with(";; Will print out a Stacks address of the transaction sender\n```"));
        assert!(functions_index(ClarityVersions::All)
            .chain(keywords_index(ClarityVersions::All))
            .all(|name| lookup(name)
                .unwrap()
                .matches("```")
                .count()
                .is_multiple_of(2)));
    }

    #[test]
    fn describe_adds_a_heading() {
        assert!(describe("<=").unwrap().starts_with("# <=\n## Usage\n"));
        assert_eq!(describe("not-a-clarity-name"), None);
    }

    #[test]
    fn lookup_keeps_paragraphs_and_lists() {
        let doc = lookup("stx-transfer?").unwrap();
        assert!(
            doc.contains("`tx-sender`.\n\nThis function returns"),
            "{doc}"
        );
        assert!(doc.contains(":\n\n* `(err u1)` -- "), "{doc}");
        assert!(doc.contains("to transfer\n* `(err u2)` -- "), "{doc}");
        let doc = lookup("secp256k1-decompress?").unwrap();
        assert!(doc.contains(" error code:\n- `(err u1)` "), "{doc}");
    }

    #[test]
    fn lookup_keeps_nested_lists() {
        let doc = lookup("get-stacks-block-info?").unwrap();
        assert!(doc.contains("\n  - Greater than the timestamp"), "{doc}");
        let doc = lookup("get-burn-block-info?").unwrap();
        assert!(doc.contains(":\n* They each have type signature"), "{doc}");
        assert!(doc.contains("following:\n   * `0x00` means"), "{doc}");
    }

    #[test]
    fn lookup_ignores_versions() {
        assert!(lookup("block-height").is_some());
        assert!(lookup("stacks-block-height").is_some());
    }

    #[test]
    fn indexes_are_sorted_and_describable() {
        assert!(functions_index(ClarityVersions::All).is_sorted());
        assert!(keywords_index(ClarityVersions::All).is_sorted());
        assert!(functions_index(ClarityVersions::All)
            .chain(keywords_index(ClarityVersions::All))
            .all(|name| lookup(name).is_some()));
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
