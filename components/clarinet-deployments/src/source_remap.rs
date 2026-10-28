//! Textual rewriting of principals and contract identifiers in Clarity source.
//!
//! Textual rather than syntactic: a requirement's `remap_principals` maps a
//! bare issuer address, and that remap has to reach every position a
//! principal can appear in, comments and string literals included, which a
//! token matcher cannot do.

use std::cmp::Reverse;

use clarity::types::StacksEpochId;
use clarity::vm::ast::parser;
use clarity::vm::ast::stack_depth_checker::StackDepthLimits;
use clarity::vm::representations::{
    ContractName, PreSymbolicExpression, PreSymbolicExpressionType,
};
use clarity::vm::types::QualifiedContractIdentifier;

use crate::types::ContractRemaps;

/// Qualify the relative references (`.name`, `.name.trait`) to contracts in
/// `remaps`, so the textual rewrite can reach them. Only parsed references are
/// touched, and they keep the original ID so the rewrite picks the destination.
pub(crate) fn qualify_remapped_relative_references(
    source: &str,
    original_contract_id: &QualifiedContractIdentifier,
    remaps: &ContractRemaps,
) -> Option<String> {
    let issuer = &original_contract_id.issuer;
    if !remaps.covers_issuer(issuer) {
        return None;
    }
    let expressions =
        parser::v2::parse(source, StackDepthLimits::for_epoch(StacksEpochId::latest())).ok()?;
    let is_remapped = |name: &ContractName| {
        remaps.contains(&QualifiedContractIdentifier::new(
            issuer.clone(),
            name.clone(),
        ))
    };
    let mut positions = vec![];
    collect_relative_positions(&expressions, &is_remapped, &mut positions);
    if positions.is_empty() {
        return None;
    }

    let prefix = format!("'{issuer}");
    let mut qualified = String::new();
    let (mut line, mut column) = (1, 1);
    for c in source.chars() {
        if positions.contains(&(line, column)) {
            qualified.push_str(&prefix);
        }
        qualified.push(c);
        (line, column) = if c == '\n' {
            (line + 1, 1)
        } else {
            (line, column + 1)
        };
    }
    Some(qualified)
}

fn collect_relative_positions(
    expressions: &[PreSymbolicExpression],
    is_remapped: &impl Fn(&ContractName) -> bool,
    positions: &mut Vec<(u32, u32)>,
) {
    for expression in expressions {
        match &expression.pre_expr {
            PreSymbolicExpressionType::SugaredContractIdentifier(name)
            | PreSymbolicExpressionType::SugaredFieldIdentifier(name, _)
                if is_remapped(name) =>
            {
                positions.push((expression.span.start_line, expression.span.start_column));
            }
            PreSymbolicExpressionType::List(items) | PreSymbolicExpressionType::Tuple(items) => {
                collect_relative_positions(items, is_remapped, positions);
            }
            _ => {}
        }
    }
}

/// Whether `c` can continue a Stacks address or contract name.
fn is_identifier_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || matches!(c, '-' | '_')
}

/// The length of `from` if it occurs whole at the start of `rest`, i.e. not
/// followed by a character that would extend it into a longer identifier.
fn whole_match_len(rest: &str, from: &str) -> Option<usize> {
    let tail = rest.strip_prefix(from)?;
    (!tail.chars().next().is_some_and(is_identifier_char)).then_some(from.len())
}

/// Replace each whole occurrence of a `from` identifier with its `to`.
///
/// A single pass over the original source: at each identifier start the
/// longest matching `from` wins, so a contract ID takes precedence over a bare
/// remap of its issuer, and replaced text is never matched again, so `A -> B`
/// and `B -> C` send `A` to `B`. Identifiers continuing past a match are left
/// alone, so remapping `SP….nft-trait` does not touch `SP….nft-trait-v2`.
pub fn remap_contract_ids<S: AsRef<str>>(
    source: &str,
    pairs: impl IntoIterator<Item = (S, S)>,
) -> String {
    let mut pairs: Vec<_> = pairs
        .into_iter()
        .filter(|(from, _)| !from.as_ref().is_empty())
        .collect();
    if pairs.is_empty() {
        return source.to_string();
    }
    pairs.sort_by_key(|(from, _)| Reverse(from.as_ref().len()));

    let mut remapped = String::with_capacity(source.len());
    let mut previous = None;
    let mut index = 0;
    while let Some(rest) = source.get(index..).filter(|rest| !rest.is_empty()) {
        let at_identifier_start = !previous.is_some_and(is_identifier_char);
        let replacement = at_identifier_start
            .then(|| {
                pairs.iter().find_map(|(from, to)| {
                    whole_match_len(rest, from.as_ref()).map(|len| (len, from, to))
                })
            })
            .flatten();

        match replacement {
            Some((len, from, to)) => {
                remapped.push_str(to.as_ref());
                previous = from.as_ref().chars().next_back();
                index += len;
            }
            None => {
                let c = rest.chars().next().expect("rest is not empty");
                remapped.push(c);
                previous = Some(c);
                index += c.len_utf8();
            }
        }
    }
    remapped
}

/// Whether `source` contains `identifier` as a whole identifier, the same
/// occurrences [`remap_contract_ids`] would rewrite.
pub fn contains_identifier(source: &str, identifier: &str) -> bool {
    source.match_indices(identifier).any(|(start, _)| {
        let extends_before = source[..start]
            .chars()
            .next_back()
            .is_some_and(is_identifier_char);
        !extends_before && whole_match_len(&source[start..], identifier).is_some()
    })
}

#[cfg(test)]
mod tests {
    use indoc::formatdoc;

    use super::*;

    const NFT_TRAIT: &str = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait";
    const DEPLOYER: &str = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM";

    fn pairs(pairs: &[(&str, &str)]) -> Vec<(String, String)> {
        pairs
            .iter()
            .map(|(from, to)| (from.to_string(), to.to_string()))
            .collect()
    }

    #[test]
    fn only_whole_identifiers_are_rewritten() {
        let canonical = "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.nft-trait";
        let source = formatdoc! {r#"
            (impl-trait '{NFT_TRAIT}.nft-trait)
            (use-trait v2 '{NFT_TRAIT}-v2.nft-trait)
            (define-constant ID "{NFT_TRAIT}")
        "#};

        assert_eq!(
            remap_contract_ids(&source, pairs(&[(NFT_TRAIT, canonical)])),
            formatdoc! {r#"
                (impl-trait '{canonical}.nft-trait)
                (use-trait v2 '{NFT_TRAIT}-v2.nft-trait)
                (define-constant ID "{canonical}")
            "#},
        );
    }

    #[test]
    fn a_longer_address_is_not_rewritten() {
        let source = "(contract-call? 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9X.token f)";
        assert_eq!(
            remap_contract_ids(
                source,
                pairs(&[("SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9", DEPLOYER)])
            ),
            source,
        );
    }

    #[test]
    fn a_contract_override_beats_its_issuer_remap() {
        // A redeployed requirement remaps its issuer to the deployer, while a
        // dependency at the same issuer is remapped to an existing contract.
        let canonical = "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.canonical-trait";
        let source = formatdoc! {"
            (contract-call? '{NFT_TRAIT} f)
            (contract-call? 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.wrapper f)
        "};

        assert_eq!(
            remap_contract_ids(
                &source,
                pairs(&[
                    ("SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9", DEPLOYER),
                    (NFT_TRAIT, canonical),
                ])
            ),
            formatdoc! {"
                (contract-call? '{canonical} f)
                (contract-call? '{DEPLOYER}.wrapper f)
            "},
        );
    }

    #[test]
    fn replacements_do_not_cascade() {
        let old = "SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.old-trait";
        let canonical = "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT.canonical-trait";
        let other = format!("{DEPLOYER}.other-trait");
        let source = format!("(contract-call? '{old} f) (contract-call? '{canonical} f)");

        assert_eq!(
            remap_contract_ids(&source, pairs(&[(old, canonical), (canonical, &other)])),
            format!("(contract-call? '{canonical} f) (contract-call? '{other} f)"),
        );
    }

    #[test]
    fn non_ascii_text_is_preserved() {
        let source = format!(";; café ☕ {NFT_TRAIT}\n(define-constant é u1)");
        assert_eq!(
            remap_contract_ids(&source, pairs(&[(NFT_TRAIT, "ST1.x")])),
            ";; café ☕ ST1.x\n(define-constant é u1)",
        );
    }

    #[test]
    fn contains_matches_the_rewritten_occurrences() {
        assert!(contains_identifier(
            &format!("(define-constant c '{NFT_TRAIT})"),
            NFT_TRAIT
        ));
        assert!(!contains_identifier(
            &format!("(define-constant c '{NFT_TRAIT}-v2)"),
            NFT_TRAIT
        ));
        assert!(!contains_identifier(
            &format!("(define-constant c 'X{NFT_TRAIT})"),
            NFT_TRAIT
        ));
    }

    #[test]
    fn only_parsed_relative_references_to_remapped_contracts_are_qualified() {
        let original = QualifiedContractIdentifier::parse(&format!("{DEPLOYER}.wrapper")).unwrap();
        let base = format!("{DEPLOYER}.base");
        let remaps = ContractRemaps::from_iter([(
            QualifiedContractIdentifier::parse(&base).unwrap(),
            QualifiedContractIdentifier::parse(NFT_TRAIT).unwrap(),
        )]);
        let source = |base: &str| {
            formatdoc! {r#"
                (impl-trait {base}.sample)
                (contract-call? {base} f)
                (define-constant c .base-v2) ;; .base
                (define-constant s ".base")
            "#}
        };
        assert_eq!(
            qualify_remapped_relative_references(&source(".base"), &original, &remaps),
            Some(source(&format!("'{base}")))
        );
        assert_eq!(
            qualify_remapped_relative_references("(contract-call? .other f)", &original, &remaps),
            None
        );
    }

    #[test]
    fn an_address_remap_qualifies_every_relative_reference() {
        let original = QualifiedContractIdentifier::parse(&format!("{DEPLOYER}.wrapper")).unwrap();
        let mut remaps = ContractRemaps::default();
        remaps
            .insert_str(DEPLOYER, "ST1NXBK3K5YYMD6FD41MVNP3JS1GABZ8TRVX023PT")
            .unwrap();
        assert_eq!(
            qualify_remapped_relative_references(
                "(contract-call? .base f) (contract-call? .other f)",
                &original,
                &remaps
            ),
            Some(format!(
                "(contract-call? '{DEPLOYER}.base f) (contract-call? '{DEPLOYER}.other f)"
            ))
        );
    }
}
