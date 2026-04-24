use ::clarity::types::StacksEpochId;
use ::clarity::vm::ast::parser;
use ::clarity::vm::ast::stack_depth_checker::StackDepthLimits;
use ::clarity::vm::events::{FTEventType, NFTEventType, STXEventType, StacksTransactionEvent};
use ::clarity::vm::representations::PreSymbolicExpressionType::{self as PSEType, Comment};
use ::clarity::vm::representations::{PreSymbolicExpression, Span};
use serde_json::json;
use strum::{Display, EnumString};

use crate::analysis::annotation::AnnotationKind;
use crate::repl::clarity_values::value_to_string;

pub const CHECK_ENVIRONMENTS: [Environment; 2] = [Environment::OnChain, Environment::Simnet];

#[derive(Debug, Clone, Copy, EnumString, Display, PartialEq, Eq, Hash)]
#[strum(serialize_all = "lowercase")]
pub enum Environment {
    OnChain,
    Simnet,
}

pub fn serialize_event(event: &StacksTransactionEvent) -> serde_json::Value {
    match event {
        StacksTransactionEvent::SmartContractEvent(event_data) => json!({
            "type": "contract_event",
            "contract_event": {
                "contract_identifier": event_data.key.0.to_string(),
                "topic": event_data.key.1,
                "value": value_to_string(&event_data.value),
            }
        }),
        StacksTransactionEvent::STXEvent(STXEventType::STXTransferEvent(event_data)) => json!({
            "type": "stx_transfer_event",
            "stx_transfer_event": event_data.json_serialize()
        }),
        StacksTransactionEvent::STXEvent(STXEventType::STXMintEvent(event_data)) => json!({
            "type": "stx_mint_event",
            "stx_mint_event": event_data.json_serialize()
        }),
        StacksTransactionEvent::STXEvent(STXEventType::STXBurnEvent(event_data)) => json!({
            "type": "stx_burn_event",
            "stx_burn_event": event_data.json_serialize()
        }),
        StacksTransactionEvent::STXEvent(STXEventType::STXLockEvent(event_data)) => json!({
            "type": "stx_lock_event",
            "stx_lock_event": event_data.json_serialize()
        }),
        StacksTransactionEvent::NFTEvent(NFTEventType::NFTTransferEvent(event_data)) => json!({
            "type": "nft_transfer_event",
            "nft_transfer_event": {
                "asset_identifier": format!("{}", event_data.asset_identifier),
                "sender": format!("{}", event_data.sender),
                "recipient": format!("{}", event_data.recipient),
                "value": value_to_string(&event_data.value),
            }
        }),
        StacksTransactionEvent::NFTEvent(NFTEventType::NFTMintEvent(event_data)) => json!({
            "type": "nft_mint_event",
            "nft_mint_event": {
                "asset_identifier": format!("{}", event_data.asset_identifier),
                "recipient": format!("{}", event_data.recipient),
                "value": value_to_string(&event_data.value),
            }
        }),
        StacksTransactionEvent::NFTEvent(NFTEventType::NFTBurnEvent(event_data)) => json!({
            "type": "nft_burn_event",
            "nft_burn_event": {
                "asset_identifier": format!("{}", event_data.asset_identifier),
                "sender": format!("{}",event_data.sender),
                "value": value_to_string(&event_data.value),
            }
        }),
        StacksTransactionEvent::FTEvent(FTEventType::FTTransferEvent(event_data)) => json!({
            "type": "ft_transfer_event",
            "ft_transfer_event": event_data.json_serialize()
        }),
        StacksTransactionEvent::FTEvent(FTEventType::FTMintEvent(event_data)) => json!({
            "type": "ft_mint_event",
            "ft_mint_event": event_data.json_serialize()
        }),
        StacksTransactionEvent::FTEvent(FTEventType::FTBurnEvent(event_data)) => json!({
            "type": "ft_burn_event",
            "ft_burn_event": event_data.json_serialize()
        }),
    }
}

/// Returns the spans of all `#[env(simnet)]` annotated blocks in the source.
/// Each span covers from the annotation comment through the annotated expression.
pub fn get_env_simnet_spans(source: &str) -> Result<Vec<Span>, String> {
    let (pre_expressions, _diagnostics, success) = parser::v2::parse_collect_diagnostics(
        source,
        StackDepthLimits::for_epoch(StacksEpochId::latest()),
    );

    if !success {
        return Err("failed to parse pre_expressions from source".to_string());
    }

    let mut spans = Vec::new();
    collect_env_simnet_spans(&pre_expressions, &mut spans);
    Ok(spans)
}

pub fn remove_env_simnet(source: String) -> Result<(String, bool), String> {
    let spans = get_env_simnet_spans(&source)?;

    if spans.is_empty() {
        return Ok((source, false));
    }

    let mut lines = source.lines().map(Some).collect::<Vec<Option<&str>>>();
    for span in &spans {
        for i in span.start_line..=span.end_line {
            lines[(i - 1) as usize] = None;
        }
    }

    let mut result = String::new();
    for line in lines {
        if let Some(line) = line {
            result.push_str(line);
        }
        result.push('\n');
    }

    Ok((result, true))
}

fn collect_env_simnet_spans(exprs: &[PreSymbolicExpression], out: &mut Vec<Span>) {
    let mut annotation_start: Option<Span> = None;

    for (idx, expr) in exprs.iter().enumerate() {
        if let Some(start_span) = annotation_start.take() {
            if matches!(expr.pre_expr, Comment(_)) {
                // intermediate comment, keep accumulating
                annotation_start = Some(start_span);
            } else {
                out.push(Span {
                    start_line: start_span.start_line,
                    start_column: start_span.start_column,
                    end_line: expr.span.end_line,
                    end_column: expr.span.end_column,
                });
            }
            continue;
        }

        if let Comment(comment) = &expr.pre_expr {
            let is_env_annotation = comment
                .trim()
                .strip_prefix("#[")
                .and_then(|s| s.strip_suffix(']'))
                .is_some_and(|inner| matches!(inner.trim().parse(), Ok(AnnotationKind::Env(_))));
            if is_env_annotation {
                let annotation_line = expr.span.start_line;
                let is_eol_comment = exprs[..idx].iter().any(|prev| {
                    prev.span.end_line == annotation_line && !matches!(prev.pre_expr, Comment(_))
                });

                if !is_eol_comment {
                    annotation_start = Some(expr.span.clone());
                }
            }
        }

        // recurse into nested lists and tuples
        if let PSEType::List(children) | PSEType::Tuple(children) = &expr.pre_expr {
            collect_env_simnet_spans(children, out);
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;

    #[test]
    fn can_remove_env_simnet() {
        #[rustfmt::skip]
        let with_env_simnet = indoc!(r#"
            (define-public (mint (amount uint) (recipient principal))
                (begin
                    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
                    (minty-fresh amount recipient)
                )
            )
            ;; mint post comment

            ;; #[env(simnet)]
            (define-public (minty-fresh (amount uint) (recipient principal)) ;; eol
                (begin
                    (ft-mint? drachma amount recipient)
                )
            )
        "#);

        #[rustfmt::skip]
        let without_env_simnet = indoc!(r#"
            (define-public (mint (amount uint) (recipient principal))
                (begin
                    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
                    (minty-fresh amount recipient)
                )
            )
            ;; mint post comment







        "#);

        // test that we can remove a marked fn
        let (clean, found) =
            remove_env_simnet(with_env_simnet.to_string()).expect("remove_env_simnet failed");
        assert_eq!(clean, without_env_simnet);
        assert!(found);

        // test that nothing is removed if nothing is marked
        let (clean, found) =
            remove_env_simnet(without_env_simnet.to_string()).expect("remove_env_simnet failed");
        assert_eq!(clean, without_env_simnet);
        assert!(!found);
    }

    #[test]
    fn can_remove_nested_env_simnet() {
        #[rustfmt::skip]
        let with_env_simnet = indoc!(r#"
            (define-public (mint (amount uint) (recipient principal))
                (begin
                    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
                    ;; #[env(simnet)]
                    (minty-fresh amount recipient)
                    (ok true)
                )
            )
        "#);

        #[rustfmt::skip]
        let without_env_simnet = indoc!(r#"
            (define-public (mint (amount uint) (recipient principal))
                (begin
                    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)


                    (ok true)
                )
            )
        "#);

        let (clean, found) =
            remove_env_simnet(with_env_simnet.to_string()).expect("remove_env_simnet failed");
        assert_eq!(clean, without_env_simnet);
        assert!(found);
    }

    #[test]
    fn ignores_eol_env_simnet_annotation() {
        #[rustfmt::skip]
        let source = indoc!(r#"
            (define-public (mint (amount uint) (recipient principal))
                (begin
                    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY) ;; #[env(simnet)]
                    (ok true)
                )
            )
        "#);

        let (clean, found) =
            remove_env_simnet(source.to_string()).expect("remove_env_simnet failed");
        assert_eq!(clean, source);
        assert!(!found);
    }

    #[test]
    fn get_spans_top_level() {
        #[rustfmt::skip]
        let source = indoc!(r#"
            (define-public (mint (amount uint)) (ok true))

            ;; #[env(simnet)]
            (define-public (minty-fresh (amount uint) (recipient principal))
                (begin
                    (ft-mint? drachma amount recipient)
                )
            )
        "#);

        let spans = get_env_simnet_spans(source).unwrap();
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].start_line, 3);
        assert_eq!(spans[0].end_line, 8);
    }

    #[test]
    fn get_spans_nested() {
        #[rustfmt::skip]
        let source = indoc!(r#"
            (define-public (mint (amount uint) (recipient principal))
                (begin
                    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
                    ;; #[env(simnet)]
                    (minty-fresh amount recipient)
                    (ok true)
                )
            )
        "#);

        let spans = get_env_simnet_spans(source).unwrap();
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].start_line, 4);
        assert_eq!(spans[0].end_line, 5);
    }

    #[test]
    fn get_spans_eol_annotation_ignored() {
        #[rustfmt::skip]
        let source = indoc!(r#"
            (define-public (mint (amount uint) (recipient principal))
                (begin
                    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY) ;; #[env(simnet)]
                    (ok true)
                )
            )
        "#);

        let spans = get_env_simnet_spans(source).unwrap();
        assert!(spans.is_empty());
    }

    #[test]
    fn get_spans_no_annotations() {
        let source = "(define-read-only (get-owner) (ok tx-sender))";
        let spans = get_env_simnet_spans(source).unwrap();
        assert!(spans.is_empty());
    }

    #[test]
    fn get_spans_multiple() {
        #[rustfmt::skip]
        let source = indoc!(r#"
            ;; #[env(simnet)]
            (define-constant FIRST u1)

            (define-constant KEEP u2)

            ;; #[env(simnet)]
            (define-constant SECOND u3)
        "#);

        let spans = get_env_simnet_spans(source).unwrap();
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].start_line, 1);
        assert_eq!(spans[0].end_line, 2);
        assert_eq!(spans[1].start_line, 6);
        assert_eq!(spans[1].end_line, 7);
    }

    #[test]
    fn ignores_malformed_env_annotation_missing_hash() {
        #[rustfmt::skip]
        let source = indoc!(r#"
            ;; [env(simnet)]
            (define-public (set-admin (new-admin principal))
                (begin
                    (ok (var-set contract-owner new-admin))
                )
            )
        "#);

        let spans = get_env_simnet_spans(source).unwrap();
        assert!(spans.is_empty());
    }

    #[test]
    fn ignores_malformed_env_annotation_hash_inside_brackets() {
        #[rustfmt::skip]
        let source = indoc!(r#"
            ;; [#env(simnet)]
            (define-public (set-admin (new-admin principal))
                (begin
                    (ok (var-set contract-owner new-admin))
                )
            )
        "#);

        let spans = get_env_simnet_spans(source).unwrap();
        assert!(spans.is_empty());
    }
}
