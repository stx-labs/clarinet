use ::clarity::types::StacksEpochId;
use ::clarity::vm::ast::parser;
use ::clarity::vm::events::{FTEventType, NFTEventType, STXEventType, StacksTransactionEvent};
use ::clarity::vm::representations::PreSymbolicExpressionType::Comment;

use crate::repl::clarity_values::value_to_string;
use crate::repl::Epoch;

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

pub fn remove_env_simnet(epoch: Epoch, source: String) -> Result<String, String> {
    let (pre_expressions, mut _diagnostics, success) = if epoch >= StacksEpochId::Epoch21 {
        parser::v2::parse_collect_diagnostics(&source)
    } else {
        let parse_result = parser::v1::parse(&source);
        match parse_result {
            Ok(pre_expressions) => (pre_expressions, vec![], true),
            Err(error) => (vec![], vec![error.diagnostic], false),
        }
    };

    if !success {
        return Err("failed to parse pre_expressions from source".to_string());
    }

    let mut lines = source.lines().map(Some).collect::<Vec<Option<&str>>>();
    let mut found_env_simnet = false;
    for expr in &pre_expressions {
        // remove all comments and first non-comment
        if found_env_simnet {
            for i in expr.span.start_line..=expr.span.end_line {
                lines[(i - 1) as usize] = None;
            }
            if !matches!(expr.pre_expr, Comment(_)) {
                found_env_simnet = false;
            }
        }

        if let Comment(comment) = &expr.pre_expr {
            if comment.contains("#[env(simnet)]") {
                found_env_simnet = true;
                for i in expr.span.start_line..=expr.span.end_line {
                    lines[(i - 1) as usize] = None;
                }
            }
        }
    }

    let mut source = String::new();
    for line in lines.iter().flatten() {
        source.push_str(line);
        source.push('\n');
    }

    Ok(source)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::repl::DEFAULT_EPOCH;

    #[test]
    fn can_remove_env_simnet() {
        let with_env_simnet = r#"
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
"#;

        let without_env_simnet = r#"
(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (minty-fresh amount recipient)
    )
)
;; mint post comment

"#;

        let epoch = DEFAULT_EPOCH;
        let epoch = Epoch::Specific(epoch);

        // test that we can remove a marked fn
        let clean = remove_env_simnet(epoch.clone(), with_env_simnet.to_string())
            .expect("remove_env_simnet failed");
        assert_eq!(clean, without_env_simnet);

        // test that nothing is removed if nothing is marked
        let clean = remove_env_simnet(epoch.clone(), without_env_simnet.to_string())
            .expect("remove_env_simnet failed");
        assert_eq!(clean, without_env_simnet);
    }
}
