use crate::repl::{
    clarity_values::value_to_string, ClarityCodeSource, ClarityContract, ContractDeployer, Epoch,
    Session, SessionSettings, DEFAULT_EPOCH,
};
use ::clarity::vm::{
    events::{FTEventType, NFTEventType, STXEventType, StacksTransactionEvent},
    types::QualifiedContractIdentifier,
    ClarityVersion,
};

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

pub fn remove_env_simnet(
    id: &QualifiedContractIdentifier,
    source: String,
) -> Result<String, String> {
    let mut settings = SessionSettings::default();
    settings.repl_settings.analysis.enable_all_passes();

    let session = Session::new(settings.clone());
    let epoch = DEFAULT_EPOCH;
    let contract = ClarityContract {
        code_source: ClarityCodeSource::ContractInMemory(source.clone()),
        deployer: ContractDeployer::Address(id.issuer.to_string()),
        name: id.name.to_string(),
        clarity_version: ClarityVersion::default_for_epoch(epoch),
        epoch: Epoch::Specific(epoch),
    };

    // parse AST
    let (mut ast, mut _diagnostics, success) = session.interpreter.build_ast(&contract);

    if !success {
        return Err("Failed to parse AST for contract {loc}".to_string());
    }

    // remove any top level exprs marked #[env(simnet)]
    let mut exprs = Vec::new();
    let mut lines = source
        .lines()
        .map(|line| Some(line))
        .collect::<Vec<Option<&str>>>();
    for expr in &ast.expressions {
        let mut is_env_simnet = false;
        for (text, _span) in &expr.pre_comments {
            if text.find("#[env(simnet)]").is_some() {
                is_env_simnet = true;
                break;
            }
        }

        if !is_env_simnet {
            exprs.push(expr.clone());
        } else {
            for i in expr.span.start_line..=expr.span.end_line {
                lines[(i - 1) as usize] = None;
            }

            for (_, span) in &expr.pre_comments {
                for i in span.start_line..=span.end_line {
                    lines[(i - 1) as usize] = None;
                }
            }

            for (_, span) in &expr.post_comments {
                for i in span.start_line..=span.end_line {
                    lines[(i - 1) as usize] = None;
                }
            }
        }
    }

    ast.expressions = exprs;
    let mut source = String::new();
    for line in &lines {
        if let Some(line) = line {
            source.push_str(line);
            source.push('\n');
        }
    }

    Ok(source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_remove_env_simnet() {
        let with_env_simnet = r#"
(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (ft-mint? drachma amount recipient)
    )
)

;; #[env(simnet)]
(define-public (minty-fresh (amount uint) (recipient principal)) ;; eol
    (begin
        (ft-mint? drachma amount recipient)
    )
)
;; post comment
"#;

        let without_env_simnet = r#"
(define-public (mint (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (ft-mint? drachma amount recipient)
    )
)

"#;

        let contract_id = QualifiedContractIdentifier::transient();

        // test that we can remove a marked fn
        let clean = remove_env_simnet(&contract_id, with_env_simnet.to_string())
            .expect("remove_env_simnet failed");
        assert_eq!(clean, without_env_simnet);

        // test that nothing is removed if nothing is marked
        let clean = remove_env_simnet(&contract_id, without_env_simnet.to_string())
            .expect("remove_env_simnet failed");
        assert_eq!(clean, without_env_simnet);
    }
}
