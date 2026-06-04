;; https://github.com/alexgo-io/alex-v1/blob/dev/clarity/contracts/stx404-token/token-stx404.clar#L60-L94
(define-public (transfer
        (amount-or-id uint)
        (sender principal)
        (recipient principal)
    )
    (begin
        (asserts! (is-eq sender tx-sender) err-not-authorised)
        (if (<= amount-or-id max-supply) ;; id transfer
            (let (
                    (check-id (asserts! (is-id-owned-by-or-default amount-or-id sender)
                        err-invalid-id
                    ))
                    (owned-by-sender (get-owned-or-default sender))
                    (owned-by-recipient (get-owned-or-default recipient))
                    (id-idx (unwrap-panic (index-of? owned-by-sender amount-or-id)))
                )
                (map-set owned sender (pop owned-by-sender id-idx))
                (map-set owned recipient
                    (unwrap-panic (as-max-len? (append owned-by-recipient amount-or-id) u10000))
                )
                (try! (ft-transfer? stx404 one-8 sender recipient))
                (try! (nft-transfer? stx404nft amount-or-id sender recipient))
                (ok true)
            )
            (let (
                    (balance-sender (unwrap-panic (get-balance sender)))
                    (balance-recipient (unwrap-panic (get-balance recipient)))
                    (check-balance (try! (ft-transfer? stx404 amount-or-id sender recipient)))
                    (no-to-treasury (- (/ balance-sender one-8)
                        (/ (- balance-sender amount-or-id) one-8)
                    ))
                    (no-to-recipient (- (/ (+ balance-recipient amount-or-id) one-8)
                        (/ balance-recipient one-8)
                    ))
                    (owned-by-sender (get-owned-or-default sender))
                    (owned-by-recipient (get-owned-or-default recipient))
                    (ids-to-treasury (if (is-eq no-to-treasury u0)
                        (list)
                        (unwrap-panic (slice? owned-by-sender
                            (- (len owned-by-sender) no-to-treasury)
                            (len owned-by-sender)
                        ))
                    ))
                    (new-available-ids (if (is-eq no-to-treasury u0)
                        (var-get available-ids)
                        (unwrap-panic (as-max-len?
                            (concat (var-get available-ids) ids-to-treasury)
                            u10000
                        ))
                    ))
                    (ids-to-recipient (if (is-eq no-to-recipient u0)
                        (list)
                        (unwrap-panic (slice? new-available-ids
                            (- (len new-available-ids) no-to-recipient)
                            (len new-available-ids)
                        ))
                    ))
                )
                (var-set sender-temp sender)
                (var-set recipient-temp (as-contract tx-sender))
                (and
                    (> no-to-treasury u0)
                    (try! (fold check-err (map nft-transfer-iter ids-to-treasury)
                        (ok true)
                    ))
                )
                (var-set sender-temp (as-contract tx-sender))
                (var-set recipient-temp recipient)
                (and
                    (> no-to-recipient u0)
                    (try! (fold check-err (map nft-transfer-iter ids-to-recipient)
                        (ok true)
                    ))
                )
                (map-set owned sender
                    (if (is-eq no-to-treasury u0)
                        owned-by-sender
                        (unwrap-panic (slice? owned-by-sender u0
                            (- (len owned-by-sender) no-to-treasury)
                        ))
                    ))
                (map-set owned recipient
                    (if (is-eq no-to-recipient u0)
                        owned-by-recipient
                        (unwrap-panic (as-max-len? (concat owned-by-recipient ids-to-recipient)
                            u10000
                        ))
                    ))
                (var-set available-ids
                    (if (is-eq no-to-recipient u0)
                        new-available-ids
                        (unwrap-panic (slice? new-available-ids u0
                            (- (len new-available-ids) no-to-recipient)
                        ))
                    ))
                (ok true)
            )
        )
    )
)
