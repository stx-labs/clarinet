;; Stacks the contract's own balance through the *mainnet* PoX address, which
;; is what a developer writing for mainnet would naturally type. Simnet's chain
;; state is testnet-flavored, so the principal is rewritten to
;; ST000000000000000000002AMW42H at deployment time and the lock actually
;; happens. See https://github.com/stx-labs/clarinet/issues/2491
;;
;; `as-contract` makes this contract both tx-sender and contract-caller, so
;; pox-3 locks its own STX.
(define-public (stack (amount uint))
    (as-contract
        (contract-call? 'SP000000000000000000002Q6VF78.pox-3 stack-stx
            amount
            { version: 0x00, hashbytes: 0x7321b74e2b6a7e949e6c4ad313035b1665095017 }
            burn-block-height
            u1)))
