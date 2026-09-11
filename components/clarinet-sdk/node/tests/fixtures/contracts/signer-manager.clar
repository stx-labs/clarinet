;; A minimal pox-5 signer manager, just enough to reach `stake`.
;;
;; `grant-signer-key` and `register-signer` both require
;; `contract-caller` to be the signer manager itself, so those calls have to
;; originate here rather than from the test.
;;
;; Written against the *mainnet* pox-5 address throughout, which is what a
;; developer targeting mainnet would type -- simnet rewrites it on deployment.

(use-trait signer-manager-trait 'SP000000000000000000002Q6VF78.pox-5.signer-manager-trait)
(impl-trait 'SP000000000000000000002Q6VF78.pox-5.signer-manager-trait)

(define-constant SELF (as-contract tx-sender))

;; Accept every staker. The real thing would gate membership here.
(define-public (validate-stake!
        (staker principal)
        (first-index uint)
        (num-indexes uint)
        (amount-ustx uint)
        (amount-sats uint)
        (is-bond bool)
        (signer-calldata (optional (buff 500)))
    )
    (ok true)
)

(define-public (grant (signer-key (buff 33)) (auth-id uint) (signer-sig (buff 65)))
    (contract-call? 'SP000000000000000000002Q6VF78.pox-5 grant-signer-key
        signer-key SELF auth-id signer-sig
    )
)

;; `self` is this contract, supplied by the caller. A contract cannot name
;; itself as a trait argument -- that is a circular reference at analysis time --
;; but it can pass one it was handed. `contract-caller` inside pox-5 is still
;; this contract, which is what `register-signer` checks.
(define-public (register (self <signer-manager-trait>) (signer-key (buff 33)))
    (contract-call? 'SP000000000000000000002Q6VF78.pox-5 register-signer
        self signer-key
    )
)
