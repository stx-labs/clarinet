;; counter contract
(define-data-var count uint u0)
(define-map participants
  principal
  bool
)

(use-trait multiplier-trait .multiplier-trait.multiplier)

(define-constant OWNER tx-sender)

(define-read-only (get-count)
  (ok { count: (var-get count) })
)

(define-read-only (get-count-at-block (height uint))
  (ok (var-get count))
)

(define-private (inner-increment)
  (begin
    (print "call inner-increment")
    (if (is-none (map-get? participants tx-sender))
      (map-insert participants tx-sender true)
      (map-set participants tx-sender true)
    )
    (var-set count (+ (var-get count) u1))
  )
)

(define-public (increment)
  (begin
    (print "call increment")
    (try! (stx-transfer? u1000000 tx-sender (as-contract tx-sender)))
    (ok (inner-increment))
  )
)

(define-public (add (n uint))
  (begin
    (print "call add")
    (if (is-none (map-get? participants tx-sender))
      (map-insert participants tx-sender true)
      (map-set participants tx-sender true)
    )
    (try! (stx-transfer? u1000000 tx-sender (as-contract tx-sender)))
    (ok (var-set count (+ (var-get count) n)))
  )
)

(define-public (withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender OWNER) (err u1))
    (as-contract (stx-transfer? amount tx-sender OWNER))
  )
)

(define-public (call-multiply (multiplier-contract <multiplier-trait>))
  (ok (try! (contract-call? multiplier-contract multiply u2 u2)))
)

(define-public (transfer-100 (to principal))
  (stx-transfer? u100 tx-sender to)
)
