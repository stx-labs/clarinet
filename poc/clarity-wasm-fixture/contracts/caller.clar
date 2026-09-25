(define-public (bump-counter)
  (contract-call? .counter increment))

(define-public (add-to-counter (n uint))
  (contract-call? .counter add n))

(define-read-only (read-counter)
  (contract-call? .counter get-count))

(define-read-only (pox-info)
  (contract-call? 'ST000000000000000000002AMW42H.pox-4 get-pox-info))

(define-read-only (reward-cycle-of (height uint))
  (contract-call? 'ST000000000000000000002AMW42H.pox-4 burn-height-to-reward-cycle height))

(define-public (unwrap-none)
  (ok (unwrap-panic (get-nothing))))

(define-private (get-nothing)
  (if true none (some u1)))

(define-public (divide-by-zero (a uint))
  (ok (/ a u0)))
