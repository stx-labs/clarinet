;; A simple counter contract for the DAP debug demo.
;; Set breakpoints on any line inside `safe-increment` or `add` to watch
;; Clarity execution pause mid-function.

(define-data-var count uint u0)

(define-read-only (get-count)
  (var-get count)
)

(define-private (safe-increment (current uint))
  (let ((next (+ current u1)))
    (asserts! (< next u1000) (err u1))
    (ok next)
  )
)

(define-public (increment)
  (let (
      (current (var-get count))
      (next (try! (safe-increment current)))
    )
    (var-set count next)
    (ok next)
  )
)

(define-public (add (n uint))
  (let (
      (current (var-get count))
      (result (+ current n))
    )
    (var-set count result)
    (ok result)
  )
)

(define-public (reset)
  (begin
    (var-set count u0)
    (ok true)
  )
)
