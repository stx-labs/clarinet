(define-data-var count uint u0)

(define-constant MAX_COUNT u100)

(define-map test-map uint uint)

(define-public (increment)
  (ok (var-set count (+ (var-get count) u1)))
)

(define-public (set-map-entry (key uint) (value uint))
  (ok (map-set test-map key value))
)
