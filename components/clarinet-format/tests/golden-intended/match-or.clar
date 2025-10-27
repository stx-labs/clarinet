;; Determines if a character is a vowel (a, e, i, o, u, and y).
(define-private (is-vowel (char (buff 1)))
  (or
    (is-eq char 0x61) ;; a
    (is-eq char 0x65) ;; e
    (is-eq char 0x69) ;; i
    (is-eq char 0x6f) ;; o
    (is-eq char 0x75) ;; u
    (is-eq char 0x79) ;; y
  )
)

;; pre comment
(define-private (something)
  (match opt
    value (ok (handle-new-value value))
    (ok 1)
  )
)

(define-read-only (is-borroweable-isolated (asset principal))
  (match (index-of? (contract-call? .pool-reserve-data get-borroweable-isolated-read)
    asset
  )
    res true
    false
  )
)
