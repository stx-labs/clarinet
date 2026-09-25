(define-constant ERR_TOO_BIG (err u100))

(define-data-var count uint u0)
(define-map totals principal uint)

(define-public (increment)
  (let ((next (+ (var-get count) u1)))
    (var-set count next)
    (map-set totals tx-sender (+ (default-to u0 (map-get? totals tx-sender)) u1))
    (print { event: "increment", count: next, sender: tx-sender })
    (ok next)))

(define-public (add (n uint))
  (if (> n u10)
    ERR_TOO_BIG
    (begin
      (var-set count (+ (var-get count) n))
      (ok (var-get count)))))

(define-read-only (get-count)
  (var-get count))

(define-read-only (get-total (who principal))
  (default-to u0 (map-get? totals who)))

(define-read-only (divide (a uint) (b uint))
  (/ a b))

(define-read-only (panic-none)
  (unwrap-panic (as-max-len? (list u1 u2) u1)))
