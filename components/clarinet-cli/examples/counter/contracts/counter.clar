
;; counter
;; let's get started with smart contracts
(define-data-var counter uint u1)

(define-public (increment (step uint))
    (let ((current (var-get counter)))
        (asserts! (<= step (- u340282366920938463463374607431768211455 current)) (err u1))
        (let ((new-val (+ step current)))
            (var-set counter new-val)
            (print { object: "counter", action: "incremented", value: new-val })
            (ok new-val))))

(define-public (decrement (step uint))
    (let ((current (var-get counter)))
        (asserts! (>= current step) (err u2))
        (let ((new-val (- current step)))
            (var-set counter new-val)
            (print { object: "counter", action: "decremented", value: new-val })
            (ok new-val))))

(define-read-only (read-counter)
    (ok (var-get counter)))
