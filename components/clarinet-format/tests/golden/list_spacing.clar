;; max_line_length: 80, indentation: 2
(define-private (is-sorted-principals? (plist (list 20 principal)))
  (let ((n (len plist)))
    (if (> n u20)
      false
      (if (<= n u1)
        true ;; 0/1 trivial
        (get sorted
          (fold sorted-fold-step
            (list
              u0               u1               u2               u3
                            u4               u5               u6               u7
                            u8               u9
              u10               u11               u12               u13               u14
                            u15               u16               u17
            ) {
            sorted: true,
            plist: plist,
            n: n,
          })
        )
      )
    )
  )
)
