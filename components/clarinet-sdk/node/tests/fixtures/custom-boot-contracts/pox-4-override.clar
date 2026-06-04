;; `sentinel` does not exist on the default pox-4 — calling it succeeds
;; only when this override has actually replaced the boot source.
(define-read-only (sentinel) (ok "override-active"))
