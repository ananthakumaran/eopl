;; An alternative design that also avoids the linear search in
;; apply-handler is to use two continuations, a normal continuation
;; and an exception continuation. Achieve this goal by modifying the
;; interpreter of this section to take two continuations instead of
;; one

;; see eopl.core.exception-lang
