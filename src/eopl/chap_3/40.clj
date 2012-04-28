;; Extend the lexical address translator and interpretor to handle
;; letrec. Do this by modifying the context argument to translation-of
;; so that it keeps track of not only the name of each bound variable,
;; but also whether it was bound by letrec or not. For a reference to
;; a variable that was bound by a letrec, then continue to use the
;; nameless environment representation above, and the interpreter can
;; do the right thing with a nameless-letrec-var-exp

;; see eopl.core.nameless-lang

;; NOTE: The letrec implemented in different way The translator
;; extends the senv with the recursive procedure names so the var
;; reference to proc will be handle normally. The interpreter uses the
;; extend-env-rec to extend env so that the reference to proc will be
;; point to the correct place.
