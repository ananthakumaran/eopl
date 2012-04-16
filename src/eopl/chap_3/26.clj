;; In our data-structure representation of procedures, we have kept
;; the entire environment in the closure. But of course all we need
;; are the bindings for the free variables. Modify the representation
;; of procedures to retains only the free variables


;; see eopl.core.proc-lang/fv
