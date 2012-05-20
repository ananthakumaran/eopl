;; In the language of this section, all variables are mutable, as they
;; are in scheme. Another alternative is to allow both mutable and
;; immutable variable bindings.

;; ExpVal = Int + Bool + Proc
;; DenVal = Ref(ExpVal) + ExpVal

;; Variable assignment should world only when the variable to be
;; assigned to has a mutable binding. Dereferencing occurs implicitly
;; when the denoted value is a reference.

;; Modify the language of this section so that let introduces immutable variables, as before, but mutable variables are introduced by a letmutable expression, with syntax given by

;; Expression ::= letmutable Identifer = Expression in Expression

;; set eopl.core.mixed-ref-lang
