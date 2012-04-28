;; Modify the lexical address translator and interpreter to handle let
;; expressions, procedures, and procedure calls with multiple
;; arguments, as in exercise 3.21. Do this using a nameless version of
;; the ribcage representation of environments (exercise 2.11). For
;; this representation, the lexical address will consist of two
;; nonnegative integers: the lexical depth, to indicate the number of
;; contours crossed, as before; and a position, to indicate the
;; position of the variable in th declaration.

;; see eopl.core.nameless-lang
