;; We suggested earlier the use of assignment to make a program more modular by allowing one procedure to communicate information to a distant procedure without requiring intermediate procedures to be aware of it. Very often such an assignment should only be temporary, lasting for the execution of a procedure call. Add ot the language a facility for dynamic assignment (also called fluid binding) to accomplish this. Use the production

;; Expression ::= setdynamic Identifier = Expression during Expression
;;                setdynamic-exp (var exp1 body)

;; The effect of the setdynamic expression is to assign temporarily
;; the value of exp to var, evaluate body, reassign var to its
;; original value, and return the value of body. the variable var must
;; already be bound

;; eopl.core.implicit-ref-lang

