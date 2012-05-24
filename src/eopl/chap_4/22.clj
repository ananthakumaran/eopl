;; So far our languages have been expression-oriented: the primary
;; syntactic category of interest has been expressions and we have
;; primarily been inter- ested in their values. Extend the language to
;; model the simple statement-oriented language whose specification is
;; sketched below. Be sure to Follow the Grammar by writing separate
;; procedures to handle programs, statements, and expressions.

;; Program ::= Statement
;; Statement ::= Identifier = Expression
;;           ::= print Expression
;;           ::= {{Statement}∗(;) }
;;           ::= if Expression Statement Statement
;;           ::= while Expression Statement
;;           ::= var {Identifier}∗(,) ; Statement


;; see eopl.core.statement-lang
