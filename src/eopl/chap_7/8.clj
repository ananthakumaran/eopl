;; Add pairof types to the language. Say that a value is of type
;; pairof t1 * t2 if and only if it is a pair consisting of a value of
;; type t1 and a value of type t2. Add to the language the following
;; productions:


;; Type ::= pairof Type * Type
;; ---------------------------
;;          pair-type (ty1 ty2)
;;
;; Expression ::= newpair (Expression , Expression)
;; ------------------------------------------------
;;                pair-exp (exp1 exp2)
;;
;; Expression ::= unpair Identifier Identifier = Expression
;; in Expression
;; ￼￼￼pair-exp (exp1 exp2)
;;----------------------------------------------------------
;;               unpair-exp (var1 var2 exp body)
;;

;; see eopl.core.checker-lang
￼￼￼￼
