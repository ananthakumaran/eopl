;; Add arrays to this language. Introduce new operators newarray,
;; arrayref, and arrayset that create, dereference, and update
;; arrays. This leads to


;; ArrVal = (Ref(ExpVal))∗
;; ExpVal = Int + Bool + Proc + ArrVal DenVal = Ref(ExpVal)


;; Since the locations in an array are consecutive, use a
;; representation like the second representation above. What should be
;; the result of the following program?


;; see eopl.core.mutable-pair-lang
