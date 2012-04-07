;; In PROC, procedures have only one argument, but one can get the
;; effect of multiple argument procedures by using procedures that
;; return other procedures. For example, one might write code like

;; let f = proc (x) proc (y) ..
;; in ((f 3) 4)

;; This trick is called currying, and the procedure is said to be
;; curried. Write a curried procedure that takes tow arguments and
;; returns their sum. You can write x + y in our language by writing
;; -(x, -(0,y))

(ns eopl.chap-3.20
  (:use eopl.core.proc-lang)
  (:use clojure.test))

(deftest curry-test
  (is (= (result "let sum = proc (x) proc (y) -(x, -(0,y))
                  in ((sum 3) 4)")
         7)))



