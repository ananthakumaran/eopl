;; Extend the language above to allow the declaration of a recursive
;; procedure of possibly many arguments, as in exercise 3.21

(ns eopl.chap-3.31
  (:use eopl.core.letrec-lang)
  (:use clojure.test))


(deftest letrec-multivars-test
  (is (= (result "letrec
                   number(x acc) =
                     if equal?(1,x)
                     then
                        cons(1,acc)
                     else
                        (number -(x,1) cons(x,acc))
                   in
                    (number 5 emptylist)")
         '(1 2 3 4 5))))

(run-tests)
