;; Extend the language above to allow the declaration of any number of
;; mutually recursive unary procedures, for example

(ns eopl.chap-3.32
  (:use eopl.core.letrec-lang)
  (:use clojure.test))

(deftest letrec-mutual-test
  (is (= (result "letrec
                    even(x) = if zero?(x) then true else (odd -(x,1))
                    odd(x) = if zero?(x) then false else (even -(x,1))
                  in (odd 13)")
         true)))
