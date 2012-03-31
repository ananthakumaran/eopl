;; Add an expression to the defined language

;; Expression ::= unpack { Identifier }* = Expession in Expression

;; so that unpack x y z = list in ... binds x, y, and z to the
;; elements of lst if lst is a list of exactly three elements and
;; reports an error otherwise. For example

(ns eopl.chap-3.18
  (:use eopl.core.let-lang)
  (:use clojure.test))


(deftest unpack-test
  (is (= (result "let u = 7
                  in unpack x y = cons(u, cons(3, emptylist))
                     in -(x,y)")
         4)))

(run-tests)


