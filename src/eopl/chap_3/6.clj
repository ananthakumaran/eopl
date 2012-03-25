;; Extend the language by adding operators for addition,
;; multiplication and integer quotient

(ns eopl.chap-3.6
  (:use eopl.core.let-lang)
  (:use clojure.test))

(deftest minus-test
  (is (= (result "+(1,2)")
         3))
  (is (= (result "*(2,2)")
         4))
  (is (= (result "/(10,5)")
         2)))

(run-tests)
