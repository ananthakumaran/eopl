;; Extend the language by adding a new operation print that takes one
;; argument, prints it and returns the integer 1. Why is this
;; operation not expressible in our specification framework

(ns eopl.chap-3.15
  (:use eopl.core.let-lang)
  (:use clojure.test))

(deftest print-test
  (is (= (result "print(list(1,2,3))")
       1)))
