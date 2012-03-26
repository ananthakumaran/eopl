;; Extend the language by adding a new operator minus that takes one
;; argument n and returns -n. For example, the value
;; minus(-(minus(5),9)) should be 14

(ns eopl.chap-3.6
  (:use eopl.core.let-lang)
  (:use clojure.test))

(deftest minus-test
  (is (= (result "minus(-(minus(5),9))")
         14)))

(run-tests)
