;; Extend the language so that a let declaration can declare an
;; arbitrary number of variables, using the grammar

;; Expression ::= let { Identifier = Expression }* in Expression

;; As in scheme's let, each of the right hand sides is evaluated in the current enviroment, and the body is evaluated with each new variable bound to the values of its associated right-hand side. For example,


(ns eopl.chap-3.16
  (:use eopl.core.let-lang)
  (:use clojure.test))

(deftest let-multiple-test
  (is (= (result "let x = 30
                  in let x = -(x,1)
                         y = -(x,2)
                     in -(x,y)")
         1)))

(run-tests)
