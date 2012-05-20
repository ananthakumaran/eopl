;; Extend the language of this section to include procedures with
;; multiple arguments and calls with multiple operands, as suggested
;; by the grammar

;; Expression ::= proc ({Identifier}* ) Expression
;; Expression ::= (Expression {Expression}*)

(ns eopl.chap-3.21
  (:use eopl.core.proc-lang)
  (:use clojure.test))


(deftest proc-args-test
  (is (= (result "let f = proc (x y) -(x, y)
                  in (f 10 5)")
         5)
      )
  (is (= (result "letproc f (x y) -(x, y)
                  in (f 10 5)")
         5)))

(run-tests)

