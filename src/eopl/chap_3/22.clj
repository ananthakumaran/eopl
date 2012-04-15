;; The concrete syntax of this section uses different syntax for a
;; built-in operation, such as difference, from a procedure
;; call. Modify the concrete syntax so that the user of this language
;; need not know which operations are built-in and which are defined
;; procedures.

(ns eopl.chap-3.22
  (:use eopl.core.proc-lang)
  (:use clojure.test))

(deftest concrete-syntax-test
  (is (= (result "(* 0 1)")
         (result "*(0,1)")))
  (is (= (result "-(1,2)")
         (result "(- 1 2)")))
  (is (= (result "zero?(0)")
         (result "(zero? 0)")))
  (is (= (result "list(1,2,3)")
         (result "(list 1 2 3)")))
  (is (= (result "(cons 1 emptylist)")
         (result "cons(1,emptylist)"))))

(run-tests)
