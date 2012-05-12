;; Extend the interpreter of the preceding exercise to have procedures
;; of multiple arguments

(ns eopl.chap-4.13
  (:use clojure.test)
  (:use eopl.core.store-pass-lang))

(deftest store-pass-test
  (is (= (result "let y = proc (a b) b
                  in let r = newref(0)
                     in (y setref(r, 11) de-ref(r))")
         11)))

(run-tests)
