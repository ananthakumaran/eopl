;; Add a numeric equality predicate equql? and numeric order
;; predicates greater? and less? to the set of operations in the
;; defined language

(ns eopl.chap-3.8
  (:use eopl.core.let-lang)
  (:use clojure.test))


(deftest comparision-test
  (is (= (result "less?(minus(1),minus(2))")
         false))
  (is (= (result "equal?(2,2)")
         true))
  (is (= (result "greater?(50,5)")
         true)))

(run-tests)
