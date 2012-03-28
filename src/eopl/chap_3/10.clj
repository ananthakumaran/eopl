;; Add an operation list to the language. This operation should take
;; any number of arguments, and return an expressed value containing
;; the list of their values.


(ns eopl.chap-3.10
  (:use eopl.core.let-lang)
  (:use clojure.test))

(deftest list-test
  (is (= (result "let x = 4
                  in list(x, -(x,1), -(x,3))")
         '(4 3 1)))
  (is (= (result "list()")
         '()))
  (is (= (result "let x = 4
                  in list(x)")
         '(4)))
  (is (= (result "let x = 10
                  in list(10, list(x, 5))")
         '(10 (10 5)))))

(run-tests)
