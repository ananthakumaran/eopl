;; Extend the language with a let* expression that works like the
;; scheme let*


(ns eopl.chap-3.17
  (:use eopl.core.let-lang)
  (:use clojure.test))

(deftest let*-test
  (is (= (result "let x = 30
                  in let* x = -(x,1)
                         y = -(x,2)
                     in -(x,y)")
         2)))

(run-tests)
