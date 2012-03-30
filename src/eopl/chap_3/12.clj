;; Add to the defined language a facility that adds a cond expression.
;; Use the grammar
;;
;; Expression ::= cond { Expression ==> Expression }* end
;;
;; In this expression, the expressions on the left-hand sides of the
;; ==> are evaluated in order until one of them returns a true
;; value. Then the value of the entire expression is the value of the
;; corresponding right hand expression. If none of the tests succeeds,
;; the expression should report an error.


(ns eopl.chap-3.12
  (:use eopl.core.let-lang)
  (:use clojure.test))


(deftest cond-test
  (is (= (result "let x = 100
           in
           cond zero?(x) ==> 0
                less?(x, 10) ==> 9
                greater?(x, 10) ==> 11
                equal?(x, 10) ==> 10
           end")
         11))
  (is (thrown-with-msg? Exception #"unhandled condition"
        (result "cond zero?(10) ==> 0 end"))))

(run-tests)

