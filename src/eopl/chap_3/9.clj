;; Add list processing operations to the language, including cons,
;; car, cdr, null? and emptylist. A list should be able to contain any
;; expressed value, including another list. Give the definitions of
;; the expressed and denoted values of the language, as in section
;; 3.2.2. For example

(ns eopl.chap-3.9
  (:use eopl.core.let-lang)
  (:use clojure.test))


(deftest list-test
  (is (= (result "let x = 4
                  in cons(x,
                         cons(cons(-(x, 1),
                                   emptylist),
                               emptylist))")
         '(4 (3))))
  (is (= (result "car(cons(4,emptylist))")
         4))
  (is (= (result "car(emptylist)")
         '()))
  (is (= (result "cdr(emptylist)")
         '()))
  (is (= (result "null?(emptylist)")
         true))
  (is (= (result "cdr(cons(4,cons(4,emptylist)))")
         '(4))))

(run-tests)
