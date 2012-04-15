;; The tricks of the previous exercises can be generalized to show
;; that we can define any recursive procedure in PROC. Consider the
;; following bit of code;

(ns eopl.chap-3.25
  (:use eopl.core.proc-lang)
  (:use clojure.test))

(deftest recursive-test
  (is (= (result "let makerec = proc (f)
                                 let d = proc (x)
                                          proc (z) ((f (x x)) z)
                                    in proc (n) ((f (d d)) n)
                  in let maketimes4 = proc (f)
                                        proc (x)
                                          if zero?(x)
                                          then 0
                                          else -((f -(x,1)), minus(4))
                    in let times4 = (makerec maketimes4)
                       in (times4 3)")
         12)))

(run-tests)

;; show that is returns 12

;; -- another y combinator impl
