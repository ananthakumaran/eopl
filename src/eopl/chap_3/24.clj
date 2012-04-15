;; Use the tricks of the program above to write the pair of mutually
;; recursive procedures, odd and even, as in exercise 3.32

(ns eopl.chap-3.24
  (:use eopl.core.proc-lang)
  (:use clojure.test))

(deftest mutually-recursive-test
  (is (= (result "let _even = proc(e?)
                      proc(o?)
                       proc(n)
                        if zero?(n)
                        then true
                        else (((o? o?) e?) -(n,1))
           in let _odd = proc(o?)
                          proc(e?)
                           proc(n)
                           if zero?(n)
                           then false
                           else if equal?(n,1)
                           then true
                           else (((e? e?) o?) -(n,1))
             in let even = proc (n)
                            (((_even _even) _odd) n)
                in let odd = proc (n)
                              (((_odd _odd) _even) n)
                   in (list (even 4) (odd 4) (odd 5) (even 5))")
         '(true false true false))))

(run-tests)


