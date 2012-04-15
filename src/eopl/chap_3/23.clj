(ns eopl.chap-3.23
  (:use eopl.core.proc-lang)
  (:use clojure.test))


;; What is the value of the following PROC program?

(deftest self-test
  (is (= (result "let makemult = proc (maker)
                          proc (x)
                            if zero?(x)
                            then 0
                            else -(((maker maker) -(x,1)), minus(4))
          in let times4 = proc (x) ((makemult makemult) x)
             in (times4 3)")
         12))

;; Use the trucks of this program to write a procedure for factorial in PROC. As a hint,
;; remember that you can use Currying to define a two argument procedure times.

  (is (= (result "let _fact = proc (f)
                                    proc (n)
                                      if less?(n,2)
                                      then 1
                                      else +(((f f) -(n,1)), n)
                      in let factorial = proc (n) ((_fact _fact) n)
                         in (factorial 5)")
         15))
  (is (= (result "let y = proc (f)
                  (proc (x)
                    (f proc (v)
                       ((x x) v))
                   proc (x)
                    (f proc (v)
                      ((x x) v)))
           in let fact = (y proc (cont)
                              proc (n)
                                if less?(n,2)
                                then 1
                                else +((cont -(n,1)), n))
            in (fact 5)")
         15)))

(run-tests)
