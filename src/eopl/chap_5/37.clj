(ns eopl.chap-5.37
  (:use eopl.core.exception-lang)
  (:use clojure.test))

;; Modify the defined language to raise an exception when a procedure
;; is called with the wrong number of arguments


(deftest wrong-number-of-args
  (is (= (with-out-str
           (result "let f = proc(x) x
                       in (f 5 5)"))
         "unhandled exception Wrong number of arguments expected 1 actual 2")))


