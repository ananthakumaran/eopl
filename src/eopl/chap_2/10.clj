;; Add to the environment interface a constructor extend-env*, and
;; implement it using the a-list representation. This constructor
;; takes a list of variables, a list of values of the same length, and
;; an enviroment, and is specifieb by
;; (extend-env* (var1 .. vark) (val1 .. valk) [f]) = [g],
;; where g(var) = { vali,  if var = vari for some i such that 1 <= i <= k
;;                { f(var) otherwise
;;

(ns eopl.chap-2.10
  (:use clojure.test)
  (:use eopl.chap-2.5)
  (:use eopl.chap-2.8)
  (:use eopl.chap-2.9))

(defn extend-env* [env vars vals]
  (if (seq vars)
    (let [new-env (extend-env (first vars)
                              (first vals)
                              env)]
      (extend-env* new-env (rest vars) (rest vals)))
    env))

(deftest extend-env*-test
  (is (= (extend-env* '() '(a b) '(1 2)) '([b 2] [a 1]))))

(run-tests)
