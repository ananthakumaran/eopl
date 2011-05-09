;; Add to the environment interface an observer called has-binding?
;; that takes an environment env and a variable s and tests to see if
;; s has an associated value in env. Implement it using the a-list
;; representation

(ns eopl.chap-2.9
  (:use clojure.test)
  (:use eopl.chap-2.5)
  (:use eopl.chap-2.8))

(defn has-binding? [env s]
  (if (not (empty-env? env))
    (let [[var val] (first env)]
      (if (= var s)
        true
        (recur (rest env) s)))
    false))

(deftest has-binding-test
  (let [env (extend-env 'y "y"
                        (extend-env 'x "x" (empty-env)))]
    (is (has-binding? env 'x))
    (is (has-binding? env 'y))
    (is (not (has-binding? env 'z)))))

(run-tests)
