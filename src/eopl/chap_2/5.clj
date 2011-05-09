;; We can use any data structure for representing environments, if we
;; can distinguish empty environments from non-empty-ones, and in
;; which one can extract the pieces of a non-empty
;; environment. Implement environments using a representation in which
;; the empty environment is represented as the empty list, and in which
;; extend-env builds an environment like a a-list representation

(ns eopl.chap-2.5
  (:use clojure.test))

(defn empty-env []
  '())

(defn extend-env [var val env]
  (cons [var val] env))

(defn apply-env [env search-var]
  (if (seq env)
    (let [[var val] (first env)]
      (if (= var search-var)
        val
        (recur (rest env) search-var)))
    (throw (IllegalAccessError. (str "var " search-var " not found in env")))))


(deftest env-test
  (let [env (extend-env 'y "y"
                        (extend-env 'x "x" (empty-env)))]
    (is (= (apply-env env 'y) "y"))
    (is (= (apply-env env 'x) "x"))
    (is (thrown? IllegalAccessException (apply-env env 'z)))))

(run-tests)
