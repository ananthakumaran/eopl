;; A naive implementation of extend-env* from the preceding exercise
;; requires time propotional to k to run. It is possible to represent
;; environments so that extend-env* requires only constant time:
;; represent the empty environment by the empty list, and represent a
;; non-empty environment by the date structure (fig) such an
;; environment might look like ribcage representation. The environment
;; is represented as a list of pairs called ribs; each left rib is a
;; list of variables and each right rib is the corresponding list of
;; values.

;; Implement the environment interface, including extend-env*, in this
;; representation.

(ns eopl.chap-2.11
  (:use clojure.test))

(defn empty-env []
  '())

(defn empty-env? [env]
  (= env (empty-env)))

(defn extend-env [env var val]
  (cons [`(~var) `(~val)] env))

(defn has-binding? [env search-var]
  (if (not (empty-env? env))
    (let [pair (first env)
          r (find (zipmap (first pair)
                          (second pair))
                  search-var)]
      (if r
        (second r)
        (recur (rest env) search-var)))
    false))

(defn extend-env* [env vars vals]
  (cons [vars vals] env))

(defn apply-env [env search-var]
  (if (empty-env? env)
    (throw (IllegalAccessException. (str "var " search-var " not found in env")))
    (let [pair (first env)
          r (find (zipmap (first pair)
                          (second pair))
                  search-var)]
      (if r
        (second r)
        (recur (rest env) search-var)))))

(deftest env-test
  (let [env (extend-env (extend-env (empty-env) 'x "x") 'y "y")]
    (is (= (apply-env env 'y) "y"))
    (is (= (apply-env env 'x) "x"))
    (is (= (apply-env (extend-env* env '(a b) '(1 2)) 'a) 1))
    (is (thrown? IllegalAccessException (apply-env env 'z)))))

(run-tests)
