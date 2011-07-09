(ns eopl.core.env
  (:use clojure.test))

(defn empty-env []
  {})

(defn empty-env? [env]
  (empty? env))

(defn extend-env [env var val]
  (conj env [var val]))

(defn has-binding? [env search-var]
  (boolean (env search-var)))

(defn apply-env [env search-var]
  (let [val (env search-var)]
    (if val
      val
      (throw (IllegalAccessException. (str "var " search-var " not found in env"))))))

(deftest env-test
  (let [env (extend-env (extend-env (empty-env) 'x "x") 'y "y")]
    (is (= (apply-env env 'y) "y"))
    (is (= (apply-env env 'x) "x"))
    (is (thrown? IllegalAccessException (apply-env env 'z)))))

(run-tests)
