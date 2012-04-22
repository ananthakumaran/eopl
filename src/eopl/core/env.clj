(ns eopl.core.env
  (:use clojure.test))

(defn empty-env []
  {})

(defn environment? [x]
  (map? x))

(defn empty-env? [env]
  (empty? env))

(defn extend-env [env var val]
  (conj env [var val]))

(defn atom? [x]
  (instance? clojure.lang.Atom x))

(defn extend-env-rec [env name proc]
  (let [place-holder (atom :new-env)
        new-env (conj env [name place-holder])]
    (reset! place-holder (proc new-env))
    new-env))

(defn has-binding? [env search-var]
  (boolean (env search-var)))

(defn apply-env [env search-var]
  (let [val (env search-var)]
    (if val
      (if (atom? val) (deref val) val)
      (throw (IllegalAccessException. (str "var " search-var " not found in env"))))))

(deftest env-test
  (let [env (extend-env (extend-env (empty-env) 'x "x") 'y "y")]
    (is (= (apply-env env 'y) "y"))
    (is (= (apply-env env 'x) "x"))
    (is (thrown? IllegalAccessException (apply-env env 'z)))))

(run-tests)
