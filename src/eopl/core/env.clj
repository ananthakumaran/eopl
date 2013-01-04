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

(defn extend-env-rec [env proc names]
  (let [place-holders (map (fn [name] [name (atom :new-env)]) names)
        new-env (reduce conj
                        env
                        place-holders)]
    (doseq [[name place-holder] place-holders]
      (reset! place-holder (proc name new-env)))
    new-env))

(defn has-binding? [env search-var]
  (boolean (env search-var)))


(defrecord Module [name val env])
(defrecord TModule [name interface])

(defn apply-env [env search-var]
  (let [val (env search-var)]
    (if val
      (cond (atom? val) (deref val)
            (instance? Module val) (throw (Exception. "not implemented"))
            (instance? TModule val) (throw (Exception. "not implemented"))
            :else val)
      (throw (IllegalAccessException. (str "var " search-var " not found in env"))))))

(deftest env-test
  (let [env (extend-env (extend-env (empty-env) 'x "x") 'y "y")]
    (is (= (apply-env env 'y) "y"))
    (is (= (apply-env env 'x) "x"))
    (is (thrown? IllegalAccessException (apply-env env 'z)))))

(run-tests)
