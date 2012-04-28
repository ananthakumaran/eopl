(ns eopl.core.nameless-env)

(defn empty-env []
  '())

(defn environment? [x]
  (seq? x))

(defn empty-env? [env]
  (empty? env))

(defn extend-env [env vals]
  (if (not (coll? vals))
    (throw (Exception. (str "Expected type collection " vals)))
    (conj env vals)))


(defn atom? [x]
  (instance? clojure.lang.Atom x))

(defn extend-env-rec [env proc count]

  (let [place-holders (map (fn [i] (atom :new-env)) (range count))
        new-env (extend-env env (apply vector place-holders))]
    (doseq [[i place-holder] (map vector (range count) place-holders)]
      (reset! place-holder (proc i new-env)))
    new-env))

(defn apply-env [env [n1 n2]]
  (let [val (nth (nth env n1) n2)]
    (if (atom? val) (deref val) val)))


