(ns eopl.core.senv)

(defn empty-env []
  '())

(defn extend-env [env vars]
  (cons vars env))

(defn apply-env [env var]
  (if (empty? env)
    (throw (IllegalAccessException. (str "var " var " not found in env")))
    (let [index (.indexOf (first env) var)]
      (if (>= index 0)
        [0 index]
        (let [[n1 n2] (apply-env (rest env) var)]
          [(+ n1 1) n2])))))
