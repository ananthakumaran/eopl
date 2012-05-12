(ns eopl.core.world-ref
  (:use clojure.test))

(def empty-store [])

(def reference? number?)

(defn newref [val world]
  (let [new-world (conj world val)]
    [(count world) new-world]))

(defn de-ref [ref world]
  [(nth world ref) world])

(defn setref! [ref val world]
  [val (assoc world ref val)])
