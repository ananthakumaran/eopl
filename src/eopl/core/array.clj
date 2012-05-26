(ns eopl.core.array
  (:use eopl.core.vector-ref))

(defn newarray [size val]
  (let [ptr (newref val)]
    (dotimes [i (dec size)]
      (newref val))
    [ptr size]))

(defn check-range [i size]
  (if (> i (dec size))
    (throw (Exception. (str "indexoutofbounds")))))

(defn arrayset [[ptr size] i val]
  (check-range i size)
  (setref! (+ ptr i) val))

(defn arraylength [[ptr size]]
  size)

(defn arrayref [[ptr size] i]
  (check-range i size)
  (de-ref (+ ptr i)))

(def array? vector?)



