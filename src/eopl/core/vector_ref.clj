(ns eopl.core.vector-ref
  (:use clojure.test))

(def empty-store [])

(def store (atom :undefined))

(defn initialize-store! []
  (reset! store empty-store))

(def reference? number?)

(defn newref [val]
  (let [next-ref (count @store)]
    (swap! store conj val)
    next-ref))

(defn de-ref [ref]
  (nth @store ref))

(defn setref! [ref val]
  (swap! store assoc ref val))


(deftest vector-ref-test
  (is (= (do (initialize-store!)
             (de-ref (newref 'x)))
         'x))
  (is (= (do (initialize-store!)
             (let [x (newref 'x)]
               (setref! 0 'y)
               (de-ref x)))
         'y)))

(run-tests)
