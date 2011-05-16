;; Implement the stack data type of exercise 2.4 using a procedure
;; representation

(ns eopl.chap-2.12
  (:use clojure.test))

(defn empty-stack []
  (fn [type]
    (cond (= type 'empty?) true
          :else (throw (IllegalArgumentException.)))))

(defn push [s x]
  (fn [type]
    (cond (= type 'empty?) false
          (= type 'top) x
          (= type 'pop) s
          :else (throw (IllegalArgumentException.)))))

(defn empty-stack? [s]
  (s 'empty?))

(defn pop [s]
  (s 'pop))

(defn top [s]
  (s 'top))


(deftest stack-test
  (is (empty-stack? (empty-stack)))
  (is (empty-stack? (pop (push (empty-stack) 10))))
  (is (= (top (push (empty-stack) 10)) 10)))

(run-tests)
