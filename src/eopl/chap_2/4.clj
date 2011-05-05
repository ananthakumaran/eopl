;; consider the data type of stacks of values, with an interface
;; consisting of the procedures empty-stack, push, pop, top and
;; empty-stack? write a specification for these operations in the
;; style of the example above. Which operations are constructors and
;; which are observers?

(ns eopl.chap-2.4
  (:use clojure.test))

;; constructor
(defn empty-stack []
  '())

;; constructor
(defn push [s x]
  (cons x s))

;; constructor
(defn pop [s]
  (rest s))

;; observer
(defn top [s]
  (first s))

;; observer
(defn empty-stack? [s]
  (empty? s))
