;; Using the define-datatype, implement the stack datatype of ex 2.4

(ns eopl.chap-2.22
  (:use clojure.test))

;; TODO test
(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack
   (top any?)
   (queue stack?)))

