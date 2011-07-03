;; Using the define-datatype, implement the stack datatype of ex 2.4

(ns eopl.chap-2.22
  (:use clojure.test)
  (:use eopl.core.define-datatype))

(defn any? [x]
  true)

(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack
   (top any?)
   (queue stack?)))

(defn push [s x]
  (non-empty-stack x s))

(defn pop [s]
  (cases stack s
         (empty-stack () (throw (IllegalAccessError.)))
         (non-empty-stack (top queue) queue)))

(defn top [s]
  (cases stack s
         (empty-stack () (throw (IllegalAccessError.)))
         (non-empty-stack (top queue) top)))

(defn empty-stack? [s]
  (cases stack s
         (empty-stack () true)
         (non-empty-stack (top queue) false)))

(deftest stack-test
  (is (empty-stack? (empty-stack)))
  (is (empty-stack? (pop (push (empty-stack) 10))))
  (is (= (top (push (empty-stack) 10)) 10)))

(run-tests)
