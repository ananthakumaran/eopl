;; Write the rule for and implement multiprocedure letrec expressions.

(ns eopl.chap-4.18
  (:use clojure.test)
  (:use eopl.core.implicit-ref-lang)
  (:use eopl.core.feature))

(letrec-multi-feature)

(run-tests)
