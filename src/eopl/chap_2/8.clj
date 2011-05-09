;; Add to the environment interface an observer called empty-env? and
;; implement it using the a-list representation

(ns eopl.chap-2.8
  (:use clojure.test)
  (:use eopl.chap-2.5))

(defn empty-env? [env]
  (= env (empty-env)))

