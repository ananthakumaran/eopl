;; Extend the procedure representation to implement empty-env? by
;; representing the environment by a list of two procedures. one that
;; returns the value associated with a variable, as before, and one
;; that returns whether or not the environment is empty.

(ns eopl.chap-2.13
  (:use clojure.test))

(defn empty-env []
  [(fn [search-var]
     (throw (IllegalStateException.)))
   (fn [] true)])

(declare apply-env)

(defn extend-env [saved-env saved-var saved-val]
  [(fn [search-var]
     (if (= saved-var search-var)
       saved-val
       (apply-env saved-env search-var)))
   (fn [] false)])

(defn apply-env [env search-var]
  ((first env) search-var))

(defn empty-env? [env]
  ((second env)))

(deftest env-test
  (let [env (extend-env (extend-env (empty-env) 'x "x") 'y "y")]
    (is (= (apply-env env 'y) "y"))
    (is (= (apply-env env 'x) "x"))
    (is (empty-env? (empty-env)))
    (is (not (empty-env? env)))
    (is (thrown? IllegalStateException (apply-env env 'z)))))

(run-tests)
