;; Extend the representation of the preceding exercise to include a
;; third procedure that implements has-binding? (see exercise 2.9)

(ns eopl.chap-2.14
  (:use clojure.test))

(defn empty-env []
  [(fn [search-var]
     (throw (IllegalStateException.)))
   (fn [search-var] false)
   (fn [] true)])

(declare apply-env)
(declare has-binding?)

(defn extend-env [saved-env saved-var saved-val]
  [(fn [search-var]
     (if (= saved-var search-var)
       saved-val
       (apply-env saved-env search-var)))
   (fn [search-var]
     (if (= saved-var search-var)
       true
       (has-binding? saved-env search-var)))
   (fn [] false)])

(defn apply-env [env search-var]
  ((first env) search-var))

(defn has-binding? [env search-var]
  ((second env) search-var))

(defn empty-env? [env]
  ((nth env 2)))

(deftest has-binding-test
  (let [env (extend-env (extend-env (empty-env) 'x "x") 'y "y")]
    (is (= (apply-env env 'y) "y"))
    (is (= (apply-env env 'x) "x"))
    (is (empty-env? (empty-env)))
    (is (thrown? IllegalStateException (apply-env env 'z)))
    (is (not (empty-env? env)))
    (is (has-binding? env 'x))
    (is (has-binding? env 'y))
    (is (not (has-binding? env 'z)))))

(run-tests)

