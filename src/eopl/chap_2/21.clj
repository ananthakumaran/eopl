;; Implement the data type of environments, as in section 2.2.2, using
;; define-datatype. Then include has-binding? of exercise 2.9

(ns eopl.chap-2.21
  (:use clojure.test)
  (:use eopl.core.define-datatype))

(define-datatype env env?
  (empty-env)
  (non-empty-env
   (var symbol?)
   (val (fn [x] true?))
   (saved-env env?)))

(defn has-binding? [e search-var]
  (cases env e
         (empty-env () false)
         (non-empty-env (var val saved-env)
                        (if (= var search-var)
                          true
                          (has-binding? saved-env search-var)))))

(deftest has-binding-test
  (let [env (non-empty-env 'y "y"
                        (non-empty-env 'x "x" (empty-env)))]
    (is (has-binding? env 'x))
    (is (has-binding? env 'y))
    (is (not (has-binding? env 'z)))))

(run-tests)
