;; Implement the data type of environments, as in section 2.2.2, using
;; define-datatype. Then include has-binding? of exercise 2.9

(ns eopl.chap-2.21
  (:use clojure.test))

;; TODO test
(define-datatype env env?
  (empty-env)
  (non-empty-env
   (var symbol?)
   (val (fn [x] true?))
   (saved-env env?)))

(defn has-binding? [e search-var]
  (cases env env
         (empty-env () false?)
         (non-empty-env (var val saved-env)
                        (if (= var search-var)
                          true
                          (recur saved-env search-var)))))
