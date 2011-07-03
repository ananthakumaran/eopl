;; (filter-in pred lst) returns the list of those elements in lst that
;; satisfy the predicate pred.

(ns eopl.chap-1.22
  (:use clojure.test))


(defn filter-in [pred lst]
  (loop [result ()
         left lst]
    (if (empty? left)
      (reverse result)
      (let [x (first left)]
        (recur (if (pred x) (cons x result) result) (rest left))))))

(deftest filter-in-test
  (is (= (filter-in number? '(a 2 (1 3) b 7)) '(2 7)))
  (is (= (filter-in symbol? '(a (b c) 17 foo)) '(a foo))))

(run-tests)
