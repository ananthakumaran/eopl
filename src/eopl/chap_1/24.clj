;; (every? pred lst) returns #f if any elements of lst fails to satisfy pred, and return #t otherwise

(ns eopl.chap-1.24
  (:use clojure.test))

(defn every? [pred lst]
  (loop [left lst]
    (if (empty? left)
      true
      (if (pred (first left))
        (recur (rest left))
        false))))

(deftest every?-test
  (is (= (every? number? '(a b c 3 e)) false))
  (is (= (every? number? '(1 2 3 5 4)) true)))

(run-tests)
