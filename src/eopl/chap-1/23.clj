;; (list-index pred lst) returns the 0-based position of the first
;; element of lst that satisfies the predicate pred. If no element of
;; lst satisfies the predicate, then list-index returns #f.

(ns eopl.chap-1
  (:use clojure.test))

(defn list-index [pred lst]
  (loop [left lst
         count 0]
    (if (empty? left)
      false
      (if (pred (first left))
        count
        (recur (rest left) (inc count))))))

(deftest list-index-test
  (is (= (list-index number? '(a 2 (1 3) b 7)) 1))
  (is (= (list-index symbol? '(a (b c) 17 foo)) 0))
  (is (= (list-index symbol? '(1 2 (a b) 3)))))

(run-tests)

