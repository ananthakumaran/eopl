;; (exists? pred lst) returns #t if any element of lst satisfies pred,
;; and returns #f otherwise

(ns eopl.chap-1
  (:use clojure.test))

(defn exists? [pred lst]
  (loop [left lst]
    (if (empty? left)
      false
      (if (pred (first left))
        true
        (recur (rest left))))))

(deftest exists?-test
  (is (= (exists? number? '(a b c 3 e)) true))
  (is (= (exists? number? '(a b c d e)) false)))

(run-tests)
