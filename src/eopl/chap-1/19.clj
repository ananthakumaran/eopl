;; (list-set 1st n x) returns a list like lst, except that the n'th
;; element, using zero-based indexing, is x

(ns eopl.chap-1
  (:use clojure.test))

(defn list-set [lst n x]
  (loop [count n
         front ()
         back lst]
    (if (zero? count)
      (concat (reverse front) (list x) (rest back))
      (recur (- count 1) (cons (first back) front) (rest back)))))

(deftest list-set-test
  (is (= (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d)))
  (is (= (list-set '(a b c d) 3 '(1 5 10)) '(a b c (1 5 10)))))

(run-tests)


