;; (up lst) removes a pair of parentheses from each top-level element of
;; lst. If a top-level element is not a list, it is included in the result,
;; as is. the value of (up (down lst)) is equivalent to lst, but (down (up lst)) is not necessarily lst.


(ns eopl.chap-1
  (:use clojure.test))

(defn up [lst]
  (loop [left lst
         result ()]
    (if (empty? left)
      result
      (if (list? (first left))
        (recur (rest left) (concat result (first left)))
        (recur (rest left) (concat result (list (first left))))))))

(deftest up-test
  (is (= (up '((1 2) (3 4))) '(1 2 3 4)))
  (is (= (up '((x (y)) z)) '(x (y) z))))
