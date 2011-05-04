;; write a procedure path that takes an integer n and a binary search
;; tree bst that contains the integer n, and returns a list of lefts
;; ans rights showing how to find the node containing n. If n is fount
;; at the root, it returns the empty list.

(ns eopl.chap-1.34
  (:use clojure.test)
  (:use eopl.chap-1.31))

(defn path [n bst]
  (let [x (content-of bst)]
    (cond (= x n) '()
          (< x n) (cons 'right (path n (rson bst)))
          (> x n) (cons 'left (path n (lson bst))))))

(deftest path-test
  (is (= '(right left left)
         (path 17 '(14 (7 (6) (12))
                       (26 (20 (17) (21))
                           (31)))))))

(run-tests)
