;; Write a procedure double-tree that takes a bintree,
;; as represented in definition 1.1.7 and produce another
;; bintree like the original, but will all the integers
;; in the leaves doubled

(ns eopl.chap-1.32
  (:use clojure.test)
  (:use eopl.chap-1.31))

(defn double-tree [tree]
  (if (leaf? tree)
    (leaf (* 2 (content-of tree)))
    (interior-node (content-of tree)
                   (double-tree (lson tree))
                   (double-tree (rson tree)))))

(deftest double-tree-test
  (is (= (interior-node 10
                        (leaf 10)
                        (leaf 10))
         (double-tree (interior-node 10
                                     (leaf 5)
                                     (leaf 5))))))

(run-tests)





