;; write the following procedure for calculating on a bintree leaf and interior-node, which build bintrees, leaf? which test whether a bintree is a leaf, and lson, rson, and contents-of, which extract the components of a node. contents-of should work on both leaves and interior nodes.

;; first element is content, second element is left-node and third element is right node

(ns eopl.chap-1.31
  (:use clojure.test))

(defn leaf [content]
  (list content))

(defn leaf? [node]
  (empty? (rest node)))

(defn interior-node [content left right]
  (list content left right))

(defn lson [node]
  (first (rest node)))

(defn rson [node]
  (first (rest (rest node))))

(defn content-of [node]
  (first node))
