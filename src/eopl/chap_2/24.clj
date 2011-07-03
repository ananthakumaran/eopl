;; Here is the difintion of binary trees using define-datatype.

(ns eopl.chap-2.24
  (:use clojure.test)
  (:use eopl.core.define-datatype))


(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

;; implement a bintree-to-list procedure for binary trees, so that

;; (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))

;; returns

;; '(interior-node
;;   a
;;   (leaf-node 3)
;;   (leaf-node 4))

(defn bintree-to-list [bt]
  (cases bintree bt
         (leaf-node (num) `(~'leaf-node ~num))
         (interior-node (key left right)
                        (list 'interior-node
                              key
                              (bintree-to-list left)
                              (bintree-to-list right)))))

(deftest bintree-to-list-test
  (is (= (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
         '(interior-node
           a
           (leaf-node 3)
           (leaf-node 4)))))

(run-tests)
