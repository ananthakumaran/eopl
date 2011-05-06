;; write a procedure number-leaves that takes a bintree and produces a
;; bintree like the original, except the contents of the leaves are
;; numbered starting from 0.

(ns eopl.chap-1.35
  (:use clojure.test)
  (:use eopl.chap-1.31))

(defn number-leaves
  ([bintree] (rest (number-leaves bintree 0)))
  ([bintree leaf-count]
     (if (leaf? bintree)
       (cons (inc leaf-count) (leaf leaf-count))
       (let [[left-count & left-bintree] (number-leaves (lson bintree) leaf-count)
             [right-count & right-bintree] (number-leaves (rson bintree) left-count)]
         (cons right-count (interior-node (content-of bintree)
                                            left-bintree
                                            right-bintree))))))

(deftest test-number-leaves
  (is (= '(foo (bar (0) (1))
               (baz (2) (quxx (3) (4)))))
      (number-leaves (interior-node 'foo
                                    (interior-node 'bar
                                                   (leaf 26)
                                                   (leaf 12))
                                    (interior-node 'baz
                                                   (leaf 11)
                                                   (interior-node 'quux
                                                                  (leaf 117)
                                                                  (leaf 14)))))))

(run-tests)

