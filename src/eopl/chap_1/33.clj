;; write a procedure mark-leaves-with-red-depth that takes a bintree
;; and produces a bintree of the same shape as the original, except
;; that in the new, each leaf contains the integer of nodes between it
;; and the root that contain the symbol red

(ns eopl.chap-1.33
  (:use clojure.test)
  (:use eopl.chap-1.31))

(defn mark-leaves-with-red-depth
  ([bintree] (mark-leaves-with-red-depth bintree 0))
  ([bintree count]
     (if (leaf? bintree)
       (leaf count)
       (let [count (if (= 'red (content-of bintree)) (inc count) count)]
         (interior-node (content-of bintree)
                        (mark-leaves-with-red-depth (lson bintree) count)
                        (mark-leaves-with-red-depth (rson bintree) count))))))

(deftest mark-leaves-with-red-depth-test
  (is (= '(red
           (bar (1) (1))
           (red (2) (quux (2) (2))))
         (mark-leaves-with-red-depth
           (interior-node 'red
                          (interior-node 'bar
                                         (leaf 26)
                                         (leaf 12))
                          (interior-node 'red
                                         (leaf 11)
                                         (interior-node 'quux
                                                        (leaf 117)
                                                        (leaf 14))))))))

(run-tests)
