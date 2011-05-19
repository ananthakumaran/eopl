;; A binary tree with empty leaves and with interior nodes labeled
;; with integers could be represented using the grammar

;; Bintree ::= () | (Int Bintree Bintree)

;; In this representation, implement the procedure number->bintree,
;; which takes a number and produces a binary tree consisting of a
;; single node containing that number. Also implement current-element,
;; move-to-left-son, move-to-right-son, at-leaf?, insert-to-left, and
;; insert-to-right.

(ns eopl.chap-2.19
  (:use clojure.test))

(defn number->bintree [n]
  `(~n () ()))

(defn make-bintree [n left right]
  `(~n ~left ~right))

(defn current-element [bintree]
  (first bintree))

(defn move-to-left [bintree]
  (second bintree))

(defn move-to-right [bintree]
  (first (rest (rest bintree))))

(defn insert-to-left [n bintree]
  (make-bintree (current-element bintree)
                (make-bintree n (move-to-left bintree) '())
                (move-to-right bintree)))

(defn insert-to-right [n bintree]
  (make-bintree (current-element bintree)
                (move-to-left bintree)
                (make-bintree n '() (move-to-right bintree))))

(defn at-leaf? [bintree]
  (empty? bintree))

(deftest bintree-test
  (let [t (insert-to-right 14
                           (insert-to-left 12
                                           (number->bintree 13)))]
    (is (= '(13 (12 () ()) (14 () ())) t))
    (is (= '(13 () ()) (number->bintree 13)))
    (is (= '(12 () ()) (move-to-left t)))
    (is (= 12 (current-element (move-to-left t))))
    (is (at-leaf? (move-to-right (move-to-left t))))
    (is (= '(13
             (15
              (12 () ())
              ())
             (14 () ()))
           (insert-to-left 15 t)))))

(run-tests)


