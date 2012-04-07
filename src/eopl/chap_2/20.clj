;; In the representation of binary trees in exercise 2.19 it is easy
;; to move from a parent node to one of its sons, but it is impossible
;; to move from a sont to its parent without the help of context
;; arguments. Extend the representation of lists in exercise 2.18 to
;; represent nodes in a binary tree. As a hint, consider representing
;; the portion of the tree above the current node by a reversed list,
;; as in exercise 2.18

;; In this representation, implement the procedures from exercise
;; 2.19. Also implement move-up, at-root? and at-leaf?


(ns eopl.chap-2.20
  (:use clojure.test))

(defn number->bintree [n]
  `(~n () () ()))


(defn make-bintree [n left right]
  (list n '() left right))

(defn current-element [bintree]
  (first bintree))

(defn lson [bintree]
  (first (next (next bintree))))

(defn rson [bintree]
  (first (next (next (next bintree)))))

(defn at-leaf? [bintree]
  (nil? (first bintree)))

(defn move-to-left [bintree]
  {:pre [(not (at-leaf? bintree))]}
  (let [[n parents left right] bintree]
    (list (current-element left)
          (cons (list n '() right) parents)
          (lson left)
          (rson left))))

(defn move-to-right [bintree]
  {:pre [(not (at-leaf?  bintree))]}
  (let [[n parents left right] bintree]
    (list (current-element right)
          (cons (list n left '()) parents)
          (lson right)
          (rson right))))

(defn move-up [bintree]
  (let [[e parents left right] bintree]
    (if (nil? e)
      (list (current-element (first parents))
            (rest parents)
            '()
            '())
      (let [[pe pleft pright] (first parents)]
       (list pe
             (rest parents)
             (if (empty? pleft) (list e '() left right) pleft)
             (if (empty? pright) (list e '() left right) pright))))))


(defn insert-to-left [n bintree]
  (list (current-element bintree)
        (second bintree)
        (make-bintree n (lson bintree) '())
        (rson bintree)))

(defn insert-to-right [n bintree]
  (list (current-element bintree)
        (second bintree)
        (lson bintree)
        (make-bintree n '() (rson bintree))))


(deftest bintree-test
  (let [t  (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13)))]
    (is (= '(13 () (12 () () ()) (14 () () ())) t))
    (is (= '(13 () () ()) (number->bintree 13)))
    (is (= '(12 ((13 () (14 () () ()))) () ()) (move-to-left t)))
    (is (= 12 (current-element (move-to-left t))))
    (is (at-leaf? (move-to-right (move-to-left t))))
    (is (= '(13
             ()
             (15
              ()
              (12 () () ())
              ())
             (14 () () ()))
           (insert-to-left 15 t)))
    (is (= (move-up (move-to-right (number->bintree 13)))
           (number->bintree 13)))))

(run-tests)


