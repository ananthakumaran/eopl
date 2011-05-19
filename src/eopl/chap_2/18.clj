;; We usually represent a sequence of values as a list. In this
;; representation, it is easy to move from one element in a sequence
;; to the preceding one without the help of context
;; arguments. Implement non-empty bidirectional sequences of integers,
;; as suggested by the grammer

;; NodeInSequence ::= (Int Listof(Int) Listof(Int))

;; The first list of numbers is the elements of the sequence preceding
;; the current one in reverse order, and the second list is the
;; elements of the sequence after the current one. For example, (6 (5
;; 4 3 2 1) (7 8 9)) represents the list (1 2 3 4 5 6 7 8 9), with the
;; focus on the element 6.

;; In this representation, implement the procedure number->sequence,
;; which takes a number and produces a sequence consisting of exactly
;; that number. Also implement current-element, move-to-left,
;; move-to-right, insret-to-left, insert-to-right, at-left-end?, and
;; at-right-end?

;; The procedure move-to-right should fail if its argument is at the
;; right end of the sequence, and the procedure move-to-left should
;; fail if its argument is at the left end of the sequence

(ns eopl.chap-2.18
  (:use clojure.test))

(defn number->sequence [n]
  `(~n () ()))

(defn current-element [nseq]
  (first nseq))

(defn move-to-left [nseq]
  (let [[current previous next] nseq]
    (if (seq previous)
      (list (first previous) (rest previous) (cons current next))
      (throw (IllegalAccessException.)))))

(defn move-to-right [nseq]
  (let [[current previous next] nseq]
    (if (seq next)
      (list (first next) (cons current previous) (rest next))
      (throw (IllegalAccessException.)))))

(defn insert-to-left [n nseq]
  (let [[current previous next] nseq]
    (list current (cons n previous) next)))

(defn insert-to-right [n nseq]
  (let [[current previous next] nseq]
    (list current previous (cons n next))))

(deftest node-sequence-test
  (is (= '(7 () ()) (number->sequence 7)))
  (is (= 6 (current-element '(6 (5 4 3 2 1) (7 8 9)))))
  (is (= '(7 (6 5 4 3 2 1) (8 9)) (move-to-right '(6 (5 4 3 2 1) (7 8 9)))))
  (is (= '(5 (4 3 2 1) (6 7 8 9)) (move-to-left '(6 (5 4 3 2 1) (7 8 9)))))
  (is (= '(6 (13 5 4 3 2 1) (7 8 9)) (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))))
  (is (= '(6 (5 4 3 2 1) (13 7 8 9)) (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))))))

(run-tests)
