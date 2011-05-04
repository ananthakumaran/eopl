;; write a procedure g such that number-elements from page 23 could be
;; defined as

(ns eopl.chap-1.36
  (:use clojure.test)
  (:use eopl.chap-1.31))

(defn number-elements [lst]
  (if (empty? lst) '()
      (g (list 0 (first lst)) (number-elements (rest lst)))))

(defn g [item lst]
  (if (empty? lst)
    (list item)
    (let [new-list (map (fn [it]
                          (cons (inc (first it)) (rest it)))
                        lst)]
      (cons item new-list))))

(deftest number-elements-test
  (is (= '((0 a) (1 b) (2 c) (3 d))
         (number-elements '(a b c d)))))

(run-tests)
