;; (down lst) wraps parentheses around each top-level elements of lst
(ns eopl.chap-1.17
  (:use clojure.test))


(defn down [lst]
  (map #(list %1) lst))

(deftest down-test
  (is (= (down '(1 2 3)) '((1) (2) (3))))
  (is (= (down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea)))))
  (is (= (down '(a (more (complicated)) object)) '((a) ((more (complicated))) (object)))))

(run-tests)
