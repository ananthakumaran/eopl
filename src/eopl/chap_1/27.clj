;; (flatten slist) returns a list of the symbols contained in slist in
;; the order in which theay occur when slist is printed. Intuitively,
;; flatten removes all the inner parentheses from its argument.

(ns eopl.chap-1.27
  (:use clojure.test))

(defn flatten [lst]
  (loop [left lst
         result ()]
    (if (empty? left)
      result
      (if (list? (first left))
        (recur (rest left) (concat result (flatten (first left))))
        (recur (rest left) (concat result (list (first left))))))))

(deftest flatten-test
  (is (= (flatten '(a b c)) '(a b c)))
  (is (= (flatten '((a) () (b ()) () (c))) '(a b c)))
  (is (= (flatten '((a b) c (((d))) e)) '(a b c d e)))
  (is (= (flatten '(a b (() (c)))) '(a b c))))

(run-tests)
