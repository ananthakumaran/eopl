;; (product sos1 sos2), where sos1 and sos2 are each a list of symbols
;; without repetitions, returns a list of 2-lists that represents the
;; cartesian product of sos1 ans sos2. The 2-list may appear in any
;; order

(ns eopl.chap-1
  (:use clojure.test))

(defn product [x y]
  (loop [result ()
         left x]
    (if (empty? left)
      result
      (recur (concat result (map #(list (first left) %) y)) (rest left)))))

(deftest product-test
  (is (= (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))))

(run-tests)
