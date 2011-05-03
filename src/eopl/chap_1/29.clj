;; (sort loi) returns a list of the elements of loi in ascending order

(ns eopl.chap-1
  (:use clojure.test))

(defn sort [loi]
  (if (or (empty? loi) (empty? (rest loi)))
    loi
    (let [pivot (first loi)
          left (filter #(<= % pivot) (rest loi))
          right (filter #(> % pivot) (rest loi))]
      (concat (sort left) (list pivot) (sort right)))))

(deftest sort-test
  (is (= '(2 2 3 5 8) (sort '(8 2 5 2 3)))))

(run-tests)
