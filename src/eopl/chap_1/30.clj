;; (sort/predicate pred loi) returns a list of elements sorted by the predicate

(ns eopl.chap-1
  (:use clojure.test))

(defn sort-predicate [pred loi]
  (if (or (empty? loi) (empty? (rest loi)))
    loi
    (let [pivot (first loi)
          left (filter #(pred % pivot) (rest loi))
          right (filter #((complement pred) % pivot) (rest loi))]
      (concat (sort-predicate pred left) (list pivot) (sort-predicate pred right)))))


(deftest sort-test
  (is (= '(2 2 3 5 8) (sort-predicate < '(8 2 5 2 3))))
  (is (= '(8 5 3 2 2) (sort-predicate > '(8 2 5 2 3)))))

(run-tests)
