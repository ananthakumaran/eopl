;; (invert lst), where lst is a list of 2-list (list of length two),
;; returns a list with each 2-list reversed


(ns eopl.chap-1
  (:use clojure.test))

(defn invert [lst]
  (map (fn [l]
         (list (fnext l) (first l)))
       lst))


(deftest invert-test
  (is (= (invert '((a 1) (a 2) (1 b) (2 b))) '((1 a) (2 a) (b 1) (b 2)))))

(run-tests)
