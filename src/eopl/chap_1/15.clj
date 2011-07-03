;; (duple n x) returns a list containing n copies of x


(ns eopl.chap-1.15
  (:use clojure.test))

(defn duple [n x]
  (loop [count n
         result '()]
    (if (zero? count)
      result
      (recur (dec count) (cons x result)))))

(deftest duple-test
  (is (= (duple 2 3) '(3 3)))
  (is (= (duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha))))
  (is (= (duple 0 '(blah)) '())))

(run-tests)
