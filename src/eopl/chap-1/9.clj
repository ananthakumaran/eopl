(ns eopl.chap-1
  (:use clojure.test))

(defn remove [x list]
  (cond (empty? list) '()
        (= x (first list)) (remove x (rest list))
        :else (cons (first list) (remove x (rest list)))))

(deftest remove-test
  (is (= '(a b c) (remove 'd '(a b c d))))
  (is (= '(a c) (remove 'b '(a b b c)))))
