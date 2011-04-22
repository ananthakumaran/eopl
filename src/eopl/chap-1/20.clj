;; (count-occurences s slist) returns the number of occurences of s in slist

(ns eopl.chap-1
  (:use clojure.test))

(defn count-occurences [s list]
  (reduce (fn [count x]
            (if (symbol? x)
              (if (= x s) (+ count 1) count)
              (+ count (count-occurences s x))))
          0
          list))

(deftest count-occurences-test
  (is (= (count-occurences 'x '((f x) y (((x z) x)))) 3))
  (is (= (count-occurences 'x '((f x) y (((x z) () x)))) 3))
  (is (= (count-occurences 'w '((f x) y (((x z) x)))))))

(run-tests)
