;; (swapper s1 s2 slist) returns a list the same as slist, but with all
;; occurrences of s1 replaced by s2 and all occurences of s2 replaced by s1.

(ns eopl.chap-1.18
  (:use clojure.test))

(defn swapper [s1 s2 slist]
  (map (fn [x]
         (if (symbol? x)
           (cond (= s1 x) s2
                 (= s2 x) s1
                 :else x)
           (swapper s1 s2 x)))
       slist))

(deftest swapper-test
  (is (= (swapper 'a 'd '(a b c d)) '(d b c a)))
  (is (= (swapper 'a 'd '(a d () c d)) '(d a () c a)))
  (is (= (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))))

(run-tests)
