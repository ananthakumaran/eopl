;; write subst using map

(ns eopl.chap-1
  (:use clojure.test))

(defn subst [new old list]
  (map (fn [sexp]
         (if (symbol? sexp)
           (if (= old sexp) new sexp)
           (subst new old sexp)))
       list))

(deftest subst-test
  (is (= (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))))

(run-tests)

