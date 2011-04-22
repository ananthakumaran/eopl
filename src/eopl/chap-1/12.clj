(ns eopl.chap-1
  (:use clojure.test))

(defn subst [new old list]
  (if (empty? list)
    '()
    (cons (let [sexp (first list)]
            (if (symbol? sexp)
              (if (= old sexp) new sexp)
              (subst new old sexp)))
          (subst new old (rest list)))))

(deftest subst-test
  (is (= (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))))

(run-tests)


