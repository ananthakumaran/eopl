;; Modify the implementation to use a representation in which there
;; are no parentheses around the bound variable in a lambda expression

(ns eopl.chap-2.16
  (:use clojure.test))

(defn var-exp [var]
  var)

(defn var-exp? [exp]
  (symbol? exp))

(defn var-exp->var [exp]
  exp)

(defn lambda-exp [var lc-exp]
  `(~'lambda ~var ~lc-exp))

(defn lambda-exp? [exp]
  (and (seq? exp)
       (= 'lambda (first exp))))

(defn lambda-exp->bound-var [exp]
  (second exp))

(defn lambda-exp->body [exp]
  (first (rest (rest exp))))

(defn app-exp [lc-exp1 lc-exp2]
  `(~lc-exp1 ~lc-exp2))

(defn app-exp? [exp]
  (and (seq? exp)
       (= (count exp) 2)))

(defn app-exp->rator [exp]
  (first exp))

(defn app-exp->rand [exp]
  (second exp))

(defn occurs-free? [search-var exp]
  (cond (var-exp? exp) (= search-var (var-exp->var exp))
        (lambda-exp? exp) (and
                           (not (= search-var (lambda-exp->bound-var exp)))
                           (recur search-var (lambda-exp->body exp)))
        :else (or
               (occurs-free? search-var (app-exp->rator exp))
               (occurs-free? search-var (app-exp->rand exp)))))

(deftest occurs-free-test
  (is (occurs-free? 'x 'x))
  (is (not (occurs-free? 'x 'y)))
  (is (not (occurs-free? 'x (lambda-exp 'x '(x y)))))
  (is (occurs-free? 'x (lambda-exp 'y '(x y))))
  (is (occurs-free? 'x (app-exp (lambda-exp 'x 'x) (app-exp 'x 'y))))
  (is (occurs-free? 'x (lambda-exp 'y (lambda-exp 'z (app-exp 'x (app-exp 'y 'z)))))))

(run-tests)
