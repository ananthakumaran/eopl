;; The definition of lc-exp ignores the condition in definition that
;; says "Identifier is any symbol other than lambda". Modify the
;; definition of identifier? to capture this condition. As a hint,
;; remember that any predicate can be used in define-datatype, even
;; ones you define.

(ns eopl.chap-2.23
  (:use clojure.test)
  (:use eopl.core.define-datatype))

(defn identifier? [x]
  (and (symbol? x) (not= 'lambda x)))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(defn occurs-free? [search-var exp]
  (cases lc-exp exp
         (var-exp (var) (= var search-var))
         (lambda-exp (bound-var body)
                     (and (not (= search-var bound-var))
                          (occurs-free? search-var body)))
         (app-exp (rator rand)
                  (or (occurs-free? search-var rator)
                      (occurs-free? search-var rand)))))

(deftest occurs-free-test
  (is (occurs-free? 'x (var-exp 'x)))
  (is (not (occurs-free? 'x (var-exp 'y))))
  (is (not (occurs-free? 'x (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))))
  (is (occurs-free? 'x (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y)))))
  (is (occurs-free? 'x (app-exp (lambda-exp 'x (var-exp 'x)) (app-exp (var-exp 'x) (var-exp 'y)))))
  (is (occurs-free? 'x (lambda-exp 'y (lambda-exp 'z (app-exp (var-exp 'x) (app-exp (var-exp 'y) (var-exp 'z)))))))
  (is (thrown? AssertionError (var-exp 'lambda))))

(run-tests)
