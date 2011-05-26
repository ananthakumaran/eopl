;; The definition of lc-exp ignores the condition in definition that
;; says "Identifier is any symbol other than lambda". Modify the
;; definition of identifier? to capture this condition. As a hint,
;; remember that any predicate can be used in define-datatype, even
;; ones you define.

(ns eopl.chap-2.23
  (:use clojure.test))

;; TODO test
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
