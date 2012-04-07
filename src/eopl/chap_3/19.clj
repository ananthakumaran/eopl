;; In many languages, procedures must be created and named at the same
;; time. Modify the language of this section to have this property by
;; replacing the proc expression with a letproc expression

(ns eopl.chap-3.19
  (:use eopl.core.proc-lang)
  (:use clojure.test))

(deftest letproc-test
  (is (= (result "let x = 200
                  in letproc f (z) -(z,x)
                     in let x = 100
                        in letproc g (z) -(z,x)
                           in -((f 1), (g 1))")
         -100)))
