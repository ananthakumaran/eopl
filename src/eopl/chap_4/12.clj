;; Our understanding of the store as expressed in this interpreter
;; depends on the meaning of effects in scheme. In particular, it
;; depends on us knowing when these effects take place in a scheme
;; program. We can avoid this dependency by writing an interpreter
;; that more closely mimics the specification. In this interpreter
;; value-of would return both a value and a store, just as in the
;; specification. A fragment of this interpreter appears in figure
;; 4.6. We call this a store-passing interpreter. Extend this
;; interpreter to cover all of the language explicit-refs

;; Every procedure that might modify the store return not just its
;; usual value but also a new store. These are packaged in a data type
;; called answer. Complete this definition of value-of


(ns eopl.chap-4.12
  (:use clojure.test)
  (:use eopl.core.store-pass-lang))

(deftest store-pass-test
  (is (= (result "let x = newref(newref(0))
                  in begin
                    setref(de-ref(x), 11);
                    de-ref(de-ref(x))
                  end")
         11))

  (is (= (result "let x = newref(0)
                   in begin
                     cons(setref(x, emptylist), de-ref(x))
                   end"))
      23)

  (is (= (result "let y = newref(0)
                    in if zero? (setref(y, 10)) then 1 else de-ref(y)")
         10)))


(run-tests)
