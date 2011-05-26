;; Sometimes it is useful to specify a concrete syntax as a sequence
;; of symbols and integers, surrounded by parentheses. For example,
;; one might define the set of prefix list by

;; Prefix-list ::= (Prefix-exp)
;; Prefix-exp  ::= Int
;;             ::= - Prefix-exp Prefix-exp

;; so that (- - 3 2 - 4 - 12 7) is a legal prefix list. This is
;; sometimes called Polish prefix notation, after its inventor, Jan
;; Lukasiewicz. Write a parser to convert a prefix-list to the
;; abstract syntax

;; (define-datatype prefix-exp prefix-exp?
;;   (const-exp
;;    (num integer?))
;;   (diff-exp
;;    (operand1 prefix-exp?)
;;    (operand2 prefix-exp?)))


;; so that example above produces the same abstract syntax tree as the
;; sequence of constructors

;; (diff-exp
;;  (diff-exp
;;   (const-exp 3)
;;   (const-exp 2))
;;  (diff-exp
;;   (const-exp 4)
;;   (diff-exp
;;    (const-exp 12)
;;    (const-exp 7))))

;; As a hint, consider writing a procedure that takes a list and
;; produces a prefix-exp and the list of leftover list elements

(ns eopl.chap-2.31
  (:use clojure.test))

(def prefix-exp? list?)

(defn const-exp [x]
  (list 'const-exp x))
(defn diff-exp [x y]
  (list 'diff-exp x y))

(defn prefix? [x]
  (or (prefix-exp? x) (number? x)))

(defn exp [x]
  (if (number? x)
    (const-exp x)
    x))

(defn parse-prefix
  ([ls]
     (if (prefix-exp? (first ls))
       (first ls)
       (parse-prefix (list (second ls) (first ls)) (rest (rest ls)))))
  ([l r]
     (let [[f s & x] l]
       (if (and (prefix? f) (prefix? s))
         (parse-prefix (concat (reverse (rest x))
                               (list* (diff-exp
                                       (exp s)
                                       (exp f)) r)))
         (recur (cons (first r) l) (rest r))))))

(deftest parse-prefix-test
  (is (= (parse-prefix '(- - 3 2 - 4 - 12 7))
         '(diff-exp
           (diff-exp
            (const-exp 3)
            (const-exp 2))
           (diff-exp
            (const-exp 4)
            (diff-exp
             (const-exp 12)
             (const-exp 7)))))))
(run-tests)
