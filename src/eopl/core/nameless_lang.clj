(ns eopl.core.nameless-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.nameless-env)
  (:require [eopl.core.senv :as s])
  (:use eopl.core.nameless-lang-parser)
  (:use clojure.set)
  (:use clojure.test))

(defn expval->num [val]
  (cases expval val
         (num-val (num) num)
         (else (throw (Exception. (str "invalid number " val))))))

(defn expval->bool [val]
  (cases expval val
         (bool-val (bool) bool)
         (else (throw (Exception. (str "invalid bool " val))))))

(defn expval->list [val]
  (cases expval val
         (list-val (lst) lst)
         (else (throw (Exception. (str "invalid list " val))))))

(defn expval->proc [val]
  (cases expval val
         (proc-val (proc) proc)
         (else (throw (Exception. (str "invalid proc " val))))))

(defn expval->val [val]
  (cases expval val
         (num-val (num) num)
         (bool-val (bool) bool)
         (list-val (lst)
                   (map #(expval->val %1) lst))))

(declare value-of)

(defn apply-procedure [p args]
  (cases proc p
         (procedure (body saved-env)
                    (value-of body (extend-env saved-env args)))))

(defn num-val-of-exp [op env & exps]
  (num-val
   (apply op (map #(expval->num (value-of %1 env))
                  exps))))

(defn bool-val-of-exp [op env & exps]
  (bool-val
   (apply op (map #(expval->num (value-of %1 env))
                  exps))))

(defn- proc-binding-to-map [proc-bindings]
  (reduce
   (fn [m pb]
    (cases proc-binding pb
      (proc-binding-exp (name vars body)
                        (conj m [name [name vars body]]))))
   {}
   proc-bindings))

(defn value-of [exp env]
  (cases expression exp
         (const-exp (num) (num-val num))
         (diff-exp (exp1 exp2)
                   (num-val-of-exp - env exp1 exp2))
         (add-exp (exp1 exp2)
                  (num-val-of-exp + env exp1 exp2))
         (mul-exp (exp1 exp2)
                  (num-val-of-exp * env exp1 exp2))
         (div-exp (exp1 exp2)
                  (num-val-of-exp quot env exp1 exp2))
         (minus-exp (exp)
                    (num-val-of-exp - env exp))
         (cons-exp (exp1 exp2)
                   (list-val
                    (cons (value-of exp1 env)
                          (expval->list (value-of exp2 env)))))
         (car-exp (exp1)
                  (let [lst (expval->list (value-of exp1 env))]
                    (if (empty? lst)
                      (list-val '())
                      (first lst))))

         (cdr-exp (exp1)
                  (let [lst (expval->list (value-of exp1 env))]
                    (if (empty? lst)
                      (list-val '())
                      (list-val (next lst)))))

         (emptylist-exp () (list-val '()))

         (equal?-exp (exp1 exp2)
                     (bool-val-of-exp = env exp1 exp2))
         (less?-exp (exp1 exp2)
                    (bool-val-of-exp < env exp1 exp2))
         (greater?-exp (exp1 exp2)
                       (bool-val-of-exp > env exp1 exp2))

         (null?-exp (exp1)
                    (bool-val (empty? (expval->list (value-of exp1 env)))))
         (zero?-exp (exp1)
                    (bool-val (zero? (expval->num (value-of exp1 env)))))
         (true-exp () (bool-val true))
         (false-exp () (bool-val false))

         (list-exp (args)
                   (list-val (map #(value-of %1 env) args)))

         (print-exp (exp)
                    (do (print (expval->val (value-of exp env)))
                        (num-val 1)))
         (if-exp (exp1 exp2 exp3)
                 (if (expval->bool (value-of exp1 env))
                   (value-of exp2 env)
                   (value-of exp3 env)))

         (nameless-proc-exp (body)
                            (proc-val (procedure body env)))

         (call-exp (rator rands)
                   (let [proc (expval->proc (value-of rator env))
                         args (map
                               #(value-of %1 env)
                               rands)]
                     (apply-procedure proc args)))


         (nameless-var-exp (pos) (apply-env env pos))

         (nameless-let-exp (body exps)
                           (value-of body
                            (reduce (fn [new-env exp]
                                      (extend-env new-env [(value-of exp env)]))
                                    env
                                    exps)))

         (nameless-letrec-exp (body exps)
                              (value-of body
                                        (extend-env-rec
                                         env
                                         (fn [i new-env]
                                           (proc-val (procedure (nth exps i) new-env)))
                                         (count exps))))


         (nameless-unpack-exp (body value)
                              (value-of body
                                        (extend-env env (expval->list (value-of value env)))))

         (cond-exp (conditions)
                   (or (first (keep (fn [cond]
                                      (cases condition cond
                                             (clause-exp (predicate consequence)
                                                    (if (expval->bool (value-of predicate env))
                                                      (value-of consequence env)))))
                                    conditions))
                       (throw (Exception. (str "unhandled condition " conditions)))))
         (else (throw (Exception. (str "unkonwn exp " exp))))))


(defn- bindings-to-map [bindings]
  (reduce
   (fn [m bind]
     (cases binding bind
       (binding-exp (var exp)
                    (conj m [var exp]))))
   {}
   bindings))

(defn translation-of [exp senv]
  (cases expression exp

         (const-exp (num) (const-exp num))

         (var-exp (var)
                  (nameless-var-exp
                   (s/apply-env senv var)))

         (let-exp (body bindings)
                  (let [binding-map (bindings-to-map bindings)]
                    (nameless-let-exp (translation-of body
                                                      (reduce
                                                       (fn [senv var]
                                                         (s/extend-env senv [var]))
                                                       senv
                                                       (keys binding-map)))
                                      (map #(translation-of %1 senv)
                                           (vals binding-map)))))


         (letrec-exp (body proc-bindings)
                     (let [pbs (proc-binding-to-map proc-bindings)
                           new-env (s/extend-env senv (keys pbs))]
                       (nameless-letrec-exp (translation-of body new-env)
                                            (map (fn [[name vars body]]
                                                   (translation-of body (s/extend-env new-env vars)))
                                                 (vals pbs)))))

         (proc-exp (vars body)
                   (nameless-proc-exp
                    (translation-of body
                                    (s/extend-env senv vars))))

         (call-exp (rator rands)
                   (call-exp (translation-of rator senv)
                             (map #(translation-of %1 senv) rands)))

         (unpack-exp (body vars value)
                     (nameless-unpack-exp
                      (translation-of body (s/extend-env senv vars))
                      (translation-of value senv)))

         (cond-exp (conditions)
                   (cond-exp
                    (map (fn [con]
                           (cases condition con
                             (clause-exp (predicate consequence)
                                         (clause-exp (translation-of predicate senv)
                                                     (translation-of consequence senv)))))
                         conditions)))

         (list-exp (args)
                   (apply list-exp (map #(translation-of %1 senv) args)))

         (else (let [f (ns-resolve 'eopl.core.nameless-lang-parser
                                   (:variant exp))
                     args (vals (:values exp))]
                 (apply f (map (fn [exp]
                                 (if (expression? exp)
                                   (translation-of exp senv)
                                   (throw (Exception. (str "not implemented " exp)))))
                               args))))))


(defn translation-of-programm [pgm]
  (cases program pgm
         (a-program (exp1)
                    (translation-of exp1 (s/empty-env)))))

(defn run [program]
  (value-of (translation-of-programm (parse program))
            (empty-env)))

(defn result [program]
  (expval->val (run program)))

(deftest proc-test
  (is (= (result "let x = 5 in x")
         5))
  (is (= (result "let x = 200
                  in let f = proc (z) -(z,x)
                     in let x = 100
                        in let g = proc (z) -(z,x)
                           in -((f 1), (g 1))")
         -100))
  (is (= (result "letrec factorial(x) = if less?(x,2) then 1 else (+ x (factorial -(x,1)))
                 in (factorial 5)")
         15))
  (is (= (result "letrec
                    even(x) = if zero?(x) then true else (odd -(x,1))
                    odd(x) = if zero?(x) then false else (even -(x,1))
                  in (odd 13)")
         true))

  (is (= (result "let x = 4
                  in list(x, -(x,1), -(x,3))")
         '(4 3 1)))
  (is (= (result "list()")
         '()))
  (is (= (result "let x = 4
                  in list(x)")
         '(4)))
  (is (= (result "let x = 10
                  in list(10, list(x, 5))")
         '(10 (10 5))))

  (is (= (result "let x = 100
           in
           cond zero?(x) ==> 0
                less?(x, 10) ==> 9
                greater?(x, 10) ==> 11
                equal?(x, 10) ==> 10
           end")
         11))
  (is (thrown-with-msg? Exception #"unhandled condition"
        (result "cond zero?(10) ==> 0 end")))
  (is (= (result "let u = 7
                  in unpack x y = cons(u, cons(3, emptylist))
                     in -(x,y)")
         4)))

(run-tests)
