(ns eopl.core.letrec-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use eopl.core.letrec-lang-parser)
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
         (procedure (vars body saved-env)
                    (let [new-env (reduce
                                   (fn [new-env [var arg]]
                                     (extend-env new-env var arg))
                                   saved-env
                                   (map (fn [x y] [x y]) vars args))]
                      (value-of body new-env)))))

(defn num-val-of-exp [op env & exps]
  (num-val
   (apply op (map #(expval->num (value-of %1 env))
                  exps))))

(defn bool-val-of-exp [op env & exps]
  (bool-val
   (apply op (map #(expval->num (value-of %1 env))
                  exps))))

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

         (proc-exp (vars body)
                   (proc-val (procedure vars body env)))

         (letproc-exp (name vars proc-body body)
                      (let [new-env (extend-env env name (proc-val (procedure vars proc-body env)))]
                        (value-of body new-env)))

         (call-exp (rator rands)
                   (let [proc (expval->proc (value-of rator env))
                         args (map
                               #(value-of %1 env)
                               rands)]
                     (apply-procedure proc args)))
         (var-exp (var) (apply-env env var))
         (let-exp (body bindings)
                  (value-of body
                            (reduce (fn [new-env bind]
                                      (cases binding bind
                                             (binding-exp (var exp)
                                                          (extend-env new-env var (value-of exp env)))))
                                    env
                                    bindings)))
         (let*-exp (body bindings)
                  (value-of body
                            (reduce (fn [new-env bind]
                                      (cases binding bind
                                             (binding-exp (var exp)
                                                          (extend-env new-env var (value-of exp new-env)))))
                                    env
                                    bindings)))

         (letrec-exp (name vars proc-body body)
                     (value-of body
                               (extend-env-rec
                                env
                                name
                                (fn [new-env]
                                  (proc-val (procedure vars proc-body new-env))))))

         (unpack-exp (body vars value)
                     (value-of body
                               (reduce (fn [new-env [var val]]
                                         (extend-env new-env var val))
                                       env
                                       (map (fn [var val]
                                              [var val])
                                            vars
                                            (expval->list (value-of value env))))))

         (cond-exp (conditions)
                   (or (first (keep (fn [cond]
                                      (cases condition cond
                                             (clause-exp (predicate consequence)
                                                    (if (expval->bool (value-of predicate env))
                                                      (value-of consequence env)))))
                                    conditions))
                       (throw (Exception. (str "unhandled condition " conditions)))))
         (else (throw (Exception. (str "unkonwn exp " exp))))))

(defn value-of-program [pgm]
  (cases program pgm
         (a-program (exp1)
                    (value-of exp1 (empty-env)))))


(defn run [program]
  (value-of-program (parse program)))

(defn result [program]
  (expval->val (run program)))

(deftest proc-test
  (is (= (result "letrec factorial(x) = if less?(x,2) then 1 else (+ x (factorial -(x,1)))
                 in (factorial 5)")
         15)))

(run-tests)
