(ns eopl.core.let-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use eopl.core.let-lang-parser)
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

(defn expval->val [val]
  (cases expval val
         (num-val (num) num)
         (bool-val (bool) bool)
         (list-val (lst)
                   (map #(expval->val %1) lst))))

(declare value-of)

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
         (equal?-exp (exp1 exp2)
                     (bool-val-of-exp = env exp1 exp2))
         (less?-exp (exp1 exp2)
                    (bool-val-of-exp < env exp1 exp2))
         (greater?-exp (exp1 exp2)
                       (bool-val-of-exp > env exp1 exp2))

         (null?-exp (exp1)
                    (bool-val (empty? (expval->list (value-of exp1 env)))))

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

         (list-exp (args)
                   (list-val (map #(value-of %1 env) args)))

         (zero?-exp (exp1)
                    (bool-val (zero? (expval->num (value-of exp1 env)))))
         (if-exp (exp1 exp2 exp3)
                 (if (expval->bool (value-of exp1 env))
                   (value-of exp2 env)
                   (value-of exp3 env)))
         (var-exp (var) (apply-env env var))
         (let-exp (var exp1 body)
                  (let [new-env (extend-env env var (value-of exp1 env))]
                    (value-of body new-env)))

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

(deftest let-test
  (is (= (result "-(10,10)")
         0))
  (is (= (result "let x = 10 in x")
         10))
  (is (= (result "if zero? (0) then 1 else 2")
         1)))

(run-tests)
