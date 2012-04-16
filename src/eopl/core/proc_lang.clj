(ns eopl.core.proc-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use eopl.core.proc-lang-parser)
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

;; xxx should cache the fv of a exp instead of find it every time
(defn fv [exp env]
  "returns list of free variables in the exp"
  (cases expression exp
         (diff-exp (e1 e2) (union (fv e1 env) (fv e2 env)))
         (add-exp (e1 e2) (union (fv e1 env) (fv e2 env)))
         (mul-exp (e1 e2) (union (fv e1 env) (fv e2 env)))
         (div-exp (e1 e2) (union (fv e1 env) (fv e2 env)))
         (minus-exp (e1) (union (fv e1 env)))
         (cons-exp (e1 e2) (union (fv e1 env) (fv e2 env)))
         (car-exp (e1) (union (fv e1 env)))
         (cdr-exp (e1) (union (fv e1 env)))
         (list-exp (items) (apply union (map (fn [i] (fv i env)) items)))

         (cond-exp (conditions)
                   (apply union (map (fn [c]
                                       (cases condition c
                                              (clause-exp (predicate consequence)
                                                          (union (fv predicate env)
                                                                 (fv consequence env)))))
                                     conditions)))

         (if-exp (e1 e2 e3) (union (fv e1 env) (fv e2 env) (fv e3 env)))
         (var-exp (var) (if (contains? env var)
                          #{}
                          #{var}))
         (print-exp (e1) (union (fv e1 env)))
         (let-exp (body bindings)
                  (let [vf (map
                            (fn [b]
                              (cases binding b
                                     (binding-exp (var value)
                                                  [var (fv value env)])))
                            bindings)
                        n-fv (apply union (map #(second %1) vf))
                        n-env (union env (set (map #(first %1) vf)))]
                    (union (fv body n-env)
                           n-fv)))

         (let*-exp (body bindings)
                   (let [[n-env n-fv] (reduce
                                       (fn [[n-env n-fv] b]
                                         (cases binding b
                                                (binding-exp (var value)
                                                             [(union n-env #{var}) (union (fv value n-env) n-fv)])))
                                       [env #{}]
                                       bindings)]
                     (union (fv body n-env)
                            n-fv)))


         (unpack-exp (body vars value)
                     (let [n-fv (fv value env)
                           n-env (union env (set vars))]
                       (union (fv body n-env)
                              n-fv)))

         (proc-exp (vars body)
                   (fv body (union env (set vars))))

         (letproc-exp (name vars proc-body body)
                   (union (fv proc-body (union env (set vars)))
                           (fv body (union env #{name}))))

         (call-exp (rator rands)
                   (union (fv rator env)
                          (apply union (map #(fv %1 env) rands))))

         (equal?-exp (e1 e2) (union (fv e1 env) (fv e2 env)))
         (greater?-exp (e1 e2) (union (fv e1 env) (fv e2 env)))
         (less?-exp (e1 e2) (union (fv e1 env) (fv e2 env)))
         (zero?-exp (e1) (union (fv e1 env)))
         (null?-exp (e1) (union (fv e1 env)))
         (else #{})))


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
                   (proc-val (procedure vars body (select-keys
                                                   env
                                                   (fv body (set vars))))))

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
  (is (= (result "let x = 200
                  in let f = proc (z) -(z,x)
                     in let x = 100
                        in let g = proc (z) -(z,x)
                           in -((f 1), (g 1))")
         -100)))

(run-tests)


