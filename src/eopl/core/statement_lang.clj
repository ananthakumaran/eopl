(ns eopl.core.statement-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use eopl.core.statement-lang-parser)
  ;;  (:use eopl.core.link-ref)
  (:use eopl.core.vector-ref)
  (:use clojure.set)
  (:use clojure.test)
  (:use eopl.core.feature))

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
                                (extend-env new-env var (newref arg)))
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

    (not-exp (exp)
             (bool-val (not (expval->bool (value-of exp env)))))

    (zero?-exp (exp1)
               (bool-val (zero? (expval->num (value-of exp1 env)))))
    (true-exp () (bool-val true))
    (false-exp () (bool-val false))

    (list-exp (args)
              (list-val (map #(value-of %1 env) args)))

    (print-exp (exp)
               (do (print (expval->val (value-of exp env)))
                   (num-val 1)))

    (begin-exp (exps)
               (reduce (fn [last-val exp]
                         (value-of exp env))
                       nil
                       exps))

    (if-exp (exp1 exp2 exp3)
            (if (expval->bool (value-of exp1 env))
              (value-of exp2 env)
              (value-of exp3 env)))

    (proc-exp (vars body)
              (proc-val (procedure vars body env)))

    (letproc-exp (name vars proc-body body)
                 (let [new-env (extend-env env name (newref (proc-val (procedure vars proc-body env))))]
                   (value-of body new-env)))

    (call-exp (rator rands)
              (let [proc (expval->proc (value-of rator env))
                    args (map
                          #(value-of %1 env)
                          rands)]
                (apply-procedure proc args)))
    (var-exp (var) (de-ref (apply-env env var)))

    (assign-exp (var exp)
                (do
                  (setref! (apply-env env var)
                           (value-of exp env))
                  (num-val 27)))

    (let-exp (body bindings)
             (value-of body
                       (reduce (fn [new-env bind]
                                 (cases binding bind
                                   (binding-exp (var exp)
                                                (extend-env new-env var (newref (value-of exp env))))))
                               env
                               bindings)))

    (setdynamic-exp (body bindings)
                    (let [var-map (into {} (map
                                            (fn [bind]
                                              (cases binding bind
                                                (binding-exp (var exp)
                                                             [var (value-of exp env)])))
                                            bindings))
                          vars (keys var-map)
                          old-map (into {} (map #(vector %1 (de-ref (apply-env env %1))) vars))]

                      (doseq [v vars]
                        (setref! (apply-env env v)
                                 (get var-map v)))

                      (let [result (value-of body env)]
                        (doseq [v vars]
                          (setref! (apply-env env v)
                                   (get old-map v)))
                        result)))

    (let*-exp (body bindings)
              (value-of body
                        (reduce (fn [new-env bind]
                                  (cases binding bind
                                    (binding-exp (var exp)
                                                 (extend-env new-env var (newref (value-of exp new-env))))))
                                env
                                bindings)))

    (letrec-exp (body proc-bindings)
                (let [pbs (into {} (map (fn [pb]
                                          (cases proc-binding pb
                                            (proc-binding-exp (name vars body)
                                                              [name [name vars body]])))
                                        proc-bindings))]
                  (value-of body
                            (extend-env-rec
                             env
                             (fn [name new-env]
                               (let [[name vars body] (get pbs name)]
                                 (newref (proc-val (procedure vars body new-env)))))
                             (keys pbs)))))

    (unpack-exp (body vars value)
                (value-of body
                          (reduce (fn [new-env [var val]]
                                    (extend-env new-env var (newref val)))
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

(defn execute [stmt env]
  (cases statement stmt
    (assign-stmt (var exp)
                 (do
                   (setref! (apply-env env var)
                            (value-of exp env))))
    (print-stmt (exp)
                (do (print (expval->val (value-of exp env)))))

    (if-stmt (predicate then else)
             (if (expval->bool (value-of predicate env))
               (execute then env)
               (execute else env)))

    (while-stmt (predicate body)
                (while (expval->bool (value-of predicate env))
                  (execute body env)))

    (block-stmt (vars body)
                (let [new-env (reduce
                               (fn [env var]
                                 (extend-env env var (newref (num-val 0))))
                               env
                               vars)]
                  (execute body new-env)))

    (multi-stmt (stmts)
                (doseq [stmt stmts]
                  (execute stmt env)))
    (else (throw (Exception. (str "unkonwn statement " stmt))))))

(defn value-of-program [pgm]
  (cases program pgm
    (a-program (stmt)
               (execute stmt (empty-env)))))


(defn run [program]
  (initialize-store!)
  (value-of-program (parse program)))

(defn result [program]
  (run program))


(deftest statement-test
  (is (= (with-out-str
           (result "var x,y; {x = 3; y = 4; print +(x,y)}"))
         "7"))
  (is (= (with-out-str
           (result "var x,y,z; {
    x = 3;
    y = 4;
    z = 0;
    while not(zero?(x))
    { z = +(z,y); x = -(x,1) };
    print z
}"))
         "12"))
  (is (= (with-out-str
           (result "var f,x; {f = proc(x y) *(x,y);
                              x = 3;
                              print (f 4 x)}"))
         "12")))

(run-tests)
