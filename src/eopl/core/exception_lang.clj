(ns eopl.core.exception-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use eopl.core.exception-lang-parser)
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
                   (map #(expval->val %1) lst))
         (string-val (string) string)))

(declare value-of-k)

(defn apply-cont [cont val]
  (cont val))

(defn end-cont []
  (fn [val]
    (do
      (println "end continuation.")
      val)))

(defn exception-cont []
  (fn [val]
    (do
      (print (format "unhandled exception %s" (expval->val val)))
      val)))

(defn build-cont
  ([env exps econt cb results]
     (if (empty? exps)
       (cb results)
       (value-of-k (first exps) env econt
                   #(build-cont env (rest exps) econt cb (conj results %)))))
  ([env exps econt cb]
     (build-cont env exps econt cb [])))

(defn let-cont [env new-env bindings econt cb]
  (if (empty? bindings)
    (cb new-env)
    (cases binding (first bindings)
      (binding-exp (var exp)
                   (value-of-k exp env econt
                               #(let-cont env
                                          (extend-env new-env var (newref %))
                                          (rest bindings)
                                          econt
                                          cb))))))

(defn let*-cont [env bindings econt cb]
  (if (empty? bindings)
    (cb env)
    (cases binding (first bindings)
      (binding-exp (var exp)
                   (value-of-k exp env econt
                               #(let*-cont (extend-env env var (newref %))
                                           (rest bindings)
                                           econt
                                           cb))))))
(defn apply-procedure [p args econt cont]
  (cases proc p
    (procedure (vars body saved-env)
               (if (= (count args) (count vars))
                 (let [new-env (reduce
                                (fn [new-env [var arg]]
                                  (extend-env new-env var (newref arg)))
                                saved-env
                                (map (fn [x y] [x y]) vars args))]
                   (value-of-k body new-env econt cont))
                 (apply-cont econt (string-val (format "Wrong number of arguments expected %s actual %s" (count vars) (count args))))))))


(defn num-val-of-exp [op env econt cont & exps]
  (build-cont env exps econt
              #(apply-cont cont
                           (num-val
                            (apply op (map expval->num %))))))

(defn bool-val-of-exp [op env econt cont & exps]
  (build-cont env exps econt
              #(apply-cont cont
                           (bool-val
                            (apply op (map expval->num %))))))

(defn value-of-k [exp env econt cont]
  (cases expression exp

         (const-exp (num) (apply-cont cont (num-val num)))

         (diff-exp (exp1 exp2)
                   (num-val-of-exp - env econt cont exp1 exp2))
         (add-exp (exp1 exp2)
                  (num-val-of-exp + env econt cont exp1 exp2))
         (mul-exp (exp1 exp2)
                  (num-val-of-exp * env econt cont exp1 exp2))
         (div-exp (exp1 exp2)
                  (num-val-of-exp quot env econt cont exp1 exp2))
         (minus-exp (exp)
                    (num-val-of-exp - env econt cont exp))

         (cons-exp (exp1 exp2)
                   (build-cont env [exp1 exp2] econt
                               #(apply-cont
                                 cont
                                 (list-val (cons (first %) (expval->list (second %)))))))

         (car-exp (exp1)
                  (value-of-k exp1 env econt
                              (fn [val]
                                (let [lst (expval->list val)]
                                  (apply-cont
                                   cont
                                   (if (empty? lst)
                                     (list-val '())
                                     (first lst)))))))

         (cdr-exp (exp1)
                  (value-of-k exp1 env econt
                              (fn [val]
                                (let [lst (expval->list val)]
                                  (apply-cont
                                   cont
                                   (if (empty? lst)
                                     (list-val '())
                                     (list-val (next lst))))))))


         (emptylist-exp () (apply-cont cont (list-val '())))

         (equal?-exp (exp1 exp2)
                     (bool-val-of-exp = env econt cont exp1 exp2))
         (less?-exp (exp1 exp2)
                    (bool-val-of-exp < env econt cont exp1 exp2))
         (greater?-exp (exp1 exp2)
                       (bool-val-of-exp > env econt cont exp1 exp2))

         (null?-exp (exp1)
                    (value-of-k exp1 env econt
                                #(apply-cont cont (bool-val (empty? (expval->list %))))))

         (zero?-exp (exp1)
                    (value-of-k exp1 env econt
                                #(apply-cont cont (bool-val (zero? (expval->num %))))))

         (true-exp () (apply-cont cont (bool-val true)))
         (false-exp () (apply-cont cont (bool-val false)))

         (list-exp (args)
                   (build-cont env args econt
                               #(apply-cont cont
                                           (list-val (seq %)))))


         (if-exp (exp1 exp2 exp3)
                 (value-of-k exp1 env econt
                             (fn [predicate]
                               (if (expval->bool predicate)
                                 (value-of-k exp2 env econt #(apply-cont cont %))
                                 (value-of-k exp3 env econt #(apply-cont cont %))))))

         (proc-exp (vars body)
                   (apply-cont cont (proc-val (procedure vars body env))))

         (letproc-exp (name vars proc-body body)
                      (let [new-env (extend-env env name (newref (proc-val (procedure vars proc-body env))))]
                        (value-of-k body new-env econt cont)))

         (call-exp (rator rands)
                   (value-of-k rator env econt
                               (fn [val]
                                 (let [proc (expval->proc val)]
                                   (build-cont env rands cont
                                               (fn [args]
                                                 (apply-procedure proc args econt cont)))))))


         (var-exp (var) (apply-cont cont (de-ref (apply-env env var))))

         (assign-exp (var exp)
                     (value-of-k exp env econt
                                 (fn [val]
                                   (do
                                     (setref! (apply-env env var)
                                              val)
                                     (apply-cont cont (num-val 27))))))

         (begin-exp (exps)
                    (build-cont env exps econt
                                #(apply-cont cont (last %))))

         (raise-exp (exp)
                    (value-of-k exp env econt
                                #(apply-cont econt %)))

         (try-exp (exp var handler)
                  (value-of-k exp env
                              (fn [exception]
                                (value-of-k handler
                                            (extend-env env var (newref exception))
                                            econt
                                            cont))
                              #(apply-cont cont %)))


         (let-exp (body bindings)
                  (let-cont env env bindings econt
                            #(value-of-k body % econt cont)))


         (let*-exp (body bindings)
                   (let*-cont env bindings econt
                             #(value-of-k body % econt cont)))

         (letrec-exp (body proc-bindings)
                     (let [pbs (into {} (map (fn [pb]
                                               (cases proc-binding pb
                                                      (proc-binding-exp (name vars body)
                                                                        [name [name vars body]])))
                                             proc-bindings))]
                       (value-of-k body
                               (extend-env-rec
                                env
                                (fn [name new-env]
                                  (let [[name vars body] (get pbs name)]
                                    (newref (proc-val (procedure vars body new-env)))))
                                (keys pbs))
                               econt
                               cont)))

         (else (throw (Exception. (str "unkonwn exp " exp))))))

(defn value-of-program [pgm]
  (cases program pgm
         (a-program (exp1)
                    (value-of-k exp1 (empty-env) (exception-cont) (end-cont)))))


(defn run [program]
  (value-of-program (parse program)))

(defn result [program]
  (expval->val (run program)))

(constant-feature)
(diff-feature)
(arithimetic-feature)
(if-feature)
(proc-feature)
(proc-multi-feature)
(let-proc-feature)
(let-feature)
(let-multi-feature)
(let*-feature)
(letrec-feature)
(list-feature)

(assign-feature)
(exception-feature)

(run-tests)
