(ns eopl.core.thread-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use eopl.core.thread-lang-parser)
  (:use eopl.core.vector-ref)
  (:use eopl.core.scheduler)
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

(defn expval->mutex [val]
  (cases expval val
         (mutex-val (mutex) mutex)
         (else (throw (Exception. (str "invalid mutex " val))))))

(defn expval->val [val]
  (cases expval val
         (num-val (num) num)
         (bool-val (bool) bool)
         (list-val (lst)
                   (map #(expval->val %1) lst))))

(declare value-of-k)

(defn apply-cont [cont val]
  (if (time-expired?)
    (do
      (place-on-ready-queue!
       (fn [] (apply-cont cont val)))
        (run-next-thread))
    (do
      (decrement-timer!)
      (cont val))))

(defn end-main-thread-cont []
  (fn [val]
    (do
      (set-final-answer! val)
      (run-next-thread))))

(defn build-cont
  ([env exps cb results]
     (if (empty? exps)
       (cb results)
       (value-of-k (first exps) env
                   #(build-cont env (rest exps) cb (conj results %)))))
  ([env exps cb]
     (build-cont env exps cb [])))

(defn let-cont [env new-env bindings cb]
  (if (empty? bindings)
    (cb new-env)
    (cases binding (first bindings)
      (binding-exp (var exp)
                   (value-of-k exp env
                               #(let-cont env
                                          (extend-env new-env var (newref %))
                                          (rest bindings)
                                          cb))))))

(defn let*-cont [env bindings cb]
  (if (empty? bindings)
    (cb env)
    (cases binding (first bindings)
      (binding-exp (var exp)
                   (value-of-k exp env
                               #(let*-cont (extend-env env var (newref %))
                                          (rest bindings)
                                          cb))))))
(defn apply-procedure [p args cont]
  (cases proc p
    (procedure (vars body saved-env)
               (let [new-env (reduce
                              (fn [new-env [var arg]]
                                (extend-env new-env var (newref arg)))
                              saved-env
                              (map (fn [x y] [x y]) vars args))]
                 (value-of-k body new-env cont)))))


(defn num-val-of-exp [op env cont & exps]
  (build-cont env exps
              #(apply-cont cont
                           (num-val
                            (apply op (map expval->num %))))))

(defn bool-val-of-exp [op env cont & exps]
  (build-cont env exps
              #(apply-cont cont
                           (bool-val
                            (apply op (map expval->num %))))))

(defn value-of-k [exp env cont]
  (cases expression exp

         (const-exp (num) (apply-cont cont (num-val num)))

         (diff-exp (exp1 exp2)
                   (num-val-of-exp - env cont exp1 exp2))
         (add-exp (exp1 exp2)
                  (num-val-of-exp + env cont exp1 exp2))
         (mul-exp (exp1 exp2)
                  (num-val-of-exp * env cont exp1 exp2))
         (div-exp (exp1 exp2)
                  (num-val-of-exp quot env cont exp1 exp2))
         (minus-exp (exp)
                    (num-val-of-exp - env cont exp))

         (cons-exp (exp1 exp2)
                   (build-cont env [exp1 exp2]
                               #(apply-cont
                                 cont
                                 (list-val (cons (first %) (expval->list (second %)))))))

         (car-exp (exp1)
                  (value-of-k exp1 env
                              (fn [val]
                                (let [lst (expval->list val)]
                                  (apply-cont
                                   cont
                                   (if (empty? lst)
                                     (list-val '())
                                     (first lst)))))))

         (cdr-exp (exp1)
                  (value-of-k exp1 env
                              (fn [val]
                                (let [lst (expval->list val)]
                                  (apply-cont
                                   cont
                                   (if (empty? lst)
                                     (list-val '())
                                     (list-val (next lst))))))))


         (emptylist-exp () (apply-cont cont (list-val '())))

         (equal?-exp (exp1 exp2)
                     (bool-val-of-exp = env cont exp1 exp2))
         (less?-exp (exp1 exp2)
                    (bool-val-of-exp < env cont exp1 exp2))
         (greater?-exp (exp1 exp2)
                       (bool-val-of-exp > env cont exp1 exp2))

         (null?-exp (exp1)
                    (value-of-k exp1 env
                                #(apply-cont cont (bool-val (empty? (expval->list %))))))

         (zero?-exp (exp1)
                    (value-of-k exp1 env
                                #(apply-cont cont (bool-val (zero? (expval->num %))))))

         (true-exp () (apply-cont cont (bool-val true)))
         (false-exp () (apply-cont cont (bool-val false)))

         (list-exp (args)
                   (build-cont env args
                               #(apply-cont cont
                                           (list-val (seq %)))))


         (if-exp (exp1 exp2 exp3)
                 (value-of-k exp1 env
                             (fn [predicate]
                               (if (expval->bool predicate)
                                 (value-of-k exp2 env #(apply-cont cont %))
                                 (value-of-k exp3 env #(apply-cont cont %))))))

         (proc-exp (vars body)
                   (apply-cont cont (proc-val (procedure vars body env))))

         (letproc-exp (name vars proc-body body)
                      (let [new-env (extend-env env name (newref (proc-val (procedure vars proc-body env))))]
                        (value-of-k body new-env cont)))

         (call-exp (rator rands)
                   (value-of-k rator env
                               (fn [val]
                                 (let [proc (expval->proc val)]
                                   (build-cont env rands
                                               (fn [args]
                                                 (apply-procedure proc args cont)))))))


         (var-exp (var) (apply-cont cont (de-ref (apply-env env var))))

         (print-exp (exp)
                    (value-of-k exp env
                                (fn [val]
                                  (println (expval->val val))
                                  (apply-cont cont (num-val 5)))))

         (spawn-exp (exp)
                    (value-of-k exp env
                                (fn [val]
                                  (let [proc (expval->proc val)]
                                    (place-on-ready-queue!
                                     (fn []
                                       (apply-procedure proc (num-val 28) (fn [_] (run-next-thread)))))
                                    (apply-cont cont (num-val 73))))))

         (yield-exp ()
                    (place-on-ready-queue!
                     (fn []
                       (apply-cont cont (num-val 99))))
                    (run-next-thread))

         (mutex-exp ()
                    (apply-cont cont (mutex-val (a-mutex
                                                 (newref false)
                                                 (newref '())))))

         (wait-exp (exp)
                   (value-of-k exp env
                               (fn [val]
                                 (wait-for-mutex
                                  (expval->mutex val)
                                  (fn [] (apply-cont cont (num-val 52)))))))

         (signal-exp (exp)
                     (value-of-k exp env
                                 (fn [val]
                                   (signal-mutex
                                    (expval->mutex val)
                                    (fn [] (apply-cont cont (num-val 52)))))))

         (assign-exp (var exp)
                     (value-of-k exp env
                                 (fn [val]
                                   (do
                                     (setref! (apply-env env var)
                                              val)
                                     (apply-cont cont (num-val 27))))))

         (begin-exp (exps)
                    (build-cont env exps
                                #(apply-cont cont (last %))))


         (let-exp (body bindings)
                  (let-cont env env bindings
                            #(value-of-k body % cont)))


         (let*-exp (body bindings)
                   (let*-cont env bindings
                             #(value-of-k body % cont)))

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
                               cont)))

         (else (throw (Exception. (str "unkonwn exp " exp))))))

(defn value-of-program [pgm]
  (initialize-scheduler! 2)
  (cases program pgm
    (a-program (exp1)
               (value-of-k exp1 (empty-env) (end-main-thread-cont)))))


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
(lock-feature)

(run-tests)
