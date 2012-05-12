(ns eopl.core.store-pass-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use eopl.core.explicit-ref-lang-parser)
  (:use eopl.core.world-ref)
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

(defn expval->ref [val]
  (cases expval val
    (ref-val (ref) ref)
    (else (throw (Exception. (str "invalid ref " val))))))

(defn expval->val [val]
  (cases expval val
    (num-val (num) num)
    (bool-val (bool) bool)
    (list-val (lst)
              (map #(expval->val %1) lst))
    (ref-val (ref) ref)))

(declare value-of)


(defn reduce-with-world [initial coll env]
  (let [[result world] (reduce (fn [[acc world] item]
                                 (let [[res world] (value-of item env world)]
                                   [(cons res acc) world]))
                               initial
                               coll)]
    [(reverse result) world]))


(defn apply-procedure [p args world]
  (cases proc p
    (procedure (vars body saved-env)
               (let [new-env (reduce
                              (fn [new-env [var arg]]
                                (extend-env new-env var arg))
                              saved-env
                              (map (fn [x y] [x y]) vars args))]
                 (value-of body new-env world)))))


(defn value-of [exp env world]
  (cases expression exp
    (const-exp (num) [(num-val num) world])

    (cons-exp (exp1 exp2)
              (let [[left world] (value-of exp1 env world)
                    [right world] (value-of exp2 env world)]
                [(list-val (cons left (expval->list right))) world]))

    (car-exp (exp1)
             (let [[list-exp world] (value-of exp1 env world)
                   lst (expval->list list-exp)]
               (if (empty? lst)
                 [(list-val '()) world]
                 [(first lst) world])))

    (cdr-exp (exp1)
             (let [[list-exp world] (value-of exp1 env world)
                   lst (expval->list list-exp)]
               (if (empty? lst)
                 [(list-val '()) world]
                 [(list-val (next lst)) world])))

    (emptylist-exp () [(list-val '()) world])


    (null?-exp (exp1)
               (let [[list-exp world] (value-of exp1 env world)]
                 [(bool-val (empty? (expval->list list-exp))) world]))

    (zero?-exp (exp1)
               (let [[num-exp world] (value-of exp1 env world)]
                 [(bool-val (zero? (expval->num num-exp))) world]))

    (true-exp () [(bool-val true) world])
    (false-exp () [(bool-val false) world])

    (list-exp (args)
              (let [[list-exp world] (reduce-with-world
                                       ['() world]
                                       args
                                       env)]
                [(list-val list-exp) world]))

    (newref-exp (exp)
                (let [[val world] (value-of exp env world)
                      [ref world] (newref val world)]
                  [(ref-val ref) world]))

    (de-ref-exp (exp)
                (let [[ref world] (value-of exp env world)]
                  (de-ref (expval->ref ref) world)))

    (setref-exp (ref-exp value-exp)
                (do
                  (let [[ref world] (value-of ref-exp env world)
                        [val world] (value-of value-exp env world)
                        [_ world] (setref! (expval->ref ref) val world)]
                    [(num-val 23) world])))

    (begin-exp (exps)
               (reduce (fn [[last-val world] exp]
                         (value-of exp env world))
                       [nil world]
                       exps))

    (if-exp (exp1 exp2 exp3)
            (let [[predicate world] (value-of exp1 env world)]
              (if (expval->bool predicate)
                (value-of exp2 env world)
                (value-of exp3 env world))))

    (proc-exp (vars body)
              [(proc-val (procedure vars body env)) world])


    (call-exp (rator rands)
              (let [[proc-exp world] (value-of rator env world)
                    proc (expval->proc proc-exp)
                    [args world] (reduce-with-world
                                   ['() world]
                                   rands
                                   env)]
                (apply-procedure proc args world)))

    (var-exp (var) [(apply-env env var) world])

    (let-exp (body bindings)
             (let [[new-env world] (reduce (fn [[new-env world] bind]
                                             (cases binding bind
                                               (binding-exp (var exp)
                                                            (let [[val world] (value-of exp env world)]
                                                              [(extend-env new-env var val) world]))))
                                           [env world]
                                           bindings)]
               (value-of body new-env world)))

    (else (throw (Exception. (str "unkonwn/unimplemented  exp" exp))))))

(defn value-of-program [pgm]
  (cases program pgm
    (a-program (exp1)
               (let [[result world] (value-of exp1 (empty-env) empty-store)]
                 result))))


(defn run [program]
  (value-of-program (parse program)))

(defn result [program]
  (expval->val (run program)))

(run-tests)
