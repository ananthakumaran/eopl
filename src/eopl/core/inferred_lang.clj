(ns eopl.core.inferred-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use eopl.core.inferred-lang-parser)
  (:use eopl.core.feature)
  (:use clojure.test))


(defn make-proc-type [vars result-type]
  (proc-type (map (fn [var]
                    (cases typed var
                           (typed-var (name type)
                                      type)))
                  vars)
             result-type))

(defn extend-env-with-vars [env vars]
  (reduce (fn [nenv var]
            (cases typed var
                   (typed-var (name type)
                              (extend-env nenv name type))))
          env vars))

(defn type-to-external-form [t]
  (cases type t
         (int-type () :int)
         (bool-type () :bool)
         (proc-type (vars result-type)
                    (list (map type-to-external-form vars)
                          :->
                          (type-to-external-form result-type)))
         (pairof-type (a b) (list :pairof (type-to-external-form a) (type-to-external-form b)))
         (var-type (serial-number) (list :var serial-number))))

(defn apply-one-subst [source from to]
  (cases type source
         (int-type () source)
         (bool-type () source)
         (proc-type (vars result-type)
                    (proc-type
                     (map #(apply-one-subst %1 from to) vars)
                     (apply-one-subst result-type from to)))
         (var-type (serial-number)
                   (if (= source from) to source))))

(defn apply-subst-to-type [ty subst]
  (cases type ty
         (int-type () ty)
         (bool-type () ty)
         (proc-type (vars result-type)
                    (proc-type
                     (map #(apply-subst-to-type %1 subst) vars)
                     (apply-subst-to-type result-type subst)))
         (var-type (serial-number)
                   (or (get subst ty) ty))))


(defn map-hash [f hash]
  (into {}
        (map (fn [[key val]]
               (f key val))
             hash)))

(defn empty-subst [] {})
(def substitution? map?)
(defn extend-subst [subst tvar tval]
  (assoc (map-hash (fn [key val]
                     [key (apply-one-subst val tvar tval)])
                   subst)
    tvar tval))

(defn no-occurrence? [tvar ty]
  (cases type ty
         (int-type () true)
         (bool-type () true)
         (proc-type (vars result-type)
                    (and (map #(no-occurrence? tvar %1) vars)
                         (no-occurrence? tvar result-type)))
         (tvar-type (serial-number) (not (= tvar ty)))))



(defn occurrence-violation [ty1 ty2 exp]
  (throw (Exception. "occurrence-violation")))

(declare unifier)

(defn unify-procs [ty1 ty2 subst exp]
  (reduce (fn [subst [left right]]
            (unifier left right subst exp))
          subst
          (cons [(data-value ty1 :result-type)
                 (data-value ty2 :result-type)]
                (map vector (data-value ty1 :args) (data-value ty2 :args)))))

(defn unifier [ty1 ty2 subst exp]
  (let [ty1 (apply-subst-to-type ty1 subst)
        ty2 (apply-subst-to-type ty2 subst)]
    (cond (= ty1 ty2) subst
          (var-type? ty1) (if (no-occurrence? ty1 ty2)
                            (extend-subst subst ty1 ty2)
                            (occurrence-violation ty1 ty2 exp))
          (var-type? ty2) (if (no-occurrence? ty2 ty1)
                            (extend-subst subst ty2 ty1)
                            (occurrence-violation ty2 ty1 exp))
          (and (proc-type? ty1) (proc-type? ty2)) (unify-procs ty1 ty2 subst exp)
          :else (Exception. "unification failed"))))

(define-datatype answer answer?
  (an-answer
   (ty type?)
   (subst substitution?)))


(defn type-of [exp tenv subst]
  (cases expression exp

         (const-exp (num) (an-answer (int-type) subst))
         (true-exp () (an-answer (bool-type) subst))
         (false-exp () (an-answer (bool-type) subst))

         (var-exp (var) (an-answer (apply-env tenv var) subst))

         (diff-exp (exp1 exp2)
                   (cases answer (type-of exp1 tenv subst)
                          (an-answer (ty1 subst1)
                                     (cases answer (type-of exp2 tenv
                                                            (unifier ty1 (int-type) subst1 exp1))
                                            (an-answer (ty2 subst2)
                                                       (an-answer (int-type)
                                                                  (unifier ty2 (int-type) subst2 exp2)))))))

         (zero?-exp (exp1)
                    (cases answer (type-of exp1 tenv subst)
                           (an-answer (ty1 subst)
                                      (an-answer (bool-type) (unifier ty1 (int-type) subst exp)))))


         (if-exp (exp1 exp2 exp3)
                 (cases answer (type-of exp1 tenv subst)
                        (an-answer (ty1 subst)
                                   (let [subst (unifier ty1 (bool-type) subst exp1)]
                                     (cases answer (type-of exp2 tenv subst)
                                            (an-answer (ty2 subst)
                                                       (cases answer (type-of exp3 tenv subst)
                                                              (an-answer (ty3 subst)
                                                                         (an-answer ty2 (unifier ty2 ty3 subst exp))))))))))



         (let-exp (body bindings)
                  (let [[tenv susbt]
                        (reduce (fn [[nenv nsubst] b]
                                  (cases binding b
                                         (binding-exp (var exp1)
                                                      (cases answer (type-of exp1 tenv nsubst)
                                                             (an-answer (exp1-type nsubst)
                                                                        [(extend-env nenv var exp1-type) nsubst])))))
                                [tenv subst]
                                bindings)]
                    (type-of body tenv subst)))



         (letrec-exp (body letrec-bindings)
                     (let [penv (reduce (fn [nenv lb]
                                          (cases letrec-binding lb
                                                 (letrec-binding-exp (return name vars body)
                                                                     (extend-env nenv name (make-proc-type vars return)))))
                                        tenv
                                        letrec-bindings)
                           subst (reduce (fn [subst lb]
                                           (cases letrec-binding lb
                                                  (letrec-binding-exp (return name vars body)
                                                                      (cases answer (type-of body (extend-env-with-vars penv vars) subst)
                                                                             (an-answer (body-type subst)
                                                                                        (unifier body-type return subst body))))))
                                         subst
                                         letrec-bindings)]
                       (type-of body
                                penv
                                subst)))


         (proc-exp (vars body)
                   (cases answer (type-of body
                                          (extend-env-with-vars tenv vars)
                                          subst)
                          (an-answer (body-type subst)
                                     (an-answer (make-proc-type vars body-type)
                                                subst))))


         (call-exp (rator rands)
                   (let [result-type (fresh-var-type)]
                     (cases answer (type-of rator tenv subst)
                            (an-answer (rator-type subst)
                                       (let [[rand-types subst]
                                             (reduce
                                              (fn [[rand-types subst] rand]
                                                (cases answer (type-of rand tenv subst)
                                                       (an-answer (rand-type subst)
                                                                  [(conj rand-types rand-type) subst])))
                                              [[] subst]
                                              rands)]
                                         (an-answer result-type
                                                    (unifier rator-type
                                                             (proc-type rand-types result-type)
                                                             subst
                                                             exp)))))))


         (else (throw (Exception. (str "unkonwn exp " exp))))))

(defn type-of-program [pgm]
  (cases program pgm
         (a-program (exp)
                    (cases answer (type-of exp (empty-env) (empty-subst))
                           (an-answer (type subst)
                                      (apply-subst-to-type type subst))))))


(defn type [program]
  (reset-fresh-var-serial)
  (type-to-external-form (type-of-program (parse program))))

(inferred-type-feature)
(run-tests)

