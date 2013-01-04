(ns eopl.core.simple-module-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use eopl.core.simple-module-lang-parser)
  (:use eopl.core.feature)
  (:use clojure.test))


;; env extension

(defn extend-env-with-module [env name val]
  (conj env [name (->Module name val env)]))

(defn lookup-module [env m-name]
  (let [module (env m-name)]
    (if module
      (:val module)
      (throw (IllegalAccessException. (str "module " m-name " not found in env"))))))

(defn lookup-qualified-var-in-env [env m-name var]
  (cases typed-module (lookup-module env m-name)
         (simple-module (bindings)
                        (apply-env bindings var))))

(defn lookup-tmodule [env m-name]
  (let [module (env m-name)]
    (if module
      (:interface module)
      (throw (IllegalAccessException. (str "tmodule " m-name " not found in env"))))))

(defn extend-env-with-tmodule [tenv name interface]
  (conj tenv [name (->TModule name interface)]))

(defn lookup-variable-name-in-decls [search-var decls]
  (or (some (fn [decl]
              (cases declaration decl
                     (val-decl (var type)
                               (when (= var search-var)
                                 type))))
            decls)
      (throw (IllegalAccessException. (str "decl " search-var " not found in declarations")))))

(defn lookup-qualified-var-in-tenv [tenv m-name var]
  (cases interface (lookup-tmodule tenv m-name)
         (simple-iface (decls)
                       (lookup-variable-name-in-decls var decls))))



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
    (pairof-type (a b) (list :pairof (type-to-external-form a) (type-to-external-form b)))))

(defn check-equal-type! [type1 type2 exp]
  (when (not (= type1 type2))
    (throw (Exception. (str "Types didn't match: "
                            (type-to-external-form type1)
                            " != "
                            (type-to-external-form type2)
                            " in "
                            exp)))))


(defn type-of [exp tenv]
  (cases expression exp
    (const-exp (num) (int-type))
    (true-exp () (bool-type))
    (false-exp () (bool-type))
    (var-exp (var)
             (apply-env tenv var))
    (diff-exp (exp1 exp2)
              (doseq [e [exp1 exp2]]
                (check-equal-type! (type-of e tenv) (int-type) e))
              (int-type))
    (zero?-exp (exp1)
               (check-equal-type! (type-of exp1 tenv) (int-type) exp1)
               (bool-type))
    (if-exp (exp1 exp2 exp3)
            (check-equal-type! (type-of exp1 tenv) (bool-type) exp1)
            (check-equal-type! (type-of exp2 tenv) (type-of exp3 tenv) exp)
            (type-of exp2 tenv))
    (pair-exp (exp1 exp2)
              (pairof-type (type-of exp1 tenv) (type-of exp2 tenv)))
    (unpair-exp (a b exp body)
                (let [expt (type-of exp tenv)]
                  (cases type expt
                    (pairof-type (at bt)
                                 (type-of body
                                          (extend-env (extend-env tenv a at) b bt))))))
    (let-exp (body bindings)
             (type-of body
                      (reduce (fn [nenv b]
                                (cases binding b
                                  (binding-exp (var exp1)
                                               (extend-env nenv var (type-of exp1 tenv)))))
                              tenv
                              bindings)))

    (letrec-exp (body letrec-bindings)
                (let [penv (reduce (fn [nenv lb]
                                     (cases letrec-binding lb
                                       (letrec-binding-exp (return name vars body)
                                                           (extend-env nenv name (make-proc-type vars return)))))
                                   tenv
                                   letrec-bindings)]
                  (doseq [lb letrec-bindings]
                    (cases letrec-binding lb
                      (letrec-binding-exp (return name vars body)
                                          (check-equal-type!
                                           return
                                           (type-of body
                                                    (extend-env-with-vars penv vars))
                                           body))))
                  (type-of body
                           penv)))

    (proc-exp (vars body)
              (let [result-type
                    (type-of body
                             (extend-env-with-vars tenv vars))]
                (make-proc-type vars result-type)))

    (call-exp (rator rands)
              (let [rator-type (type-of rator tenv)
                    rands-type (map #(type-of %1 tenv) rands)]
                (cases type rator-type
                  (proc-type (args result-type)
                             (doseq [[a b] (zipmap args rands-type)]
                               (check-equal-type! a b exp))
                             result-type)
                  (else (throw (Exception. (str "Rator not a proc: " (type-to-external-form rator-type))))))))

    (qualified-var-exp (m-name var)
                       (lookup-qualified-var-in-tenv tenv m-name var))

    (else (throw (Exception. (str "unkonwn exp " exp))))))


(defn <:-decls [decls1 decls2 tenv]
  (cond (empty? decls2) true
        (empty? decls1) true
        :else
        (let [name1 (data-value (first decls1) :name)
              name2 (data-value (first decls2) :name)]
          (if (= name1 name2)
            (and (= (data-value (first decls1) :type)
                    (data-value (first decls2) :type))
                 (<:-decls (rest decls1) (rest decls2) tenv))
            (<:-decls (rest decls1) decls2 tenv)))))

(defn <:-iface [iface1 iface2 tenv]
  (cases interface iface1
         (simple-iface (decls1)
                       (cases interface iface2
                              (simple-iface (decls2)
                                            (<:-decls decls1 decls2 tenv))))))

(defn defns-to-decls [defns tenv]
  (map (fn [defn]
         (cases definition defn
                (val-defn (var-name exp)
                          (let [ty (type-of exp tenv)]
                            (val-decl var-name ty)))))
       defns))

(defn interface-of [m-body tenv]
  (cases module-body m-body
         (defns-module-body (defns)
           (simple-iface (defns-to-decls defns tenv)))))

(defn add-module-defns-to-tenv [tenv defns]
  (if (empty? defns)
    tenv
    (cases module-defn (first defns)
           (a-module-definition (m-name expected-interface m-body)
                                (let [actual-interface (interface-of m-body tenv)]
                                  (if (<:-iface actual-interface expected-interface tenv)
                                    (let [ntenv (extend-env-with-tmodule tenv m-name expected-interface)]
                                      (add-module-defns-to-tenv ntenv (rest defns)))
                                    (throw (Exception. (str "interface doesn't match")))))))))

(defn type-of-program [pgm]
  (cases program pgm
    (a-program (module-defns body)
               (type-of body (add-module-defns-to-tenv (empty-env) module-defns)))))

(defn check [program]
  (type-of-program (parse program)))

(defn type [program]
  (type-to-external-form (check program)))

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

(defn typed-vars->vars [vars]
  (map #(data-value %1 :name) vars))

(defn value-of [exp env]
  (cases expression exp
         (const-exp (num) (num-val num))
         (true-exp () (bool-val true))
         (false-exp () (bool-val false))

         (diff-exp (exp1 exp2)
                   (num-val-of-exp - env exp1 exp2))

         (zero?-exp (exp1)
                    (bool-val (zero? (expval->num (value-of exp1 env)))))

         (list-exp (args)
                   (list-val (map #(value-of %1 env) args)))

         (if-exp (exp1 exp2 exp3)
                 (if (expval->bool (value-of exp1 env))
                   (value-of exp2 env)
                   (value-of exp3 env)))

         (proc-exp (vars body)
                   (proc-val (procedure (typed-vars->vars vars) body env)))

         (call-exp (rator rands)
                   (let [proc (expval->proc (value-of rator env))
                         args (map
                               #(value-of %1 env)
                               rands)]
                     (apply-procedure proc args)))

         (var-exp (var) (apply-env env var))

         (qualified-var-exp (m-name var)
                            (lookup-qualified-var-in-env env m-name var))

         (let-exp (body bindings)
                  (value-of body
                            (reduce (fn [new-env bind]
                                      (cases binding bind
                                             (binding-exp (var exp)
                                                          (extend-env new-env var (value-of exp env)))))
                                    env
                                    bindings)))

         (letrec-exp (body letrec-bindings)
                     (let [lbs (into {} (map (fn [lb]
                                               (cases letrec-binding lb
                                                      (letrec-binding-exp (return name vars body)
                                                                        [name [name (typed-vars->vars vars) body]])))
                                             letrec-bindings))]
                       (value-of body
                                 (extend-env-rec
                                  env
                                  (fn [name new-env]
                                    (let [[name vars body] (get lbs name)]
                                      (proc-val (procedure vars body new-env))))
                                  (keys lbs)))))

         (else (throw (Exception. (str "unkonwn exp " exp))))))

(defn defns-to-env [defns env]
  (if (empty? defns)
    (empty-env)
    (cases definition (first defns)
           (val-defn (var exp)
                     (let [val (value-of exp env)
                           nenv (extend-env env var val)]
                       (extend-env (defns-to-env (rest defns) nenv) var val))))))

(defn value-of-module-body [body env]
  (cases module-body body
         (defns-module-body (defns)
           (simple-module (defns-to-env defns env)))))

(defn add-module-defns-to-env [defns env]
  (if (empty? defns)
    env
    (cases module-defn (first defns)
           (a-module-definition (name interface body)
                                (add-module-defns-to-env (rest defns)
                                                         (extend-env-with-module
                                                           env
                                                           name
                                                           (value-of-module-body body env)))))))

(defn value-of-program [pgm]
  (cases program pgm
         (a-program (m-defns body)
                    (expval->val (value-of body
                                           (add-module-defns-to-env m-defns (empty-env)))))))

(defn check-and-run [pgm]
  (let [ast (parse pgm)]
    (type-of-program ast)
    (value-of-program ast)))

(simple-module-feature)
(run-tests)


