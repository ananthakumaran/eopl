(ns eopl.core.checker-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use eopl.core.checker-lang-parser)
  (:use eopl.core.feature)
  (:use clojure.test))


(defn type-to-external-form [t]
  (cases type t
    (int-type () :int)
    (bool-type () :bool)
    (proc-type (vars result-type)
               (list (map type-to-external-form vars)
                     :->
                     (type-to-external-form result-type)))))

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
    (var-exp (var) (apply-env tenv var))
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
    (let-exp (body bindings)
             (type-of body
                      (reduce (fn [nenv b]
                                (cases binding b
                                  (binding-exp (var exp1)
                                               (extend-env nenv var (type-of exp1 tenv)))))
                              tenv
                              bindings)))
    (proc-exp (vars body)
              (let [result-type
                    (type-of body
                             (reduce
                              (fn [nenv var]
                                (cases typed var
                                  (typed-var (name type)
                                             (extend-env nenv name type))))
                              tenv vars))]
                (proc-type (map (fn [var]
                                  (cases typed var
                                    (typed-var (name type)
                                               type)))
                                vars)
                           result-type)))

    (call-exp (rator rands)
              (let [rator-type (type-of rator tenv)
                    rands-type (map #(type-of %1 tenv) rands)]
                (cases type rator-type
                  (proc-type (args result-type)
                             (doseq [[a b] (zipmap args rands-type)]
                               (check-equal-type! a b exp))
                             result-type)
                  (else (throw (Exception. (str "Rator not a proc: " (type-to-external-form rator-type))))))))
    (else (throw (Exception. (str "unkonwn exp " exp))))))

(defn type-of-program [pgm]
  (cases program pgm
    (a-program (exp) (type-of exp (empty-env)))))

(defn check [program]
  (type-of-program (parse program)))

(defn type [program]
  (type-to-external-form (check program)))

(type-feature)

(run-tests)


