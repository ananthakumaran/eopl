(ns eopl.core.let-lang
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use clojure.test))

(defn identifier? [x]
  (symbol? x))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?)))

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(defn boolean? [x]
  (or (true? x) (false? x)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

(defn expval->num [val]
  (cases expval val
         (num-val (num) num)
         (else (throw (Exception. (str "invalid number " val))))))

(defn expval->bool [val]
  (cases expval val
         (bool-val (bool) bool)
         (else (throw (Exception. (str "invalid bool " val))))))


(defn value-of [exp env]
  (cases expression exp
         (const-exp (num) (num-val num))
         (diff-exp (exp1 exp2)
                   (num-val
                    (- (expval->num (value-of exp1 env))
                       (expval->num (value-of exp2 env)))))
         (zero?-exp (exp1)
                    (bool-val (zero? (expval->num (value-of exp1 env)))))
         (if-exp (exp1 exp2 exp3)
                 (if (expval->bool (value-of exp1 env))
                   (value-of exp2 env)
                   (value-of exp3 env)))
         (var-exp (var) (apply-env env var))
         (let-exp (var exp1 body)
                  (let [new-env (extend-env env var (value-of exp1 env))]
                    (value-of body new-env)))))

(defn value-of-program [pgm]
  (cases program pgm
         (a-program (exp1)
                    (value-of exp1 (empty-env)))))


(defn run [program]
  (value-of-program program))

(defn result [program]
  (cases expval (run program)
         (num-val (num) num)
         (bool-val (bool) bool)))

(deftest let-test
  (is (= (result (a-program
                  (diff-exp (const-exp 10)
                            (const-exp 10))))
         0))
  (is (= (result (a-program
                  (let-exp 'x (const-exp 10)
                           (var-exp 'x))))
         10))
  (is (= (result (a-program
                  (if-exp
                   (zero?-exp (const-exp 0))
                   (const-exp 1)
                   (const-exp 2))))
         1)))

(run-tests)
