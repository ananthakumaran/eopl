(ns eopl.core.exception-lang-parser
  (:use eopl.core.define-datatype)
  (:use eopl.core.env)
  (:use name.choi.joshua.fnparse)
  (:use eopl.core.link-ref))

(def identifier? (partial re-matches #"([a-zA-Z_][a-zA-Z0-9_?]*)|[*+/-]"))

(declare condition?)
(declare binding?)
(declare proc-binding?)

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mul-exp
   (exp1 expression?)
   (exp2 expression?))
  (div-exp
   (exp1 expression?)
   (exp2 expression?))
  (minus-exp
   (exp1 expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp expression?))
  (cdr-exp
   (exp expression?))
  (emptylist-exp)
  (list-exp
   (items & #(every? expression? %1)))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (cond-exp
   (conditions #(every? condition? %1)))
  (var-exp
   (var identifier?))
  (print-exp
   (exp expression?))
  (let-exp
   (body expression?)
   (bindings #(every? binding? %1)))
  (setdynamic-exp
   (body expression?)
   (bindings #(every? binding? %1)))
  (let*-exp
   (body expression?)
   (bindings #(every? binding? %1)))
  (unpack-exp
   (body expression?)
   (vars #(every? identifier? %1))
   (value expression?))
  (proc-exp
   (vars #(every? identifier? %1))
   (body expression?))
  (letproc-exp
   (name identifier?)
   (vars #(every? identifier? %1))
   (proc-body expression?)
   (body expression?))
  (letrec-exp
   (body expression?)
   (bindings #(every? proc-binding? %1)))
  (call-exp
   (rator expression?)
   (rands #(every? expression? %1)))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (null?-exp
   (exp expression?))
  (true-exp)
  (false-exp)
  (begin-exp
   (exps #(every? expression? %1)))
  (assign-exp
   (var identifier?)
   (exp expression?))
  (ref-exp
   (var identifier?))
  (try-exp
   (exp expression?)
   (var identifier?)
   (handler expression?))
  (raise-exp
   (exp expression?)))

(define-datatype binding binding?
  (binding-exp
   (var identifier?)
   (value expression?)))

(define-datatype proc-binding proc-binding?
  (proc-binding-exp
   (name identifier?)
   (vars #(every? identifier? %1))
   (body expression?)))

(define-datatype proc proc?
  (procedure
   (vars #(every? identifier? %1))
   (body expression?)
   (saved-env environment?)))

(define-datatype condition condition?
  (clause-exp
   (predicate expression?)
   (consequence expression?)))

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(defn boolean? [x]
  (or (true? x) (false? x)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (list-val
   (list seq?))
  (proc-val
   (proc proc?)))

(def space* (rep* (lit-alt-seq " \n\t")))
(def space+ (rep+ (lit-alt-seq " \n\t")))

(def parse-const-exp
  (complex [num (rep+ (lit-alt-seq (mapcat str (range 0 10))))]
           (const-exp (Integer/parseInt (apply str num)))))

(declare parse-expression)

(def primitives (atom '{}))

(defn primitive [name op]
  (swap! primitives assoc (symbol op) (symbol name)))

(defn primitive? [rator]
  (cases expression rator
    (var-exp (var)
             (contains? @primitives (symbol var)))
    (else false)))

(defn primitive-exp [rator rands]
  (let [result (cases expression rator
                 (var-exp (var)
                          (apply
                           (ns-resolve 'eopl.core.exception-lang-parser
                                       (symbol (str ((symbol var) @primitives) "-exp")))
                           rands))
                 (else (throw (Exception. (str "invalid primitive " rator)))))]
    result))

(defmacro def-parse-delimited-exps [name delimiter]
  `(def ~name
     (alt (complex [_# space*
                    exp# parse-expression
                    _# space*
                    _# ~delimiter
                    exps# ~name]
                   (cons exp# exps#))
          (complex [_# space*
                    exp# parse-expression
                    _# space*]
                   (list exp#)))))

(def-parse-delimited-exps parse-arg-list (lit \,))


(def parse-args
  (complex [_ space*
            _ (lit \()
            _ space*
            exps (alt parse-arg-list
                      (complex [_ emptiness]
                               (list)))
            _ space*
            _ (lit \))]
           exps))

(primitive 'list 'list)
(def parse-list-exp
  (complex [_ (lit-conc-seq "list")
            args parse-args]
           (apply list-exp args)))

(defmacro def-parse-1-arg [name op]
  `(do
     (primitive '~name ~op)
     (def ~(symbol (str "parse-" name "-exp"))
       (complex [_# (lit-conc-seq ~op)
                 _# space*
                 _# (lit \()
                 _# space*
                 exp1# parse-expression
                 _# space*
                 _# (lit \))]
                (~(symbol (str name "-exp")) exp1#)))))

(defmacro def-parse-2-arg [name op]
  `(do
     (primitive '~name ~op)
     (def ~(symbol (str "parse-" name "-exp"))
       (complex [_# (lit-conc-seq ~op)
                 _# space*
                 _# (lit \()
                 _# space*
                 exp1# parse-expression
                 _# space*
                 _# (lit \,)
                 _# space*
                 exp2# parse-expression
                 _# space*
                 _# (lit \))]
                (~(symbol (str name "-exp")) exp1# exp2#)))))


(def-parse-2-arg diff "-")
(def-parse-2-arg add "+")
(def-parse-2-arg mul "*")
(def-parse-2-arg div "/")
(def-parse-2-arg equal? "equal?")
(def-parse-2-arg greater? "greater?")
(def-parse-2-arg less? "less?")
(def-parse-2-arg cons "cons")

(def-parse-1-arg minus "minus")
(def-parse-1-arg zero? "zero?")
(def-parse-1-arg car "car")
(def-parse-1-arg cdr "cdr")
(def-parse-1-arg null? "null?")
(def-parse-1-arg print "print")

(def parse-if-exp
  (complex [_ (lit-conc-seq "if")
            _ space+
            exp1 parse-expression
            _ space+
            _ (lit-conc-seq "then")
            _ space+
            exp2 parse-expression
            _ space+
            _ (lit-conc-seq "else")
            _ space+
            exp3 parse-expression]
           (if-exp exp1 exp2 exp3)))

(declare parse-identifier)

(def parse-var-list
  (alt (complex [_ space*
                 var parse-identifier
                 _ space+
                 vars parse-var-list]
                (cons var vars))
       (complex [_ space*
                 var parse-identifier]
                (list var))))

(def parse-proc-exp
  (complex [_ (lit-conc-seq "proc")
            _ space*
            _ (lit \()
            vars parse-var-list
            _ space*
            _ (lit \))
            _ space*
            body parse-expression]
           (proc-exp vars body)))

(def parse-proc-binding
  (complex [_ space*
            name parse-identifier
            _ space*
            _ (lit \()
            vars parse-var-list
            _ space*
            _ (lit \))
            _ space*
            _ (lit \=)
            _ space*
            body parse-expression]
           (proc-binding-exp name vars body)))

(def parse-letrec-exp
  (complex [_ (lit-conc-seq "letrec")
            proc-bindings (rep+ parse-proc-binding)
            _ space+
            _ (lit-conc-seq "in")
            _ space+
            body parse-expression]
           (letrec-exp body proc-bindings)))

(def parse-letproc-exp
  (complex [_ (lit-conc-seq "letproc")
            _ space+
            name parse-identifier
            _ space*
            _ (lit \()
            vars parse-var-list
            _ space*
            _ (lit \))
            _ space*
            proc-body parse-expression
            _ space+
            _ (lit-conc-seq "in")
            _ space+
            body parse-expression]
           (letproc-exp name vars proc-body body)))

(def parse-rands
  (alt (complex [_ space*
                 rand parse-expression
                 _ space+
                 rands parse-rands]
                (cons rand rands))
       (complex [_ space*
                 rand parse-expression]
                (list rand))))

(def parse-arithimetic-rator
  (complex [op (alt (lit \*)
                    (lit \+)
                    (lit \/)
                    (lit \-))]
           (var-exp (str op))))

(def parse-call-exp
  (complex [_ (lit \()
            _ space*
            rator (alt parse-expression
                       parse-arithimetic-rator)
            rands (alt (complex [_ space+
                                 rands parse-rands]
                                rands)
                       (complex [_ space*]
                                (list)))
            _ space*
            _ (lit \))]
           (if (primitive? rator)
             (primitive-exp rator rands)
             (call-exp rator rands))))


(defmacro defkeyword [name]
  `(def ~(symbol (str "parse-" name "-exp"))
     (complex [_# space*
               _# (lit-conc-seq ~name)]
              (~(symbol (str name "-exp"))))))

(defkeyword "emptylist")
(defkeyword "true")
(defkeyword "false")

(defn parse-charset [seq]
  (lit-alt-seq (mapcat (fn [[lower higher]]
                         (map char (range (int lower) (inc (int higher)))))
                       seq)))

(def parse-identifier
  (complex [iden (conc (alt (lit \_)
                            (parse-charset
                             [[\a \z]
                              [\A \Z]]))
                       (rep* (alt (lit \_)
                                  (lit \?)
                                  (parse-charset
                                   [[\a \z]
                                    [\A \Z]
                                    [\0 \9]]))))]
           (apply str (flatten iden))))

(def parse-var-exp
  (complex [var parse-identifier]
           (var-exp var)))

(def parse-binding (complex [_ space+
                             var parse-identifier
                             _ space+
                             _ (lit \=)
                             _ space+
                             exp parse-expression]
                            (binding-exp var exp)))

(def parse-bindings (rep+ parse-binding))

(def parse-let-exp
  (complex [_ (lit-conc-seq "let")
            bindings parse-bindings
            _ space*
            _ (lit-conc-seq "in")
            _ space+
            body parse-expression]
           (let-exp body bindings)))

(def parse-setdynamic-exp
  (complex [_ (lit-conc-seq "setdynamic")
            bindings parse-bindings
            _ space*
            _ (lit-conc-seq "during")
            _ space+
            body parse-expression]
           (setdynamic-exp body bindings)))

(def parse-let*-exp
  (complex [_ (lit-conc-seq "let*")
            bindings parse-bindings
            _ space*
            _ (lit-conc-seq "in")
            _ space+
            body parse-expression]
           (let*-exp body bindings)))

(def parse-var
  (complex [_ space+
            var parse-identifier]
           var))

(def parse-unpack-exp
  (complex [_ (lit-conc-seq "unpack")
            vars (rep+ parse-var)
            _ space*
            _ (lit \=)
            _ space*
            value parse-expression
            _ space*
            _ (lit-conc-seq "in")
            _ space*
            body parse-expression]
           (unpack-exp body vars value)))


(def parse-clause
  (complex [_ space*
            predicate parse-expression
            _ space+
            _ (lit-conc-seq "==>")
            _ space+
            consequence parse-expression]
           (clause-exp predicate consequence)))

(def parse-cond-exp
  (complex [_ (lit-conc-seq "cond")
            _ space+
            conditions (rep+ parse-clause)
            _ space+
            _ (lit-conc-seq "end")]
           (cond-exp conditions)))

(def-parse-delimited-exps parse-exps (lit \;))

(def parse-begin-exp
  (complex [_ (lit-conc-seq "begin")
            _ space+
            exps parse-exps
            _ space*
            _ (lit-conc-seq "end")]
           (begin-exp exps)))

(def parse-assign-exp
  (complex [_ (lit-conc-seq "set")
            _ space+
            var parse-identifier
            _ space*
            _ (lit \=)
            _ space*
            exp parse-expression]
           (assign-exp var exp)))

(def parse-ref-exp
  (complex [_ (lit-conc-seq "ref")
            _ space+
            var parse-identifier]
           (ref-exp var)))

(def parse-try-exp
  (complex [_ (lit-conc-seq "try")
            _ space+
            exp parse-expression
            _ space*
            _ (lit-conc-seq "catch")
            _ space*
            _ (lit \()
            _ space*
            var parse-identifier
            _ space*
            _ (lit \))
            _ space*
            handler parse-expression]
           (try-exp exp var handler)))

(def parse-raise-exp
  (complex [_ (lit-conc-seq "raise")
            _ space+
            exp parse-expression]
           (raise-exp exp)))

(def parse-expression
  (alt parse-emptylist-exp
       parse-const-exp
       parse-minus-exp
       parse-diff-exp
       parse-add-exp
       parse-div-exp
       parse-mul-exp
       parse-cons-exp
       parse-car-exp
       parse-cdr-exp
       parse-equal?-exp
       parse-greater?-exp
       parse-less?-exp
       parse-zero?-exp
       parse-null?-exp
       parse-true-exp
       parse-false-exp
       parse-list-exp
       parse-if-exp
       parse-assign-exp
       parse-begin-exp
       parse-let-exp
       parse-setdynamic-exp
       parse-let*-exp
       parse-unpack-exp
       parse-cond-exp
       parse-print-exp
       parse-try-exp
       parse-raise-exp
       parse-letproc-exp
       parse-letrec-exp
       parse-proc-exp
       parse-call-exp
       parse-ref-exp
       parse-var-exp))

(def parse-program
  (complex [exp parse-expression]
           (a-program exp)))

(defn parse [stream]
  (rule-match parse-program  prn prn {:remainder stream}))
