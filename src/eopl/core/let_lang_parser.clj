(ns eopl.core.let-lang-parser
  (:use eopl.core.define-datatype)
  (:use name.choi.joshua.fnparse))

(def identifier? (partial re-matches #"[a-zA-Z_][a-zA-Z0-9_]*"))

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
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (minus-exp
   (exp1 expression?))
  (zero?-exp
   (exp1 expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp expression?))
  (cdr-exp
   (exp expression?))
  (null?-exp
   (exp expression?))
  (emptylist-exp)
  (list-exp
   (items & #(every? expression? %1)))
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
   (bool boolean?))
  (list-val
   (list seq?)))

(def space* (rep* (lit-alt-seq " \n\t")))
(def space+ (rep+ (lit-alt-seq " \n\t")))

(def parse-const-exp
  (complex [num (rep+ (lit-alt-seq (mapcat str (range 0 10))))]
           (const-exp (Integer/parseInt (apply str num)))))

(declare parse-expression)

(def parse-arg-list
  (alt (complex [_ space*
                 exp parse-expression
                 _ space*
                 _ (lit \,)
                 exps parse-arg-list]
                (concat (list exp) exps))
       (complex [_ space*
                 exp parse-expression
                 _ space*]
                (list exp))))

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

(def parse-list-exp
  (complex [_ (lit-conc-seq "list")
            args parse-args]
           (list-exp args)))

(defmacro def-parse-1-arg [name op]
  `(def ~(symbol (str "parse-" name "-exp"))
     (complex [_# ~op
               _# space*
               _# (lit \()
               _# space*
               exp1# parse-expression
               _# space*
               _# (lit \))]
              (~(symbol (str name "-exp")) exp1#))))

(defmacro def-parse-2-arg [name op]
  `(def ~(symbol (str "parse-" name "-exp"))
     (complex [_# ~op
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
              (~(symbol (str name "-exp")) exp1# exp2#))))


(def-parse-2-arg diff (lit \-))
(def-parse-2-arg add (lit \+))
(def-parse-2-arg mul (lit \*))
(def-parse-2-arg div (lit \/))
(def-parse-2-arg equal? (lit-conc-seq "equal?"))
(def-parse-2-arg greater? (lit-conc-seq "greater?"))
(def-parse-2-arg less? (lit-conc-seq "less?"))
(def-parse-2-arg cons (lit-conc-seq "cons"))

(def-parse-1-arg minus (lit-conc-seq "minus"))
(def-parse-1-arg zero? (lit-conc-seq "zero?"))
(def-parse-1-arg car (lit-conc-seq "car"))
(def-parse-1-arg cdr (lit-conc-seq "cdr"))
(def-parse-1-arg null? (lit-conc-seq "null?"))


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

(def parse-emptylist-exp
  (complex [_ space*
            _ (lit-conc-seq "emptylist")
            _ space*]
           (emptylist-exp)))

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
                                  (parse-charset
                                   [[\a \z]
                                    [\A \Z]
                                    [\0 \9]]))))]
           (apply str (flatten iden))))

(def parse-var-exp
  (complex [var parse-identifier]
           (var-exp var)))

(def parse-let-exp
  (complex [_ (lit-conc-seq "let")
            _ space+
            var parse-identifier
            _ space+
            _ (lit \=)
            _ space+
            exp parse-expression
            _ space+
            _ (lit-conc-seq "in")
            _ space+
            body parse-expression]
           (let-exp var exp body)))

(def parse-expression
  (alt parse-const-exp
       parse-minus-exp
       parse-diff-exp
       parse-add-exp
       parse-div-exp
       parse-mul-exp
       parse-equal?-exp
       parse-greater?-exp
       parse-less?-exp
       parse-zero?-exp
       parse-cons-exp
       parse-car-exp
       parse-cdr-exp
       parse-null?-exp
       parse-emptylist-exp
       parse-list-exp
       parse-if-exp
       parse-let-exp
       parse-var-exp))

(def parse-program
  (complex [exp parse-expression]
           (a-program exp)))

(defn parse [stream]
  (rule-match parse-program  prn prn {:remainder stream}))

