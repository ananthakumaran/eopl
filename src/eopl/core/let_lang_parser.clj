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
  (minus-exp
   (exp1 expression?))
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

(def space* (rep* (lit \space)))
(def space+ (rep+ (lit \space)))

(def parse-const-exp
  (complex [num (rep+ (lit-alt-seq (mapcat str (range 0 10))))]
           (const-exp (Integer/parseInt (apply str num)))))

(declare parse-expression)

(def parse-diff-exp
  (complex [_ (lit \-)
            _ space*
            _ (lit \()
            _ space*
            exp1 parse-expression
            _ space*
            _ (lit \,)
            _ space*
            exp2 parse-expression
            _ space*
            _ (lit \))
            ]
           (diff-exp exp1 exp2)))

(def parse-minus-exp
  (complex [_ space*
            _ (lit-conc-seq "minus")
            _ space*
            _ (lit \()
            _ space*
            exp parse-expression
            _ space*
            - (lit \))]
           (minus-exp exp)))

(def parse-zero?-exp
  (complex [_ (lit-conc-seq "zero?")
            _ space+
            _ (lit \()
            _ space*
            exp parse-expression
            _ space*
            _ (lit \))]
           (zero?-exp exp)))

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
       parse-zero?-exp
       parse-if-exp
       parse-let-exp
       parse-var-exp))

(def parse-program
  (complex [exp parse-expression]
           (a-program exp)))

(defn parse [stream]
  (rule-match parse-program  prn prn {:remainder stream}))

