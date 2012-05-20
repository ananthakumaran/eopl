(ns eopl.core.feature
  (:use clojure.test))

(defmacro defeature [name & body]
  `(defn ~(symbol (str name "-feature")) []
     (eval '(deftest ~(symbol (str name "-test"))
              ~@body))))

(defeature list
  (is (= (result "let x = 4
                  in cons(x,
                         cons(cons(-(x, 1),
                                   emptylist),
                               emptylist))")
         '(4 (3))))
  (is (= (result "car(cons(4,emptylist))")
         4))
  (is (= (result "car(emptylist)")
         '()))
  (is (= (result "cdr(emptylist)")
         '()))
  (is (= (result "null?(emptylist)")
         true))
  (is (= (result "cdr(cons(4,cons(4,emptylist)))")
         '(4))))

(defeature implicit-ref
  (is (= (result "let x = newref(newref(0))
                  in begin
                    setref(de-ref(x), 11);
                    de-ref(de-ref(x))
                  end")
         11)))

(defeature arithimetic
  (is (= (result "+(1,2)")
         3))
  (is (= (result "*(2,2)")
         4))
  (is (= (result "/(10,5)")
         2)))

(defeature if
  (is (= (result "if zero? (0) then 1 else 2")
         1)))

(defeature proc
  (is (= (result "let sum = proc (x) proc (y) -(x, -(0,y))
                  in ((sum 3) 4)")
         7)))

(defeature proc-multi
  (is (= (result "let f = proc (x y) -(x, y)
                  in (f 10 5)")
         5)
      )
  (is (= (result "letproc f (x y) -(x, y)
                  in (f 10 5)")
         5)))

(defeature let-proc
  (is (= (result "let x = 200
                  in letproc f (z) -(z,x)
                     in let x = 100
                        in letproc g (z) -(z,x)
                           in -((f 1), (g 1))")
         -100)))

(defeature let
  (is (= (result "let x = 30
                  in let x = -(x,1)
                         y = -(x,2)
                     in -(x,y)")
         1)))

(defeature let-multi
  (is (= (result "let x = 30
                  in let x = -(x,1)
                         y = -(x,2)
                     in -(x,y)")
         1)))

(defeature let*
  (is (= (result "let x = 30
                  in let* x = -(x,1)
                         y = -(x,2)
                     in -(x,y)")
         2)))

(defeature letrec
  (is (= (result "letrec
                   number(x acc) =
                     if equal?(1,x)
                     then
                        cons(1,acc)
                     else
                        (number -(x,1) cons(x,acc))
                   in
                    (number 5 emptylist)")
         '(1 2 3 4 5))))

(defeature letrec-multi
  (is (= (result "letrec
                    even(x) = if zero?(x) then true else (odd -(x,1))
                    odd(x) = if zero?(x) then false else (even -(x,1))
                  in (odd 13)")
         true)))

(defeature unpack
  (is (= (result "let u = 7
                  in unpack x y = cons(u, cons(3, emptylist))
                     in -(x,y)")
         4)))

(defeature cond
  (is (= (result "let x = 100
           in
           cond zero?(x) ==> 0
                less?(x, 10) ==> 9
                greater?(x, 10) ==> 11
                equal?(x, 10) ==> 10
           end")
         11))
  (is (thrown-with-msg? Exception #"unhandled condition"
        (result "cond zero?(10) ==> 0 end"))))


(defeature assign
  (is (= (result "let f = proc (x) proc (y)
                   begin
                     set x = -(x, minus(1));
                     -(x,y)
                   end
         in ((f 44) 33)")
         12))
  (is (= (result "let times4 = 0
         in begin
           set times4 = proc (x)
                      if zero?(x)
                      then 0
                      else -((times4 -(x,1)), minus(4)) ;
           (times4 3)
         end")
         12)))

(defeature assign-mutable
  (is (= (result "letmutable times4 = 0
         in begin
           set times4 = proc (x)
                      if zero?(x)
                      then 0
                      else -((times4 -(x,1)), minus(4)) ;
           (times4 3)
         end")
         12)))
