(ns eopl.core.feature
  (:use clojure.test))

(defmacro defeature [name & body]
  `(defn ~(symbol (str name "-feature")) []
     (eval '(deftest ~(symbol (str name "-test"))
              ~@body))))

(defeature constant
  (is (= (result "4") 4)))

(defeature diff
  (is (= (result "-(4, -(5, 4))") 3)))

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
         '(4)))
  (is (= (result "list(1,2,-(4,1))")
         '(1 2 3))))

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
         2))
  (is (= (result "minus(5)")
         -5)))

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

(defeature letrec-factorial
  (is (= (result "letrec factorial(x) = if less?(x,2) then 1 else (+ x (factorial -(x,1)))
                 in (factorial 5)")
         15)))

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

(defeature setdynamic
  (is (= (result "let x = 11
                  in let p = proc (y) -(y,x)
                     in -(setdynamic x = 17 during (p 22),
                          (p 13))")
         3)))


(defeature statement
  (is (= (with-out-str
           (result "var x,y; {x = 3; y = 4; print +(x,y)}"))
         "7"))
  (is (= (with-out-str
           (result "var x,y,z; {
    x = 3;
    y = 4;
    z = 0;
    while not(zero?(x))
    { z = +(z,y); x = -(x,1) };
    print z
}"))
         "12"))

  (is (= (with-out-str
           (result "var x,y,z; {
    x = 0;
    z = 0;
    y = 4;
    do-while not(zero?(x))
    { z = +(z,y) };
    print z
}"))
         "4"))

  (is (= (with-out-str
           (result "var f,x; {f = proc(x y) *(x,y);
                              x = 3;
                              print (f 4 x)}"))
         "12"))

  (is (= (with-out-str
           (binding [*in* (BufferedReader. (InputStreamReader. (ByteArrayInputStream. (.getBytes "10\n"))))]
             (result "var x; { read x; print x }")))
         "10")))


(defeature mutpair
  (is (= (result "let glo = pair(11,22)
         in let f = proc (loc)
                     let d1 = setright(loc, left(loc))
                     in let d2 = setleft(glo, 99)
                        in -(left(loc),right(loc))
            in (f glo)")
         88)))

(defeature array
  (is (= (result "let a = newarray(2,minus(99))
             p = proc (x)
                   let v = arrayref(x,1)
                   in arrayset(x,1,-(v,minus(1)))
         in begin arrayset(a,1,0); (p a); (p a); arrayref(a,1) end")
         2))
  (is (thrown-with-msg? Exception #"indexoutofbounds"
      (result "let a = newarray(2,minus(99))
             in arrayref(a,2)")))
  (is (= (result "let a = newarray(2,minus(99))
             in arraylength(a)")
         2)))

(defeature ref
  (is (= (result "let a = 3
             b = 4
             swap = proc (x y)
                    let temp = x
                      in begin
                         set x = y;
                         set y = temp
                      end
          in begin
             (swap ref a ref b);
            -(a,b)
          end")
         1)))

(defeature exception
  (is (= (with-out-str
           (result "raise 0"))
         "unhandled exception 0"))
  (is (= (with-out-str
           (result "try raise 0 catch(x) raise x"))
         "unhandled exception 0"))
  (is (= (result "try raise 0 catch(x) 0") 0))
  (is (= (result "try 0 catch(x) raise 9") 0))
  (is (= (result "try try raise 0 catch(x) raise x catch(x) x") 0)))

(defeature lock
  (is (= (with-out-str
           (result "let x = 0
         in let mut = mutex
         in let incr_x = proc (id)
                           proc(dummy)
                             begin
                               wait(mut);
                               set x = -(x, minus(1));
                               signal(mut);
                               print(x)
                             end
            in begin
               spawn((incr_x 100));
               spawn((incr_x 200));
               spawn((incr_x 300));
               spawn((incr_x 400));
               spawn((incr_x 500))
               end"))
         "1\n2\n3\n4\n5\n")))

