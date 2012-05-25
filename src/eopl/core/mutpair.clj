(ns eopl.core.mutpair
  (:use eopl.core.define-datatype)
  (:use eopl.core.vector-ref))


(define-datatype mutpair mutpair?
  (a-pair
   (left-loc reference?)
   (right-loc reference?)))

(defn make-pair [left right]
  (a-pair (newref left)
          (newref right)))

(defn left [mp]
  (cases mutpair mp
    (a-pair (left-loc right-loc)
            (de-ref left-loc))))

(defn right [mp]
  (cases mutpair mp
    (a-pair (left-loc right-loc)
            (de-ref right-loc))))


(defn set-left [mp value]
  (cases mutpair mp
    (a-pair (left-loc right-loc)
            (setref! left-loc
                     value))))

(defn set-right [mp value]
  (cases mutpair mp
    (a-pair (left-loc right-loc)
            (setref! right-loc
                     value))))
